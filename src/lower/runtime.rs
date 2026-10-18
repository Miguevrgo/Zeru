//! What the generated code does on its own: safety checks and the panic they
//! end in, the Vec and `T?`/`T!` builtins, buffers, printing, the output
//! streams and their flush, and inline assembly.

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    attributes::{Attribute, AttributeLoc},
    intrinsics::Intrinsic,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, IntType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
        ValueKind,
    },
};

use crate::tir::{BinOp, Builtin, Expr, FnId, Place};
use crate::ty::{Ty, TyKind, Types};

use super::{Lower, Val};

pub(super) const FLUSH_FN: &str = "__zeru_flush";

impl<'a, 'ctx> Lower<'a, 'ctx> {
    /// A read-only global holding `bytes` and a terminating NUL, which a
    /// syscall taking a C string needs; a `\0` inside is kept as data.
    pub(super) fn const_bytes(&self, bytes: &[u8]) -> PointerValue<'ctx> {
        let value = self.context.const_string(bytes, true);
        let global = self.module.add_global(value.get_type(), None, "str");
        global.set_initializer(&value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.as_pointer_value()
    }

    fn call_fn(
        &self,
        function: FnId,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Option<BasicValueEnum<'ctx>> {
        let value = self.functions[function.0 as usize];
        match self
            .builder
            .build_call(value, args, "call")
            .unwrap()
            .try_as_basic_value()
        {
            ValueKind::Basic(value) => Some(value),
            ValueKind::Instruction(_) => None,
        }
    }

    /// What every failed check calls: flush what was printed, write the
    /// message to stderr, abort.
    fn panic_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(function) = self.panic {
            return function;
        }
        let void = self.context.void_type();
        let function = self.module.add_function(
            "__zeru_panic",
            void.fn_type(&[self.ptr_type().into(), self.usize_type().into()], false),
            Some(Linkage::Internal),
        );
        for attribute in ["noreturn", "cold"] {
            let kind = Attribute::get_named_enum_kind_id(attribute);
            function.add_attribute(
                AttributeLoc::Function,
                self.context.create_enum_attribute(kind, 0),
            );
        }
        let abort = self.module.get_function("abort").unwrap_or_else(|| {
            self.module
                .add_function("abort", void.fn_type(&[], false), Some(Linkage::External))
        });
        self.in_helper(function, |this| {
            let (text, len) = (
                function.get_nth_param(0).unwrap(),
                function.get_nth_param(1).unwrap(),
            );
            if let (Some([_, err]), Some(flush)) = (this.streams, this.flush) {
                this.call_fn(
                    this.program.runtime.write_bytes,
                    &[err.into(), text.into(), len.into()],
                );
                this.builder.build_call(flush, &[], "").unwrap();
            }
            this.builder.build_call(abort, &[], "").unwrap();
            this.builder.build_unreachable().unwrap();
        });
        self.panic = Some(function);
        function
    }

    /// Abort when `condition` holds, then carry on in a fresh block. Every
    /// safety check funnels through here, and none exists in ReleaseFast.
    fn trap_if(&mut self, condition: IntValue<'ctx>, label: &str) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        let what = match label {
            "null" => "null pointer dereference",
            "bounds" => "index out of bounds",
            "div" => "division by zero, or MIN / -1",
            "shift" => "shift by the operand's width or more",
            "overflow" => "arithmetic overflow",
            "unwrap_none" => "unwrap on None",
            "unwrap" => "unwrap on Err",
            "unwrap_err" => "unwrap_err on Ok",
            "float_cast" => "float out of range for the integer type",
            other => other,
        };
        let message = match self.sources.position(self.span) {
            Some((file, line, column)) => format!("panic at {file}:{line}:{column}: {what}\n"),
            None => format!("panic: {what}\n"),
        };
        let panic_bb = self.append(&format!("{label}_panic"));
        let ok_bb = self.append(&format!("{label}_ok"));
        self.builder
            .build_conditional_branch(condition, panic_bb, ok_bb)
            .unwrap();
        let panic = self.panic_fn();
        self.builder.position_at_end(panic_bb);
        let text = self.const_bytes(message.as_bytes());
        let len = self.usize_type().const_int(message.len() as u64, false);
        self.builder
            .build_call(panic, &[text.into(), len.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();
        self.builder.position_at_end(ok_bb);
    }

    pub(super) fn check_null(&mut self, ptr: PointerValue<'ctx>) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        let is_null = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                ptr,
                self.ptr_type().const_null(),
                "is_null",
            )
            .unwrap();
        self.trap_if(is_null, "null");
    }

    /// Trap when the usize `index` falls outside `0..len`.
    pub(super) fn check_bounds(&mut self, index: IntValue<'ctx>, len: IntValue<'ctx>) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        if let (Some(at), Some(len)) = (
            index.get_sign_extended_constant(),
            len.get_zero_extended_constant(),
        ) && (0..len as i64).contains(&at)
        {
            return;
        }
        let out = self
            .builder
            .build_int_compare(IntPredicate::UGE, index, len, "out_of_range")
            .unwrap();
        self.trap_if(out, "bounds");
    }

    /// Trap on a zero divisor, and on `MIN / -1`, which LLVM leaves undefined.
    pub(super) fn check_division(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        signed: bool,
    ) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        let int_type = rhs.get_type();
        let b = &self.builder;
        let mut invalid = b
            .build_int_compare(IntPredicate::EQ, rhs, int_type.const_zero(), "div_zero")
            .unwrap();
        if signed {
            let min = int_type.const_int(1 << (int_type.get_bit_width() - 1), false);
            let lhs_is_min = b
                .build_int_compare(IntPredicate::EQ, lhs, min, "lhs_is_min")
                .unwrap();
            let minus_one = b
                .build_int_compare(
                    IntPredicate::EQ,
                    rhs,
                    int_type.const_all_ones(),
                    "rhs_is_neg_one",
                )
                .unwrap();
            let overflows = b.build_and(lhs_is_min, minus_one, "div_overflow").unwrap();
            invalid = b.build_or(invalid, overflows, "div_invalid").unwrap();
        }
        self.trap_if(invalid, "div");
    }

    pub(super) fn check_shift(&mut self, amount: IntValue<'ctx>) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        let int_type = amount.get_type();
        let width = int_type.const_int(u64::from(int_type.get_bit_width()), false);
        let too_wide = self
            .builder
            .build_int_compare(IntPredicate::UGE, amount, width, "shift_wide")
            .unwrap();
        self.trap_if(too_wide, "shift");
    }

    /// Trap unless `value`, truncated, fits `int_type`. NaN never fits.
    pub(super) fn check_float_to_int(
        &mut self,
        value: FloatValue<'ctx>,
        int_type: IntType<'ctx>,
        unsigned: bool,
    ) {
        if !self.mode.emit_safety_checks() {
            return;
        }
        let float_type = value.get_type();
        let trunc = Intrinsic::find("llvm.trunc")
            .and_then(|i| i.get_declaration(self.module, &[float_type.into()]))
            .expect("llvm.trunc exists");
        let whole = self
            .builder
            .build_call(trunc, &[value.into()], "whole")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_float_value();
        let high = 2f64.powi(int_type.get_bit_width() as i32 - i32::from(!unsigned));
        let low = if unsigned { 0.0 } else { -high };
        let b = &self.builder;
        let below = b
            .build_float_compare(
                FloatPredicate::ULT,
                whole,
                float_type.const_float(low),
                "below",
            )
            .unwrap();
        let above = b
            .build_float_compare(
                FloatPredicate::UGE,
                whole,
                float_type.const_float(high),
                "above",
            )
            .unwrap();
        let outside = b.build_or(below, above, "outside").unwrap();
        self.trap_if(outside, "float_cast");
    }

    /// `+`, `-` and `*` through LLVM's overflow intrinsics, trapping when one
    /// reports an overflow.
    pub(super) fn checked_arith(
        &mut self,
        op: BinOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        signed: bool,
    ) -> BasicValueEnum<'ctx> {
        let name = match (op, signed) {
            (BinOp::Add, true) => "llvm.sadd.with.overflow",
            (BinOp::Add, false) => "llvm.uadd.with.overflow",
            (BinOp::Sub, true) => "llvm.ssub.with.overflow",
            (BinOp::Sub, false) => "llvm.usub.with.overflow",
            (BinOp::Mul, true) => "llvm.smul.with.overflow",
            _ => "llvm.umul.with.overflow",
        };
        let declaration = Intrinsic::find(name)
            .and_then(|i| i.get_declaration(self.module, &[l.get_type().into()]))
            .expect("the overflow intrinsics exist");
        let pair = self
            .builder
            .build_call(declaration, &[l.into(), r.into()], "arith")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_struct_value();
        let overflowed = self
            .builder
            .build_extract_value(pair, 1, "overflowed")
            .unwrap()
            .into_int_value();
        self.trap_if(overflowed, "overflow");
        self.builder.build_extract_value(pair, 0, "value").unwrap()
    }

    pub(super) fn counted_loop(
        &mut self,
        count: IntValue<'ctx>,
        mut body: impl FnMut(&mut Self, IntValue<'ctx>),
    ) {
        let usize_type = self.usize_type();
        let index_slot = self.alloca(Types::USIZE, "i");
        self.builder
            .build_store(index_slot, usize_type.const_zero())
            .unwrap();
        let cond_bb = self.append("count_cond");
        let body_bb = self.append("count_body");
        let after = self.append("count_after");
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let index = self
            .builder
            .build_load(usize_type, index_slot, "i")
            .unwrap()
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, count, "more")
            .unwrap();
        self.builder
            .build_conditional_branch(more, body_bb, after)
            .unwrap();
        self.builder.position_at_end(body_bb);
        body(self, index);
        let next = self
            .builder
            .build_int_add(index, usize_type.const_int(1, false), "next")
            .unwrap();
        self.builder.build_store(index_slot, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(after);
    }

    pub(super) fn buffer_slot(
        &mut self,
        data: PointerValue<'ctx>,
        elem: Ty,
        index: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let elem = self.llvm_ty(elem);
        unsafe {
            self.builder
                .build_gep(elem, data, &[index], "slot")
                .unwrap()
        }
    }

    fn bytes_for(&mut self, elem: Ty, count: IntValue<'ctx>) -> IntValue<'ctx> {
        let size = self.size_of(elem);
        self.builder.build_int_mul(count, size, "bytes").unwrap()
    }

    /// `bytes_for` a buffer about to be allocated, trapping on overflow.
    fn bytes_to_allocate(&mut self, elem: Ty, count: IntValue<'ctx>) -> IntValue<'ctx> {
        let size = self.size_of(elem);
        self.int_arith(BinOp::Mul, count, size, false)
            .into_int_value()
    }

    pub(super) fn alloc_buffer(&mut self, elem: Ty, count: IntValue<'ctx>) -> PointerValue<'ctx> {
        let size = self.bytes_to_allocate(elem, count);
        let heap = self.heap.expect("the heap comes first");
        self.call_fn(self.program.runtime.alloc, &[heap.into(), size.into()])
            .unwrap()
            .into_pointer_value()
    }

    fn resize_buffer(
        &self,
        data: PointerValue<'ctx>,
        old: IntValue<'ctx>,
        new: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let heap = self.heap.expect("the heap comes first");
        self.call_fn(
            self.program.runtime.realloc,
            &[heap.into(), data.into(), old.into(), new.into()],
        )
        .unwrap()
        .into_pointer_value()
    }

    pub(super) fn free_buffer(
        &mut self,
        data: PointerValue<'ctx>,
        elem: Ty,
        capacity: IntValue<'ctx>,
    ) {
        let size = self.bytes_for(elem, capacity);
        self.resize_buffer(data, size, self.usize_type().const_zero());
    }

    /// Copy `count` elements from `src` to `dst`; the two may overlap.
    pub(super) fn move_elements(
        &mut self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        elem: Ty,
        count: IntValue<'ctx>,
    ) {
        let size = self.bytes_for(elem, count);
        self.builder.build_memmove(dst, 1, src, 1, size).unwrap();
    }

    pub(super) fn store_vec(
        &mut self,
        dst: PointerValue<'ctx>,
        ty: Ty,
        data: PointerValue<'ctx>,
        len: IntValue<'ctx>,
        cap: IntValue<'ctx>,
    ) {
        for (at, value) in [
            (0, BasicValueEnum::from(data)),
            (1, len.into()),
            (2, cap.into()),
        ] {
            let field = self.field_ptr(dst, ty, at);
            self.builder.build_store(field, value).unwrap();
        }
    }

    /// Make room for `needed` elements: half as many again plus eight, or
    /// `needed` itself if that is more.
    fn reserve(&mut self, vec: PointerValue<'ctx>, ty: Ty, needed: IntValue<'ctx>) {
        let elem = self.element_ty(ty);
        let usize_type = self.usize_type();
        let cap = self.load_field(vec, ty, 2).into_int_value();
        let short = self
            .builder
            .build_int_compare(IntPredicate::UGT, needed, cap, "short")
            .unwrap();
        self.if_then(short, |this| {
            let b = &this.builder;
            let half = b
                .build_int_unsigned_div(cap, usize_type.const_int(2, false), "half")
                .unwrap();
            let grown = b.build_int_add(cap, half, "grown").unwrap();
            let grown = b
                .build_int_add(grown, usize_type.const_int(8, false), "grown")
                .unwrap();
            let enough = b
                .build_int_compare(IntPredicate::UGT, needed, grown, "enough")
                .unwrap();
            let new_cap = b
                .build_select(enough, needed, grown, "new_cap")
                .unwrap()
                .into_int_value();
            let data = this.load_field(vec, ty, 0).into_pointer_value();
            let old_size = this.bytes_for(elem, cap);
            let new_size = this.bytes_to_allocate(elem, new_cap);
            let resized = this.resize_buffer(data, old_size, new_size);
            let ptr_field = this.field_ptr(vec, ty, 0);
            this.builder.build_store(ptr_field, resized).unwrap();
            let cap_field = this.field_ptr(vec, ty, 2);
            this.builder.build_store(cap_field, new_cap).unwrap();
        });
    }

    /// The Vec a builtin's first argument, `AddressOf` it, points at.
    fn receiver(&mut self, args: &'a [Expr]) -> (PointerValue<'ctx>, Ty) {
        let receiver = &args[0];
        let ty = match self.types().kind(receiver.ty) {
            TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
            _ => receiver.ty,
        };
        (self.expr(receiver).ptr(), ty)
    }

    pub(super) fn builtin(&mut self, builtin: Builtin, args: &'a [Expr], ty: Ty) -> Val<'ctx> {
        let usize_type = self.usize_type();
        let one = usize_type.const_int(1, false);
        match builtin {
            Builtin::Flush => {
                let flush = self.flush.expect("the prelude declares the streams");
                self.builder.build_call(flush, &[], "").unwrap();
                Val::Unit
            }
            Builtin::CopyBytes => {
                let to = self.imm(&args[0]).into_pointer_value();
                let from = self.imm(&args[1]).into_pointer_value();
                let count = self.imm(&args[2]).into_int_value();
                self.builder.build_memmove(to, 1, from, 1, count).unwrap();
                Val::Unit
            }
            Builtin::VecNew | Builtin::VecWithCapacity => {
                let elem = self.element_ty(ty);
                let (data, cap) = match args.first() {
                    Some(cap) => {
                        let cap = self.imm(cap).into_int_value();
                        (self.alloc_buffer(elem, cap), cap)
                    }
                    None => (self.ptr_type().const_null(), usize_type.const_zero()),
                };
                let slot = self.alloca(ty, "vec");
                self.store_vec(slot, ty, data, usize_type.const_zero(), cap);
                Val::Mem(slot)
            }
            Builtin::SliceLen => {
                let slice_ty = args[0].ty;
                let slice = self.expr(&args[0]).ptr();
                Val::Imm(self.load_field(slice, slice_ty, 1))
            }
            Builtin::IsSome | Builtin::IsOk | Builtin::IsNone | Builtin::IsErr => {
                let held_ty = args[0].ty;
                let held = self.expr(&args[0]).ptr();
                let tag = self.load_field(held, held_ty, 0).into_int_value();
                Val::Imm(match builtin {
                    Builtin::IsNone | Builtin::IsErr => {
                        self.builder.build_not(tag, "not").unwrap().into()
                    }
                    _ => tag.into(),
                })
            }
            Builtin::Unwrap | Builtin::UnwrapErr => {
                let held_ty = args[0].ty;
                let held = self.expr(&args[0]).ptr();
                let tag = self.load_field(held, held_ty, 0).into_int_value();
                let (failed, label, at) = match (builtin, self.types().kind(held_ty)) {
                    (Builtin::UnwrapErr, _) => (tag, "unwrap_err", 2),
                    (_, TyKind::Optional(_)) => (
                        self.builder.build_not(tag, "empty").unwrap(),
                        "unwrap_none",
                        1,
                    ),
                    _ => (self.builder.build_not(tag, "is_err").unwrap(), "unwrap", 1),
                };
                self.trap_if(failed, label);
                let payload = self.field_ptr(held, held_ty, at);
                self.load_val(payload, ty)
            }
            _ => self.vec_method(builtin, args, ty, one),
        }
    }

    fn vec_method(
        &mut self,
        builtin: Builtin,
        args: &'a [Expr],
        ty: Ty,
        one: IntValue<'ctx>,
    ) -> Val<'ctx> {
        let (vec, vec_ty) = self.receiver(args);
        let elem = self.element_ty(vec_ty);
        let usize_type = self.usize_type();
        let len = |this: &mut Self| this.load_field(vec, vec_ty, 1).into_int_value();
        let set_len = |this: &mut Self, value: IntValue<'ctx>| {
            let field = this.field_ptr(vec, vec_ty, 1);
            this.builder.build_store(field, value).unwrap();
        };
        let data = |this: &mut Self| this.load_field(vec, vec_ty, 0).into_pointer_value();
        match builtin {
            Builtin::VecLen => Val::Imm(len(self).into()),
            Builtin::VecCapacity => Val::Imm(self.load_field(vec, vec_ty, 2)),
            Builtin::VecIsEmpty => {
                let len = len(self);
                let empty = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, len, usize_type.const_zero(), "is_empty")
                    .unwrap();
                Val::Imm(empty.into())
            }
            Builtin::VecPush | Builtin::VecInsert => {
                let at = (builtin == Builtin::VecInsert).then(|| {
                    let signed = self.is_signed(args[1].ty);
                    let index = self.imm(&args[1]).into_int_value();
                    self.widen(index, signed)
                });
                let item = args.last().unwrap();
                let staged = self.alloca(elem, "item");
                self.write(item, staged);
                let current = len(self);
                let grown = self.builder.build_int_add(current, one, "grown").unwrap();
                let at = match at {
                    Some(at) => {
                        self.check_bounds(at, grown);
                        at
                    }
                    None => current,
                };
                self.reserve(vec, vec_ty, grown);
                let data = data(self);
                let slot = self.buffer_slot(data, elem, at);
                if builtin == Builtin::VecInsert {
                    let next = self.builder.build_int_add(at, one, "next").unwrap();
                    let after = self.buffer_slot(data, elem, next);
                    let tail = self.builder.build_int_sub(current, at, "tail").unwrap();
                    self.move_elements(after, slot, elem, tail);
                }
                self.copy_bits(slot, staged, elem);
                set_len(self, grown);
                Val::Unit
            }
            Builtin::VecRemove => {
                let signed = self.is_signed(args[1].ty);
                let at = self.imm(&args[1]).into_int_value();
                let at = self.widen(at, signed);
                let current = len(self);
                self.check_bounds(at, current);
                let data = data(self);
                let slot = self.buffer_slot(data, elem, at);
                let removed = self.alloca(elem, "removed");
                self.copy_bits(removed, slot, elem);
                let next = self.builder.build_int_add(at, one, "next").unwrap();
                let after = self.buffer_slot(data, elem, next);
                let tail = self.builder.build_int_sub(current, next, "tail").unwrap();
                self.move_elements(slot, after, elem, tail);
                let shrunk = self.builder.build_int_sub(current, one, "shrunk").unwrap();
                set_len(self, shrunk);
                self.load_val(removed, elem)
            }
            Builtin::VecPop | Builtin::VecGet => {
                let current = len(self);
                let (present, index) = match builtin {
                    Builtin::VecPop => {
                        let present = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                current,
                                usize_type.const_zero(),
                                "has_elem",
                            )
                            .unwrap();
                        (present, None)
                    }
                    _ => {
                        let signed = self.is_signed(args[1].ty);
                        let index = self.imm(&args[1]).into_int_value();
                        let index = self.widen(index, signed);
                        let present = self
                            .builder
                            .build_int_compare(IntPredicate::ULT, index, current, "in_bounds")
                            .unwrap();
                        (present, Some(index))
                    }
                };
                let result = self.alloca(ty, "elem");
                let tag = self.field_ptr(result, ty, 0);
                self.builder
                    .build_store(tag, self.context.bool_type().const_zero())
                    .unwrap();
                self.if_then(present, |this| {
                    let index = index.unwrap_or_else(|| {
                        let last = this.builder.build_int_sub(current, one, "new_len").unwrap();
                        set_len(this, last);
                        last
                    });
                    let data = data(this);
                    let slot = this.buffer_slot(data, elem, index);
                    let payload = this.field_ptr(result, ty, 1);
                    match builtin {
                        Builtin::VecGet => {
                            this.builder.build_store(payload, slot).unwrap();
                        }
                        _ => this.copy_bits(payload, slot, elem),
                    }
                    let tag = this.field_ptr(result, ty, 0);
                    this.builder
                        .build_store(tag, this.context.bool_type().const_int(1, false))
                        .unwrap();
                });
                Val::Mem(result)
            }
            Builtin::VecClear => {
                if self.program.needs_drop(elem) {
                    let current = len(self);
                    let data = data(self);
                    self.counted_loop(current, |this, at| {
                        let item = this.buffer_slot(data, elem, at);
                        this.call_drop(item, elem);
                    });
                }
                set_len(self, usize_type.const_zero());
                Val::Unit
            }
            Builtin::VecReserve => {
                let more = self.imm(&args[1]).into_int_value();
                let current = len(self);
                let needed = self.builder.build_int_add(current, more, "needed").unwrap();
                self.reserve(vec, vec_ty, needed);
                Val::Unit
            }
            Builtin::VecShrinkToFit => {
                let current = len(self);
                let cap = self.load_field(vec, vec_ty, 2).into_int_value();
                let data = data(self);
                let old = self.bytes_for(elem, cap);
                let new = self.bytes_for(elem, current);
                let resized = self.resize_buffer(data, old, new);
                let ptr_field = self.field_ptr(vec, vec_ty, 0);
                self.builder.build_store(ptr_field, resized).unwrap();
                let cap_field = self.field_ptr(vec, vec_ty, 2);
                self.builder.build_store(cap_field, current).unwrap();
                Val::Unit
            }
            _ => unreachable!("{builtin:?} is not a Vec method"),
        }
    }

    /// The stdout and stderr `OutStream` globals, empty with their fd set,
    /// and the program's `Heap`, empty.
    pub(super) fn init_globals(&mut self) {
        let stream_ty = self.program.runtime.out_stream;
        let shape = self.struct_type(stream_ty);
        let streams = [("__stdout_stream", 1), ("__stderr_stream", 2)].map(|(name, fd)| {
            let fields: Vec<BasicValueEnum> = shape
                .get_field_types()
                .iter()
                .enumerate()
                .map(|(at, field)| match at {
                    0 => self.context.i32_type().const_int(fd, false).into(),
                    _ => field.const_zero(),
                })
                .collect();
            let global = self
                .module
                .add_global(shape, Some(AddressSpace::default()), name);
            global.set_initializer(&shape.const_named_struct(&fields));
            global.as_pointer_value()
        });
        self.streams = Some(streams);
        let heap = self.struct_type(self.program.runtime.heap);
        let global = self
            .module
            .add_global(heap, Some(AddressSpace::default()), "__zeru_heap");
        global.set_initializer(&heap.const_zero());
        self.heap = Some(global.as_pointer_value());
    }

    /// `__zeru_flush`, which writes out what both streams hold: run as the
    /// program ends, by `exit`, and before a panic aborts.
    pub(super) fn create_flush(&mut self) {
        let streams = self.streams.expect("streams come first");
        let function = self.module.add_function(
            FLUSH_FN,
            self.context.void_type().fn_type(&[], false),
            Some(Linkage::Internal),
        );
        self.flush = Some(function);
        self.in_helper(function, |this| {
            for stream in streams {
                this.call_fn(this.program.runtime.flush, &[stream.into()]);
            }
            this.builder.build_return(None).unwrap();
        });
        let i32_type = self.context.i32_type();
        let ptr_type = self.ptr_type();
        let entry_type = self
            .context
            .struct_type(&[i32_type.into(), ptr_type.into(), ptr_type.into()], false);
        let entry = entry_type.const_named_struct(&[
            i32_type.const_int(65535, false).into(),
            function.as_global_value().as_pointer_value().into(),
            ptr_type.const_null().into(),
        ]);
        let entries = entry_type.const_array(&[entry]);
        let global = self.module.add_global(
            entries.get_type(),
            Some(AddressSpace::default()),
            "llvm.global_dtors",
        );
        global.set_linkage(Linkage::Appending);
        global.set_initializer(&entries);
    }

    /// The format's text around each `{}` is written as it is, each value by
    /// the `OutStream` method for its kind.
    pub(super) fn print(
        &mut self,
        stderr: bool,
        newline: bool,
        pieces: &'a [Vec<u8>],
        values: &'a [Expr],
    ) {
        let [out, err] = self.streams.expect("the prelude declares the streams");
        let stream = if stderr { err } else { out };
        let runtime = &self.program.runtime;
        let (write_bytes, flush) = (runtime.write_bytes, runtime.flush);
        let lowered: Vec<Val<'ctx>> = values.iter().map(|value| self.snapshot(value)).collect();
        for (at, piece) in pieces.iter().enumerate() {
            if !piece.is_empty() {
                let text = self.const_bytes(piece);
                let len = self.usize_type().const_int(piece.len() as u64, false);
                self.call_fn(write_bytes, &[stream.into(), text.into(), len.into()]);
            }
            if let (Some(value), Some(&val)) = (values.get(at), lowered.get(at)) {
                self.print_value(stream, value.ty, val);
            }
        }
        if newline {
            let text = self.const_bytes(b"\n");
            let one = self.usize_type().const_int(1, false);
            self.call_fn(write_bytes, &[stream.into(), text.into(), one.into()]);
            self.call_fn(flush, &[stream.into()]);
        }
    }

    /// The value of `value` as it is now, kept apart from the place it was
    /// read from, so what later arguments do cannot change it.
    fn snapshot(&mut self, value: &'a Expr) -> Val<'ctx> {
        match self.expr(value) {
            Val::Mem(src) => {
                let slot = self.alloca(value.ty, "printed");
                self.copy_bits(slot, src, value.ty);
                Val::Mem(slot)
            }
            other => other,
        }
    }

    fn print_value(&mut self, stream: PointerValue<'ctx>, ty: Ty, lowered: Val<'ctx>) {
        let runtime = &self.program.runtime;
        let (write_bytes, write_int, write_uint, write_float) = (
            runtime.write_bytes,
            runtime.write_int,
            runtime.write_uint,
            runtime.write_float,
        );
        let i64_type = self.context.i64_type();
        match (self.types().kind(ty), lowered) {
            (TyKind::Bool, Val::Imm(flag)) => {
                let flag = flag.into_int_value();
                let (yes, no) = (self.const_bytes(b"true"), self.const_bytes(b"false"));
                let text = self
                    .builder
                    .build_select(flag, yes, no, "bool_text")
                    .unwrap();
                let len = self
                    .builder
                    .build_select(
                        flag,
                        i64_type.const_int(4, false),
                        i64_type.const_int(5, false),
                        "bool_len",
                    )
                    .unwrap();
                self.call_fn(write_bytes, &[stream.into(), text.into(), len.into()]);
            }
            (TyKind::Int(int), Val::Imm(number)) => {
                let signed = int.signed();
                let wide = self
                    .builder
                    .build_int_cast_sign_flag(number.into_int_value(), i64_type, signed, "wide")
                    .unwrap();
                let method = if signed { write_int } else { write_uint };
                self.call_fn(method, &[stream.into(), wide.into()]);
            }
            (TyKind::F32 | TyKind::F64, Val::Imm(number)) => {
                let wide = self
                    .builder
                    .build_float_ext(number.into_float_value(), self.context.f64_type(), "wide")
                    .unwrap();
                let single = self
                    .context
                    .bool_type()
                    .const_int(u64::from(ty == Types::F32), false);
                self.call_fn(write_float, &[stream.into(), wide.into(), single.into()]);
            }
            (_, Val::Mem(text)) => {
                let ptr = self.load_field(text, ty, 0);
                let len = self.load_field(text, ty, 1);
                self.call_fn(write_bytes, &[stream.into(), ptr.into(), len.into()]);
            }
            _ => {}
        }
    }

    pub(super) fn asm(
        &mut self,
        template: &str,
        constraints: &str,
        outputs: &'a [Place],
        inputs: &'a [Expr],
        volatile: bool,
        ty: Ty,
    ) -> Val<'ctx> {
        let inputs: Vec<BasicValueEnum> = inputs.iter().map(|input| self.imm(input)).collect();
        let output_tys: Vec<BasicTypeEnum> = outputs
            .iter()
            .map(|place| {
                let ty = self.place_ty(place);
                self.llvm_ty(ty)
            })
            .collect();
        let word = self.usize_type().as_basic_type_enum();
        let output_type = match output_tys.as_slice() {
            [] => word,
            [single] => *single,
            many => self.context.struct_type(many, false).into(),
        };
        let params: Vec<_> = inputs.iter().map(|v| v.get_type().into()).collect();
        let fn_type = output_type.fn_type(&params, false);
        let asm = self.context.create_inline_asm(
            fn_type,
            template.to_string(),
            constraints.to_string(),
            volatile,
            false,
            None,
            false,
        );
        let args: Vec<BasicMetadataValueEnum> = inputs.iter().map(|v| (*v).into()).collect();
        let result = self
            .builder
            .build_indirect_call(fn_type, asm, &args, "asm")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap();
        for (at, place) in outputs.iter().enumerate() {
            let target = self.place(place);
            let value = match outputs.len() {
                1 => result,
                _ => self
                    .builder
                    .build_extract_value(result.into_struct_value(), at as u32, "asm_out")
                    .unwrap(),
            };
            self.builder.build_store(target, value).unwrap();
        }
        let llvm = self.llvm_ty(ty);
        if result.get_type() == llvm {
            return Val::Imm(result);
        }
        Val::Imm(llvm.const_zero())
    }
}
