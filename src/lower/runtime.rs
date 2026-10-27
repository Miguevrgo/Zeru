//! What the generated code does on its own: safety checks and the panic they
//! end in, the Vec builtins, buffers, printing, the output streams and their
//! flush, and inline assembly.

use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    intrinsics::Intrinsic,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, IntType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
        ValueKind,
    },
};

use std::collections::HashSet;
use std::sync::LazyLock;

use crate::mir::{Asm, Operand, Place};
use crate::tir::{BinOp, BitOp, FnId};
use crate::ty::{Ty, TyKind, Types};

use super::{Lower, Val};

/// The entry point and the functions LLVM calls by name, in assembly.
const START: &str = include_str!("start.s");

/// Whether start.s, or the `__zeru_start` it calls, defines `symbol`, which
/// a function of the program then cannot take.
pub(super) fn runtime_symbol(symbol: &str) -> bool {
    static LABELS: LazyLock<HashSet<&str>> = LazyLock::new(|| {
        START
            .lines()
            .filter_map(|line| line.strip_suffix(':'))
            .collect()
    });
    symbol == "__zeru_start" || LABELS.contains(symbol)
}

impl<'a, 'ctx> Lower<'a, 'ctx> {
    /// A read-only global holding `bytes` and a terminating NUL, which a
    /// syscall taking a C string needs; a `\0` inside is kept as data.
    pub(super) fn const_bytes(&self, bytes: &[u8]) -> PointerValue<'ctx> {
        self.read_only(self.context.const_string(bytes, true).into(), "str")
    }

    /// A private global holding `value`, which nothing writes.
    pub(super) fn read_only(&self, value: BasicValueEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let global = self.module.add_global(value.get_type(), None, name);
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

    /// A block that panics with the message `what` names, at the span
    /// being lowered.
    pub(super) fn panic_block(&mut self, what: &str) -> BasicBlock<'ctx> {
        let message = match self.sources.position(self.span) {
            Some((file, line, column)) => format!("panic at {file}:{line}:{column}: {what}\n"),
            None => format!("panic: {what}\n"),
        };
        self.abort_block(&message)
    }

    /// A block that writes `message` to stderr and aborts.
    pub(super) fn abort_block(&mut self, message: &str) -> BasicBlock<'ctx> {
        let here = self.builder.get_insert_block().expect("inside a block");
        let panic_bb = self.append("panic");
        let panic = self.panic_fn();
        self.builder.position_at_end(panic_bb);
        let text = self.const_bytes(message.as_bytes());
        let len = self.usize_type().const_int(message.len() as u64, false);
        self.builder
            .build_call(panic, &[text.into(), len.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();
        self.builder.position_at_end(here);
        panic_bb
    }

    /// A call to the LLVM intrinsic `name` at type `ty`.
    fn intrinsic(
        &self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let declaration = Intrinsic::find(name)
            .and_then(|i| i.get_declaration(self.module, &[ty]))
            .expect("the intrinsics used exist");
        self.builder
            .build_call(declaration, args, name)
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
    }

    /// Panic when `condition` holds, then carry on in a fresh block.
    fn trap_if(&mut self, condition: IntValue<'ctx>, what: &str) {
        let panic_bb = self.panic_block(what);
        let ok_bb = self.append("ok");
        self.builder
            .build_conditional_branch(condition, panic_bb, ok_bb)
            .unwrap();
        self.builder.position_at_end(ok_bb);
    }

    /// Trap on a zero divisor, and on `MIN / -1`, which LLVM leaves undefined.
    pub(super) fn check_division(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        signed: bool,
    ) {
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
        self.trap_if(invalid, "division by zero, or MIN / -1");
    }

    pub(super) fn check_shift(&mut self, amount: IntValue<'ctx>) {
        let int_type = amount.get_type();
        let width = int_type.const_int(u64::from(int_type.get_bit_width()), false);
        let too_wide = self
            .builder
            .build_int_compare(IntPredicate::UGE, amount, width, "shift_wide")
            .unwrap();
        self.trap_if(too_wide, "shift by the operand's width or more");
    }

    /// Trap unless `value`, truncated, fits `int_type`. NaN never fits.
    pub(super) fn check_float_to_int(
        &mut self,
        value: FloatValue<'ctx>,
        int_type: IntType<'ctx>,
        unsigned: bool,
    ) {
        let float_type = value.get_type();
        let whole = self
            .intrinsic("llvm.trunc", float_type.into(), &[value.into()])
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
        self.trap_if(outside, "float out of range for the integer type");
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
        let pair = self
            .intrinsic(name, l.get_type().into(), &[l.into(), r.into()])
            .into_struct_value();
        let overflowed = self
            .builder
            .build_extract_value(pair, 1, "overflowed")
            .unwrap()
            .into_int_value();
        self.trap_if(overflowed, "arithmetic overflow");
        self.builder.build_extract_value(pair, 0, "value").unwrap()
    }

    /// What `op` makes of the integer `value`: a count of its bits, as a
    /// usize, or the integer with its bits rotated by `amount` or its bytes
    /// in the opposite order.
    pub(super) fn bits(
        &mut self,
        op: BitOp,
        value: &Operand,
        amount: Option<&Operand>,
    ) -> BasicValueEnum<'ctx> {
        let value = self.operand(value).imm().into_int_value();
        let int_type = value.get_type();
        let mut args: Vec<BasicMetadataValueEnum> = vec![value.into()];
        let name = match op {
            BitOp::CountOnes => "llvm.ctpop",
            BitOp::LeadingZeros => "llvm.ctlz",
            BitOp::TrailingZeros => "llvm.cttz",
            BitOp::RotateLeft => "llvm.fshl",
            BitOp::RotateRight => "llvm.fshr",
            BitOp::SwapBytes if int_type.get_bit_width() == 8 => return value.into(),
            BitOp::SwapBytes => "llvm.bswap",
        };
        match op {
            BitOp::LeadingZeros | BitOp::TrailingZeros => {
                args.push(self.context.bool_type().const_zero().into());
            }
            BitOp::RotateLeft | BitOp::RotateRight => {
                let amount = self.operand(amount.expect("a rotation has an amount"));
                let amount = self
                    .builder
                    .build_int_cast_sign_flag(
                        amount.imm().into_int_value(),
                        int_type,
                        false,
                        "amount",
                    )
                    .unwrap();
                args.extend::<[BasicMetadataValueEnum; 2]>([value.into(), amount.into()]);
            }
            _ => {}
        }
        let result = self
            .intrinsic(name, int_type.into(), &args)
            .into_int_value();
        match op {
            BitOp::CountOnes | BitOp::LeadingZeros | BitOp::TrailingZeros => self
                .builder
                .build_int_cast_sign_flag(result, self.usize_type(), false, "count")
                .unwrap()
                .into(),
            _ => result.into(),
        }
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

    /// `bytes_for` a buffer about to be allocated, trapping on overflow in a
    /// build with checks.
    fn bytes_to_allocate(&mut self, elem: Ty, count: IntValue<'ctx>) -> IntValue<'ctx> {
        let size = self.size_of(elem);
        let checked = self.mode.emit_safety_checks();
        self.int_arith(BinOp::Mul, count, size, false, checked)
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

    /// The Vec at `place`, with its type and element type.
    fn vec_at(&mut self, place: &Place) -> (PointerValue<'ctx>, Ty, Ty) {
        let ty = self.place_ty(place);
        (self.place(place), ty, self.element_ty(ty))
    }

    /// `push`, or `insert` at `at`, the bounds already checked.
    pub(super) fn vec_insert(&mut self, place: &Place, at: Option<&Operand>, item: &Operand) {
        let at = at.map(|at| self.operand(at).imm().into_int_value());
        let staged = self.operand(item);
        let (vec, ty, elem) = self.vec_at(place);
        let one = self.usize_type().const_int(1, false);
        let current = self.load_field(vec, ty, 1).into_int_value();
        let grown = self.builder.build_int_add(current, one, "grown").unwrap();
        self.reserve(vec, ty, grown);
        let data = self.load_field(vec, ty, 0).into_pointer_value();
        let slot = self.buffer_slot(data, elem, at.unwrap_or(current));
        if let Some(at) = at {
            let next = self.builder.build_int_add(at, one, "next").unwrap();
            let after = self.buffer_slot(data, elem, next);
            let tail = self.builder.build_int_sub(current, at, "tail").unwrap();
            self.move_elements(after, slot, elem, tail);
        }
        self.store_val(slot, staged, elem);
        let len_field = self.field_ptr(vec, ty, 1);
        self.builder.build_store(len_field, grown).unwrap();
    }

    /// `remove(at)`, the bounds already checked.
    pub(super) fn vec_remove(&mut self, place: &Place, at: &Operand) -> Val<'ctx> {
        let at = self.operand(at).imm().into_int_value();
        let (vec, ty, elem) = self.vec_at(place);
        let one = self.usize_type().const_int(1, false);
        let current = self.load_field(vec, ty, 1).into_int_value();
        let data = self.load_field(vec, ty, 0).into_pointer_value();
        let slot = self.buffer_slot(data, elem, at);
        let removed = self.alloca(elem, "removed");
        self.copy_bits(removed, slot, elem);
        let next = self.builder.build_int_add(at, one, "next").unwrap();
        let after = self.buffer_slot(data, elem, next);
        let tail = self.builder.build_int_sub(current, next, "tail").unwrap();
        self.move_elements(slot, after, elem, tail);
        let shrunk = self.builder.build_int_sub(current, one, "shrunk").unwrap();
        let len_field = self.field_ptr(vec, ty, 1);
        self.builder.build_store(len_field, shrunk).unwrap();
        self.load_val(removed, elem)
    }

    /// `pop()`, or with `at`, `get(at)`, into the `T?` or `(&T)?` at `dst`.
    pub(super) fn vec_take(
        &mut self,
        place: &Place,
        at: Option<&Operand>,
        dst: PointerValue<'ctx>,
        ty: Ty,
    ) {
        let index = at.map(|at| self.operand(at).imm().into_int_value());
        let (vec, vec_ty, elem) = self.vec_at(place);
        let usize_type = self.usize_type();
        let one = usize_type.const_int(1, false);
        let current = self.load_field(vec, vec_ty, 1).into_int_value();
        let present = match index {
            Some(index) => self
                .builder
                .build_int_compare(IntPredicate::ULT, index, current, "in_bounds")
                .unwrap(),
            None => self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    current,
                    usize_type.const_zero(),
                    "has_elem",
                )
                .unwrap(),
        };
        self.store_tag(dst, ty, false);
        self.if_then(present, |this| {
            let index = index.unwrap_or_else(|| {
                let last = this.builder.build_int_sub(current, one, "new_len").unwrap();
                let len_field = this.field_ptr(vec, vec_ty, 1);
                this.builder.build_store(len_field, last).unwrap();
                last
            });
            let data = this.load_field(vec, vec_ty, 0).into_pointer_value();
            let slot = this.buffer_slot(data, elem, index);
            let payload = this.field_ptr(dst, ty, 1);
            match at {
                Some(_) => {
                    this.builder.build_store(payload, slot).unwrap();
                }
                None => this.copy_bits(payload, slot, elem),
            }
            this.store_tag(dst, ty, true);
        });
    }

    pub(super) fn vec_clear(&mut self, place: &Place) {
        let (vec, ty, elem) = self.vec_at(place);
        if self.program.needs_drop(elem) {
            let current = self.load_field(vec, ty, 1).into_int_value();
            let data = self.load_field(vec, ty, 0).into_pointer_value();
            self.counted_loop(current, |this, at| {
                let item = this.buffer_slot(data, elem, at);
                this.call_drop(item, elem);
            });
        }
        let len_field = self.field_ptr(vec, ty, 1);
        self.builder
            .build_store(len_field, self.usize_type().const_zero())
            .unwrap();
    }

    pub(super) fn vec_reserve(&mut self, place: &Place, more: &Operand) {
        let more = self.operand(more).imm().into_int_value();
        let (vec, ty, _) = self.vec_at(place);
        let current = self.load_field(vec, ty, 1).into_int_value();
        let needed = self.builder.build_int_add(current, more, "needed").unwrap();
        self.reserve(vec, ty, needed);
    }

    pub(super) fn vec_shrink(&mut self, place: &Place) {
        let (vec, ty, elem) = self.vec_at(place);
        let current = self.load_field(vec, ty, 1).into_int_value();
        let cap = self.load_field(vec, ty, 2).into_int_value();
        let data = self.load_field(vec, ty, 0).into_pointer_value();
        let old = self.bytes_for(elem, cap);
        let new = self.bytes_for(elem, current);
        let resized = self.resize_buffer(data, old, new);
        self.store_vec(vec, ty, resized, current, current);
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

    /// `__zeru_flush`, which writes out what both streams hold: run as
    /// `main` returns, by `exit`, and before a panic aborts.
    pub(super) fn create_flush(&mut self) {
        let streams = self.streams.expect("streams come first");
        let function = self.module.add_function(
            crate::tir::FLUSH_FN,
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
    }

    /// `__zeru_start`, which `_start` in start.s calls with no libc under it:
    /// `main`, or for `zeru test` the test its first argument numbers, then
    /// the flush, giving back the exit code 0.
    pub(super) fn create_start(&mut self) {
        self.module.set_inline_assembly(START);
        let main = self.bodies.iter().position(|body| body.symbol == "main");
        let flush = self.flush.expect("the flush comes first");
        let i32_type = self.context.i32_type();
        let function = self
            .module
            .add_function("__zeru_start", i32_type.fn_type(&[], false), None);
        self.in_helper(function, |this| {
            match main {
                _ if !this.program.tests.is_empty() => {
                    let index = this.call_fn(this.program.runtime.test_index, &[]);
                    let index = index.expect("a number").into_int_value();
                    let done = this.context.append_basic_block(function, "done");
                    let cases: Vec<_> = this
                        .program
                        .tests
                        .iter()
                        .enumerate()
                        .map(|(at, &test)| {
                            let block = this.context.append_basic_block(function, "test");
                            this.builder.position_at_end(block);
                            this.call_fn(test, &[]);
                            this.builder.build_unconditional_branch(done).unwrap();
                            (index.get_type().const_int(at as u64, false), block)
                        })
                        .collect();
                    let entry = function.get_first_basic_block().expect("an entry");
                    this.builder.position_at_end(entry);
                    this.builder.build_switch(index, done, &cases).unwrap();
                    this.builder.position_at_end(done);
                }
                Some(main) => _ = this.call_fn(FnId(main as u32), &[]),
                None => {}
            }
            this.builder.build_call(flush, &[], "").unwrap();
            this.builder
                .build_return(Some(&i32_type.const_zero()))
                .unwrap();
        });
    }

    /// The format's text around each `{}` is written as it is, each value by
    /// the `OutStream` method for its kind.
    pub(super) fn print(
        &mut self,
        stderr: bool,
        newline: bool,
        pieces: &[Vec<u8>],
        values: &[Operand],
    ) {
        let [out, err] = self.streams.expect("the prelude declares the streams");
        let stream = if stderr { err } else { out };
        let runtime = &self.program.runtime;
        let (write_bytes, flush) = (runtime.write_bytes, runtime.flush);
        for (at, piece) in pieces.iter().enumerate() {
            if !piece.is_empty() {
                let text = self.const_bytes(piece);
                let len = self.usize_type().const_int(piece.len() as u64, false);
                self.call_fn(write_bytes, &[stream.into(), text.into(), len.into()]);
            }
            if let Some(value) = values.get(at) {
                let ty = self.operand_ty(value);
                let lowered = self.operand(value);
                self.print_value(stream, ty, lowered);
            }
        }
        if newline {
            let text = self.const_bytes(b"\n");
            let one = self.usize_type().const_int(1, false);
            self.call_fn(write_bytes, &[stream.into(), text.into(), one.into()]);
            self.call_fn(flush, &[stream.into()]);
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

    pub(super) fn asm(&mut self, asm: &'a Asm) {
        let Asm {
            template,
            constraints,
            outputs,
            inputs,
            volatile,
        } = asm;
        let inputs: Vec<BasicValueEnum> = inputs
            .iter()
            .map(|input| self.operand(input).imm())
            .collect();
        let output_tys: Vec<BasicTypeEnum> = outputs
            .iter()
            .map(|place| {
                let ty = self.place_ty(place);
                self.llvm_ty(ty)
            })
            .collect();
        let params: Vec<_> = inputs.iter().map(|v| v.get_type().into()).collect();
        let fn_type = match output_tys.as_slice() {
            [] => self.context.void_type().fn_type(&params, false),
            [single] => single.fn_type(&params, false),
            many => self
                .context
                .struct_type(many, false)
                .fn_type(&params, false),
        };
        let asm_fn = self.context.create_inline_asm(
            fn_type,
            template.clone(),
            constraints.clone(),
            *volatile,
            false,
            None,
            false,
        );
        let args: Vec<BasicMetadataValueEnum> = inputs.iter().map(|v| (*v).into()).collect();
        let call = self
            .builder
            .build_indirect_call(fn_type, asm_fn, &args, "asm")
            .unwrap();
        let Some(result) = call.try_as_basic_value().basic() else {
            return;
        };
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
    }
}
