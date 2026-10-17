//! Expressions and places.

use inkwell::{
    FloatPredicate, IntPredicate,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue, ValueKind},
};

use crate::tir::{Arm, BinOp, Expr, ExprKind, FnId, Payload, Place, UnOp};
use crate::ty::{Ty, TyKind, Types};

use super::{Lower, Val};

impl<'a, 'ctx> Lower<'a, 'ctx> {
    pub(super) fn imm(&mut self, expr: &'a Expr) -> BasicValueEnum<'ctx> {
        self.expr(expr).imm()
    }

    /// Whether `expr` is built in place, so it can be written straight into
    /// the memory meant to hold it.
    fn builds_in_place(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Tuple(_)
            | ExprKind::Struct(_)
            | ExprKind::Array(_)
            | ExprKind::ArrayRepeat(..)
            | ExprKind::VecLiteral(_)
            | ExprKind::VecRepeat(..)
            | ExprKind::Some(_)
            | ExprKind::None
            | ExprKind::Ok(_)
            | ExprKind::Err(_)
            | ExprKind::Str(_) => true,
            ExprKind::Variant(..) | ExprKind::Call(..) => {
                expr.ty != Types::VOID && !self.is_scalar(expr.ty)
            }
            _ => false,
        }
    }

    /// Lower `expr`, a check in it reporting where `expr` starts, as rustc
    /// gives each check the span of its own operation.
    pub(super) fn expr(&mut self, expr: &'a Expr) -> Val<'ctx> {
        let outer = std::mem::replace(&mut self.span, expr.span);
        let value = self.value(expr);
        self.span = outer;
        value
    }

    fn value(&mut self, expr: &'a Expr) -> Val<'ctx> {
        if self.builds_in_place(expr) {
            let slot = self.alloca(expr.ty, "value");
            self.build_into(expr, slot);
            return Val::Mem(slot);
        }
        match &expr.kind {
            ExprKind::Int(value) => {
                let llvm = self.llvm_ty(expr.ty);
                Val::Imm(match llvm {
                    BasicTypeEnum::FloatType(t) => t.const_float(*value as f64).into(),
                    _ => Self::int_const(llvm.into_int_type(), *value).into(),
                })
            }
            ExprKind::Float(value) => Val::Imm(
                self.llvm_ty(expr.ty)
                    .into_float_type()
                    .const_float(*value)
                    .into(),
            ),
            ExprKind::Bool(value) => Val::Imm(
                self.context
                    .bool_type()
                    .const_int(u64::from(*value), false)
                    .into(),
            ),
            ExprKind::Void => Val::Unit,
            ExprKind::Read(place) => self.read(place),
            ExprKind::Move(place) => {
                let value = self.read(place);
                if let Place::Local(local) = place
                    && let Some(flag) = self.flags[local.0 as usize]
                {
                    let lowered = self.context.bool_type().const_zero();
                    self.builder.build_store(flag, lowered).unwrap();
                }
                value
            }
            ExprKind::Copy(place) => {
                let ty = self.place_ty(place);
                let source = self.place(place);
                let slot = self.alloca(ty, "copy");
                if self.program.needs_drop(ty) {
                    self.call_copy(slot, source, ty);
                } else {
                    self.copy_bits(slot, source, ty);
                }
                self.load_val(slot, ty)
            }
            ExprKind::AddressOf(place) => Val::Imm(self.place(place).into()),
            ExprKind::Unary(op, operand) => Val::Imm(self.unary(*op, operand)),
            ExprKind::Binary(op, left, right) => Val::Imm(self.binary(*op, left, right)),
            ExprKind::And(left, right) => Val::Imm(self.short_circuit(left, right, true)),
            ExprKind::Or(left, right) => Val::Imm(self.short_circuit(left, right, false)),
            ExprKind::Cast(value) => Val::Imm(self.cast(value, expr.ty)),
            ExprKind::Call(function, args) => self.call(*function, args, None),
            ExprKind::Builtin(builtin, args) => self.builtin(*builtin, args, expr.ty),
            ExprKind::Variant(index, _) => Val::Imm(
                self.context
                    .i32_type()
                    .const_int(u64::from(*index), false)
                    .into(),
            ),
            ExprKind::Assign {
                place,
                value,
                drop_old,
            } => {
                self.assign(place, value, *drop_old);
                Val::Unit
            }
            ExprKind::CompoundAssign { place, op, value } => {
                let rhs = self.imm(value);
                let target = self.place(place);
                let ty = self.place_ty(place);
                let llvm = self.llvm_ty(ty);
                let current = self.builder.build_load(llvm, target, "current").unwrap();
                let result = self.arith(*op, current, rhs, ty);
                self.builder.build_store(target, result).unwrap();
                Val::Unit
            }
            ExprKind::Match { scrutinee, arms } => self.match_expr(scrutinee, arms, expr.ty),
            ExprKind::Try(value) => self.try_expr(value, expr.ty),
            ExprKind::Fallback(value, fallback) => self.fallback(value, fallback, expr.ty),
            ExprKind::Block(block) => {
                self.block(block);
                Val::Unit
            }
            ExprKind::Print {
                stderr,
                newline,
                pieces,
                values,
            } => {
                self.print(*stderr, *newline, pieces, values);
                Val::Unit
            }
            ExprKind::Asm(asm) => self.asm(
                &asm.template,
                &asm.constraints,
                &asm.outputs,
                &asm.inputs,
                asm.volatile,
                expr.ty,
            ),
            ExprKind::Tuple(_)
            | ExprKind::Struct(_)
            | ExprKind::Array(_)
            | ExprKind::ArrayRepeat(..)
            | ExprKind::VecLiteral(_)
            | ExprKind::VecRepeat(..)
            | ExprKind::Some(_)
            | ExprKind::None
            | ExprKind::Ok(_)
            | ExprKind::Err(_)
            | ExprKind::Str(_) => unreachable!("built in place above"),
        }
    }

    /// Store the value of `expr` at `dst`.
    pub(super) fn write(&mut self, expr: &'a Expr, dst: PointerValue<'ctx>) {
        if self.builds_in_place(expr) {
            self.build_into(expr, dst);
            return;
        }
        let value = self.expr(expr);
        self.store_val(dst, value, expr.ty);
    }

    pub(super) fn store_val(&mut self, dst: PointerValue<'ctx>, value: Val<'ctx>, ty: Ty) {
        match value {
            Val::Unit => {}
            Val::Imm(value) => {
                self.builder.build_store(dst, value).unwrap();
            }
            Val::Mem(src) => self.copy_bits(dst, src, ty),
        }
    }

    pub(super) fn load_val(&mut self, ptr: PointerValue<'ctx>, ty: Ty) -> Val<'ctx> {
        if self.is_unit(ty) {
            return Val::Unit;
        }
        if !self.is_scalar(ty) {
            return Val::Mem(ptr);
        }
        let llvm = self.llvm_ty(ty);
        Val::Imm(self.builder.build_load(llvm, ptr, "load").unwrap())
    }

    pub(super) fn copy_bits(&mut self, dst: PointerValue<'ctx>, src: PointerValue<'ctx>, ty: Ty) {
        if self.is_scalar(ty) {
            let llvm = self.llvm_ty(ty);
            let value = self.builder.build_load(llvm, src, "bits").unwrap();
            self.builder.build_store(dst, value).unwrap();
            return;
        }
        let size = self.size_of(ty);
        self.builder.build_memmove(dst, 1, src, 1, size).unwrap();
    }

    fn build_into(&mut self, expr: &'a Expr, dst: PointerValue<'ctx>) {
        let ty = expr.ty;
        match &expr.kind {
            ExprKind::Str(bytes) => {
                let data = self.const_bytes(bytes);
                let len = self.usize_type().const_int(bytes.len() as u64, false);
                let ptr_field = self.field_ptr(dst, ty, 0);
                self.builder.build_store(ptr_field, data).unwrap();
                let len_field = self.field_ptr(dst, ty, 1);
                self.builder.build_store(len_field, len).unwrap();
            }
            ExprKind::Tuple(parts) => {
                for (at, part) in parts.iter().enumerate() {
                    let field = self.field_ptr(dst, ty, at as u32);
                    self.write(part, field);
                }
            }
            ExprKind::Struct(fields) => {
                for (at, value) in fields {
                    let field = self.field_ptr(dst, ty, *at);
                    self.write(value, field);
                }
            }
            ExprKind::Array(elements) => {
                for (at, element) in elements.iter().enumerate() {
                    let index = self.usize_type().const_int(at as u64, false);
                    let slot = self.element_ptr(dst, ty, index);
                    self.write(element, slot);
                }
            }
            ExprKind::ArrayRepeat(value, count) => {
                let count = self.usize_type().const_int(*count, false);
                self.counted_loop(count, |this, index| {
                    let slot = this.element_ptr(dst, ty, index);
                    this.write(value, slot);
                });
            }
            ExprKind::VecLiteral(elements) => {
                let elem = self.element_ty(ty);
                let count = self.usize_type().const_int(elements.len() as u64, false);
                let data = self.alloc_buffer(elem, count);
                for (at, element) in elements.iter().enumerate() {
                    let index = self.usize_type().const_int(at as u64, false);
                    let slot = self.buffer_slot(data, elem, index);
                    self.write(element, slot);
                }
                self.store_vec(dst, ty, data, count, count);
            }
            ExprKind::VecRepeat(value, count) => {
                let elem = self.element_ty(ty);
                let count = self.usize_type().const_int(*count, false);
                let data = self.alloc_buffer(elem, count);
                self.counted_loop(count, |this, index| {
                    let slot = this.buffer_slot(data, elem, index);
                    this.write(value, slot);
                });
                self.store_vec(dst, ty, data, count, count);
            }
            ExprKind::Variant(index, values) => {
                let tag = self.context.i32_type().const_int(u64::from(*index), false);
                let tag_field = self.field_ptr(dst, ty, 0);
                self.builder.build_store(tag_field, tag).unwrap();
                for (at, value) in values.iter().enumerate() {
                    let slot = self.variant_field_ptr(dst, ty, *index, at as u32);
                    self.write(value, slot);
                }
            }
            ExprKind::Some(value) | ExprKind::Ok(value) => {
                self.store_tag(dst, ty, true);
                let payload = self.field_ptr(dst, ty, 1);
                self.write(value, payload);
            }
            ExprKind::None => self.store_tag(dst, ty, false),
            ExprKind::Err(error) => {
                self.store_tag(dst, ty, false);
                let slot = self.field_ptr(dst, ty, 2);
                self.write(error, slot);
            }
            ExprKind::Call(function, args) => {
                self.call(*function, args, Some(dst));
            }
            _ => unreachable!("not built in place"),
        }
    }

    fn store_tag(&mut self, dst: PointerValue<'ctx>, ty: Ty, set: bool) {
        let tag = self.context.bool_type().const_int(u64::from(set), false);
        let field = self.field_ptr(dst, ty, 0);
        self.builder.build_store(field, tag).unwrap();
    }

    fn read(&mut self, place: &'a Place) -> Val<'ctx> {
        let ty = self.place_ty(place);
        let ptr = self.place(place);
        self.load_val(ptr, ty)
    }

    /// The type of what `place` holds.
    pub(super) fn place_ty(&self, place: &Place) -> Ty {
        match place {
            Place::Local(local) => self.function.unwrap().locals[local.0 as usize].ty,
            Place::Field(base, at) => {
                let base = self.place_ty(base);
                match self.types().kind(base) {
                    TyKind::Tuple(parts) => parts[*at as usize],
                    _ => self.program.structs[&base].fields[*at as usize],
                }
            }
            Place::Index(base, _) => self.element_ty(self.place_ty(base)),
            Place::Deref(pointer) => match self.types().kind(pointer.ty) {
                TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                _ => unreachable!("only a pointer is dereferenced"),
            },
            Place::Temp(value) => value.ty,
        }
    }

    pub(super) fn place(&mut self, place: &'a Place) -> PointerValue<'ctx> {
        match place {
            Place::Local(local) => {
                self.addresses[local.0 as usize].expect("a local is declared before use")
            }
            Place::Field(base, at) => {
                let ty = self.place_ty(base);
                let base = self.place(base);
                self.field_ptr(base, ty, *at)
            }
            Place::Index(base, index) => {
                let seq = self.place_ty(base);
                let base = self.place(base);
                let signed = self.is_signed(index.ty);
                let index = self.imm(index).into_int_value();
                let index = self.widen(index, signed);
                let len = match self.types().kind(seq) {
                    TyKind::Array(_, len) => self.usize_type().const_int(*len, false),
                    _ => self.load_field(base, seq, 1).into_int_value(),
                };
                self.check_bounds(index, len);
                self.element_ptr(base, seq, index)
            }
            Place::Deref(pointer) => {
                let ptr = self.imm(pointer).into_pointer_value();
                self.check_null(ptr);
                ptr
            }
            Place::Temp(value) => {
                let slot = self.alloca(value.ty, "temp");
                self.write(value, slot);
                self.adopt_temporary(slot, value.ty);
                slot
            }
        }
    }

    /// `index` as a usize, extended by its sign.
    pub(super) fn widen(&self, index: IntValue<'ctx>, signed: bool) -> IntValue<'ctx> {
        let usize_type = self.usize_type();
        if index.get_type().get_bit_width() >= usize_type.get_bit_width() {
            return index;
        }
        let b = &self.builder;
        if signed {
            b.build_int_s_extend(index, usize_type, "index").unwrap()
        } else {
            b.build_int_z_extend(index, usize_type, "index").unwrap()
        }
    }

    /// `place = value`, the value evaluated first. The value replaced is
    /// dropped first when `drop_old` says so, for a local only while it
    /// still owns one.
    fn assign(&mut self, place: &'a Place, value: &'a Expr, drop_old: bool) {
        let ty = value.ty;
        let mut rhs = self.expr(value);
        if drop_old && let Val::Mem(src) = rhs {
            let kept = self.alloca(ty, "assigned");
            self.copy_bits(kept, src, ty);
            rhs = Val::Mem(kept);
        }
        let target = self.place(place);
        let target_ty = self.place_ty(place);
        let flag = match place {
            Place::Local(local) => self.flags[local.0 as usize],
            _ => None,
        };
        if drop_old {
            match flag {
                Some(flag) => self.drop_owned(super::Owned {
                    slot: target,
                    flag,
                    ty: target_ty,
                }),
                None => self.call_drop(target, target_ty),
            }
        }
        self.store_val(target, rhs, target_ty);
        if let Some(flag) = flag {
            let raised = self.context.bool_type().const_int(1, false);
            self.builder.build_store(flag, raised).unwrap();
        }
    }

    pub(super) fn call(
        &mut self,
        function: FnId,
        args: &'a [Expr],
        dst: Option<PointerValue<'ctx>>,
    ) -> Val<'ctx> {
        let callee = &self.program.functions[function.0 as usize];
        let value = self.functions[function.0 as usize];
        let mut lowered: Vec<BasicMetadataValueEnum> = Vec::new();
        let sret = self
            .in_memory(callee.ret)
            .then(|| dst.unwrap_or_else(|| self.alloca(callee.ret, "result")));
        if let Some(sret) = sret {
            lowered.push(sret.into());
        }
        for (arg, &param) in args.iter().zip(&callee.params) {
            let local = &callee.locals[param.0 as usize];
            if local.view || self.is_scalar(arg.ty) {
                lowered.push(self.imm(arg).into());
                continue;
            }
            let copy = self.alloca(arg.ty, "arg");
            self.write(arg, copy);
            lowered.push(copy.into());
        }
        let result = self.builder.build_call(value, &lowered, "call").unwrap();
        if let Some(sret) = sret {
            return Val::Mem(sret);
        }
        match result.try_as_basic_value() {
            ValueKind::Basic(value) => Val::Imm(value),
            ValueKind::Instruction(_) => Val::Unit,
        }
    }

    fn unary(&mut self, op: UnOp, operand: &'a Expr) -> BasicValueEnum<'ctx> {
        let value = self.imm(operand);
        match (op, value) {
            (UnOp::Neg, BasicValueEnum::IntValue(v)) => {
                let zero = v.get_type().const_zero();
                self.int_arith(BinOp::Sub, zero, v, true)
            }
            (UnOp::Neg, BasicValueEnum::FloatValue(v)) => {
                self.builder.build_float_neg(v, "fneg").unwrap().into()
            }
            (UnOp::Not, BasicValueEnum::IntValue(v)) => {
                self.builder.build_not(v, "not").unwrap().into()
            }
            _ => unreachable!("checked operand"),
        }
    }

    fn binary(&mut self, op: BinOp, left: &'a Expr, right: &'a Expr) -> BasicValueEnum<'ctx> {
        let ty = left.ty;
        let l = self.imm(left);
        let r = self.imm(right);
        match op {
            BinOp::PtrAdd | BinOp::PtrSub => {
                let pointee = match self.types().kind(ty) {
                    TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                    _ => unreachable!("pointer arithmetic on a pointer"),
                };
                let elem = self.llvm_ty(pointee);
                let offset = self.widen(r.into_int_value(), self.is_signed(right.ty));
                let step = match op {
                    BinOp::PtrSub => self.builder.build_int_neg(offset, "back").unwrap(),
                    _ => offset,
                };
                unsafe {
                    self.builder
                        .build_gep(elem, l.into_pointer_value(), &[step], "ptr")
                        .unwrap()
                        .into()
                }
            }
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                self.compare(op, l, r, ty)
            }
            _ => self.arith(op, l, r, ty),
        }
    }

    fn compare(
        &mut self,
        op: BinOp,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        ty: Ty,
    ) -> BasicValueEnum<'ctx> {
        let (signed, unsigned, float) = match op {
            BinOp::Eq => (IntPredicate::EQ, IntPredicate::EQ, FloatPredicate::OEQ),
            BinOp::Ne => (IntPredicate::NE, IntPredicate::NE, FloatPredicate::UNE),
            BinOp::Lt => (IntPredicate::SLT, IntPredicate::ULT, FloatPredicate::OLT),
            BinOp::Le => (IntPredicate::SLE, IntPredicate::ULE, FloatPredicate::OLE),
            BinOp::Gt => (IntPredicate::SGT, IntPredicate::UGT, FloatPredicate::OGT),
            _ => (IntPredicate::SGE, IntPredicate::UGE, FloatPredicate::OGE),
        };
        let b = &self.builder;
        match (l, r) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                b.build_float_compare(float, l, r, "fcmp").unwrap().into()
            }
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let usize_type = self.usize_type();
                let l = b.build_ptr_to_int(l, usize_type, "ptr_l").unwrap();
                let r = b.build_ptr_to_int(r, usize_type, "ptr_r").unwrap();
                b.build_int_compare(unsigned, l, r, "ptr_cmp")
                    .unwrap()
                    .into()
            }
            (l, r) => {
                let predicate = if self.is_signed(ty) { signed } else { unsigned };
                b.build_int_compare(predicate, l.into_int_value(), r.into_int_value(), "cmp")
                    .unwrap()
                    .into()
            }
        }
    }

    pub(super) fn arith(
        &mut self,
        op: BinOp,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        ty: Ty,
    ) -> BasicValueEnum<'ctx> {
        match (l, r) {
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                let b = &self.builder;
                match op {
                    BinOp::Add => b.build_float_add(l, r, "fadd"),
                    BinOp::Sub => b.build_float_sub(l, r, "fsub"),
                    BinOp::Mul => b.build_float_mul(l, r, "fmul"),
                    BinOp::Div => b.build_float_div(l, r, "fdiv"),
                    BinOp::Rem => b.build_float_rem(l, r, "frem"),
                    other => unreachable!("{other:?} does not apply to floats"),
                }
                .unwrap()
                .into()
            }
            (l, r) => {
                let signed = self.is_signed(ty);
                self.int_arith(op, l.into_int_value(), r.into_int_value(), signed)
            }
        }
    }

    /// The operations that can go wrong check first: `+ - *` trap on
    /// overflow, `/ %` on a zero divisor or `MIN / -1`, shifts on an
    /// oversized amount. All are skipped in ReleaseFast.
    pub(super) fn int_arith(
        &mut self,
        op: BinOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        signed: bool,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul if self.mode.emit_safety_checks() => {
                return self.checked_arith(op, l, r, signed);
            }
            BinOp::Div | BinOp::Rem => self.check_division(l, r, signed),
            BinOp::Shl | BinOp::Shr => self.check_shift(r),
            _ => {}
        }
        let b = &self.builder;
        match op {
            BinOp::Add | BinOp::AddWrap => b.build_int_add(l, r, "add"),
            BinOp::Sub | BinOp::SubWrap => b.build_int_sub(l, r, "sub"),
            BinOp::Mul | BinOp::MulWrap => b.build_int_mul(l, r, "mul"),
            BinOp::Div if signed => b.build_int_signed_div(l, r, "div"),
            BinOp::Div => b.build_int_unsigned_div(l, r, "udiv"),
            BinOp::Rem if signed => b.build_int_signed_rem(l, r, "rem"),
            BinOp::Rem => b.build_int_unsigned_rem(l, r, "urem"),
            BinOp::BitAnd => b.build_and(l, r, "and"),
            BinOp::BitOr => b.build_or(l, r, "or"),
            BinOp::BitXor => b.build_xor(l, r, "xor"),
            BinOp::Shl => b.build_left_shift(l, r, "shl"),
            BinOp::Shr => b.build_right_shift(l, r, signed, "shr"),
            _ => unreachable!("{op:?} is not arithmetic"),
        }
        .unwrap()
        .into()
    }

    fn short_circuit(
        &mut self,
        left: &'a Expr,
        right: &'a Expr,
        is_and: bool,
    ) -> BasicValueEnum<'ctx> {
        let bool_type = self.context.bool_type();
        let lhs = self.imm(left).into_int_value();
        let entry = self.builder.get_insert_block().unwrap();
        let rhs_bb = self.append("rhs_eval");
        let merge = self.append("merge");
        let (on_true, on_false) = if is_and {
            (rhs_bb, merge)
        } else {
            (merge, rhs_bb)
        };
        self.builder
            .build_conditional_branch(lhs, on_true, on_false)
            .unwrap();
        self.builder.position_at_end(rhs_bb);
        let rhs = self.imm(right).into_int_value();
        let rhs_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge).unwrap();
        self.builder.position_at_end(merge);
        let phi = self.builder.build_phi(bool_type, "result").unwrap();
        let short = if is_and {
            bool_type.const_zero()
        } else {
            bool_type.const_all_ones()
        };
        phi.add_incoming(&[(&short, entry), (&rhs, rhs_end)]);
        phi.as_basic_value()
    }

    /// `value as to`, the conversion read off the two types.
    fn cast(&mut self, value: &'a Expr, to: Ty) -> BasicValueEnum<'ctx> {
        let from = value.ty;
        let source = self.expr(value);
        let target = self.llvm_ty(to);
        let signed = self.is_signed(from) && from != Types::BOOL;
        if let Val::Mem(slice) = source {
            let ptr = self.load_field(slice, from, 0);
            return match target {
                BasicTypeEnum::IntType(t) => self
                    .builder
                    .build_ptr_to_int(ptr.into_pointer_value(), t, "str_ptrtoint")
                    .unwrap()
                    .into(),
                _ => ptr,
            };
        }
        let b = &self.builder;
        match (source, target) {
            (Val::Imm(BasicValueEnum::IntValue(v)), BasicTypeEnum::IntType(t)) => {
                let (from_bits, to_bits) = (v.get_type().get_bit_width(), t.get_bit_width());
                match from_bits.cmp(&to_bits) {
                    std::cmp::Ordering::Equal => v.into(),
                    std::cmp::Ordering::Less if signed => {
                        b.build_int_s_extend(v, t, "sext").unwrap().into()
                    }
                    std::cmp::Ordering::Less => b.build_int_z_extend(v, t, "zext").unwrap().into(),
                    std::cmp::Ordering::Greater => {
                        b.build_int_truncate(v, t, "trunc").unwrap().into()
                    }
                }
            }
            (Val::Imm(BasicValueEnum::FloatValue(v)), BasicTypeEnum::IntType(t)) => {
                let unsigned = !self.is_signed(to);
                self.check_float_to_int(v, t, unsigned);
                let b = &self.builder;
                if unsigned {
                    b.build_float_to_unsigned_int(v, t, "fptoui")
                        .unwrap()
                        .into()
                } else {
                    b.build_float_to_signed_int(v, t, "fptosi").unwrap().into()
                }
            }
            (Val::Imm(BasicValueEnum::IntValue(v)), BasicTypeEnum::FloatType(t)) => {
                if signed {
                    b.build_signed_int_to_float(v, t, "sitofp").unwrap().into()
                } else {
                    b.build_unsigned_int_to_float(v, t, "uitofp")
                        .unwrap()
                        .into()
                }
            }
            (Val::Imm(BasicValueEnum::FloatValue(v)), BasicTypeEnum::FloatType(t)) => {
                match v.get_type().get_bit_width().cmp(&t.get_bit_width()) {
                    std::cmp::Ordering::Equal => v.into(),
                    std::cmp::Ordering::Less => b.build_float_ext(v, t, "fext").unwrap().into(),
                    std::cmp::Ordering::Greater => {
                        b.build_float_trunc(v, t, "ftrunc").unwrap().into()
                    }
                }
            }
            (Val::Imm(BasicValueEnum::PointerValue(v)), BasicTypeEnum::IntType(t)) => {
                b.build_ptr_to_int(v, t, "ptrtoint").unwrap().into()
            }
            (Val::Imm(BasicValueEnum::IntValue(v)), BasicTypeEnum::PointerType(t)) => {
                b.build_int_to_ptr(v, t, "inttoptr").unwrap().into()
            }
            (Val::Imm(BasicValueEnum::PointerValue(v)), BasicTypeEnum::PointerType(_)) => v.into(),
            _ => unreachable!("checked cast"),
        }
    }

    fn match_expr(&mut self, scrutinee: &'a Place, arms: &'a [Arm], ty: Ty) -> Val<'ctx> {
        let subject = self.place_ty(scrutinee);
        let place = self.place(scrutinee);
        let tag = match self.types().kind(subject) {
            TyKind::Optional(_) | TyKind::Result(..) => {
                self.load_field(place, subject, 0).into_int_value()
            }
            TyKind::Adt(..) if !self.is_scalar(subject) => {
                self.load_field(place, subject, 0).into_int_value()
            }
            _ => {
                let llvm = self.llvm_ty(subject);
                self.builder
                    .build_load(llvm, place, "subject")
                    .unwrap()
                    .into_int_value()
            }
        };
        let result = (!self.is_unit(ty)).then(|| self.alloca(ty, "match_result"));
        let merge = self.append("match_merge");
        let mut cases = Vec::new();
        let mut default = None;
        let mut blocks = Vec::new();
        for arm in arms {
            let block = self.append("match_arm");
            match arm.tag {
                Some(value) => cases.push((Self::int_const(tag.get_type(), value), block)),
                None => default = Some(block),
            }
            blocks.push(block);
        }
        let switch_bb = self.builder.get_insert_block().unwrap();
        let default = default.unwrap_or_else(|| {
            let block = self.append("match_default");
            self.builder.position_at_end(block);
            self.builder.build_unreachable().unwrap();
            block
        });
        self.builder.position_at_end(switch_bb);
        self.builder.build_switch(tag, default, &cases).unwrap();

        for (arm, block) in arms.iter().zip(blocks) {
            self.builder.position_at_end(block);
            for (local, payload) in &arm.bindings {
                let address = match *payload {
                    Payload::SomeValue | Payload::OkValue => self.field_ptr(place, subject, 1),
                    Payload::ErrValue => self.field_ptr(place, subject, 2),
                    Payload::Variant { variant, field } => {
                        self.variant_field_ptr(place, subject, variant, field)
                    }
                };
                self.addresses[local.0 as usize] = Some(address);
            }
            match result {
                Some(slot) => self.write(&arm.body, slot),
                None => {
                    self.expr(&arm.body);
                }
            }
            self.branch_if_open(merge);
        }
        self.builder.position_at_end(merge);
        match result {
            Some(slot) => self.load_val(slot, ty),
            None => Val::Unit,
        }
    }

    /// `try value`: on an error, return it from the function, dropping what
    /// the function owns; otherwise go on with the payload.
    fn try_expr(&mut self, value: &'a Expr, ty: Ty) -> Val<'ctx> {
        let result_ty = value.ty;
        let result = self.expr(value).ptr();
        let is_ok = self.load_field(result, result_ty, 0).into_int_value();
        let failed = self.append("try_failed");
        let ok = self.append("try_ok");
        self.builder
            .build_conditional_branch(is_ok, ok, failed)
            .unwrap();

        self.builder.position_at_end(failed);
        let returned = self.function.unwrap().ret;
        let out = self
            .sret
            .unwrap_or_else(|| self.alloca(returned, "failure"));
        self.store_tag(out, returned, false);
        let error = self.field_ptr(result, result_ty, 2);
        let error_slot = self.field_ptr(out, returned, 2);
        let error_ty = match self.types().kind(result_ty) {
            TyKind::Result(_, error) => *error,
            _ => unreachable!("try takes a result"),
        };
        self.copy_bits(error_slot, error, error_ty);
        self.drop_everything();
        self.build_return(None);

        self.builder.position_at_end(ok);
        let payload = self.field_ptr(result, result_ty, 1);
        self.load_val(payload, ty)
    }

    /// `value orelse fallback`, `value catch fallback`: the payload when
    /// there is one, else the fallback, evaluated only then.
    fn fallback(&mut self, value: &'a Expr, fallback: &'a Expr, ty: Ty) -> Val<'ctx> {
        let held_ty = value.ty;
        let held = self.expr(value).ptr();
        let has = self.load_field(held, held_ty, 0).into_int_value();
        let slot = self.alloca(ty, "or");
        let present = self.append("present");
        let absent = self.append("fallback");
        let merge = self.append("fallback_merge");
        self.builder
            .build_conditional_branch(has, present, absent)
            .unwrap();
        self.builder.position_at_end(present);
        let payload = self.field_ptr(held, held_ty, 1);
        self.copy_bits(slot, payload, ty);
        self.builder.build_unconditional_branch(merge).unwrap();
        self.builder.position_at_end(absent);
        self.write(fallback, slot);
        self.branch_if_open(merge);
        self.builder.position_at_end(merge);
        self.load_val(slot, ty)
    }
}
