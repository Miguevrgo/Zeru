//! Places, operands and rvalues.

use inkwell::{
    FloatPredicate, IntPredicate,
    types::BasicTypeEnum,
    values::{BasicValueEnum, IntValue, PointerValue},
};

use crate::mir::{Aggregate, ConstValue, Constant, Operand, Place, Projection, Rvalue};
use crate::tir::{BinOp, UnOp};
use crate::ty::{Ty, TyKind, Types};

use super::{Lower, Slot, Val};

impl<'a, 'ctx> Lower<'a, 'ctx> {
    pub(super) fn place_ty(&self, place: &Place) -> Ty {
        self.body().place_ty(self.program, place)
    }

    pub(super) fn operand_ty(&self, operand: &Operand) -> Ty {
        self.body().operand_ty(self.program, operand)
    }

    /// The address of `place`, its projections followed from its local.
    pub(super) fn place(&mut self, place: &Place) -> PointerValue<'ctx> {
        let mut ty = self.local_ty(place.local);
        let mut projections = place.projection.iter();
        let mut address = match self.locals[place.local.0 as usize] {
            Slot::Mem(slot) => slot,
            Slot::Ssa(value) => {
                projections.next();
                ty = self.program.project_ty(ty, Projection::Deref, &mut None);
                value.expect("assigned before use").into_pointer_value()
            }
        };
        let mut variant = None;
        for &projection in projections {
            address = match (projection, variant) {
                (Projection::Deref, _) => self
                    .builder
                    .build_load(self.ptr_type(), address, "deref")
                    .unwrap()
                    .into_pointer_value(),
                (Projection::Field(at), Some(chosen)) => {
                    self.variant_field_ptr(address, ty, chosen, at)
                }
                (Projection::Field(at), None) => self.field_ptr(address, ty, at),
                (Projection::Index(index), _) => {
                    let index = self.read(&Place::local(index)).imm().into_int_value();
                    self.element_ptr(address, ty, index)
                }
                (Projection::Downcast(_), _) => address,
            };
            ty = self.program.project_ty(ty, projection, &mut variant);
        }
        address
    }

    /// What `place` holds: a scalar loaded, an aggregate by its address.
    pub(super) fn read(&mut self, place: &Place) -> Val<'ctx> {
        if let Some(local) = place.as_local()
            && let Slot::Ssa(value) = self.locals[local.0 as usize]
        {
            return Val::Imm(value.expect("assigned before use"));
        }
        let ty = self.place_ty(place);
        let address = self.place(place);
        self.load_val(address, ty)
    }

    pub(super) fn operand(&mut self, operand: &Operand) -> Val<'ctx> {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => self.read(place),
            Operand::Constant(constant) => self.constant(constant),
        }
    }

    fn constant(&mut self, constant: &Constant) -> Val<'ctx> {
        let llvm = match &constant.value {
            ConstValue::Str(_) => {
                let slot = self.alloca(constant.ty, "str");
                self.build_str(constant, slot);
                return Val::Mem(slot);
            }
            _ => self.llvm_ty(constant.ty),
        };
        Val::Imm(match (&constant.value, llvm) {
            (ConstValue::Int(_), BasicTypeEnum::PointerType(t)) => t.const_null().into(),
            (ConstValue::Int(value), BasicTypeEnum::FloatType(t)) => {
                t.const_float(*value as f64).into()
            }
            (ConstValue::Int(value), _) => Self::int_const(llvm.into_int_type(), *value).into(),
            (ConstValue::Float(value), _) => llvm.into_float_type().const_float(*value).into(),
            (ConstValue::Bool(value), _) => self
                .context
                .bool_type()
                .const_int(u64::from(*value), false)
                .into(),
            _ => unreachable!("handled above"),
        })
    }

    fn build_str(&mut self, constant: &Constant, dst: PointerValue<'ctx>) {
        let ConstValue::Str(bytes) = &constant.value else {
            unreachable!("a str")
        };
        let data = self.const_bytes(bytes);
        let len = self.usize_type().const_int(bytes.len() as u64, false);
        let ptr_field = self.field_ptr(dst, constant.ty, 0);
        self.builder.build_store(ptr_field, data).unwrap();
        let len_field = self.field_ptr(dst, constant.ty, 1);
        self.builder.build_store(len_field, len).unwrap();
    }

    /// Store `value` of type `ty` at `dest`.
    pub(super) fn store(&mut self, dest: &Place, value: Val<'ctx>, ty: Ty) {
        if let Some(local) = dest.as_local()
            && let Slot::Ssa(slot) = &mut self.locals[local.0 as usize]
        {
            *slot = Some(value.imm());
            return;
        }
        let address = self.place(dest);
        self.store_val(address, value, ty);
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

    /// Copy a value of `ty`. One made of a few scalars goes through
    /// registers: a block copy right after
    /// the parts were stored one by one stalls the processor.
    pub(super) fn copy_bits(&mut self, dst: PointerValue<'ctx>, src: PointerValue<'ctx>, ty: Ty) {
        let llvm = self.llvm_ty(ty);
        let mut leaves = Vec::new();
        if scalar_leaves(llvm, &mut vec![0], &mut leaves) {
            let i32_type = self.context.i32_type();
            for (path, leaf) in leaves {
                let indices: Vec<IntValue> = path
                    .iter()
                    .map(|&at| i32_type.const_int(u64::from(at), false))
                    .collect();
                let (from, to) = unsafe {
                    (
                        self.builder
                            .build_in_bounds_gep(llvm, src, &indices, "from")
                            .unwrap(),
                        self.builder
                            .build_in_bounds_gep(llvm, dst, &indices, "to")
                            .unwrap(),
                    )
                };
                let value = self.builder.build_load(leaf, from, "part").unwrap();
                self.builder.build_store(to, value).unwrap();
            }
            return;
        }
        let align = self.target.get_abi_alignment(&llvm);
        let size = self.size_of(ty);
        self.builder
            .build_memmove(dst, align, src, align, size)
            .unwrap();
    }

    pub(super) fn assign(&mut self, dest: &'a Place, rvalue: &'a Rvalue) {
        let ty = self.place_ty(dest);
        let value = match rvalue {
            Rvalue::Use(Operand::Constant(constant))
                if matches!(constant.value, ConstValue::Str(_)) =>
            {
                let address = self.place(dest);
                return self.build_str(constant, address);
            }
            Rvalue::Aggregate(..)
            | Rvalue::Repeat(..)
            | Rvalue::VecRepeat(..)
            | Rvalue::VecNew(_)
            | Rvalue::DeepCopy(_)
            | Rvalue::VecPop(_)
            | Rvalue::VecGet(..) => {
                let address = self.place(dest);
                return self.build_into(rvalue, address, ty);
            }
            Rvalue::Use(operand) => self.operand(operand),
            Rvalue::AddressOf(place) => Val::Imm(self.place(place).into()),
            Rvalue::Binary {
                op,
                left,
                right,
                checked,
            } => Val::Imm(self.binary(*op, left, right, *checked)),
            Rvalue::Unary(op, operand) => {
                let value = self.operand(operand).imm();
                Val::Imm(match (op, value) {
                    (UnOp::Neg, BasicValueEnum::FloatValue(v)) => {
                        self.builder.build_float_neg(v, "fneg").unwrap().into()
                    }
                    (UnOp::Not, BasicValueEnum::IntValue(v)) => {
                        self.builder.build_not(v, "not").unwrap().into()
                    }
                    _ => unreachable!("checked operand"),
                })
            }
            Rvalue::Cast { value, to, checked } => Val::Imm(self.cast(value, *to, *checked)),
            Rvalue::Len(place) => {
                let seq = self.place_ty(place);
                match self.types().kind(seq) {
                    TyKind::Array(_, len) => {
                        Val::Imm(self.usize_type().const_int(*len, false).into())
                    }
                    _ => {
                        let address = self.place(place);
                        Val::Imm(self.load_field(address, seq, 1))
                    }
                }
            }
            Rvalue::Capacity(place) => {
                let vec = self.place_ty(place);
                let address = self.place(place);
                Val::Imm(self.load_field(address, vec, 2))
            }
            Rvalue::Discriminant(place) => {
                let subject = self.place_ty(place);
                match self.is_scalar(subject) {
                    true => self.read(place),
                    false => {
                        let address = self.place(place);
                        Val::Imm(self.load_field(address, subject, 0))
                    }
                }
            }
            Rvalue::VecRemove(vec, at) => self.vec_remove(vec, at),
        };
        self.store(dest, value, ty);
    }

    fn build_into(&mut self, rvalue: &'a Rvalue, dst: PointerValue<'ctx>, ty: Ty) {
        match rvalue {
            Rvalue::Aggregate(kind, values) => self.aggregate(*kind, values, dst, ty),
            Rvalue::Repeat(value, count) => {
                let elem = self.operand_ty(value);
                let element = self.operand(value);
                let count = self.usize_type().const_int(*count, false);
                self.counted_loop(count, |this, index| {
                    let slot = this.element_ptr(dst, ty, index);
                    this.store_val(slot, element, elem);
                });
            }
            Rvalue::VecRepeat(value, count) => {
                let elem = self.element_ty(ty);
                let count = self.operand(count).imm().into_int_value();
                let element = self.operand(value);
                let owns = self.program.needs_drop(elem);
                let data = self.alloc_buffer(elem, count);
                self.counted_loop(count, |this, index| {
                    let slot = this.buffer_slot(data, elem, index);
                    match (owns, element) {
                        (true, Val::Mem(source)) => this.call_copy(slot, source, elem),
                        _ => this.store_val(slot, element, elem),
                    }
                });
                self.store_vec(dst, ty, data, count, count);
            }
            Rvalue::VecNew(capacity) => {
                let elem = self.element_ty(ty);
                let usize_type = self.usize_type();
                let (data, cap) = match capacity {
                    Some(capacity) => {
                        let cap = self.operand(capacity).imm().into_int_value();
                        (self.alloc_buffer(elem, cap), cap)
                    }
                    None => (self.ptr_type().const_null(), usize_type.const_zero()),
                };
                self.store_vec(dst, ty, data, usize_type.const_zero(), cap);
            }
            Rvalue::DeepCopy(place) => {
                let source = self.place(place);
                self.call_copy(dst, source, ty);
            }
            Rvalue::VecPop(vec) => self.vec_take(vec, None, dst, ty),
            Rvalue::VecGet(vec, at) => self.vec_take(vec, Some(at), dst, ty),
            _ => unreachable!("not built in place"),
        }
    }

    fn aggregate(
        &mut self,
        kind: Aggregate,
        values: &'a [Operand],
        dst: PointerValue<'ctx>,
        ty: Ty,
    ) {
        let put = |this: &mut Self, slot: PointerValue<'ctx>, value: &Operand| {
            let value_ty = this.operand_ty(value);
            let value = this.operand(value);
            this.store_val(slot, value, value_ty);
        };
        match kind {
            Aggregate::Tuple => {
                for (at, value) in values.iter().enumerate() {
                    let field = self.field_ptr(dst, ty, at as u32);
                    put(self, field, value);
                }
            }
            Aggregate::Array => {
                for (at, value) in values.iter().enumerate() {
                    let index = self.usize_type().const_int(at as u64, false);
                    let slot = self.element_ptr(dst, ty, index);
                    put(self, slot, value);
                }
            }
            Aggregate::Vec => {
                let elem = self.element_ty(ty);
                let count = self.usize_type().const_int(values.len() as u64, false);
                let data = self.alloc_buffer(elem, count);
                for (at, value) in values.iter().enumerate() {
                    let index = self.usize_type().const_int(at as u64, false);
                    let slot = self.buffer_slot(data, elem, index);
                    put(self, slot, value);
                }
                self.store_vec(dst, ty, data, count, count);
            }
            Aggregate::Variant(index) => {
                let tag = self.context.i32_type().const_int(u64::from(index), false);
                let tag_field = self.field_ptr(dst, ty, 0);
                self.builder.build_store(tag_field, tag).unwrap();
                for (at, value) in values.iter().enumerate() {
                    let slot = self.variant_field_ptr(dst, ty, index, at as u32);
                    put(self, slot, value);
                }
            }
            Aggregate::Some | Aggregate::Ok => {
                self.store_tag(dst, ty, true);
                let payload = self.field_ptr(dst, ty, 1);
                put(self, payload, &values[0]);
            }
            Aggregate::None => self.store_tag(dst, ty, false),
            Aggregate::Err => {
                self.store_tag(dst, ty, false);
                let slot = self.field_ptr(dst, ty, 2);
                put(self, slot, &values[0]);
            }
        }
    }

    pub(super) fn store_tag(&mut self, dst: PointerValue<'ctx>, ty: Ty, set: bool) {
        let tag = self.context.bool_type().const_int(u64::from(set), false);
        let field = self.field_ptr(dst, ty, 0);
        self.builder.build_store(field, tag).unwrap();
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

    fn binary(
        &mut self,
        op: BinOp,
        left: &Operand,
        right: &Operand,
        checked: bool,
    ) -> BasicValueEnum<'ctx> {
        let ty = self.operand_ty(left);
        let l = self.operand(left).imm();
        let r = self.operand(right).imm();
        match op {
            BinOp::PtrAdd | BinOp::PtrSub => {
                let pointee = match self.types().kind(ty) {
                    TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner) => *inner,
                    _ => unreachable!("pointer arithmetic on a pointer"),
                };
                let elem = self.llvm_ty(pointee);
                let right_ty = self.operand_ty(right);
                let offset = self.widen(r.into_int_value(), self.is_signed(right_ty));
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
            _ => self.arith(op, l, r, ty, checked),
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

    fn arith(
        &mut self,
        op: BinOp,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        ty: Ty,
        checked: bool,
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
                self.int_arith(op, l.into_int_value(), r.into_int_value(), signed, checked)
            }
        }
    }

    /// With `checked`, the operations that can go wrong check first: `+ - *`
    /// trap on overflow, `/ %` on a zero divisor or `MIN / -1`, shifts on an
    /// oversized amount.
    pub(super) fn int_arith(
        &mut self,
        op: BinOp,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        signed: bool,
        checked: bool,
    ) -> BasicValueEnum<'ctx> {
        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul if checked => {
                return self.checked_arith(op, l, r, signed);
            }
            BinOp::Div | BinOp::Rem if checked => self.check_division(l, r, signed),
            BinOp::Shl | BinOp::Shr if checked => self.check_shift(r),
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

    /// `value as to`, the conversion read off the two types.
    fn cast(&mut self, value: &Operand, to: Ty, checked: bool) -> BasicValueEnum<'ctx> {
        let from = self.operand_ty(value);
        let source = self.operand(value);
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
                if checked {
                    self.check_float_to_int(v, t, unsigned);
                }
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
}

/// The scalars `ty` is made of, each with the indices that reach it from
/// `path`; false when part of it is no scalar or the parts are too many to
/// be worth copying one by one.
fn scalar_leaves<'ctx>(
    ty: BasicTypeEnum<'ctx>,
    path: &mut Vec<u32>,
    leaves: &mut Vec<(Vec<u32>, BasicTypeEnum<'ctx>)>,
) -> bool {
    let parts: Vec<BasicTypeEnum> = match ty {
        BasicTypeEnum::StructType(shape) => shape.get_field_types(),
        BasicTypeEnum::ArrayType(array) if array.len() <= 4 => {
            vec![array.get_element_type(); array.len() as usize]
        }
        BasicTypeEnum::IntType(_) | BasicTypeEnum::FloatType(_) | BasicTypeEnum::PointerType(_) => {
            leaves.push((path.clone(), ty));
            return leaves.len() <= 4;
        }
        _ => return false,
    };
    parts.into_iter().enumerate().all(|(at, part)| {
        path.push(at as u32);
        let fits = scalar_leaves(part, path, leaves);
        path.pop();
        fits
    })
}
