//! std.simd: each operation on whole vectors, the arrays the checker sized
//! to the target's vector registers, as LLVM vector instructions.

use inkwell::{
    FloatPredicate, IntPredicate,
    types::{BasicTypeEnum, VectorType},
    values::{BasicValueEnum, IntValue, PointerValue, VectorValue},
};

use crate::mir::Operand;
use crate::tir::SimdOp;
use crate::ty::{Ty, TyKind};

use super::{Lower, Val};

impl<'a, 'ctx> Lower<'a, 'ctx> {
    /// The LLVM vector type of the array type `ty`.
    fn vector_type(&mut self, ty: Ty) -> VectorType<'ctx> {
        let TyKind::Array(elem, lanes) = *self.types().kind(ty) else {
            unreachable!("a vector is an array")
        };
        match self.llvm_ty(elem) {
            BasicTypeEnum::IntType(int) => int.vec_type(lanes as u32),
            BasicTypeEnum::FloatType(float) => float.vec_type(lanes as u32),
            _ => unreachable!("lanes are numbers"),
        }
    }

    /// The bytes of one lane of the array type `ty`, which is all the
    /// alignment an array has.
    fn lane_bytes(&mut self, ty: Ty) -> u32 {
        let elem = self.element_ty(ty);
        let lane = self.llvm_ty(elem);
        self.target.get_abi_alignment(&lane)
    }

    fn load_vector(&mut self, ty: Ty, address: PointerValue<'ctx>) -> VectorValue<'ctx> {
        let vector = self.vector_type(ty);
        let align = self.lane_bytes(ty);
        let load = self.builder.build_load(vector, address, "lanes").unwrap();
        let lanes = load.into_vector_value();
        if let Some(instruction) = lanes.as_instruction() {
            let _ = instruction.set_alignment(align);
        }
        lanes
    }

    pub(super) fn store_vector(
        &mut self,
        ty: Ty,
        address: PointerValue<'ctx>,
        value: VectorValue<'ctx>,
    ) {
        let align = self.lane_bytes(ty);
        let store = self.builder.build_store(address, value).unwrap();
        let _ = store.set_alignment(align);
    }

    /// The vector an array operand holds.
    fn vector(&mut self, operand: &Operand) -> VectorValue<'ctx> {
        let ty = self.operand_ty(operand);
        let address = match self.operand(operand) {
            Val::Mem(address) => address,
            value => {
                let slot = self.alloca(ty, "lanes");
                self.builder.build_store(slot, value.imm()).unwrap();
                slot
            }
        };
        self.load_vector(ty, address)
    }

    /// Where the elements of a slice operand start, and how many it has.
    pub(super) fn slice_parts(
        &mut self,
        operand: &Operand,
    ) -> (PointerValue<'ctx>, IntValue<'ctx>) {
        let ty = self.operand_ty(operand);
        let [data, len] = match self.operand(operand) {
            Val::Mem(slice) => [0, 1].map(|at| self.load_field(slice, ty, at)),
            value => {
                let slice = value.imm().into_struct_value();
                [0, 1].map(|at| self.builder.build_extract_value(slice, at, "part").unwrap())
            }
        };
        (data.into_pointer_value(), len.into_int_value())
    }

    /// `simd::store`: `value` written over the first lanes of `to`.
    pub(super) fn simd_store(&mut self, to: &Operand, value: &Operand) {
        let (data, _) = self.slice_parts(to);
        let ty = self.operand_ty(value);
        let lanes = self.vector(value);
        self.store_vector(ty, data, lanes);
    }

    /// The vector, or for `sum` the number, a std.simd operation gives, of
    /// type `ty`.
    pub(super) fn simd_value(
        &mut self,
        op: SimdOp,
        operands: &[Operand],
        ty: Ty,
    ) -> BasicValueEnum<'ctx> {
        if op == SimdOp::Load {
            let (data, _) = self.slice_parts(&operands[0]);
            return self.load_vector(ty, data).into();
        }
        let first = self.operand_ty(&operands[0]);
        let elem = self.element_ty(first);
        let signed = self.is_signed(elem);
        let float = matches!(self.llvm_ty(elem), BasicTypeEnum::FloatType(_));
        let a = self.vector(&operands[0]);
        let b = match op {
            SimdOp::Sum | SimdOp::Shl | SimdOp::Shr => None,
            _ => Some(self.vector(&operands[1])),
        };
        let b_vec = || b.expect("a second vector");
        let builder = &self.builder;
        match op {
            SimdOp::Add if float => builder.build_float_add(a, b_vec(), "add").unwrap().into(),
            SimdOp::Sub if float => builder.build_float_sub(a, b_vec(), "sub").unwrap().into(),
            SimdOp::Mul if float => builder.build_float_mul(a, b_vec(), "mul").unwrap().into(),
            SimdOp::Add => builder.build_int_add(a, b_vec(), "add").unwrap().into(),
            SimdOp::Sub => builder.build_int_sub(a, b_vec(), "sub").unwrap().into(),
            SimdOp::Mul => builder.build_int_mul(a, b_vec(), "mul").unwrap().into(),
            SimdOp::And => builder.build_and(a, b_vec(), "and").unwrap().into(),
            SimdOp::Or => builder.build_or(a, b_vec(), "or").unwrap().into(),
            SimdOp::Xor => builder.build_xor(a, b_vec(), "xor").unwrap().into(),
            SimdOp::Min | SimdOp::Max if float => {
                let predicate = match op {
                    SimdOp::Min => FloatPredicate::OLT,
                    _ => FloatPredicate::OGT,
                };
                let wins = builder
                    .build_float_compare(predicate, a, b_vec(), "wins")
                    .unwrap();
                builder.build_select(wins, a, b_vec(), "pick").unwrap()
            }
            SimdOp::Min | SimdOp::Max => {
                let name = match (op, signed) {
                    (SimdOp::Min, true) => "llvm.smin",
                    (SimdOp::Max, true) => "llvm.smax",
                    (SimdOp::Min, false) => "llvm.umin",
                    _ => "llvm.umax",
                };
                self.intrinsic(name, a.get_type().into(), &[a.into(), b_vec().into()])
            }
            SimdOp::Eq | SimdOp::Lt | SimdOp::Gt => {
                let holds = match float {
                    true => {
                        let predicate = match op {
                            SimdOp::Eq => FloatPredicate::OEQ,
                            SimdOp::Lt => FloatPredicate::OLT,
                            _ => FloatPredicate::OGT,
                        };
                        builder
                            .build_float_compare(predicate, a, b_vec(), "cmp")
                            .unwrap()
                    }
                    false => {
                        let predicate = match (op, signed) {
                            (SimdOp::Eq, _) => IntPredicate::EQ,
                            (SimdOp::Lt, true) => IntPredicate::SLT,
                            (SimdOp::Lt, false) => IntPredicate::ULT,
                            (_, true) => IntPredicate::SGT,
                            _ => IntPredicate::UGT,
                        };
                        builder
                            .build_int_compare(predicate, a, b_vec(), "cmp")
                            .unwrap()
                    }
                };
                let mask = self.vector_type(ty);
                self.builder
                    .build_int_s_extend(holds, mask, "mask")
                    .unwrap()
                    .into()
            }
            SimdOp::Select => {
                let zero = a.get_type().const_zero();
                let chosen = builder
                    .build_int_compare(IntPredicate::NE, a, zero, "chosen")
                    .unwrap();
                let otherwise = self.vector(&operands[2]);
                self.builder
                    .build_select(chosen, b_vec(), otherwise, "select")
                    .unwrap()
            }
            SimdOp::Shl | SimdOp::Shr => {
                let amount = self.operand(&operands[1]).imm().into_int_value();
                let lane = a.get_type().get_element_type().into_int_type();
                if self.mode.emit_safety_checks() {
                    self.check_shift(amount, lane);
                }
                let amount = self
                    .builder
                    .build_int_cast_sign_flag(amount, lane, false, "amount")
                    .unwrap();
                let lanes = a.get_type().get_size();
                let one = lane.vec_type(1).get_undef();
                let single = self
                    .builder
                    .build_insert_element(one, amount, self.context.i32_type().const_zero(), "one")
                    .unwrap();
                let spread = self.context.i32_type().vec_type(lanes).const_zero();
                let amounts = self
                    .builder
                    .build_shuffle_vector(single, one, spread, "amounts")
                    .unwrap();
                let builder = &self.builder;
                match op {
                    SimdOp::Shl => builder.build_left_shift(a, amounts, "shl"),
                    _ => builder.build_right_shift(a, amounts, signed, "shr"),
                }
                .unwrap()
                .into()
            }
            SimdOp::Sum if float => {
                let zero = a
                    .get_type()
                    .get_element_type()
                    .into_float_type()
                    .const_float(-0.0);
                self.intrinsic(
                    "llvm.vector.reduce.fadd",
                    a.get_type().into(),
                    &[zero.into(), a.into()],
                )
            }
            SimdOp::Sum => {
                self.intrinsic("llvm.vector.reduce.add", a.get_type().into(), &[a.into()])
            }
            SimdOp::Madd => {
                let lanes = a.get_type().get_size();
                let wide = self.context.i32_type().vec_type(lanes);
                let builder = &self.builder;
                let a = builder.build_int_s_extend(a, wide, "a").unwrap();
                let b = builder.build_int_s_extend(b_vec(), wide, "b").unwrap();
                let products = builder.build_int_mul(a, b, "products").unwrap();
                let pick = |first: u64| {
                    let indices: Vec<_> = (0..lanes as u64 / 2)
                        .map(|at| self.context.i32_type().const_int(2 * at + first, false))
                        .collect();
                    VectorType::const_vector(&indices)
                };
                let (even, odd) = (pick(0), pick(1));
                let even = builder
                    .build_shuffle_vector(products, products, even, "even")
                    .unwrap();
                let odd = builder
                    .build_shuffle_vector(products, products, odd, "odd")
                    .unwrap();
                builder.build_int_add(even, odd, "madd").unwrap().into()
            }
            SimdOp::Load | SimdOp::Store => unreachable!("handled apart"),
        }
    }
}
