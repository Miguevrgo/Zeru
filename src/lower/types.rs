//! LLVM types for the IR's types, and addressing inside aggregates.
//!
//! Layouts: `Vec<T>` is `{ *T, len, cap }`, a slice `{ *T, len }`, `T?` is
//! `{ i1, T }`, `T!E` is `{ i1, T, E }`, an enum whose variants carry
//! nothing is its `i32` tag, and one that carries values is `{ i32, [N x
//! i64] }`, the values of the variant held in the payload area.

use inkwell::{
    AddressSpace,
    types::{BasicType, BasicTypeEnum, IntType, PointerType, StructType},
    values::{BasicValueEnum, IntValue, PointerValue},
};

use crate::ty::{Ty, TyKind, Types};

use super::Lower;

impl<'ctx> Lower<'_, 'ctx> {
    pub(super) fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    pub(super) fn usize_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    /// The constant `value` of type `ty`. A negative value is sign-extended:
    /// LLVM keeps the bits it is given, and a wider pattern than the type
    /// would make two equal constants compare unequal when folded.
    pub(super) fn int_const(ty: IntType<'ctx>, value: i128) -> IntValue<'ctx> {
        ty.const_int(value as u64, value < 0)
    }

    /// Whether a `ty` is an SSA value rather than memory.
    pub(super) fn is_scalar(&self, ty: Ty) -> bool {
        match self.types().kind(ty) {
            TyKind::Int(_)
            | TyKind::F32
            | TyKind::F64
            | TyKind::Bool
            | TyKind::Ptr(_)
            | TyKind::Ref(_)
            | TyKind::RefMut(_) => true,
            TyKind::Adt(..) => self
                .program
                .enums
                .get(&ty)
                .is_some_and(|e| e.variants.iter().all(Vec::is_empty)),
            _ => false,
        }
    }

    pub(super) fn llvm_ty(&mut self, ty: Ty) -> BasicTypeEnum<'ctx> {
        if let Some(&llvm) = self.llvm_types.get(&ty) {
            return llvm;
        }
        let context = self.context;
        let llvm: BasicTypeEnum = match self.types().kind(ty).clone() {
            TyKind::Int(int) => context.custom_width_int_type(int.bits()).into(),
            TyKind::F32 => context.f32_type().into(),
            TyKind::F64 => context.f64_type().into(),
            TyKind::Bool => context.bool_type().into(),
            TyKind::Ptr(_) | TyKind::Ref(_) | TyKind::RefMut(_) => self.ptr_type().into(),
            TyKind::Vec(_) => {
                let usize_type = self.usize_type();
                context
                    .struct_type(
                        &[self.ptr_type().into(), usize_type.into(), usize_type.into()],
                        false,
                    )
                    .into()
            }
            TyKind::Slice(_) => context
                .struct_type(&[self.ptr_type().into(), self.usize_type().into()], false)
                .into(),
            TyKind::Optional(inner) => {
                let inner = self.llvm_ty(inner);
                context
                    .struct_type(&[context.bool_type().into(), inner], false)
                    .into()
            }
            TyKind::Result(ok, err) => {
                let (ok, err) = (self.llvm_ty(ok), self.llvm_ty(err));
                context
                    .struct_type(&[context.bool_type().into(), ok, err], false)
                    .into()
            }
            TyKind::Array(elem, len) => self.llvm_ty(elem).array_type(len as u32).into(),
            TyKind::Tuple(parts) => {
                let parts: Vec<BasicTypeEnum> = parts.iter().map(|&p| self.llvm_ty(p)).collect();
                context.struct_type(&parts, false).into()
            }
            TyKind::Adt(..) if self.program.structs.contains_key(&ty) => {
                let named = context.opaque_struct_type(&self.types().show(ty));
                self.llvm_types.insert(ty, named.into());
                let fields = self.program.structs[&ty].fields.clone();
                let fields: Vec<BasicTypeEnum> = fields.iter().map(|&f| self.llvm_ty(f)).collect();
                named.set_body(&fields, false);
                named.into()
            }
            TyKind::Adt(..) if self.is_scalar(ty) => context.i32_type().into(),
            TyKind::Adt(..) => {
                let variants = self.program.enums[&ty].variants.clone();
                let payloads: Vec<_> = variants
                    .iter()
                    .filter_map(|f| self.payload_type(f))
                    .collect();
                let words = payloads
                    .iter()
                    .map(|payload| self.target.get_abi_size(payload))
                    .max()
                    .unwrap_or(0)
                    .div_ceil(8);
                let area = context.i64_type().array_type(words as u32);
                context
                    .struct_type(&[context.i32_type().into(), area.into()], false)
                    .into()
            }
            TyKind::Void | TyKind::Param(_) | TyKind::Error => {
                unreachable!("{} has no values", self.types().show(ty))
            }
        };
        self.llvm_types.insert(ty, llvm);
        llvm
    }

    pub(super) fn struct_type(&mut self, ty: Ty) -> StructType<'ctx> {
        self.llvm_ty(ty).into_struct_type()
    }

    /// The values a variant carries, laid out as one struct in the payload
    /// area of its enum.
    pub(super) fn payload_type(&mut self, fields: &[Ty]) -> Option<StructType<'ctx>> {
        let fields: Vec<BasicTypeEnum> = fields.iter().map(|&f| self.llvm_ty(f)).collect();
        (!fields.is_empty()).then(|| self.context.struct_type(&fields, false))
    }

    pub(super) fn size_of(&mut self, ty: Ty) -> IntValue<'ctx> {
        self.llvm_ty(ty).size_of().unwrap()
    }

    /// The address of part `at` of the aggregate of type `ty` at `base`.
    pub(super) fn field_ptr(
        &mut self,
        base: PointerValue<'ctx>,
        ty: Ty,
        at: u32,
    ) -> PointerValue<'ctx> {
        let shape = self.struct_type(ty);
        self.builder
            .build_struct_gep(shape, base, at, "field")
            .unwrap()
    }

    pub(super) fn load_field(
        &mut self,
        base: PointerValue<'ctx>,
        ty: Ty,
        at: u32,
    ) -> BasicValueEnum<'ctx> {
        let shape = self.struct_type(ty);
        let field = self.field_ptr(base, ty, at);
        let field_type = shape.get_field_type_at_index(at).unwrap();
        self.builder.build_load(field_type, field, "field").unwrap()
    }

    /// The element type of an array, a Vec or a slice.
    pub(super) fn element_ty(&self, seq: Ty) -> Ty {
        match self.types().kind(seq) {
            TyKind::Array(elem, _) | TyKind::Vec(elem) | TyKind::Slice(elem) => *elem,
            _ => unreachable!("{} has no elements", self.types().show(seq)),
        }
    }

    /// The address of element `index` (a usize) of the sequence at `base`,
    /// unchecked.
    pub(super) fn element_ptr(
        &mut self,
        base: PointerValue<'ctx>,
        seq: Ty,
        index: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let elem = self.element_ty(seq);
        let elem_llvm = self.llvm_ty(elem);
        if let TyKind::Array(..) = self.types().kind(seq) {
            let array = self.llvm_ty(seq);
            let zero = self.usize_type().const_zero();
            return unsafe {
                self.builder
                    .build_in_bounds_gep(array, base, &[zero, index], "element")
                    .unwrap()
            };
        }
        let data = self.load_field(base, seq, 0).into_pointer_value();
        unsafe {
            self.builder
                .build_gep(elem_llvm, data, &[index], "element")
                .unwrap()
        }
    }

    /// The address of value `field` of `variant` in the enum at `base`.
    pub(super) fn variant_field_ptr(
        &mut self,
        base: PointerValue<'ctx>,
        ty: Ty,
        variant: u32,
        field: u32,
    ) -> PointerValue<'ctx> {
        let fields = self.program.enums[&ty].variants[variant as usize].clone();
        let payload = self
            .payload_type(&fields)
            .expect("the variant carries values");
        let area = self.field_ptr(base, ty, 1);
        self.builder
            .build_struct_gep(payload, area, field, "value")
            .unwrap()
    }

    pub(super) fn is_signed(&self, ty: Ty) -> bool {
        self.types().as_int(ty).is_none_or(|int| int.signed())
    }

    pub(super) fn is_unit(&self, ty: Ty) -> bool {
        ty == Types::VOID
    }
}
