//! Layout of the built-in aggregates (`Vec<T>`, `T?`, `T!`, slices). They have
//! no nominal LLVM type; which one a value is comes from the analyser's types.

use inkwell::{
    types::{BasicTypeEnum, StructType},
    values::{BasicValueEnum, StructValue},
};

use crate::codegen::compiler::Compiler;

/// `Vec<T>` — `{ *T, usize len, usize cap }`.
pub(super) const VEC_PTR: u32 = 0;
pub(super) const VEC_LEN: u32 = 1;
pub(super) const VEC_CAP: u32 = 2;

/// `T?` — `{ bool has_value, T }`.
pub(super) const OPTION_TAG: u32 = 0;
pub(super) const OPTION_VALUE: u32 = 1;

/// `T!` — `{ bool is_ok, T, i32 error_code }`.
pub(super) const RESULT_TAG: u32 = 0;
pub(super) const RESULT_VALUE: u32 = 1;
pub(super) const RESULT_ERR: u32 = 2;

/// `[]T` and `str` — `{ *T, usize len }`.
pub(super) const SLICE_PTR: u32 = 0;
pub(super) const SLICE_LEN: u32 = 1;

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn build_struct(
        &self,
        st: StructType<'ctx>,
        fields: &[BasicValueEnum<'ctx>],
        name: &str,
    ) -> StructValue<'ctx> {
        let mut val = st.get_undef();
        for (i, field) in fields.iter().enumerate() {
            val = self
                .builder
                .build_insert_value(val, *field, i as u32, name)
                .unwrap()
                .into_struct_value();
        }
        val
    }

    pub(super) fn option_type(&self, elem_ty: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        self.context
            .struct_type(&[self.context.bool_type().into(), elem_ty], false)
    }

    pub(super) fn result_type(&self, ok_ty: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.bool_type().into(),
                ok_ty,
                self.context.i32_type().into(),
            ],
            false,
        )
    }

    pub(super) fn slice_type(&self) -> StructType<'ctx> {
        self.context
            .struct_type(&[self.ptr_type().into(), self.usize_type().into()], false)
    }

    pub(super) fn vec_type(&self) -> StructType<'ctx> {
        let usize_type = self.usize_type();
        self.context.struct_type(
            &[self.ptr_type().into(), usize_type.into(), usize_type.into()],
            false,
        )
    }

    pub(super) fn build_option_some(&self, value: BasicValueEnum<'ctx>) -> StructValue<'ctx> {
        let opt_ty = self.option_type(value.get_type());
        let tag = self.context.bool_type().const_int(1, false);
        self.build_struct(opt_ty, &[tag.into(), value], "some")
    }

    /// An empty `T?`; the payload slot still needs some bit pattern.
    pub(super) fn build_option_none(&self, elem_ty: BasicTypeEnum<'ctx>) -> StructValue<'ctx> {
        let opt_ty = self.option_type(elem_ty);
        let tag = self.context.bool_type().const_int(0, false);
        self.build_struct(opt_ty, &[tag.into(), self.zero_value_for(elem_ty)], "none")
    }
}
