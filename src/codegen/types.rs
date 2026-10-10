//! LLVM types for the analyser's types, struct layout, and signedness queries.

use inkwell::types::{BasicType, BasicTypeEnum, StructType};

use crate::{
    ast::Expression,
    codegen::compiler::Compiler,
    sema::types::{FloatWidth, IntWidth, Signedness, Type},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn is_unsigned(ty: &Type) -> bool {
        matches!(
            ty,
            Type::Integer {
                signed: Signedness::Unsigned,
                ..
            }
        )
    }

    pub(super) fn is_unsigned_expr(expr: &Expression) -> bool {
        expr.ty.as_ref().is_some_and(Self::is_unsigned)
    }

    /// The values a variant carries, laid out as one struct in the payload
    /// area of its enum.
    pub(super) fn payload_type(&self, fields: &[Type]) -> Option<StructType<'ctx>> {
        let types: Vec<_> = fields
            .iter()
            .filter_map(|ty| self.llvm_type_of(ty))
            .collect();
        (!types.is_empty()).then(|| self.context.struct_type(&types, false))
    }

    pub(super) fn llvm_type_of(&self, ty: &Type) -> Option<BasicTypeEnum<'ctx>> {
        Some(match ty {
            Type::Integer { width, .. } => match width {
                IntWidth::W8 => self.context.i8_type().into(),
                IntWidth::W16 => self.context.i16_type().into(),
                IntWidth::W32 => self.context.i32_type().into(),
                IntWidth::W64 | IntWidth::WSize => self.usize_type().into(),
            },
            Type::Float(FloatWidth::W32) => self.context.f32_type().into(),
            Type::Float(FloatWidth::W64) => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Enum(name) if self.types.enum_has_data(name) => {
                let words = self
                    .types
                    .enum_variants(name)?
                    .iter()
                    .filter_map(|(_, fields)| self.payload_type(fields))
                    .map(|payload| self.target.get_abi_size(&payload))
                    .max()
                    .unwrap_or(0)
                    .div_ceil(8);
                let area = self.context.i64_type().array_type(words as u32);
                let tag = self.context.i32_type();
                self.context
                    .struct_type(&[tag.into(), area.into()], false)
                    .into()
            }
            Type::Enum(_) => self.context.i32_type().into(),
            Type::Pointer(_) | Type::Ref(_) | Type::RefMut(_) => self.ptr_type().into(),
            Type::Slice { .. } => self.slice_type().into(),
            Type::Vec { .. } => self.vec_type().into(),
            Type::Optional(inner) => self.option_type(self.llvm_type_of(inner)?).into(),
            Type::Result { ok_type, .. } => self.result_type(self.llvm_type_of(ok_type)?).into(),
            Type::Array { elem_type, len } => {
                self.llvm_type_of(elem_type)?.array_type(*len as u32).into()
            }
            Type::Tuple(types) => {
                let fields: Vec<_> = types.iter().filter_map(|t| self.llvm_type_of(t)).collect();
                self.context.struct_type(&fields, false).into()
            }
            Type::Struct(name) => {
                let struct_type = *self.struct_defs.get(name)?;
                if struct_type.is_opaque() {
                    let fields: Vec<_> = self
                        .types
                        .struct_fields(name)
                        .iter()
                        .filter_map(|(_, ty)| self.llvm_type_of(ty))
                        .collect();
                    struct_type.set_body(&fields, false);
                }
                struct_type.as_basic_type_enum()
            }
            Type::Void | Type::ParamType(_) | Type::Unknown => return None,
        })
    }
}
