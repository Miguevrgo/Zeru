//! LLVM types for the analyser's types, struct layout, and signedness queries.

use inkwell::types::{BasicType, BasicTypeEnum};

use crate::{
    ast::Expression,
    codegen::compiler::Compiler,
    sema::types::{FloatWidth, IntWidth, Signedness, Type},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn compile_struct_body(&mut self, name: &str) {
        let fields = self.types.struct_fields(name);
        let field_types: Vec<_> = fields
            .iter()
            .filter_map(|(_, ty)| self.llvm_type_of(ty))
            .collect();
        let field_indices = (0..)
            .zip(fields)
            .map(|(at, (field, _))| (field.clone(), at))
            .collect();

        if let Some((struct_type, indices)) = self.struct_defs.get_mut(name) {
            struct_type.set_body(&field_types, false);
            *indices = field_indices;
        }
    }

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

    /// LLVM type for a type the analyser already resolved.
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
            Type::Struct(name) => self.struct_defs.get(name)?.0.as_basic_type_enum(),
            Type::Void | Type::ParamType(_) | Type::Unknown => return None,
        })
    }
}
