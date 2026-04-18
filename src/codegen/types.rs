//! LLVM type mapping, struct/enum layout, and signedness queries.

use std::collections::HashMap;

use inkwell::types::{BasicType, BasicTypeEnum};

use crate::{
    ast::{Expression, ExpressionKind, TypeSpec},
    codegen::compiler::Compiler,
    errors::Span,
    sema::types::{Signedness, Type},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn compile_struct_body(
        &mut self,
        name: &str,
        fields: &[(String, TypeSpec)],
        span: Span,
    ) {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();

        for (i, (field_name, field_spec)) in fields.iter().enumerate() {
            if let Some(ty) = self.get_llvm_type(field_spec) {
                field_types.push(ty);
                field_indices.insert(field_name.clone(), i as u32);
            } else {
                self.error(
                    format!("Compiler: Unknown type in struct field '{}'", field_name),
                    span,
                );
                return;
            }
        }

        if let Some((struct_type, indices)) = self.struct_defs.get_mut(name) {
            struct_type.set_body(&field_types, false);
            *indices = field_indices;
        }
    }

    pub(super) fn infer_type_from_expression(&self, expr: &Expression) -> TypeSpec {
        match &expr.kind {
            ExpressionKind::Int(_) => TypeSpec::Named("i32".to_string()),
            ExpressionKind::Float(_) => TypeSpec::Named("f64".to_string()),
            ExpressionKind::Boolean(_) => TypeSpec::Named("bool".to_string()),
            ExpressionKind::StringLit(_) => TypeSpec::Named("str".to_string()),
            ExpressionKind::Identifier(name) => {
                if let Some((_, ty, _)) = self.variables.get(name) {
                    self.llvm_type_to_type_spec(*ty)
                } else if let Some(val) = self.constants.get(name) {
                    self.llvm_type_to_type_spec(val.get_type())
                } else {
                    TypeSpec::Named("i32".to_string())
                }
            }
            _ => TypeSpec::Named("i32".to_string()),
        }
    }

    fn llvm_type_to_type_spec(&self, ty: BasicTypeEnum<'ctx>) -> TypeSpec {
        match ty {
            BasicTypeEnum::IntType(int_ty) => {
                let width = int_ty.get_bit_width();
                let name = match width {
                    1 => "bool",
                    8 => "i8",
                    16 => "i16",
                    32 => "i32",
                    64 => "i64",
                    _ => "i32",
                };
                TypeSpec::Named(name.to_string())
            }
            BasicTypeEnum::FloatType(float_ty) => {
                let name = if float_ty == self.context.f32_type() {
                    "f32"
                } else {
                    "f64"
                };
                TypeSpec::Named(name.to_string())
            }
            BasicTypeEnum::PointerType(_) => {
                TypeSpec::Pointer(Box::new(TypeSpec::Named("u8".to_string())))
            }
            BasicTypeEnum::StructType(st) => {
                TypeSpec::Named(st.get_name().unwrap().to_str().unwrap().to_string())
            }
            _ => TypeSpec::Named("i32".to_string()),
        }
    }

    pub(super) fn is_unsigned_expr(expr: &Expression) -> bool {
        if let Some(ty) = &expr.ty {
            return matches!(
                ty,
                Type::Integer {
                    signed: Signedness::Unsigned,
                    ..
                }
            );
        }
        false
    }

    pub(super) fn is_signed_integer(&self, expr: &Expression) -> Option<bool> {
        if let Some(ty) = &expr.ty {
            return Some(!matches!(
                ty,
                Type::Integer {
                    signed: Signedness::Unsigned,
                    ..
                }
            ));
        }
        if let ExpressionKind::Identifier(name) = &expr.kind
            && let Some((_, _, is_unsigned)) = self.variables.get(name)
        {
            return Some(!is_unsigned);
        }
        None
    }

    pub(super) fn is_unsigned_type(spec: &TypeSpec) -> bool {
        match spec {
            TypeSpec::Named(name) => {
                matches!(name.as_str(), "u8" | "u16" | "u32" | "u64" | "usize")
            }
            TypeSpec::Pointer(inner) => Self::is_unsigned_type(inner),
            TypeSpec::Tuple(_) | TypeSpec::Optional(_) | TypeSpec::Result(_) => false,
            TypeSpec::Generic { args, .. } => {
                args.first().map(Self::is_unsigned_type).unwrap_or(false)
            }
            TypeSpec::IntLiteral(_) => false,
            TypeSpec::Slice(_) => false,
            TypeSpec::Ref(inner) | TypeSpec::RefMut(inner) => Self::is_unsigned_type(inner),
        }
    }

    pub(super) fn expr_to_typespec(expr: &Expression) -> Option<TypeSpec> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => Some(TypeSpec::Named(name.clone())),
            ExpressionKind::Dereference(inner) => {
                Self::expr_to_typespec(inner).map(|t| TypeSpec::Pointer(Box::new(t)))
            }
            _ => None,
        }
    }

    pub(super) fn get_llvm_type(&self, spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        match spec {
            TypeSpec::Named(name) => {
                if let Some(substituted) = self.current_type_substitutions.get(name) {
                    return self.get_llvm_type(substituted);
                }
                self.get_named_llvm_type(name)
            }
            TypeSpec::Generic { name, args } => {
                if name == "Array" && args.len() == 2 {
                    let elem_type = self.get_llvm_type(&args[0])?;
                    let len = match &args[1] {
                        TypeSpec::IntLiteral(val) => *val as u32,
                        _ => return None,
                    };
                    return Some(elem_type.array_type(len).into());
                }
                // Vec<T> is represented as a struct { ptr: *T, len: usize, cap: usize }
                if name == "Vec" && args.len() == 1 {
                    let ptr_type = self
                        .context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into();
                    let usize_type = self.context.i64_type().into();
                    return Some(
                        self.context
                            .struct_type(&[ptr_type, usize_type, usize_type], false)
                            .into(),
                    );
                }
                if name == "Result" && args.len() == 2 {
                    let ok_type = self
                        .get_llvm_type(&args[0])
                        .unwrap_or(self.context.i8_type().into());
                    let err_type = self
                        .get_llvm_type(&args[1])
                        .unwrap_or(self.context.i8_type().into());
                    let tag_type = self.context.bool_type().into();
                    let data_type = if self.type_size(ok_type) >= self.type_size(err_type) {
                        ok_type
                    } else {
                        err_type
                    };
                    return Some(
                        self.context
                            .struct_type(&[tag_type, data_type], false)
                            .into(),
                    );
                }
                None
            }
            TypeSpec::IntLiteral(_) => None,
            TypeSpec::Tuple(types) => {
                let field_types: Vec<_> =
                    types.iter().filter_map(|t| self.get_llvm_type(t)).collect();
                Some(self.context.struct_type(&field_types, false).into())
            }
            TypeSpec::Pointer(_) => Some(
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ),
            TypeSpec::Optional(inner) => {
                let inner_type = self.get_llvm_type(inner)?;
                let tag_type = self.context.bool_type().into();
                Some(
                    self.context
                        .struct_type(&[tag_type, inner_type], false)
                        .into(),
                )
            }
            TypeSpec::Result(inner) => {
                let inner_type = self.get_llvm_type(inner)?;
                let tag_type = self.context.bool_type().into();
                let error_code_type = self.context.i32_type().into();
                Some(
                    self.context
                        .struct_type(&[tag_type, inner_type, error_code_type], false)
                        .into(),
                )
            }
            TypeSpec::Slice(_) => {
                let ptr_type = self
                    .context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into();
                let len_type = self.context.i64_type().into();
                Some(
                    self.context
                        .struct_type(&[ptr_type, len_type], false)
                        .into(),
                )
            }
            // &T and &var T are represented as pointers (with gen-ref metadata at runtime)
            // For now, they compile down to simple pointers. Generational reference
            // checking will be added in a later phase.
            TypeSpec::Ref(_) | TypeSpec::RefMut(_) => Some(
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ),
        }
    }

    fn type_size(&self, ty: BasicTypeEnum<'ctx>) -> u64 {
        match ty {
            BasicTypeEnum::IntType(t) => t.get_bit_width() as u64,
            BasicTypeEnum::FloatType(t) => {
                if t == self.context.f32_type() {
                    32
                } else {
                    64
                }
            }
            BasicTypeEnum::PointerType(_) => 64,
            BasicTypeEnum::ArrayType(t) => t.len() as u64 * self.type_size(t.get_element_type()),
            BasicTypeEnum::StructType(t) => {
                t.get_field_types().iter().map(|f| self.type_size(*f)).sum()
            }
            BasicTypeEnum::VectorType(t) => t.get_size() as u64,
            BasicTypeEnum::ScalableVectorType(_) => 64,
        }
    }

    fn get_named_llvm_type(&self, name: &str) -> Option<BasicTypeEnum<'ctx>> {
        let int_type = match name {
            "i8" | "u8" => Some(self.context.i8_type()),
            "i16" | "u16" => Some(self.context.i16_type()),
            "i32" | "u32" => Some(self.context.i32_type()),
            "i64" | "u64" | "isize" | "usize" => Some(self.context.i64_type()),
            _ => None,
        };
        if let Some(ty) = int_type {
            return Some(ty.into());
        }

        match name {
            "f32" => return Some(self.context.f32_type().into()),
            "f64" => return Some(self.context.f64_type().into()),
            "bool" => return Some(self.context.bool_type().into()),
            "void" => return None,
            "self" => {
                return self
                    .current_struct_context
                    .as_ref()
                    .and_then(|struct_name| self.struct_defs.get(struct_name))
                    .map(|(st, _)| st.as_basic_type_enum());
            }
            _ => {}
        }

        if let Some((struct_ty, _)) = self.struct_defs.get(name) {
            return Some(struct_ty.as_basic_type_enum());
        }
        if self.enum_defs.contains_key(name) {
            return Some(self.context.i32_type().into());
        }

        None
    }
}
