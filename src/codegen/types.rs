//! LLVM type mapping, struct/enum layout, and signedness queries.

use std::collections::HashMap;

use inkwell::types::{BasicType, BasicTypeEnum};

use crate::{
    ast::{Expression, ExpressionKind, TypeSpec},
    codegen::compiler::Compiler,
    errors::Span,
    sema::types::{FloatWidth, IntWidth, Signedness, Type},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn compile_struct_body(
        &mut self,
        name: &str,
        fields: &[(String, TypeSpec)],
        span: Span,
    ) {
        let mut field_types = Vec::with_capacity(fields.len());
        let mut field_indices = HashMap::with_capacity(fields.len());

        for (i, (field_name, field_spec)) in fields.iter().enumerate() {
            let Some(ty) = self.get_llvm_type(field_spec) else {
                self.error(format!("Unknown type in struct field '{field_name}'"), span);
                return;
            };
            field_types.push(ty);
            field_indices.insert(field_name.clone(), i as u32);
        }

        if let Some((struct_type, indices)) = self.struct_defs.get_mut(name) {
            struct_type.set_body(&field_types, false);
            *indices = field_indices;
        }
    }

    /// Type argument for a generic call, inferred from the argument expression.
    pub(super) fn infer_type_from_expression(&self, expr: &Expression) -> TypeSpec {
        let named = |name: &str| TypeSpec::Named(name.to_string());

        // The analyser already resolved this. Re-deriving it from the shape of
        // the expression reads i32 out of anything but a literal or a plain
        // variable, so a field or a call picks the wrong instantiation.
        if let Some(ty) = &expr.ty
            && *ty != Type::Unknown
        {
            return ty.to_spec();
        }

        match &expr.kind {
            ExpressionKind::Int(_) => named("i32"),
            ExpressionKind::Float(_) => named("f64"),
            ExpressionKind::Boolean(_) => named("bool"),
            ExpressionKind::StringLit(_) => named("str"),
            ExpressionKind::Identifier(name) => {
                let llvm_type = match self.variables.get(name) {
                    Some((_, ty, _)) => Some(*ty),
                    None => self.constants.get(name).map(|val| val.get_type()),
                };
                llvm_type.map_or_else(|| named("i32"), |ty| self.llvm_type_to_type_spec(ty))
            }
            _ => named("i32"),
        }
    }

    fn llvm_type_to_type_spec(&self, ty: BasicTypeEnum<'ctx>) -> TypeSpec {
        let named = |name: &str| TypeSpec::Named(name.to_string());

        match ty {
            BasicTypeEnum::IntType(t) => named(match t.get_bit_width() {
                1 => "bool",
                8 => "i8",
                16 => "i16",
                64 => "i64",
                _ => "i32",
            }),
            BasicTypeEnum::FloatType(t) if t == self.context.f32_type() => named("f32"),
            BasicTypeEnum::FloatType(_) => named("f64"),
            BasicTypeEnum::PointerType(_) => TypeSpec::Pointer(Box::new(named("u8"))),
            BasicTypeEnum::StructType(st) => match st.get_name().and_then(|n| n.to_str().ok()) {
                Some(name) => named(name),
                None => named("i32"),
            },
            _ => named("i32"),
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

    /// Whether `expr` is a signed integer, falling back to the variable table
    /// when the analyser left no type behind. `None` when nothing is known.
    pub(super) fn is_signed_integer(&self, expr: &Expression) -> Option<bool> {
        if let Some(ty) = &expr.ty {
            return Some(!Self::is_unsigned(ty));
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
            TypeSpec::Pointer(inner) | TypeSpec::Ref(inner) | TypeSpec::RefMut(inner) => {
                Self::is_unsigned_type(inner)
            }
            TypeSpec::Generic { args, .. } => args.first().is_some_and(Self::is_unsigned_type),
            _ => false,
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
            TypeSpec::Named(name) => match self.current_type_substitutions.get(name) {
                Some(substituted) => self.get_llvm_type(substituted),
                None => self.get_named_llvm_type(name),
            },

            TypeSpec::Generic { name, args } => match (name.as_str(), args.as_slice()) {
                ("Array", [elem, TypeSpec::IntLiteral(len)]) => {
                    Some(self.get_llvm_type(elem)?.array_type(*len as u32).into())
                }
                ("Vec", [_]) => Some(self.vec_type().into()),
                // `Result<T, E>` shares the layout of `T!`: the error is always
                // an i32 code, so `E` carries no representation of its own.
                ("Result", [ok, _]) => Some(self.result_type(self.get_llvm_type(ok)?).into()),
                _ => None,
            },

            TypeSpec::Optional(inner) => Some(self.option_type(self.get_llvm_type(inner)?).into()),
            TypeSpec::Result(inner) => Some(self.result_type(self.get_llvm_type(inner)?).into()),
            TypeSpec::Slice(_) => Some(self.slice_type().into()),

            TypeSpec::Tuple(types) => {
                let fields: Vec<_> = types.iter().filter_map(|t| self.get_llvm_type(t)).collect();
                Some(self.context.struct_type(&fields, false).into())
            }

            // References lower to plain pointers until generational checks land.
            TypeSpec::Pointer(_) | TypeSpec::Ref(_) | TypeSpec::RefMut(_) => {
                Some(self.ptr_type().into())
            }

            TypeSpec::IntLiteral(_) => None,
        }
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
            Type::Enum { .. } => self.context.i32_type().into(),
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
            Type::Struct { name, .. } => self.struct_defs.get(name)?.0.as_basic_type_enum(),
            Type::Void | Type::ParamType(_) | Type::Unknown => return None,
        })
    }

    fn get_named_llvm_type(&self, name: &str) -> Option<BasicTypeEnum<'ctx>> {
        let primitive = match name {
            "i8" | "u8" => Some(self.context.i8_type().into()),
            "i16" | "u16" => Some(self.context.i16_type().into()),
            "i32" | "u32" => Some(self.context.i32_type().into()),
            "i64" | "u64" | "isize" | "usize" => Some(self.usize_type().into()),
            "f32" => Some(self.context.f32_type().into()),
            "f64" => Some(self.context.f64_type().into()),
            "bool" => Some(self.context.bool_type().into()),
            _ => None,
        };
        if primitive.is_some() {
            return primitive;
        }

        let struct_name = match name {
            "void" => return None,
            "self" => self.current_struct_context.as_deref()?,
            other => other,
        };

        if let Some((struct_ty, _)) = self.struct_defs.get(struct_name) {
            return Some(struct_ty.as_basic_type_enum());
        }
        // Enum variants are plain tags.
        self.enum_defs
            .contains_key(struct_name)
            .then(|| self.context.i32_type().into())
    }
}
