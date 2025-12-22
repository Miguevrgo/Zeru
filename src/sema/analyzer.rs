use crate::{
    ast::{Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec},
    errors::{Span, ZeruError},
    sema::{
        symbol_table::SymbolTable,
        types::{FloatWidth, IntWidth, Signedness, Type},
    },
};
use std::collections::HashMap;

pub struct SemanticAnalyzer {
    pub errors: Vec<ZeruError>,

    symbols: SymbolTable,
    struct_defs: HashMap<String, Type>,
    enum_defs: HashMap<String, Type>,
    current_fn_return_type: Option<Type>,

    in_loop: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut symbols = SymbolTable::new();

        // Compiler intrinsics only - print/println/exit are now pure Zeru in std/builtin.zr
        // OutStream pointer type for __stdout()/__stderr()
        let ptr_outstream = Type::Pointer(Box::new(Type::Struct {
            name: "OutStream".to_string(),
            fields: vec![], // Will be resolved when OutStream is parsed
        }));

        symbols.insert_fn("__stdout".to_string(), vec![], ptr_outstream.clone());
        symbols.insert_fn("__stderr".to_string(), vec![], ptr_outstream);
        symbols.insert_fn(
            "__exit".to_string(),
            vec![Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W32,
            }],
            Type::Never, // noreturn
        );

        Self {
            errors: Vec::new(),
            symbols,
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            current_fn_return_type: None,
            in_loop: false,
        }
    }

    pub fn analyze(&mut self, program: &Program) {
        self.scan_types(&program.statements);
        self.scan_functions(&program.statements);
        self.analyze_bodies(&program.statements);
    }

    fn scan_types(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Struct { name, .. } => {
                    if self.struct_defs.contains_key(name) || self.enum_defs.contains_key(name) {
                        self.error(format!("Type {name} is already defined"), stmt.span);
                        continue;
                    }

                    let struct_type = Type::Struct {
                        name: name.clone(),
                        fields: vec![],
                    };

                    self.struct_defs.insert(name.clone(), struct_type);
                }
                StatementKind::Enum { name, variants } => {
                    if self.enum_defs.contains_key(name) || self.struct_defs.contains_key(name) {
                        self.error(format!("Type '{name}' is already defined"), stmt.span);
                        continue;
                    }

                    let enum_type = Type::Enum {
                        name: name.clone(),
                        variants: variants.clone(),
                    };
                    self.enum_defs.insert(name.clone(), enum_type);
                }
                _ => {}
            }
        }

        for stmt in stmts {
            if let StatementKind::Struct { name, fields, .. } = &stmt.kind {
                let mut resolved_fields = Vec::with_capacity(fields.len());
                for (f_name, f_type_spec) in fields {
                    let f_ty = self.resolve_spec(f_type_spec);
                    resolved_fields.push((f_name.clone(), f_ty));
                }

                if let Some(Type::Struct { fields: f, .. }) = self.struct_defs.get_mut(name) {
                    *f = resolved_fields;
                }
            }
        }
    }

    fn scan_functions(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Function {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    self.register_function(name.clone(), params, return_type, None);
                }

                StatementKind::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let StatementKind::Function {
                            name: method_name,
                            params,
                            return_type,
                            ..
                        } = &method.kind
                        {
                            let full_name = format!("{struct_name}::{method_name}");
                            self.register_function(
                                full_name,
                                params,
                                return_type,
                                Some(struct_name),
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn register_function(
        &mut self,
        name: String,
        params: &Vec<(String, TypeSpec, bool)>,
        return_type: &Option<TypeSpec>,
        associated_struct: Option<&str>,
    ) {
        if self.symbols.lookup_current_scope(&name).is_some() {
            self.error(
                format!("Function '{name}' is already defined"),
                Span::default(),
            );
            return;
        }

        if name == "main" {
            if !params.is_empty() {
                self.error(
                    "Function 'main' must not take arguments".into(),
                    Span::default(),
                );
            }

            if let Some(rt_spec) = return_type {
                let ret_ty = self.resolve_spec(rt_spec);
                if ret_ty != Type::Void {
                    self.error(
                        format!("Function 'main' must return void. Found {ret_ty:?}"),
                        Span::default(),
                    );
                }
            }
        }

        let mut param_types = Vec::new();
        for (param_name, type_spec, _is_mut) in params {
            if param_name == "self" {
                if let Some(struct_name) = associated_struct {
                    if let Some(ty) = self.struct_defs.get(struct_name) {
                        param_types.push(ty.clone())
                    } else {
                        param_types.push(Type::Unknown);
                        self.error(
                            "Self used in unknown struct context".into(),
                            Span::default(),
                        );
                    }
                } else {
                    self.error(
                        "'self' parameter allowed only in struct methods".into(),
                        Span::default(),
                    );
                    param_types.push(Type::Unknown);
                }
            } else {
                param_types.push(self.resolve_spec(type_spec));
            }
        }

        let ret_ty = if let Some(rt_spec) = return_type {
            self.resolve_spec(rt_spec)
        } else {
            Type::Void
        };

        self.symbols.insert_fn(name.clone(), param_types, ret_ty);
    }

    fn analyze_bodies(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Function {
                    name, params, body, ..
                } => {
                    self.check_function_body(name.clone(), params, body);
                }
                StatementKind::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let StatementKind::Function {
                            name: method_name,
                            params,
                            body,
                            ..
                        } = &method.kind
                        {
                            let full_name = format!("{struct_name}::{method_name}");
                            self.check_function_body(full_name, params, body);
                        }
                    }
                }
                StatementKind::Var { .. } => self.check_statement(stmt),
                _ => {}
            }
        }
    }

    fn check_function_body(
        &mut self,
        name: String,
        params: &[(String, TypeSpec, bool)],
        body: &[Statement],
    ) {
        let function_symbol = self.symbols.lookup(&name).unwrap().clone();

        if let super::symbol_table::Symbol::Function {
            ret_type,
            params: params_type_def,
        } = function_symbol
        {
            let prev_ret = self.current_fn_return_type.replace(ret_type);
            self.symbols.enter_scope();

            for (i, (param_name, _, is_mut)) in params.iter().enumerate() {
                let ty = params_type_def.get(i).unwrap_or(&Type::Unknown).clone();
                let is_const = !is_mut;
                self.symbols.insert_var(param_name.clone(), ty, is_const);
            }

            for s in body {
                self.check_statement(s);
            }

            self.symbols.exit_scope();
            self.current_fn_return_type = prev_ret;
        }
    }

    fn resolve_spec(&mut self, spec: &TypeSpec) -> Type {
        match spec {
            TypeSpec::Named(name) => self.resolve_named_type(name),
            TypeSpec::Generic { name, args } => {
                if name == "Array" && args.len() == 2 {
                    let elem_type = self.resolve_spec(&args[0]);

                    let len = if let TypeSpec::IntLiteral(val) = args[1] {
                        val as usize
                    } else {
                        self.error(
                            "Array length must be an integer literal".into(),
                            Span::default(),
                        );
                        0
                    };

                    return Type::Array {
                        elem_type: Box::new(elem_type),
                        len,
                    };
                }
                self.error(
                    format!("Unknown generic type or invalid args: {}", name),
                    Span::default(),
                );
                Type::Unknown
            }
            TypeSpec::IntLiteral(_) => {
                self.error(
                    "Unexpected integer literal in type position".into(),
                    Span::default(),
                );
                Type::Unknown
            }
            TypeSpec::Tuple(types) => {
                let resolved: Vec<Type> = types.iter().map(|t| self.resolve_spec(t)).collect();
                Type::Tuple(resolved)
            }
            TypeSpec::Pointer(inner) => {
                let elem_type = self.resolve_spec(inner);
                Type::Pointer(Box::new(elem_type))
            }
            TypeSpec::Optional(inner) => {
                let elem_type = self.resolve_spec(inner);
                Type::Optional(Box::new(elem_type))
            }
        }
    }

    fn resolve_named_type(&mut self, name: &str) -> Type {
        if let Some(ty) = self.struct_defs.get(name) {
            return ty.clone();
        }

        if let Some(ty) = self.enum_defs.get(name) {
            return ty.clone();
        }

        static PRIMITIVES: &[(&str, Signedness, IntWidth)] = &[
            ("i8", Signedness::Signed, IntWidth::W8),
            ("u8", Signedness::Unsigned, IntWidth::W8),
            ("i16", Signedness::Signed, IntWidth::W16),
            ("u16", Signedness::Unsigned, IntWidth::W16),
            ("i32", Signedness::Signed, IntWidth::W32),
            ("u32", Signedness::Unsigned, IntWidth::W32),
            ("i64", Signedness::Signed, IntWidth::W64),
            ("u64", Signedness::Unsigned, IntWidth::W64),
            ("isize", Signedness::Signed, IntWidth::WSize),
            ("usize", Signedness::Unsigned, IntWidth::WSize),
        ];
        for (type_name, signed, width) in PRIMITIVES {
            if name == *type_name {
                return Type::Integer {
                    signed: *signed,
                    width: *width,
                };
            }
        }

        match name {
            "f32" => return Type::Float(FloatWidth::W32),
            "f64" => return Type::Float(FloatWidth::W64),
            "bool" => return Type::Bool,
            "void" => return Type::Void,
            "self" => return Type::Unknown,
            _ => {}
        }

        let candidates: Vec<&str> = PRIMITIVES
            .iter()
            .map(|(n, _, _)| *n)
            .chain(["f32", "f64", "bool"].iter().copied())
            .chain(self.struct_defs.keys().map(|s| s.as_str()))
            .chain(self.enum_defs.keys().map(|s| s.as_str()))
            .collect();

        if let Some(suggestion) = self.find_closest_match(name, &candidates) {
            self.error(
                format!("Unknown type '{}'. Did you mean '{}'?", name, suggestion),
                Span::default(),
            );
        } else {
            self.error(format!("Unknown type '{}'", name), Span::default());
        }
        Type::Unknown
    }

    fn find_closest_match<'a>(&self, name: &str, candidates: &[&'a str]) -> Option<&'a str> {
        candidates
            .iter()
            .map(|c| (*c, Self::levenshtein_distance(name, c)))
            .filter(|(_, dist)| *dist <= 2)
            .min_by_key(|(_, dist)| *dist)
            .map(|(name, _)| name)
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                let expected_type = type_annotation.as_ref().map(|spec| self.resolve_spec(spec));

                let value_type = self.check_expression(value, expected_type.as_ref());

                let final_type = if let Some(expected) = expected_type {
                    let is_compatible = if expected.accepts(&value_type) {
                        true
                    } else {
                        match (&expected, &value_type) {
                            (Type::Float(FloatWidth::W64), Type::Float(FloatWidth::W32)) => true,
                            (
                                Type::Array {
                                    elem_type: t1,
                                    len: l1,
                                },
                                Type::Array {
                                    elem_type: t2,
                                    len: l2,
                                },
                            ) => {
                                if l1 != l2 {
                                    false
                                } else {
                                    t1.accepts(t2)
                                }
                            }
                            _ => false,
                        }
                    };

                    if !is_compatible && value_type != Type::Unknown {
                        self.error(format!(
                            "Type mismatch for variable '{name}. Annotated as {:?} but got {:?}",
                            expected.to_string(),
                            value_type.to_string()
                        ), stmt.span);
                    }

                    expected
                } else {
                    if value_type == Type::Unknown {
                        self.error(format!(
                            "Cannot infer type for variable '{}'. Please add a type annotation.",
                            name
                        ), stmt.span);
                    }
                    value_type
                };

                self.symbols.insert_var(name.clone(), final_type, *is_const);
            }

            StatementKind::Return(opt_expr) => {
                let expected = self.current_fn_return_type.clone();

                let expr_type = if let Some(expr) = opt_expr {
                    self.check_expression(expr, expected.as_ref())
                } else {
                    Type::Void
                };

                if let Some(expected) = expected {
                    if !expected.accepts(&expr_type) {
                        self.error(
                            format!(
                                "Invalid return type. Function expects {:?}, returning {:?}",
                                expected.to_string(),
                                expr_type.to_string()
                            ),
                            stmt.span,
                        );
                    }
                } else {
                    self.error(
                        "Return statement illegal if not inside a function".into(),
                        stmt.span,
                    );
                }
            }

            StatementKind::Block(stmts) => {
                self.symbols.enter_scope();
                for s in stmts {
                    self.check_statement(s);
                }
                self.symbols.exit_scope();
            }

            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expression(condition, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(
                        format!(
                            "If condition must be boolean, got {:?}",
                            cond_type.to_string()
                        ),
                        condition.span,
                    );
                }

                self.check_statement(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt);
                }
            }

            StatementKind::While { cond, body } => {
                let cond_type = self.check_expression(cond, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(
                        format!(
                            "While condition must be boolean, got: {:?}",
                            cond_type.to_string()
                        ),
                        cond.span,
                    );
                }

                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body);
                self.in_loop = prev_loop;
            }

            StatementKind::Break | StatementKind::Continue => {
                if !self.in_loop {
                    self.error(
                        "Break/Continue can only be used inside loops".into(),
                        stmt.span,
                    );
                }
            }

            StatementKind::Expression(expr) => {
                self.check_expression(expr, None);
            }

            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => {
                let iterable_type = self.check_expression(iterable, None);

                let item_type = match iterable_type {
                    Type::Array { elem_type, .. } => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(
                            format!("Type {:?} is not iterable.", iterable_type),
                            iterable.span,
                        );
                        Type::Unknown
                    }
                };

                let prev_loop = self.in_loop;
                self.in_loop = true;

                self.symbols.enter_scope();
                self.symbols.insert_var(variable.clone(), item_type, true);
                self.check_statement(body);

                self.symbols.exit_scope();
                self.in_loop = prev_loop;
            }

            StatementKind::Import { path: _, symbols } => {
                for sym in symbols {
                    if !self.struct_defs.contains_key(sym) && !self.enum_defs.contains_key(sym) {
                        self.struct_defs.insert(
                            sym.clone(),
                            Type::Struct {
                                name: sym.clone(),
                                fields: vec![],
                            },
                        );
                    }
                }
            }
            _ => {}
        }
    }

    fn check_expression(&mut self, expr: &Expression, expected_type: Option<&Type>) -> Type {
        match &expr.kind {
            ExpressionKind::Int(val) => {
                if let Some(Type::Integer { width, signed }) = expected_type {
                    if self.fits_in_int(*val, *width, *signed) {
                        return Type::Integer {
                            width: *width,
                            signed: *signed,
                        };
                    } else {
                        self.error(
                            format!(
                                "Literal {} does not fit in type {:?}",
                                val,
                                expected_type.unwrap()
                            ),
                            expr.span,
                        );
                    }
                }
                Type::Integer {
                    signed: Signedness::Signed,
                    width: IntWidth::W32,
                }
            }
            ExpressionKind::Float(_) => {
                if let Some(Type::Float(width)) = expected_type {
                    return Type::Float(*width);
                }
                Type::Float(FloatWidth::W32)
            }
            ExpressionKind::Boolean(_) => Type::Bool,
            ExpressionKind::StringLit(_) => Type::Pointer(Box::new(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W8,
            })),
            ExpressionKind::None => {
                if let Some(Type::Optional(inner)) = expected_type {
                    Type::Optional(inner.clone())
                } else if let Some(exp_type) = expected_type {
                    self.error(
                        format!(
                            "'None' can only be assigned to optional types, got {:?}",
                            exp_type
                        ),
                        expr.span,
                    );
                    Type::Unknown
                } else {
                    Type::Unknown
                }
            }

            ExpressionKind::Identifier(name) => {
                if name.contains("::") {
                    let parts: Vec<&str> = name.split("::").collect();

                    if parts.len() == 2 {
                        let enum_name = parts[0];
                        let variant_name = parts[1];
                        if let Some(Type::Enum { variants, .. }) = self.enum_defs.get(enum_name) {
                            if variants.contains(&variant_name.to_string()) {
                                return self.enum_defs.get(enum_name).unwrap().clone();
                            } else {
                                self.error(
                                    format!(
                                        "Enum '{}' has no variant '{}'",
                                        enum_name, variant_name
                                    ),
                                    expr.span,
                                );
                                return Type::Unknown;
                            }
                        }
                    }
                }

                if let Some(symbol) = self.symbols.lookup(name) {
                    match symbol {
                        super::symbol_table::Symbol::Var { ty, .. } => ty.clone(),
                        _ => {
                            self.error(
                                format!("'{name}' is a function, not a variable"),
                                expr.span,
                            );
                            Type::Unknown
                        }
                    }
                } else {
                    let suggestion = self.find_similar_variable(name);
                    if let Some(similar) = suggestion {
                        self.error(
                            format!(
                                "Undeclared variable '{}'. Did you mean '{}'?",
                                name, similar
                            ),
                            expr.span,
                        );
                    } else {
                        self.error(format!("Undeclared variable '{}'.", name), expr.span);
                    }
                    Type::Unknown
                }
            }

            ExpressionKind::Assign {
                target,
                operator: _,
                value,
            } => {
                let target_type = if let ExpressionKind::Identifier(name) = &target.kind {
                    if let Some(super::symbol_table::Symbol::Var { is_const, ty }) =
                        self.symbols.lookup(name).cloned()
                    {
                        if is_const {
                            self.error(
                                format!("Cannot reassign constant variable '{}'.", name),
                                expr.span,
                            );
                        }
                        Some(ty)
                    } else {
                        None
                    }
                } else if let ExpressionKind::Get { object, .. } = &target.kind
                    && let ExpressionKind::Identifier(obj_name) = &object.kind
                    && obj_name == "self"
                    && let Some(super::symbol_table::Symbol::Var { is_const, .. }) =
                        self.symbols.lookup("self")
                {
                    if *is_const {
                        self.error(
                            "Cannot modify field of immutable 'self'. Declare 'self' as mutable."
                                .to_string(),
                            expr.span,
                        );
                    }

                    Some(self.check_expression(target, None))
                } else {
                    Some(self.check_expression(target, None))
                };

                let val_type = self.check_expression(value, target_type.as_ref());

                if let Some(ty) = target_type
                    && !ty.accepts(&val_type)
                    && val_type != Type::Unknown
                {
                    self.error(
                        format!(
                            "Type mismatch in assignment. Expected {:?}, got {:?}.",
                            ty.to_string(),
                            val_type.to_string()
                        ),
                        expr.span,
                    );
                }
                Type::Void
            }

            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => {
                if *operator == crate::token::Token::DoubleColon
                    && let (
                        ExpressionKind::Identifier(enum_name),
                        ExpressionKind::Identifier(variant_name),
                    ) = (&left.kind, &right.kind)
                {
                    if let Some(Type::Enum { variants, .. }) = self.enum_defs.get(enum_name) {
                        if variants.contains(variant_name) {
                            return self.enum_defs.get(enum_name).unwrap().clone();
                        } else {
                            self.error(
                                format!("Enum '{}' has no variant '{}'", enum_name, variant_name),
                                expr.span,
                            );
                            return Type::Unknown;
                        }
                    } else {
                        self.error(format!("'{}' is not an enum type", enum_name), expr.span);
                        return Type::Unknown;
                    }
                }

                let l_ty = self.check_expression(left, expected_type);
                let r_ty = self.check_expression(right, Some(&l_ty));

                if l_ty == Type::Unknown || r_ty == Type::Unknown {
                    return Type::Unknown;
                }

                if let Type::Pointer(_) = &l_ty
                    && matches!(
                        operator,
                        crate::token::Token::Plus | crate::token::Token::Minus
                    )
                    && matches!(r_ty, Type::Integer { .. })
                {
                    return l_ty;
                }

                if !l_ty.accepts(&r_ty) {
                    self.error(format!("Binary operation '{operator:?}' requires operands of same type. Got {:?} and {:?}.", l_ty.to_string(), r_ty.to_string()), expr.span);
                    return Type::Unknown;
                }

                match operator {
                    crate::token::Token::Eq
                    | crate::token::Token::NotEq
                    | crate::token::Token::Lt
                    | crate::token::Token::Gt
                    | crate::token::Token::Leq
                    | crate::token::Token::Geq => Type::Bool,

                    _ => l_ty,
                }
            }

            ExpressionKind::Call {
                function,
                arguments,
            } => {
                if let ExpressionKind::Identifier(name) = &function.kind {
                    return self.check_call(name, arguments, None, expr.span);
                }

                if let ExpressionKind::Get {
                    object,
                    name: method_name,
                } = &function.kind
                {
                    let obj_type = self.check_expression(object, None);

                    let struct_name = match &obj_type {
                        Type::Struct { name, .. } => name.clone(),
                        Type::Pointer(elem_type) => {
                            if let Type::Struct { name, .. } = elem_type.as_ref() {
                                name.clone()
                            } else {
                                "".into()
                            }
                        }
                        _ => "".into(),
                    };

                    if struct_name.is_empty() {
                        self.error(
                            format!(
                                "Cannot call method on non-struct type {:?}",
                                obj_type.to_string()
                            ),
                            expr.span,
                        );
                        return Type::Unknown;
                    }
                    let full_name = format!("{struct_name}::{method_name}");
                    return self.check_call(&full_name, arguments, Some(obj_type), expr.span);
                }

                self.error("Invalid call expression".into(), expr.span);
                Type::Unknown
            }

            ExpressionKind::Get { object, name } => {
                let obj_type = self.check_expression(object, None);

                let actual_type = if let Type::Pointer(elem_type) = &obj_type {
                    elem_type
                } else {
                    &obj_type
                };

                if let Type::Struct {
                    name: struct_name, ..
                } = actual_type
                {
                    if let Some(Type::Struct { fields, .. }) = self.struct_defs.get(struct_name) {
                        for (f_name, f_type) in fields {
                            if f_name == name {
                                return f_type.clone();
                            }
                        }
                        self.error(
                            format!("Struct '{struct_name}' has no field '{name}'"),
                            expr.span,
                        );
                    }
                } else if obj_type != Type::Unknown {
                    self.error(
                        "Cannot access property on non-struct type.".into(),
                        expr.span,
                    );
                }
                Type::Unknown
            }

            ExpressionKind::StructLiteral { name, fields } => {
                if let Some(def) = self.struct_defs.get(name).cloned() {
                    if let Type::Struct {
                        fields: def_fields, ..
                    } = &def
                    {
                        for (field_name, _) in fields {
                            if !def_fields.iter().any(|(n, _)| n == field_name) {
                                self.error(
                                    format!("Unknown field '{}' in struct '{}'", field_name, name),
                                    expr.span,
                                );
                            }
                        }

                        for (def_name, def_type) in def_fields {
                            let found = fields.iter().find(|(n, _)| n == def_name);
                            if let Some((_, field_expr)) = found {
                                let expr_type = self.check_expression(field_expr, Some(def_type));
                                let types_match = match (def_type, &expr_type) {
                                    (Type::Float(_), Type::Integer { .. }) => false,
                                    (Type::Integer { .. }, Type::Float(_)) => false,
                                    _ => def_type.accepts(&expr_type),
                                };
                                if !types_match && expr_type != Type::Unknown {
                                    self.error(format!(
                                        "Type mismatch: Field '{}' in struct '{}' expected {:?}, got {:?}.",
                                        def_name,
                                        name,
                                        def_type.to_string(),
                                        expr_type.to_string()
                                    ), field_expr.span);
                                }
                            } else {
                                self.error(
                                    format!("Missing field '{def_name}' in struct literal {name}"),
                                    expr.span,
                                );
                            }
                        }
                        return def;
                    }
                } else {
                    self.error(format!("Unknown struct type '{name}'."), expr.span);
                }
                Type::Unknown
            }

            ExpressionKind::Match { value, arms } => {
                let _match_type = self.check_expression(value, None);

                if arms.is_empty() {
                    return Type::Void;
                }

                let first_arm_type = self.check_expression(&arms[0].1, expected_type);

                for (i, (_, result)) in arms.iter().enumerate().skip(1) {
                    let arm_type = self.check_expression(result, Some(&first_arm_type));
                    if !first_arm_type.accepts(&arm_type)
                        && arm_type != Type::Unknown
                        && first_arm_type != Type::Unknown
                    {
                        self.error(
                            format!(
                                "Match arm {} has inconsistent type. Expected {:?}, got {:?}",
                                i + 1,
                                first_arm_type.to_string(),
                                arm_type.to_string()
                            ),
                            result.span,
                        );
                    }
                }

                first_arm_type
            }
            ExpressionKind::Prefix { operator, right } => match operator {
                crate::token::Token::Minus => {
                    if let ExpressionKind::Int(val) = &right.kind
                        && let Some(Type::Integer { width, signed }) = expected_type
                    {
                        let negated = -val;
                        if self.fits_in_int(negated, *width, *signed) {
                            return Type::Integer {
                                width: *width,
                                signed: *signed,
                            };
                        } else {
                            self.error(
                                format!(
                                    "Literal {} does not fit in type {:?}",
                                    negated,
                                    expected_type.unwrap()
                                ),
                                expr.span,
                            );
                            return Type::Integer {
                                width: *width,
                                signed: *signed,
                            };
                        }
                    }

                    let right_type = self.check_expression(right, expected_type);
                    match &right_type {
                        Type::Integer { .. } | Type::Float(_) => right_type,
                        _ => {
                            self.error(
                                format!(
                                    "Cannot negate non-numeric type {:?}",
                                    right_type.to_string()
                                ),
                                expr.span,
                            );
                            Type::Unknown
                        }
                    }
                }
                crate::token::Token::Bang => {
                    let right_type = self.check_expression(right, None);
                    if right_type != Type::Bool && right_type != Type::Unknown {
                        self.error(
                            format!(
                                "Logical NOT requires bool, got {:?}",
                                right_type.to_string()
                            ),
                            expr.span,
                        );
                    }
                    Type::Bool
                }
                _ => self.check_expression(right, expected_type),
            },
            ExpressionKind::Cast { left, target } => {
                let _source_type = self.check_expression(left, None);

                if let ExpressionKind::Identifier(type_name) = &target.kind {
                    self.resolve_named_type(type_name)
                } else {
                    Type::Unknown
                }
            }
            ExpressionKind::Index { left, index } => {
                let left_type = self.check_expression(left, None);
                let _index_type = self.check_expression(
                    index,
                    Some(&Type::Integer {
                        signed: Signedness::Unsigned,
                        width: IntWidth::WSize,
                    }),
                );

                match left_type {
                    Type::Array { elem_type, .. } => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(
                            format!("Cannot index type {:?}", left_type.to_string()),
                            expr.span,
                        );
                        Type::Unknown
                    }
                }
            }
            ExpressionKind::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    if let Some(Type::Array { elem_type, len }) = expected_type {
                        return Type::Array {
                            elem_type: elem_type.clone(),
                            len: *len,
                        };
                    }
                    return Type::Array {
                        elem_type: Box::new(Type::Unknown),
                        len: 0,
                    };
                }

                let elem_hint = if let Some(Type::Array { elem_type, .. }) = expected_type {
                    Some(elem_type.as_ref())
                } else {
                    None
                };

                let first_type = self.check_expression(&elements[0], elem_hint);

                for (i, elem) in elements.iter().enumerate().skip(1) {
                    let elem_type = self.check_expression(elem, Some(&first_type));

                    if !first_type.accepts(&elem_type) {
                        self.error(
                            format!(
                                "Array element at index {} type mismatch. Expected {:?}, got {:?}.",
                                i,
                                first_type.to_string(),
                                elem_type.to_string()
                            ),
                            elem.span,
                        );
                    }
                }

                Type::Array {
                    elem_type: Box::new(first_type),
                    len: elements.len(),
                }
            }
            ExpressionKind::AddressOf(inner) => {
                let inner_type = self.check_expression(inner, None);

                match &inner.kind {
                    ExpressionKind::Identifier(_)
                    | ExpressionKind::Get { .. }
                    | ExpressionKind::Index { .. }
                    | ExpressionKind::Dereference(_) => {}
                    _ => {
                        self.error("Cannot take address of a temporary value".into(), expr.span);
                    }
                }

                Type::Pointer(Box::new(inner_type))
            }
            ExpressionKind::Dereference(inner) => {
                let inner_type = self.check_expression(inner, None);

                match inner_type {
                    Type::Pointer(elem_type) => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(
                            format!(
                                "Cannot dereference non-pointer type {:?}",
                                inner_type.to_string()
                            ),
                            expr.span,
                        );
                        Type::Unknown
                    }
                }
            }
            ExpressionKind::Tuple(elements) => {
                let expected_types = if let Some(Type::Tuple(types)) = expected_type {
                    Some(types.as_slice())
                } else {
                    None
                };

                let mut result_types = Vec::with_capacity(elements.len());

                for (i, elem) in elements.iter().enumerate() {
                    let expected = expected_types.and_then(|types| types.get(i));
                    let elem_type = self.check_expression(elem, expected);
                    result_types.push(elem_type);
                }

                if let Some(expected_types) = expected_types
                    && expected_types.len() != elements.len()
                {
                    self.error(
                        format!(
                            "Tuple has {} elements, but expected {}",
                            elements.len(),
                            expected_types.len()
                        ),
                        expr.span,
                    );
                }

                Type::Tuple(result_types)
            }
            ExpressionKind::InlineAsm {
                template: _,
                outputs,
                inputs,
                clobbers: _,
                is_volatile: _,
            } => {
                for operand in inputs {
                    self.check_expression(&operand.expr, None);
                }

                for operand in outputs {
                    let ty = self.check_expression(&operand.expr, None);
                    if !matches!(operand.expr.kind, ExpressionKind::Identifier(_)) {
                        self.error(
                            "Inline assembly output must be a variable".to_string(),
                            operand.expr.span,
                        );
                    }
                    let _ = ty;
                }
                Type::Integer {
                    signed: Signedness::Signed,
                    width: IntWidth::W64,
                }
            }
        }
    }

    fn check_call(
        &mut self,
        name: &str,
        args: &[Expression],
        implicit_self: Option<Type>,
        call_span: Span,
    ) -> Type {
        if let Some(super::symbol_table::Symbol::Function { params, ret_type }) =
            self.symbols.lookup(name).cloned()
        {
            let mut expected_args = params.clone();

            if let Some(self_type) = implicit_self
                && !expected_args.is_empty()
            {
                if !expected_args[0].accepts(&self_type) {
                    self.error(
                        format!(
                            "Method '{name}' called on wrong type. Expected {:?}, got {:?}",
                            expected_args[0].to_string(),
                            self_type.to_string()
                        ),
                        call_span,
                    );
                }
                expected_args.remove(0);
            }

            if args.len() != expected_args.len() {
                self.error(
                    format!(
                        "Function '{name}' expects {} arguments, got {}",
                        expected_args.len(),
                        args.len()
                    ),
                    call_span,
                );
            } else {
                for (i, expr) in args.iter().enumerate() {
                    let arg_type = self.check_expression(expr, Some(&expected_args[i]));
                    if !expected_args[i].accepts(&arg_type) {
                        self.error(format!("Argument {} type mismatch.", i + 1), expr.span);
                    }
                }
            }
            return ret_type;
        }

        self.error(format!("Function '{name}' not defined."), call_span);
        Type::Unknown
    }

    fn error(&mut self, msg: String, span: Span) {
        self.errors.push(ZeruError::semantic(msg, span, 0));
    }

    fn fits_in_int(&self, val: i64, width: IntWidth, signed: Signedness) -> bool {
        let (min, max) = match (width, signed) {
            (IntWidth::W8, Signedness::Unsigned) => (0, u8::MAX as i64),
            (IntWidth::W8, Signedness::Signed) => (i8::MIN as i64, i8::MAX as i64),
            (IntWidth::W16, Signedness::Unsigned) => (0, u16::MAX as i64),
            (IntWidth::W16, Signedness::Signed) => (i16::MIN as i64, i16::MAX as i64),
            (IntWidth::W32, Signedness::Unsigned) => (0, u32::MAX as i64),
            (IntWidth::W32, Signedness::Signed) => (i32::MIN as i64, i32::MAX as i64),
            _ => (i64::MIN, i64::MAX),
        };
        val >= min && val <= max
    }

    fn find_similar_variable(&self, name: &str) -> Option<String> {
        let mut best_match: Option<String> = None;
        let mut min_dist = usize::MAX;

        for scope in self.symbols.get_all_scopes() {
            for (var_name, symbol) in scope {
                if let super::symbol_table::Symbol::Var { .. } = symbol {
                    let dist = Self::levenshtein_distance(name, var_name);
                    if dist < min_dist && dist <= 2 {
                        min_dist = dist;
                        best_match = Some(var_name.clone());
                    }
                }
            }
        }

        best_match
    }

    fn levenshtein_distance(s1: &str, s2: &str) -> usize {
        let v1: Vec<char> = s1.chars().collect();
        let v2: Vec<char> = s2.chars().collect();
        let l1 = v1.len();
        let l2 = v2.len();

        let mut d = vec![vec![0; l2 + 1]; l1 + 1];

        for (i, d) in d.iter_mut().enumerate().take(l1 + 1) {
            d[0] = i;
        }
        for (j, d) in d.iter_mut().enumerate().take(l2 + 1) {
            d[0] = j;
        }

        for i in 1..=l1 {
            for j in 1..=l2 {
                let cost = if v1[i - 1] == v2[j - 1] { 0 } else { 1 };
                d[i][j] = std::cmp::min(
                    std::cmp::min(d[i - 1][j] + 1, d[i][j - 1] + 1),
                    d[i - 1][j - 1] + cost,
                );
            }
        }
        d[l1][l2]
    }
}
