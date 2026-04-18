use crate::{
    ast::{Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec},
    errors::{Span, ZeruError},
    sema::{
        symbol_table::SymbolTable,
        types::{FloatWidth, IntWidth, Signedness, Type},
    },
};
use std::collections::HashMap;

type TraitMethod = (String, Vec<Type>, Option<Type>);

enum CallKind {
    Named(String),
    Method {
        method_name: String,
        is_vec_static: bool,
    },
    Unknown,
}

pub struct SemanticAnalyzer {
    pub errors: Vec<ZeruError>,

    symbols: SymbolTable,
    struct_defs: HashMap<String, Type>,
    enum_defs: HashMap<String, Type>,
    trait_defs: HashMap<String, Vec<TraitMethod>>,
    current_fn_return_type: Option<Type>,
    current_type_params: Vec<String>,

    in_loop: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut symbols = SymbolTable::new();

        let str_type = Type::Slice {
            elem_type: Box::new(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W8,
            }),
        };

        symbols.insert_fn("print".to_string(), vec![str_type.clone()], Type::Void);
        symbols.insert_fn("println".to_string(), vec![str_type.clone()], Type::Void);
        symbols.insert_fn("eprint".to_string(), vec![str_type.clone()], Type::Void);
        symbols.insert_fn("eprintln".to_string(), vec![str_type], Type::Void);

        Self {
            errors: Vec::new(),
            symbols,
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            current_fn_return_type: None,
            current_type_params: Vec::new(),
            in_loop: false,
        }
    }

    pub fn analyze(&mut self, program: &mut Program) {
        self.scan_types(&program.statements);
        self.scan_functions(&program.statements);
        self.analyze_bodies(&mut program.statements);
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
                StatementKind::Trait { name, methods } => {
                    if self.trait_defs.contains_key(name) {
                        self.error(format!("Trait '{name}' is already defined"), stmt.span);
                        continue;
                    }

                    let mut trait_methods = Vec::new();
                    for method in methods {
                        let param_types: Vec<Type> = method
                            .params
                            .iter()
                            .map(|(_, ty, _)| self.resolve_spec(ty))
                            .collect();
                        let ret_type = method.return_type.as_ref().map(|t| self.resolve_spec(t));
                        trait_methods.push((method.name.clone(), param_types, ret_type));
                    }
                    self.trait_defs.insert(name.clone(), trait_methods);
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
                    type_params,
                    ..
                } => {
                    self.register_function(name.clone(), params, return_type, None, type_params);
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
                            type_params,
                            ..
                        } = &method.kind
                        {
                            let full_name = format!("{struct_name}::{method_name}");
                            self.register_function(
                                full_name,
                                params,
                                return_type,
                                Some(struct_name),
                                type_params,
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
        type_params: &[crate::ast::TypeParameter],
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

        let prev_type_params = std::mem::take(&mut self.current_type_params);
        self.current_type_params = type_params.iter().map(|tp| tp.name.clone()).collect();

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

        self.current_type_params = prev_type_params;

        self.symbols.insert_fn(name.clone(), param_types, ret_ty);
    }

    fn analyze_bodies(&mut self, stmts: &mut [Statement]) {
        for stmt in stmts.iter_mut() {
            match &stmt.kind {
                StatementKind::Function { .. }
                | StatementKind::Struct { .. }
                | StatementKind::Var { .. } => {}
                _ => {}
            }
            self.check_statement_top_level(stmt);
        }
    }

    fn check_statement_top_level(&mut self, stmt: &mut Statement) {
        match &stmt.kind {
            StatementKind::Function {
                name, type_params, ..
            } => {
                let name = name.clone();
                let type_params = type_params.clone();
                if let StatementKind::Function { params, body, .. } = &mut stmt.kind {
                    self.check_function_body(&name, params, body, &type_params);
                }
            }
            StatementKind::Struct {
                name: struct_name, ..
            } => {
                let struct_name = struct_name.clone();
                if let StatementKind::Struct { methods, .. } = &mut stmt.kind {
                    for method in methods.iter_mut() {
                        if let StatementKind::Function {
                            name: method_name,
                            type_params,
                            ..
                        } = &method.kind
                        {
                            let full_name = format!("{struct_name}::{}", method_name.clone());
                            let type_params = type_params.clone();
                            if let StatementKind::Function { params, body, .. } = &mut method.kind {
                                self.check_function_body(&full_name, params, body, &type_params);
                            }
                        }
                    }
                }
            }
            StatementKind::Var { .. } => self.check_statement(stmt),
            _ => {}
        }
    }

    fn check_function_body(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
        body: &mut [Statement],
        type_params: &[crate::ast::TypeParameter],
    ) {
        let Some(function_symbol) = self.symbols.lookup(name).cloned() else {
            return;
        };

        if let super::symbol_table::Symbol::Function {
            ret_type,
            params: params_type_def,
        } = function_symbol
        {
            let prev_ret = self.current_fn_return_type.replace(ret_type);
            let prev_type_params = std::mem::take(&mut self.current_type_params);
            self.current_type_params = type_params.iter().map(|tp| tp.name.clone()).collect();
            self.symbols.enter_scope();

            for (i, (param_name, _, is_mut)) in params.iter().enumerate() {
                let ty = params_type_def.get(i).unwrap_or(&Type::Unknown).clone();
                let is_const = !is_mut;
                self.symbols.insert_var(param_name.clone(), ty, is_const);
            }

            for s in body.iter_mut() {
                self.check_statement(s);
            }

            self.symbols.exit_scope();
            self.current_type_params = prev_type_params;
            self.current_fn_return_type = prev_ret;
        }
    }

    /// Resolves a TypeSpec from the AST into a concrete Type.
    /// This handles named types (structs, enums), pointers, tuples, optionals, etc.
    /// Returns Type::Unknown if the type cannot be resolved.
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
                if name == "Vec" && args.len() == 1 {
                    let elem_type = self.resolve_spec(&args[0]);
                    return Type::Vec {
                        elem_type: Box::new(elem_type),
                    };
                }
                if name == "Result" && args.len() == 2 {
                    let ok_type = self.resolve_spec(&args[0]);
                    let err_type = self.resolve_spec(&args[1]);
                    return Type::Result {
                        ok_type: Box::new(ok_type),
                        err_type: Box::new(err_type),
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
            TypeSpec::Result(inner) => {
                let ok_type = self.resolve_spec(inner);
                Type::Result {
                    ok_type: Box::new(ok_type),
                    err_type: Box::new(Type::Struct {
                        name: "Error".to_string(),
                        fields: vec![(
                            "code".to_string(),
                            Type::Integer {
                                signed: crate::sema::types::Signedness::Signed,
                                width: crate::sema::types::IntWidth::W32,
                            },
                        )],
                    }),
                }
            }
            TypeSpec::Slice(inner) => {
                let elem_type = self.resolve_spec(inner);
                Type::Slice {
                    elem_type: Box::new(elem_type),
                }
            }
            TypeSpec::Ref(inner) => {
                let elem_type = self.resolve_spec(inner);
                Type::Ref(Box::new(elem_type))
            }
            TypeSpec::RefMut(inner) => {
                let elem_type = self.resolve_spec(inner);
                Type::RefMut(Box::new(elem_type))
            }
        }
    }

    fn resolve_named_type(&mut self, name: &str) -> Type {
        if self.current_type_params.contains(&name.to_string()) {
            return Type::ParamType(name.to_string());
        }

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

    fn check_statement(&mut self, stmt: &mut Statement) {
        let span = stmt.span;
        match &mut stmt.kind {
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
                        ), span);
                    }

                    expected
                } else {
                    if value_type == Type::Unknown {
                        self.error(format!(
                            "Cannot infer type for variable '{}'. Please add a type annotation.",
                            name
                        ), span);
                    }
                    value_type.clone()
                };

                // If initializing from an identifier with move semantics, mark source as moved
                if final_type.has_move_semantics()
                    && let ExpressionKind::Identifier(source_name) = &value.kind
                {
                    self.symbols.mark_moved(source_name);
                }

                self.symbols.insert_var(name.clone(), final_type, *is_const);
            }

            StatementKind::Return(opt_expr) => {
                let expected = self.current_fn_return_type.clone();

                let expr_type = if let Some(expr) = opt_expr {
                    let ty = self.check_expression(expr, expected.as_ref());

                    if ty.has_move_semantics()
                        && let ExpressionKind::Identifier(var_name) = &expr.kind
                    {
                        self.symbols.mark_moved(var_name);
                    }
                    ty
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
                            span,
                        );
                    }
                } else {
                    self.error(
                        "Return statement illegal if not inside a function".into(),
                        span,
                    );
                }
            }

            StatementKind::Block(stmts) => {
                self.symbols.enter_scope();
                for s in stmts.iter_mut() {
                    self.check_statement(s);
                }
                self.symbols.exit_scope();
            }

            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_span = condition.span;
                let cond_type = self.check_expression(condition, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(
                        format!(
                            "If condition must be boolean, got {:?}",
                            cond_type.to_string()
                        ),
                        cond_span,
                    );
                }

                self.check_statement(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt);
                }
            }

            StatementKind::While { cond, body } => {
                let cond_span = cond.span;
                let cond_type = self.check_expression(cond, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(
                        format!(
                            "While condition must be boolean, got: {:?}",
                            cond_type.to_string()
                        ),
                        cond_span,
                    );
                }

                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body);
                self.in_loop = prev_loop;
            }

            StatementKind::Break | StatementKind::Continue if !self.in_loop => {
                self.error("Break/Continue can only be used inside loops".into(), span);
            }
            StatementKind::Break | StatementKind::Continue => {}

            StatementKind::Expression(expr) => {
                self.check_expression(expr, None);
            }

            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => {
                let iter_span = iterable.span;
                let iterable_type = self.check_expression(iterable, None);

                let item_type = match iterable_type {
                    Type::Array { elem_type, .. } => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(
                            format!("Type {:?} is not iterable.", iterable_type),
                            iter_span,
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
            _ => {}
        }
    }

    /// Type-checks an expression, writes the resolved type into `expr.ty`, and returns it.
    ///
    /// This is the core type-checking function that recursively validates
    /// expressions and ensures type safety throughout the program.
    ///
    /// # Arguments
    /// * `expr` - The expression to type-check (mutably, so `expr.ty` is populated)
    /// * `expected_type` - Optional hint for expected type (from context like assignment)
    ///
    /// # Returns
    /// The inferred type, or `Type::Unknown` if type checking fails
    fn check_expression(&mut self, expr: &mut Expression, expected_type: Option<&Type>) -> Type {
        let ty = self.check_expression_inner(expr, expected_type);
        expr.ty = Some(ty.clone());
        ty
    }

    fn check_expression_inner(
        &mut self,
        expr: &mut Expression,
        expected_type: Option<&Type>,
    ) -> Type {
        let span = expr.span;
        match &mut expr.kind {
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
                            span,
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
            ExpressionKind::StringLit(_) => Type::Slice {
                elem_type: Box::new(Type::Integer {
                    signed: Signedness::Unsigned,
                    width: IntWidth::W8,
                }),
            },
            ExpressionKind::None => {
                if let Some(Type::Optional(inner)) = expected_type {
                    Type::Optional(inner.clone())
                } else if let Some(exp_type) = expected_type {
                    self.error(
                        format!(
                            "'None' can only be assigned to optional types, got {:?}",
                            exp_type
                        ),
                        span,
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
                        let enum_name = parts[0].to_string();
                        let variant_name = parts[1].to_string();
                        if let Some(Type::Enum { variants, .. }) = self.enum_defs.get(&enum_name) {
                            if variants.contains(&variant_name) {
                                return self.enum_defs.get(&enum_name).cloned().unwrap();
                            } else {
                                self.error(
                                    format!(
                                        "Enum '{}' has no variant '{}'",
                                        enum_name, variant_name
                                    ),
                                    span,
                                );
                                return Type::Unknown;
                            }
                        }
                    }
                }

                if let Some(symbol) = self.symbols.lookup(name).cloned() {
                    match symbol {
                        super::symbol_table::Symbol::Var { ty, is_moved, .. } => {
                            if is_moved {
                                self.error(
                                    format!(
                                        "Use of moved value '{}'. Value was previously moved and is no longer valid.",
                                        name
                                    ),
                                    span,
                                );
                                return Type::Unknown;
                            }
                            ty
                        }
                        _ => {
                            self.error(format!("'{name}' is a function, not a variable"), span);
                            Type::Unknown
                        }
                    }
                } else {
                    let name_owned = name.clone();
                    let suggestion = self.find_similar_variable(&name_owned);
                    if let Some(similar) = suggestion {
                        self.error(
                            format!(
                                "Undeclared variable '{}'. Did you mean '{}'?",
                                name_owned, similar
                            ),
                            span,
                        );
                    } else {
                        self.error(format!("Undeclared variable '{}'.", name_owned), span);
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
                    if let Some(super::symbol_table::Symbol::Var {
                        is_const,
                        ty,
                        is_moved,
                    }) = self.symbols.lookup(name).cloned()
                    {
                        if is_moved {
                            self.error(
                                format!(
                                    "Cannot assign to moved variable '{}'. Value was previously moved.",
                                    name
                                ),
                                span,
                            );
                        }
                        if is_const {
                            self.error(
                                format!("Cannot reassign constant variable '{}'.", name),
                                span,
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
                            span,
                        );
                    }
                    None
                } else {
                    None
                };

                let target_ty = self.check_expression(target, target_type.as_ref());
                let resolved_target = target_type.unwrap_or(target_ty);
                let val_type = self.check_expression(value, Some(&resolved_target));

                if !resolved_target.accepts(&val_type)
                    && val_type != Type::Unknown
                    && resolved_target != Type::Unknown
                {
                    self.error(
                        format!(
                            "Type mismatch in assignment. Expected {:?}, got {:?}.",
                            resolved_target.to_string(),
                            val_type.to_string()
                        ),
                        span,
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
                    let enum_name = enum_name.clone();
                    let variant_name = variant_name.clone();
                    if let Some(Type::Enum { variants, .. }) = self.enum_defs.get(&enum_name) {
                        if variants.contains(&variant_name) {
                            return self.enum_defs.get(&enum_name).unwrap().clone();
                        } else {
                            self.error(
                                format!("Enum '{}' has no variant '{}'", enum_name, variant_name),
                                span,
                            );
                            return Type::Unknown;
                        }
                    } else {
                        self.error(format!("'{}' is not an enum type", enum_name), span);
                        return Type::Unknown;
                    }
                }

                let operator = operator.clone();
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
                    self.error(format!("Binary operation '{operator:?}' requires operands of same type. Got {:?} and {:?}.", l_ty.to_string(), r_ty.to_string()), span);
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
                let call_kind = match &function.kind {
                    ExpressionKind::Identifier(name) => CallKind::Named(name.clone()),
                    ExpressionKind::Get {
                        object,
                        name: method_name,
                    } => {
                        let is_vec_static =
                            matches!(&object.kind, ExpressionKind::Identifier(n) if n == "Vec");
                        CallKind::Method {
                            method_name: method_name.clone(),
                            is_vec_static,
                        }
                    }
                    _ => CallKind::Unknown,
                };

                match call_kind {
                    CallKind::Named(name) if name == "Ok" => {
                        if arguments.len() != 1 {
                            self.error("Ok() takes exactly one argument".into(), span);
                            return Type::Unknown;
                        }
                        let inner_type = self.check_expression(&mut arguments[0], None);
                        Type::Result {
                            ok_type: Box::new(inner_type),
                            err_type: Box::new(Type::Struct {
                                name: "Error".to_string(),
                                fields: vec![(
                                    "code".to_string(),
                                    Type::Integer {
                                        signed: Signedness::Signed,
                                        width: IntWidth::W32,
                                    },
                                )],
                            }),
                        }
                    }
                    CallKind::Named(name) if name == "Err" => {
                        if arguments.len() != 1 {
                            self.error(
                                "Err() takes exactly one argument (error code)".into(),
                                span,
                            );
                            return Type::Unknown;
                        }
                        let code_type = self.check_expression(
                            &mut arguments[0],
                            Some(&Type::Integer {
                                signed: Signedness::Signed,
                                width: IntWidth::W32,
                            }),
                        );
                        if !matches!(code_type, Type::Integer { .. }) {
                            self.error("Err() code must be an integer".into(), span);
                        }
                        if let Some(Type::Result { ok_type, err_type }) = expected_type {
                            return Type::Result {
                                ok_type: ok_type.clone(),
                                err_type: err_type.clone(),
                            };
                        }
                        Type::Result {
                            ok_type: Box::new(Type::Unknown),
                            err_type: Box::new(Type::Struct {
                                name: "Error".to_string(),
                                fields: vec![(
                                    "code".to_string(),
                                    Type::Integer {
                                        signed: Signedness::Signed,
                                        width: IntWidth::W32,
                                    },
                                )],
                            }),
                        }
                    }
                    CallKind::Named(name) => self.check_call_mut(&name, arguments, None, span),
                    CallKind::Method {
                        method_name,
                        is_vec_static: true,
                    } => {
                        let elem_type = if let Some(Type::Vec { elem_type }) = expected_type {
                            elem_type.as_ref().clone()
                        } else {
                            self.error(
                                "Cannot infer Vec element type. Please add a type annotation."
                                    .into(),
                                span,
                            );
                            Type::Unknown
                        };
                        self.check_vec_method_mut(&method_name, &elem_type, arguments, span)
                    }
                    CallKind::Method {
                        method_name,
                        is_vec_static: false,
                    } => {
                        let obj_type =
                            if let ExpressionKind::Get { object, .. } = &mut function.kind {
                                self.check_expression(object, None)
                            } else {
                                unreachable!()
                            };

                        if method_name == "copy" && arguments.is_empty() {
                            if !obj_type.has_move_semantics() {
                                self.error(
                                    format!(
                                        "Method 'copy' is not needed for type {} (it's already Copy)",
                                        obj_type
                                    ),
                                    span,
                                );
                            }
                            return obj_type;
                        }

                        if let Type::Vec { elem_type } = &obj_type {
                            let elem_type = elem_type.clone();
                            return self.check_vec_method_mut(
                                &method_name,
                                &elem_type,
                                arguments,
                                span,
                            );
                        }

                        if let Type::Ref(inner) | Type::RefMut(inner) = &obj_type
                            && let Type::Vec { elem_type } = inner.as_ref()
                        {
                            let elem_type = elem_type.clone();
                            return self.check_vec_method_mut(
                                &method_name,
                                &elem_type,
                                arguments,
                                span,
                            );
                        }

                        if let Type::Slice { .. } = &obj_type {
                            if method_name == "len" && arguments.is_empty() {
                                return Type::Integer {
                                    signed: Signedness::Unsigned,
                                    width: IntWidth::WSize,
                                };
                            }
                            self.error(format!("Slice has no method '{}'", method_name), span);
                            return Type::Unknown;
                        }

                        let struct_name = match &obj_type {
                            Type::Struct { name, .. } => name.clone(),
                            Type::Pointer(elem_type) => {
                                if let Type::Struct { name, .. } = elem_type.as_ref() {
                                    name.clone()
                                } else {
                                    String::new()
                                }
                            }
                            _ => String::new(),
                        };

                        if struct_name.is_empty() {
                            self.error(
                                format!(
                                    "Cannot call method on non-struct type {:?}",
                                    obj_type.to_string()
                                ),
                                span,
                            );
                            return Type::Unknown;
                        }
                        let full_name = format!("{struct_name}::{method_name}");
                        self.check_call_mut(&full_name, arguments, Some(obj_type), span)
                    }
                    CallKind::Unknown => {
                        self.error("Invalid call expression".into(), span);
                        Type::Unknown
                    }
                }
            }

            ExpressionKind::Get { object, name } => {
                let name = name.clone();
                let obj_type = self.check_expression(object, None);

                let actual_type = if let Type::Pointer(elem_type) = &obj_type {
                    elem_type.as_ref().clone()
                } else {
                    obj_type.clone()
                };

                if let Type::Struct {
                    name: struct_name, ..
                } = &actual_type
                {
                    let struct_name = struct_name.clone();
                    if let Some(Type::Struct { fields, .. }) = self.struct_defs.get(&struct_name) {
                        for (f_name, f_type) in fields {
                            if f_name == &name {
                                return f_type.clone();
                            }
                        }
                        self.error(
                            format!("Struct '{struct_name}' has no field '{name}'"),
                            span,
                        );
                    }
                } else if obj_type != Type::Unknown {
                    self.error("Cannot access property on non-struct type.".into(), span);
                }
                Type::Unknown
            }

            ExpressionKind::StructLiteral { name, fields } => {
                let name = name.clone();
                if let Some(def) = self.struct_defs.get(&name).cloned() {
                    if let Type::Struct {
                        fields: def_fields, ..
                    } = &def
                    {
                        for (field_name, _) in fields.iter() {
                            if !def_fields.iter().any(|(n, _)| n == field_name) {
                                self.error(
                                    format!("Unknown field '{}' in struct '{}'", field_name, name),
                                    span,
                                );
                            }
                        }

                        let def_fields = def_fields.clone();
                        for (def_name, def_type) in &def_fields {
                            let found = fields.iter_mut().find(|(n, _)| n == def_name);
                            if let Some((_, field_expr)) = found {
                                let field_span = field_expr.span;
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
                                    ), field_span);
                                }
                            } else {
                                self.error(
                                    format!("Missing field '{def_name}' in struct literal {name}"),
                                    span,
                                );
                            }
                        }
                        return def;
                    }
                } else {
                    self.error(format!("Unknown struct type '{name}'."), span);
                }
                Type::Unknown
            }

            ExpressionKind::Match { value, arms } => {
                let _match_type = self.check_expression(value, None);

                if arms.is_empty() {
                    return Type::Void;
                }

                let first_arm_type = self.check_expression(&mut arms[0].1, expected_type);

                for (i, (_, result)) in arms.iter_mut().enumerate().skip(1) {
                    let first = first_arm_type.clone();
                    let arm_type = self.check_expression(result, Some(&first));
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
            ExpressionKind::Prefix { operator, right } => {
                let operator = operator.clone();
                match &operator {
                    crate::token::Token::Minus => {
                        if let ExpressionKind::Int(val) = &right.kind
                            && let Some(Type::Integer { width, signed }) = expected_type
                        {
                            let negated = -val;
                            let width = *width;
                            let signed = *signed;
                            if self.fits_in_int(negated, width, signed) {
                                return Type::Integer { width, signed };
                            } else {
                                self.error(
                                    format!(
                                        "Literal {} does not fit in type {:?}",
                                        negated,
                                        expected_type.unwrap()
                                    ),
                                    span,
                                );
                                return Type::Integer { width, signed };
                            }
                        }

                        let right_type = self.check_expression(right, expected_type);
                        match &right_type {
                            Type::Integer { .. } | Type::Float(_) | Type::ParamType(_) => {
                                right_type
                            }
                            _ => {
                                self.error(
                                    format!(
                                        "Cannot negate non-numeric type {:?}",
                                        right_type.to_string()
                                    ),
                                    span,
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
                                span,
                            );
                        }
                        Type::Bool
                    }
                    _ => self.check_expression(right, expected_type),
                }
            }
            ExpressionKind::Cast { left, target } => {
                let _source_type = self.check_expression(left, None);

                if let ExpressionKind::Identifier(type_name) = &target.kind {
                    let type_name = type_name.clone();
                    self.resolve_named_type(&type_name)
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
                            span,
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
                    Some(elem_type.as_ref().clone())
                } else {
                    None
                };

                let first_type = self.check_expression(&mut elements[0], elem_hint.as_ref());
                let len = elements.len();

                for (i, elem) in elements.iter_mut().enumerate().skip(1) {
                    let first = first_type.clone();
                    let elem_span = elem.span;
                    let elem_type = self.check_expression(elem, Some(&first));

                    if !first_type.accepts(&elem_type) {
                        self.error(
                            format!(
                                "Array element at index {} type mismatch. Expected {:?}, got {:?}.",
                                i,
                                first_type.to_string(),
                                elem_type.to_string()
                            ),
                            elem_span,
                        );
                    }
                }

                Type::Array {
                    elem_type: Box::new(first_type),
                    len,
                }
            }
            ExpressionKind::AddressOf(inner) => {
                let inner_kind_is_lvalue = matches!(
                    &inner.kind,
                    ExpressionKind::Identifier(_)
                        | ExpressionKind::Get { .. }
                        | ExpressionKind::Index { .. }
                        | ExpressionKind::Dereference(_)
                );
                let inner_type = self.check_expression(inner, None);
                if !inner_kind_is_lvalue {
                    self.error("Cannot take address of a temporary value".into(), span);
                }
                Type::Pointer(Box::new(inner_type))
            }
            ExpressionKind::BorrowRef(inner) => {
                let inner_kind = inner.kind.clone();
                let inner_type = self.check_expression(inner, None);

                if let ExpressionKind::Identifier(name) = &inner_kind {
                    let is_moved =
                        if let Some(super::symbol_table::Symbol::Var { is_moved, .. }) =
                            self.symbols.lookup(name)
                        {
                            *is_moved
                        } else {
                            false
                        };

                    if is_moved {
                        self.error(format!("Cannot borrow '{}': value was moved", name), span);
                    }
                } else if !matches!(
                    inner_kind,
                    ExpressionKind::Get { .. }
                        | ExpressionKind::Index { .. }
                        | ExpressionKind::Dereference(_)
                ) {
                    self.error("Cannot create reference to a temporary value".into(), span);
                }

                Type::Ref(Box::new(inner_type))
            }
            ExpressionKind::BorrowRefMut(inner) => {
                let inner_kind = inner.kind.clone();
                let inner_type = self.check_expression(inner, None);

                if let ExpressionKind::Identifier(name) = &inner_kind {
                    let (is_moved, is_const) =
                        if let Some(super::symbol_table::Symbol::Var {
                            is_const, is_moved, ..
                        }) = self.symbols.lookup(name)
                        {
                            (*is_moved, *is_const)
                        } else {
                            (false, false)
                        };

                    if is_moved {
                        self.error(format!("Cannot borrow '{}': value was moved", name), span);
                    }
                    if is_const {
                        self.error(
                            format!(
                                "Cannot create mutable reference to immutable variable '{}'",
                                name
                            ),
                            span,
                        );
                    }
                } else if !matches!(
                    inner_kind,
                    ExpressionKind::Get { .. }
                        | ExpressionKind::Index { .. }
                        | ExpressionKind::Dereference(_)
                ) {
                    self.error(
                        "Cannot create mutable reference to a temporary value".into(),
                        span,
                    );
                }

                Type::RefMut(Box::new(inner_type))
            }
            ExpressionKind::Dereference(inner) => {
                let inner_type = self.check_expression(inner, None);

                match inner_type {
                    Type::Pointer(elem_type) => *elem_type,
                    Type::Ref(elem_type) => *elem_type,
                    Type::RefMut(elem_type) => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(
                            format!("Cannot dereference type {:?}", inner_type.to_string()),
                            span,
                        );
                        Type::Unknown
                    }
                }
            }
            ExpressionKind::Tuple(elements) => {
                let expected_types: Option<Vec<Type>> =
                    if let Some(Type::Tuple(types)) = expected_type {
                        Some(types.clone())
                    } else {
                        None
                    };

                let mut result_types = Vec::with_capacity(elements.len());
                let len = elements.len();

                for (i, elem) in elements.iter_mut().enumerate() {
                    let expected = expected_types.as_ref().and_then(|types| types.get(i));
                    let elem_type = self.check_expression(elem, expected);
                    result_types.push(elem_type);
                }

                if let Some(ref expected_types) = expected_types
                    && expected_types.len() != len
                {
                    self.error(
                        format!(
                            "Tuple has {} elements, but expected {}",
                            len,
                            expected_types.len()
                        ),
                        span,
                    );
                }

                Type::Tuple(result_types)
            }
            ExpressionKind::InlineAsm {
                outputs, inputs, ..
            } => {
                for operand in inputs.iter_mut() {
                    self.check_expression(&mut operand.expr, None);
                }

                for operand in outputs.iter_mut() {
                    let operand_span = operand.expr.span;
                    let is_ident = matches!(operand.expr.kind, ExpressionKind::Identifier(_));
                    let ty = self.check_expression(&mut operand.expr, None);
                    if !is_ident {
                        self.error(
                            "Inline assembly output must be a variable".to_string(),
                            operand_span,
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

    /// Type-check Vec<T> built-in methods
    fn check_vec_method_mut(
        &mut self,
        method_name: &str,
        elem_type: &Type,
        arguments: &mut [Expression],
        span: Span,
    ) -> Type {
        let usize_type = Type::Integer {
            signed: Signedness::Unsigned,
            width: IntWidth::WSize,
        };

        match method_name {
            "new" => {
                if !arguments.is_empty() {
                    self.error("Vec::new() takes no arguments".into(), span);
                }
                Type::Vec {
                    elem_type: Box::new(elem_type.clone()),
                }
            }
            "with_capacity" => {
                if arguments.len() != 1 {
                    self.error(
                        "Vec::with_capacity() takes exactly one argument".into(),
                        span,
                    );
                } else {
                    let arg_type = self.check_expression(&mut arguments[0], Some(&usize_type));
                    if !usize_type.accepts(&arg_type) {
                        self.error("Vec::with_capacity() argument must be usize".into(), span);
                    }
                }
                Type::Vec {
                    elem_type: Box::new(elem_type.clone()),
                }
            }
            "push" => {
                if arguments.len() != 1 {
                    self.error("Vec::push() takes exactly one argument".into(), span);
                } else {
                    let elem = elem_type.clone();
                    let arg_type = self.check_expression(&mut arguments[0], Some(&elem));
                    if !elem_type.accepts(&arg_type) {
                        self.error(
                            format!(
                                "Vec::push() argument type mismatch. Expected {}, got {}",
                                elem_type, arg_type
                            ),
                            span,
                        );
                    }
                }
                Type::Void
            }
            "pop" => {
                if !arguments.is_empty() {
                    self.error("Vec::pop() takes no arguments".into(), span);
                }
                Type::Optional(Box::new(elem_type.clone()))
            }
            "len" => {
                if !arguments.is_empty() {
                    self.error("Vec::len() takes no arguments".into(), span);
                }
                usize_type
            }
            "capacity" => {
                if !arguments.is_empty() {
                    self.error("Vec::capacity() takes no arguments".into(), span);
                }
                usize_type
            }
            "is_empty" => {
                if !arguments.is_empty() {
                    self.error("Vec::is_empty() takes no arguments".into(), span);
                }
                Type::Bool
            }
            "get" => {
                if arguments.len() != 1 {
                    self.error("Vec::get() takes exactly one argument".into(), span);
                } else {
                    let arg_type = self.check_expression(&mut arguments[0], Some(&usize_type));
                    if !usize_type.accepts(&arg_type) {
                        self.error("Vec::get() index must be usize".into(), span);
                    }
                }
                Type::Optional(Box::new(elem_type.clone()))
            }
            "clear" => {
                if !arguments.is_empty() {
                    self.error("Vec::clear() takes no arguments".into(), span);
                }
                Type::Void
            }
            "copy" => {
                if !arguments.is_empty() {
                    self.error("Vec::copy() takes no arguments".into(), span);
                }
                Type::Vec {
                    elem_type: Box::new(elem_type.clone()),
                }
            }
            _ => {
                self.error(format!("Vec<T> has no method '{}'", method_name), span);
                Type::Unknown
            }
        }
    }

    fn check_call_mut(
        &mut self,
        name: &str,
        args: &mut [Expression],
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
                for i in 0..args.len() {
                    let expected = expected_args[i].clone();
                    let arg_span = args[i].span;
                    let arg_type = self.check_expression(&mut args[i], Some(&expected));

                    if !expected.accepts(&arg_type) {
                        self.error(format!("Argument {} type mismatch.", i + 1), arg_span);
                    }

                    // If argument is passed by value (not reference) and has move semantics,
                    // mark the source variable as moved
                    if !matches!(expected, Type::Ref(_) | Type::RefMut(_))
                        && arg_type.has_move_semantics()
                        && let ExpressionKind::Identifier(var_name) = &args[i].kind
                    {
                        let var_name = var_name.clone();
                        self.symbols.mark_moved(&var_name);
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
