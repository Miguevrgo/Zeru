use crate::{
    ast::{Expression, Program, Statement, TypeSpec},
    sema::{
        symbol_table::SymbolTable,
        types::{FloatWidth, IntWidth, Signedness, Type},
    },
};
use std::collections::HashMap;

pub struct SemanticAnalyzer {
    pub errors: Vec<String>,

    symbols: SymbolTable,
    struct_defs: HashMap<String, Type>,
    enum_defs: HashMap<String, Type>,
    current_fn_return_type: Option<Type>,

    in_loop: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            symbols: SymbolTable::new(),
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
            match stmt {
                Statement::Struct { name, fields, .. } => {
                    if self.struct_defs.contains_key(name) || self.enum_defs.contains_key(name) {
                        self.error(format!("Type {name} is already defined"));
                        continue;
                    }

                    let mut resolved_fields = Vec::new();
                    for (f_name, f_type_spec) in fields {
                        let f_ty = self.resolve_spec(f_type_spec);
                        resolved_fields.push((f_name.clone(), f_ty));
                    }

                    let struct_type = Type::Struct {
                        name: name.clone(),
                        fields: resolved_fields,
                    };

                    self.struct_defs.insert(name.clone(), struct_type);
                }
                Statement::Enum { name, variants } => {
                    if self.enum_defs.contains_key(name) || self.struct_defs.contains_key(name) {
                        self.error(format!("Type '{name}' is already defined"));
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
    }

    fn scan_functions(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match stmt {
                Statement::Function {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    self.register_function(name.clone(), params, return_type, None);
                }

                Statement::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let Statement::Function {
                            name: method_name,
                            params,
                            return_type,
                            ..
                        } = method
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
        params: &Vec<(String, TypeSpec)>,
        return_type: &Option<TypeSpec>,
        associated_struct: Option<&str>,
    ) {
        if self.symbols.lookup_current_scope(&name).is_some() {
            self.error(format!("Function '{name}' is already defined"));
            return;
        }

        if name == "main" {
            if !params.is_empty() {
                self.error("Function 'main' must not take arguments".into());
            }

            if let Some(rt_spec) = return_type {
                let ret_ty = self.resolve_spec(rt_spec);
                if ret_ty != Type::Void {
                    match ret_ty {
                        // FIX: Returning integer will only be allowed while there is not
                        // exit() function. (Because it is easier to debug that way)
                        Type::Void => {}
                        Type::Integer {
                            width: IntWidth::W32,
                            signed: Signedness::Signed,
                        } => {}
                        _ => self.error(format!(
                            "Function 'main' must return void or i32. Found {:?}.",
                            ret_ty
                        )),
                    }
                }
            }
        }

        let mut param_types = Vec::new();
        for (param_name, type_spec) in params {
            if param_name == "self" {
                if let Some(struct_name) = associated_struct {
                    if let Some(ty) = self.struct_defs.get(struct_name) {
                        param_types.push(ty.clone())
                    } else {
                        param_types.push(Type::Unknown);
                        self.error("Self used in unknown struct context".into());
                    }
                } else {
                    self.error("'self' parameter allowed only in struct methods".into());
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
            match stmt {
                Statement::Function {
                    name, params, body, ..
                } => {
                    self.check_function_body(name.clone(), params, body);
                }
                Statement::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let Statement::Function {
                            name: method_name,
                            params,
                            body,
                            ..
                        } = method
                        {
                            let full_name = format!("{struct_name}::{method_name}");
                            self.check_function_body(full_name, params, body);
                        }
                    }
                }
                Statement::Var { .. } => self.check_statement(stmt),
                _ => {}
            }
        }
    }

    fn check_function_body(
        &mut self,
        name: String,
        params: &[(String, TypeSpec)],
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

            for (i, (param_name, _)) in params.iter().enumerate() {
                let ty = params_type_def.get(i).unwrap_or(&Type::Unknown).clone();
                self.symbols.insert_var(param_name.clone(), ty, true);
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
                        self.error("Array length must be an integer literal".into());
                        0
                    };

                    return Type::Array {
                        elem_type: Box::new(elem_type),
                        len,
                    };
                }
                self.error(format!("Unknown generic type or invalid args: {}", name));
                Type::Unknown
            }
            TypeSpec::IntLiteral(_) => {
                self.error("Unexpected integer literal in type position".into());
                Type::Unknown
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

        match name {
            "i8" => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W8,
            },
            "u8" => Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W8,
            },
            "i16" => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W16,
            },
            "u16" => Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W16,
            },
            "i32" => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W32,
            },
            "u32" => Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W32,
            },
            "i64" => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W64,
            },
            "u64" => Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W64,
            },
            "isize" => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::WSize,
            },
            "usize" => Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::WSize,
            },
            "f32" => Type::Float(FloatWidth::W32),
            "f64" => Type::Float(FloatWidth::W64),
            "bool" => Type::Bool,
            "String" => Type::String,
            "void" => Type::Void,
            "self" => Type::Unknown,
            _ => {
                let mut candidates = vec![
                    "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f32", "f64", "bool",
                    "String",
                ];
                for k in self.struct_defs.keys() {
                    candidates.push(k);
                }
                for k in self.enum_defs.keys() {
                    candidates.push(k);
                }

                let mut best_match = "";
                let mut min_dist = usize::MAX;

                for candidate in candidates {
                    let dist = Self::levenshtein_distance(name, candidate);
                    if dist < min_dist {
                        min_dist = dist;
                        best_match = candidate;
                    }
                }

                if min_dist <= 2 && !best_match.is_empty() {
                    self.error(format!(
                        "Unknown type '{}'. Did you mean '{}'?",
                        name, best_match
                    ));
                } else {
                    self.error(format!("Unknown type '{}'", name));
                }
                Type::Unknown
            }
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Var {
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
                            (Type::Integer { .. }, Type::Integer { .. }) => true,
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
                                    matches!(
                                        (&**t1, &**t2),
                                        (Type::Integer { .. }, Type::Integer { .. })
                                    )
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
                        ));
                    }

                    expected
                } else {
                    if value_type == Type::Unknown {
                        self.error(format!(
                            "Cannot infer type for variable '{}'. Please add a type annotation.",
                            name
                        ));
                    }
                    value_type
                };

                self.symbols.insert_var(name.clone(), final_type, *is_const);
            }

            Statement::Return(opt_expr) => {
                let expected = self.current_fn_return_type.clone();

                let expr_type = if let Some(expr) = opt_expr {
                    self.check_expression(expr, expected.as_ref())
                } else {
                    Type::Void
                };

                if let Some(expected) = expected {
                    if !expected.accepts(&expr_type) {
                        self.error(format!(
                            "Invalid return type. Function expects {:?}, returning {:?}",
                            expected.to_string(),
                            expr_type.to_string()
                        ));
                    }
                } else {
                    self.error("Return statement illegal if not inside a function".into());
                }
            }

            Statement::Block(stmts) => {
                self.symbols.enter_scope();
                for s in stmts {
                    self.check_statement(s);
                }
                self.symbols.exit_scope();
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.check_expression(condition, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(format!(
                        "If condition must be boolean, got {:?}",
                        cond_type.to_string()
                    ));
                }

                self.check_statement(then_branch);
                if let Some(else_stmt) = else_branch {
                    self.check_statement(else_stmt);
                }
            }

            Statement::While { cond, body } => {
                let cond_type = self.check_expression(cond, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(format!(
                        "While condition must be boolean, got: {:?}",
                        cond_type.to_string()
                    ));
                }

                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.check_statement(body);
                self.in_loop = prev_loop;
            }

            Statement::Break | Statement::Continue => {
                if !self.in_loop {
                    self.error("Break/Continue can only be used inside loops".into());
                }
            }

            Statement::Expression(expr) => {
                self.check_expression(expr, None);
            }

            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                let iterable_type = self.check_expression(iterable, None);

                let item_type = match iterable_type {
                    Type::Array { elem_type, .. } => *elem_type,
                    Type::String => Type::Integer {
                        signed: Signedness::Unsigned,
                        width: IntWidth::W8,
                    },
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(format!("Type {:?} is not iterable.", iterable_type));
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

            Statement::Import { path: _, symbols } => {
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
        match expr {
            Expression::Int(val) => {
                if let Some(Type::Integer { width, signed }) = expected_type {
                    if self.fits_in_int(*val, *width, *signed) {
                        return Type::Integer {
                            width: *width,
                            signed: *signed,
                        };
                    } else {
                        self.error(format!(
                            "Literal {} does not fit in type {:?}",
                            val,
                            expected_type.unwrap()
                        ));
                    }
                }
                Type::Integer {
                    signed: Signedness::Signed,
                    width: IntWidth::W32,
                }
            }
            Expression::Float(_) => {
                if let Some(Type::Float(width)) = expected_type {
                    return Type::Float(*width);
                }
                Type::Float(FloatWidth::W32)
            }
            Expression::Boolean(_) => Type::Bool,
            Expression::StringLit(_) => Type::String,
            Expression::None => Type::Void,

            Expression::Identifier(name) => {
                if let Some(symbol) = self.symbols.lookup(name) {
                    match symbol {
                        super::symbol_table::Symbol::Var { ty, .. } => ty.clone(),
                        _ => {
                            self.error(format!("'{name}' is a function, not a variable"));
                            Type::Unknown
                        }
                    }
                } else {
                    self.error(format!("Undeclared variable '{}'.", name));
                    Type::Unknown
                }
            }

            Expression::Assign {
                target,
                operator: _,
                value,
            } => {
                let target_type = if let Expression::Identifier(name) = &**target {
                    if let Some(super::symbol_table::Symbol::Var { is_const, ty }) =
                        self.symbols.lookup(name).cloned()
                    {
                        if is_const {
                            self.error(format!("Cannot reassign constant variable '{}'.", name));
                        }
                        Some(ty)
                    } else {
                        None
                    }
                } else {
                    Some(self.check_expression(target, None))
                };

                let val_type = self.check_expression(value, target_type.as_ref());

                if let Some(ty) = target_type
                    && !ty.accepts(&val_type)
                    && val_type != Type::Unknown
                {
                    self.error(format!(
                        "Type mismatch in assignment. Expected {:?}, got {:?}.",
                        ty.to_string(),
                        val_type.to_string()
                    ));
                }
                Type::Void
            }

            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let l_ty = self.check_expression(left, expected_type);
                let r_ty = self.check_expression(right, Some(&l_ty));

                if l_ty == Type::Unknown || r_ty == Type::Unknown {
                    return Type::Unknown;
                }

                if !l_ty.accepts(&r_ty) {
                    self.error(format!("Binary operation '{operator:?}' requires operands of same type. Got {:?} and {:?}.", l_ty.to_string(), r_ty.to_string()));
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

            Expression::Call {
                function,
                arguments,
            } => {
                if let Expression::Identifier(name) = &**function {
                    self.check_call(name, arguments, None);
                }

                if let Expression::Get {
                    object,
                    name: method_name,
                } = &**function
                {
                    let obj_type = self.check_expression(object, None);

                    let struct_name = match &obj_type {
                        Type::Struct { name, .. } => name.clone(),
                        Type::Pointer { elem_type, .. } => {
                            if let Type::Struct { name, .. } = &**elem_type {
                                name.clone()
                            } else {
                                "".into()
                            }
                        }
                        _ => "".into(),
                    };

                    if struct_name.is_empty() {
                        self.error(format!(
                            "Cannot call method on non-struct type {:?}",
                            obj_type.to_string()
                        ));
                        return Type::Unknown;
                    }
                    let full_name = format!("{struct_name}::{method_name}");
                    return self.check_call(&full_name, arguments, Some(obj_type));
                }

                self.error("Invalid call expression".into());
                Type::Unknown
            }

            Expression::Get { object, name } => {
                let obj_type = self.check_expression(object, None);

                let actual_type = if let Type::Pointer { elem_type, .. } = &obj_type {
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
                        self.error(format!("Struct '{struct_name}' has no field '{name}'"));
                    }
                } else if obj_type != Type::Unknown {
                    self.error("Cannot access property on non-struct type.".into());
                }
                Type::Unknown
            }

            Expression::StructLiteral { name, fields } => {
                if let Some(def) = self.struct_defs.get(name).cloned() {
                    if let Type::Struct {
                        fields: def_fields, ..
                    } = &def
                    {
                        for (def_name, def_type) in def_fields {
                            let found = fields.iter().find(|(n, _)| n == def_name);
                            if let Some((_, expr)) = found {
                                let expr_type = self.check_expression(expr, Some(def_type));
                                if !def_type.accepts(&expr_type) {
                                    self.error(format!(
                                        "Field '{}' in struct '{}' expected {:?}, got {:?}.",
                                        def_name,
                                        name,
                                        def_type.to_string(),
                                        expr_type.to_string()
                                    ));
                                }
                            } else {
                                self.error(format!(
                                    "Missing field '{def_name}' in struct literal {name}"
                                ));
                            }
                        }
                        return def;
                    }
                } else {
                    self.error(format!("Unknown struct type '{name}'."));
                }
                Type::Unknown
            }

            Expression::Match { arms, .. } => {
                if !arms.is_empty() {
                    self.check_expression(&arms[0].1, expected_type)
                } else {
                    Type::Void
                }
            }

            Expression::ArrayLiteral(elements) => {
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
                    Some(&**elem_type)
                } else {
                    None
                };

                let first_type = self.check_expression(&elements[0], elem_hint);

                for (i, elem) in elements.iter().enumerate().skip(1) {
                    let elem_type = self.check_expression(elem, Some(&first_type));

                    if !first_type.accepts(&elem_type) {
                        self.error(format!(
                            "Array element at index {} type mismatch. Expected {:?}, got {:?}.",
                            i,
                            first_type.to_string(),
                            elem_type.to_string()
                        ));
                    }
                }

                Type::Array {
                    elem_type: Box::new(first_type),
                    len: elements.len(),
                }
            }

            _ => Type::Unknown,
        }
    }

    fn check_call(&mut self, name: &str, args: &[Expression], implicit_self: Option<Type>) -> Type {
        if let Some(super::symbol_table::Symbol::Function { params, ret_type }) =
            self.symbols.lookup(name).cloned()
        {
            let mut expected_args = params.clone();

            if let Some(self_type) = implicit_self
                && !expected_args.is_empty()
            {
                if !expected_args[0].accepts(&self_type) {
                    self.error(format!(
                        "Method '{name}' called on wrong type. Expected {:?}, got {:?}",
                        expected_args[0].to_string(),
                        self_type.to_string()
                    ));
                }
                expected_args.remove(0);
            }

            if args.len() != expected_args.len() {
                self.error(format!(
                    "Function '{name}' expects {} arguments, got {}",
                    expected_args.len(),
                    args.len()
                ));
            } else {
                for (i, expr) in args.iter().enumerate() {
                    let arg_type = self.check_expression(expr, Some(&expected_args[i]));
                    if !expected_args[i].accepts(&arg_type) {
                        self.error(format!("Argument {} type mismatch.", i + 1));
                    }
                }
            }
            return ret_type;
        }

        self.error(format!("Function '{name}' not defined."));
        Type::Unknown
    }

    fn error(&mut self, msg: String) {
        self.errors.push(format!("Semantic Error: {}", msg));
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
