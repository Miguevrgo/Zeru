use crate::{
    ast::{Expression, Program, Statement},
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
                    for (f_name, f_type_str) in fields {
                        let f_ty = self.resolve_type(f_type_str);
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
            if let Statement::Function {
                name,
                params,
                return_type,
                ..
            } = stmt
            {
                if self.symbols.lookup_current_scope(name).is_some() {
                    self.error(format!("Function '{name}' is already defined"));
                    continue;
                }

                if name == "main" {
                    if !params.is_empty() {
                        self.error("Function 'main' must not take arguments".into());
                    }

                    if let Some(rt_str) = return_type {
                        let ret_ty = self.resolve_type(&rt_str);
                        if ret_ty != Type::Void {
                            self.error(format!(
                                "Function 'main' must return void (or imply void). Found {:?}.",
                                ret_ty
                            ));
                        }
                    }
                }

                let mut param_types = Vec::new();
                for (_, type_name) in params {
                    param_types.push(self.resolve_type(type_name))
                }

                let ret_ty = if let Some(rt_str) = return_type {
                    self.resolve_type(&rt_str)
                } else {
                    Type::Void
                };

                self.symbols.insert_fn(name.clone(), param_types, ret_ty);
            }
        }
    }

    fn analyze_bodies(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match stmt {
                Statement::Function {
                    name, params, body, ..
                } => {
                    let function_symbol = self.symbols.lookup(name).unwrap().clone();

                    if let super::symbol_table::Symbol::Function { ret_type, .. } = function_symbol
                    {
                        let prev_ret = self.current_fn_return_type.replace(ret_type.clone());

                        self.symbols.enter_scope();

                        for (param_name, type_str) in params {
                            let param_type = self.resolve_type(type_str);
                            self.symbols
                                .insert_var(param_name.clone(), param_type, true);
                        }

                        for s in body {
                            self.check_statement(s);
                        }

                        self.symbols.exit_scope();
                        self.current_fn_return_type = prev_ret;
                    }
                }
                Statement::Var { .. } => self.check_statement(stmt),
                _ => {}
            }
        }
    }

    fn resolve_type(&mut self, type_name: &str) -> Type {
        if let Some(ty) = Type::from_string(type_name) {
            return ty;
        }

        if let Some(ty) = self.struct_defs.get(type_name) {
            return ty.clone();
        }

        if let Some(ty) = self.enum_defs.get(type_name) {
            return ty.clone();
        }

        match type_name {
            "int" | "signed" => {
                self.error(format!(
                    "Zeru is strict about sizes. 'int' is ambiguous. Did you mean 'i32'?"
                ));
                return Type::Unknown;
            }
            "uint" | "unsigned" => {
                self.error(format!(
                    "Zeru is strict about sizes. 'unsigned' is ambiguous. Did you mean 'u32'?"
                ));
                return Type::Unknown;
            }
            "float" => {
                self.error(format!("Ambiguous float size. Did you mean 'f32'?"));
                return Type::Unknown;
            }
            "double" => {
                self.error(format!("Ambiguous float size. Did you mean 'f64'?"));
                return Type::Unknown;
            }
            "char" => {
                self.error(format!("Zeru uses 'u8' for bytes or 'String' for text. 'char' is not supported directly."));
                return Type::Unknown;
            }
            "long" | "short" => {
                self.error(format!("C-style qualifiers like '{}' are not supported. Use explicit sizes (e.g., i64, i16).", type_name));
                return Type::Unknown;
            }
            _ => {}
        }

        let mut candidates = vec![
            "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "isize", "usize", "f32", "f64",
            "bool", "String", "void",
        ];

        for name in self.struct_defs.keys() {
            candidates.push(name);
        }
        for name in self.enum_defs.keys() {
            candidates.push(name);
        }

        let mut best_match = "";
        let mut min_dist = usize::MAX;

        for candidate in candidates {
            let dist = Self::levenshtein_distance(type_name, candidate);
            if dist < min_dist {
                min_dist = dist;
                best_match = candidate;
            }
        }

        if min_dist <= 2 && !best_match.is_empty() {
            self.error(format!(
                "Unknown type '{}'. Did you mean '{}'?",
                type_name, best_match
            ));
        } else {
            self.error(format!("Unknown type '{}'", type_name));
        }

        Type::Unknown
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                let value_type = self.check_expression(value);

                let final_type = if let Some(type_str) = type_annotation {
                    let expected = self.resolve_type(type_str);

                    if !expected.accepts(&value_type) && value_type != Type::Unknown {
                        self.error(format!(
                            "Type mismatch for variable '{name}'. Annotated as {:?} but got {:?}.",
                            expected.to_string(),
                            value_type.to_string(),
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
                let expr_type = if let Some(expr) = opt_expr {
                    self.check_expression(expr)
                } else {
                    Type::Void
                };

                if let Some(expected) = &self.current_fn_return_type {
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
                let cond_type = self.check_expression(condition);
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
                let cond_type = self.check_expression(cond);
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
                self.check_expression(expr);
            }
            //TODO: ForIn, Struct, Enum, Import
            _ => {}
        }
    }

    fn check_expression(&mut self, expr: &Expression) -> Type {
        match expr {
            Expression::Int(_) => Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W32,
            },
            Expression::Float(_) => Type::Float(FloatWidth::W32),
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
                if let Expression::Identifier(name) = &**target {
                    if let Some(symbol) = self.symbols.lookup(name).cloned() {
                        if let super::symbol_table::Symbol::Var { is_const, ty } = symbol {
                            if is_const {
                                self.error(format!(
                                    "Cannot reassign constant variable '{}'.",
                                    name
                                ));
                            }
                            let val_type = self.check_expression(value);
                            if !ty.accepts(&val_type) && val_type != Type::Unknown {
                                self.error(format!(
                                       "Type mismatch in assignment. Variable '{}' is {:?}, value is {:?}.", 
                                       name, ty.to_string(), val_type.to_string()
                                ));
                            }
                        }
                    } else {
                        self.error(format!("Undeclared variable '{}'.", name));
                    }
                } else {
                    //TODO: Complex assignment
                    self.check_expression(target);
                    self.check_expression(value);
                }
                Type::Void
            }

            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let l_ty = self.check_expression(left);
                let r_ty = self.check_expression(right);

                if l_ty == Type::Unknown || r_ty == Type::Unknown {
                    return Type::Unknown;
                }

                if !l_ty.accepts(&r_ty) {
                    self.error(format!("Binary operation '{operator:?}' requires operands of same type. Got {:?} and {:?}.", l_ty.to_string(), r_ty.to_string()));
                    return Type::Unknown;
                }

                match operator {
                    crate::token::Token::Plus
                    | crate::token::Token::Minus
                    | crate::token::Token::Star
                    | crate::token::Token::Slash => l_ty,

                    crate::token::Token::Eq
                    | crate::token::Token::NotEq
                    | crate::token::Token::Lt
                    | crate::token::Token::Gt
                    | crate::token::Token::Leq
                    | crate::token::Token::Geq => Type::Bool,

                    _ => Type::Unknown,
                }
            }

            Expression::Call {
                function,
                arguments,
            } => {
                if let Expression::Identifier(name) = &**function {
                    if let Some(super::symbol_table::Symbol::Function { params, ret_type }) =
                        self.symbols.lookup(name).cloned()
                    {
                        if arguments.len() != params.len() {
                            self.error(format!(
                                "Fucntion '{name}' expects {} arguments, got {}",
                                params.len(),
                                arguments.len()
                            ));
                        } else {
                            for (i, arg_expr) in arguments.iter().enumerate() {
                                let arg_type = self.check_expression(arg_expr);
                                if !params[i].accepts(&arg_type) && arg_type != Type::Unknown {
                                    self.error(format!("Argument {} mismatch in call to '{name}'. Expected {:?}, got {:?}", i + 1, params[i].to_string(), arg_type.to_string()));
                                }
                            }
                        }
                        return ret_type;
                    } else {
                        self.error(format!("Function '{name}' not found"));
                    }
                }
                Type::Unknown
            }

            Expression::Get { object, name } => {
                let obj_type = self.check_expression(object);

                let struct_def_type = match &obj_type {
                    Type::Struct { .. } => Some(&obj_type),
                    Type::Pointer { elem_type, .. } => match &**elem_type {
                        Type::Struct { .. } => Some(&**elem_type),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some(Type::Struct {
                    name: struct_name,
                    fields: _,
                }) = struct_def_type
                {
                    if let Some(Type::Struct {
                        fields: def_fields, ..
                    }) = self.struct_defs.get(struct_name)
                    {
                        for (f_name, f_type) in def_fields {
                            if f_name == name {
                                return f_type.clone();
                            }
                        }

                        self.error(format!(
                            "Struct '{struct_name}' has no field named '{name}'"
                        ));
                    }
                } else if obj_type != Type::Unknown {
                    self.error(format!(
                        "Type '{:?}' is not a struct, cannot access property '{}'.",
                        obj_type.to_string(),
                        name
                    ));
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
                                let expr_type = self.check_expression(expr);
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
            _ => Type::Unknown,
        }
    }

    fn error(&mut self, msg: String) {
        self.errors.push(format!("Semantic Error: {}", msg));
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
