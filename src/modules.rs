//! Give each module's declarations a name of their own.
//!
//! Two modules may both declare `min`, so every declaration is renamed to
//! `module::min` and every reference in that module is pointed at the new name.
//! The rename walks the parsed tree, which is what keeps it away from string
//! literals and comments: they are not names. It also tracks the locals in
//! scope, since a parameter or a variable called `min` is not the module's.

use std::collections::HashMap;

use crate::ast::{
    Expression, ExpressionKind, Program, Statement, StatementKind, TypeParameter, TypeSpec,
};

/// Rename `program`'s declarations to `module::name` and redirect its
/// references, including the names a selective import brought in directly.
///
/// `module` is `None` for the root file and the builtin prelude, which keep
/// their names and only need their aliases applied.
pub fn qualify(program: &mut Program, module: Option<&str>, aliases: &HashMap<String, String>) {
    let mut renames = aliases.clone();
    if let Some(module) = module {
        for name in declarations(&program.statements) {
            renames.insert(name.clone(), format!("{module}::{name}"));
        }
    }

    if renames.is_empty() {
        return;
    }
    let mut renamer = Renamer {
        renames: &renames,
        locals: Vec::new(),
    };
    for statement in &mut program.statements {
        renamer.declaration(statement);
    }
}

/// Names declared at the top level of a module.
fn declarations(statements: &[Statement]) -> Vec<&String> {
    statements
        .iter()
        .filter_map(|statement| match &statement.kind {
            StatementKind::Function { name, .. }
            | StatementKind::Struct { name, .. }
            | StatementKind::Enum { name, .. }
            | StatementKind::Trait { name, .. }
            | StatementKind::Var {
                name,
                is_const: true,
                ..
            } => Some(name),
            _ => None,
        })
        .collect()
}

struct Renamer<'a> {
    renames: &'a HashMap<String, String>,
    /// Names bound by the scopes open at this point, innermost last.
    locals: Vec<String>,
}

impl Renamer<'_> {
    /// A value or a path. A local hides a module item of the same name, and a
    /// path such as `Color::Red` is renamed by its first part.
    fn name(&self, name: &mut String) {
        let head = name.split("::").next().unwrap_or_default();
        if self.locals.iter().any(|local| local == head) {
            return;
        }
        if let Some(renamed) = self.renames.get(head) {
            *name = format!("{renamed}{}", &name[head.len()..]);
        }
    }

    /// A type, which no local can hide.
    fn type_name(&self, name: &mut String) {
        if let Some(renamed) = self.renames.get(name.as_str()) {
            *name = renamed.clone();
        }
    }

    fn declaration(&mut self, statement: &mut Statement) {
        match &mut statement.kind {
            StatementKind::Var {
                name,
                value,
                type_annotation,
                ..
            } => {
                self.type_name(name);
                self.expression(value);
                self.optional_type(type_annotation);
            }

            StatementKind::Function {
                name,
                type_params,
                params,
                return_type,
                body,
            } => {
                self.type_name(name);
                self.function(type_params, params, return_type, body);
            }

            StatementKind::Struct {
                name,
                type_params,
                fields,
                methods,
            } => {
                self.type_name(name);
                self.bounds(type_params);
                for (_, spec) in fields.iter_mut() {
                    self.ty(spec);
                }
                // A method's name belongs to its struct, not to the module.
                for method in methods {
                    if let StatementKind::Function {
                        type_params,
                        params,
                        return_type,
                        body,
                        ..
                    } = &mut method.kind
                    {
                        self.function(type_params, params, return_type, body);
                    }
                }
            }

            StatementKind::Enum { name, .. } => self.type_name(name),

            StatementKind::Trait { name, methods } => {
                self.type_name(name);
                for method in methods {
                    for (_, spec, _) in method.params.iter_mut() {
                        self.ty(spec);
                    }
                    self.optional_type(&mut method.return_type);
                }
            }

            _ => self.statement(statement),
        }
    }

    fn function(
        &mut self,
        type_params: &mut [TypeParameter],
        params: &mut [(String, TypeSpec, bool)],
        return_type: &mut Option<TypeSpec>,
        body: &mut [Statement],
    ) {
        self.bounds(type_params);
        for (_, spec, _) in params.iter_mut() {
            self.ty(spec);
        }
        self.optional_type(return_type);

        let outer = self.locals.len();
        self.locals
            .extend(params.iter().map(|(name, _, _)| name.clone()));
        self.statements(body);
        self.locals.truncate(outer);
    }

    fn bounds(&self, type_params: &mut [TypeParameter]) {
        for bound in type_params.iter_mut().filter_map(|p| p.bound.as_mut()) {
            self.type_name(bound);
        }
    }

    /// Statements sharing one scope: what they declare is gone after them.
    fn statements(&mut self, statements: &mut [Statement]) {
        let outer = self.locals.len();
        for statement in statements {
            self.statement(statement);
        }
        self.locals.truncate(outer);
    }

    fn statement(&mut self, statement: &mut Statement) {
        match &mut statement.kind {
            // The value is read before the name exists: `var min = min(1, 2)`
            // calls the module's `min`.
            StatementKind::Var {
                name,
                value,
                type_annotation,
                ..
            } => {
                self.expression(value);
                self.optional_type(type_annotation);
                self.locals.push(name.clone());
            }

            StatementKind::Return(value) => {
                if let Some(value) = value {
                    self.expression(value);
                }
            }
            StatementKind::Expression(expr) => self.expression(expr),
            StatementKind::Block(body) => self.statements(body),
            StatementKind::While { cond, body } => {
                self.expression(cond);
                self.statement(body);
            }
            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => {
                self.expression(iterable);
                let outer = self.locals.len();
                self.locals.push(variable.clone());
                self.statement(body);
                self.locals.truncate(outer);
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.expression(condition);
                self.statement(then_branch);
                if let Some(branch) = else_branch {
                    self.statement(branch);
                }
            }

            StatementKind::Function { .. }
            | StatementKind::Struct { .. }
            | StatementKind::Enum { .. }
            | StatementKind::Trait { .. } => self.declaration(statement),

            StatementKind::Break | StatementKind::Continue | StatementKind::Import { .. } => {}
        }
    }

    fn expression(&mut self, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Identifier(name) => self.name(name),

            ExpressionKind::StructLiteral { name, fields } => {
                self.name(name);
                for (_, value) in fields.iter_mut() {
                    self.expression(value);
                }
            }

            ExpressionKind::Prefix { right, .. } => self.expression(right),
            ExpressionKind::Infix { left, right, .. } => {
                self.expression(left);
                self.expression(right);
            }
            ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.expression(function);
                self.expressions(arguments);
            }
            // The field name belongs to the struct, not the module.
            ExpressionKind::Get { object, .. } => self.expression(object),
            ExpressionKind::Assign { target, value, .. } => {
                self.expression(target);
                self.expression(value);
            }
            ExpressionKind::Index { left, index } => {
                self.expression(left);
                self.expression(index);
            }
            ExpressionKind::Cast { left, target } => {
                self.expression(left);
                self.ty(target);
            }
            ExpressionKind::Match { value, arms } => {
                self.expression(value);
                for (pattern, result) in arms.iter_mut() {
                    self.expression(pattern);
                    self.expression(result);
                }
            }
            ExpressionKind::ArrayLiteral(elements) | ExpressionKind::Tuple(elements) => {
                self.expressions(elements)
            }
            ExpressionKind::BorrowRef(inner)
            | ExpressionKind::BorrowRefMut(inner)
            | ExpressionKind::Dereference(inner) => self.expression(inner),
            ExpressionKind::InlineAsm {
                outputs, inputs, ..
            } => {
                for operand in outputs.iter_mut().chain(inputs) {
                    self.expression(&mut operand.expr);
                }
            }

            ExpressionKind::Int(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::StringLit(_)
            | ExpressionKind::Boolean(_)
            | ExpressionKind::None => {}
        }
    }

    fn expressions(&mut self, expressions: &mut [Expression]) {
        for expr in expressions {
            self.expression(expr);
        }
    }

    fn optional_type(&self, spec: &mut Option<TypeSpec>) {
        if let Some(spec) = spec {
            self.ty(spec);
        }
    }

    fn ty(&self, spec: &mut TypeSpec) {
        match spec {
            TypeSpec::Named(name) => self.type_name(name),
            TypeSpec::Generic { name, args } => {
                self.type_name(name);
                for arg in args.iter_mut() {
                    self.ty(arg);
                }
            }
            TypeSpec::Tuple(types) => {
                for ty in types.iter_mut() {
                    self.ty(ty);
                }
            }
            TypeSpec::Pointer(inner)
            | TypeSpec::Optional(inner)
            | TypeSpec::Result(inner)
            | TypeSpec::Slice(inner)
            | TypeSpec::Ref(inner)
            | TypeSpec::RefMut(inner) => self.ty(inner),
            TypeSpec::IntLiteral(_) => {}
        }
    }
}
