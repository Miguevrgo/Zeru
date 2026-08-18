//! Give each module's declarations a name of their own.
//!
//! Two modules may both declare `min`, so every declaration is renamed to
//! `module::min` and every reference in that module is pointed at the new name.
//! The rename walks the parsed tree, which is what keeps it away from string
//! literals and comments: they are not names.

use std::collections::HashMap;

use crate::ast::{Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec};

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
    for statement in &mut program.statements {
        rename_statement(statement, &renames);
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

type Renames = HashMap<String, String>;

fn rename(name: &mut String, renames: &Renames) {
    if let Some(renamed) = renames.get(name) {
        *name = renamed.clone();
    }
}

fn rename_statement(statement: &mut Statement, renames: &Renames) {
    match &mut statement.kind {
        StatementKind::Var {
            name,
            value,
            type_annotation,
            ..
        } => {
            rename(name, renames);
            rename_expression(value, renames);
            rename_optional_type(type_annotation, renames);
        }

        StatementKind::Function {
            name,
            params,
            return_type,
            body,
            ..
        } => {
            rename(name, renames);
            for (_, spec, _) in params.iter_mut() {
                rename_type(spec, renames);
            }
            rename_optional_type(return_type, renames);
            rename_all(body, renames);
        }

        StatementKind::Struct {
            name,
            fields,
            methods,
            ..
        } => {
            rename(name, renames);
            for (_, spec) in fields.iter_mut() {
                rename_type(spec, renames);
            }
            rename_all(methods, renames);
        }

        StatementKind::Enum { name, .. } | StatementKind::Trait { name, .. } => {
            rename(name, renames)
        }

        StatementKind::Return(value) => {
            if let Some(value) = value {
                rename_expression(value, renames);
            }
        }
        StatementKind::Expression(expr) => rename_expression(expr, renames),
        StatementKind::Block(body) => rename_all(body, renames),
        StatementKind::While { cond, body } => {
            rename_expression(cond, renames);
            rename_statement(body, renames);
        }
        StatementKind::ForIn { iterable, body, .. } => {
            rename_expression(iterable, renames);
            rename_statement(body, renames);
        }
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rename_expression(condition, renames);
            rename_statement(then_branch, renames);
            if let Some(branch) = else_branch {
                rename_statement(branch, renames);
            }
        }

        StatementKind::Break | StatementKind::Continue | StatementKind::Import { .. } => {}
    }
}

fn rename_all(statements: &mut [Statement], renames: &Renames) {
    for statement in statements {
        rename_statement(statement, renames);
    }
}

fn rename_expression(expr: &mut Expression, renames: &Renames) {
    match &mut expr.kind {
        ExpressionKind::Identifier(name) => rename(name, renames),

        ExpressionKind::StructLiteral { name, fields } => {
            rename(name, renames);
            for (_, value) in fields.iter_mut() {
                rename_expression(value, renames);
            }
        }

        ExpressionKind::Prefix { right, .. } => rename_expression(right, renames),
        ExpressionKind::Infix { left, right, .. } => {
            rename_expression(left, renames);
            rename_expression(right, renames);
        }
        ExpressionKind::Call {
            function,
            arguments,
        } => {
            rename_expression(function, renames);
            rename_each(arguments, renames);
        }
        // The field name belongs to the struct, not the module.
        ExpressionKind::Get { object, .. } => rename_expression(object, renames),
        ExpressionKind::Assign { target, value, .. } => {
            rename_expression(target, renames);
            rename_expression(value, renames);
        }
        ExpressionKind::Index { left, index } => {
            rename_expression(left, renames);
            rename_expression(index, renames);
        }
        ExpressionKind::Cast { left, target } => {
            rename_expression(left, renames);
            rename_expression(target, renames);
        }
        ExpressionKind::Match { value, arms } => {
            rename_expression(value, renames);
            for (pattern, result) in arms.iter_mut() {
                rename_expression(pattern, renames);
                rename_expression(result, renames);
            }
        }
        ExpressionKind::ArrayLiteral(elements) | ExpressionKind::Tuple(elements) => {
            rename_each(elements, renames)
        }
        ExpressionKind::AddressOf(inner)
        | ExpressionKind::BorrowRef(inner)
        | ExpressionKind::BorrowRefMut(inner)
        | ExpressionKind::Dereference(inner) => rename_expression(inner, renames),
        ExpressionKind::InlineAsm {
            outputs, inputs, ..
        } => {
            for operand in outputs.iter_mut().chain(inputs) {
                rename_expression(&mut operand.expr, renames);
            }
        }

        ExpressionKind::Int(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::StringLit(_)
        | ExpressionKind::Boolean(_)
        | ExpressionKind::None => {}
    }
}

fn rename_each(expressions: &mut [Expression], renames: &Renames) {
    for expr in expressions {
        rename_expression(expr, renames);
    }
}

fn rename_optional_type(spec: &mut Option<TypeSpec>, renames: &Renames) {
    if let Some(spec) = spec {
        rename_type(spec, renames);
    }
}

fn rename_type(spec: &mut TypeSpec, renames: &Renames) {
    match spec {
        TypeSpec::Named(name) => rename(name, renames),
        TypeSpec::Generic { args, .. } => {
            for arg in args.iter_mut() {
                rename_type(arg, renames);
            }
        }
        TypeSpec::Tuple(types) => {
            for ty in types.iter_mut() {
                rename_type(ty, renames);
            }
        }
        TypeSpec::Pointer(inner)
        | TypeSpec::Optional(inner)
        | TypeSpec::Result(inner)
        | TypeSpec::Slice(inner)
        | TypeSpec::Ref(inner)
        | TypeSpec::RefMut(inner) => rename_type(inner, renames),
        TypeSpec::IntLiteral(_) => {}
    }
}
