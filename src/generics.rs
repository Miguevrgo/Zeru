//! Instantiate a generic declaration for one set of concrete types.
//!
//! A generic struct is not a type on its own, so each `Pair<i32>` becomes a
//! struct of its own with the parameter substituted throughout, methods
//! included. Everything downstream then sees an ordinary struct.

use std::collections::HashMap;

use crate::ast::{Expression, ExpressionKind, Statement, StatementKind, TypeParameter, TypeSpec};

pub type Substitutions = HashMap<String, TypeSpec>;

/// Replace every type parameter in `spec` with the type it stands for.
pub fn substitute(spec: &TypeSpec, subs: &Substitutions) -> TypeSpec {
    let boxed = |inner: &TypeSpec| Box::new(substitute(inner, subs));
    let all = |types: &[TypeSpec]| types.iter().map(|t| substitute(t, subs)).collect();

    match spec {
        TypeSpec::Named(name) => subs.get(name).cloned().unwrap_or_else(|| spec.clone()),
        TypeSpec::Pointer(inner) => TypeSpec::Pointer(boxed(inner)),
        TypeSpec::Optional(inner) => TypeSpec::Optional(boxed(inner)),
        TypeSpec::Result(inner) => TypeSpec::Result(boxed(inner)),
        TypeSpec::Slice(inner) => TypeSpec::Slice(boxed(inner)),
        TypeSpec::Ref(inner) => TypeSpec::Ref(boxed(inner)),
        TypeSpec::RefMut(inner) => TypeSpec::RefMut(boxed(inner)),
        TypeSpec::Tuple(elems) => TypeSpec::Tuple(all(elems)),
        TypeSpec::Generic { name, args } => TypeSpec::Generic {
            name: name.clone(),
            args: all(args),
        },
        TypeSpec::IntLiteral(_) => spec.clone(),
    }
}

/// The name an instantiation is emitted under: `Pair` with `T = i32` becomes
/// `Pair__i32_`, which no longer mentions a parameter.
pub fn mangle(base: &str, type_params: &[TypeParameter], subs: &Substitutions) -> String {
    let mut mangled = format!("{base}__");
    for param in type_params {
        if let Some(concrete) = subs.get(&param.name) {
            mangled.push_str(&mangle_type(concrete));
            mangled.push('_');
        }
    }
    mangled
}

pub fn mangle_type(spec: &TypeSpec) -> String {
    let joined = |types: &[TypeSpec]| types.iter().map(mangle_type).collect::<Vec<_>>().join("_");

    match spec {
        TypeSpec::Named(name) => name.clone(),
        TypeSpec::IntLiteral(n) => format!("lit{n}"),
        TypeSpec::Pointer(t) => format!("ptr_{}", mangle_type(t)),
        TypeSpec::Optional(t) => format!("opt_{}", mangle_type(t)),
        TypeSpec::Result(t) => format!("res_{}", mangle_type(t)),
        TypeSpec::Slice(t) => format!("slice_{}", mangle_type(t)),
        TypeSpec::Ref(t) => format!("ref_{}", mangle_type(t)),
        TypeSpec::RefMut(t) => format!("refmut_{}", mangle_type(t)),
        TypeSpec::Tuple(elems) => format!("tuple_{}", joined(elems)),
        TypeSpec::Generic { name, args } => format!("{name}_{}", joined(args)),
    }
}

/// `decl` with every parameter replaced and a name of its own, so it reads as
/// an ordinary struct declaration.
pub fn instantiate_struct(decl: &Statement, name: String, subs: &Substitutions) -> Statement {
    let mut decl = decl.clone();
    if let StatementKind::Struct {
        name: decl_name,
        type_params,
        ..
    } = &mut decl.kind
    {
        *decl_name = name;
        type_params.clear();
    }
    map_types(&mut decl, &mut |spec| *spec = substitute(spec, subs));
    decl
}

/// Apply `f` to every type written anywhere in `statement`, including the
/// bodies of the functions it holds.
pub fn map_types(statement: &mut Statement, f: &mut impl FnMut(&mut TypeSpec)) {
    match &mut statement.kind {
        StatementKind::Var {
            value,
            type_annotation,
            ..
        } => {
            map_expression_types(value, f);
            if let Some(spec) = type_annotation {
                f(spec);
            }
        }

        StatementKind::Function {
            params,
            return_type,
            body,
            ..
        } => {
            for (_, spec, _) in params.iter_mut() {
                f(spec);
            }
            if let Some(spec) = return_type {
                f(spec);
            }
            map_all(body, f);
        }

        StatementKind::Struct {
            fields, methods, ..
        } => {
            for (_, spec) in fields.iter_mut() {
                f(spec);
            }
            map_all(methods, f);
        }

        StatementKind::Trait { methods, .. } => {
            for method in methods.iter_mut() {
                for (_, spec, _) in method.params.iter_mut() {
                    f(spec);
                }
                if let Some(spec) = &mut method.return_type {
                    f(spec);
                }
            }
        }

        StatementKind::Return(value) => {
            if let Some(value) = value {
                map_expression_types(value, f);
            }
        }
        StatementKind::Expression(expr) => map_expression_types(expr, f),
        StatementKind::Block(body) => map_all(body, f),
        StatementKind::While { cond, body } => {
            map_expression_types(cond, f);
            map_types(body, f);
        }
        StatementKind::ForIn { iterable, body, .. } => {
            map_expression_types(iterable, f);
            map_types(body, f);
        }
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            map_expression_types(condition, f);
            map_types(then_branch, f);
            if let Some(branch) = else_branch {
                map_types(branch, f);
            }
        }

        StatementKind::Enum { .. }
        | StatementKind::Break
        | StatementKind::Continue
        | StatementKind::Import { .. } => {}
    }
}

fn map_all(statements: &mut [Statement], f: &mut impl FnMut(&mut TypeSpec)) {
    for statement in statements {
        map_types(statement, f);
    }
}

fn map_expression_types(expr: &mut Expression, f: &mut impl FnMut(&mut TypeSpec)) {
    match &mut expr.kind {
        // The target of a cast is written as a name, so only a type that is
        // itself a name can be spelled there.
        ExpressionKind::Cast { left, target } => {
            map_expression_types(left, f);
            if let ExpressionKind::Identifier(name) = &mut target.kind {
                let mut spec = TypeSpec::Named(name.clone());
                f(&mut spec);
                if let TypeSpec::Named(mapped) = spec {
                    *name = mapped;
                }
            }
        }

        ExpressionKind::StructLiteral { fields, .. } => {
            for (_, value) in fields.iter_mut() {
                map_expression_types(value, f);
            }
        }
        ExpressionKind::Prefix { right, .. } => map_expression_types(right, f),
        ExpressionKind::Infix { left, right, .. } => {
            map_expression_types(left, f);
            map_expression_types(right, f);
        }
        ExpressionKind::Call {
            function,
            arguments,
        } => {
            map_expression_types(function, f);
            map_each(arguments, f);
        }
        ExpressionKind::Get { object, .. } => map_expression_types(object, f),
        ExpressionKind::Assign { target, value, .. } => {
            map_expression_types(target, f);
            map_expression_types(value, f);
        }
        ExpressionKind::Index { left, index } => {
            map_expression_types(left, f);
            map_expression_types(index, f);
        }
        ExpressionKind::Match { value, arms } => {
            map_expression_types(value, f);
            for (pattern, result) in arms.iter_mut() {
                map_expression_types(pattern, f);
                map_expression_types(result, f);
            }
        }
        ExpressionKind::ArrayLiteral(elements) | ExpressionKind::Tuple(elements) => {
            map_each(elements, f)
        }
        ExpressionKind::AddressOf(inner)
        | ExpressionKind::BorrowRef(inner)
        | ExpressionKind::BorrowRefMut(inner)
        | ExpressionKind::Dereference(inner) => map_expression_types(inner, f),
        ExpressionKind::InlineAsm {
            outputs, inputs, ..
        } => {
            for operand in outputs.iter_mut().chain(inputs) {
                map_expression_types(&mut operand.expr, f);
            }
        }

        ExpressionKind::Identifier(_)
        | ExpressionKind::Int(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::StringLit(_)
        | ExpressionKind::Boolean(_)
        | ExpressionKind::None => {}
    }
}

fn map_each(expressions: &mut [Expression], f: &mut impl FnMut(&mut TypeSpec)) {
    for expr in expressions {
        map_expression_types(expr, f);
    }
}
