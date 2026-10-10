//! Instantiate a generic declaration for one set of concrete types.
//!
//! A generic struct is not a type on its own, so each `Pair<i32>` becomes a
//! struct of its own with the parameter substituted throughout, methods
//! included. Everything downstream then sees an ordinary struct.

use std::collections::HashMap;

use crate::ast::{Statement, StatementKind, TypeParameter, TypeSpec, Visitor, walk_item};

pub type Substitutions = HashMap<String, TypeSpec>;

/// Replace every type parameter in `spec` with the type it stands for.
pub fn substitute(spec: &TypeSpec, subs: &Substitutions) -> TypeSpec {
    let boxed = |inner: &TypeSpec| Box::new(substitute(inner, subs));
    let all = |types: &[TypeSpec]| types.iter().map(|t| substitute(t, subs)).collect();

    match spec {
        TypeSpec::Named(name) => subs.get(name).cloned().unwrap_or_else(|| spec.clone()),
        TypeSpec::Pointer(inner) => TypeSpec::Pointer(boxed(inner)),
        TypeSpec::Optional(inner) => TypeSpec::Optional(boxed(inner)),
        TypeSpec::Result(ok, error) => TypeSpec::Result(boxed(ok), error.as_deref().map(boxed)),
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
        TypeSpec::Result(t, None) => format!("res_{}", mangle_type(t)),
        TypeSpec::Result(t, Some(e)) => format!("res_{}_{}", mangle_type(t), mangle_type(e)),
        TypeSpec::Slice(t) => format!("slice_{}", mangle_type(t)),
        TypeSpec::Ref(t) => format!("ref_{}", mangle_type(t)),
        TypeSpec::RefMut(t) => format!("refmut_{}", mangle_type(t)),
        TypeSpec::Tuple(elems) => format!("tuple_{}", joined(elems)),
        TypeSpec::Generic { name, args } => format!("{name}_{}", joined(args)),
    }
}

/// `decl`, a generic struct or function, with every parameter replaced and a
/// name of its own, so it reads as an ordinary declaration.
pub fn instantiate(decl: &Statement, name: String, subs: &Substitutions) -> Statement {
    let mut decl = decl.clone();
    if let StatementKind::Struct {
        name: decl_name,
        type_params,
        ..
    }
    | StatementKind::Function {
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

/// Apply `f` to every type written anywhere in `statement`, a top-level item,
/// including the bodies of the functions it holds.
pub fn map_types(statement: &mut Statement, f: &mut impl FnMut(&mut TypeSpec)) {
    struct MapTypes<F>(F);
    impl<F: FnMut(&mut TypeSpec)> Visitor for MapTypes<F> {
        fn ty(&mut self, spec: &mut TypeSpec) {
            (self.0)(spec);
        }
    }
    walk_item(&mut MapTypes(f), statement);
}
