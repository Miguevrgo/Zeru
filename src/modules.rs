//! Give each module's declarations a name of their own.
//!
//! Two modules may both declare `min`, so every declaration is renamed to
//! `module::min` and every reference in that module is pointed at the new name.
//! The rename walks the parsed tree, which is what keeps it away from string
//! literals and comments: they are not names. It also tracks the locals in
//! scope, since a parameter or a variable called `min` is not the module's.

use std::collections::HashMap;

use crate::ast::{Program, Statement, StatementKind, TypeSpec, Visitor, walk_item};

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
        walk_item(&mut renamer, statement);
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

impl Visitor for Renamer<'_> {
    /// A local hides a module item of the same name, and a path such as
    /// `Color::Red` is renamed by its first part.
    fn name(&mut self, name: &mut String) {
        let head = name.split("::").next().unwrap_or_default();
        if self.locals.iter().any(|local| local == head) {
            return;
        }
        if let Some(renamed) = self.renames.get(head) {
            *name = format!("{renamed}{}", &name[head.len()..]);
        }
    }

    /// A declaration or a type, which no local can hide.
    fn item(&mut self, name: &mut String) {
        if let Some(renamed) = self.renames.get(name.as_str()) {
            *name = renamed.clone();
        }
    }

    fn ty(&mut self, spec: &mut TypeSpec) {
        match spec {
            TypeSpec::Named(name) => self.item(name),
            TypeSpec::Generic { name, args } => {
                self.item(name);
                args.iter_mut().for_each(|arg| self.ty(arg));
            }
            TypeSpec::Tuple(types) => types.iter_mut().for_each(|ty| self.ty(ty)),
            TypeSpec::Pointer(inner)
            | TypeSpec::Optional(inner)
            | TypeSpec::Result(inner)
            | TypeSpec::Slice(inner)
            | TypeSpec::Ref(inner)
            | TypeSpec::RefMut(inner) => self.ty(inner),
            TypeSpec::IntLiteral(_) => {}
        }
    }

    fn bind(&mut self, name: &str) {
        self.locals.push(name.to_string());
    }

    fn scope(&self) -> usize {
        self.locals.len()
    }

    fn leave(&mut self, scope: usize) {
        self.locals.truncate(scope);
    }
}
