//! Types as written, resolved to interned [`Ty`]s.

use crate::ast::{PRIMITIVES, TypeSpec};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::ty::{Ty, TyKind, Types};

use super::{Checker, Scope, suggest};

impl Checker<'_> {
    pub(super) fn resolve_type(&mut self, spec: &TypeSpec, scope: &Scope, span: Span) -> Ty {
        match spec {
            TypeSpec::Named(name) => self.resolve_named(name, scope, span),
            TypeSpec::Generic { name, args } if name == "Array" && args.len() == 2 => {
                let elem = self.resolve_type(&args[0], scope, span);
                let len = match &args[1] {
                    TypeSpec::IntLiteral(len) => self.fitting_length(i128::from(*len), span),
                    TypeSpec::Named(name) => match self.items.lookup_value(scope.module, name) {
                        Some(def) if self.items.item(def).kind == ItemKind::Const => {
                            self.check_visible(def, scope.module, span);
                            let mut value = self.const_value(def, span);
                            value.span = span;
                            self.length(&value)
                        }
                        _ => {
                            self.error(format!("Array length '{name}' is not a constant"), span);
                            return Types::ERROR;
                        }
                    },
                    _ => {
                        self.error("Array length must be an integer or a constant", span);
                        return Types::ERROR;
                    }
                };
                let Some(len) = len else {
                    return Types::ERROR;
                };
                self.intern_or_error(TyKind::Array(elem, len), &[elem])
            }
            TypeSpec::Generic { name, args } if name == "Vec" && args.len() == 1 => {
                let elem = self.resolve_type(&args[0], scope, span);
                self.intern_or_error(TyKind::Vec(elem), &[elem])
            }
            TypeSpec::Generic { name, args } => {
                let Some(def) = self.items.lookup_type(scope.module, name) else {
                    if self.foreign_generic(name, span) {
                        return Types::ERROR;
                    }
                    return self.resolve_named(name, scope, span);
                };
                self.check_visible(def, scope.module, span);
                let args: Vec<Ty> = args
                    .iter()
                    .map(|arg| self.resolve_type(arg, scope, span))
                    .collect();
                self.generic_adt(def, args, span)
            }
            TypeSpec::IntLiteral(_) => {
                self.error("Unexpected integer literal in type position", span);
                Types::ERROR
            }
            TypeSpec::Tuple(parts) => {
                let parts: Vec<Ty> = parts
                    .iter()
                    .map(|part| self.resolve_type(part, scope, span))
                    .collect();
                self.intern_or_error(TyKind::Tuple(parts.clone()), &parts)
            }
            TypeSpec::Pointer(inner) if **inner == TypeSpec::Named("void".to_string()) => {
                self.types.intern(TyKind::Ptr(Types::VOID))
            }
            TypeSpec::Pointer(inner) => self.wrap(inner, scope, span, TyKind::Ptr),
            TypeSpec::Optional(inner) => self.wrap(inner, scope, span, TyKind::Optional),
            TypeSpec::Slice(inner) => self.wrap(inner, scope, span, TyKind::Slice),
            TypeSpec::SliceMut(inner) => self.wrap(inner, scope, span, TyKind::SliceMut),
            TypeSpec::Ref(inner) => self.wrap(inner, scope, span, TyKind::Ref),
            TypeSpec::RefMut(inner) => self.wrap(inner, scope, span, TyKind::RefMut),
            TypeSpec::Result(ok) => self.wrap(ok, scope, span, |ok| TyKind::Result(ok, Types::I32)),
        }
    }

    /// A function's return type, which alone may be `void`, as it is when
    /// none is written.
    pub(super) fn resolve_return(
        &mut self,
        spec: Option<&TypeSpec>,
        scope: &Scope,
        span: Span,
    ) -> Ty {
        match spec {
            None => Types::VOID,
            Some(TypeSpec::Named(name)) if name == "void" => Types::VOID,
            Some(spec) => self.resolve_type(spec, scope, span),
        }
    }

    fn wrap(&mut self, inner: &TypeSpec, scope: &Scope, span: Span, kind: fn(Ty) -> TyKind) -> Ty {
        let inner = self.resolve_type(inner, scope, span);
        self.intern_or_error(kind(inner), &[inner])
    }

    /// A type built of parts, or the error type if a part already failed.
    pub(super) fn intern_or_error(&mut self, kind: TyKind, parts: &[Ty]) -> Ty {
        if parts.iter().any(|&part| self.types.is_error(part)) {
            return Types::ERROR;
        }
        self.types.intern(kind)
    }

    fn resolve_named(&mut self, name: &str, scope: &Scope, span: Span) -> Ty {
        if let Some(at) = scope.generics.iter().position(|g| g == name) {
            return match scope.args.get(at) {
                Some(&arg) => arg,
                None => self.types.intern(TyKind::Param(at as u32)),
            };
        }
        if name == "String" {
            return Types::STRING;
        }
        if let Some(def) = self.items.lookup_type(scope.module, name) {
            self.check_visible(def, scope.module, span);
            let generics = super::generics_of(self.items.item(def).decl).len();
            if generics > 0 {
                let base = self.items.item(def).name.clone();
                self.error(
                    format!("'{base}' takes {generics} type argument(s), got 0"),
                    span,
                );
                return Types::ERROR;
            }
            return self.types.intern(TyKind::Adt(def, Vec::new()));
        }
        if name != "str"
            && let Some(ty) = self.types.primitive(name)
        {
            if ty == Types::VOID {
                self.error(
                    "void has no values; only what a function returns can be void",
                    span,
                );
                return Types::ERROR;
            }
            return ty;
        }
        if name == "self" {
            return Types::ERROR;
        }
        let mut candidates: Vec<String> = PRIMITIVES.iter().map(|p| p.to_string()).collect();
        candidates.extend(
            self.items
                .items
                .iter()
                .filter(|item| matches!(item.kind, ItemKind::Struct | ItemKind::Enum))
                .filter(|item| self.items.lookup_type(scope.module, &item.name).is_some())
                .map(|item| item.name.clone()),
        );
        let sources = self.sources;
        let at = sources.word_in(span, name);
        let error = self.error(format!("Unknown type '{name}'"), at.unwrap_or(span));
        if let Some(ours) = suggest::type_alias(name) {
            match ours {
                "str" => error.help("text is 'str', or 'String' where it owns its bytes"),
                _ => error.help(format!("Zeru calls it '{ours}'")),
            };
            if let Some(at) = at {
                error.fix(at, ours);
            }
        } else if let Some(module) = name
            .split_once("::")
            .and_then(|(short, item)| suggest::std_module_with(short, item))
        {
            suggest::suggest_import(sources, error, span, &module, None);
        } else if let Some(module) = suggest::std_home(name) {
            suggest::suggest_import(sources, error, span, &module, Some(name));
        } else {
            suggest::did_you_mean(error, name, &candidates, at);
        }
        Types::ERROR
    }

    /// `Option<T>` or `Result<T, E>`, which Zeru writes `T?` and `T!`.
    fn foreign_generic(&mut self, name: &str, span: Span) -> bool {
        let (zeru, mark) = match name {
            "Option" => ("an optional", '?'),
            "Result" => ("a result", '!'),
            _ => return false,
        };
        let written = suggest::generic_in(self.sources, span, name);
        let error = self.error(
            format!("Unknown type '{name}'"),
            written.as_ref().map_or(span, |w| w.0),
        );
        error.help(format!("Zeru writes {zeru} of T as T{mark}"));
        if let Some((at, inner)) = written {
            let first = split_top(&inner);
            let wrapped = match first.starts_with(['&', '*']) {
                true => format!("({first}){mark}"),
                false => format!("{first}{mark}"),
            };
            error.fix(at, wrapped);
        }
        true
    }

    /// `Pair<i32>`: the struct at those arguments, if they meet its bounds.
    pub(super) fn generic_adt(&mut self, def: DefId, args: Vec<Ty>, span: Span) -> Ty {
        if args.iter().any(|&arg| self.types.is_error(arg)) {
            return Types::ERROR;
        }
        let generics = super::generics_of(self.items.item(def).decl).to_vec();
        if generics.len() != args.len() {
            let base = self.items.item(def).name.clone();
            self.error(
                format!(
                    "'{base}' takes {} type argument(s), got {}",
                    generics.len(),
                    args.len()
                ),
                span,
            );
            return Types::ERROR;
        }
        if !self.check_bounds(def, &generics, &args, span) {
            return Types::ERROR;
        }
        self.types.intern(TyKind::Adt(def, args))
    }
}

/// The first of the comma-separated parts of `text`, not looking inside
/// brackets: `i32` of `i32, Error`.
fn split_top(text: &str) -> &str {
    let mut depth = 0;
    for (at, c) in text.char_indices() {
        match c {
            '<' | '(' | '[' => depth += 1,
            '>' | ')' | ']' => depth -= 1,
            ',' if depth == 0 => return text[..at].trim(),
            _ => {}
        }
    }
    text.trim()
}
