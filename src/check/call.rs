//! Calls: functions, methods, generic inference, the constructors of enums,
//! results and Vecs, the built-in methods, and `print`.

use crate::ast::{Expression, ExpressionKind, TypeSpec, format_pieces};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::tir::{Arm, Builtin, Expr, ExprKind, Payload, Place};
use crate::ty::{Ty, TyKind, Types};

use super::Receiver;
use super::body::FnCx;
use super::expr::is_number_literal;

/// The built-in ways to write text: to stdout or stderr, with or without a
/// newline after.
const PRINTS: &[&str] = &["print", "println", "eprint", "eprintln"];

/// Writes out what the print streams hold; the compiler emits it.
const COPY_FN: &str = "__zeru_copy_bytes";
/// str methods, each the prelude's `__zeru_str_<name>` with the str first.
const STR_METHODS: &[&str] = &[
    "split",
    "lines",
    "trim",
    "starts_with",
    "ends_with",
    "parse_int",
    "parse_float",
];

/// Vec methods written in the prelude, each `__zeru_vec_<name>` taking a
/// reference to the Vec first.
const VEC_STD_METHODS: &[&str] = &["sort", "swap", "contains", "sum", "min", "max", "push_str"];

/// The `Vec` methods that change it, so need a receiver declared `var`.
const VEC_MUTATORS: &[&str] = &[
    "sort",
    "push_str",
    "swap",
    "push",
    "pop",
    "clear",
    "insert",
    "remove",
    "reserve",
    "shrink_to_fit",
];

impl FnCx<'_, '_> {
    /// A call, `type_args` given only to a function declared with type
    /// parameters.
    pub(super) fn call(
        &mut self,
        function: &Expression,
        type_args: &[TypeSpec],
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let (call, name) = match &function.kind {
            ExpressionKind::Identifier(name) => (
                self.named_call(name, function.span, type_args, args, expected, span),
                name,
            ),
            ExpressionKind::Get { object, name } => (
                self.method_call(object, name, type_args, args, expected, span),
                name,
            ),
            _ => {
                self.error("Invalid call expression", span);
                return self.error_expr(span);
            }
        };
        let takes_types = matches!(
            call.kind,
            ExprKind::Call(..) | ExprKind::Builtin(Builtin::VecNew | Builtin::VecWithCapacity, _)
        );
        if !type_args.is_empty() && !takes_types && !self.cx.types.is_error(call.ty) {
            self.error(format!("'{name}' takes no type arguments"), span);
        }
        call
    }

    fn check_args_only(&mut self, args: &[Expression]) {
        for arg in args {
            self.expr(arg, None);
        }
    }

    fn named_call(
        &mut self,
        name: &str,
        function_span: Span,
        type_args: &[TypeSpec],
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let module = self.scope.module;
        if PRINTS.contains(&name) {
            return self.print(name, args, span);
        }
        if name == "assert" {
            return self.assert(args, span);
        }
        if name == crate::tir::FLUSH_FN && self.local(name).is_none() {
            return self.mk(
                ExprKind::Builtin(Builtin::Flush, Vec::new()),
                Types::VOID,
                span,
            );
        }
        if name == COPY_FN && self.local(name).is_none() {
            let byte_ptr = self.cx.types.intern(TyKind::Ptr(Types::U8));
            if args.len() != 3 {
                self.error(
                    format!("{COPY_FN} takes a destination, a source and a count"),
                    span,
                );
                self.check_args_only(args);
                return self.error_expr(span);
            }
            let values = args
                .iter()
                .zip([byte_ptr, byte_ptr, Types::USIZE])
                .map(|(arg, want)| {
                    let value = self.expr(arg, Some(want));
                    self.coerce_or(value, want, arg.span, |want, got| {
                        format!("{COPY_FN} expects {want}, got {got}")
                    })
                })
                .collect();
            return self.mk(
                ExprKind::Builtin(Builtin::CopyBytes, values),
                Types::VOID,
                span,
            );
        }
        if let Some((owner, variant)) = name.rsplit_once("::")
            && let Some(def) = self.cx.items.lookup_type(module, owner)
        {
            match self.cx.items.item(def).kind {
                ItemKind::Enum => return self.variant(def, owner, variant, args, span),
                ItemKind::Struct => {
                    self.error(
                        format!("Call a struct's function with '.': {owner}.{variant}()"),
                        function_span,
                    );
                    self.check_args_only(args);
                    return self.error_expr(span);
                }
                _ => {}
            }
        }
        match name {
            "Ok" => return self.ok(args, expected, span),
            "Err" => return self.err(args, expected, span),
            _ => {}
        }
        let Some(def) = self
            .cx
            .items
            .lookup_value(module, name)
            .filter(|&def| self.cx.items.item(def).kind == ItemKind::Fn)
        else {
            let message = match name {
                "Some" => {
                    "Write the value itself, not Some(..): it becomes a T? wherever one is wanted"
                        .to_string()
                }
                _ => format!("Function '{name}' not defined"),
            };
            self.error(message, span);
            return self.error_expr(span);
        };
        self.cx.check_visible(def, module, span);
        let Some(known) = self.known_type_args(def, Vec::new(), type_args, span) else {
            self.check_args_only(args);
            return self.error_expr(span);
        };
        self.call_def(def, known, None, args, expected, span)
    }

    fn method_call(
        &mut self,
        object: &Expression,
        name: &str,
        type_args: &[TypeSpec],
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        if let ExpressionKind::Identifier(type_name) = &object.kind
            && self.local(type_name).is_none()
        {
            if type_name == "Vec" {
                return self.vec_static(name, type_args, args, expected, span);
            }
            if type_name == "String" {
                if !type_args.is_empty() {
                    self.error("'String' takes no type arguments", span);
                    self.check_args_only(args);
                    return self.error_expr(span);
                }
                if name == "from" {
                    let def = self.cx.prelude_def("__zeru_string_from");
                    return self.call_def(def, Vec::new(), None, args, expected, span);
                }
                return self.vec_static(name, &[], args, Some(Types::STRING), span);
            }
            if let Some(def) = self.cx.items.lookup_type(self.scope.module, type_name)
                && self.cx.items.item(def).kind == ItemKind::Struct
            {
                let Some((method, known)) =
                    self.static_callee(def, type_name, name, type_args, expected, span)
                else {
                    self.check_args_only(args);
                    return self.error_expr(span);
                };
                return self.call_def(method, known, None, args, expected, span);
            }
        }

        let (place, ty) = self.place(object);
        let types = &self.cx.types;
        if types.is_error(ty) {
            self.check_args_only(args);
            return self.error_expr(span);
        }

        if name == "copy"
            && args.is_empty()
            && let TyKind::Optional(reference) = *self.cx.types.kind(ty)
            && let TyKind::Ref(inner) | TyKind::RefMut(inner) = *self.cx.types.kind(reference)
        {
            if self.copies_drop(inner, span) {
                return self.error_expr(span);
            }
            return self.copy_referent(place, reference, inner, span);
        }
        let through = matches!(self.cx.types.kind(ty), TyKind::Ref(_) | TyKind::RefMut(_));
        let (place, ty) = self.through_ref(place, ty, object.span);
        if name == "copy" && args.is_empty() {
            if self.copies_drop(ty, span) {
                return self.error_expr(span);
            }
            if self.cx.owns_heap(ty) {
                return self.mk(ExprKind::Copy(place), ty, span);
            }
            if !through && !self.in_instance {
                let shown = self.ty(ty);
                self.error(
                    format!("{shown} holds no memory of its own, so it is copied already; drop the .copy()"),
                    span,
                );
            }
            return self.mk(ExprKind::Read(place), ty, span);
        }

        match self.cx.types.kind(ty).clone() {
            TyKind::Vec(elem) if elem == Types::U8 && STR_METHODS.contains(&name) => {
                let text = self.mk(ExprKind::Read(place), ty, object.span);
                let text = self.view(text);
                self.str_method(text, name, args, expected, span)
            }
            TyKind::Vec(elem) => {
                let mutable = VEC_MUTATORS.contains(&name);
                if mutable {
                    self.require_mutable(&place, object, span);
                }
                let receiver = self.address(place, ty, mutable, object.span);
                if name == "push_str" && elem != Types::U8 {
                    let shown = self.ty(ty);
                    self.error(format!("push_str() needs a String, not {shown}"), span);
                    return self.error_expr(span);
                }
                if VEC_STD_METHODS.contains(&name) {
                    let def = self.cx.prelude_def(&format!("__zeru_vec_{name}"));
                    return self.call_def(def, vec![elem], Some(receiver), args, expected, span);
                }
                self.vec_method(name, elem, receiver, args, span)
            }
            TyKind::Slice(_) => {
                if name == "len" && args.is_empty() {
                    let slice = self.mk(ExprKind::Read(place), ty, object.span);
                    return self.mk(
                        ExprKind::Builtin(Builtin::Len, vec![slice]),
                        Types::USIZE,
                        span,
                    );
                }
                if ty == Types::STR && STR_METHODS.contains(&name) {
                    let text = self.mk(ExprKind::Read(place), ty, object.span);
                    return self.str_method(text, name, args, expected, span);
                }
                let shown = self.ty(ty);
                self.error(format!("{shown} has no method '{name}'"), span);
                self.error_expr(span)
            }
            TyKind::Optional(_) | TyKind::Result(..) => {
                self.fallible_method(name, place, ty, object.span, args, span)
            }
            TyKind::Adt(..) | TyKind::Ptr(_) => {
                let (place, struct_ty) = match *self.cx.types.kind(ty) {
                    TyKind::Ptr(_) => self.deref(place, ty, object.span),
                    _ => (place, ty),
                };
                let Some(owner) = self.cx.is_struct(struct_ty) else {
                    let shown = self.ty(ty);
                    self.error(format!("{shown} has no method '{name}'"), span);
                    return self.error_expr(span);
                };
                let TyKind::Adt(_, struct_args) = self.cx.types.kind(struct_ty).clone() else {
                    unreachable!()
                };
                let shown = self.ty(struct_ty);
                let Some(def) = self.cx.items.method(owner, name) else {
                    self.error(format!("Struct '{shown}' has no method '{name}'"), span);
                    self.check_args_only(args);
                    return self.error_expr(span);
                };
                self.cx.check_visible(def, self.scope.module, span);
                let sig = self.cx.sig(def);
                let receiver = sig.as_ref().map(|sig| sig.receiver);
                match receiver {
                    Some(Receiver::None) => {
                        let full = self.cx.items.item(def).name.clone();
                        self.error(
                            format!("'{full}' takes no 'self'; call it on the type, not a value"),
                            span,
                        );
                        self.check_args_only(args);
                        return self.error_expr(span);
                    }
                    Some(Receiver::Mutable) => self.require_mutable(&place, object, span),
                    _ => {}
                }
                let Some(known) = self.known_type_args(def, struct_args, type_args, span) else {
                    self.check_args_only(args);
                    return self.error_expr(span);
                };
                let mutable = receiver == Some(Receiver::Mutable);
                let receiver = self.address(place, struct_ty, mutable, object.span);
                self.call_def(def, known, Some(receiver), args, expected, span)
            }
            kind => {
                let shown = self.ty(ty);
                let hint = match (kind, name) {
                    (TyKind::Array(_, len), "len") => format!("; its length is {len}"),
                    _ => String::new(),
                };
                self.error(format!("{shown} has no method '{name}'{hint}"), span);
                self.error_expr(span)
            }
        }
    }

    /// Report a `.copy()` of a `ty` that runs a `drop`, which would run once
    /// for each copy.
    fn copies_drop(&mut self, ty: Ty, span: Span) -> bool {
        if !self.cx.runs_drop(ty) {
            return false;
        }
        let shown = self.ty(ty);
        self.error(
            format!("Cannot copy {shown}: it runs a drop, or holds a value that does, which both copies would run"),
            span,
        );
        true
    }

    /// `.copy()` on a `(&T)?`: a `T?` holding a copy of what it refers to.
    fn copy_referent(&mut self, scrutinee: Place, reference: Ty, inner: Ty, span: Span) -> Expr {
        self.enter();
        let binding = self.declare("", reference, false, true);
        self.leave();
        let pointer = self.mk(ExprKind::Read(Place::Local(binding)), reference, span);
        let referent = Place::Deref(Box::new(pointer));
        let value = match self.cx.owns_heap(inner) {
            true => ExprKind::Copy(referent),
            false => ExprKind::Read(referent),
        };
        let ty = self.cx.types.intern(TyKind::Optional(inner));
        let some = self.mk(
            ExprKind::Some(Box::new(self.mk(value, inner, span))),
            ty,
            span,
        );
        let arms = vec![
            Arm {
                tags: vec![1],
                bindings: vec![(binding, vec![Payload::SomeValue])],
                body: some,
                span,
            },
            Arm {
                tags: Vec::new(),
                bindings: Vec::new(),
                body: self.mk(ExprKind::None, ty, span),
                span,
            },
        ];
        self.mk(ExprKind::Match { scrutinee, arms }, ty, span)
    }

    /// `text.name(args)`, a `str` method the prelude writes as
    /// `__zeru_str_<name>`.
    fn str_method(
        &mut self,
        text: Expr,
        name: &str,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let def = self.cx.prelude_def(&format!("__zeru_str_{name}"));
        self.call_def(def, Vec::new(), Some(text), args, expected, span)
    }

    pub(super) fn address(&mut self, place: Place, ty: Ty, mutable: bool, span: Span) -> Expr {
        let pointer = match mutable {
            true => self.cx.types.intern(TyKind::RefMut(ty)),
            false => self.cx.types.intern(TyKind::Ref(ty)),
        };
        self.note_borrow(&place);
        self.mk(ExprKind::AddressOf(place), pointer, span)
    }

    /// The function `Type.name(..)` calls, a struct's function that takes no
    /// `self`, and the type arguments known for it: all of them, a generic
    /// struct's first, when written as in `Pair.make<i32>(..)`, or else
    /// those of a generic struct, from the type the result goes to.
    fn static_callee(
        &mut self,
        def: DefId,
        type_name: &str,
        name: &str,
        type_args: &[TypeSpec],
        expected: Option<Ty>,
        span: Span,
    ) -> Option<(DefId, Vec<Ty>)> {
        self.cx.check_visible(def, self.scope.module, span);
        let generic = !super::generics_of(self.cx.items.item(def).decl).is_empty();
        let struct_args = if generic && type_args.is_empty() {
            match expected.map(|ty| (ty, self.cx.types.kind(ty).clone())) {
                Some((_, TyKind::Adt(d, args))) if d == def => args,
                Some((ty, _)) if self.cx.types.is_error(ty) => return None,
                _ => {
                    self.error(
                        format!("Cannot tell what {type_name}'s type arguments are here; give the variable a type, as in var x: {type_name}<..> = {type_name}.{name}()"),
                        span,
                    );
                    return None;
                }
            }
        } else {
            Vec::new()
        };
        let Some(method) = self.cx.items.method(def, name) else {
            let shown = self.cx.items.item(def).name.clone();
            self.error(format!("Struct '{shown}' has no method '{name}'"), span);
            return None;
        };
        self.cx.check_visible(method, self.scope.module, span);
        if self
            .cx
            .sig(method)
            .is_some_and(|sig| sig.receiver != Receiver::None)
        {
            let full = self.cx.items.item(method).name.clone();
            self.error(
                format!("'{full}' takes 'self'; call it on a value, as in x.{name}()"),
                span,
            );
            return None;
        }
        let known = self.known_type_args(method, struct_args, type_args, span)?;
        Some((method, known))
    }

    /// The type arguments of `def` known before its call is checked: `known`,
    /// then those written, which must be all the rest.
    fn known_type_args(
        &mut self,
        def: DefId,
        mut known: Vec<Ty>,
        written: &[TypeSpec],
        span: Span,
    ) -> Option<Vec<Ty>> {
        if written.is_empty() {
            return Some(known);
        }
        let wanted = self.cx.sig(def)?.generics.len() - known.len();
        if written.len() != wanted {
            let (full, got) = (self.cx.items.item(def).name.clone(), written.len());
            self.error(
                format!("'{full}' takes {wanted} type argument(s), got {got}"),
                span,
            );
            return None;
        }
        let scope = self.scope.clone();
        for spec in written {
            known.push(self.cx.resolve_type(spec, &scope, span));
        }
        (!known.iter().any(|&ty| self.cx.types.is_error(ty))).then_some(known)
    }

    /// A call to `def`, with the type arguments `known` given, and the others
    /// inferred: from the arguments, then from the type the result goes to,
    /// then from written numbers and `None`, which fit whatever the others
    /// settle.
    fn call_def(
        &mut self,
        def: DefId,
        known: Vec<Ty>,
        receiver: Option<Expr>,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        self.inference_root(expected, span, |this| {
            this.generic_call(def, known, receiver, args, expected, span)
        })
    }

    fn generic_call(
        &mut self,
        def: DefId,
        known: Vec<Ty>,
        receiver: Option<Expr>,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let full = self.cx.items.shown(def);
        let Some(sig) = self.cx.sig(def) else {
            return self.error_expr(span);
        };
        let skip = usize::from(receiver.is_some());
        let wanted = sig.params.len() - skip;
        if args.len() != wanted {
            let plural = if wanted == 1 { "" } else { "s" };
            self.error(
                format!(
                    "Function '{full}' expects {wanted} argument{plural}, got {}",
                    args.len()
                ),
                span,
            );
            self.check_args_only(args);
            return self.error_expr(span);
        }
        let mut generics = known;
        for param in &sig.generics[generics.len()..] {
            let var = self.new_var(&param.name, &full, span);
            generics.push(var);
        }
        let params = self.cx.types.subst_all(&sig.params[skip..], &generics);
        let ret = self.cx.types.subst(sig.ret, &generics);

        let (late, early): (Vec<usize>, Vec<usize>) =
            (0..args.len()).partition(|&at| written_value(&args[at]));
        let mut checked: Vec<Option<Expr>> = (0..args.len()).map(|_| None).collect();
        for at in early {
            checked[at] = Some(self.argument(&args[at], params[at], at));
        }
        if let Some(expected) = expected {
            self.fits(expected, ret, false);
        }
        for at in late {
            checked[at] = Some(self.argument(&args[at], params[at], at));
        }

        let Some(id) = self.call_instance(def, generics, span) else {
            return self.error_expr(span);
        };
        let mut values: Vec<Expr> = receiver.into_iter().collect();
        values.extend(checked.into_iter().map(Option::unwrap));
        self.mk(ExprKind::Call(id, values), ret, span)
    }

    /// Argument `at` of a call, going where `param` is wanted.
    fn argument(&mut self, arg: &Expression, param: Ty, at: usize) -> Expr {
        let value = self.value(arg, Some(param));
        let index = at + 1;
        let mut value = self.coerce_or(value, param, arg.span, |want, got| {
            format!("Argument {index} type mismatch: expected {want}, got {got}")
        });
        let param = self.resolve(param);
        if !matches!(
            self.cx.types.kind(param),
            TyKind::Ref(_) | TyKind::RefMut(_)
        ) {
            self.consume(&mut value);
        }
        value
    }

    fn variant(
        &mut self,
        def: DefId,
        owner: &str,
        variant: &str,
        args: &[Expression],
        span: Span,
    ) -> Expr {
        self.cx.check_visible(def, self.scope.module, span);
        let path = format!("{owner}::{variant}");
        let variants = self.cx.variants_of(def);
        let ty = self.cx.types.intern(TyKind::Adt(def, Vec::new()));
        let Some(index) = variants.iter().position(|(v, _)| v == variant) else {
            self.error(format!("Enum '{owner}' has no variant '{variant}'"), span);
            self.check_args_only(args);
            return self.error_expr(span);
        };
        let fields = variants[index].1.clone();
        if args.len() != fields.len() {
            self.error(
                format!(
                    "'{path}' carries {} value(s), got {}",
                    fields.len(),
                    args.len()
                ),
                span,
            );
            self.check_args_only(args);
            return self.mk(
                ExprKind::Variant(index as u32, Vec::new()),
                Types::ERROR,
                span,
            );
        }
        let mut values = Vec::new();
        for (arg, &field) in args.iter().zip(&fields) {
            let value = self.expr(arg, Some(field));
            let mut value = self.coerce_or(value, field, arg.span, |want, got| {
                format!("'{path}' takes {want} here, got {got}")
            });
            self.consume(&mut value);
            values.push(value);
        }
        self.mk(ExprKind::Variant(index as u32, values), ty, span)
    }

    /// `Ok()`, the success of a `!` function, which has no value to give; a
    /// value goes into a `T!` by itself.
    fn ok(&mut self, args: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        if let (Some(ret), []) = (expected, args)
            && self.cx.types.is_unit_result(ret)
        {
            return self.unit_ok(ret, span);
        }
        let message = match args {
            [] => "Ok() is the success of a ! function, which has no value to give",
            _ => "Write the value itself, not Ok(..): it becomes a T! wherever one is wanted",
        };
        self.error(message, span);
        self.check_args_only(args);
        self.error_expr(span)
    }

    fn err(&mut self, args: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let [arg] = args else {
            self.error("Err() takes exactly one argument, the error", span);
            self.check_args_only(args);
            return self.error_expr(span);
        };
        if expected.is_some_and(|ty| self.cx.types.is_error(ty)) {
            self.check_args_only(args);
            return self.error_expr(span);
        }
        let Some((ty, err)) = expected.and_then(|ty| match self.cx.types.kind(ty) {
            TyKind::Result(_, err) => Some((ty, *err)),
            _ => None,
        }) else {
            self.expr(arg, None);
            self.error(
                "Err() needs a T! to be part of; give it one, as in var r: i32! = Err(1)",
                span,
            );
            return self.error_expr(span);
        };
        let value = self.expr(arg, Some(err));
        let value = self.coerce_or(value, err, span, |want, got| {
            format!("Err() takes {want}, got {got}")
        });
        self.mk(ExprKind::Err(Box::new(value)), ty, span)
    }

    fn vec_static(
        &mut self,
        name: &str,
        type_args: &[TypeSpec],
        args: &[Expression],
        mut expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        match type_args {
            [] => {}
            [elem] => {
                let scope = self.scope.clone();
                let elem = self.cx.resolve_type(elem, &scope, span);
                expected = Some(self.cx.intern_or_error(TyKind::Vec(elem), &[elem]));
            }
            _ => {
                self.error(
                    format!("'Vec' takes 1 type argument(s), got {}", type_args.len()),
                    span,
                );
                self.check_args_only(args);
                return self.error_expr(span);
            }
        }
        let (ty, elem) = match expected.map(|ty| (ty, self.cx.types.kind(ty).clone())) {
            Some((ty, TyKind::Vec(elem))) => (ty, elem),
            Some((ty, _)) if self.cx.types.is_error(ty) => {
                self.check_args_only(args);
                return self.error_expr(span);
            }
            _ => {
                self.error(
                    "Cannot infer Vec element type. Please add a type annotation",
                    span,
                );
                self.check_args_only(args);
                return self.error_expr(span);
            }
        };
        let builtin = match name {
            "new" => Builtin::VecNew,
            "with_capacity" => Builtin::VecWithCapacity,
            _ => {
                let shown = self.ty(ty);
                self.error(format!("{shown} has no function '{name}'"), span);
                return self.error_expr(span);
            }
        };
        let values = self.vec_args(name, elem, args, span);
        self.mk(ExprKind::Builtin(builtin, values), ty, span)
    }

    fn vec_args(&mut self, name: &str, elem: Ty, args: &[Expression], span: Span) -> Vec<Expr> {
        let arity = match name {
            "with_capacity" | "push" | "get" | "remove" | "reserve" => 1,
            "insert" => 2,
            _ => 0,
        };
        let vec = self.cx.types.intern(TyKind::Vec(elem));
        let owner = self.ty(vec);
        if args.len() != arity {
            let plural = if arity == 1 { "" } else { "s" };
            self.error(
                format!("{owner}.{name}() takes exactly {arity} argument{plural}"),
                span,
            );
            self.check_args_only(args);
            return Vec::new();
        }
        let mut values = Vec::new();
        for (at, arg) in args.iter().enumerate() {
            let is_item = matches!(name, "push" | "insert") && at == arity - 1;
            if is_item {
                let value = self.expr(arg, Some(elem));
                let mut value = self.coerce_or(value, elem, span, |want, got| {
                    format!("{owner}.{name}() expects {want}, got {got}")
                });
                self.consume(&mut value);
                values.push(value);
            } else {
                let value = self.expr(arg, Some(Types::USIZE));
                if value.ty != Types::USIZE && !self.cx.types.is_error(value.ty) {
                    self.error(
                        format!("{owner}.{name}() index or count must be usize"),
                        span,
                    );
                }
                values.push(value);
            }
        }
        values
    }

    fn vec_method(
        &mut self,
        name: &str,
        elem: Ty,
        receiver: Expr,
        args: &[Expression],
        span: Span,
    ) -> Expr {
        let builtin = match name {
            "len" => Builtin::Len,
            "capacity" => Builtin::VecCapacity,
            "is_empty" => Builtin::VecIsEmpty,
            "push" => Builtin::VecPush,
            "insert" => Builtin::VecInsert,
            "remove" => Builtin::VecRemove,
            "pop" => Builtin::VecPop,
            "get" => Builtin::VecGet,
            "clear" => Builtin::VecClear,
            "reserve" => Builtin::VecReserve,
            "shrink_to_fit" => Builtin::VecShrinkToFit,
            _ => {
                self.check_args_only(args);
                let vec = self.cx.types.intern(TyKind::Vec(elem));
                let shown = self.ty(vec);
                self.error(format!("{shown} has no method '{name}'"), span);
                return self.error_expr(span);
            }
        };
        let mut values = vec![receiver];
        values.extend(self.vec_args(name, elem, args, span));
        let ty = match builtin {
            Builtin::Len | Builtin::VecCapacity => Types::USIZE,
            Builtin::VecIsEmpty => Types::BOOL,
            Builtin::VecRemove => elem,
            Builtin::VecGet => {
                let reference = self.cx.types.intern(TyKind::Ref(elem));
                self.cx.types.intern(TyKind::Optional(reference))
            }
            Builtin::VecPop => self.cx.types.intern(TyKind::Optional(elem)),
            _ => Types::VOID,
        };
        self.mk(ExprKind::Builtin(builtin, values), ty, span)
    }

    fn fallible_method(
        &mut self,
        name: &str,
        place: Place,
        ty: Ty,
        object_span: Span,
        args: &[Expression],
        span: Span,
    ) -> Expr {
        if !args.is_empty() {
            self.error(format!("{name}() takes exactly 0 arguments"), span);
        }
        let (builtin, result) = match (self.cx.types.kind(ty).clone(), name) {
            (TyKind::Optional(_), "is_some") => (Builtin::IsSome, Types::BOOL),
            (TyKind::Optional(_), "is_none") => (Builtin::IsNone, Types::BOOL),
            (TyKind::Result(..), "is_ok") => (Builtin::IsOk, Types::BOOL),
            (TyKind::Result(..), "is_err") => (Builtin::IsErr, Types::BOOL),
            (TyKind::Optional(inner) | TyKind::Result(inner, _), "unwrap") => {
                (Builtin::Unwrap, inner)
            }
            (TyKind::Result(_, err), "unwrap_err") => (Builtin::UnwrapErr, err),
            _ => {
                let shown = self.ty(ty);
                self.error(format!("{shown} has no method '{name}'"), span);
                return self.error_expr(span);
            }
        };
        let mut value = self.mk(ExprKind::Read(place), ty, object_span);
        if builtin == Builtin::Unwrap {
            self.consume(&mut value);
        }
        self.mk(ExprKind::Builtin(builtin, vec![value]), result, span)
    }

    /// `print("x is {}", x)`: a literal format with one `{}` per value, each
    /// a number, a bool or a `str`.
    fn print(&mut self, name: &str, args: &[Expression], span: Span) -> Expr {
        let Some((pieces, values)) = self.format(args, span) else {
            return self.error_expr(span);
        };
        let kind = ExprKind::Print {
            stderr: name.starts_with('e'),
            newline: name.ends_with("ln"),
            pieces,
            values,
        };
        self.mk(kind, Types::VOID, span)
    }

    /// `assert(cond)`, or `assert(cond, "format", values..)` with a message
    /// to print when `cond` is false.
    fn assert(&mut self, args: &[Expression], span: Span) -> Expr {
        let Some((cond, message)) = args.split_first() else {
            self.error("assert takes a condition, then maybe a message", span);
            return self.error_expr(span);
        };
        let cond = self.condition(cond, "assert takes a bool, got ");
        let (pieces, values) = match message {
            [] => (Vec::new(), Vec::new()),
            _ => match self.format(message, span) {
                Some(format) => format,
                None => return self.error_expr(span),
            },
        };
        let kind = ExprKind::Assert {
            cond: Box::new(cond),
            pieces,
            values,
        };
        self.mk(kind, Types::VOID, span)
    }

    /// A format string literal and the values for its `{}`, checked.
    fn format(&mut self, args: &[Expression], span: Span) -> Option<(Vec<Vec<u8>>, Vec<Expr>)> {
        let Some((format, values)) = args.split_first() else {
            self.error("print takes a format string first", span);
            return None;
        };
        let ExpressionKind::StringLit(text) = &format.kind else {
            self.error(
                "The format must be a string literal, as in print(\"{}\", x)",
                format.span,
            );
            self.check_args_only(values);
            return None;
        };
        let pieces = match format_pieces(text) {
            Err(message) => {
                self.error(message, format.span);
                Vec::new()
            }
            Ok(pieces) if pieces.len() - 1 != values.len() => {
                self.error(
                    format!(
                        "The format has {} '{{}}' but {} value(s) follow it",
                        pieces.len() - 1,
                        values.len()
                    ),
                    span,
                );
                Vec::new()
            }
            Ok(pieces) => pieces,
        };
        let mut checked = Vec::new();
        for value in values {
            let value = self.expr(value, None);
            let value = match self.cx.types.kind(value.ty) {
                TyKind::Ref(_) | TyKind::RefMut(_) => {
                    let (ty, span) = (value.ty, value.span);
                    let (place, ty) = self.deref(Place::Temp(Box::new(value)), ty, span);
                    self.mk(ExprKind::Read(place), ty, span)
                }
                _ if value.ty == Types::STRING => self.view(value),
                _ => value,
            };
            let types = &self.cx.types;
            let printable = types.is_number(value.ty)
                || value.ty == Types::BOOL
                || value.ty == Types::STR
                || types.is_error(value.ty);
            if !printable {
                let shown = self.ty(value.ty);
                self.error(
                    format!("Cannot print {shown}: print takes numbers, bools and str"),
                    value.span,
                );
            }
            checked.push(value);
        }
        Some((pieces, checked))
    }
}

/// A number written out, `None`, or arithmetic, an array or a tuple of
/// them: it fits whichever type the rest of a call settles.
fn written_value(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Infix { left, right, .. } => written_value(left) && written_value(right),
        ExpressionKind::ArrayLiteral(items) | ExpressionKind::Tuple(items) => {
            items.iter().all(written_value)
        }
        ExpressionKind::ArrayRepeat { value, .. } => written_value(value),
        ExpressionKind::None => true,
        _ => is_number_literal(expr),
    }
}
