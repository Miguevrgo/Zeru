//! Calls: functions, methods, generic inference, the constructors of enums,
//! results and Vecs, the built-in methods, and `print`.

use crate::ast::{Expression, ExpressionKind, format_pieces};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::tir::{Arm, Builtin, Expr, ExprKind, Payload, Place};
use crate::ty::{Ty, TyKind, Types};

use super::body::FnCx;
use super::expr::is_number_literal;
use super::{Receiver, Sig};

/// The built-in ways to write text: to stdout or stderr, with or without a
/// newline after.
const PRINTS: &[&str] = &["print", "println", "eprint", "eprintln"];

/// Writes out what the print streams hold; the compiler emits it.
const FLUSH_FN: &str = "__zeru_flush";
const COPY_FN: &str = "__zeru_copy_bytes";

/// The `Vec` methods that change it, so need a receiver declared `var`.
const VEC_MUTATORS: &[&str] = &[
    "push",
    "pop",
    "clear",
    "insert",
    "remove",
    "reserve",
    "shrink_to_fit",
];

impl FnCx<'_, '_> {
    pub(super) fn call(
        &mut self,
        function: &Expression,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        match &function.kind {
            ExpressionKind::Identifier(name) => {
                self.named_call(name, function.span, args, expected, span)
            }
            ExpressionKind::Get { object, name } => {
                self.method_call(object, name, args, expected, span)
            }
            _ => {
                self.error("Invalid call expression", span);
                self.error_expr(span)
            }
        }
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
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let module = self.scope.module;
        if PRINTS.contains(&name) {
            return self.print(name, args, span);
        }
        if name == FLUSH_FN && self.local(name).is_none() {
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
            self.error(format!("Function '{name}' not defined"), span);
            return self.error_expr(span);
        };
        self.cx.check_visible(def, module, span);
        self.call_def(def, Vec::new(), None, args, expected, span)
    }

    fn method_call(
        &mut self,
        object: &Expression,
        name: &str,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        if let ExpressionKind::Identifier(type_name) = &object.kind
            && self.local(type_name).is_none()
        {
            if type_name == "Vec" {
                return self.vec_static(name, args, expected, span);
            }
            if let Some(def) = self.cx.items.lookup_type(self.scope.module, type_name)
                && self.cx.items.item(def).kind == ItemKind::Struct
            {
                return self.static_call(def, type_name, name, args, expected, span);
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
            return self.copy_referent(place, reference, inner, span);
        }
        let through = matches!(self.cx.types.kind(ty), TyKind::Ref(_) | TyKind::RefMut(_));
        let (place, ty) = self.through_ref(place, ty, object.span);
        if name == "copy" && args.is_empty() {
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
            TyKind::Vec(elem) => {
                if VEC_MUTATORS.contains(&name) {
                    self.require_mutable(&place, object, span);
                }
                let receiver = self.address(place, ty, object.span);
                self.vec_method(name, elem, receiver, args, span)
            }
            TyKind::Slice(_) => {
                if name == "len" && args.is_empty() {
                    let slice = self.mk(ExprKind::Read(place), ty, object.span);
                    return self.mk(
                        ExprKind::Builtin(Builtin::SliceLen, vec![slice]),
                        Types::USIZE,
                        span,
                    );
                }
                self.error(format!("Slice has no method '{name}'"), span);
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
                    self.error(
                        format!("Cannot call method on non-struct type {shown}"),
                        span,
                    );
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
                match sig.as_ref().map(|sig| sig.receiver) {
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
                let receiver = self.address(place, struct_ty, object.span);
                self.call_def(def, struct_args, Some(receiver), args, expected, span)
            }
            _ => {
                let shown = self.ty(ty);
                self.error(
                    format!("Cannot call method on non-struct type {shown}"),
                    span,
                );
                self.error_expr(span)
            }
        }
    }

    /// `.copy()` on a `(&T)?`: a `T?` holding a copy of what it refers to,
    /// as Rust's `cloned()`.
    fn copy_referent(&mut self, scrutinee: Place, reference: Ty, inner: Ty, span: Span) -> Expr {
        self.enter();
        let binding = self.declare("", reference, false, true, true);
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
            },
            Arm {
                tags: Vec::new(),
                bindings: Vec::new(),
                body: self.mk(ExprKind::None, ty, span),
            },
        ];
        self.mk(ExprKind::Match { scrutinee, arms }, ty, span)
    }

    fn address(&mut self, place: Place, ty: Ty, span: Span) -> Expr {
        let pointer = self.cx.types.intern(TyKind::Ptr(ty));
        self.mk(ExprKind::AddressOf(place), pointer, span)
    }

    /// `Type.function(..)`, a struct's function that takes no `self`. A
    /// generic struct's arguments come from the type the result goes to.
    fn static_call(
        &mut self,
        def: DefId,
        type_name: &str,
        name: &str,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        self.cx.check_visible(def, self.scope.module, span);
        let generic = !super::generics_of(self.cx.items.item(def).decl).is_empty();
        let struct_args = if generic {
            match expected.map(|ty| (ty, self.cx.types.kind(ty).clone())) {
                Some((_, TyKind::Adt(d, args))) if d == def => args,
                Some((ty, _)) if self.cx.types.is_error(ty) => {
                    self.check_args_only(args);
                    return self.error_expr(span);
                }
                _ => {
                    self.error(
                        format!("Cannot tell what {type_name}'s type arguments are here; give the variable a type, as in var x: {type_name}<..> = {type_name}.{name}()"),
                        span,
                    );
                    self.check_args_only(args);
                    return self.error_expr(span);
                }
            }
        } else {
            Vec::new()
        };
        let Some(method) = self.cx.items.method(def, name) else {
            let shown = self.cx.items.item(def).name.clone();
            self.error(format!("Struct '{shown}' has no method '{name}'"), span);
            self.check_args_only(args);
            return self.error_expr(span);
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
            self.check_args_only(args);
            return self.error_expr(span);
        }
        self.call_def(method, struct_args, None, args, expected, span)
    }

    /// A call to `def`, with the arguments of its struct's type parameters
    /// known, and those of its own inferred from the arguments: written
    /// numbers and `None` last, since they fit whatever the others settle.
    fn call_def(
        &mut self,
        def: DefId,
        struct_args: Vec<Ty>,
        receiver: Option<Expr>,
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let full = self.cx.items.item(def).name.clone();
        let Some(sig) = self.cx.sig(def) else {
            return self.error_expr(span);
        };
        let Sig {
            params,
            generics,
            own_generics,
            ret,
            ..
        } = sig;
        let mut subs: Vec<Option<Ty>> = struct_args.into_iter().map(Some).collect();
        subs.resize(generics.len(), None);
        let params: Vec<Ty> = params[usize::from(receiver.is_some())..].to_vec();

        if args.len() != params.len() {
            self.error(
                format!(
                    "Function '{full}' expects {} arguments, got {}",
                    params.len(),
                    args.len()
                ),
                span,
            );
            self.check_args_only(args);
            return self.error_expr(span);
        }

        let deferred =
            |arg: &Expression| written_number(arg) || matches!(arg.kind, ExpressionKind::None);
        let order: Vec<usize> = (0..args.len())
            .filter(|&at| !deferred(&args[at]))
            .chain((0..args.len()).filter(|&at| deferred(&args[at])))
            .collect();
        let mut checked: Vec<Option<Expr>> = (0..args.len()).map(|_| None).collect();
        let pending = self.pending_loans.len();
        let mut poisoned = false;
        let mut expected_bound = false;
        for at in order {
            if deferred(&args[at]) && !expected_bound {
                expected_bound = true;
                if let Some(expected) = expected {
                    self.bind(ret, expected, &mut subs);
                }
            }
            let hint = self.partial(params[at], &subs);
            let value = self.expr(&args[at], Some(hint));
            poisoned |= self.cx.types.is_error(value.ty);
            self.bind(params[at], value.ty, &mut subs);
            let target = self.partial(params[at], &subs);
            let mut value = if self.cx.types.contains_param(target) {
                value
            } else {
                let index = at + 1;
                self.coerce_or(value, target, args[at].span, |want, got| {
                    format!("Argument {index} type mismatch: expected {want}, got {got}")
                })
            };
            if !matches!(
                self.cx.types.kind(target),
                TyKind::Ref(_) | TyKind::RefMut(_)
            ) {
                self.consume(&mut value);
            }
            let loans = self.loans_of(&value);
            self.pending_loans.extend(loans);
            checked[at] = Some(value);
        }
        self.pending_loans.truncate(pending);

        let mut resolved = Vec::with_capacity(subs.len());
        for (param, sub) in generics.iter().zip(&subs) {
            match sub {
                Some(ty) if self.cx.types.is_error(*ty) => return self.error_expr(span),
                Some(ty) => resolved.push(*ty),
                None if poisoned => return self.error_expr(span),
                None => {
                    self.error(
                        format!(
                            "Cannot tell what '{}' is in this call to '{full}'",
                            param.name
                        ),
                        span,
                    );
                    return self.error_expr(span);
                }
            }
        }
        let own_from = generics.len() - own_generics;
        if !self
            .cx
            .check_bounds(def, &generics[own_from..], &resolved[own_from..], span)
        {
            return self.error_expr(span);
        }
        let ret = self.cx.types.subst(ret, &resolved);
        self.cx.note_types_in(ret, span);
        let id = self.cx.instance(def, resolved, span);
        let mut values: Vec<Expr> = receiver.into_iter().collect();
        values.extend(checked.into_iter().map(Option::unwrap));
        self.mk(ExprKind::Call(id, values), ret, span)
    }

    /// `ty` with the parameters known so far substituted.
    fn partial(&mut self, ty: Ty, subs: &[Option<Ty>]) -> Ty {
        let args: Vec<Ty> = subs
            .iter()
            .enumerate()
            .map(|(at, sub)| sub.unwrap_or_else(|| self.cx.types.intern(TyKind::Param(at as u32))))
            .collect();
        self.cx.types.subst(ty, &args)
    }

    /// Record what each type parameter in `param` stands for, read off the
    /// argument's type in the same position: `*T` given a `*i64` makes T i64.
    fn bind(&mut self, param: Ty, arg: Ty, subs: &mut [Option<Ty>]) {
        if self.cx.types.is_error(arg) {
            return;
        }
        let types = &self.cx.types;
        match (types.kind(param).clone(), types.kind(arg).clone()) {
            (TyKind::Param(at), _) => {
                if subs[at as usize].is_none() && !types.contains_param(arg) {
                    subs[at as usize] = Some(arg);
                }
            }
            (TyKind::Ptr(p), TyKind::Ptr(a) | TyKind::RefMut(a))
            | (TyKind::Ref(p), TyKind::Ref(a) | TyKind::RefMut(a))
            | (TyKind::RefMut(p), TyKind::RefMut(a))
            | (TyKind::Optional(p), TyKind::Optional(a))
            | (TyKind::Slice(p), TyKind::Slice(a))
            | (TyKind::Vec(p), TyKind::Vec(a))
            | (TyKind::Array(p, _), TyKind::Array(a, _)) => self.bind(p, a, subs),
            (TyKind::Result(p, pe), TyKind::Result(a, ae)) => {
                self.bind(p, a, subs);
                self.bind(pe, ae, subs);
            }
            (TyKind::Tuple(ps), TyKind::Tuple(args))
            | (TyKind::Adt(_, ps), TyKind::Adt(_, args))
                if ps.len() == args.len() =>
            {
                if let (TyKind::Adt(pd, _), TyKind::Adt(ad, _)) =
                    (types.kind(param), types.kind(arg))
                    && pd != ad
                {
                    return;
                }
                for (p, a) in ps.into_iter().zip(args) {
                    self.bind(p, a, subs);
                }
            }
            _ => {}
        }
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

    fn ok(&mut self, args: &[Expression], expected: Option<Ty>, span: Span) -> Expr {
        let [arg] = args else {
            self.error("Ok() takes exactly one argument", span);
            self.check_args_only(args);
            return self.error_expr(span);
        };
        let (hint, err) = match expected.map(|ty| self.cx.types.kind(ty).clone()) {
            Some(TyKind::Result(ok, err)) => (Some(ok), err),
            _ => (None, Types::I32),
        };
        let value = self.expr(arg, hint);
        let mut value = match hint {
            Some(hint) => match self.coerce(value, hint) {
                Ok(value) | Err(value) => value,
            },
            None => value,
        };
        self.consume(&mut value);
        let ty = self
            .cx
            .intern_or_error(TyKind::Result(value.ty, err), &[value.ty, err]);
        self.mk(ExprKind::Ok(Box::new(value)), ty, span)
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
        args: &[Expression],
        expected: Option<Ty>,
        span: Span,
    ) -> Expr {
        let (ty, elem) = match expected.map(|ty| (ty, self.cx.types.kind(ty).clone())) {
            Some((ty, TyKind::Vec(elem))) => (ty, elem),
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
                self.error(format!("Vec<T> has no method '{name}'"), span);
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
        if args.len() != arity {
            let plural = if arity == 1 { "" } else { "s" };
            self.error(
                format!("Vec::{name}() takes exactly {arity} argument{plural}"),
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
                    format!("Vec::{name}() expects {want}, got {got}")
                });
                self.consume(&mut value);
                values.push(value);
            } else {
                let value = self.expr(arg, Some(Types::USIZE));
                if value.ty != Types::USIZE && !self.cx.types.is_error(value.ty) {
                    self.error(format!("Vec::{name}() index or count must be usize"), span);
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
            "len" => Builtin::VecLen,
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
                self.error(format!("Vec<T> has no method '{name}'"), span);
                return self.error_expr(span);
            }
        };
        let root = match &receiver.kind {
            ExprKind::AddressOf(place) => super::body::local_root(place),
            _ => None,
        };
        let mut values = vec![receiver];
        values.extend(self.vec_args(name, elem, args, span));
        if let (Builtin::VecPush | Builtin::VecInsert, Some(item)) = (builtin, values.last()) {
            let loans = self.loans_of(item);
            match root {
                Some(root) => self.hold(root, loans, span),
                None => self.require_outliving(&loans, "store"),
            }
        }
        let ty = match builtin {
            Builtin::VecLen | Builtin::VecCapacity => Types::USIZE,
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
        let Some((format, values)) = args.split_first() else {
            self.error("print takes a format string first", span);
            return self.error_expr(span);
        };
        let ExpressionKind::StringLit(text) = &format.kind else {
            self.error(
                "The format must be a string literal, as in print(\"{}\", x)",
                format.span,
            );
            self.check_args_only(values);
            return self.error_expr(span);
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
        let kind = ExprKind::Print {
            stderr: name.starts_with('e'),
            newline: name.ends_with("ln"),
            pieces,
            values: checked,
        };
        self.mk(kind, Types::VOID, span)
    }
}

/// A number written out, or arithmetic on such numbers: it fits whichever
/// numeric type the rest of a call settles.
fn written_number(expr: &Expression) -> bool {
    match &expr.kind {
        ExpressionKind::Infix { left, right, .. } => written_number(left) && written_number(right),
        _ => is_number_literal(expr),
    }
}
