//! Inference of a generic call's type arguments: each unknown
//! one is an inference variable, bound by unifying the types it meets, and
//! once the outermost call is checked every variable is written back into
//! the typed IR or reported. The type arguments of `var x = T.new()` are
//! variables too, which live until the end of the function.

use crate::ast::{Expression, ExpressionKind};
use crate::errors::Span;
use crate::items::{DefId, ItemKind};
use crate::tir::{Block, Expr, ExprKind, FnId};
use crate::ty::{Ty, TyKind, Types};

use super::body::FnCx;

/// An inference variable: what it stands for once known, the type a number
/// written for it takes if nothing else decides, and the type parameter and
/// the call it was made for. A `local` one is a type argument of a variable
/// declared as `var x = T.new()`, which the rest of the function decides.
pub(super) struct Var {
    value: Option<Ty>,
    literal: Option<Ty>,
    param: String,
    call: String,
    span: Span,
    local: bool,
}

/// A call whose instance waits on variables an enclosing call binds. Until
/// they are written back it names `FnId(u32::MAX - k)`, `k` its position.
pub(super) struct DeferredCall {
    def: DefId,
    args: Vec<Ty>,
    span: Span,
}

impl FnCx<'_, '_> {
    pub(super) fn new_var(&mut self, param: &str, call: &str, span: Span) -> Ty {
        let var = self.vars.len() as u32;
        self.vars.push(Var {
            value: None,
            literal: None,
            param: param.to_string(),
            call: call.to_string(),
            span,
            local: false,
        });
        self.cx.types.intern(TyKind::Infer(var))
    }

    /// For `var x = T.new()` with no type written: `T` with a local variable
    /// for each of its type arguments, so the uses of `x` that follow decide
    /// them. `Vec` and generic structs only, through `new` or `with_capacity`.
    pub(super) fn open_type(&mut self, value: &Expression) -> Option<Ty> {
        let ExpressionKind::Call {
            function,
            type_args,
            ..
        } = &value.kind
        else {
            return None;
        };
        let ExpressionKind::Get { object, name } = &function.kind else {
            return None;
        };
        let ExpressionKind::Identifier(type_name) = &object.kind else {
            return None;
        };
        if !type_args.is_empty()
            || !matches!(name.as_str(), "new" | "with_capacity")
            || self.local(type_name).is_some()
        {
            return None;
        }
        let open = |this: &mut Self, param: &str| {
            let ty = this.new_var(param, type_name, value.span);
            this.vars.last_mut().expect("just made").local = true;
            ty
        };
        if type_name == "Vec" {
            let elem = open(self, "T");
            return Some(self.cx.types.intern(TyKind::Vec(elem)));
        }
        let def = self.cx.items.lookup_type(self.scope.module, type_name)?;
        let item = self.cx.items.item(def);
        let params: Vec<String> = super::generics_of(item.decl)
            .iter()
            .map(|param| param.name.clone())
            .collect();
        if item.kind != ItemKind::Struct || params.is_empty() {
            return None;
        }
        let args = params.iter().map(|param| open(self, param)).collect();
        Some(self.cx.types.intern(TyKind::Adt(def, args)))
    }

    /// `ty` with every variable bound so far replaced by what it stands for.
    pub(super) fn resolve(&mut self, mut ty: Ty) -> Ty {
        while self.cx.types.contains_var(ty) {
            let vars = &self.vars;
            let next = self.cx.types.replace(ty, &|kind| match kind {
                TyKind::Infer(var) => vars[*var as usize].value,
                _ => None,
            });
            if next == ty {
                break;
            }
            ty = next;
        }
        ty
    }

    /// Whether a value of type `actual` may go where `target` is wanted,
    /// binding the variables either holds to make it so: the same type, or,
    /// unless `exact`, pointers differing only in what they allow. What a
    /// `&var T` or a `*T` points at can be written, so its `T` is exact.
    pub(super) fn fits(&mut self, target: Ty, actual: Ty, exact: bool) -> bool {
        let (target, actual) = (self.resolve(target), self.resolve(actual));
        let types = &self.cx.types;
        if target == actual || types.is_error(target) || types.is_error(actual) {
            return true;
        }
        match (types.kind(target).clone(), types.kind(actual).clone()) {
            (TyKind::Infer(var), _) => self.bind(var, actual),
            (_, TyKind::Infer(var)) => self.bind(var, target),
            (TyKind::Ptr(t), TyKind::Ptr(a))
            | (TyKind::RefMut(t), TyKind::RefMut(a))
            | (TyKind::SliceMut(t), TyKind::SliceMut(a)) => self.fits(t, a, true),
            (TyKind::Ptr(t), TyKind::RefMut(a)) if !exact => self.fits(t, a, true),
            (TyKind::Ref(t), TyKind::Ref(a))
            | (TyKind::Optional(t), TyKind::Optional(a))
            | (TyKind::Vec(t), TyKind::Vec(a))
            | (TyKind::Slice(t), TyKind::Slice(a)) => self.fits(t, a, exact),
            (TyKind::Ref(t), TyKind::RefMut(a)) | (TyKind::Slice(t), TyKind::SliceMut(a))
                if !exact =>
            {
                self.fits(t, a, false)
            }
            (TyKind::Array(t, n), TyKind::Array(a, m)) => n == m && self.fits(t, a, exact),
            (TyKind::Result(t), TyKind::Result(a)) => self.fits(t, a, exact),
            (TyKind::Tuple(ts), TyKind::Tuple(parts)) => {
                ts.len() == parts.len()
                    && ts
                        .into_iter()
                        .zip(parts)
                        .all(|(t, a)| self.fits(t, a, exact))
            }
            (TyKind::Adt(d, ts), TyKind::Adt(e, args)) => {
                d == e && ts.into_iter().zip(args).all(|(t, a)| self.fits(t, a, true))
            }
            _ => false,
        }
    }

    /// The variable `expected` still is, if any, which a number written there
    /// leaves open: it becomes `default` (`i32`, or `f64` once a float was
    /// written) only if nothing else decides it.
    pub(super) fn literal_var(&mut self, expected: Option<Ty>, default: Ty) -> Option<Ty> {
        let ty = self.resolve(expected?);
        let TyKind::Infer(at) = *self.cx.types.kind(ty) else {
            return None;
        };
        if self.vars[at as usize].local {
            self.bind(at, default);
            return None;
        }
        let var = &mut self.vars[at as usize];
        if var.literal != Some(Types::F64) {
            var.literal = Some(default);
        }
        Some(ty)
    }

    /// The type the variable `ty` takes from a number written for it, bound
    /// now, for a closure's parameter that must be known before its body.
    pub(super) fn settle_literal(&mut self, ty: Ty) -> Option<Ty> {
        let TyKind::Infer(var) = *self.cx.types.kind(ty) else {
            return None;
        };
        let literal = self.vars[var as usize].literal?;
        self.bind(var, literal).then_some(literal)
    }

    fn bind(&mut self, var: u32, ty: Ty) -> bool {
        if self.cx.types.has(ty, &|kind| *kind == TyKind::Infer(var)) {
            return false;
        }
        self.vars[var as usize].value = Some(ty);
        true
    }

    /// A call to `def` at `args`, which may hold variables still unbound.
    pub(super) fn call_instance(&mut self, def: DefId, args: Vec<Ty>, span: Span) -> Option<FnId> {
        let args: Vec<Ty> = args.into_iter().map(|arg| self.resolve(arg)).collect();
        if args.iter().any(|&arg| self.cx.types.contains_var(arg)) {
            self.deferred.push(DeferredCall { def, args, span });
            return Some(FnId(u32::MAX - (self.deferred.len() as u32 - 1)));
        }
        if self.cx.recursive(def)
            && args.iter().any(|&arg| {
                self.cx
                    .types
                    .too_big(arg, super::MAX_TYPE_SIZE, super::MAX_TYPE_DEPTH)
            })
        {
            let name = self.cx.items.shown(def);
            self.error(
                format!("Checking '{name}' asks for ever larger instances of it, without end"),
                span,
            );
            return None;
        }
        let sig = self.cx.sig(def)?;
        let own_from = sig.generics.len() - sig.own_generics;
        if !self
            .cx
            .check_bounds(def, &sig.generics[own_from..], &args[own_from..], span)
        {
            return None;
        }
        let ret = self.cx.types.subst(sig.ret, &args);
        self.cx.note_types_in(ret, span);
        Some(self.cx.instance(def, args, span))
    }

    /// Check what `check` gives with variables of its own, unless `expected`
    /// holds an enclosing call's (a local variable is none): then those are
    /// resolved with the enclosing call's. The numbers written then decide
    /// what is left, floats first, so a float anywhere among unified
    /// variables makes them all `f64`.
    /// Each variable left unbound is reported.
    pub(super) fn inference_root(
        &mut self,
        expected: Option<Ty>,
        span: Span,
        check: impl FnOnce(&mut Self) -> Expr,
    ) -> Expr {
        let vars = &self.vars;
        let enclosing =
            |kind: &TyKind| matches!(kind, TyKind::Infer(v) if !vars[*v as usize].local);
        if expected.is_some_and(|ty| self.cx.types.has(ty, &enclosing)) {
            return check(self);
        }
        let (vars, errors) = (self.vars.len(), self.cx.errors.len());
        let mut expr = check(self);
        if self.vars.len() == vars {
            return expr;
        }
        let mut defaults: Vec<(usize, Ty)> = (vars..self.vars.len())
            .filter_map(|var| Some((var, self.vars[var].literal?)))
            .collect();
        defaults.sort_by_key(|&(_, default)| default != Types::F64);
        for (var, default) in defaults {
            let ty = self.cx.types.intern(TyKind::Infer(var as u32));
            self.fits(ty, default, true);
        }
        let open: Vec<usize> = (vars..self.vars.len())
            .filter(|&var| {
                let ty = self.cx.types.intern(TyKind::Infer(var as u32));
                let ty = self.resolve(ty);
                let own = |kind: &TyKind| matches!(kind, TyKind::Infer(v) if *v as usize >= vars);
                self.cx.types.has(ty, &own)
            })
            .collect();
        let resolved = open.is_empty() && self.write_back(&mut expr);
        if self.cx.errors.len() == errors {
            for var in open {
                let Var {
                    param, call, span, ..
                } = &self.vars[var];
                let message = format!("Cannot tell what '{param}' is in this call to '{call}'");
                self.cx.error(message, *span);
            }
        }
        if resolved {
            expr
        } else {
            self.error_expr(span)
        }
    }

    /// Put what each variable stands for into `expr`, and make the instance
    /// of each call that waited on one; false if one breaks its bounds.
    fn write_back(&mut self, expr: &mut Expr) -> bool {
        if self.cx.types.contains_var(expr.ty) {
            let ty = self.resolve(expr.ty);
            match expr.kind {
                ExprKind::Int(value)
                    if self.cx.types.as_int(ty).is_some() || self.cx.types.is_float(ty) =>
                {
                    let magnitude = value.unsigned_abs() as u64;
                    *expr = self.int_literal(magnitude, value < 0, Some(ty), expr.span);
                }
                ExprKind::Float(value) if self.cx.types.is_float(ty) => {
                    *expr = self.float_literal(value, Some(ty), expr.span);
                }
                ExprKind::Int(_) | ExprKind::Float(_) => {
                    let (written, shown) =
                        (self.cx.sources.text(expr.span), self.cx.types.show(ty));
                    self.error(format!("Literal {written} is not {shown}"), expr.span);
                    expr.ty = Types::ERROR;
                    return false;
                }
                _ => expr.ty = ty,
            }
            self.cx.note_types_in(expr.ty, expr.span);
        }
        let mut resolved = true;
        if let ExprKind::Call(id, _) = &mut expr.kind
            && let Some(call) = self.deferred.get((u32::MAX - id.0) as usize)
        {
            let (def, args, span) = (call.def, call.args.clone(), call.span);
            match self.call_instance(def, args, span) {
                Some(instance) => *id = instance,
                None => resolved = false,
            }
        }
        if let ExprKind::Block(block) = &mut expr.kind {
            block.for_each_expr(&mut |child| resolved &= self.write_back(child));
        }
        expr.for_each_child(&mut |child| resolved &= self.write_back(child));
        resolved
    }

    /// `expr` with its type as far as known, unless that is still a local
    /// variable: a value taken out of a `var x = T.new()` before anything
    /// said what `x` holds, which is then reported, and the variable taken
    /// as an error from there on.
    pub(super) fn known(&mut self, mut expr: Expr) -> Expr {
        expr.ty = self.resolve(expr.ty);
        if !matches!(self.cx.types.kind(expr.ty), TyKind::Infer(_))
            || !self.open(expr.ty, expr.span)
        {
            return expr;
        }
        self.error_expr(expr.span)
    }

    /// Whether `ty` still holds a local variable, which is then reported at
    /// `span` and taken as an error from there on.
    pub(super) fn open(&mut self, ty: Ty, span: Span) -> bool {
        let ty = self.resolve(ty);
        let Some(var) = (0..self.vars.len() as u32).find(|&var| {
            self.vars[var as usize].local
                && self.cx.types.has(ty, &|kind| *kind == TyKind::Infer(var))
        }) else {
            return false;
        };
        let call = self.vars[var as usize].call.clone();
        self.bind(var, Types::ERROR);
        self.error(
            format!("Cannot tell what this is yet: nothing before it says what the {call} holds"),
            span,
        )
        .help(name_it(&call, "add to it before this"));
        true
    }

    /// At the end of a body, the types its `var x = T.new()` variables were
    /// left with: each local variable no use decided is reported, unless an
    /// earlier error may be why; with no error, all are written into the
    /// body and its locals.
    pub(super) fn settle_locals(&mut self, block: &mut Block) {
        let quiet = self.cx.errors.len() > self.errors_before;
        let mut reported = Vec::new();
        for var in 0..self.vars.len() {
            let ty = self.cx.types.intern(TyKind::Infer(var as u32));
            let ty = self.resolve(ty);
            let Var {
                call, span, local, ..
            } = &self.vars[var];
            if !*local || !self.cx.types.contains_var(ty) || reported.contains(span) {
                continue;
            }
            let (call, span) = (call.clone(), *span);
            reported.push(span);
            if !quiet {
                self.error(format!("Cannot tell what this {call} holds"), span)
                    .help(name_it(&call, "add to it later in this function"));
            }
        }
        if quiet || !reported.is_empty() {
            return;
        }
        block.for_each_expr(&mut |expr| {
            self.write_back(expr);
        });
        for at in 0..self.locals.len() {
            let ty = self.resolve(self.locals[at].ty);
            self.locals[at].ty = ty;
            self.cx.note_types_in(ty, self.declared[at]);
        }
    }
}

/// The help for a `T.new()` whose type arguments are still unknown: `first`,
/// or name them.
fn name_it(call: &str, first: &str) -> String {
    let example = if call == "Vec" { "i32" } else { ".." };
    format!("{first}, or name what it holds, as in {call}.new<{example}>()")
}
