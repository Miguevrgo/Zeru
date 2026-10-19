//! Inference of a generic call's type arguments, as rustc's: each unknown
//! one is an inference variable, bound by unifying the types it meets, and
//! once the outermost call is checked every variable is written back into
//! the typed IR or reported.

use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{Expr, ExprKind, FnId};
use crate::ty::{Ty, TyKind, Types};

use super::body::FnCx;

/// An inference variable: what it stands for once known, the type a number
/// written for it takes if nothing else decides, and the type parameter and
/// the call it was made for.
pub(super) struct Var {
    value: Option<Ty>,
    literal: Option<Ty>,
    param: String,
    call: String,
    span: Span,
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
        });
        self.cx.types.intern(TyKind::Infer(var))
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
    /// `&var T` or a `*T` points at can be written, so its `T` is exact, as
    /// rustc's `&mut T` is invariant in `T`.
    pub(super) fn fits(&mut self, target: Ty, actual: Ty, exact: bool) -> bool {
        let (target, actual) = (self.resolve(target), self.resolve(actual));
        let types = &self.cx.types;
        if target == actual || types.is_error(target) || types.is_error(actual) {
            return true;
        }
        match (types.kind(target).clone(), types.kind(actual).clone()) {
            (TyKind::Infer(var), _) => self.bind(var, actual),
            (_, TyKind::Infer(var)) => self.bind(var, target),
            (TyKind::Ptr(t), TyKind::Ptr(a)) | (TyKind::RefMut(t), TyKind::RefMut(a)) => {
                self.fits(t, a, true)
            }
            (TyKind::Ptr(t), TyKind::RefMut(a)) if !exact => self.fits(t, a, true),
            (TyKind::Ref(t), TyKind::Ref(a))
            | (TyKind::Optional(t), TyKind::Optional(a))
            | (TyKind::Vec(t), TyKind::Vec(a))
            | (TyKind::Slice(t), TyKind::Slice(a)) => self.fits(t, a, exact),
            (TyKind::Ref(t), TyKind::RefMut(a)) if !exact => self.fits(t, a, false),
            (TyKind::Array(t, n), TyKind::Array(a, m)) => n == m && self.fits(t, a, exact),
            (TyKind::Result(t, te), TyKind::Result(a, ae)) => {
                self.fits(t, a, exact) && self.fits(te, ae, exact)
            }
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
    /// written) only if nothing else decides it, as in rustc.
    pub(super) fn literal_var(&mut self, expected: Option<Ty>, default: Ty) -> Option<Ty> {
        let ty = self.resolve(expected?);
        let TyKind::Infer(var) = *self.cx.types.kind(ty) else {
            return None;
        };
        let var = &mut self.vars[var as usize];
        if var.literal != Some(Types::F64) {
            var.literal = Some(default);
        }
        Some(ty)
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
        if args.iter().any(|&arg| {
            self.cx
                .types
                .too_big(arg, super::MAX_TYPE_SIZE, super::MAX_TYPE_DEPTH)
        }) {
            let name = self.cx.items.item(def).name.clone();
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
    /// holds an enclosing call's: then those are resolved with the enclosing
    /// call's. Each variable left unbound is reported.
    pub(super) fn inference_root(
        &mut self,
        expected: Option<Ty>,
        span: Span,
        check: impl FnOnce(&mut Self) -> Expr,
    ) -> Expr {
        if expected.is_some_and(|ty| self.cx.types.contains_var(ty)) {
            return check(self);
        }
        let (vars, deferred, errors) = (self.vars.len(), self.deferred.len(), self.cx.errors.len());
        let mut expr = check(self);
        if self.vars.len() == vars {
            return expr;
        }
        for var in vars..self.vars.len() {
            if let Some(default) = self.vars[var].literal {
                let ty = self.cx.types.intern(TyKind::Infer(var as u32));
                self.fits(ty, default, true);
            }
        }
        let open: Vec<usize> = (vars..self.vars.len())
            .filter(|&var| {
                let ty = self.cx.types.intern(TyKind::Infer(var as u32));
                let ty = self.resolve(ty);
                self.cx.types.contains_var(ty)
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
        self.vars.truncate(vars);
        self.deferred.truncate(deferred);
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
                ExprKind::Int(value) => {
                    let magnitude = value.unsigned_abs() as u64;
                    *expr = self.int_literal(magnitude, value < 0, Some(ty), expr.span);
                }
                ExprKind::Float(value) if self.cx.types.is_float(ty) => {
                    *expr = self.float_literal(value, Some(ty), expr.span);
                }
                ExprKind::Float(value) => {
                    let shown = self.cx.types.show(ty);
                    self.error(format!("Literal {value} is not {shown}"), expr.span);
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
        expr.for_each_child(&mut |child| resolved &= self.write_back(child));
        resolved
    }
}
