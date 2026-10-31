//! Closures: functions without a name, checked where they are written.
//! What a body uses from around it is captured, read through a `&` or,
//! when the body changes it, a `&var`; a closure is a struct of those
//! references, and the borrow checker sees it as one.

use crate::ast::{Closure, ClosureBody, Expression};
use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{self, Block, Expr, ExprKind, Place, Stmt, StmtKind};
use crate::ty::{Ty, TyKind, Types};

use super::Checker;
use super::body::{Fixed, FnCx};

impl FnCx<'_, '_> {
    /// A closure, given the parameter types and the return type `expected`
    /// where it goes, when it goes to a `fn(..)` parameter.
    pub(super) fn closure(
        &mut self,
        closure: &Closure,
        expected: Option<(Vec<Ty>, Ty)>,
        span: Span,
    ) -> Expr {
        if let Some((wanted, _)) = &expected
            && wanted.len() != closure.params.len()
        {
            let (count, wanted) = (closure.params.len(), wanted.len());
            let plural = |n: usize| if n == 1 { "" } else { "s" };
            self.error(
                format!(
                    "This closure takes {count} parameter{}, but it is given {wanted} value{}",
                    plural(count),
                    plural(wanted)
                ),
                span,
            );
            return self.error_expr(span);
        }
        let scope = self.scope.clone();
        let mut params = Vec::new();
        for (at, (name, written, _)) in closure.params.iter().enumerate() {
            let wanted = expected.as_ref().map(|(wanted, _)| wanted[at]);
            let wanted = wanted.map(|ty| self.resolve(ty));
            let ty = match (written, wanted) {
                (Some(spec), _) => self.cx.resolve_type(spec, &scope, span),
                (None, Some(ty)) if self.cx.types.is_concrete(ty) => ty,
                (None, _) => {
                    self.error(format!("Cannot tell the type of '{name}' here"), span)
                        .help(format!("write it, as in {name}: i32"));
                    Types::ERROR
                }
            };
            params.push(ty);
        }
        let written = closure
            .return_type
            .as_ref()
            .map(|spec| self.cx.resolve_return(Some(spec), &scope, span));
        let agrees = expected.as_ref().is_none_or(|(wanted, _)| {
            wanted
                .iter()
                .zip(&params)
                .all(|(&want, &param)| self.resolve(want) == param)
        });
        let wanted = expected
            .as_ref()
            .filter(|_| agrees)
            .map(|(_, ret)| *ret)
            .map(|ret| self.resolve(ret))
            .filter(|&ret| self.cx.types.is_concrete(ret));
        let known = match &closure.body {
            ClosureBody::Block(_) => written.or(wanted),
            ClosureBody::Expression(_) => wanted,
        };
        let ty = self
            .cx
            .types
            .new_closure(params.clone(), known.unwrap_or(Types::VOID));

        let outer = self.visible();
        let in_instance = self.in_instance;
        let mut inner = FnCx::new(self.cx, scope, known.unwrap_or(Types::VOID));
        inner.in_instance = in_instance;
        inner.in_closure = true;
        inner.own_name = self.naming.take();
        inner.outer = outer;
        inner.enter();
        let env = inner.cx.types.intern(TyKind::Ref(ty));
        let mut locals = vec![inner.declare("", env, false, false, span)];
        for ((name, _, mutable), &param) in closure.params.iter().zip(&params) {
            let view = inner.cx.owns_heap(param);
            if view && *mutable {
                let at = inner.cx.sources.word_in(span, name).unwrap_or(span);
                inner
                    .error(
                        format!("'{name}' cannot be var: a closure only borrows what it is given"),
                        at,
                    )
                    .help(format!(
                        "change a copy of it, as in var own = {name}.copy();"
                    ));
            }
            let local = inner.declare(name, param, *mutable, view, span);
            if !*mutable {
                inner.fix(local, if view { Fixed::Lent } else { Fixed::Param });
            }
            locals.push(local);
        }
        let body = match &closure.body {
            ClosureBody::Block(statements) => {
                inner.infer_ret = known.is_none();
                let mut block = inner.block(statements);
                let ret = inner.ret_ty();
                if inner.cx.types.is_unit_result(ret) && !inner.diverges {
                    let ok = inner.unit_ok(ret, span);
                    block.stmts.push(Stmt {
                        kind: StmtKind::Return(Some(ok)),
                        span,
                    });
                } else if ret != Types::VOID && !inner.diverges {
                    let shown = inner.cx.types.show(ret);
                    inner
                        .error("This closure can finish without returning a value", span)
                        .help(format!(
                            "return {} on every path",
                            crate::errors::with_article(&shown)
                        ));
                }
                block
            }
            ClosureBody::Expression(body) => inner.short_body(body, known),
        };
        let ret = inner.ret_ty();
        inner.warn_unread(locals.len());
        let captures = std::mem::take(&mut inner.captures);
        let body_locals = inner.take_locals();
        drop(inner);

        self.cx.types.set_closure_ret(ty, ret);
        if holds_closure(&self.cx.types, ret) || self.cx.types.closure_sig(ret).is_some() {
            self.error("A closure cannot return a closure", span)
                .help("a closure only borrows what it uses, so it never outlives where it is made");
        }
        if let ClosureBody::Expression(body) = &closure.body
            && !closure.argument
            && !self.cx.types.is_error(ret)
        {
            self.short_outside_call(body, ret);
        }
        let fields: Vec<Ty> = captures
            .iter()
            .map(|captured| match captured.mutable {
                true => self.cx.types.intern(TyKind::RefMut(captured.outer.ty)),
                false => self.cx.types.intern(TyKind::Ref(captured.outer.ty)),
            })
            .collect();
        let function = tir::Function {
            symbol: format!("closure{}", self.cx.closure_count()),
            params: locals,
            method: false,
            ret,
            locals: body_locals,
            body,
            span,
        };
        self.cx.add_closure(ty, function, fields, false);

        let head = self.cx.sources.first_line(span);
        let mut values = Vec::new();
        for (at, captured) in captures.into_iter().enumerate() {
            let name = &captured.outer.name;
            let (place, ty) = self
                .variable(name)
                .expect("a capture is visible where it was made");
            if let Place::Capture(outer) = place
                && captured.mutable
            {
                self.captures[outer as usize].mutable = true;
            }
            values.push((at as u32, self.address(place, ty, captured.mutable, head)));
        }
        self.mk(ExprKind::Struct(values), ty, span)
    }

    /// The body of a closure written as one expression: what it returns,
    /// of the type `known` when the closure goes where that is known.
    fn short_body(&mut self, body: &Expression, known: Option<Ty>) -> Block {
        let mut value = self.value(body, known);
        if let Some(ret) = known {
            value = self.coerce_or(value, ret, body.span, |want, got| {
                format!("This closure returns {want}, but this value is {got}")
            });
        }
        self.set_ret(value.ty);
        let kind = match value.ty == Types::VOID {
            true => StmtKind::Expr(value),
            false => {
                self.consume(&mut value);
                StmtKind::Return(Some(value))
            }
        };
        Block {
            stmts: vec![Stmt {
                kind,
                span: body.span,
            }],
        }
    }

    /// Report a closure whose body is one expression where only a block
    /// goes: anywhere but as a whole argument of a call.
    fn short_outside_call(&mut self, body: &Expression, ret: Ty) {
        let text = self.cx.sources.text(body.span).to_string();
        let block = match ret == Types::VOID {
            true => format!("{{ {text}; }}"),
            false => format!("{} {{ return {text}; }}", self.cx.types.show(ret)),
        };
        self.error(
            "A closure outside a call's arguments has its body in braces, as a function does",
            body.span,
        )
        .help("write its body as a block")
        .fix(body.span, block);
    }

    /// Whether `name` is a variable, its own or of the code around it,
    /// that holds a closure.
    pub(super) fn names_closure(&self, name: &str) -> bool {
        let ty = match self.local(name) {
            Some(local) => self.local_ty(local),
            None => match self.outer.iter().rev().find(|outer| outer.name == name) {
                Some(outer) => outer.ty,
                None => return false,
            },
        };
        self.cx.types.closure_sig(ty).is_some()
    }

    /// The call of the closure at `place`, of type `ty`, with `args`.
    pub(super) fn call_closure(
        &mut self,
        place: Place,
        ty: Ty,
        args: &[Expression],
        span: Span,
    ) -> Expr {
        let (params, ret) = self.cx.types.closure_sig(ty).expect("a closure");
        if args.len() != params.len() {
            let (wanted, given) = (params.len(), args.len());
            let plural = if wanted == 1 { "" } else { "s" };
            let given = match given {
                1 => "1 was given".to_string(),
                given => format!("{given} were given"),
            };
            self.error(
                format!("This closure takes {wanted} argument{plural}, but {given}"),
                span,
            );
            for arg in args {
                self.expr(arg, None);
            }
            return self.error_expr(span);
        }
        let (body, named) = self.cx.closure_body(ty);
        let mut values = Vec::new();
        if !named {
            values.push(self.address(place, ty, false, span));
        }
        for (arg, &param) in args.iter().zip(&params) {
            let value = self.value(arg, Some(param));
            let value = self.coerce_or(value, param, arg.span, |want, got| {
                format!("This closure takes {want} here, but this is {got}")
            });
            if !self.cx.owns_heap(param) {
                values.push(value);
                continue;
            }
            let place = match value.kind {
                ExprKind::Read(place) => place,
                _ => Place::Temp(Box::new(value)),
            };
            values.push(self.address(place, param, false, arg.span));
        }
        self.mk(ExprKind::Call(body, values), ret, span)
    }

    /// A function named where a value goes, as in `v.map(double)`: a
    /// closure that captures nothing and runs it.
    pub(super) fn function_value(&mut self, def: DefId, name: &str, span: Span) -> Expr {
        if let Some(&ty) = self.cx.named_closures.get(&def) {
            return self.mk(ExprKind::Struct(Vec::new()), ty, span);
        }
        let Some(sig) = self.cx.sig(def) else {
            return self.error_expr(span);
        };
        if !sig.generics.is_empty() || !sig.callables.is_empty() {
            self.error(format!("'{name}' is generic, so it is not a value"), span)
                .help(format!("call it inside a closure, as in fn(x) {name}(x)"));
            return self.error_expr(span);
        }
        if let Some(&owned) = sig.params.iter().find(|&&ty| self.cx.owns_heap(ty)) {
            let shown = self.cx.types.show(owned);
            self.error(
                format!(
                    "'{name}' takes a {shown} to keep, but a closure only borrows what it is given"
                ),
                span,
            )
            .help(format!(
                "call it inside a closure, as in fn(x) {name}(x.copy())"
            ));
            return self.error_expr(span);
        }
        let body = self.cx.instance(def, Vec::new(), span);
        let ty = self.cx.types.new_closure(sig.params, sig.ret);
        self.cx.named_closures.insert(def, ty);
        self.cx.closures.push(super::ClosureInfo {
            ty,
            body,
            fields: Vec::new(),
            named: true,
        });
        self.mk(ExprKind::Struct(Vec::new()), ty, span)
    }
}

impl Checker<'_> {
    fn closure_count(&self) -> usize {
        self.closures.len()
    }
}

/// Whether `ty` keeps a closure inside another value, as `Vec<fn(i32)>`
/// would, which no closure may outlive its references in.
pub(super) fn holds_closure(types: &Types, ty: Ty) -> bool {
    holds(types, ty, &|inner| types.closure_sig(inner).is_some())
}

/// Whether `ty` keeps inside another value a type that `is` accepts.
pub(super) fn holds(types: &Types, ty: Ty, is: &dyn Fn(Ty) -> bool) -> bool {
    let inside = |inner: &Ty| is(*inner) || holds(types, *inner, is);
    match types.kind(ty) {
        TyKind::Vec(inner) | TyKind::Array(inner, _) | TyKind::Optional(inner) => inside(inner),
        TyKind::Result(ok, err) => inside(ok) || inside(err),
        TyKind::Tuple(parts) | TyKind::Adt(_, parts) => parts.iter().any(inside),
        _ => false,
    }
}
