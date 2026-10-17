//! Checking one function instance: its locals and scopes, which of them are
//! moved along each path, and its statements.

use std::collections::HashSet;

use crate::ast::{Expression, ExpressionKind, Statement, StatementKind};
use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{self, Block, Expr, ExprKind, LocalId, Place, Stmt, StmtKind};
use crate::ty::{Ty, TyKind, Types};

use super::{Checker, Receiver, Scope};

pub(super) struct LocalInfo {
    mutable: bool,
    borrowed: bool,
}

/// Which locals have given their value away, and where.
pub(super) type Moves = Vec<Option<Span>>;

struct LoopFrame {
    first_local: usize,
    before: Moves,
    exits: Vec<Moves>,
}

/// What an enclosing `for .. in &var` or `match` with bindings refers into,
/// which must not change meanwhile: a local and the fields and elements
/// below it.
#[derive(Clone)]
pub(super) struct PlacePath {
    root: LocalId,
    steps: Vec<String>,
}

pub(super) struct FnCx<'c, 'a> {
    pub(super) cx: &'c mut Checker<'a>,
    pub(super) scope: Scope,
    ret: Ty,
    /// A generic instance, whose body is checked once per set of types.
    pub(super) in_instance: bool,
    locals: Vec<tir::Local>,
    info: Vec<LocalInfo>,
    names: Vec<(String, LocalId)>,
    marks: Vec<usize>,
    moved: Moves,
    loops: Vec<LoopFrame>,
    locked: Vec<PlacePath>,
    reported: HashSet<Span>,
}

pub(super) fn check_function(cx: &mut Checker, def: DefId, args: &[Ty]) -> tir::Function {
    let item = cx.items.item(def);
    let StatementKind::Function { params, body, .. } = &item.decl.kind else {
        unreachable!("an instance is a function")
    };
    let span = item.decl.span;
    let name = item.name.clone();
    let symbol = match args {
        [] => name.clone(),
        _ => format!(
            "{name}<{}>",
            args.iter()
                .map(|&a| cx.types.show(a))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    };
    let sig = cx.sig(def).expect("a function has a signature");
    let param_tys = cx.types.subst_all(&sig.params, args);
    let ret = cx.types.subst(sig.ret, args);
    for &ty in param_tys.iter().chain([&ret]) {
        cx.note_types_in(ty, span);
    }
    let errors_before = cx.errors.len();

    let scope = cx.scope_of(def, args);
    let mut fx = FnCx::new(cx, scope, ret);
    fx.in_instance = !args.is_empty();
    fx.enter();
    let params: Vec<LocalId> = params
        .iter()
        .zip(&param_tys)
        .map(|((param, _, mutable), &ty)| {
            let receiver = param == "self" && sig.receiver != Receiver::None;
            fx.declare(param, ty, *mutable, receiver, receiver)
        })
        .collect();
    let body_block = fx.block(body);
    let is_main = fx.cx.is_main(def);
    if ret != Types::VOID && !is_main && !always_leaves(body, false) {
        let at = body.last().map_or(span, |stmt| stmt.span);
        fx.cx.error(
            format!("Function '{name}' can finish without returning a value"),
            at,
        );
    }
    let locals = std::mem::take(&mut fx.locals);

    if !args.is_empty() {
        let id = fx.cx.instances[&(def, args.to_vec())];
        let origin = fx.cx.instance_origin(id);
        for error in &mut fx.cx.errors[errors_before..] {
            error
                .notes
                .push((format!("in '{symbol}', instantiated here"), origin));
        }
    }
    tir::Function {
        symbol,
        params,
        ret,
        locals,
        body: body_block,
        span,
        exported: is_main,
    }
}

impl<'c, 'a> FnCx<'c, 'a> {
    fn new(cx: &'c mut Checker<'a>, scope: Scope, ret: Ty) -> Self {
        Self {
            cx,
            scope,
            ret,
            in_instance: false,
            locals: Vec::new(),
            info: Vec::new(),
            names: Vec::new(),
            marks: Vec::new(),
            moved: Vec::new(),
            loops: Vec::new(),
            locked: Vec::new(),
            reported: HashSet::new(),
        }
    }

    /// The context a global constant's value is checked in: no locals, and
    /// nothing to return.
    pub(super) fn for_constant(cx: &'c mut Checker<'a>, scope: Scope) -> Self {
        Self::new(cx, scope, Types::VOID)
    }

    pub(super) fn ret_ty(&self) -> Ty {
        self.ret
    }

    pub(super) fn local_mutable(&self, id: LocalId) -> bool {
        self.info[id.0 as usize].mutable
    }

    pub(super) fn ty(&self, ty: Ty) -> String {
        self.cx.types.show(ty)
    }

    pub(super) fn error(&mut self, message: impl Into<String>, span: Span) {
        self.cx.error(message, span);
    }

    pub(super) fn enter(&mut self) {
        self.marks.push(self.names.len());
    }

    pub(super) fn leave(&mut self) {
        let mark = self.marks.pop().expect("scopes are balanced");
        self.names.truncate(mark);
    }

    /// A new local, which later statements see by `name`. A `view` is a
    /// pointer into another place and owns nothing.
    pub(super) fn declare(
        &mut self,
        name: &str,
        ty: Ty,
        mutable: bool,
        borrowed: bool,
        view: bool,
    ) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(tir::Local {
            name: name.to_string(),
            ty,
            view,
        });
        self.info.push(LocalInfo { mutable, borrowed });
        self.moved.push(None);
        self.names.push((name.to_string(), id));
        id
    }

    /// Like `declare`, for a copy of an element the local does not own.
    pub(super) fn declare_borrowed_copy(&mut self, name: &str, ty: Ty, mutable: bool) -> LocalId {
        self.declare(name, ty, mutable, true, false)
    }

    pub(super) fn local(&self, name: &str) -> Option<LocalId> {
        self.names
            .iter()
            .rev()
            .find(|(local, _)| local == name)
            .map(|(_, id)| *id)
    }

    pub(super) fn local_ty(&self, id: LocalId) -> Ty {
        self.locals[id.0 as usize].ty
    }

    pub(super) fn local_name(&self, id: LocalId) -> &str {
        &self.locals[id.0 as usize].name
    }

    pub(super) fn local_names(&self) -> Vec<String> {
        self.names.iter().map(|(name, _)| name.clone()).collect()
    }

    pub(super) fn moved_at(&self, id: LocalId) -> Option<Span> {
        self.moved[id.0 as usize]
    }

    pub(super) fn set_moved(&mut self, id: LocalId, at: Option<Span>) {
        self.moved[id.0 as usize] = at;
    }

    pub(super) fn snapshot(&self) -> Moves {
        self.moved.clone()
    }

    pub(super) fn restore(&mut self, moves: &Moves) {
        for (at, slot) in self.moved.iter_mut().enumerate() {
            *slot = moves.get(at).copied().flatten();
        }
    }

    pub(super) fn merge(into: &mut Moves, from: &Moves) {
        if into.len() < from.len() {
            into.resize(from.len(), None);
        }
        for (slot, moved) in into.iter_mut().zip(from) {
            if slot.is_none() {
                *slot = *moved;
            }
        }
    }

    /// `expr` is given away by value: to a variable, a call, a field, a
    /// collection or the caller. What `Some(..)` wraps is given away with it.
    /// A temporary gives its value up and is then not dropped. A local is
    /// moved and may not be used again;
    /// a value inside a field, an element or behind a pointer cannot be moved
    /// out at all, since what holds it would keep a second owner of it.
    pub(super) fn consume(&mut self, expr: &mut Expr) {
        if !self.cx.owns_heap(expr.ty) {
            return;
        }
        let local = match &mut expr.kind {
            ExprKind::Some(value) | ExprKind::Ok(value) => return self.consume(value),
            ExprKind::Read(Place::Temp(value)) => {
                let value = std::mem::replace(
                    value.as_mut(),
                    Expr {
                        kind: ExprKind::Void,
                        ty: Types::ERROR,
                        span: expr.span,
                    },
                );
                *expr = value;
                return;
            }
            ExprKind::Read(Place::Local(local)) => *local,
            ExprKind::Read(Place::Field(..) | Place::Index(..) | Place::Deref(_)) => {
                self.error(
                    "Cannot move a value out of a field, an element or a pointer; call .copy() on it",
                    expr.span,
                );
                return;
            }
            _ => return,
        };
        if self.info[local.0 as usize].borrowed {
            let name = self.local_name(local).to_string();
            self.error(
                format!("Cannot move '{name}', which is borrowed; call .copy() on it"),
                expr.span,
            );
        }
        self.set_moved(local, Some(expr.span));
        let ExprKind::Read(place) = std::mem::replace(&mut expr.kind, ExprKind::Void) else {
            unreachable!()
        };
        expr.kind = ExprKind::Move(place);
    }

    /// Check a loop, `turn` saying whether one turn always leaves. A local
    /// from outside must hold a value again wherever the loop goes round, or
    /// the next turn would use it moved.
    pub(super) fn check_loop<T>(&mut self, turn: impl FnOnce(&mut Self) -> (T, bool)) -> T {
        let before = self.snapshot();
        self.loops.push(LoopFrame {
            first_local: self.locals.len(),
            before: before.clone(),
            exits: Vec::new(),
        });
        let (result, leaves) = turn(self);
        if !leaves {
            self.check_back_edge();
        }
        let frame = self.loops.pop().expect("pushed above");
        let mut after = before;
        if !leaves {
            Self::merge(&mut after, &self.snapshot());
        }
        for exit in &frame.exits {
            Self::merge(&mut after, exit);
        }
        self.restore(&after);
        result
    }

    fn check_back_edge(&mut self) {
        let Some(frame) = self.loops.last() else {
            return;
        };
        let repeated: Vec<(LocalId, Span)> = (0..frame.first_local)
            .filter_map(|at| {
                let now = self.moved[at]?;
                frame
                    .before
                    .get(at)
                    .copied()
                    .flatten()
                    .is_none()
                    .then_some((LocalId(at as u32), now))
            })
            .collect();
        for (local, span) in repeated {
            if self.reported.insert(span) {
                let name = self.local_name(local).to_string();
                self.error(
                    format!("Cannot move '{name}' inside a loop: the next turn would use it again"),
                    span,
                );
            }
        }
    }

    /// Whether `value` of type `actual` can go where `target` is wanted, and
    /// the expression that puts it there: unchanged, retyped between pointer
    /// kinds, or wrapped in `Some` or `Ok`.
    pub(super) fn coerce(&mut self, mut value: Expr, target: Ty) -> Result<Expr, Expr> {
        let types = &self.cx.types;
        if value.ty == target || types.is_error(value.ty) || types.is_error(target) {
            return Ok(value);
        }
        if self.same_repr(target, value.ty) {
            value.ty = target;
            return Ok(value);
        }
        let (inner, wrap): (Ty, fn(Box<Expr>) -> ExprKind) = match *types.kind(target) {
            TyKind::Optional(inner) => (inner, ExprKind::Some),
            TyKind::Result(inner, _) => (inner, ExprKind::Ok),
            _ => return Err(value),
        };
        if !self.same_repr(inner, value.ty) {
            return Err(value);
        }
        value.ty = inner;
        let span = value.span;
        Ok(Expr {
            kind: wrap(Box::new(value)),
            ty: target,
            span,
        })
    }

    /// Whether a value of `actual` is laid out as a `target` and may be used
    /// as one: the same type, or pointers differing only in what they allow.
    fn same_repr(&self, target: Ty, actual: Ty) -> bool {
        let types = &self.cx.types;
        if target == actual || types.is_error(target) || types.is_error(actual) {
            return true;
        }
        match (types.kind(target), types.kind(actual)) {
            (TyKind::Ptr(t), TyKind::Ptr(a) | TyKind::RefMut(a))
            | (TyKind::Ref(t), TyKind::Ref(a) | TyKind::RefMut(a))
            | (TyKind::RefMut(t), TyKind::RefMut(a))
            | (TyKind::Optional(t), TyKind::Optional(a))
            | (TyKind::Vec(t), TyKind::Vec(a))
            | (TyKind::Slice(t), TyKind::Slice(a)) => self.same_repr(*t, *a),
            (TyKind::Array(t, n), TyKind::Array(a, m)) => n == m && self.same_repr(*t, *a),
            (TyKind::Result(t, te), TyKind::Result(a, ae)) => {
                self.same_repr(*t, *a) && self.same_repr(*te, *ae)
            }
            (TyKind::Tuple(t), TyKind::Tuple(a)) => {
                t.len() == a.len() && t.iter().zip(a).all(|(t, a)| self.same_repr(*t, *a))
            }
            _ => false,
        }
    }

    /// `coerce`, reporting `message(target, actual)` when it cannot.
    pub(super) fn coerce_or(
        &mut self,
        value: Expr,
        target: Ty,
        span: Span,
        message: impl FnOnce(&str, &str) -> String,
    ) -> Expr {
        match self.coerce(value, target) {
            Ok(value) => value,
            Err(value) => {
                let (target, actual) = (self.ty(target), self.ty(value.ty));
                self.error(message(&target, &actual), span);
                value
            }
        }
    }

    /// The path of a place made of locals, fields and elements, for telling
    /// whether two places overlap.
    pub(super) fn place_path(&self, place: &Expression) -> Option<PlacePath> {
        match &place.kind {
            ExpressionKind::Identifier(name) => Some(PlacePath {
                root: self.local(name)?,
                steps: Vec::new(),
            }),
            ExpressionKind::Get { object, name } => {
                let mut path = self.place_path(object)?;
                path.steps.push(format!(".{name}"));
                Some(path)
            }
            ExpressionKind::Index { left, .. } => {
                let mut path = self.place_path(left)?;
                path.steps.push("[]".into());
                Some(path)
            }
            _ => None,
        }
    }

    fn show_path(&self, path: &PlacePath) -> String {
        format!("{}{}", self.local_name(path.root), path.steps.concat())
    }

    pub(super) fn lock(&mut self, path: PlacePath) {
        self.locked.push(path);
    }

    pub(super) fn unlock(&mut self) {
        self.locked.pop();
    }

    /// Report a change to what an enclosing `for .. in &var` or `match`
    /// refers into, or to what holds it.
    pub(super) fn require_unlocked(&mut self, place: &Expression, span: Span) {
        let Some(path) = self.place_path(place) else {
            return;
        };
        let within = |outer: &PlacePath, inner: &PlacePath| {
            outer.root == inner.root && inner.steps.starts_with(&outer.steps)
        };
        let Some(locked) = self
            .locked
            .iter()
            .find(|locked| within(locked, &path) || within(&path, locked))
            .cloned()
        else {
            return;
        };
        let (path, locked) = (self.show_path(&path), self.show_path(&locked));
        self.error(
            format!(
                "Cannot change '{path}' while a loop or match around it refers into '{locked}'"
            ),
            span,
        );
    }

    /// Report a write into `place` when the local it belongs to is not
    /// declared `var`. Behind a pointer the storage is the pointee's, which a
    /// `*T` or `&var T` may write and a `&T` may not.
    pub(super) fn require_mutable(&mut self, place: &Place, ast: &Expression, span: Span) {
        self.require_unlocked(ast, span);
        self.require_var_root(place, span);
    }

    fn require_var_root(&mut self, place: &Place, span: Span) {
        match place {
            Place::Local(local) => {
                if !self.info[local.0 as usize].mutable {
                    let name = self.local_name(*local).to_string();
                    self.error(
                        format!("Cannot modify '{name}', which is not declared 'var'"),
                        span,
                    );
                }
            }
            Place::Field(base, _) | Place::Index(base, _) => self.require_var_root(base, span),
            Place::Deref(pointer) => {
                if matches!(self.cx.types.kind(pointer.ty), TyKind::Ref(_)) {
                    self.error("Cannot write through a '&' reference", span);
                }
            }
            Place::Temp(_) => {}
        }
    }

    pub(super) fn block(&mut self, statements: &[Statement]) -> Block {
        self.enter();
        let stmts = statements.iter().filter_map(|s| self.stmt(s)).collect();
        self.leave();
        Block { stmts }
    }

    fn branch(&mut self, statement: &Statement) -> Block {
        match &statement.kind {
            StatementKind::Block(statements) => self.block(statements),
            _ => {
                self.enter();
                let stmts = self.stmt(statement).into_iter().collect();
                self.leave();
                Block { stmts }
            }
        }
    }

    fn stmt(&mut self, statement: &Statement) -> Option<Stmt> {
        let span = statement.span;
        let kind = match &statement.kind {
            StatementKind::Var {
                name,
                is_const,
                value,
                type_annotation,
                ..
            } => {
                let expected = type_annotation.as_ref().map(|spec| {
                    let scope = self.scope.clone();
                    self.cx.resolve_type(spec, &scope, span)
                });
                let errors = self.cx.errors.len();
                let checked = self.expr(value, expected);
                let ty = match expected {
                    Some(ty) => ty,
                    None if matches!(value.kind, ExpressionKind::None)
                        && self.cx.errors.len() == errors =>
                    {
                        self.error(
                            format!(
                                "Cannot infer type for variable '{name}'. Please add a type annotation"
                            ),
                            span,
                        );
                        Types::ERROR
                    }
                    None => checked.ty,
                };
                let mut value = self.coerce_or(checked, ty, span, |want, got| {
                    format!(
                        "Type mismatch for variable '{name}'. Annotated as {want} but got {got}"
                    )
                });
                self.consume(&mut value);
                self.cx.note_types_in(ty, span);
                let local = self.declare(name, ty, !is_const, false, false);
                StmtKind::Let(local, value)
            }
            StatementKind::Return(value) => {
                let ret = self.ret;
                let value = value.as_ref().map(|value| {
                    let checked = self.expr(value, Some(ret));
                    let mut value = self.coerce_or(checked, ret, span, |want, got| {
                        format!("Invalid return type. Function expects {want}, returning {got}")
                    });
                    self.consume(&mut value);
                    value
                });
                if value.is_none() && ret != Types::VOID {
                    let want = self.ty(ret);
                    self.error(
                        format!("Invalid return type. Function expects {want}, returning void"),
                        span,
                    );
                }
                StmtKind::Return(value)
            }
            StatementKind::Block(statements) => StmtKind::Block(self.block(statements)),
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.condition(condition, "If condition must be boolean, got ");
                let before = self.snapshot();
                let mut after = Moves::new();
                let then = self.branch(then_branch);
                if !always_leaves(std::slice::from_ref(then_branch), true) {
                    Self::merge(&mut after, &self.snapshot());
                }
                self.restore(&before);
                let otherwise = else_branch.as_ref().map(|branch| {
                    let block = self.branch(branch);
                    if !always_leaves(std::slice::from_ref(branch), true) {
                        Self::merge(&mut after, &self.snapshot());
                    }
                    block
                });
                if otherwise.is_none() {
                    Self::merge(&mut after, &before);
                }
                self.restore(&after);
                StmtKind::If {
                    cond,
                    then,
                    otherwise,
                }
            }
            StatementKind::While { cond, body } => self.check_loop(|this| {
                let cond = this.condition(cond, "While condition must be boolean, got ");
                let body_block = this.branch(body);
                let leaves = always_leaves(std::slice::from_ref(body), true);
                (
                    StmtKind::While {
                        cond,
                        body: body_block,
                    },
                    leaves,
                )
            }),
            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => self.for_in(variable, iterable, body),
            StatementKind::Break | StatementKind::Continue if self.loops.is_empty() => {
                self.error("Break/Continue can only be used inside loops", span);
                return None;
            }
            StatementKind::Break | StatementKind::Continue => {
                let is_break = matches!(statement.kind, StatementKind::Break);
                if !is_break {
                    self.check_back_edge();
                }
                let moves = self.snapshot();
                self.loops.last_mut().unwrap().exits.push(moves);
                if is_break {
                    StmtKind::Break
                } else {
                    StmtKind::Continue
                }
            }
            StatementKind::Expression(expr) => StmtKind::Expr(self.expr(expr, None)),
            StatementKind::Function { .. }
            | StatementKind::Struct { .. }
            | StatementKind::Enum { .. }
            | StatementKind::Trait { .. }
            | StatementKind::Import { .. } => {
                self.error(
                    "A declaration inside a function body is not supported",
                    span,
                );
                return None;
            }
        };
        Some(Stmt { kind, span })
    }

    fn condition(&mut self, condition: &Expression, message: &str) -> Expr {
        let cond = self.expr(condition, Some(Types::BOOL));
        if cond.ty != Types::BOOL && !self.cx.types.is_error(cond.ty) {
            let shown = self.ty(cond.ty);
            self.error(format!("{message}{shown}"), condition.span);
        }
        cond
    }

    /// `for i in a..b` counts; `for x in v` reads each element of an array or
    /// a Vec; `for x in &var v` lets each be written through `x`, so `v` must
    /// not change under it meanwhile.
    fn for_in(&mut self, variable: &str, iterable: &Expression, body: &Statement) -> StmtKind {
        if let ExpressionKind::Range { start, end } = &iterable.kind {
            let (start, end) = self.operands(start, end, None);
            let ty = match (start.ty, end.ty) {
                (a, b) if self.cx.types.is_error(a) || self.cx.types.is_error(b) => Types::ERROR,
                (a, b) if a == b && self.cx.types.as_int(a).is_some() => a,
                (a, b) => {
                    let (a, b) = (self.ty(a), self.ty(b));
                    self.error(
                        format!("A range counts between two integers of one type, not {a} and {b}"),
                        iterable.span,
                    );
                    Types::ERROR
                }
            };
            return self.check_loop(|this| {
                this.enter();
                let var = this.declare_borrowed_copy(variable, ty, false);
                let body_block = this.branch(body);
                this.leave();
                let leaves = always_leaves(std::slice::from_ref(body), true);
                (
                    StmtKind::ForRange {
                        var,
                        start,
                        end,
                        body: body_block,
                    },
                    leaves,
                )
            });
        }

        let (inner, writable) = match &iterable.kind {
            ExpressionKind::BorrowRefMut(inner) => (inner.as_ref(), true),
            ExpressionKind::BorrowRef(inner) => (inner.as_ref(), false),
            _ => (iterable, false),
        };
        let mut seq = self.place(inner);
        if let TyKind::Ref(pointee) | TyKind::RefMut(pointee) = *self.cx.types.kind(seq.1) {
            let reference = self.mk(ExprKind::Read(seq.0), seq.1, inner.span);
            seq = (Place::Deref(Box::new(reference)), pointee);
        }
        if writable {
            self.require_mutable(&seq.0, inner, iterable.span);
            if !super::expr::is_assignable(inner) {
                self.error(
                    "Cannot create reference to a temporary value",
                    iterable.span,
                );
            }
        }
        let seq_ty = seq.1;
        let item = match self.cx.types.kind(seq_ty) {
            TyKind::Array(elem, _) | TyKind::Vec(elem) => *elem,
            TyKind::Error => Types::ERROR,
            _ => {
                let shown = if writable {
                    let inner = self.ty(seq_ty);
                    format!("&var {inner}")
                } else {
                    self.ty(seq_ty)
                };
                self.error(format!("{shown} is not iterable"), iterable.span);
                Types::ERROR
            }
        };
        let locked = self.place_path(inner);
        if let Some(path) = locked.clone() {
            self.lock(path);
        }
        let kind = self.check_loop(|this| {
            this.enter();
            let var = this.declare(variable, item, writable, true, writable);
            let body_block = this.branch(body);
            this.leave();
            let leaves = always_leaves(std::slice::from_ref(body), true);
            (
                StmtKind::ForEach {
                    var,
                    seq: seq.0,
                    body: body_block,
                },
                leaves,
            )
        });
        if locked.is_some() {
            self.unlock();
        }
        kind
    }
}

/// Whether a `break` in `body` leaves the loop `body` belongs to, rather than
/// one inside it.
fn breaks_out(body: &[Statement]) -> bool {
    body.iter().any(|stmt| match &stmt.kind {
        StatementKind::Break => true,
        StatementKind::Block(inner) => breaks_out(inner),
        StatementKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            breaks_out(std::slice::from_ref(then_branch))
                || else_branch
                    .as_deref()
                    .is_some_and(|branch| breaks_out(std::slice::from_ref(branch)))
        }
        StatementKind::Expression(Expression {
            kind: ExpressionKind::Match { arms, .. },
            ..
        }) => arms
            .iter()
            .any(|(_, arm)| matches!(&arm.kind, ExpressionKind::Block(inner) if breaks_out(inner))),
        _ => false,
    })
}

/// Whether control never falls off the end of `body`: every path ends in a
/// `return`, or with `jumps`, in a `break` or `continue` too. A loop may run
/// zero times, so it never counts.
pub(super) fn always_leaves(body: &[Statement], jumps: bool) -> bool {
    body.iter().any(|stmt| {
        match &stmt.kind {
        StatementKind::Return(_) => true,
        StatementKind::While {
            cond: Expression {
                kind: ExpressionKind::Boolean(true),
                ..
            },
            body,
        } => !breaks_out(std::slice::from_ref(body)),
        StatementKind::Break | StatementKind::Continue => jumps,
        StatementKind::Block(inner) => always_leaves(inner, jumps),
        StatementKind::If {
            then_branch,
            else_branch: Some(else_branch),
            ..
        } => {
            always_leaves(std::slice::from_ref(then_branch), jumps)
                && always_leaves(std::slice::from_ref(else_branch), jumps)
        }
        StatementKind::Expression(Expression {
            kind: ExpressionKind::Match { arms, .. },
            ..
        }) => arms.iter().all(|(_, arm)| {
            matches!(&arm.kind, ExpressionKind::Block(inner) if always_leaves(inner, jumps))
        }),
        _ => false,
    }
    })
}
