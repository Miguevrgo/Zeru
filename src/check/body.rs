//! Checking one function instance: its locals and scopes, whether control
//! can reach each point, and its statements.

use crate::ast::{Expression, ExpressionKind, Names, Statement, StatementKind};
use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{self, Block, Expr, ExprKind, LocalId, Place, Stmt, StmtKind, Unpack, Walk};
use crate::ty::{Ty, TyKind, Types};

use super::infer::{DeferredCall, Var};
use super::{Checker, Receiver, Scope};

/// One function being checked. `diverges` says that
/// control cannot reach the point being checked, since every path to it
/// returned, broke out or went on; `breaks` says, for each loop around,
/// whether a `break` leaves it; `borrowed` marks the locals a reference was
/// taken into.
pub(super) struct FnCx<'c, 'a> {
    pub(super) cx: &'c mut Checker<'a>,
    pub(super) scope: Scope,
    ret: Ty,
    pub(super) in_instance: bool,
    locals: Vec<tir::Local>,
    mutable: Vec<bool>,
    borrowed: Vec<bool>,
    names: Vec<(String, LocalId)>,
    marks: Vec<usize>,
    pub(super) diverges: bool,
    breaks: Vec<bool>,
    pub(super) vars: Vec<Var>,
    pub(super) deferred: Vec<DeferredCall>,
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
            fx.declare(param, ty, *mutable, receiver)
        })
        .collect();
    let mut body_block = fx.block(body);
    if fx.cx.types.is_unit_result(ret) && !fx.diverges {
        let ok = fx.unit_ok(ret, span);
        body_block.stmts.push(tir::Stmt {
            kind: StmtKind::Return(Some(ok)),
            span,
        });
    } else if ret != Types::VOID && !fx.diverges {
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
        let shown = match symbol.len() > 120 {
            true => format!("{}...", &symbol[..120]),
            false => symbol.clone(),
        };
        for error in &mut fx.cx.errors[errors_before..] {
            error
                .notes
                .push((format!("in '{shown}', instantiated here"), origin));
        }
    }
    tir::Function {
        symbol,
        params,
        ret,
        locals,
        body: body_block,
        span,
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
            mutable: Vec::new(),
            borrowed: Vec::new(),
            names: Vec::new(),
            marks: Vec::new(),
            diverges: false,
            breaks: Vec::new(),
            vars: Vec::new(),
            deferred: Vec::new(),
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
        self.mutable[id.0 as usize]
    }

    pub(super) fn ty(&mut self, ty: Ty) -> String {
        let ty = self.resolve(ty);
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
    pub(super) fn declare(&mut self, name: &str, ty: Ty, mutable: bool, view: bool) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(tir::Local {
            name: name.to_string(),
            ty,
            view,
        });
        self.mutable.push(mutable);
        self.borrowed.push(false);
        self.names.push((name.to_string(), id));
        id
    }

    /// A reference into `place` is taken, so a `match` binding it starts
    /// from must be a view of the scrutinee.
    pub(super) fn note_borrow(&mut self, place: &Place) {
        if let Some(local) = place.root() {
            self.borrowed[local.0 as usize] = true;
        }
    }

    /// Decide, once its arm is checked, whether `binding` views the
    /// `scrutinee` or holds a copy: a copy of a value that owns no memory
    /// and is never borrowed reads the same, and lets the arm change the
    /// scrutinee.
    pub(super) fn settle_binding(&mut self, binding: LocalId, scrutinee: &Place) {
        let view = self.borrowed[binding.0 as usize] || self.cx.owns_heap(self.local_ty(binding));
        self.locals[binding.0 as usize].view = view;
        if view {
            self.note_borrow(scrutinee);
        }
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

    /// `expr` is given away by value: to a variable, a call, a field, a
    /// collection or the caller. What `Some(..)` wraps is given away with it.
    /// A temporary gives its value up and is then not dropped. A local is
    /// moved, and the borrow checker sees that it is not used again; a value
    /// inside a field, an element, a view or behind a pointer cannot be
    /// moved out at all, since what holds it would keep a second owner of it.
    pub(super) fn consume(&mut self, expr: &mut Expr) {
        if !self.cx.owns_heap(expr.ty) {
            return;
        }
        match &mut expr.kind {
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
            ExprKind::Read(Place::Local(local)) if self.locals[local.0 as usize].view => {
                let name = self.local_name(*local).to_string();
                self.error(
                    format!("Cannot move '{name}', which is borrowed; call .copy() on it"),
                    expr.span,
                );
                return;
            }
            ExprKind::Read(Place::Local(_)) => {}
            ExprKind::Read(Place::Field(..) | Place::Index(..) | Place::Deref(_)) => {
                self.error(
                    "Cannot move a value out of a field, an element or a pointer; call .copy() on it",
                    expr.span,
                );
                return;
            }
            _ => return,
        }
        let ExprKind::Read(place) = std::mem::replace(&mut expr.kind, ExprKind::Void) else {
            unreachable!()
        };
        expr.kind = ExprKind::Move(place);
    }

    /// Whether `value` of type `actual` can go where `target` is wanted, and
    /// the expression that puts it there: unchanged, retyped between pointer
    /// kinds, or wrapped in `Some` or `Ok`.
    pub(super) fn coerce(&mut self, mut value: Expr, target: Ty) -> Result<Expr, Expr> {
        let types = &self.cx.types;
        if value.ty == target || types.is_error(value.ty) || types.is_error(target) {
            return Ok(value);
        }
        if value.ty == Types::STRING && target == Types::STR {
            return Ok(self.view(value));
        }
        if let TyKind::Slice(wanted) = *types.kind(target)
            && let TyKind::Ref(held) | TyKind::RefMut(held) = *types.kind(value.ty)
            && let TyKind::Vec(elem) | TyKind::Array(elem, _) = *types.kind(held)
            && self.fits(wanted, elem, false)
        {
            let (span, ty) = (value.span, value.ty);
            let address = match value.kind {
                ExprKind::AddressOf(_) => value,
                _ => self.mk(ExprKind::AddressOf(Place::Deref(Box::new(value))), ty, span),
            };
            let from = self.mk(ExprKind::Int(0), Types::USIZE, span);
            let all = ExprKind::Builtin(tir::Builtin::Slice, vec![address, from]);
            let target = self.resolve(target);
            return Ok(self.mk(all, target, span));
        }
        if self.fits(target, value.ty, false) {
            value.ty = self.resolve(target);
            return Ok(value);
        }
        let target = self.resolve(target);
        let (inner, wrap): (Ty, fn(Box<Expr>) -> ExprKind) = match *self.cx.types.kind(target) {
            TyKind::Optional(inner) => (inner, ExprKind::Some),
            TyKind::Result(inner, _) => (inner, ExprKind::Ok),
            _ => return Err(value),
        };
        let value = self.coerce(value, inner)?;
        let span = value.span;
        Ok(Expr {
            kind: wrap(Box::new(value)),
            ty: target,
            span,
        })
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

    /// Report a write into `place` when the local it belongs to is not
    /// declared `var`. Behind a pointer the storage is the pointee's, which a
    /// `*T` or `&var T` may write and a `&T` may not.
    pub(super) fn require_mutable(&mut self, place: &Place, ast: &Expression, span: Span) {
        match self.constant_written(place, ast) {
            Some(name) => self.error(format!("Cannot modify constant '{name}'"), span),
            None => self.require_var_root(place, span),
        }
    }

    fn require_var_root(&mut self, place: &Place, span: Span) {
        match place {
            Place::Local(local) => {
                if !self.mutable[local.0 as usize] {
                    let name = self.local_name(*local).to_string();
                    self.error(
                        format!("Cannot modify '{name}', which is not declared 'var'"),
                        span,
                    );
                }
            }
            Place::Index(base, _)
                if self
                    .place_ty(base)
                    .is_some_and(|ty| matches!(self.cx.types.kind(ty), TyKind::Slice(_))) =>
            {
                self.error(
                    "Cannot write through a slice, which may point at read-only memory",
                    span,
                );
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
                let checked = self.value(value, expected);
                let ty = expected.unwrap_or(checked.ty);
                let mut value = self.coerce_or(checked, ty, span, |want, got| {
                    format!(
                        "Type mismatch for variable '{name}'. Annotated as {want} but got {got}"
                    )
                });
                self.consume(&mut value);
                self.cx.note_types_in(ty, span);
                let local = self.declare(name, ty, !is_const, false);
                StmtKind::Let(local, value)
            }
            StatementKind::VarTuple {
                names,
                is_const,
                value,
            } => {
                self.named_once(names, span);
                let hints: Vec<Option<Ty>> =
                    names.iter().map(|names| self.names_type(names)).collect();
                let mut value = match &value.kind {
                    ExpressionKind::Tuple(items) if items.len() == names.len() => {
                        self.tuple(items, hints, value.span)
                    }
                    _ => {
                        let whole = hints.iter().copied().collect::<Option<Vec<Ty>>>();
                        let whole = whole.map(|parts| self.cx.types.intern(TyKind::Tuple(parts)));
                        self.value(value, whole)
                    }
                };
                self.consume(&mut value);
                self.cx.note_types_in(value.ty, span);
                let target = self.declare_names(names, value.ty, !is_const, span);
                let kind = ExprKind::Unpack {
                    target,
                    value: Box::new(value),
                };
                StmtKind::Expr(self.mk(kind, Types::VOID, span))
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
                self.diverges = true;
                if value.is_none() && self.cx.types.is_unit_result(ret) {
                    self.error("A ! function returns early with 'return Ok();'", span);
                } else if value.is_none() && ret != Types::VOID {
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
                let before = self.diverges;
                let then = self.branch(then_branch);
                let then_leaves = std::mem::replace(&mut self.diverges, before);
                let otherwise = else_branch.as_ref().map(|branch| self.branch(branch));
                self.diverges = before || (then_leaves && otherwise.is_some() && self.diverges);
                StmtKind::If {
                    cond,
                    then,
                    otherwise,
                }
            }
            StatementKind::While { cond, body } => {
                let forever = matches!(cond.kind, ExpressionKind::Boolean(true));
                let cond = self.condition(cond, "While condition must be boolean, got ");
                let (body, broke) = self.loop_body(|this| this.branch(body));
                self.diverges |= forever && !broke;
                StmtKind::While { cond, body }
            }
            StatementKind::ForIn {
                index,
                variable,
                iterable,
                body,
            } => self.for_in(index.as_deref(), variable, iterable, body),
            StatementKind::Break | StatementKind::Continue if self.breaks.is_empty() => {
                self.error("Break/Continue can only be used inside loops", span);
                return None;
            }
            StatementKind::Break | StatementKind::Continue => {
                self.diverges = true;
                match statement.kind {
                    StatementKind::Break => {
                        *self.breaks.last_mut().unwrap() = true;
                        StmtKind::Break
                    }
                    _ => StmtKind::Continue,
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

    pub(super) fn condition(&mut self, condition: &Expression, message: &str) -> Expr {
        let cond = self.expr(condition, Some(Types::BOOL));
        if cond.ty != Types::BOOL && !self.cx.types.is_error(cond.ty) {
            let shown = self.ty(cond.ty);
            let hint = match &condition.kind {
                ExpressionKind::Assign { .. } => "; to compare, write ==",
                _ => "",
            };
            self.error(format!("{message}{shown}{hint}"), condition.span);
        }
        cond
    }

    /// `for i in a..b` counts; `for x in v` reads each element of an array,
    /// a Vec or a str, viewing one that owns memory rather than copying it;
    /// `for x in &var v` lets each be written through `x`; `for i, x in v`
    /// also counts each element's index in `i`; `for (a, b) in v` takes each
    /// element apart, and `for (x, y) in zip(xs, ys)` walks two at once.
    fn for_in(
        &mut self,
        index: Option<&str>,
        variable: &Names,
        iterable: &Expression,
        body: &Statement,
    ) -> StmtKind {
        if let ExpressionKind::Range { start, end } = &iterable.kind {
            if index.is_some() {
                self.error(
                    "A range counts already; write 'for i in a..b'",
                    iterable.span,
                );
            }
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
            let name = match variable {
                Names::One(name, _) => name.as_str(),
                Names::Skip => "",
                Names::Tuple(_) => {
                    self.error("A range gives one number at a time", iterable.span);
                    ""
                }
            };
            self.enter();
            let var = self.declare(name, ty, false, false);
            let (body, _) = self.loop_body(|this| this.branch(body));
            self.leave();
            return StmtKind::ForRange {
                var,
                start,
                end,
                body,
            };
        }
        if let Names::Tuple(names) = variable {
            self.named_once(names, iterable.span);
        }
        let sequences = match (&iterable.kind, variable) {
            (
                ExpressionKind::Call {
                    function,
                    arguments,
                    ..
                },
                Names::Tuple(names),
            ) if self.is_zip(function) && arguments.len() == 2 && names.len() == 2 => {
                arguments.iter().zip(names).collect()
            }
            (ExpressionKind::Call { function, .. }, _) if self.is_zip(function) => {
                self.error(
                    "zip walks two sequences side by side: for (a, b) in zip(xs, ys)",
                    iterable.span,
                );
                return StmtKind::Block(Block { stmts: Vec::new() });
            }
            _ => vec![(iterable, variable)],
        };
        let sequences: Vec<_> = sequences
            .into_iter()
            .map(|(iterable, names)| (self.sequence(iterable), names.clone()))
            .collect();
        self.enter();
        let index = index.map(|index| self.declare(index, Types::USIZE, false, false));
        let walks = sequences
            .into_iter()
            .map(|((seq, item, writable, walked), names)| {
                let names = match walked {
                    Walked::Set => Names::Tuple(vec![names, Names::Skip]),
                    _ => names,
                };
                let view = writable || self.cx.owns_heap(item);
                let (var, unpack) = match &names {
                    Names::One(..) | Names::Skip if walked == Walked::Map && writable => {
                        self.error(
                            "A map's keys cannot change; write for (key, value) in &var map",
                            iterable.span,
                        );
                        (self.declare("", item, false, view), None)
                    }
                    Names::One(name, _) => (self.declare(name, item, writable, view), None),
                    Names::Skip => (self.declare("", item, writable, view), None),
                    Names::Tuple(parts) => {
                        let var = self.declare("", item, writable, view);
                        let unpack = self.view_names(parts, item, writable, iterable.span);
                        if walked != Walked::List {
                            self.fix_key(&unpack);
                        }
                        (var, Some(unpack))
                    }
                };
                Walk { seq, var, unpack }
            })
            .collect();
        let (body, _) = self.loop_body(|this| this.branch(body));
        self.leave();
        StmtKind::ForEach {
            index,
            walks,
            body,
            within: iterable.span,
        }
    }

    /// Whether `function` is `zip`, which only a `for` walks, rather than a
    /// function of that name.
    fn is_zip(&self, function: &Expression) -> bool {
        matches!(&function.kind, ExpressionKind::Identifier(name) if name == "zip")
            && self.local("zip").is_none()
            && self
                .cx
                .items
                .lookup_value(self.scope.module, "zip")
                .is_none()
    }

    /// Make the key a loop over a map names unchangeable, as changing it
    /// would lose the entry.
    fn fix_key(&mut self, unpack: &Unpack) {
        fn locals(unpack: &Unpack, found: &mut Vec<LocalId>) {
            match unpack {
                Unpack::Declare(local) => found.push(*local),
                Unpack::Tuple(parts) => parts.iter().for_each(|part| locals(part, found)),
                Unpack::Skip | Unpack::Store(_) => {}
            }
        }
        let Unpack::Tuple(parts) = unpack else {
            return;
        };
        let mut keys = Vec::new();
        if let Some(key) = parts.first() {
            locals(key, &mut keys);
        }
        for key in keys {
            self.mutable[key.0 as usize] = false;
        }
    }

    /// The std collection `ty` is, which a `for` walks by its entries.
    fn walked(&self, ty: Ty) -> Walked {
        let Some(def) = self
            .cx
            .is_struct(ty)
            .filter(|&def| self.cx.items.is_std(def))
        else {
            return Walked::List;
        };
        match self.cx.items.item(def).name.as_str() {
            "hashmap::HashMap" => Walked::Map,
            "hashset::HashSet" => Walked::Set,
            _ => Walked::List,
        }
    }

    /// The field `name` of the struct at `place`, of type `ty`.
    fn field_of(&mut self, place: Place, ty: Ty, name: &str) -> (Place, Ty) {
        let fields = self.cx.fields_of(ty);
        let at = fields
            .iter()
            .position(|(field, _)| field == name)
            .expect("a std collection has the field");
        (Place::Field(Box::new(place), at as u32), fields[at].1)
    }

    /// What a `for` walks: the place of the sequence, the type of its
    /// elements, whether the loop may write them, and whether it walks a
    /// map or a set, by the list of their entries.
    fn sequence(&mut self, iterable: &Expression) -> (Place, Ty, bool, Walked) {
        let (inner, mut writable) = match &iterable.kind {
            ExpressionKind::BorrowRefMut(inner) => (inner.as_ref(), true),
            ExpressionKind::BorrowRef(inner) => (inner.as_ref(), false),
            _ => (iterable, false),
        };
        let (place, ty) = self.place(inner);
        let through_var = matches!(self.cx.types.kind(ty), TyKind::RefMut(_));
        let seq = self.through_ref(place, ty, inner.span);
        if through_var && std::ptr::eq(inner, iterable) {
            writable = true;
        } else if writable {
            self.require_mutable(&seq.0, inner, iterable.span);
            if self.temporary(&seq.0, seq.1, inner) {
                self.error(
                    "Cannot create reference to a temporary value",
                    iterable.span,
                );
            }
        }
        let walked = self.walked(seq.1);
        let seq = match walked {
            Walked::List => seq,
            Walked::Map => self.field_of(seq.0, seq.1, "entries"),
            Walked::Set => {
                let (members, ty) = self.field_of(seq.0, seq.1, "members");
                self.field_of(members, ty, "entries")
            }
        };
        let seq_ty = seq.1;
        let item = match self.cx.types.kind(seq_ty) {
            TyKind::Array(elem, _) | TyKind::Vec(elem) => *elem,
            TyKind::Slice(elem) if !writable => *elem,
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
        (seq.0, item, writable, walked)
    }

    /// Report a name `names` gives twice, which would hide the first.
    fn named_once(&mut self, names: &[Names], span: Span) {
        fn collect<'n>(names: &'n [Names], seen: &mut Vec<&'n str>) -> Option<&'n str> {
            for names in names {
                match names {
                    Names::One(name, _) if seen.contains(&name.as_str()) => return Some(name),
                    Names::One(name, _) => seen.push(name),
                    Names::Tuple(inner) => {
                        if let Some(twice) = collect(inner, seen) {
                            return Some(twice);
                        }
                    }
                    Names::Skip => {}
                }
            }
            None
        }
        if let Some(name) = collect(names, &mut Vec::new()) {
            self.error(format!("'{name}' is named twice"), span);
        }
    }

    /// The type `names` declare when each name has one written.
    fn names_type(&mut self, names: &Names) -> Option<Ty> {
        match names {
            Names::One(_, Some(spec)) => {
                let scope = self.scope.clone();
                Some(self.cx.resolve_type(spec, &scope, Span::default()))
            }
            Names::One(_, None) | Names::Skip => None,
            Names::Tuple(names) => {
                let parts = names
                    .iter()
                    .map(|names| self.names_type(names))
                    .collect::<Option<Vec<Ty>>>()?;
                Some(self.cx.types.intern(TyKind::Tuple(parts)))
            }
        }
    }

    /// The parts of a tuple of type `ty` that `names` declare, `mutable`
    /// for `var`, each checked against its written type.
    fn declare_names(&mut self, names: &[Names], ty: Ty, mutable: bool, span: Span) -> Unpack {
        let parts = self.parts(names.len(), ty, span);
        let parts = names
            .iter()
            .zip(parts)
            .map(|(names, part)| match names {
                Names::Skip => Unpack::Skip,
                Names::Tuple(inner) => self.declare_names(inner, part, mutable, span),
                Names::One(name, spec) => {
                    let written = spec.as_ref().and_then(|_| self.names_type(names));
                    if let Some(want) = written
                        && want != part
                        && !self.cx.types.is_error(part)
                    {
                        let (want, got) = (self.ty(want), self.ty(part));
                        self.error(
                            format!("Type mismatch for variable '{name}'. Annotated as {want} but got {got}"),
                            span,
                        );
                    }
                    Unpack::Declare(self.declare(name, part, mutable, false))
                }
            })
            .collect();
        Unpack::Tuple(parts)
    }

    /// The parts of each element of type `ty` a `for` names, each a view of
    /// the element's part or a copy of it, as the element itself would be.
    fn view_names(&mut self, names: &[Names], ty: Ty, writable: bool, span: Span) -> Unpack {
        let parts = self.parts(names.len(), ty, span);
        let parts = names
            .iter()
            .zip(parts)
            .map(|(names, part)| match names {
                Names::Skip => Unpack::Skip,
                Names::Tuple(inner) => self.view_names(inner, part, writable, span),
                Names::One(name, _) => {
                    let view = writable || self.cx.owns_heap(part);
                    Unpack::Declare(self.declare(name, part, writable, view))
                }
            })
            .collect();
        Unpack::Tuple(parts)
    }

    /// The types of the `count` parts of a tuple of type `ty`, or errors
    /// when it is not one.
    pub(super) fn parts(&mut self, count: usize, ty: Ty, span: Span) -> Vec<Ty> {
        match self.cx.types.kind(ty).clone() {
            TyKind::Tuple(parts) if parts.len() == count => parts,
            TyKind::Error => vec![Types::ERROR; count],
            _ => {
                let shown = self.ty(ty);
                let names = if count == 1 { "name" } else { "names" };
                self.error(
                    format!("Cannot take {shown} apart into {count} {names}"),
                    span,
                );
                vec![Types::ERROR; count]
            }
        }
    }

    /// A loop's body, checked by `body`, and whether a `break` leaves it.
    /// Control reaches past a loop as far as it reaches the loop, since the
    /// body may run no times.
    fn loop_body<T>(&mut self, body: impl FnOnce(&mut Self) -> T) -> (T, bool) {
        let before = self.diverges;
        self.breaks.push(false);
        let checked = body(self);
        self.diverges = before;
        (checked, self.breaks.pop().expect("pushed above"))
    }
}

/// Whether `place` is part of a temporary value, not reached through a
/// pointer.
pub(super) fn in_temporary(place: &Place) -> bool {
    match place {
        Place::Field(base, _) | Place::Index(base, _) => in_temporary(base),
        Place::Temp(_) => true,
        Place::Local(_) | Place::Deref(_) => false,
    }
}

/// How a `for` walks what it is given: as the list it is, or by the
/// entries of a map or the keys of a set.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Walked {
    List,
    Map,
    Set,
}
