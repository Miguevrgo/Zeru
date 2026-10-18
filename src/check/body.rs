//! Checking one function instance: its locals and scopes, which of them are
//! moved along each path, and its statements.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expression, ExpressionKind, Statement, StatementKind};
use crate::errors::Span;
use crate::items::DefId;
use crate::tir::{self, Block, Expr, ExprKind, LocalId, Place, Stmt, StmtKind};
use crate::ty::{Ty, TyKind, Types};

use super::{Checker, Receiver, Scope};

pub(super) struct LocalInfo {
    mutable: bool,
    borrowed: bool,
    /// What the reference it holds points into.
    loans: Vec<Loan>,
    /// For a view into another place, what a reference to it points into.
    within: Option<Vec<Loan>>,
    /// The block it lives in, 0 for a parameter, and the statement there
    /// that declares it.
    home: usize,
    declared_at: usize,
}

/// A borrow taken at `at`: of a local, or with no root of a temporary value
/// made in the statement checked at depth `depth`.
#[derive(Clone, Copy)]
pub(super) struct Loan {
    root: Option<LocalId>,
    mutable: bool,
    at: Span,
    depth: usize,
}

/// A block being checked, the statement it is at, the statements that use
/// each name, and those that declare one.
struct Position {
    block: usize,
    at: usize,
    uses: HashMap<String, Vec<usize>>,
    declares: HashMap<String, Vec<usize>>,
}

/// Which locals have given their value away, and where.
pub(super) type Moves = Vec<Option<Span>>;

struct LoopFrame {
    first_local: usize,
    /// How many blocks were open around the loop.
    depth: usize,
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
    positions: Vec<Position>,
    /// The locals that have held a reference.
    holders: Vec<LocalId>,
    /// Loans of call arguments already checked, held while the rest are.
    pub(super) pending_loans: Vec<Loan>,
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
            positions: Vec::new(),
            holders: Vec::new(),
            pending_loans: Vec::new(),
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
        let ending: Vec<LocalId> = self.names[mark..].iter().map(|(_, id)| *id).collect();
        self.names.truncate(mark);
        for local in ending {
            if let Some((holder, at)) = self.holder_of(local, false, true) {
                let (name, holder) = (self.local_name(local), self.describe(holder));
                let message =
                    format!("{holder} refers into '{name}', which ends with its block first");
                self.error(message, at);
            }
        }
    }

    /// Whether `local` is used from here on, its loans holding until then as
    /// in Rust: later in any block between its own and the current one, or
    /// with `after` later in the current one too. A use in a statement around
    /// the current one may come after it, or in the next turn of a loop, so
    /// it counts; a parameter is used throughout.
    fn live(&self, local: LocalId, after: bool) -> bool {
        self.used_up_to(local, self.positions.len() - 1, after)
    }

    /// Whether `local` is used after the current statement of the block at
    /// `depth`, or with `strict` false in it too, looking no deeper.
    fn used_up_to(&self, local: LocalId, depth: usize, strict: bool) -> bool {
        let info = &self.info[local.0 as usize];
        let Some(home) = self.depth_of(info.home) else {
            return info.home == 0;
        };
        let name = self.local_name(local);
        let looping = self
            .loops
            .iter()
            .any(|frame| frame.depth > home && frame.depth <= depth + 1);
        (home..=depth).any(|level| {
            let at = &self.positions[level];
            let redeclared = match level == home {
                true => at
                    .declares
                    .get(name)
                    .and_then(|indices| indices.iter().copied().find(|&i| i > info.declared_at)),
                false => None,
            };
            at.uses.get(name).is_some_and(|indices| {
                indices.iter().any(|&used| {
                    let current = used == at.at && (looping || !strict || level < depth);
                    (used > at.at || current) && redeclared.is_none_or(|end| used <= end)
                })
            })
        })
    }

    /// Where the block `home` is among those being checked; none for a
    /// parameter's, or a block already left.
    fn depth_of(&self, home: usize) -> Option<usize> {
        self.positions.iter().position(|at| at.block == home)
    }

    /// A live local whose reference points into `root`, and where it took
    /// it: any, or with `mutable` only a `&var` one. With `after`, one used
    /// after the current statement.
    fn holder_of(
        &self,
        root: LocalId,
        mutable: bool,
        after: bool,
    ) -> Option<(Option<LocalId>, Span)> {
        let wanted = |loan: &&Loan| loan.root == Some(root) && (loan.mutable || !mutable);
        let held = self.holders.iter().find_map(|&local| {
            let loan = self.info[local.0 as usize].loans.iter().find(wanted)?;
            self.live(local, after).then_some((Some(local), loan.at))
        });
        held.or_else(|| {
            let loan = self.pending_loans.iter().find(wanted)?;
            Some((None, loan.at))
        })
    }

    /// A holder `holder_of` found, as an error names it.
    fn describe(&self, holder: Option<LocalId>) -> String {
        match holder {
            Some(local) => format!("'{}'", self.local_name(local)),
            None => "an earlier argument".to_string(),
        }
    }

    /// Give `holder` the loans `loans`, refusing to share what a live `&var`
    /// may change meanwhile.
    pub(super) fn hold(&mut self, holder: LocalId, loans: Vec<Loan>, span: Span) {
        for loan in &loans {
            match loan.root {
                None if self.used_up_to(holder, loan.depth, true) => self.error(
                    "Cannot keep a reference into a temporary value past its statement; store the value in a variable first",
                    loan.at,
                ),
                Some(root)
                    if !loan.mutable
                        && self
                            .mutable_holder(root)
                            .is_some_and(|other| other != Some(holder)) =>
                {
                    self.require_unchanging(root, span);
                }
                _ => {}
            }
        }
        if !loans.is_empty() && self.info[holder.0 as usize].loans.is_empty() {
            self.holders.push(holder);
        }
        self.info[holder.0 as usize].loans.extend(loans);
    }

    /// Report a reference in `loans` that may outlive what it points into:
    /// one into a local or a temporary of this function, where only the
    /// views it was given (`self`) live on past it.
    pub(super) fn require_outliving(&mut self, loans: &[Loan], doing: &str) {
        for loan in loans {
            let name = match loan.root {
                None => "a temporary value".to_string(),
                Some(root)
                    if self.locals[root.0 as usize].view
                        && self.info[root.0 as usize].home == 0 =>
                {
                    continue;
                }
                Some(root) => format!("'{}'", self.local_name(root)),
            };
            self.error(
                format!("Cannot {doing} a reference into {name}, which ends with this function"),
                loan.at,
            );
        }
    }

    fn mutable_holder(&self, root: LocalId) -> Option<Option<LocalId>> {
        self.holder_of(root, true, false).map(|(holder, _)| holder)
    }

    /// Report a reference into `root` taken while a live `&var` can change it.
    pub(super) fn require_unchanging(&mut self, root: LocalId, span: Span) {
        if let Some(other) = self.mutable_holder(root) {
            let (name, other) = (self.local_name(root), self.describe(other));
            self.error(
                format!("Cannot refer into '{name}' while {other} can change it"),
                span,
            );
        }
    }

    /// Make a reference to the view `local` borrow what `place` borrows,
    /// unless that is a temporary.
    pub(super) fn view_into(&mut self, local: LocalId, place: &Place, at: Span) {
        let loans = self.place_loans(place, Some(at));
        if !loans.is_empty() {
            self.info[local.0 as usize].within = Some(loans);
        }
    }

    pub(super) fn clear_loans(&mut self, local: LocalId) {
        self.info[local.0 as usize].loans.clear();
    }

    /// What the value of `expr` borrows, when it holds a reference.
    pub(super) fn loans_of(&mut self, expr: &Expr) -> Vec<Loan> {
        let Some(mutable) = self.cx.holds_ref(expr.ty) else {
            return Vec::new();
        };
        let mut loans = match &expr.kind {
            ExprKind::AddressOf(place) => self.place_loans(place, Some(expr.span)),
            ExprKind::Read(place) | ExprKind::Move(place) | ExprKind::Copy(place) => {
                self.place_loans(place, None)
            }
            ExprKind::Some(inner)
            | ExprKind::Ok(inner)
            | ExprKind::Err(inner)
            | ExprKind::Try(inner) => self.loans_of(inner),
            ExprKind::Call(_, values) | ExprKind::Builtin(_, values) => values
                .iter()
                .flat_map(|value| match &value.kind {
                    ExprKind::AddressOf(place) => self.place_loans(place, Some(value.span)),
                    _ => self.loans_of(value),
                })
                .collect(),
            ExprKind::Tuple(values) | ExprKind::Array(values) | ExprKind::VecLiteral(values) => {
                values.iter().flat_map(|v| self.loans_of(v)).collect()
            }
            ExprKind::Struct(fields) => fields.iter().flat_map(|(_, v)| self.loans_of(v)).collect(),
            ExprKind::Match { scrutinee, arms } => {
                let mut loans = self.place_loans(scrutinee, None);
                for arm in arms {
                    loans.extend(self.loans_of(&arm.body));
                }
                loans
            }
            _ => Vec::new(),
        };
        if mutable {
            loans.iter_mut().for_each(|loan| loan.mutable = true);
        }
        loans
    }

    /// The loans of a value read from `place`, or of a reference to it
    /// taken at `borrowed`.
    pub(super) fn place_loans(&mut self, place: &Place, borrowed: Option<Span>) -> Vec<Loan> {
        match place {
            Place::Local(local) => {
                let info = &self.info[local.0 as usize];
                let mut loans = info.loans.clone();
                match (borrowed, &info.within) {
                    (Some(at), Some(within)) => {
                        loans.extend(within.iter().map(|loan| Loan { at, ..*loan }));
                    }
                    (Some(at), None) => loans.push(Loan {
                        root: Some(*local),
                        mutable: false,
                        at,
                        depth: 0,
                    }),
                    (None, _) => {}
                }
                loans
            }
            Place::Field(base, _) | Place::Index(base, _) => self.place_loans(base, borrowed),
            Place::Deref(pointer) => self.loans_of(pointer),
            Place::Temp(value) => {
                let mut loans = self.loans_of(value);
                if let Some(at) = borrowed {
                    loans.push(Loan {
                        root: None,
                        mutable: false,
                        at,
                        depth: self.positions.len().saturating_sub(1),
                    });
                }
                loans
            }
        }
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
        let (home, declared_at) = self.positions.last().map_or((0, 0), |at| (at.block, at.at));
        self.info.push(LocalInfo {
            mutable,
            borrowed,
            loans: Vec::new(),
            within: None,
            home,
            declared_at,
        });
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
        if let Some((holder, _)) = self.holder_of(local, false, false) {
            let (name, holder) = (self.local_name(local), self.describe(holder));
            self.error(
                format!("Cannot move '{name}' while {holder} still refers into it"),
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
            depth: self.positions.len(),
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
        if let Some((holder, _)) = self.holder_of(path.root, false, false) {
            let (name, holder) = (self.show_path(&path), self.describe(holder));
            self.error(
                format!("Cannot change '{name}' while {holder} still refers into it"),
                span,
            );
            return;
        }
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
        let mut uses: HashMap<String, Vec<usize>> = HashMap::new();
        let mut declares: HashMap<String, Vec<usize>> = HashMap::new();
        for (at, statement) in statements.iter().enumerate() {
            statement.names(&mut |name| {
                let indices = uses.entry(name.to_string()).or_default();
                if indices.last() != Some(&at) {
                    indices.push(at);
                }
            });
            if let StatementKind::Var { name, .. } = &statement.kind {
                declares.entry(name.clone()).or_default().push(at);
            }
        }
        self.positions.push(Position {
            block: statements.as_ptr() as usize,
            at: 0,
            uses,
            declares,
        });
        let mut stmts = Vec::new();
        for (at, statement) in statements.iter().enumerate() {
            self.positions.last_mut().expect("pushed above").at = at;
            stmts.extend(self.stmt(statement));
        }
        self.positions.pop();
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
                let loans = self.loans_of(&value);
                self.hold(local, loans, span);
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
                    let loans = self.loans_of(&value);
                    self.require_outliving(&loans, "return");
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
        if let Some(path) = &locked
            && !writable
        {
            self.require_unchanging(path.root, iterable.span);
        }
        if let Some(path) = locked.clone() {
            self.lock(path);
        }
        let kind = self.check_loop(|this| {
            this.enter();
            let var = this.declare(variable, item, writable, true, writable);
            if writable {
                this.view_into(var, &seq.0, iterable.span);
            }
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

/// The local whose own storage `place` is part of, not reached through a
/// pointer.
pub(super) fn local_root(place: &Place) -> Option<LocalId> {
    match place {
        Place::Local(local) => Some(*local),
        Place::Field(base, _) | Place::Index(base, _) => local_root(base),
        Place::Deref(_) | Place::Temp(_) => None,
    }
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
