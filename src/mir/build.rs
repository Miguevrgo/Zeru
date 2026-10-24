//! Building each function's MIR from its typed IR. Every value an expression needs is evaluated, in
//! source order, into an operand: a constant, or a temporary local. Owned
//! values are dropped by the scope that holds them, at its end or at any
//! jump out of it. Assignment evaluates its right side before its target.

use std::mem::replace;

use crate::errors::{Sources, Span, ZeruError};
use crate::lower::SafetyMode;
use crate::tir::{
    self, Arm, BinOp, Builtin, Expr, ExprKind, FnId, Payload, Program, Stmt, StmtKind, UnOp,
};
use crate::ty::{Ty, TyKind, Types};

use super::{
    Aggregate, Asm, BlockData, BlockId, Body, ConstValue, Constant, Local, LocalDecl, Operand,
    Panic, Part, Place, Projection, RETURN, Rvalue, Statement, StatementKind, Terminator,
    TerminatorKind,
};

/// The MIR of every function of `program`, indexed by [`FnId`], or the
/// errors the borrow checker found in it.
pub fn build(
    program: &mut Program,
    sources: &Sources,
    mode: SafetyMode,
) -> Result<Vec<Body>, Vec<ZeruError>> {
    let functions = std::mem::take(&mut program.functions);
    let mut errors: Vec<ZeruError> = Vec::new();
    let mut bodies = Vec::new();
    for function in &functions {
        let mut body = Builder::new(program, sources, function, mode.emit_safety_checks()).build();
        for error in super::borrowck::check(&body, program) {
            if !errors
                .iter()
                .any(|e| e.span == error.span && e.message == error.message)
            {
                errors.push(error);
            }
        }
        super::elaborate::elaborate(&mut body, program);
        bodies.push(body);
    }
    match errors.is_empty() {
        true => Ok(bodies),
        false => Err(errors),
    }
}

/// A loop being built: where `continue` and `break` go, and how many scopes
/// and temporaries were open around it.
struct LoopScope {
    next: BlockId,
    exit: BlockId,
    scopes: usize,
    temps: usize,
}

/// One function's MIR being built. `scopes` holds the locals each open
/// scope ends and whether it drops each; `temps` the temporaries of the
/// statements being built and whether each is dropped as they end.
struct Builder<'a> {
    program: &'a mut Program,
    sources: &'a Sources,
    function: &'a tir::Function,
    checks: bool,
    locals: Vec<LocalDecl>,
    blocks: Vec<(Vec<Statement>, Option<Terminator>)>,
    current: Option<BlockId>,
    map: Vec<Option<Local>>,
    views: Vec<Option<Place>>,
    scopes: Vec<Vec<(Local, bool)>>,
    temps: Vec<(Local, bool)>,
    loops: Vec<LoopScope>,
    span: Span,
}

impl<'a> Builder<'a> {
    fn new(
        program: &'a mut Program,
        sources: &'a Sources,
        function: &'a tir::Function,
        checks: bool,
    ) -> Self {
        let count = function.locals.len();
        Self {
            program,
            sources,
            function,
            checks,
            locals: vec![LocalDecl {
                ty: function.ret,
                name: None,
            }],
            blocks: Vec::new(),
            current: None,
            map: vec![None; count],
            views: vec![None; count],
            scopes: vec![Vec::new()],
            temps: Vec::new(),
            loops: Vec::new(),
            span: function.span,
        }
    }

    fn build(mut self) -> Body {
        let entry = self.new_block();
        self.current = Some(entry);
        let function = self.function;
        for &param in &function.params {
            let local = &function.locals[param.0 as usize];
            if local.view {
                let pointer = self.program.types.intern(TyKind::Ref(local.ty));
                let mir = self.local(pointer, Some(local.name.clone()));
                self.views[param.0 as usize] = Some(Place::local(mir).project(Projection::Deref));
            } else {
                let mir = self.declare(param);
                self.own(mir);
            }
        }
        self.stmts(&function.body.stmts);
        if self.current.is_some() {
            if function.ret == Types::VOID {
                self.exit(0, 0);
                self.terminate(TerminatorKind::Return);
            } else {
                self.terminate(TerminatorKind::Unreachable);
            }
        }
        let blocks = self
            .blocks
            .into_iter()
            .map(|(statements, terminator)| BlockData {
                statements,
                terminator: terminator.unwrap_or(Terminator {
                    kind: TerminatorKind::Unreachable,
                    span: function.span,
                }),
            })
            .collect();
        Body {
            symbol: function.symbol.clone(),
            span: function.span,
            arg_count: function.params.len(),
            locals: self.locals,
            blocks,
        }
    }

    fn types(&self) -> &Types {
        &self.program.types
    }

    fn new_block(&mut self) -> BlockId {
        self.blocks.push((Vec::new(), None));
        BlockId(self.blocks.len() as u32 - 1)
    }

    fn local(&mut self, ty: Ty, name: Option<String>) -> Local {
        self.locals.push(LocalDecl { ty, name });
        Local(self.locals.len() as u32 - 1)
    }

    fn temp(&mut self, ty: Ty) -> Local {
        self.local(ty, None)
    }

    fn declare(&mut self, local: tir::LocalId) -> Local {
        let decl = &self.function.locals[local.0 as usize];
        let mir = self.local(decl.ty, Some(decl.name.clone()));
        self.map[local.0 as usize] = Some(mir);
        mir
    }

    /// The current scope ends `local`, and drops it if it owns anything.
    fn own(&mut self, local: Local) {
        let drop = self.program.needs_drop(self.locals[local.0 as usize].ty);
        self.scopes.last_mut().unwrap().push((local, drop));
    }

    fn push(&mut self, kind: StatementKind) {
        if let Some(block) = self.current {
            let span = self.span;
            self.blocks[block.0 as usize]
                .0
                .push(Statement { kind, span });
        }
    }

    fn assign(&mut self, place: Place, rvalue: Rvalue) {
        self.push(StatementKind::Assign(place, rvalue));
    }

    /// End the current block with `kind`; what follows is unreachable until
    /// another block is entered.
    fn terminate(&mut self, kind: TerminatorKind) {
        if let Some(block) = self.current.take() {
            let span = self.span;
            self.blocks[block.0 as usize].1 = Some(Terminator { kind, span });
        }
    }

    fn goto(&mut self, target: BlockId) {
        self.terminate(TerminatorKind::Goto(target));
    }

    /// End the current block with `kind`, which goes on to a new block.
    fn then(&mut self, kind: impl FnOnce(BlockId) -> TerminatorKind) {
        let target = self.new_block();
        if self.current.is_some() {
            self.terminate(kind(target));
            self.current = Some(target);
        }
    }

    fn drop(&mut self, place: Place) {
        self.then(|target| TerminatorKind::Drop { place, target });
    }

    fn assert(&mut self, cond: Operand, panic: Panic) {
        self.then(|target| TerminatorKind::Assert {
            cond,
            panic,
            target,
        });
    }

    /// `assert`: when `cond` is false, write where it failed and its message,
    /// or else `cond` as written, to stderr and abort.
    fn assert_holds(&mut self, cond: &Expr, message: Option<&Expr>) {
        let holds = self.as_operand(cond);
        let (fail, next) = (self.new_block(), self.new_block());
        self.switch(holds, vec![(1, next)], fail);
        self.current = Some(fail);
        let mut first = match self.sources.position(self.span) {
            Some((file, line, column)) => format!("panic at {file}:{line}:{column}: "),
            None => String::new(),
        };
        if message.is_none() {
            first.push_str("assertion failed: ");
            first.push_str(self.sources.text(cond.span));
        }
        self.push(StatementKind::Print {
            stderr: true,
            newline: message.is_none(),
            pieces: vec![first.into_bytes()],
            values: Vec::new(),
        });
        if let Some(message) = message {
            self.expr_into(None, message);
        }
        self.terminate(TerminatorKind::Assert {
            cond: Operand::Constant(Constant {
                value: ConstValue::Bool(false),
                ty: Types::BOOL,
            }),
            panic: Panic::Abort,
            target: next,
        });
        self.current = Some(next);
    }

    fn switch(&mut self, discr: Operand, targets: Vec<(i128, BlockId)>, otherwise: BlockId) {
        self.terminate(TerminatorKind::SwitchInt {
            discr,
            targets,
            otherwise,
        });
    }

    /// Drop the temporaries from `temps` on and every value the scopes from
    /// `scopes` on hold, last made first, for a jump out of them, then end
    /// them all.
    fn exit(&mut self, temps: usize, scopes: usize) {
        let ending: Vec<(Local, bool)> = self.temps[temps..]
            .iter()
            .rev()
            .chain(self.scopes[scopes..].iter().flatten().rev())
            .copied()
            .collect();
        for &(local, drop) in &ending {
            if drop {
                self.drop(Place::local(local));
            }
        }
        for (local, _) in ending {
            self.push(StatementKind::StorageDead(local));
        }
    }

    fn drop_temps(&mut self, mark: usize) {
        self.exit(mark, self.scopes.len());
        self.temps.truncate(mark);
    }

    fn block(&mut self, block: &tir::Block) {
        self.scope(|this| this.stmts(&block.stmts));
    }

    /// `inner` built in a scope of its own, which ends after it.
    fn scope(&mut self, inner: impl FnOnce(&mut Self)) {
        self.scopes.push(Vec::new());
        inner(self);
        self.exit(self.temps.len(), self.scopes.len() - 1);
        self.scopes.pop();
    }

    /// Statements after one that leaves are never reached and not built.
    fn stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if self.current.is_none() {
                return;
            }
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        let outer = replace(&mut self.span, stmt.span);
        let mark = self.temps.len();
        match &stmt.kind {
            StmtKind::Let(local, value) => {
                let mir = self.declare(*local);
                self.expr_into(Some(Place::local(mir)), value);
                self.own(mir);
            }
            StmtKind::Expr(expr) => self.effect(expr),
            StmtKind::Return(value) => {
                if let Some(value) = value {
                    self.expr_into(Some(Place::local(RETURN)), value);
                }
                self.exit(0, 0);
                self.terminate(TerminatorKind::Return);
            }
            StmtKind::Break | StmtKind::Continue => {
                let target = self.loops.last().expect("a jump is inside a loop");
                let to = match stmt.kind {
                    StmtKind::Break => target.exit,
                    _ => target.next,
                };
                self.exit(target.temps, target.scopes);
                self.goto(to);
            }
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.condition(cond);
                let then_bb = self.new_block();
                let merge = self.new_block();
                let else_bb = match otherwise {
                    Some(_) => self.new_block(),
                    None => merge,
                };
                self.switch(cond, vec![(0, else_bb)], then_bb);
                self.current = Some(then_bb);
                self.block(then);
                self.goto(merge);
                if let Some(otherwise) = otherwise {
                    self.current = Some(else_bb);
                    self.block(otherwise);
                    self.goto(merge);
                }
                self.current = Some(merge);
            }
            StmtKind::While { cond, body } => {
                self.looping(|this| this.condition(cond), |_| {}, body, |_, _| {})
            }
            StmtKind::ForRange {
                var,
                start,
                end,
                inclusive,
                body,
            } => self.scope(|this| this.for_range(*var, start, end, *inclusive, body)),
            StmtKind::ForEach {
                index,
                walks,
                body,
                within,
            } => self.scope(|this| {
                this.span = *within;
                this.for_each(*index, walks, body)
            }),
            StmtKind::Block(block) => self.block(block),
        }
        self.drop_temps(mark);
        self.span = outer;
    }

    /// A condition, whose temporaries are done with once it is decided.
    fn condition(&mut self, cond: &Expr) -> Operand {
        let mark = self.temps.len();
        let value = self.as_operand(cond);
        self.drop_temps(mark);
        value
    }

    /// A loop: `test` decides at the top of each turn whether to take it,
    /// `at_start` begins it in the scope of `body`, and `step` ends it,
    /// given the block after the loop to leave by.
    fn looping(
        &mut self,
        test: impl FnOnce(&mut Self) -> Operand,
        at_start: impl FnOnce(&mut Self),
        body: &tir::Block,
        step: impl FnOnce(&mut Self, BlockId),
    ) {
        let cond_bb = self.new_block();
        self.goto(cond_bb);
        self.current = Some(cond_bb);
        let more = test(self);
        let body_bb = self.new_block();
        let next = self.new_block();
        let after = self.new_block();
        self.switch(more, vec![(0, after)], body_bb);
        self.current = Some(body_bb);
        self.loops.push(LoopScope {
            next,
            exit: after,
            scopes: self.scopes.len(),
            temps: self.temps.len(),
        });
        self.scope(|this| {
            at_start(this);
            this.stmts(&body.stmts);
        });
        self.loops.pop();
        self.goto(next);
        self.current = Some(next);
        step(self, after);
        self.goto(cond_bb);
        self.current = Some(after);
    }

    /// `for i in start..end`, or with `inclusive`, `start..=end`, which
    /// leaves after the turn for `end` rather than count past it.
    fn for_range(
        &mut self,
        var: tir::LocalId,
        start: &Expr,
        end: &Expr,
        inclusive: bool,
        body: &tir::Block,
    ) {
        let first = self.as_operand(start);
        let bound = reused(self.as_operand(end));
        let index = self.declare(var);
        self.own(index);
        self.assign(Place::local(index), Rvalue::Use(first));
        let ty = self.locals[index.0 as usize].ty;
        let at = Operand::Copy(Place::local(index));
        let within = match inclusive {
            true => BinOp::Le,
            false => BinOp::Lt,
        };
        let last = bound.clone();
        self.looping(
            |this| this.binary(within, at.clone(), bound),
            |_| {},
            body,
            |this, after| {
                if inclusive {
                    let last = this.binary(BinOp::Eq, at.clone(), last);
                    let more = this.new_block();
                    this.switch(last, vec![(0, more)], after);
                    this.current = Some(more);
                }
                let rvalue = Rvalue::Binary {
                    op: BinOp::AddWrap,
                    left: at.clone(),
                    right: constant(1, ty),
                    checked: false,
                };
                this.assign(Place::local(index), rvalue);
            },
        );
    }

    /// `for i, x in seq`, or `for (x, y) in zip(xs, ys)`: for each sequence
    /// a reference walks from the first element on, so the loop borrows
    /// every element throughout, and `i`, when written, counts up beside it,
    /// until the shortest sequence ends.
    fn for_each(&mut self, index: Option<tir::LocalId>, walks: &[tir::Walk], body: &tir::Block) {
        let count = match index {
            Some(index) => {
                let index = self.declare(index);
                self.own(index);
                index
            }
            None => self.temp(Types::USIZE),
        };
        self.assign(Place::local(count), Rvalue::Use(constant(0, Types::USIZE)));
        let mut walking = Vec::new();
        for walk in walks {
            let seq = self.as_place(&walk.seq);
            let elem = self
                .types()
                .elem(self.place_ty(&seq))
                .expect("only an array, a Vec or a slice is walked");
            let pointer = self.program.types.intern(TyKind::Ref(elem));
            let len = self.temp(Types::USIZE);
            self.assign(Place::local(len), Rvalue::Len(seq.clone()));
            let at = self.local(pointer, Some("for".to_string()));
            let element = seq.project(Projection::Index(count));
            self.assign(Place::local(at), Rvalue::AddressOf(element));
            walking.push((walk, len, at));
        }
        self.looping(
            |this| {
                let mut more: Option<Operand> = None;
                for &(_, len, _) in &walking {
                    let len = Operand::Copy(Place::local(len));
                    let inside = this.binary(BinOp::Lt, Operand::Copy(Place::local(count)), len);
                    more = Some(match more {
                        Some(more) => this.binary(BinOp::BitAnd, more, inside),
                        None => inside,
                    });
                }
                more.expect("a for walks at least one sequence")
            },
            |this| {
                for &(walk, _, at) in &walking {
                    let current = Place::local(at).project(Projection::Deref);
                    let var = this.element(walk.var, current);
                    if let Some(unpack) = &walk.unpack {
                        this.view_parts(unpack, var);
                    }
                }
            },
            body,
            |this, _| {
                let next = |place: Local, op| Rvalue::Binary {
                    op,
                    left: Operand::Copy(Place::local(place)),
                    right: constant(1, Types::USIZE),
                    checked: false,
                };
                for &(_, _, at) in &walking {
                    this.assign(Place::local(at), next(at, BinOp::PtrAdd));
                }
                this.assign(Place::local(count), next(count, BinOp::AddWrap));
            },
        );
    }

    /// `local`, a loop's element or a part of it, made the value at `place`:
    /// a view of it, or a copy; where the value now is.
    fn element(&mut self, local: tir::LocalId, place: Place) -> Place {
        if self.function.locals[local.0 as usize].view {
            self.views[local.0 as usize] = Some(place.clone());
            return place;
        }
        let copy = self.declare(local);
        self.scopes.last_mut().unwrap().push((copy, false));
        self.assign(Place::local(copy), Rvalue::Use(Operand::Copy(place)));
        Place::local(copy)
    }

    /// The parts of the tuple at `place` given to the locals of `unpack`,
    /// each a view or a copy as `element` makes it.
    fn view_parts(&mut self, unpack: &tir::Unpack, place: Place) {
        match unpack {
            tir::Unpack::Declare(local) => {
                self.element(*local, place);
            }
            tir::Unpack::Tuple(parts) => {
                for (at, part) in parts.iter().enumerate() {
                    self.view_parts(part, place.clone().project(Projection::Field(at as u32)));
                }
            }
            tir::Unpack::Skip | tir::Unpack::Store(_) => {}
        }
    }

    /// `target = value` for a tuple `value`, taken apart: each part goes
    /// where `target` puts it, and a part it skips is dropped. The tuple is
    /// held in a temporary nothing drops, so its parts change owner as they
    /// are copied out.
    fn unpack(&mut self, target: &tir::Unpack, value: &Expr) {
        let held = self.temp(value.ty);
        self.expr_into(Some(Place::local(held)), value);
        self.unpack_into(target, Place::local(held));
    }

    fn unpack_into(&mut self, target: &tir::Unpack, from: Place) {
        let owned = self.program.needs_drop(self.place_ty(&from));
        let value = Operand::Copy(from.clone());
        match target {
            tir::Unpack::Tuple(parts) => {
                for (at, part) in parts.iter().enumerate() {
                    self.unpack_into(part, from.clone().project(Projection::Field(at as u32)));
                }
            }
            tir::Unpack::Skip if owned => self.drop(from),
            tir::Unpack::Skip => {}
            tir::Unpack::Declare(local) => {
                let mir = self.declare(*local);
                self.assign(Place::local(mir), Rvalue::Use(value));
                self.own(mir);
            }
            tir::Unpack::Store(place) => {
                let target = self.as_place(place);
                if owned {
                    self.drop(target.clone());
                }
                self.assign(target, Rvalue::Use(value));
            }
        }
    }

    fn place_ty(&self, place: &Place) -> Ty {
        self.program
            .projected(self.locals[place.local.0 as usize].ty, &place.projection)
    }

    /// The place `place` names, what its indices and pointers need evaluated
    /// and checked now; what it holds is read where it is used.
    fn as_place(&mut self, place: &tir::Place) -> Place {
        match place {
            tir::Place::Local(local) => match &self.views[local.0 as usize] {
                Some(view) => view.clone(),
                None => Place::local(self.map[local.0 as usize].expect("declared before use")),
            },
            tir::Place::Field(base, at) => self.as_place(base).project(Projection::Field(*at)),
            tir::Place::Index(base, index) => {
                let mut base = self.as_place(base);
                if !simple(index)
                    && base
                        .projection
                        .iter()
                        .any(|p| matches!(p, Projection::Index(_)))
                {
                    let pointer = self.program.types.intern(TyKind::Ref(self.place_ty(&base)));
                    let held = self.temp(pointer);
                    self.assign(Place::local(held), Rvalue::AddressOf(base));
                    base = Place::local(held).project(Projection::Deref);
                }
                let at = self.index(index);
                if self.checks {
                    let len = self.temp(Types::USIZE);
                    self.assign(Place::local(len), Rvalue::Len(base.clone()));
                    let inside = self.binary(
                        BinOp::Lt,
                        Operand::Copy(Place::local(at)),
                        Operand::Move(Place::local(len)),
                    );
                    self.assert(inside, Panic::Bounds);
                }
                base.project(Projection::Index(at))
            }
            tir::Place::Deref(pointer) => {
                let ty = pointer.ty;
                let pointer = self.as_temp(pointer);
                if self.checks {
                    let set = self.binary(
                        BinOp::Ne,
                        Operand::Copy(Place::local(pointer)),
                        constant(0, ty),
                    );
                    self.assert(set, Panic::Null);
                }
                Place::local(pointer).project(Projection::Deref)
            }
            tir::Place::Temp(value) => {
                let temp = self.as_temp(value);
                if self.temps.last() != Some(&(temp, true)) {
                    self.temps.push((temp, false));
                }
                Place::local(temp)
            }
        }
    }

    /// An index, as a usize in a local of its own.
    fn index(&mut self, index: &Expr) -> Local {
        let at = self.temp(Types::USIZE);
        let value = match &index.kind {
            ExprKind::Int(value) => Rvalue::Use(constant(*value, Types::USIZE)),
            _ if index.ty == Types::USIZE => {
                let value = self.as_operand(index);
                Rvalue::Use(value)
            }
            _ => Rvalue::Cast {
                value: self.as_operand(index),
                to: Types::USIZE,
                checked: false,
            },
        };
        self.assign(Place::local(at), value);
        at
    }

    /// The parts of a tuple or a struct, which compare and hash in order.
    fn parts_of(&self, ty: Ty) -> Option<Vec<Ty>> {
        match self.types().kind(ty) {
            TyKind::Tuple(parts) => Some(parts.clone()),
            _ => self.program.structs.get(&ty).map(|s| s.fields.clone()),
        }
    }

    /// How the tuples or structs at `left` and `right` order, -1, 0 or 1:
    /// the first part that differs decides.
    fn tuple_order(&mut self, left: Place, right: Place, ty: Ty) -> Operand {
        let parts = self
            .parts_of(ty)
            .expect("only tuples and structs are compared part by part");
        let mut order = constant(0, Types::I32);
        for (at, part) in parts.into_iter().enumerate() {
            let field = Projection::Field(at as u32);
            let (l, r) = (left.clone().project(field), right.clone().project(field));
            let this = self.part_order(l, r, part);
            let undecided = self.binary(BinOp::Eq, order.clone(), constant(0, Types::I32));
            let undecided = self.number(undecided);
            let counted = self.arithmetic(BinOp::MulWrap, undecided, this);
            order = self.arithmetic(BinOp::AddWrap, order, counted);
        }
        order
    }

    /// How the parts at `l` and `r`, of type `ty`, order: -1, 0 or 1.
    fn part_order(&mut self, l: Place, r: Place, ty: Ty) -> Operand {
        if self.parts_of(ty).is_some() {
            return self.tuple_order(l, r, ty);
        }
        if ty == Types::STR || ty == Types::STRING {
            let (function, args) = match ty == Types::STR {
                true => (
                    self.program.runtime.str_cmp,
                    [Operand::Copy(l), Operand::Copy(r)],
                ),
                false => {
                    let pointer = self.program.types.intern(TyKind::Ref(ty));
                    let [a, b] = [l, r].map(|place| {
                        let held = self.temp(pointer);
                        self.assign(Place::local(held), Rvalue::AddressOf(place));
                        Operand::Move(Place::local(held))
                    });
                    (self.program.runtime.string_cmp, [a, b])
                }
            };
            let result = self.temp(Types::I32);
            self.then(|target| TerminatorKind::Call {
                function,
                args: args.to_vec(),
                dest: Some(Place::local(result)),
                target,
            });
            return Operand::Move(Place::local(result));
        }
        let greater = self.binary(
            BinOp::Gt,
            Operand::Copy(l.clone()),
            Operand::Copy(r.clone()),
        );
        let less = self.binary(BinOp::Lt, Operand::Copy(l), Operand::Copy(r));
        let (greater, less) = (self.number(greater), self.number(less));
        self.arithmetic(BinOp::SubWrap, greater, less)
    }

    /// The bool `flag` as an `i32`, 1 or 0.
    fn number(&mut self, flag: Operand) -> Operand {
        let value = self.temp(Types::I32);
        let cast = Rvalue::Cast {
            value: flag,
            to: Types::I32,
            checked: false,
        };
        self.assign(Place::local(value), cast);
        Operand::Copy(Place::local(value))
    }

    /// `left op right` on two `i32`s, wrapping.
    fn arithmetic(&mut self, op: BinOp, left: Operand, right: Operand) -> Operand {
        let value = self.temp(Types::I32);
        let rvalue = Rvalue::Binary {
            op,
            left,
            right,
            checked: false,
        };
        self.assign(Place::local(value), rvalue);
        Operand::Copy(Place::local(value))
    }

    /// The bits a hash map hashes for the key at `key`: the key as a `u64`,
    /// or for a tuple or struct the bits of its parts mixed in turn.
    fn hash_bits(&mut self, key: Place, ty: Ty) -> Operand {
        let Some(parts) = self.parts_of(ty) else {
            let bits = self.temp(Types::U64);
            let value = Operand::Copy(key);
            let cast = Rvalue::Cast {
                value,
                to: Types::U64,
                checked: false,
            };
            self.assign(Place::local(bits), cast);
            return Operand::Move(Place::local(bits));
        };
        let mut bits = constant(0, Types::U64);
        for (at, part) in parts.into_iter().enumerate() {
            let part_bits = self.hash_bits(key.clone().project(Projection::Field(at as u32)), part);
            let mixed = self.temp(Types::U64);
            let spread = Rvalue::Binary {
                op: BinOp::MulWrap,
                left: bits,
                right: constant(0x100000001b3, Types::U64),
                checked: false,
            };
            self.assign(Place::local(mixed), spread);
            let next = self.temp(Types::U64);
            let joined = Rvalue::Binary {
                op: BinOp::BitXor,
                left: Operand::Move(Place::local(mixed)),
                right: part_bits,
                checked: false,
            };
            self.assign(Place::local(next), joined);
            bits = Operand::Move(Place::local(next));
        }
        bits
    }

    fn binary(&mut self, op: BinOp, left: Operand, right: Operand) -> Operand {
        let result = self.temp(Types::BOOL);
        self.assign(
            Place::local(result),
            Rvalue::Binary {
                op,
                left,
                right,
                checked: false,
            },
        );
        Operand::Move(Place::local(result))
    }

    fn constant(&self, expr: &Expr) -> Option<Constant> {
        let value = match &expr.kind {
            ExprKind::Int(value) => ConstValue::Int(*value),
            ExprKind::Float(value) => ConstValue::Float(*value),
            ExprKind::Bool(value) => ConstValue::Bool(*value),
            ExprKind::Str(bytes) => ConstValue::Str(bytes.clone()),
            ExprKind::Variant(index, _) if self.is_plain_enum(expr.ty) => {
                ConstValue::Int(i128::from(*index))
            }
            _ => return None,
        };
        Some(Constant { value, ty: expr.ty })
    }

    fn is_plain_enum(&self, ty: Ty) -> bool {
        self.program
            .enums
            .get(&ty)
            .is_some_and(|e| e.variants.iter().all(Vec::is_empty))
    }

    /// The value of `expr`, a constant or a temporary.
    fn as_operand(&mut self, expr: &Expr) -> Operand {
        match self.constant(expr) {
            Some(constant) => Operand::Constant(constant),
            None => Operand::Move(Place::local(self.as_temp(expr))),
        }
    }

    /// `expr` evaluated into a new local, which the current statement drops
    /// if it holds a value of its own.
    fn as_temp(&mut self, expr: &Expr) -> Local {
        let temp = self.temp(expr.ty);
        self.expr_into(Some(Place::local(temp)), expr);
        if !matches!(expr.kind, ExprKind::Read(_)) && self.program.needs_drop(expr.ty) {
            self.temps.push((temp, true));
        }
        temp
    }

    /// `expr` evaluated for what it does; a value it owns is dropped as the
    /// statement ends.
    fn effect(&mut self, expr: &Expr) {
        match expr.ty == Types::VOID {
            true => self.expr_into(None, expr),
            false => {
                self.as_temp(expr);
            }
        }
    }

    /// Evaluate `expr` and store its value at `dest`, which is none only for
    /// an expression with no value.
    fn expr_into(&mut self, dest: Option<Place>, expr: &Expr) {
        let outer = replace(&mut self.span, expr.span);
        self.value_into(dest, expr);
        self.span = outer;
    }

    fn value_into(&mut self, dest: Option<Place>, expr: &Expr) {
        if let Some(constant) = self.constant(expr) {
            if let Some(dest) = dest {
                self.assign(dest, Rvalue::Use(Operand::Constant(constant)));
            }
            return;
        }
        let rvalue = match &expr.kind {
            ExprKind::Read(place) => Rvalue::Use(Operand::Copy(self.as_place(place))),
            ExprKind::Move(place) => Rvalue::Use(Operand::Move(self.as_place(place))),
            ExprKind::Copy(place) => Rvalue::DeepCopy(self.as_place(place)),
            ExprKind::AddressOf(place) => Rvalue::AddressOf(self.as_place(place)),
            ExprKind::Unary(op, value) => {
                let operand = self.as_operand(value);
                match (op, self.types().as_int(expr.ty)) {
                    (UnOp::Neg, Some(_)) => Rvalue::Binary {
                        op: BinOp::Sub,
                        left: constant(0, expr.ty),
                        right: operand,
                        checked: self.checks,
                    },
                    _ => Rvalue::Unary(*op, operand),
                }
            }
            ExprKind::Binary(op, left, right) if self.parts_of(left.ty).is_some() => {
                let (l, r) = (self.as_temp(left), self.as_temp(right));
                let order = self.tuple_order(Place::local(l), Place::local(r), left.ty);
                Rvalue::Binary {
                    op: *op,
                    left: order,
                    right: constant(0, Types::I32),
                    checked: false,
                }
            }
            ExprKind::Binary(op, left, right) => Rvalue::Binary {
                op: *op,
                checked: self.checked(*op, left.ty),
                left: self.as_operand(left),
                right: self.as_operand(right),
            },
            ExprKind::And(left, right) => return self.short_circuit(dest, left, right, true),
            ExprKind::Or(left, right) => return self.short_circuit(dest, left, right, false),
            ExprKind::Cast(value) => Rvalue::Cast {
                checked: self.checks
                    && self.types().is_float(value.ty)
                    && self.types().as_int(expr.ty).is_some(),
                value: self.as_operand(value),
                to: expr.ty,
            },
            ExprKind::Call(function, args) => return self.call(*function, args, dest),
            ExprKind::Builtin(builtin, args) => return self.builtin(dest, *builtin, args),
            ExprKind::Tuple(values) => self.aggregate(Aggregate::Tuple, values),
            ExprKind::Struct(fields) => {
                let mut values: Vec<(u32, Operand)> = fields
                    .iter()
                    .map(|(at, value)| (*at, self.as_operand(value)))
                    .collect();
                values.sort_by_key(|(at, _)| *at);
                Rvalue::Aggregate(
                    Aggregate::Tuple,
                    values.into_iter().map(|(_, value)| value).collect(),
                )
            }
            ExprKind::Array(values) => self.aggregate(Aggregate::Array, values),
            ExprKind::VecLiteral(values) => self.aggregate(Aggregate::Vec, values),
            ExprKind::Variant(index, values) => self.aggregate(Aggregate::Variant(*index), values),
            ExprKind::Some(value) => self.aggregate(Aggregate::Some, std::slice::from_ref(value)),
            ExprKind::Ok(value) => self.aggregate(Aggregate::Ok, std::slice::from_ref(value)),
            ExprKind::Err(value) => self.aggregate(Aggregate::Err, std::slice::from_ref(value)),
            ExprKind::None => Rvalue::Aggregate(Aggregate::None, Vec::new()),
            ExprKind::ArrayRepeat(value, count) => Rvalue::Repeat(self.as_operand(value), *count),
            ExprKind::VecRepeat(value, count) => {
                let count = self.as_operand(count);
                let value = match self.program.needs_drop(value.ty) {
                    true => Operand::Copy(Place::local(self.as_temp(value))),
                    false => self.as_operand(value),
                };
                Rvalue::VecRepeat(value, count)
            }
            ExprKind::Assign {
                place,
                value,
                drop_old,
            } => return self.assign_expr(place, value, *drop_old),
            ExprKind::CompoundAssign { place, op, value } => {
                let value = self.as_operand(value);
                let target = self.as_place(place);
                let rvalue = Rvalue::Binary {
                    op: *op,
                    checked: self.checked(*op, self.place_ty(&target)),
                    left: Operand::Copy(target.clone()),
                    right: value,
                };
                return self.assign(target, rvalue);
            }
            ExprKind::Match { scrutinee, arms } => return self.match_expr(dest, scrutinee, arms),
            ExprKind::Try(value) => return self.try_expr(dest, value),
            ExprKind::Block(block) => return self.block(block),
            ExprKind::Print {
                stderr,
                newline,
                pieces,
                values,
            } => {
                let values = values.iter().map(|value| self.as_operand(value)).collect();
                return self.push(StatementKind::Print {
                    stderr: *stderr,
                    newline: *newline,
                    pieces: pieces.clone(),
                    values,
                });
            }
            ExprKind::Assert { cond, message } => {
                if self.checks {
                    self.assert_holds(cond, message.as_deref());
                }
                return;
            }
            ExprKind::Unpack { target, value } => return self.unpack(target, value),
            ExprKind::Asm(asm) => {
                let inputs = asm
                    .inputs
                    .iter()
                    .map(|input| self.as_operand(input))
                    .collect();
                let outputs = asm
                    .outputs
                    .iter()
                    .map(|place| self.as_place(place))
                    .collect();
                return self.push(StatementKind::Asm(Box::new(Asm {
                    template: asm.template.clone(),
                    constraints: asm.constraints.clone(),
                    outputs,
                    inputs,
                    volatile: asm.volatile,
                })));
            }
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Void => unreachable!("a constant"),
        };
        if let Some(dest) = dest {
            self.assign(dest, rvalue);
        }
    }

    /// Whether `op` on a `ty` is checked: integer arithmetic that can
    /// overflow, divide by zero or shift too far, in a build with checks.
    fn checked(&self, op: BinOp, ty: Ty) -> bool {
        self.checks
            && self.types().as_int(ty).is_some()
            && matches!(
                op,
                BinOp::Add
                    | BinOp::Sub
                    | BinOp::Mul
                    | BinOp::Div
                    | BinOp::Rem
                    | BinOp::Shl
                    | BinOp::Shr
            )
    }

    fn aggregate(&mut self, kind: Aggregate, values: &[Expr]) -> Rvalue {
        let values = values.iter().map(|value| self.as_operand(value)).collect();
        Rvalue::Aggregate(kind, values)
    }

    fn call(&mut self, function: FnId, args: &[Expr], dest: Option<Place>) {
        let args = args.iter().map(|arg| self.as_operand(arg)).collect();
        self.then(|target| TerminatorKind::Call {
            function,
            args,
            dest,
            target,
        });
    }

    /// `place = value`: the value first, then the place, then the old value
    /// dropped if `drop_old` says so. A value read from a place is read
    /// where it is stored when nothing the target evaluates can change it.
    fn assign_expr(&mut self, place: &tir::Place, value: &Expr, drop_old: bool) {
        let value = match &value.kind {
            ExprKind::Read(source) if !drop_old && plain(place) => {
                let outer = replace(&mut self.span, value.span);
                let source = self.as_place(source);
                self.span = outer;
                Operand::Copy(source)
            }
            _ => self.as_operand(value),
        };
        let target = self.as_place(place);
        if drop_old {
            self.drop(target.clone());
        }
        self.assign(target, Rvalue::Use(value));
    }

    fn short_circuit(&mut self, dest: Option<Place>, left: &Expr, right: &Expr, is_and: bool) {
        let dest = dest.expect("a condition has a value");
        self.expr_into(Some(dest.clone()), left);
        let rhs = self.new_block();
        let merge = self.new_block();
        let (on_false, on_true) = match is_and {
            true => (merge, rhs),
            false => (rhs, merge),
        };
        self.switch(Operand::Copy(dest.clone()), vec![(0, on_false)], on_true);
        self.current = Some(rhs);
        self.expr_into(Some(dest), right);
        self.goto(merge);
        self.current = Some(merge);
    }

    /// `match`: a switch on the tag, or with a guard or a range, a test of
    /// each arm in turn, a failed guard going on to the next. Each binding
    /// is a copy of plain data or else a reference into the scrutinee, which
    /// each alternative of its arm sets as it enters. A temporary scrutinee
    /// that owns memory gives the bindings of an arm with no guard their
    /// values instead: an alternative that binds one takes the whole value,
    /// copies out the bound parts and drops the others.
    fn match_expr(&mut self, dest: Option<Place>, scrutinee: &tir::Place, arms: &[Arm]) {
        let place = self.as_place(scrutinee);
        let subject = self.place_ty(&place);
        let take = matches!(scrutinee, tir::Place::Temp(_)) && self.program.needs_drop(subject);
        let tag_ty = match self.types().kind(subject) {
            TyKind::Optional(_) | TyKind::Result(..) => Types::BOOL,
            TyKind::Adt(..) => Types::I32,
            _ => subject,
        };
        let tag = self.temp(tag_ty);
        self.assign(Place::local(tag), Rvalue::Discriminant(place.clone()));
        let merge = self.new_block();
        let unmatched = self.new_block();
        self.blocks[unmatched.0 as usize].1 = Some(Terminator {
            kind: TerminatorKind::Unreachable,
            span: self.span,
        });
        let chained = arms
            .iter()
            .any(|arm| arm.guard.is_some() || arm.tags.iter().any(|(low, high)| low != high));
        let mut entries = Vec::new();
        for arm in arms {
            let body = self.new_block();
            let start = match arm.guard {
                Some(_) => self.new_block(),
                None => body,
            };
            let take = take && arm.guard.is_none();
            let locals: Vec<(Local, bool)> = arm
                .bindings
                .iter()
                .map(|(local, _)| self.binding(*local, take))
                .collect();
            let alternatives: Vec<(Option<(i128, i128)>, BlockId)> = match arm.tags.is_empty() {
                true => vec![None],
                false => arm.tags.iter().copied().map(Some).collect(),
            }
            .into_iter()
            .map(|tags| match locals.is_empty() {
                true => (tags, start),
                false => (tags, self.new_block()),
            })
            .collect();
            entries.push((body, start, take, locals, alternatives));
        }
        let tests: Vec<BlockId> = match chained {
            true => arms
                .iter()
                .map(|_| self.new_block())
                .chain([unmatched])
                .collect(),
            false => Vec::new(),
        };
        match chained {
            true => {
                self.goto(tests[0]);
                let read = Operand::Copy(Place::local(tag));
                for (at, (_, _, _, _, alternatives)) in entries.iter().enumerate() {
                    self.current = Some(tests[at]);
                    for &(tags, entry) in alternatives {
                        let Some((low, high)) = tags else {
                            self.goto(entry);
                            break;
                        };
                        let inside = match low == high {
                            true => self.binary(BinOp::Eq, read.clone(), constant(low, tag_ty)),
                            false => {
                                let above =
                                    self.binary(BinOp::Ge, read.clone(), constant(low, tag_ty));
                                let below =
                                    self.binary(BinOp::Le, read.clone(), constant(high, tag_ty));
                                self.binary(BinOp::BitAnd, above, below)
                            }
                        };
                        let next = self.new_block();
                        self.switch(inside, vec![(0, next)], entry);
                        self.current = Some(next);
                    }
                    self.goto(tests[at + 1]);
                }
            }
            false => {
                let mut targets = Vec::new();
                let mut otherwise = unmatched;
                for (_, _, _, _, alternatives) in &entries {
                    for &(tags, entry) in alternatives {
                        match tags {
                            Some((value, _)) => targets.push((value, entry)),
                            None => otherwise = entry,
                        }
                    }
                }
                self.switch(Operand::Move(Place::local(tag)), targets, otherwise);
            }
        }
        for (at_arm, (arm, (body, start, take, locals, alternatives))) in
            arms.iter().zip(entries).enumerate()
        {
            let outer = replace(&mut self.span, arm.span);
            for (at, (tags, entry)) in alternatives
                .into_iter()
                .enumerate()
                .filter(|(_, (_, e))| *e != start)
            {
                self.current = Some(entry);
                let mut from = place.clone();
                let owned = |this: &Self, (binding, copy): (Local, bool)| {
                    copy && this.program.needs_drop(this.locals[binding.0 as usize].ty)
                };
                if take && locals.iter().any(|&local| owned(self, local)) {
                    let taken = self.temp(subject);
                    self.assign(
                        Place::local(taken),
                        Rvalue::Use(Operand::Move(place.clone())),
                    );
                    from = Place::local(taken);
                    let bound: Vec<Payload> = arm.bindings.iter().map(|(_, p)| p[at]).collect();
                    let variant = tags.map_or(0, |(value, _)| value);
                    for part in self.payloads(subject, variant) {
                        let reached = payload(&from, part);
                        if !bound.contains(&part)
                            && self.program.needs_drop(self.place_ty(&reached))
                        {
                            self.drop(reached);
                        }
                    }
                }
                for ((_, payloads), &(binding, copy)) in arm.bindings.iter().zip(&locals) {
                    let reached = payload(&from, payloads[at]);
                    let value = match copy {
                        true => Rvalue::Use(Operand::Copy(reached)),
                        false => Rvalue::AddressOf(reached),
                    };
                    self.assign(Place::local(binding), value);
                }
                self.goto(start);
            }
            if let Some(guard) = &arm.guard {
                self.current = Some(start);
                let holds = self.condition(guard);
                self.switch(holds, vec![(0, tests[at_arm + 1])], body);
            }
            self.span = outer;
            self.current = Some(body);
            self.scope(|this| {
                for &(local, copy) in &locals {
                    if copy {
                        this.own(local);
                    }
                }
                this.expr_into(dest.clone(), &arm.body);
            });
            self.goto(merge);
        }
        self.current = Some(merge);
    }

    /// The local a `match` binding is, named as it, and whether it holds a
    /// copy: one the checker did not make a view and that refers to
    /// nothing, or with `take`, one that owns what it holds. Otherwise it is
    /// a reference the binding reads its value through.
    fn binding(&mut self, local: tir::LocalId, take: bool) -> (Local, bool) {
        let decl = &self.function.locals[local.0 as usize];
        let name = Some(decl.name.clone()).filter(|name| !name.is_empty());
        let owned = take && self.program.needs_drop(decl.ty);
        if !decl.view && (owned || !self.program.reaches(decl.ty, Part::Ref, &mut Vec::new())) {
            let copy = self.local(decl.ty, name);
            self.map[local.0 as usize] = Some(copy);
            return (copy, true);
        }
        let pointer = self.program.types.intern(TyKind::Ref(decl.ty));
        let mir = self.local(pointer, name);
        self.views[local.0 as usize] = Some(Place::local(mir).project(Projection::Deref));
        (mir, false)
    }

    /// Where the parts of a value of `ty` with tag `tag` lie.
    fn payloads(&self, ty: Ty, tag: i128) -> Vec<Payload> {
        match self.types().kind(ty) {
            TyKind::Optional(_) if tag == 1 => vec![Payload::SomeValue],
            TyKind::Result(..) if tag == 1 => vec![Payload::OkValue],
            TyKind::Result(..) => vec![Payload::ErrValue],
            TyKind::Adt(..) => (0..self.program.enums[&ty].variants[tag as usize].len())
                .map(|field| Payload::Variant {
                    variant: tag as u32,
                    field: field as u32,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// `value?` or `value!`: the payload, or else the None or the error
    /// returned, dropping everything the function holds.
    fn try_expr(&mut self, dest: Option<Place>, value: &Expr) {
        let held = self.temp(value.ty);
        self.expr_into(Some(Place::local(held)), value);
        let held = Place::local(held);
        let tag = self.temp(Types::BOOL);
        self.assign(Place::local(tag), Rvalue::Discriminant(held.clone()));
        let ok = self.new_block();
        let failed = self.new_block();
        self.switch(Operand::Move(Place::local(tag)), vec![(0, failed)], ok);
        self.current = Some(failed);
        let failure = match self.types().kind(value.ty) {
            TyKind::Result(..) => Rvalue::Aggregate(
                Aggregate::Err,
                vec![Operand::Copy(held.clone().project(Projection::Field(2)))],
            ),
            _ => Rvalue::Aggregate(Aggregate::None, Vec::new()),
        };
        self.assign(Place::local(RETURN), failure);
        self.exit(0, 0);
        self.terminate(TerminatorKind::Return);
        self.current = Some(ok);
        if let Some(dest) = dest {
            let payload = held.project(Projection::Field(1));
            let ty = self.place_ty(&payload);
            let value = match self.program.needs_drop(ty) {
                true => Operand::Move(payload),
                false => Operand::Copy(payload),
            };
            self.assign(dest, Rvalue::Use(value));
        }
    }

    /// The place a builtin works on, and whether it takes the value away.
    /// One `borrowed` while the other arguments are evaluated is reached
    /// through a reference taken first.
    fn receiver(&mut self, arg: &Expr, borrowed: bool) -> (Place, bool) {
        let outer = replace(&mut self.span, arg.span);
        let receiver = match &arg.kind {
            ExprKind::AddressOf(place) if borrowed => {
                let place = self.as_place(place);
                let pointer = self.temp(arg.ty);
                self.span = outer;
                self.assign(Place::local(pointer), Rvalue::AddressOf(place));
                (Place::local(pointer).project(Projection::Deref), false)
            }
            ExprKind::AddressOf(place) | ExprKind::Read(place) => (self.as_place(place), false),
            ExprKind::Move(place) => (self.as_place(place), true),
            _ => (Place::local(self.as_temp(arg)), true),
        };
        self.span = outer;
        receiver
    }

    fn builtin(&mut self, dest: Option<Place>, builtin: Builtin, args: &[Expr]) {
        let operand = |this: &mut Self, at: usize| this.as_operand(&args[at]);
        let rvalue = match builtin {
            Builtin::Flush => return self.push(StatementKind::Flush),
            Builtin::CopyBytes => {
                let [to, from, count] = [0, 1, 2].map(|at| operand(self, at));
                return self.push(StatementKind::CopyBytes(to, from, count));
            }
            Builtin::VecNew => Rvalue::VecNew(None),
            Builtin::VecWithCapacity => Rvalue::VecNew(Some(operand(self, 0))),
            _ => {
                let (held, moved) = self.receiver(&args[0], args.len() > 1);
                match builtin {
                    Builtin::Len => Rvalue::Len(held),
                    Builtin::VecCapacity => Rvalue::Capacity(held),
                    Builtin::VecIsEmpty => {
                        let len = self.temp(Types::USIZE);
                        self.assign(Place::local(len), Rvalue::Len(held));
                        Rvalue::Binary {
                            op: BinOp::Eq,
                            left: Operand::Move(Place::local(len)),
                            right: constant(0, Types::USIZE),
                            checked: false,
                        }
                    }
                    Builtin::IsSome | Builtin::IsOk => Rvalue::Discriminant(held),
                    Builtin::IsNone | Builtin::IsErr => {
                        let tag = self.temp(Types::BOOL);
                        self.assign(Place::local(tag), Rvalue::Discriminant(held));
                        Rvalue::Unary(UnOp::Not, Operand::Move(Place::local(tag)))
                    }
                    Builtin::Unwrap | Builtin::UnwrapErr => self.unwrap(builtin, held, moved),
                    Builtin::VecPush => {
                        let item = operand(self, 1);
                        return self.push(StatementKind::VecPush(held, item));
                    }
                    Builtin::VecInsert => {
                        let at = operand(self, 1);
                        let item = operand(self, 2);
                        self.check_index(&held, &at, BinOp::Le);
                        return self.push(StatementKind::VecInsert(held, at, item));
                    }
                    Builtin::VecRemove => {
                        let at = operand(self, 1);
                        self.check_index(&held, &at, BinOp::Lt);
                        Rvalue::VecRemove(held, at)
                    }
                    Builtin::VecPop => Rvalue::VecPop(held),
                    Builtin::VecGet => Rvalue::VecGet(held, operand(self, 1)),
                    Builtin::VecClear => return self.push(StatementKind::VecClear(held)),
                    Builtin::VecReserve => {
                        let more = operand(self, 1);
                        return self.push(StatementKind::VecReserve(held, more));
                    }
                    Builtin::VecShrinkToFit => {
                        return self.push(StatementKind::VecShrinkToFit(held));
                    }
                    Builtin::Slice => {
                        let mutable = dest.as_ref().is_some_and(|dest| {
                            matches!(self.types().kind(self.place_ty(dest)), TyKind::SliceMut(_))
                        });
                        self.slice(held, args, mutable)
                    }
                    Builtin::HashBits => Rvalue::Use(self.hash_bits(held, args[0].ty)),
                    Builtin::Replace => {
                        let new = operand(self, 1);
                        let old = self.temp(self.place_ty(&held));
                        self.assign(Place::local(old), Rvalue::Take(held.clone()));
                        self.assign(held, Rvalue::Use(new));
                        Rvalue::Use(Operand::Move(Place::local(old)))
                    }
                    Builtin::Flush
                    | Builtin::CopyBytes
                    | Builtin::VecNew
                    | Builtin::VecWithCapacity => unreachable!("handled above"),
                }
            }
        };
        if let Some(dest) = dest {
            self.assign(dest, rvalue);
        }
    }

    /// The elements of `held` from `args[1]` up to `args[2]`, or to its end
    /// when there is none, checked to lie within it; `mutable` for a slice
    /// that writes them.
    fn slice(&mut self, held: Place, args: &[Expr], mutable: bool) -> Rvalue {
        let len = self.temp(Types::USIZE);
        self.assign(Place::local(len), Rvalue::Len(held.clone()));
        let from = self.as_temp(&args[1]);
        let to = match args.get(2) {
            Some(end) => self.as_temp(end),
            None => len,
        };
        if self.checks {
            let ordered = self.binary(
                BinOp::Le,
                Operand::Copy(Place::local(from)),
                Operand::Copy(Place::local(to)),
            );
            self.assert(ordered, Panic::Bounds);
            let inside = self.binary(
                BinOp::Le,
                Operand::Copy(Place::local(to)),
                Operand::Copy(Place::local(len)),
            );
            self.assert(inside, Panic::Bounds);
        }
        let first = held.project(Projection::Index(from));
        let elem = self.place_ty(&first);
        let pointer = match mutable {
            true => self.program.types.intern(TyKind::RefMut(elem)),
            false => self.program.types.intern(TyKind::Ref(elem)),
        };
        let data = self.temp(pointer);
        self.assign(Place::local(data), Rvalue::AddressOf(first));
        let count = self.temp(Types::USIZE);
        self.assign(
            Place::local(count),
            Rvalue::Binary {
                op: BinOp::Sub,
                left: Operand::Copy(Place::local(to)),
                right: Operand::Copy(Place::local(from)),
                checked: false,
            },
        );
        Rvalue::Aggregate(
            Aggregate::Tuple,
            vec![
                Operand::Move(Place::local(data)),
                Operand::Move(Place::local(count)),
            ],
        )
    }

    /// `unwrap()` or `unwrap_err()` of the `T?` or `T!` at `held`, checked.
    fn unwrap(&mut self, builtin: Builtin, held: Place, moved: bool) -> Rvalue {
        if self.checks {
            let tag = self.temp(Types::BOOL);
            self.assign(Place::local(tag), Rvalue::Discriminant(held.clone()));
            let panic = match (builtin, self.types().kind(self.place_ty(&held))) {
                (Builtin::UnwrapErr, _) => Panic::UnwrapErr,
                (_, TyKind::Optional(_)) => Panic::UnwrapNone,
                _ => Panic::Unwrap,
            };
            let mut cond = Operand::Move(Place::local(tag));
            if panic == Panic::UnwrapErr {
                let not = self.temp(Types::BOOL);
                self.assign(Place::local(not), Rvalue::Unary(UnOp::Not, cond));
                cond = Operand::Move(Place::local(not));
            }
            self.assert(cond, panic);
        }
        let field = match builtin {
            Builtin::UnwrapErr => 2,
            _ => 1,
        };
        let payload = held.project(Projection::Field(field));
        Rvalue::Use(match moved {
            true => Operand::Move(payload),
            false => Operand::Copy(payload),
        })
    }

    /// Check `at op len` of the Vec at `vec`, `Lt` for an element and `Le`
    /// for a position to insert at.
    fn check_index(&mut self, vec: &Place, at: &Operand, op: BinOp) {
        if !self.checks {
            return;
        }
        let len = self.temp(Types::USIZE);
        self.assign(Place::local(len), Rvalue::Len(vec.clone()));
        let inside = self.binary(op, reused(at.clone()), Operand::Move(Place::local(len)));
        self.assert(inside, Panic::Bounds);
    }
}

/// Where `payload` lies inside the scrutinee at `place`.
fn payload(place: &Place, payload: Payload) -> Place {
    match payload {
        Payload::Whole => place.clone(),
        Payload::SomeValue | Payload::OkValue => place.clone().project(Projection::Field(1)),
        Payload::ErrValue => place.clone().project(Projection::Field(2)),
        Payload::Variant { variant, field } => place
            .clone()
            .project(Projection::Downcast(variant))
            .project(Projection::Field(field)),
    }
}

/// Whether evaluating `place` runs nothing that could change another place.
fn plain(place: &tir::Place) -> bool {
    match place {
        tir::Place::Local(_) => true,
        tir::Place::Field(base, _) => plain(base),
        tir::Place::Index(base, index) => plain(base) && simple(index),
        tir::Place::Deref(pointer) => simple(pointer),
        tir::Place::Temp(_) => false,
    }
}

/// Whether `expr` is a constant or a read of a plain place.
fn simple(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Read(place) => plain(place),
        kind => matches!(kind, ExprKind::Int(_)),
    }
}

fn constant(value: i128, ty: Ty) -> Operand {
    Operand::Constant(Constant {
        value: ConstValue::Int(value),
        ty,
    })
}

/// An operand used more than once: a temporary is read, not moved.
fn reused(operand: Operand) -> Operand {
    match operand {
        Operand::Move(place) => Operand::Copy(place),
        other => other,
    }
}
