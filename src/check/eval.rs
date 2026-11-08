//! Running a global constant's value at compile time, so a function may
//! compute it. The functions it calls run over their typed IR, on plain
//! data only: numbers, bools, text, and arrays, tuples, structs, enums,
//! optionals and results of them, reached directly, through references or
//! through slices. Each `drop` runs where the program would run it. What
//! needs the heap, a pointer, assembly or output stops it with an error.

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::errors::{Span, ZeruError};
use crate::items::{DefId, ItemKind};
use crate::tir::{
    self, BinOp, BitOp, Builtin, Expr, ExprKind, FnId, LocalId, Payload, Place, Stmt, StmtKind,
    UnOp, Unpack,
};
use crate::ty::{IntTy, Ty, TyKind, Types};

use super::Checker;
use super::consts::{arith, wrap};

/// How many statements and loop turns a constant may take, and how deep its
/// calls may go, before it is taken to never end.
const MAX_STEPS: u64 = 10_000_000;
const MAX_DEPTH: usize = 256;

#[derive(Clone, Debug)]
enum Value {
    Void,
    Int(i128),
    Float(f64),
    Bool(bool),
    Text(Rc<[u8]>),
    Parts(Vec<Value>),
    Variant(u32, Vec<Value>),
    Optional(Option<Box<Value>>),
    Outcome(bool, Box<Value>),
    Ref(Loc),
    Slice(Loc, usize, usize),
}

/// Where a value lives: a slot of a frame, and the parts followed from it.
#[derive(Clone, Debug, PartialEq)]
struct Loc {
    frame: usize,
    slot: usize,
    path: Vec<usize>,
}

/// The elements of a sequence: bytes of text, or `len` elements of the
/// array at a place from `start` on.
enum Seq {
    Text(Rc<[u8]>),
    Elems(Loc, usize, usize),
}

impl Seq {
    fn len(&self) -> usize {
        match self {
            Seq::Text(bytes) => bytes.len(),
            Seq::Elems(_, _, len) => *len,
        }
    }

    /// Element `at`: a place in the array, or a byte of text.
    fn element(&self, at: usize) -> Element {
        match self {
            Seq::Text(bytes) => Element::Value(Value::Int(i128::from(bytes[at]))),
            Seq::Elems(base, start, _) => {
                let mut loc = base.clone();
                loc.path.push(start + at);
                Element::Place(loc)
            }
        }
    }
}

/// Why evaluation stopped short: a jump out of a loop or a function, or a
/// failure with its message and where.
enum Exit {
    Break,
    Continue,
    Return(Value),
    Fail(String, Span),
    Broken,
}

type Run<T> = Result<T, Exit>;

/// What a function body needs to run: its locals, its parameters and its
/// statements.
struct Body {
    locals: Vec<tir::Local>,
    params: Vec<LocalId>,
    stmts: Vec<Stmt>,
}

/// A running call: its slots, those of them that own a value with a `drop`
/// for their scope or their statement to drop, in the order they got it,
/// those whose value was moved away, and the statement it is at.
struct Frame {
    slots: Vec<Option<Value>>,
    body: Rc<Body>,
    owned: Vec<(usize, Ty)>,
    temps: Vec<(usize, Ty)>,
    moved: HashSet<usize>,
    at: Span,
}

impl Frame {
    fn new(body: Rc<Body>, at: Span) -> Frame {
        Frame {
            slots: vec![None; body.locals.len()],
            body,
            owned: Vec::new(),
            temps: Vec::new(),
            moved: HashSet::new(),
            at,
        }
    }
}

struct Eval<'c, 'a> {
    cx: &'c mut Checker<'a>,
    bodies: HashMap<FnId, Rc<Body>>,
    frames: Vec<Frame>,
    steps: u64,
    drops: HashMap<Ty, bool>,
    exit: DefId,
    swap: DefId,
}

impl Checker<'_> {
    /// The value of `expr`, the definition of the constant `name` declared
    /// at `span`, run now and written as literals. `locals` are those the
    /// definition's own `match` declares.
    pub(super) fn run_constant(
        &mut self,
        expr: &Expr,
        locals: Vec<tir::Local>,
        name: &str,
        span: Span,
    ) -> Option<Expr> {
        let body = Rc::new(Body {
            locals,
            params: Vec::new(),
            stmts: Vec::new(),
        });
        let mut eval = Eval {
            exit: self.prelude_def("exit"),
            swap: self.prelude_def("__zeru_vec_swap"),
            cx: self,
            bodies: HashMap::new(),
            frames: vec![Frame::new(body, span)],
            steps: 0,
            drops: HashMap::new(),
        };
        let value = eval.expr(expr);
        let ended = match value {
            Ok(value) if refers(&value) => cannot(
                "keep a reference or a slice: what it points at is gone once compiling ends",
                span,
            ),
            Ok(value) => eval.end_temps(0, Ok(())).map(|()| value),
            Err(exit) => Err(exit),
        };
        let value = match ended {
            Ok(value) => value,
            Err(Exit::Fail(message, at)) => {
                let mut error = ZeruError::semantic(message, at);
                if at != span {
                    error
                        .notes
                        .push((format!("while computing '{name}'"), span));
                }
                self.errors.push(error);
                return None;
            }
            Err(Exit::Broken) => return None,
            Err(_) => unreachable!("a jump stays inside its function"),
        };
        Some(self.literal(&value, expr.ty, expr.span))
    }

    /// `value`, of type `ty`, as the expression that writes it out.
    fn literal(&mut self, value: &Value, ty: Ty, span: Span) -> Expr {
        let kind = match (value, self.types.kind(ty).clone()) {
            (Value::Int(n), _) => ExprKind::Int(*n),
            (Value::Float(x), _) => ExprKind::Float(*x),
            (Value::Bool(b), _) => ExprKind::Bool(*b),
            (Value::Text(bytes), _) => ExprKind::Str(bytes.to_vec()),
            (Value::Parts(parts), TyKind::Tuple(tys)) => {
                ExprKind::Tuple(self.literals(parts, &tys, span))
            }
            (Value::Parts(parts), TyKind::Array(elem, _)) => {
                ExprKind::Array(self.literals(parts, &vec![elem; parts.len()], span))
            }
            (Value::Parts(parts), _) => {
                let tys: Vec<Ty> = self.fields_of(ty).into_iter().map(|(_, t)| t).collect();
                let values = self.literals(parts, &tys, span);
                ExprKind::Struct((0..).zip(values).collect())
            }
            (Value::Variant(at, fields), TyKind::Adt(def, args)) => {
                let tys = self.variants_of(def)[*at as usize].1.clone();
                let tys = self.types.subst_all(&tys, &args);
                ExprKind::Variant(*at, self.literals(fields, &tys, span))
            }
            (Value::Optional(None), _) => ExprKind::None,
            (Value::Optional(Some(inner)), TyKind::Optional(inner_ty)) => {
                ExprKind::Some(Box::new(self.literal(inner, inner_ty, span)))
            }
            (Value::Outcome(true, inner), TyKind::Result(inner_ty)) => {
                ExprKind::Ok(Box::new(self.literal(inner, inner_ty, span)))
            }
            (Value::Outcome(false, code), _) => {
                ExprKind::Err(Box::new(self.literal(code, Types::I32, span)))
            }
            _ => ExprKind::Void,
        };
        Expr { kind, ty, span }
    }

    fn literals(&mut self, values: &[Value], tys: &[Ty], span: Span) -> Vec<Expr> {
        values
            .iter()
            .zip(tys)
            .map(|(value, &ty)| self.literal(value, ty, span))
            .collect()
    }
}

fn fail<T>(message: impl Into<String>, span: Span) -> Run<T> {
    Err(Exit::Fail(message.into(), span))
}

/// `at` as an index into `len` values, or the error that it is outside them.
fn inside(at: i128, len: usize, span: Span) -> Run<usize> {
    if at < 0 || at >= len as i128 {
        return fail(
            format!("Index {at} is outside 0..{len} while computing a constant"),
            span,
        );
    }
    Ok(at as usize)
}

/// What a constant cannot do at compile time, as the error says it.
fn cannot<T>(what: &str, span: Span) -> Run<T> {
    fail(
        format!("Computing a constant while compiling cannot {what}"),
        span,
    )
}

/// Whether `value` holds a reference or a slice.
fn refers(value: &Value) -> bool {
    match value {
        Value::Ref(_) | Value::Slice(..) => true,
        Value::Parts(parts) | Value::Variant(_, parts) => parts.iter().any(refers),
        Value::Optional(Some(inner)) | Value::Outcome(_, inner) => refers(inner),
        _ => false,
    }
}

/// Whether evaluation stopped for good, so nothing more runs.
fn stopped<T>(result: &Run<T>) -> bool {
    matches!(result, Err(Exit::Fail(..) | Exit::Broken))
}

impl Eval<'_, '_> {
    fn types(&self) -> &Types {
        &self.cx.types
    }

    fn step(&mut self, span: Span) -> Run<()> {
        self.steps += 1;
        match self.steps > MAX_STEPS {
            true => fail(
                format!(
                    "A constant takes more than {MAX_STEPS} steps to compute; it may never end"
                ),
                span,
            ),
            false => Ok(()),
        }
    }

    /// The body of function instance `id`, checked now if it was not yet.
    fn body(&mut self, id: FnId) -> Rc<Body> {
        if let Some(body) = self.bodies.get(&id) {
            return body.clone();
        }
        self.cx.check_instance(id);
        let function = self.cx.functions[id.0 as usize].as_ref().unwrap();
        let body = Rc::new(Body {
            locals: function.locals.clone(),
            params: function.params.clone(),
            stmts: function.body.stmts.clone(),
        });
        self.bodies.insert(id, body.clone());
        body
    }

    /// Call `id`; a failure inside std is reported at the call from outside
    /// it that led there.
    fn call(&mut self, id: FnId, args: Vec<Value>, span: Span) -> Run<Value> {
        if self.frames.len() > MAX_DEPTH {
            return fail(
                format!("A constant's calls go more than {MAX_DEPTH} deep while computing it"),
                span,
            );
        }
        let def = self.cx.queued[id.0 as usize].def;
        if def == self.exit {
            return cannot("call exit(), which ends the program", span);
        }
        if def == self.swap {
            return self.swap(args, span);
        }
        let body = self.body(id);
        if self.cx.broken.contains(&id) {
            return Err(Exit::Broken);
        }
        let mut frame = Frame::new(body.clone(), span);
        for (&param, arg) in body.params.iter().zip(args) {
            frame.slots[param.0 as usize] = Some(arg);
        }
        self.frames.push(frame);
        for &param in &body.params {
            self.own(param);
        }
        let result = self.stmts(&body.stmts);
        let result = self.leave(0, result);
        self.frames.pop();
        match result {
            Ok(()) => Ok(Value::Void),
            Err(Exit::Return(value)) => Ok(value),
            Err(Exit::Fail(message, at))
                if self.cx.sources.in_std(at) && !self.cx.sources.in_std(span) =>
            {
                fail(message, span)
            }
            Err(other) => Err(other),
        }
    }

    /// `v.swap(i, j)`, which the prelude does on the bytes behind pointers:
    /// the two elements of the slice trade places.
    fn swap(&mut self, args: Vec<Value>, span: Span) -> Run<Value> {
        let mut args = args.into_iter();
        let (Some(seq), Some(Value::Int(i)), Some(Value::Int(j))) = (
            args.next().and_then(|slice| self.seq_of(slice)),
            args.next(),
            args.next(),
        ) else {
            unreachable!("a slice and two indices")
        };
        let (Element::Place(a), Element::Place(b)) = (
            seq.element(inside(i, seq.len(), span)?),
            seq.element(inside(j, seq.len(), span)?),
        ) else {
            unreachable!("a slice of elements")
        };
        let (first, second) = (self.get(&a), self.get(&b));
        self.set(&a, second);
        self.set(&b, first);
        Ok(Value::Void)
    }

    fn stmts(&mut self, stmts: &[Stmt]) -> Run<()> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    /// A block, whose locals are dropped as it ends and whose temporaries
    /// end with it.
    fn block(&mut self, block: &tir::Block) -> Run<()> {
        let frame = self.frames.last().expect("a frame runs");
        let (owned, slots) = (frame.owned.len(), frame.slots.len());
        let result = self.stmts(&block.stmts);
        let result = self.leave(owned, result);
        self.free(slots);
        result
    }

    /// A block that gives a value, its last statement.
    fn value_block(&mut self, block: &tir::Block) -> Run<Value> {
        let (last, rest) = block.stmts.split_last().expect("a block's value ends it");
        let StmtKind::Expr(last) = &last.kind else {
            unreachable!("a block's value is an expression")
        };
        let owned = self.frame().owned.len();
        let value = match self.stmts(rest) {
            Ok(()) => self.expr(last),
            Err(exit) => Err(exit),
        };
        self.leave_with(owned, value)
    }

    /// Free the slots from `slots` on, temporaries no longer used.
    fn free(&mut self, slots: usize) {
        let frame = self.frame();
        frame.slots.truncate(slots);
        frame.moved.retain(|&slot| slot < slots);
    }

    /// One statement; the temporaries it made are dropped as it ends.
    fn stmt(&mut self, stmt: &Stmt) -> Run<()> {
        self.step(stmt.span)?;
        let mark = self.frame().temps.len();
        self.frame().at = stmt.span;
        let result = self.run_stmt(stmt);
        self.end_temps(mark, result)
    }

    /// Drop the temporaries made from `mark` on, last made first.
    fn end_temps(&mut self, mark: usize, result: Run<()>) -> Run<()> {
        if stopped(&result) {
            return result;
        }
        let ending = self.frame().temps.split_off(mark);
        self.drop_all(ending)?;
        result
    }

    /// Leave the scope whose locals start at `mark` in `owned`: drop them,
    /// last made first.
    fn leave(&mut self, mark: usize, result: Run<()>) -> Run<()> {
        if stopped(&result) {
            return result;
        }
        let ending = self.frame().owned.split_off(mark);
        self.drop_all(ending)?;
        result
    }

    fn leave_with(&mut self, mark: usize, value: Run<Value>) -> Run<Value> {
        match value {
            Ok(value) => self.leave(mark, Ok(())).map(|()| value),
            Err(exit) => self.leave(mark, Err(exit)).map(|()| Value::Void),
        }
    }

    /// Drop the value in each slot of `ending` that was not moved away, last
    /// first.
    fn drop_all(&mut self, ending: Vec<(usize, Ty)>) -> Run<()> {
        let frame = self.frames.len() - 1;
        for (slot, ty) in ending.into_iter().rev() {
            if !self.frame().moved.remove(&slot) {
                let path = Vec::new();
                self.drop_at(Loc { frame, slot, path }, ty)?;
            }
        }
        Ok(())
    }

    fn needs_drop(&mut self, ty: Ty) -> bool {
        if let Some(&drops) = self.drops.get(&ty) {
            return drops;
        }
        let drops = self.cx.owns_heap(ty);
        self.drops.insert(ty, drops);
        drops
    }

    /// Let the current scope drop the value of `local` as it ends.
    fn own(&mut self, local: LocalId) {
        let at = local.0 as usize;
        let decl = &self.frames.last().expect("a frame runs").body.locals[at];
        let (view, ty) = (decl.view, decl.ty);
        if !view && self.needs_drop(ty) {
            self.frame().owned.push((at, ty));
        }
    }

    /// Run what dropping the value of type `ty` at `loc` runs: a struct's
    /// own `drop` first, then that of each part.
    fn drop_at(&mut self, loc: Loc, ty: Ty) -> Run<()> {
        if !self.needs_drop(ty) {
            return Ok(());
        }
        let at = self.frames.last().expect("a frame runs").at;
        if let TyKind::Adt(def, args) = self.types().kind(ty).clone()
            && self.cx.items.item(def).kind == ItemKind::Struct
            && let Some(method) = self.cx.items.method(def, "drop")
        {
            let id = self.cx.instance(method, args, at);
            self.call(id, vec![Value::Ref(loc.clone())], at)?;
        }
        for (index, part) in self.parts(&loc, ty).into_iter().enumerate() {
            let mut inner = loc.clone();
            inner.path.push(index);
            self.drop_at(inner, part)?;
        }
        Ok(())
    }

    /// Drop the parts of the value of type `ty` at `loc` that no binding in
    /// `bound` takes, as a `match` taking a temporary apart does.
    fn drop_except(&mut self, loc: Loc, ty: Ty, bound: &[Loc]) -> Run<()> {
        if bound.contains(&loc) {
            return Ok(());
        }
        let within = |b: &Loc| b.frame == loc.frame && b.slot == loc.slot;
        if !bound
            .iter()
            .any(|b| within(b) && b.path.starts_with(&loc.path))
        {
            return self.drop_at(loc, ty);
        }
        for (index, part) in self.parts(&loc, ty).into_iter().enumerate() {
            let mut inner = loc.clone();
            inner.path.push(index);
            self.drop_except(inner, part, bound)?;
        }
        Ok(())
    }

    /// The types of the parts the value of type `ty` at `loc` holds.
    fn parts(&mut self, loc: &Loc, ty: Ty) -> Vec<Ty> {
        match self.types().kind(ty).clone() {
            TyKind::Tuple(parts) => parts,
            TyKind::Array(elem, len) => vec![elem; len as usize],
            TyKind::Optional(inner) | TyKind::Result(inner) => match self.at(loc) {
                Value::Optional(Some(_)) | Value::Outcome(true, _) => vec![inner],
                _ => Vec::new(),
            },
            TyKind::Adt(def, args) if self.cx.items.item(def).kind == ItemKind::Enum => {
                let Value::Variant(at, _) = *self.at(loc) else {
                    unreachable!("an enum is one of its variants")
                };
                let fields = self.cx.variants_of(def)[at as usize].1.clone();
                self.cx.types.subst_all(&fields, &args)
            }
            _ => self
                .cx
                .fields_of(ty)
                .into_iter()
                .map(|(_, ty)| ty)
                .collect(),
        }
    }

    /// Note that a move took the value at `loc`, which `place` names: a
    /// local is not dropped then, and a temporary a field is moved out of
    /// has its other parts dropped now.
    fn give_up(&mut self, place: &Place, loc: &Loc) -> Run<()> {
        match (place, place.base()) {
            (Place::Local(local), _) if !self.is_view(*local) => {}
            (Place::Field(..), Place::Temp(temp)) => {
                let mut base = Loc {
                    path: Vec::new(),
                    ..loc.clone()
                };
                let mut ty = temp.ty;
                for &at in &loc.path {
                    let parts = self.cx.parts_of(ty).unwrap_or_default();
                    for (other, &part) in parts.iter().enumerate() {
                        if other != at {
                            let mut inner = base.clone();
                            inner.path.push(other);
                            self.drop_at(inner, part)?;
                        }
                    }
                    base.path.push(at);
                    ty = parts[at];
                }
            }
            _ => return Ok(()),
        }
        self.frames[loc.frame].moved.insert(loc.slot);
        Ok(())
    }

    fn run_stmt(&mut self, stmt: &Stmt) -> Run<()> {
        match &stmt.kind {
            StmtKind::Let(local, value) => {
                let mark = self.frame().temps.len();
                let held = self.expr(value)?;
                self.set_local(*local, held);
                for ty in value.borrowed_temps() {
                    let frame = self.frame();
                    if let Some(at) = frame.temps[mark..].iter().rposition(|t| t.1 == ty) {
                        let kept = frame.temps.remove(mark + at);
                        frame.owned.push(kept);
                    }
                }
                self.own(*local);
            }
            StmtKind::Expr(expr) => {
                let value = self.expr(expr)?;
                if !matches!(expr.kind, ExprKind::Read(_)) && self.needs_drop(expr.ty) {
                    let loc = self.temp(value);
                    self.frame().temps.push((loc.slot, expr.ty));
                }
            }
            StmtKind::Return(value) => {
                let value = match value {
                    Some(value) => self.expr(value)?,
                    None => Value::Void,
                };
                return Err(Exit::Return(value));
            }
            StmtKind::Break => return Err(Exit::Break),
            StmtKind::Continue => return Err(Exit::Continue),
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                if self.condition(cond)? {
                    self.block(then)?;
                } else if let Some(otherwise) = otherwise {
                    self.block(otherwise)?;
                }
            }
            StmtKind::While { cond, body } => {
                while self.condition(cond)? {
                    self.step(stmt.span)?;
                    if self.turn(body)? {
                        break;
                    }
                }
            }
            StmtKind::ForRange {
                var,
                start,
                end,
                inclusive,
                body,
            } => {
                let (first, last) = (self.int(start)?, self.int(end)?);
                for at in first..=last - i128::from(!*inclusive) {
                    self.step(stmt.span)?;
                    self.set_local(*var, Value::Int(at));
                    if self.turn(body)? {
                        break;
                    }
                }
            }
            StmtKind::ForEach {
                index, walks, body, ..
            } => {
                let mut seqs = Vec::new();
                for walk in walks {
                    let loc = self.loc(&walk.seq)?;
                    let Some(seq) = self.seq(&loc) else {
                        return cannot("walk a Vec: it lives on the heap", stmt.span);
                    };
                    seqs.push((walk, seq));
                }
                let count = seqs.iter().map(|(_, seq)| seq.len()).min().unwrap_or(0);
                for at in 0..count {
                    self.step(stmt.span)?;
                    if let Some(index) = index {
                        self.set_local(*index, Value::Int(at as i128));
                    }
                    for (walk, seq) in &seqs {
                        let element = seq.element(at);
                        self.bind(walk.var, element.clone());
                        if let Some(unpack) = &walk.unpack {
                            self.view_parts(unpack, element);
                        }
                    }
                    if self.turn(body)? {
                        break;
                    }
                }
            }
            StmtKind::Block(block) => self.block(block)?,
        }
        Ok(())
    }

    /// A condition, whose temporaries end once it is decided.
    fn condition(&mut self, cond: &Expr) -> Run<bool> {
        let frame = self.frames.last().expect("a frame runs");
        let (mark, slots) = (frame.temps.len(), frame.slots.len());
        let (truth, result) = match self.truth(cond) {
            Ok(truth) => (truth, Ok(())),
            Err(exit) => (false, Err(exit)),
        };
        self.end_temps(mark, result)?;
        self.free(slots);
        Ok(truth)
    }

    /// One turn of a loop: whether a `break` ends it.
    fn turn(&mut self, body: &tir::Block) -> Run<bool> {
        match self.block(body) {
            Ok(()) | Err(Exit::Continue) => Ok(false),
            Err(Exit::Break) => Ok(true),
            Err(other) => Err(other),
        }
    }

    /// Give `local` the element: a view of its place, or a copy.
    fn bind(&mut self, local: LocalId, element: Element) {
        let value = match element {
            Element::Value(value) => value,
            Element::Place(loc) if self.is_view(local) => Value::Ref(loc),
            Element::Place(loc) => self.get(&loc),
        };
        self.frame().slots[local.0 as usize] = Some(value);
    }

    fn view_parts(&mut self, unpack: &Unpack, element: Element) {
        match (unpack, element) {
            (Unpack::Declare(local), element) => self.bind(*local, element),
            (Unpack::Tuple(parts), Element::Place(loc)) => {
                for (at, part) in parts.iter().enumerate() {
                    let mut loc = loc.clone();
                    loc.path.push(at);
                    self.view_parts(part, Element::Place(loc));
                }
            }
            _ => {}
        }
    }

    fn frame(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("a frame runs")
    }

    fn is_view(&self, local: LocalId) -> bool {
        let frame = self.frames.last().expect("a frame runs");
        frame.body.locals[local.0 as usize].view
    }

    fn set_local(&mut self, local: LocalId, value: Value) {
        let frame = self.frame();
        frame.slots[local.0 as usize] = Some(value);
        frame.moved.remove(&(local.0 as usize));
    }

    /// A slot of the current frame for a value that has no place of its own.
    fn temp(&mut self, value: Value) -> Loc {
        let frame = self.frames.len() - 1;
        let slots = &mut self.frame().slots;
        slots.push(Some(value));
        Loc {
            frame,
            slot: slots.len() - 1,
            path: Vec::new(),
        }
    }

    fn get(&self, loc: &Loc) -> Value {
        self.at(loc).clone()
    }

    /// The value at `loc`, to look at without copying it.
    fn at(&self, loc: &Loc) -> &Value {
        let mut value = self.frames[loc.frame].slots[loc.slot]
            .as_ref()
            .expect("set before it is read");
        for &at in &loc.path {
            value = match value {
                Value::Parts(parts) | Value::Variant(_, parts) => &parts[at],
                Value::Optional(Some(inner)) | Value::Outcome(_, inner) => inner,
                _ => unreachable!("a path follows the parts of a value"),
            };
        }
        value
    }

    fn set(&mut self, loc: &Loc, new: Value) {
        let frame = &mut self.frames[loc.frame];
        if loc.path.is_empty() {
            frame.moved.remove(&loc.slot);
        }
        let mut value = frame.slots[loc.slot]
            .as_mut()
            .expect("set before it is written");
        for &at in &loc.path {
            value = match value {
                Value::Parts(parts) | Value::Variant(_, parts) => &mut parts[at],
                Value::Optional(Some(inner)) | Value::Outcome(_, inner) => inner,
                _ => unreachable!("a path follows the parts of a value"),
            };
        }
        *value = new;
    }

    /// The elements of the array, slice or text at `loc`, unless it is a
    /// Vec.
    fn seq(&self, loc: &Loc) -> Option<Seq> {
        match self.at(loc) {
            Value::Parts(parts) => Some(Seq::Elems(loc.clone(), 0, parts.len())),
            other => self.seq_of(other.clone()),
        }
    }

    /// The elements `value` holds or refers to, unless it is a Vec.
    fn seq_of(&self, value: Value) -> Option<Seq> {
        match value {
            Value::Ref(loc) => self.seq(&loc),
            Value::Text(bytes) => Some(Seq::Text(bytes)),
            Value::Slice(base, start, len) => Some(Seq::Elems(base, start, len)),
            _ => None,
        }
    }

    /// Where `place` is, its indices evaluated and checked.
    fn loc(&mut self, place: &Place) -> Run<Loc> {
        Ok(match place {
            Place::Local(local) if self.is_view(*local) => {
                match self.frames.last().unwrap().slots[local.0 as usize].clone() {
                    Some(Value::Ref(loc)) => loc,
                    _ => unreachable!("a view holds a reference"),
                }
            }
            Place::Local(local) => Loc {
                frame: self.frames.len() - 1,
                slot: local.0 as usize,
                path: Vec::new(),
            },
            Place::Field(base, at) => {
                let mut loc = self.loc(base)?;
                loc.path.push(*at as usize);
                loc
            }
            Place::Index(base, index) => match self.index(base, index)? {
                Element::Place(loc) => loc,
                Element::Value(_) => return cannot("index text in place", index.span),
            },
            Place::Deref(pointer) => match self.expr(pointer)? {
                Value::Ref(loc) => loc,
                _ => unreachable!("a reference is followed"),
            },
            Place::Temp(value) => {
                let held = self.expr(value)?;
                let loc = self.temp(held);
                if !matches!(value.kind, ExprKind::Read(_)) && self.needs_drop(value.ty) {
                    self.frame().temps.push((loc.slot, value.ty));
                }
                loc
            }
            Place::Capture(at) => {
                let env = self.frames.last().unwrap().body.params[0];
                let env = self.loc(&Place::Local(env))?;
                let mut fields = match self.at(&env) {
                    Value::Ref(closure) => closure.clone(),
                    _ => env,
                };
                fields.path.push(*at as usize);
                match self.get(&fields) {
                    Value::Ref(target) => target,
                    _ => unreachable!("a closure captures through references"),
                }
            }
        })
    }

    fn read(&mut self, place: &Place) -> Run<Value> {
        if let Some(value) = self.reinterpret(place)? {
            return Ok(value);
        }
        if !matches!(place, Place::Temp(_)) && shape(place).is_some() {
            let part = self.literal_part(place)?;
            return self.expr(part);
        }
        let loc = match place {
            Place::Index(base, index) => match self.index(base, index)? {
                Element::Place(loc) => loc,
                Element::Value(byte) => return Ok(byte),
            },
            _ => self.loc(place)?,
        };
        Ok(self.get(&loc))
    }

    /// Element `index` of the sequence at `base`: a place in an array, or a
    /// byte of text.
    fn index(&mut self, base: &Place, index: &Expr) -> Run<Element> {
        let loc = self.loc(base)?;
        let at = self.int(index)?;
        let Some(seq) = self.seq(&loc) else {
            return cannot("index a Vec: it lives on the heap", index.span);
        };
        Ok(seq.element(inside(at, seq.len(), index.span)?))
    }

    /// `*((&x) as *T)`, which reads the bits of the number `x` as a number
    /// of another type of the same size; none for any other place.
    fn reinterpret(&mut self, place: &Place) -> Run<Option<Value>> {
        let Place::Deref(pointer) = place else {
            return Ok(None);
        };
        let ExprKind::Cast(address) = &pointer.kind else {
            return Ok(None);
        };
        let ExprKind::AddressOf(source) = &address.kind else {
            return Ok(None);
        };
        let types = self.types();
        let (Some(from), Some(to)) = (types.pointee(address.ty), types.pointee(pointer.ty)) else {
            return Ok(None);
        };
        let width = |ty: Ty| match types.as_int(ty) {
            Some(int) => Some(int.bits()),
            None if ty == Types::F64 => Some(64),
            None if ty == Types::F32 => Some(32),
            None => None,
        };
        let Some(bits) = width(from).filter(|&bits| width(to) == Some(bits)) else {
            return Ok(None);
        };
        let loc = self.loc(source)?;
        let raw = match self.get(&loc) {
            Value::Float(x) if bits == 32 => u64::from((x as f32).to_bits()),
            Value::Float(x) => x.to_bits(),
            Value::Int(n) => (n as u64) & (u64::MAX >> (64 - bits)),
            _ => unreachable!("a number"),
        };
        Ok(Some(match self.types().as_int(to) {
            Some(int) => Value::Int(wrap(i128::from(raw), int)),
            None if bits == 32 => Value::Float(f64::from(f32::from_bits(raw as u32))),
            None => Value::Float(f64::from_bits(raw)),
        }))
    }

    /// The part of a written-out array, tuple or struct that `place` picks,
    /// worked out without building the rest, as a constant read inside
    /// another one is; `shape` has checked each step fits.
    fn literal_part<'e>(&mut self, place: &'e Place) -> Run<&'e Expr> {
        let (base, step) = match place {
            Place::Temp(expr) => return Ok(expr),
            Place::Index(base, _) | Place::Field(base, _) => (self.literal_part(base)?, place),
            _ => unreachable!("shape takes only these"),
        };
        Ok(match (step, &base.kind) {
            (Place::Index(_, index), ExprKind::Array(items)) => {
                &items[inside(self.int(index)?, items.len(), index.span)?]
            }
            (Place::Field(_, at), ExprKind::Tuple(items)) => &items[*at as usize],
            (Place::Field(_, at), ExprKind::Struct(fields)) => {
                &fields
                    .iter()
                    .find(|(field, _)| field == at)
                    .expect("each field")
                    .1
            }
            _ => unreachable!("shape checked each step"),
        })
    }

    fn truth(&mut self, cond: &Expr) -> Run<bool> {
        match self.expr(cond)? {
            Value::Bool(b) => Ok(b),
            _ => unreachable!("a condition is a bool"),
        }
    }

    fn int(&mut self, expr: &Expr) -> Run<i128> {
        match self.expr(expr)? {
            Value::Int(n) => Ok(n),
            Value::Bool(b) => Ok(i128::from(b)),
            Value::Variant(at, _) => Ok(i128::from(at)),
            _ => unreachable!("an integer"),
        }
    }

    /// `n` as an integer of `ty` holds it, or a failure when it does not fit.
    fn fit(&self, n: Option<i128>, ty: Ty, span: Span) -> Run<Value> {
        let int = self.types().as_int(ty).expect("an integer type");
        match n.filter(|&n| int.contains(n)) {
            Some(n) => Ok(Value::Int(n)),
            None => {
                let shown = self.types().show(ty);
                fail(
                    format!("Arithmetic overflow of {shown} while computing a constant"),
                    span,
                )
            }
        }
    }

    fn expr(&mut self, expr: &Expr) -> Run<Value> {
        let span = expr.span;
        let ty = expr.ty;
        if let TyKind::Ptr(_) = self.types().kind(ty) {
            let shown = self.types().show(ty);
            return cannot(&format!("use a pointer: this is a {shown}"), span);
        }
        Ok(match &expr.kind {
            ExprKind::Int(n) => Value::Int(*n),
            ExprKind::Float(x) => Value::Float(round(*x, ty)),
            ExprKind::Bool(b) => Value::Bool(*b),
            ExprKind::Str(bytes) => Value::Text(Rc::from(bytes.as_slice())),
            ExprKind::Void => Value::Void,
            ExprKind::Move(place) if self.needs_drop(ty) => {
                let loc = self.loc(place)?;
                let value = self.get(&loc);
                self.give_up(place, &loc)?;
                value
            }
            ExprKind::Read(place) | ExprKind::Move(place) => self.read(place)?,
            ExprKind::Copy(_) => return cannot("copy memory on the heap", span),
            ExprKind::AddressOf(place) => Value::Ref(self.loc(place)?),
            ExprKind::Unary(op, operand) => {
                let value = self.expr(operand)?;
                match (op, value) {
                    (UnOp::Neg, Value::Int(n)) => self.fit(n.checked_neg(), ty, span)?,
                    (UnOp::Neg, Value::Float(x)) => Value::Float(-x),
                    (UnOp::Not, Value::Bool(b)) => Value::Bool(!b),
                    (UnOp::Not, Value::Int(n)) => Value::Int(wrap(!n, self.int_ty(ty))),
                    _ => unreachable!("checked operand"),
                }
            }
            ExprKind::Binary(op, left, right) => {
                let (l, r) = (self.expr(left)?, self.expr(right)?);
                self.binary(*op, l, r, left.ty, ty, span)?
            }
            ExprKind::And(left, right) => Value::Bool(self.truth(left)? && self.truth(right)?),
            ExprKind::Or(left, right) => Value::Bool(self.truth(left)? || self.truth(right)?),
            ExprKind::Cast(value) => {
                let from = value.ty;
                let value = self.expr(value)?;
                self.cast(value, from, ty, span)?
            }
            ExprKind::Call(id, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.expr(arg))
                    .collect::<Run<Vec<_>>>()?;
                self.call(*id, args, span)?
            }
            ExprKind::Builtin(builtin, args) => self.builtin(*builtin, args, span)?,
            ExprKind::Tuple(values) | ExprKind::Array(values) => Value::Parts(
                values
                    .iter()
                    .map(|value| self.expr(value))
                    .collect::<Run<_>>()?,
            ),
            ExprKind::Struct(fields) => {
                let mut parts = vec![Value::Void; fields.len()];
                for (at, value) in fields {
                    parts[*at as usize] = self.expr(value)?;
                }
                Value::Parts(parts)
            }
            ExprKind::ArrayRepeat(value, count) => {
                let value = self.expr(value)?;
                Value::Parts(vec![value; *count as usize])
            }
            ExprKind::Variant(at, values) => Value::Variant(
                *at,
                values
                    .iter()
                    .map(|value| self.expr(value))
                    .collect::<Run<_>>()?,
            ),
            ExprKind::Some(value) => Value::Optional(Some(Box::new(self.expr(value)?))),
            ExprKind::None => Value::Optional(None),
            ExprKind::Ok(value) => Value::Outcome(true, Box::new(self.expr(value)?)),
            ExprKind::Err(code) => Value::Outcome(false, Box::new(self.expr(code)?)),
            ExprKind::VecLiteral(_) | ExprKind::VecRepeat(..) => {
                return cannot("make a Vec: it lives on the heap", span);
            }
            ExprKind::Assign {
                place,
                value,
                drop_old,
            } => {
                let target = value.ty;
                let value = self.expr(value)?;
                let loc = self.loc(place)?;
                if *drop_old {
                    self.drop_old(&loc, target)?;
                }
                self.set(&loc, value);
                Value::Void
            }
            ExprKind::CompoundAssign { place, op, value } => {
                let target = self.place_ty(place);
                let value = self.expr(value)?;
                let loc = self.loc(place)?;
                let old = self.get(&loc);
                let new = self.binary(*op, old, value, target, target, span)?;
                self.set(&loc, new);
                Value::Void
            }
            ExprKind::Match { scrutinee, arms } => self.match_expr(scrutinee, arms)?,
            ExprKind::Try(value) => match self.expr(value)? {
                Value::Optional(Some(inner)) | Value::Outcome(true, inner) => *inner,
                failed @ (Value::Optional(None) | Value::Outcome(false, _)) => {
                    return Err(Exit::Return(failed));
                }
                _ => unreachable!("a T? or a T!"),
            },
            ExprKind::Block(block) if ty == Types::VOID => {
                self.block(block)?;
                Value::Void
            }
            ExprKind::Block(block) => self.value_block(block)?,
            ExprKind::Print { .. } => return cannot("print", span),
            ExprKind::Assert { cond, message } => {
                if !self.truth(cond)? {
                    let failed = "An assert failed while computing a constant";
                    return match message.as_deref().and_then(|message| self.said(message)) {
                        Some(said) => fail(format!("{failed}: {said}"), span),
                        None => fail(failed, span),
                    };
                }
                Value::Void
            }
            ExprKind::Unpack { target, value } => {
                let ty = value.ty;
                let value = self.expr(value)?;
                self.unpack(target, value, ty)?;
                Value::Void
            }
            ExprKind::Asm(_) => return cannot("run assembly", span),
        })
    }

    /// Drop the value at `loc` an assignment replaces, unless it was moved
    /// away.
    fn drop_old(&mut self, loc: &Loc, ty: Ty) -> Run<()> {
        if loc.path.is_empty() && self.frames[loc.frame].moved.contains(&loc.slot) {
            return Ok(());
        }
        self.drop_at(loc.clone(), ty)
    }

    /// The text an assert's `message` prints, when its values are numbers,
    /// bools or text.
    fn said(&mut self, message: &Expr) -> Option<String> {
        let ExprKind::Print { pieces, values, .. } = &message.kind else {
            return None;
        };
        let mut said = String::from_utf8_lossy(&pieces[0]).into_owned();
        for (value, piece) in values.iter().zip(&pieces[1..]) {
            match self.expr(value).ok()? {
                Value::Int(n) => said += &n.to_string(),
                Value::Bool(b) => said += &b.to_string(),
                Value::Text(bytes) => said += &String::from_utf8_lossy(&bytes),
                _ => return None,
            }
            said += &String::from_utf8_lossy(piece);
        }
        Some(said)
    }

    /// The type of what `place` holds, in the function running.
    fn place_ty(&mut self, place: &Place) -> Ty {
        let body = self.frames.last().expect("a frame runs").body.clone();
        let closure = (body.params.first())
            .and_then(|env| self.types().pointee(body.locals[env.0 as usize].ty));
        let captures: Vec<Ty> = (self.cx.closures.iter())
            .find(|info| Some(info.ty) == closure)
            .map_or_else(Vec::new, |info| {
                let types = &self.cx.types;
                info.fields
                    .iter()
                    .filter_map(|&field| types.pointee(field))
                    .collect()
            });
        self.cx
            .place_ty(place, &|root| match *root {
                Place::Local(local) => body.locals[local.0 as usize].ty,
                Place::Capture(at) => captures[at as usize],
                _ => unreachable!("a place starts at a local or a capture"),
            })
            .expect("a checked place")
    }

    fn int_ty(&self, ty: Ty) -> IntTy {
        self.types().as_int(ty).expect("an integer type")
    }

    /// Give each part of `value`, of type `ty`, where `target` puts it; a
    /// part put nowhere is dropped.
    fn unpack(&mut self, target: &Unpack, value: Value, ty: Ty) -> Run<()> {
        match target {
            Unpack::Declare(local) => {
                self.set_local(*local, value);
                self.own(*local);
            }
            Unpack::Store(place) => {
                let loc = self.loc(place)?;
                self.drop_old(&loc, ty)?;
                self.set(&loc, value);
            }
            Unpack::Skip => {
                if self.needs_drop(ty) {
                    let loc = self.temp(value);
                    self.drop_at(loc, ty)?;
                }
            }
            Unpack::Tuple(parts) => {
                let (Value::Parts(values), TyKind::Tuple(tys)) =
                    (value, self.types().kind(ty).clone())
                else {
                    unreachable!("a tuple is taken apart")
                };
                for ((part, value), ty) in parts.iter().zip(values).zip(tys) {
                    self.unpack(part, value, ty)?;
                }
            }
        }
        Ok(())
    }

    /// A `match`. A temporary scrutinee with a `drop` gives the bindings of
    /// an arm with no guard that take a part of it that part, and drops the
    /// others.
    fn match_expr(&mut self, scrutinee: &Place, arms: &[tir::Arm]) -> Run<Value> {
        let loc = self.loc(scrutinee)?;
        let take = match scrutinee {
            Place::Temp(value) => self.needs_drop(value.ty).then_some(value.ty),
            _ => None,
        };
        let slot = |this: &Self, slot: Option<u32>| {
            let mut place = loc.clone();
            place.path.extend(slot.map(|slot| slot as usize));
            match this.at(&place) {
                Value::Ref(target) if slot.is_some() => target.clone(),
                _ => place,
            }
        };
        for arm in arms {
            let alternative = match arm.tags.is_empty() {
                true => Some(0),
                false => arm.tags.iter().position(|tests| {
                    tests.iter().all(|test| {
                        let tag = match self.at(&slot(self, test.slot)) {
                            Value::Int(n) => *n,
                            Value::Bool(b) => i128::from(*b),
                            Value::Variant(at, _) => i128::from(*at),
                            Value::Optional(inner) => i128::from(inner.is_some()),
                            Value::Outcome(ok, _) => i128::from(*ok),
                            _ => unreachable!("a tested value has a tag"),
                        };
                        test.low <= tag && tag <= test.high
                    })
                }),
            };
            let Some(at) = alternative else {
                continue;
            };
            let mut places = Vec::new();
            let mut taken = Vec::new();
            for (binding, reaches) in &arm.bindings {
                let reach = reaches[at];
                let mut place = slot(self, reach.slot);
                match reach.payload {
                    Payload::Whole => {
                        place = loc.clone();
                        place.path.extend(reach.slot.map(|slot| slot as usize));
                    }
                    Payload::SomeValue | Payload::OkValue | Payload::ErrValue => place.path.push(0),
                    Payload::Variant { field, .. } => place.path.push(field as usize),
                }
                let decl = &self.frames.last().unwrap().body.locals[binding.0 as usize];
                let (view, ty) = (decl.view, decl.ty);
                if take.is_some() && arm.guard.is_none() && !view && self.needs_drop(ty) {
                    taken.push(place.clone());
                }
                places.push((*binding, place));
            }
            if let (Some(subject), false) = (take, taken.is_empty()) {
                self.frames[loc.frame].moved.insert(loc.slot);
                self.drop_except(loc.clone(), subject, &taken)?;
            }
            for (binding, place) in places {
                self.bind(binding, Element::Place(place));
            }
            if let Some(guard) = &arm.guard
                && !self.truth(guard)?
            {
                continue;
            }
            let owned = self.frame().owned.len();
            for (binding, _) in &arm.bindings {
                let decl = &self.frames.last().unwrap().body.locals[binding.0 as usize];
                if !taken.is_empty() && !decl.view {
                    self.own(*binding);
                }
            }
            let value = self.expr(&arm.body);
            return self.leave_with(owned, value);
        }
        unreachable!("every value has an arm")
    }

    fn builtin(&mut self, builtin: Builtin, args: &[Expr], span: Span) -> Run<Value> {
        let subject = |this: &mut Self| -> Run<Value> {
            match this.expr(&args[0])? {
                Value::Ref(loc) => Ok(this.get(&loc)),
                value => Ok(value),
            }
        };
        Ok(match builtin {
            Builtin::Len | Builtin::VecIsEmpty => {
                let len = match self.expr(&args[0])? {
                    Value::Parts(parts) => parts.len(),
                    value => match self.seq_of(value) {
                        Some(seq) => seq.len(),
                        None => return cannot("measure a Vec: it lives on the heap", span),
                    },
                };
                match builtin {
                    Builtin::Len => Value::Int(len as i128),
                    _ => Value::Bool(len == 0),
                }
            }
            Builtin::IsSome | Builtin::IsNone | Builtin::IsOk | Builtin::IsErr => {
                let holds = match subject(self)? {
                    Value::Optional(inner) => inner.is_some(),
                    Value::Outcome(ok, _) => ok,
                    _ => unreachable!("a T? or a T!"),
                };
                Value::Bool(holds == matches!(builtin, Builtin::IsSome | Builtin::IsOk))
            }
            Builtin::Unwrap => match subject(self)? {
                Value::Optional(Some(inner)) | Value::Outcome(true, inner) => *inner,
                Value::Optional(None) => {
                    return fail("unwrap() of None while computing a constant", span);
                }
                Value::Outcome(false, code) => {
                    let Value::Int(code) = *code else {
                        unreachable!("an error code")
                    };
                    return fail(
                        format!("unwrap() of Err({code}) while computing a constant"),
                        span,
                    );
                }
                _ => unreachable!("a T? or a T!"),
            },
            Builtin::UnwrapErr => match subject(self)? {
                Value::Outcome(false, code) => *code,
                _ => return fail("unwrap_err() of Ok while computing a constant", span),
            },
            Builtin::Slice => self.slice(args, span)?,
            Builtin::Replace => {
                let Value::Ref(loc) = self.expr(&args[0])? else {
                    unreachable!("replace takes a reference")
                };
                let new = self.expr(&args[1])?;
                let old = self.get(&loc);
                self.set(&loc, new);
                old
            }
            Builtin::Bits(op) => {
                let value = self.int(&args[0])?;
                let amount = match args.get(1) {
                    Some(amount) => self.int(amount)?,
                    None => 0,
                };
                bits(op, value, self.int_ty(args[0].ty), amount)
            }
            Builtin::Flush => return cannot("print", span),
            Builtin::Simd(_) => return cannot("use std.simd", span),
            _ => return cannot("use a Vec or a String: they live on the heap", span),
        })
    }

    /// `args[0][args[1]..args[2]]`, or to its end without `args[2]`: a part
    /// of an array, a slice or text, checked to lie within it.
    fn slice(&mut self, args: &[Expr], span: Span) -> Run<Value> {
        let value = self.expr(&args[0])?;
        let Some(seq) = self.seq_of(value) else {
            return cannot("slice a Vec: it lives on the heap", span);
        };
        let len = seq.len();
        let from = self.int(&args[1])?;
        let to = match args.get(2) {
            Some(end) => self.int(end)?,
            None => len as i128,
        };
        if from > to || to > len as i128 {
            return fail(
                format!("Slice {from}..{to} is outside 0..{len} while computing a constant"),
                span,
            );
        }
        let (from, to) = (from as usize, to as usize);
        Ok(match seq {
            Seq::Text(bytes) => Value::Text(Rc::from(&bytes[from..to])),
            Seq::Elems(base, start, _) => Value::Slice(base, start + from, to - from),
        })
    }

    fn cast(&self, value: Value, from: Ty, to: Ty, span: Span) -> Run<Value> {
        let types = self.types();
        let target = types.as_int(to);
        Ok(match (value, target) {
            (Value::Int(n), Some(int)) => Value::Int(wrap(n, int)),
            (Value::Bool(b), Some(_)) => Value::Int(i128::from(b)),
            (Value::Variant(at, _), Some(int)) => Value::Int(wrap(i128::from(at), int)),
            (Value::Int(n), None) if to == Types::STR => Value::Text(Rc::from([n as u8])),
            (Value::Int(n), None) if to == Types::F32 => Value::Float(f64::from(n as f32)),
            (Value::Int(n), None) => Value::Float(n as f64),
            (Value::Variant(at, _), None) => Value::Float(f64::from(at)),
            (Value::Float(x), None) => Value::Float(round(x, to)),
            (Value::Float(x), Some(int)) => {
                let whole = x.trunc();
                if whole.is_nan() || !int.contains(whole as i128) {
                    let shown = types.show(to);
                    return fail(
                        format!("{x} does not fit {shown}, while computing a constant"),
                        span,
                    );
                }
                Value::Int(whole as i128)
            }
            _ => {
                let shown = types.show(from);
                return cannot(format!("turn {shown} into a number").as_str(), span);
            }
        })
    }

    fn binary(&self, op: BinOp, l: Value, r: Value, operand: Ty, ty: Ty, span: Span) -> Run<Value> {
        if let (Value::Float(a), Value::Float(b)) = (&l, &r)
            && comparison(op).is_some()
        {
            return Ok(Value::Bool(match op {
                BinOp::Eq => a == b,
                BinOp::Ne => a != b,
                BinOp::Lt => a < b,
                BinOp::Le => a <= b,
                BinOp::Gt => a > b,
                _ => a >= b,
            }));
        }
        if let Some(order) = comparison(op) {
            return Ok(Value::Bool(order(compare(&l, &r))));
        }
        Ok(match (l, r) {
            (Value::Int(a), Value::Int(b)) => {
                let int = self.int_ty(operand);
                match op {
                    BinOp::Div | BinOp::Rem if b == 0 => {
                        return fail("Division by zero while computing a constant", span);
                    }
                    BinOp::Rem if int.signed() && a == int.min() && b == -1 => {
                        return fail("Division of MIN by -1 while computing a constant", span);
                    }
                    BinOp::Shl | BinOp::Shr if !(0..i128::from(int.bits())).contains(&b) => {
                        return fail(
                            format!("Shift by {b} is out of range while computing a constant"),
                            span,
                        );
                    }
                    _ => self.fit(arith(op, a, b, Some(int)), ty, span)?,
                }
            }
            (Value::Float(a), Value::Float(b)) => Value::Float(round(
                match op {
                    BinOp::Add => a + b,
                    BinOp::Sub => a - b,
                    BinOp::Mul => a * b,
                    BinOp::Div => a / b,
                    _ => a % b,
                },
                ty,
            )),
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(match op {
                BinOp::BitAnd => a & b,
                BinOp::BitOr => a | b,
                _ => a ^ b,
            }),
            _ => return cannot("do this operation", span),
        })
    }
}

/// A part of a sequence a loop walks: a place, or a byte of text.
#[derive(Clone)]
enum Element {
    Place(Loc),
    Value(Value),
}

fn comparison(op: BinOp) -> Option<fn(i32) -> bool> {
    Some(match op {
        BinOp::Eq => |o| o == 0,
        BinOp::Ne => |o| o != 0,
        BinOp::Lt => |o| o < 0,
        BinOp::Le => |o| o <= 0,
        BinOp::Gt => |o| o > 0,
        BinOp::Ge => |o| o >= 0,
        _ => return None,
    })
}

/// How `a` and `b` order, -1, 0 or 1, as the program compares them: the
/// first part that differs decides, and a NaN is equal to anything.
fn compare(a: &Value, b: &Value) -> i32 {
    let sign = |o: std::cmp::Ordering| o as i32;
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => sign(a.cmp(b)),
        (Value::Bool(a), Value::Bool(b)) => sign(a.cmp(b)),
        (Value::Float(a), Value::Float(b)) => i32::from(a > b) - i32::from(a < b),
        (Value::Text(a), Value::Text(b)) => sign(a.cmp(b)),
        (Value::Variant(a, _), Value::Variant(b, _)) => sign(a.cmp(b)),
        (Value::Parts(a), Value::Parts(b)) => a
            .iter()
            .zip(b)
            .map(|(a, b)| compare(a, b))
            .find(|&o| o != 0)
            .unwrap_or(0),
        _ => 0,
    }
}

/// `x` as a value of the float type `ty` holds it.
fn round(x: f64, ty: Ty) -> f64 {
    match ty == Types::F32 {
        true => x as f32 as f64,
        false => x,
    }
}

/// What a bit method gives for `value`, an integer of type `int`.
fn bits(op: BitOp, value: i128, int: IntTy, amount: i128) -> Value {
    let width = int.bits();
    let mask = (1u128 << width) - 1;
    let raw = (value as u128) & mask;
    let result = match op {
        BitOp::CountOnes => return Value::Int(i128::from(raw.count_ones())),
        BitOp::LeadingZeros => return Value::Int(i128::from(raw.leading_zeros() - (128 - width))),
        BitOp::TrailingZeros => return Value::Int(i128::from(raw.trailing_zeros().min(width))),
        BitOp::RotateLeft | BitOp::RotateRight => {
            let mut by = (amount as u128 % u128::from(width)) as u32;
            if op == BitOp::RotateRight {
                by = (width - by) % width;
            }
            ((raw << by) | (raw >> ((width - by) % width))) & mask
        }
        BitOp::SwapBytes => {
            let bytes = width / 8;
            (0..bytes).fold(0u128, |swapped, at| {
                swapped | ((raw >> (8 * at) & 0xff) << (8 * (bytes - 1 - at)))
            })
        }
    };
    Value::Int(wrap(result as i128, int))
}

/// A part of the written-out array, tuple or struct `place` reads, when
/// every step to it goes into one: the shape every element of it shares.
fn shape(place: &Place) -> Option<&Expr> {
    let compound = |expr: &Expr| {
        matches!(
            expr.kind,
            ExprKind::Array(_) | ExprKind::Tuple(_) | ExprKind::Struct(_)
        )
    };
    match place {
        Place::Temp(expr) => Some(expr.as_ref()).filter(|expr| compound(expr)),
        Place::Index(base, _) => match &shape(base)?.kind {
            ExprKind::Array(items) => items.first(),
            _ => None,
        },
        Place::Field(base, at) => match &shape(base)?.kind {
            ExprKind::Tuple(items) => items.get(*at as usize),
            ExprKind::Struct(fields) => fields.iter().find(|(f, _)| f == at).map(|(_, e)| e),
            _ => None,
        },
        _ => None,
    }
}
