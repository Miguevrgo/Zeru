//! Running a global constant's value at compile time, so a function may
//! compute it. The functions it calls run over their typed IR, on plain
//! data only: numbers, bools, text, and arrays, tuples, structs, enums and
//! optionals of them, reached directly or through references. What needs the
//! heap, a pointer, assembly or output stops it with an error.

use std::collections::HashMap;
use std::rc::Rc;

use crate::errors::{Span, ZeruError};
use crate::tir::{
    self, BinOp, BitOp, Block, Builtin, Expr, ExprKind, FnId, LocalId, Payload, Place, Stmt,
    StmtKind, UnOp, Unpack,
};
use crate::ty::{IntTy, Ty, TyKind, Types};

use super::Checker;
use super::consts::wrap;

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
    Ref(Loc),
}

/// Where a value lives: a slot of a frame, and the parts followed from it.
#[derive(Clone, Debug)]
struct Loc {
    frame: usize,
    slot: usize,
    path: Vec<usize>,
}

/// Why evaluation stopped short: a jump out of a loop or a function, or a
/// failure with its message and where.
enum Exit {
    Break,
    Continue,
    Return(Value),
    Fail(String, Span),
}

type Run<T> = Result<T, Exit>;

/// What a function body needs to run: which of its locals are views, its
/// parameters and its statements.
struct Body {
    views: Vec<bool>,
    params: Vec<LocalId>,
    block: Block,
}

struct Frame {
    slots: Vec<Option<Value>>,
    views: Rc<[bool]>,
}

struct Eval<'c, 'a> {
    cx: &'c mut Checker<'a>,
    bodies: HashMap<FnId, Rc<Body>>,
    frames: Vec<Frame>,
    steps: u64,
}

impl Checker<'_> {
    /// The value of `expr`, the definition of the constant `name` declared
    /// at `span`, run now and written as literals.
    /// `views` says which of the locals the definition's own `match`
    /// declares are views.
    pub(super) fn run_constant(
        &mut self,
        expr: &Expr,
        views: &[bool],
        name: &str,
        span: Span,
    ) -> Option<Expr> {
        let mut eval = Eval {
            cx: self,
            bodies: HashMap::new(),
            frames: vec![Frame {
                slots: vec![None; views.len()],
                views: Rc::from(views),
            }],
            steps: 0,
        };
        let value = match eval.expr(expr) {
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

/// What a constant cannot do at compile time, as the error says it.
fn cannot<T>(what: &str, span: Span) -> Run<T> {
    fail(
        format!("Computing a constant while compiling cannot {what}"),
        span,
    )
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
        if self.cx.functions[id.0 as usize].is_none() {
            let instance = &self.cx.queued[id.0 as usize];
            let (def, args) = (instance.def, instance.args.clone());
            let function = super::body::check_function(self.cx, def, &args);
            self.cx.functions[id.0 as usize] = Some(function);
        }
        let function = self.cx.functions[id.0 as usize].as_ref().unwrap();
        let body = Rc::new(Body {
            views: function.locals.iter().map(|local| local.view).collect(),
            params: function.params.clone(),
            block: function.body.clone(),
        });
        self.bodies.insert(id, body.clone());
        body
    }

    fn call(&mut self, id: FnId, args: Vec<Value>, span: Span) -> Run<Value> {
        if self.frames.len() > MAX_DEPTH {
            return fail(
                format!("A constant's calls go more than {MAX_DEPTH} deep while computing it"),
                span,
            );
        }
        let body = self.body(id);
        let mut slots = vec![None; body.views.len()];
        for (&param, arg) in body.params.iter().zip(args) {
            slots[param.0 as usize] = Some(arg);
        }
        self.frames.push(Frame {
            slots,
            views: Rc::from(body.views.as_slice()),
        });
        let result = self.block(&body.block);
        self.frames.pop();
        match result {
            Ok(()) => Ok(Value::Void),
            Err(Exit::Return(value)) => Ok(value),
            Err(other) => Err(other),
        }
    }

    fn block(&mut self, block: &Block) -> Run<()> {
        for stmt in &block.stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    /// One statement; the temporaries it makes end with it.
    fn stmt(&mut self, stmt: &Stmt) -> Run<()> {
        self.step(stmt.span)?;
        let mark = self.frame().slots.len();
        let result = self.run_stmt(stmt);
        self.frame().slots.truncate(mark);
        result
    }

    fn run_stmt(&mut self, stmt: &Stmt) -> Run<()> {
        match &stmt.kind {
            StmtKind::Let(local, value) => {
                let value = self.expr(value)?;
                self.set_local(*local, value);
            }
            StmtKind::Expr(expr) => {
                self.expr(expr)?;
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
                if self.truth(cond)? {
                    self.block(then)?;
                } else if let Some(otherwise) = otherwise {
                    self.block(otherwise)?;
                }
            }
            StmtKind::While { cond, body } => {
                while self.truth(cond)? {
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
                    let len = match self.at(&loc) {
                        Value::Parts(parts) => parts.len(),
                        Value::Text(bytes) => bytes.len(),
                        _ => return cannot("walk a Vec: it lives on the heap", stmt.span),
                    };
                    seqs.push((walk, loc, len));
                }
                let count = seqs.iter().map(|(_, _, len)| *len).min().unwrap_or(0);
                for at in 0..count {
                    self.step(stmt.span)?;
                    if let Some(index) = index {
                        self.set_local(*index, Value::Int(at as i128));
                    }
                    for (walk, loc, _) in &seqs {
                        let element = self.element(loc, at, stmt.span)?;
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

    /// One turn of a loop: whether a `break` ends it.
    fn turn(&mut self, body: &Block) -> Run<bool> {
        match self.block(body) {
            Ok(()) | Err(Exit::Continue) => Ok(false),
            Err(Exit::Break) => Ok(true),
            Err(other) => Err(other),
        }
    }

    /// Element `at` of the array or text at `loc`: a place for an array's,
    /// a value for a byte of text.
    fn element(&mut self, loc: &Loc, at: usize, span: Span) -> Run<Element> {
        match self.at(loc) {
            Value::Text(bytes) => Ok(Element::Value(Value::Int(i128::from(bytes[at])))),
            Value::Parts(_) => {
                let mut loc = loc.clone();
                loc.path.push(at);
                Ok(Element::Place(loc))
            }
            _ => cannot("walk this sequence", span),
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
        frame.views.get(local.0 as usize).copied().unwrap_or(false)
    }

    fn set_local(&mut self, local: LocalId, value: Value) {
        self.frame().slots[local.0 as usize] = Some(value);
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
                Value::Optional(Some(inner)) => inner,
                _ => unreachable!("a path follows the parts of a value"),
            };
        }
        value
    }

    fn set(&mut self, loc: &Loc, new: Value) {
        let mut value = self.frames[loc.frame].slots[loc.slot]
            .as_mut()
            .expect("set before it is written");
        for &at in &loc.path {
            value = match value {
                Value::Parts(parts) | Value::Variant(_, parts) => &mut parts[at],
                Value::Optional(Some(inner)) => inner,
                _ => unreachable!("a path follows the parts of a value"),
            };
        }
        *value = new;
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
            Place::Index(base, index) => {
                let mut loc = self.loc(base)?;
                let at = self.int(index)?;
                let len = match self.at(&loc) {
                    Value::Parts(parts) => parts.len(),
                    Value::Text(_) => return cannot("index text in place", index.span),
                    _ => return cannot("index a Vec: it lives on the heap", index.span),
                };
                if at < 0 || at >= len as i128 {
                    return fail(
                        format!("Index {at} is outside 0..{len} while computing a constant"),
                        index.span,
                    );
                }
                loc.path.push(at as usize);
                loc
            }
            Place::Deref(pointer) => match self.expr(pointer)? {
                Value::Ref(loc) => loc,
                _ => return cannot("follow a pointer", pointer.span),
            },
            Place::Temp(value) => {
                let value = self.expr(value)?;
                self.temp(value)
            }
        })
    }

    fn read(&mut self, place: &Place, span: Span) -> Run<Value> {
        if let Place::Index(base, index) = place {
            let loc = self.loc(base)?;
            if let Value::Text(bytes) = self.at(&loc) {
                let bytes = bytes.clone();
                let at = self.int(index)?;
                return match bytes.get(at as usize).filter(|_| at >= 0) {
                    Some(&byte) => Ok(Value::Int(i128::from(byte))),
                    None => fail(
                        format!(
                            "Index {at} is outside 0..{} while computing a constant",
                            bytes.len()
                        ),
                        span,
                    ),
                };
            }
        }
        let loc = self.loc(place)?;
        Ok(self.get(&loc))
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
        Ok(match &expr.kind {
            ExprKind::Int(n) => Value::Int(*n),
            ExprKind::Float(x) => Value::Float(round(*x, ty)),
            ExprKind::Bool(b) => Value::Bool(*b),
            ExprKind::Str(bytes) => Value::Text(Rc::from(bytes.as_slice())),
            ExprKind::Void => Value::Void,
            ExprKind::Read(place) | ExprKind::Move(place) => self.read(place, span)?,
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
            ExprKind::Ok(_) | ExprKind::Err(_) => return cannot("make a T! result", span),
            ExprKind::VecLiteral(_) | ExprKind::VecRepeat(..) => {
                return cannot("make a Vec: it lives on the heap", span);
            }
            ExprKind::Assign { place, value, .. } => {
                let value = self.expr(value)?;
                let loc = self.loc(place)?;
                self.set(&loc, value);
                Value::Void
            }
            ExprKind::CompoundAssign { place, op, value } => {
                let target = value.ty;
                let value = self.expr(value)?;
                let loc = self.loc(place)?;
                let old = self.get(&loc);
                let new = self.binary(*op, old, value, target, target, span)?;
                self.set(&loc, new);
                Value::Void
            }
            ExprKind::Match { scrutinee, arms } => self.match_expr(scrutinee, arms)?,
            ExprKind::Try(value) => match self.expr(value)? {
                Value::Optional(Some(inner)) => *inner,
                Value::Optional(None) => return Err(Exit::Return(Value::Optional(None))),
                _ => return cannot("use a T! result", span),
            },
            ExprKind::Block(block) => {
                self.block(block)?;
                Value::Void
            }
            ExprKind::Print { .. } => return cannot("print", span),
            ExprKind::Assert { cond, .. } => {
                if !self.truth(cond)? {
                    return fail("An assert failed while computing a constant", span);
                }
                Value::Void
            }
            ExprKind::Unpack { target, value } => {
                let value = self.expr(value)?;
                self.unpack(target, value)?;
                Value::Void
            }
            ExprKind::Asm(_) => return cannot("run assembly", span),
        })
    }

    fn int_ty(&self, ty: Ty) -> IntTy {
        self.types().as_int(ty).expect("an integer type")
    }

    fn unpack(&mut self, target: &Unpack, value: Value) -> Run<()> {
        match target {
            Unpack::Declare(local) => self.set_local(*local, value),
            Unpack::Store(place) => {
                let loc = self.loc(place)?;
                self.set(&loc, value);
            }
            Unpack::Skip => {}
            Unpack::Tuple(parts) => {
                let Value::Parts(values) = value else {
                    unreachable!("a tuple is taken apart")
                };
                for (part, value) in parts.iter().zip(values) {
                    self.unpack(part, value)?;
                }
            }
        }
        Ok(())
    }

    fn match_expr(&mut self, scrutinee: &Place, arms: &[tir::Arm]) -> Run<Value> {
        let loc = self.loc(scrutinee)?;
        let tag = match self.at(&loc) {
            Value::Int(n) => *n,
            Value::Bool(b) => i128::from(*b),
            Value::Variant(at, _) => i128::from(*at),
            Value::Optional(inner) => i128::from(inner.is_some()),
            _ => unreachable!("a matched value has a tag"),
        };
        for arm in tir::tried(arms).into_iter().map(|at| &arms[at]) {
            let alternative = match arm.tags.is_empty() {
                true => Some(0),
                false => arm
                    .tags
                    .iter()
                    .position(|&(low, high)| low <= tag && tag <= high),
            };
            let Some(at) = alternative else {
                continue;
            };
            for (binding, payloads) in &arm.bindings {
                let mut place = loc.clone();
                match payloads[at] {
                    Payload::Whole => {}
                    Payload::SomeValue => place.path.push(0),
                    Payload::Variant { field, .. } => place.path.push(field as usize),
                    Payload::OkValue | Payload::ErrValue => {
                        return cannot("use a T! result", arm.span);
                    }
                }
                self.bind(*binding, Element::Place(place));
            }
            if let Some(guard) = &arm.guard
                && !self.truth(guard)?
            {
                continue;
            }
            return self.expr(&arm.body);
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
            Builtin::Len => match subject(self)? {
                Value::Parts(parts) => Value::Int(parts.len() as i128),
                Value::Text(bytes) => Value::Int(bytes.len() as i128),
                _ => return cannot("measure a Vec: it lives on the heap", span),
            },
            Builtin::IsSome | Builtin::IsNone => match subject(self)? {
                Value::Optional(inner) => {
                    Value::Bool(inner.is_some() == (builtin == Builtin::IsSome))
                }
                _ => return cannot("use a T! result", span),
            },
            Builtin::Unwrap => match subject(self)? {
                Value::Optional(Some(inner)) => *inner,
                Value::Optional(None) => {
                    return fail("unwrap() of None while computing a constant", span);
                }
                _ => return cannot("use a T! result", span),
            },
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
            _ => return cannot("use the heap or a slice", span),
        })
    }

    fn cast(&self, value: Value, from: Ty, to: Ty, span: Span) -> Run<Value> {
        let types = self.types();
        let target = types.as_int(to);
        Ok(match (value, target) {
            (Value::Int(n), Some(int)) => Value::Int(wrap(n, int)),
            (Value::Bool(b), Some(_)) => Value::Int(i128::from(b)),
            (Value::Variant(at, _), Some(int)) => Value::Int(wrap(i128::from(at), int)),
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
                let bits = i128::from(int.bits());
                match op {
                    BinOp::Add => self.fit(a.checked_add(b), ty, span)?,
                    BinOp::Sub => self.fit(a.checked_sub(b), ty, span)?,
                    BinOp::Mul => self.fit(a.checked_mul(b), ty, span)?,
                    BinOp::Div | BinOp::Rem if b == 0 => {
                        return fail("Division by zero while computing a constant", span);
                    }
                    BinOp::Div => self.fit(a.checked_div(b), ty, span)?,
                    BinOp::Rem if int.signed() && a == int.min() && b == -1 => {
                        return fail("Division of MIN by -1 while computing a constant", span);
                    }
                    BinOp::Rem => self.fit(a.checked_rem(b), ty, span)?,
                    BinOp::AddWrap => Value::Int(wrap(a.wrapping_add(b), int)),
                    BinOp::SubWrap => Value::Int(wrap(a.wrapping_sub(b), int)),
                    BinOp::MulWrap => Value::Int(wrap(a.wrapping_mul(b), int)),
                    BinOp::BitAnd => Value::Int(a & b),
                    BinOp::BitOr => Value::Int(a | b),
                    BinOp::BitXor => Value::Int(a ^ b),
                    BinOp::Shl | BinOp::Shr if !(0..bits).contains(&b) => {
                        return fail(
                            format!("Shift by {b} is out of range while computing a constant"),
                            span,
                        );
                    }
                    BinOp::Shl => Value::Int(wrap(a << b, int)),
                    BinOp::Shr => Value::Int(a >> b),
                    _ => return cannot("do pointer arithmetic", span),
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
