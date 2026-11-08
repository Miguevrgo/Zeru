//! Borrow checking and move checking on the MIR. Zeru is single threaded,
//! so a live reference forbids only what can invalidate the memory it
//! points into, never aliasing or plain writes.
//!
//! A loan is taken by `&place`, by `get`, by a `match` binding and by a
//! `for` loop, all of which the MIR writes as taking an address. A forward
//! dataflow finds which locals may hold which loans at each point, and a
//! loan is live where one of them is used later. With a loan live, these
//! are errors: moving out of what it points into; ending the local it points
//! into; dropping or reallocating a place it reaches through an owning step
//! after it (an element of a Vec, a variant's payload); and assigning to an
//! enum whose payload it points into. An access through a reference counts
//! as an access to what the reference points at, and a call as reallocating
//! what each `&var` its arguments carry points at, unless an argument's type
//! lets nothing change or a closure captures the `&var` only to read it. A
//! reference stored behind a parameter is held until the function returns,
//! and what each function may do to its arguments, store in one a reference
//! into itself or free a part of it, is found by checking it first. A second
//! dataflow finds where a move out of a local may have happened, and any use
//! of it there is an error.

use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::errors::{Sources, Span, ZeruError};
use crate::tir::{BinOp, Program};
use crate::ty::{Ty, TyKind};

use super::{
    BlockId, Body, Local, Operand, Part, Place, Projection, RETURN, Rvalue, StatementKind,
    TerminatorKind,
};

/// A step along a path, as a projection, with every index alike and the
/// payload of a `T?` or `T!` a variant like an enum's.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Step {
    Deref,
    Field(u32),
    Index { heap: bool },
    Variant(u32),
}

/// Where a reference points: a path from a local and the type there, and
/// with `deep`, anywhere inside that. A path is followed `EXACT` steps
/// deep; a longer one stands for anything inside those. Under a reference a
/// call returned, `anchor` is its loan and the steps from where it points,
/// which compare exactly with those of another path under the same one.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Path {
    root: Local,
    steps: Vec<Step>,
    ty: Ty,
    deep: bool,
    anchor: Option<(usize, Vec<Step>)>,
}

/// A loan: the place it borrows (for `get`, the element at any index), the
/// paths that place may stand for once references in it are followed,
/// whether it lends `&var`, and where it was taken. A reference a call
/// returns holds a loan of its own, `from` a loan its arguments carry, deep
/// in its paths and anchored at itself; it is `multi` when references the
/// call returned in different turns may be live at once. A `Fact` is that a
/// local may hold a loan, and whether the reference it holds may point
/// anywhere inside the loan's place, as one a call stored through a `&var`.
struct Loan {
    place: Place,
    paths: Vec<Path>,
    mutable: bool,
    span: Span,
    from: Option<usize>,
    multi: bool,
}

impl Loan {
    /// A loan of `place` taken at `span`, its paths not known yet.
    fn new(place: Place, mutable: bool, span: Span, from: Option<usize>) -> Self {
        Loan {
            place,
            paths: Vec::new(),
            mutable,
            span,
            from,
            multi: false,
        }
    }
}

type Fact = (Local, usize, bool);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Role {
    Read,
    Move,
    Write,
    Realloc,
    Drop,
    Dead,
}

/// What an access may invalidate in a loan reaching under it.
#[derive(Clone, Copy, PartialEq)]
enum Rule {
    Move,
    Owning,
    Variant,
}

const EXACT: usize = 16;

/// What a call may do to what one of its arguments reaches: `links`, store
/// in it a reference into what the same pointer points at; `frees`, free or
/// move a part of it.
#[derive(Clone, Copy, PartialEq)]
pub(super) struct Effect {
    links: bool,
    frees: bool,
}

/// `methods` tells, for each function, whether its first parameter is
/// `self`, `effects` what it may do to each of its arguments, and `changes`
/// what each closure may change through each variable it captures.
pub(super) fn check(
    body: &Body,
    program: &Program,
    sources: &Sources,
    methods: &[bool],
    effects: &[Vec<Effect>],
    changes: &HashMap<Ty, Vec<bool>>,
) -> Vec<ZeruError> {
    let mut cx = Cx::new(body, program, sources, methods, effects, changes);
    cx.run();
    cx.errors
        .sort_by_key(|error| (error.span.start, error.span.end));
    cx.errors
}

/// The effects of each function, found by checking it. Only a body with a
/// parameter that can hold a reference into itself is checked, again until
/// no function changes; any other links nothing and may free anything.
pub(super) fn effects(
    bodies: &[Body],
    program: &Program,
    sources: &Sources,
    methods: &[bool],
    changes: &HashMap<Ty, Vec<bool>>,
) -> Vec<Vec<Effect>> {
    let can = |body: &Body| {
        body.locals[1..=body.arg_count].iter().any(|decl| {
            let ty = program.types.referent(decl.ty).unwrap_or(decl.ty);
            program.may_point_into(ty, ty, false)
        })
    };
    let mut effects: Vec<Vec<Effect>> = bodies
        .iter()
        .map(|body| {
            let effect = Effect {
                links: false,
                frees: !can(body),
            };
            vec![effect; body.arg_count]
        })
        .collect();
    let mut changed = true;
    while std::mem::take(&mut changed) {
        for (at, body) in bodies.iter().enumerate().filter(|(_, body)| can(body)) {
            let mut cx = Cx::new(body, program, sources, methods, &effects, changes);
            cx.run();
            let found: Vec<Effect> = (1..=body.arg_count)
                .map(|arg| Effect {
                    links: cx.linked.contains(&Local(arg as u32)),
                    frees: cx.freed.contains(&Local(arg as u32)),
                })
                .collect();
            changed |= found != effects[at];
            effects[at] = found;
        }
    }
    effects
}

/// For each closure, whether it may change, through each variable it
/// captures, what that variable refers to.
pub(super) fn changes(bodies: &[Body], program: &Program) -> HashMap<Ty, Vec<bool>> {
    let mut changes: HashMap<Ty, Vec<bool>> = HashMap::new();
    for body in bodies.iter().filter(|body| !body.captures.is_empty()) {
        let closure = program
            .types
            .referent(body.locals[1].ty)
            .expect("a closure's environment");
        let known = changes
            .entry(closure)
            .or_insert_with(|| vec![false; body.captures.len()]);
        for (field, known) in known.iter_mut().enumerate() {
            *known |= changes_through(body, program, field as u32);
        }
    }
    changes
}

/// Whether the closure `body` may change what the variable it captures in
/// `field` refers to: by an access through it that is not a read, or by
/// reading out of it something that can change what it reaches. A copy into
/// a local is followed through that local instead.
fn changes_through(body: &Body, program: &Program, field: u32) -> bool {
    let writes = |ty: Ty| program.writes(ty, &mut Vec::new());
    let captured = [Projection::Deref, Projection::Field(field)];
    let mut copies = Vec::new();
    for (b, block) in body.blocks.iter().enumerate() {
        for (at, statement) in block.statements.iter().enumerate() {
            if let StatementKind::Assign(
                dest,
                Rvalue::Use(Operand::Copy(source))
                | Rvalue::AddressOf(source)
                | Rvalue::VecGet(source, _),
            ) = &statement.kind
            {
                copies.push(((b, at), dest, source));
            }
        }
    }
    let mut held = vec![false; body.locals.len()];
    let under = |place: &Place, held: &[bool]| {
        held[place.local.0 as usize]
            || place.local == Local(1)
                && (place.projection.starts_with(&captured)
                    || captured.starts_with(&place.projection))
    };
    while let Some(local) = copies.iter().find_map(|(_, dest, source)| {
        let local = dest.as_local()?;
        let ty = body.locals[local.0 as usize].ty;
        (!held[local.0 as usize] && under(source, &held) && writes(ty)).then_some(local)
    }) {
        held[local.0 as usize] = true;
    }
    (0..body.blocks.len()).any(|b| {
        (0..=body.blocks[b].statements.len()).any(|at| {
            body.accesses(b, at).iter().any(|(place, role)| {
                if !under(place, &held) {
                    return false;
                }
                let copy = copies
                    .iter()
                    .find(|(site, _, source)| *site == (b, at) && *source == place);
                match (role, copy) {
                    (Role::Read, Some((_, dest, _))) => {
                        dest.as_local().is_none() && writes(body.place_ty(program, dest))
                    }
                    (Role::Read, None) => writes(body.place_ty(program, place)),
                    (Role::Write, _) => place.as_local().is_none(),
                    (Role::Dead, _) => false,
                    _ => true,
                }
            })
        })
    })
}

/// The borrow checker of one body. `made` is the loan each statement that
/// takes one takes; `moves` each move out of a local that owns something,
/// listed by statement in `moves_at` and by local in `moves_of`; `holders`
/// marks the locals that can hold a reference, the only ones liveness
/// follows; `start` the loans the parameters hold as the function starts;
/// `derived` the loan a call's result holds for each loan of its arguments;
/// `behind` the loans a local holds behind the pointer it is, and where,
/// stored there for its caller to see, and so held until it ends; `linked`
/// the locals that hold one of them behind the same pointer it points
/// through; `freed` those behind which a part may be freed or moved;
/// `readers` the loan that only reads made for each that writes;
/// `sealed` the loans a closure holds of a variable it only reads through;
/// `grew` that a loan's paths grew after blocks had used them.
struct Cx<'a> {
    body: &'a Body,
    program: &'a Program,
    sources: &'a Sources,
    methods: &'a [bool],
    effects: &'a [Vec<Effect>],
    changes: &'a HashMap<Ty, Vec<bool>>,
    loans: Vec<Loan>,
    made: HashMap<(usize, usize), usize>,
    moves: Vec<(Local, Span)>,
    moves_at: HashMap<(usize, usize), Vec<usize>>,
    moves_of: Vec<Vec<usize>>,
    holders: Vec<bool>,
    parts: RefCell<HashMap<(Ty, Part), bool>>,
    start: BTreeSet<Fact>,
    derived: HashMap<(usize, usize), usize>,
    behind: HashMap<(Local, usize), Path>,
    linked: HashSet<Local>,
    freed: HashSet<Local>,
    readers: HashMap<usize, usize>,
    sealed: HashSet<usize>,
    live_in: Vec<BTreeSet<Local>>,
    grew: bool,
    reported: HashSet<usize>,
    errors: Vec<ZeruError>,
}

impl<'a> Cx<'a> {
    fn new(
        body: &'a Body,
        program: &'a Program,
        sources: &'a Sources,
        methods: &'a [bool],
        effects: &'a [Vec<Effect>],
        changes: &'a HashMap<Ty, Vec<bool>>,
    ) -> Self {
        let mut cx = Self {
            body,
            program,
            sources,
            methods,
            effects,
            changes,
            loans: Vec::new(),
            made: HashMap::new(),
            moves: Vec::new(),
            moves_at: HashMap::new(),
            moves_of: vec![Vec::new(); body.locals.len()],
            holders: Vec::new(),
            parts: RefCell::new(HashMap::new()),
            start: BTreeSet::new(),
            derived: HashMap::new(),
            behind: HashMap::new(),
            linked: HashSet::new(),
            freed: HashSet::new(),
            readers: HashMap::new(),
            sealed: HashSet::new(),
            live_in: Vec::new(),
            grew: false,
            reported: HashSet::new(),
            errors: Vec::new(),
        };
        for arg in 1..=body.arg_count {
            let local = Local(arg as u32);
            let ty = body.locals[arg].ty;
            if let Some(pointee) = program.types.referent(ty) {
                let place = Place::local(local).project(Projection::Deref);
                let path = Path {
                    root: local,
                    steps: vec![Step::Deref],
                    ty: pointee,
                    deep: false,
                    anchor: None,
                };
                cx.start.insert((local, cx.loans.len(), false));
                let mutable = matches!(program.types.kind(ty), TyKind::RefMut(_));
                cx.loans.push(Loan {
                    paths: vec![path],
                    ..Loan::new(place, mutable, body.span, None)
                });
            }
        }
        cx.holders = body
            .locals
            .iter()
            .map(|decl| cx.has(decl.ty, Part::Ref))
            .collect();
        for (b, block) in body.blocks.iter().enumerate() {
            for (at, statement) in block.statements.iter().enumerate() {
                let StatementKind::Assign(dest, rvalue) = &statement.kind else {
                    continue;
                };
                let place = match rvalue {
                    Rvalue::AddressOf(place) if cx.is_ref(dest) => place.clone(),
                    Rvalue::VecGet(place, _) => place.clone().project(Projection::Index(RETURN)),
                    _ => continue,
                };
                cx.made.insert((b, at), cx.loans.len());
                let mutable = matches!(cx.kind(cx.ty(dest)), TyKind::RefMut(_));
                cx.loans
                    .push(Loan::new(place, mutable, statement.span, None));
            }
            for at in 0..=block.statements.len() {
                for (place, role) in cx.body.accesses(b, at) {
                    if role == Role::Move && cx.tracked(place.local) {
                        cx.moves_at.entry((b, at)).or_default().push(cx.moves.len());
                        cx.moves_of[place.local.0 as usize].push(cx.moves.len());
                        cx.moves.push((place.local, cx.span(b, at)));
                    }
                }
            }
        }
        cx
    }

    fn run(&mut self) {
        self.live_in = self.liveness();
        let facts = self.facts();
        let moved = self.moved();
        for b in 0..self.body.blocks.len() {
            if let Some(facts) = &facts[b] {
                self.check_block(b, facts.clone(), moved[b].clone());
            }
        }
    }

    fn ty(&self, place: &Place) -> Ty {
        self.body.place_ty(self.program, place)
    }

    fn kind(&self, ty: Ty) -> &TyKind {
        self.program.types.kind(ty)
    }

    fn is_ref(&self, place: &Place) -> bool {
        self.program.types.referent(self.ty(place)).is_some()
    }

    /// Whether moves out of `local` are followed: it owns something.
    fn tracked(&self, local: Local) -> bool {
        local != RETURN
            && self
                .program
                .needs_drop(self.body.locals[local.0 as usize].ty)
    }

    fn span(&self, b: usize, at: usize) -> Span {
        let block = &self.body.blocks[b];
        match block.statements.get(at) {
            Some(statement) => statement.span,
            None => block.terminator.span,
        }
    }

    fn name(&self, local: Local) -> Option<&str> {
        self.body.locals[local.0 as usize].name.as_deref()
    }

    /// Whether a value of `ty`, not looking through pointers, has a part
    /// of kind `part`.
    fn has(&self, ty: Ty, part: Part) -> bool {
        if let Some(&known) = self.parts.borrow().get(&(ty, part)) {
            return known;
        }
        let found = self.program.reaches(ty, part, &mut Vec::new());
        self.parts.borrow_mut().insert((ty, part), found);
        found
    }

    /// Whether `role` on `place` reads the local it starts from.
    fn uses(&self, place: &Place, role: Role) -> bool {
        match role {
            Role::Dead => false,
            Role::Write => self.through_pointer(place),
            Role::Drop => {
                !place.projection.is_empty()
                    || self.has(self.body.locals[place.local.0 as usize].ty, Part::Drop)
            }
            _ => true,
        }
    }

    /// Whether `place` is reached through a pointer its local holds: a
    /// reference followed, or an element of a slice.
    fn through_pointer(&self, place: &Place) -> bool {
        let mut ty = self.body.locals[place.local.0 as usize].ty;
        let mut variant = None;
        place.projection.iter().any(|&projection| {
            let step = self.step(ty, projection, variant.is_some());
            ty = self.program.project_ty(ty, projection, &mut variant);
            step == Step::Deref
        })
    }

    /// Whether `role` on `place` gives its whole local a new value or ends it.
    fn defines(place: &Place, role: Role) -> bool {
        matches!(role, Role::Write | Role::Dead) && place.projection.is_empty()
    }

    fn successors(&self, b: usize) -> Vec<usize> {
        let kind = &self.body.blocks[b].terminator.kind;
        let taken = match kind {
            TerminatorKind::SwitchInt {
                discr: Operand::Constant(constant),
                targets,
                otherwise,
            } => {
                let value = match constant.value {
                    super::ConstValue::Int(value) => value,
                    super::ConstValue::Bool(value) => i128::from(value),
                    _ => return kind.successors().iter().map(|t| t.0 as usize).collect(),
                };
                let chosen = targets.iter().find(|(v, _)| *v == value);
                vec![chosen.map_or(*otherwise, |(_, target)| *target)]
            }
            _ => kind.successors(),
        };
        taken.into_iter().map(|BlockId(b)| b as usize).collect()
    }

    /// The locals each block may use before giving them a new value,
    /// found from each use back to the definitions that reach it, so the
    /// work grows with how long locals live rather than with how many there
    /// are.
    fn liveness(&self) -> Vec<BTreeSet<Local>> {
        let count = self.body.blocks.len();
        let mut preds = vec![Vec::new(); count];
        let mut uses: HashMap<Local, Vec<(usize, usize)>> = HashMap::new();
        let mut defs: HashMap<Local, Vec<(usize, usize)>> = HashMap::new();
        for b in 0..count {
            for next in self.successors(b) {
                preds[next].push(b);
            }
            for at in 0..=self.body.blocks[b].statements.len() {
                for (place, role) in self.body.accesses(b, at) {
                    if !self.holders[place.local.0 as usize] {
                        continue;
                    }
                    if self.uses(&place, role) {
                        uses.entry(place.local).or_default().push((b, at));
                    }
                    if Self::defines(&place, role) {
                        defs.entry(place.local).or_default().push((b, at));
                    }
                }
            }
        }
        let mut live_in = vec![BTreeSet::new(); count];
        for (local, used) in uses {
            let defined = defs.get(&local).map_or(&[][..], Vec::as_slice);
            let mut seen = HashSet::new();
            let mut work = Vec::new();
            for (b, at) in used {
                if !defined.iter().any(|&(d, i)| d == b && i < at) && seen.insert(b) {
                    work.push(b);
                }
            }
            while let Some(b) = work.pop() {
                live_in[b].insert(local);
                for &p in &preds[b] {
                    if !defined.iter().any(|&(d, _)| d == p) && seen.insert(p) {
                        work.push(p);
                    }
                }
            }
        }
        live_in
    }

    /// The facts that matter where the locals in `live` are live: those of
    /// a live local or held behind one, and of a local a loan they hold
    /// points into, which a reference may read the loans it holds through.
    fn alive(&self, facts: &BTreeSet<Fact>, live: &BTreeSet<Local>) -> BTreeSet<Fact> {
        let mut kept: BTreeSet<Fact> = facts
            .iter()
            .filter(|fact| live.contains(&fact.0) || self.behind.contains_key(&(fact.0, fact.1)))
            .copied()
            .collect();
        let mut work: Vec<Fact> = kept.iter().copied().collect();
        while let Some((_, loan, _)) = work.pop() {
            for path in &self.loans[loan].paths {
                if path.steps.contains(&Step::Deref) {
                    continue;
                }
                for (held, deep) in Self::held(facts, path.root) {
                    if kept.insert((path.root, held, deep)) {
                        work.push((path.root, held, deep));
                    }
                }
            }
        }
        kept
    }

    fn live_out(&self, b: usize) -> BTreeSet<Local> {
        self.successors(b)
            .into_iter()
            .flat_map(|next| self.live_in[next].iter().copied())
            .collect()
    }

    /// The locals live before each statement of block `b` and its
    /// terminator.
    fn live_through(&self, b: usize) -> Vec<BTreeSet<Local>> {
        let count = self.body.blocks[b].statements.len();
        let mut live = self.live_out(b);
        let mut before = vec![BTreeSet::new(); count + 1];
        for at in (0..=count).rev() {
            let accesses = self.body.accesses(b, at);
            for (place, role) in &accesses {
                if Self::defines(place, *role) {
                    live.remove(&place.local);
                }
            }
            for (place, role) in &accesses {
                if self.holders[place.local.0 as usize] && self.uses(place, *role) {
                    live.insert(place.local);
                }
            }
            before[at] = live.clone();
        }
        before
    }

    /// The step `projection` takes from a `ty`, `variant` saying whether a
    /// `Downcast` came just before.
    fn step(&self, ty: Ty, projection: Projection, variant: bool) -> Step {
        match (projection, self.kind(ty)) {
            (Projection::Deref, _) => Step::Deref,
            (Projection::Field(at), TyKind::Optional(_) | TyKind::Result(..)) if !variant => {
                Step::Variant(at)
            }
            (Projection::Field(at), _) => Step::Field(at),
            (Projection::Index(_), TyKind::Slice(_) | TyKind::SliceMut(_)) => Step::Deref,
            (Projection::Index(_), kind) => Step::Index {
                heap: matches!(kind, TyKind::Vec(_)),
            },
            (Projection::Downcast(at), _) => Step::Variant(at),
        }
    }

    /// The loans `local` may hold.
    fn held(facts: &BTreeSet<Fact>, local: Local) -> Vec<(usize, bool)> {
        facts
            .range((local, 0, false)..=(local, usize::MAX, true))
            .map(|&(_, loan, deep)| (loan, deep))
            .collect()
    }

    /// The paths `place` may stand for: through a pointer a local holds, the
    /// paths of the loans it may hold; through one of unknown origin, such
    /// as a parameter's, itself.
    fn canonical(&self, place: &Place, facts: &BTreeSet<Fact>) -> Vec<Path> {
        let mut ty = self.body.locals[place.local.0 as usize].ty;
        let mut paths = vec![Path {
            root: place.local,
            steps: Vec::new(),
            ty,
            deep: false,
            anchor: None,
        }];
        let mut variant = None;
        for &projection in &place.projection {
            let step = self.step(ty, projection, variant.is_some());
            ty = self.program.project_ty(ty, projection, &mut variant);
            paths = paths
                .into_iter()
                .flat_map(|path| match step {
                    Step::Deref if !path.deep && !path.steps.contains(&Step::Deref) => {
                        let held = Self::held(facts, path.root);
                        let pointed: Vec<Path> = held
                            .into_iter()
                            .filter(|&(loan, _)| !self.behind.contains_key(&(path.root, loan)))
                            .flat_map(|(loan, deep)| {
                                self.loans[loan].paths.iter().map(move |p| inside(p, deep))
                            })
                            .collect();
                        match pointed.is_empty() {
                            true => vec![extend(path, step, ty)],
                            false => pointed,
                        }
                    }
                    _ => vec![extend(path, step, ty)],
                })
                .collect();
        }
        paths.sort();
        paths.dedup();
        paths
    }

    /// The loans the value read from `place` may hold.
    fn carried(&self, place: &Place, facts: &BTreeSet<Fact>) -> Vec<(usize, bool)> {
        if !self.has(self.ty(place), Part::Ref) {
            return Vec::new();
        }
        self.canonical(place, facts)
            .iter()
            .filter(|path| !path.steps.contains(&Step::Deref))
            .flat_map(|path| Self::held(facts, path.root))
            .collect()
    }

    fn carried_operand(&self, value: &Operand, facts: &BTreeSet<Fact>) -> Vec<(usize, bool)> {
        match value {
            Operand::Copy(place) | Operand::Move(place) => self.carried(place, facts),
            Operand::Constant(_) => Vec::new(),
        }
    }

    /// `loans`, with the loans held by what they point into, and so on:
    /// all that a value holding `loans` reaches, or with `writing`, all it
    /// may change, which stops at a variable a closure only reads.
    fn reached(
        &self,
        mut loans: Vec<(usize, bool)>,
        facts: &BTreeSet<Fact>,
        writing: bool,
    ) -> Vec<(usize, bool)> {
        let mut next = 0;
        while let Some(&(loan, _)) = loans.get(next) {
            next += 1;
            if writing && self.sealed.contains(&loan) {
                continue;
            }
            for path in &self.loans[loan].paths {
                if path.steps.contains(&Step::Deref) {
                    continue;
                }
                for held in Self::held(facts, path.root) {
                    if !loans.contains(&held) {
                        loans.push(held);
                    }
                }
            }
        }
        loans
    }

    /// The loans that write which a call given `arg` may change what they
    /// point at through it: none when its type lets nothing change, and none
    /// past a variable a closure captures only to read.
    fn writable(&self, arg: &Operand, facts: &BTreeSet<Fact>) -> Vec<usize> {
        let ty = self.body.operand_ty(self.program, arg);
        if !self.program.writes(ty, &mut Vec::new()) {
            return Vec::new();
        }
        self.reached(self.carried_operand(arg, facts), facts, true)
            .into_iter()
            .map(|(loan, _)| loan)
            .filter(|&loan| self.loans[loan].mutable)
            .collect()
    }

    /// The loans the value of `rvalue`, computed at statement `at`, holds.
    /// A reference taken there holds its own loan only: the loans held by
    /// what it points at are reached through it.
    fn carried_rvalue(
        &mut self,
        rvalue: &Rvalue,
        at: (usize, usize),
        facts: &BTreeSet<Fact>,
    ) -> Vec<(usize, bool)> {
        match rvalue {
            Rvalue::AddressOf(place) | Rvalue::VecGet(place, _) => {
                let Some(&loan) = self.made.get(&at) else {
                    return self.carried(place, facts);
                };
                let mut paths = self.canonical(&self.loans[loan].place, facts);
                paths.extend(self.loans[loan].paths.iter().cloned());
                paths.sort();
                paths.dedup();
                if paths != self.loans[loan].paths {
                    let before = std::mem::replace(&mut self.loans[loan].paths, paths);
                    self.grew |= !before.is_empty();
                }
                vec![(loan, false)]
            }
            Rvalue::DeepCopy(place)
            | Rvalue::VecPop(place)
            | Rvalue::VecRemove(place, _)
            | Rvalue::Take(place) => self.carried(place, facts),
            Rvalue::Binary {
                op: BinOp::PtrAdd | BinOp::PtrSub,
                left,
                ..
            } => self.carried_operand(left, facts),
            Rvalue::Use(_) | Rvalue::Aggregate(..) | Rvalue::Repeat(..) | Rvalue::VecRepeat(..) => {
                rvalue
                    .operands()
                    .into_iter()
                    .flat_map(|value| self.carried_operand(value, facts))
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Store a value holding `carried` at `dest`: a whole local holds only
    /// those from now on, a part of one adds them. Returns those stored
    /// behind a pointer of unknown origin.
    fn store(
        &mut self,
        facts: &mut BTreeSet<Fact>,
        dest: &Place,
        carried: Vec<(usize, bool)>,
    ) -> Vec<(usize, bool)> {
        if let Some(local) = dest.as_local() {
            facts.retain(|fact| fact.0 != local);
        }
        if carried.is_empty() || !self.has(self.ty(dest), Part::Ref) {
            return Vec::new();
        }
        let paths = self.canonical(dest, facts);
        self.hold(facts, paths, carried)
    }

    /// The roots of `paths` may hold `carried` from now on, behind the
    /// pointer they are when a path follows one. Returns those stored behind
    /// a pointer of unknown origin.
    fn hold(
        &mut self,
        facts: &mut BTreeSet<Fact>,
        paths: Vec<Path>,
        carried: Vec<(usize, bool)>,
    ) -> Vec<(usize, bool)> {
        let mut escaped = Vec::new();
        for path in paths {
            if let Some(end) = path.steps.iter().rposition(|&step| step == Step::Deref) {
                escaped.extend(carried.iter().copied());
                let pointer = &path.steps[..=end];
                for &(loan, _) in &carried {
                    if self.loans[loan]
                        .paths
                        .iter()
                        .any(|p| p.root == path.root && p.steps.starts_with(pointer))
                    {
                        self.linked.insert(path.root);
                    }
                    self.behind
                        .entry((path.root, loan))
                        .or_insert_with(|| path.clone());
                }
            }
            facts.extend(carried.iter().map(|&(loan, deep)| (path.root, loan, deep)));
        }
        escaped
    }

    /// Apply statement `at` of block `b` to `facts`, returning the loans
    /// it stores behind a pointer of unknown origin.
    fn transfer(&mut self, facts: &mut BTreeSet<Fact>, b: usize, at: usize) -> Vec<(usize, bool)> {
        let body = self.body;
        let block = &body.blocks[b];
        let Some(statement) = block.statements.get(at) else {
            let TerminatorKind::Call {
                function,
                args,
                dest,
                ..
            } = &block.terminator.kind
            else {
                return Vec::new();
            };
            let method = self.methods[function.0 as usize];
            let mut returnable = Vec::new();
            let mut carried = Vec::new();
            let mut linked = Vec::new();
            for (at, arg) in args.iter().enumerate() {
                let ty = self.body.operand_ty(self.program, arg);
                let env = self
                    .program
                    .types
                    .referent(ty)
                    .is_some_and(|ty| matches!(self.kind(ty), TyKind::Closure(_)));
                let inside = match arg {
                    Operand::Copy(place) | Operand::Move(place)
                        if env || self.lent(method, at, ty) =>
                    {
                        let inside = place.clone().project(Projection::Deref);
                        Some(self.reached(self.carried(&inside, facts), facts, false))
                    }
                    _ => None,
                };
                let loans = match (env, &inside) {
                    (true, Some(inside)) => inside.clone(),
                    _ => self.reached(self.carried_operand(arg, facts), facts, false),
                };
                if self.effects[function.0 as usize][at].links {
                    linked.extend(loans.iter().map(|&(loan, _)| loan));
                }
                returnable.extend(inside.unwrap_or_else(|| loans.clone()));
                carried.extend(loans.into_iter().map(|(loan, _)| (loan, true)));
            }
            let mut escaped = Vec::new();
            let writable: Vec<usize> = args
                .iter()
                .flat_map(|arg| self.writable(arg, facts))
                .collect();
            for loan in writable {
                for path in self.loans[loan].paths.clone() {
                    let mut flowing: Vec<(usize, bool)> = carried
                        .iter()
                        .filter(|&&(other, _)| {
                            (other != loan || linked.contains(&loan))
                                && self.may_point_into(path.ty, other)
                        })
                        .copied()
                        .collect();
                    if !self.has(path.ty, Part::RefMut) {
                        for (other, _) in &mut flowing {
                            if *other == loan {
                                *other = self.reader(loan);
                            }
                        }
                    }
                    returnable.extend(flowing.iter().copied());
                    escaped.extend(self.hold(facts, vec![path], flowing));
                }
            }
            if let Some(dest) = dest {
                let returned = self.derive(b, &returnable, facts, dest);
                escaped.extend(self.store(facts, dest, returned));
            }
            return escaped;
        };
        match &statement.kind {
            StatementKind::Assign(dest, rvalue) => {
                if let (Rvalue::Aggregate(_, values), Some(changes)) =
                    (rvalue, self.changes.get(&self.ty(dest)))
                {
                    for (value, _) in values.iter().zip(changes).filter(|(_, changes)| !**changes) {
                        let sealed = self.carried_operand(value, facts);
                        self.sealed.extend(sealed.into_iter().map(|(loan, _)| loan));
                    }
                }
                let carried = self.carried_rvalue(rvalue, (b, at), facts);
                self.store(facts, dest, carried)
            }
            StatementKind::VecPush(vec, item) | StatementKind::VecInsert(vec, _, item) => {
                let carried = self.carried_operand(item, facts);
                self.store(facts, vec, carried)
            }
            StatementKind::StorageDead(local) => {
                facts.retain(|fact| fact.0 != *local);
                Vec::new()
            }
            _ => Vec::new(),
        }
    }

    /// The loans the result of the call ending block `b`, stored at `dest`,
    /// holds: for each loan its arguments carry, one of its own. It is
    /// `multi` when a reference under one the call returned before is
    /// still live.
    fn derive(
        &mut self,
        b: usize,
        carried: &[(usize, bool)],
        facts: &BTreeSet<Fact>,
        dest: &Place,
    ) -> Vec<(usize, bool)> {
        let into = self.ty(dest);
        if !self.has(into, Part::Ref) {
            return Vec::new();
        }
        let live = self.alive(facts, &self.live_out(b));
        let mut returned = Vec::new();
        for &(loan, _) in carried {
            let origin = self.loans[loan].from.unwrap_or(loan);
            if !self.may_point_into(into, loan) {
                continue;
            }
            let derived = *self.derived.entry((b, origin)).or_insert_with(|| {
                let Loan {
                    place,
                    mutable,
                    span,
                    ..
                } = &self.loans[origin];
                let loan = Loan::new(place.clone(), *mutable, *span, Some(origin));
                self.loans.push(loan);
                self.loans.len() - 1
            });
            let paths: Vec<Path> = self.loans[origin]
                .paths
                .iter()
                .map(|path| Path {
                    deep: true,
                    anchor: Some((derived, Vec::new())),
                    ..path.clone()
                })
                .collect();
            if paths != self.loans[derived].paths {
                let before = std::mem::replace(&mut self.loans[derived].paths, paths);
                self.grew |= !before.is_empty();
            }
            self.loans[derived].multi |= live.iter().any(|&(holder, held, _)| {
                Some(holder) != dest.as_local()
                    && self.loans[held]
                        .paths
                        .iter()
                        .any(|path| path.anchor.as_ref().is_some_and(|a| a.0 == derived))
            });
            returned.push((derived, false));
        }
        returned
    }

    /// Whether a value of `ty` may point into what `loan`, or the loan it
    /// comes from, borrows.
    fn may_point_into(&self, ty: Ty, loan: usize) -> bool {
        let place = &self.loans[self.loans[loan].from.unwrap_or(loan)].place;
        let element = matches!(place.projection.last(), Some(Projection::Index(_)));
        self.program.may_point_into(ty, self.ty(place), element)
    }

    /// A loan of what `loan` borrows that only reads, for the references a
    /// call stores from it where only those fit.
    fn reader(&mut self, loan: usize) -> usize {
        let reader = *self.readers.entry(loan).or_insert_with(|| {
            let Loan {
                place, span, from, ..
            } = &self.loans[loan];
            let reader = Loan::new(place.clone(), false, *span, Some(from.unwrap_or(loan)));
            self.loans.push(reader);
            self.loans.len() - 1
        });
        if self.loans[loan].paths != self.loans[reader].paths {
            let paths = self.loans[loan].paths.clone();
            let before = std::mem::replace(&mut self.loans[reader].paths, paths);
            self.grew |= !before.is_empty();
        }
        reader
    }

    /// The loans each block may start with, none for one never reached.
    fn facts(&mut self) -> Vec<Option<BTreeSet<Fact>>> {
        let count = self.body.blocks.len();
        let mut entry: Vec<Option<BTreeSet<Fact>>> = vec![None; count];
        entry[0] = Some(self.start.clone());
        let mut queued = vec![false; count];
        queued[0] = true;
        let mut work = VecDeque::from([0]);
        while let Some(b) = work.pop_front() {
            queued[b] = false;
            let mut facts = entry[b].clone().expect("queued once reached");
            for at in 0..=self.body.blocks[b].statements.len() {
                self.transfer(&mut facts, b, at);
            }
            let mut wake = Vec::new();
            for next in self.successors(b) {
                let flowing = self.alive(&facts, &self.live_in[next]);
                let changed = match &mut entry[next] {
                    Some(known) => {
                        let before = known.len();
                        known.extend(flowing);
                        known.len() != before
                    }
                    slot => {
                        *slot = Some(flowing);
                        true
                    }
                };
                if changed {
                    wake.push(next);
                }
            }
            if std::mem::take(&mut self.grew) {
                wake.extend((0..count).filter(|&b| entry[b].is_some()));
            }
            for next in wake {
                if !std::mem::replace(&mut queued[next], true) {
                    work.push_back(next);
                }
            }
        }
        entry
    }

    /// The moves that may have happened before each block.
    fn moved(&self) -> Vec<Vec<bool>> {
        let count = self.body.blocks.len();
        let mut entry = vec![vec![false; self.moves.len()]; count];
        let mut seen = vec![false; count];
        let mut queued = vec![false; count];
        let mut work = VecDeque::from([0]);
        while let Some(b) = work.pop_front() {
            queued[b] = false;
            let mut moved = entry[b].clone();
            for at in 0..=self.body.blocks[b].statements.len() {
                self.apply_moves(&mut moved, b, at);
            }
            for next in self.successors(b) {
                let before = entry[next].clone();
                entry[next]
                    .iter_mut()
                    .zip(&moved)
                    .for_each(|(a, &b)| *a |= b);
                let changed = entry[next] != before || !std::mem::replace(&mut seen[next], true);
                if changed && !std::mem::replace(&mut queued[next], true) {
                    work.push_back(next);
                }
            }
        }
        entry
    }

    fn apply_moves(&self, moved: &mut [bool], b: usize, at: usize) {
        for (place, role) in self.body.accesses(b, at) {
            if Self::defines(&place, role) {
                for &site in &self.moves_of[place.local.0 as usize] {
                    moved[site] = false;
                }
            }
        }
        for &site in self.moves_at.get(&(b, at)).into_iter().flatten() {
            moved[site] = true;
        }
    }

    fn check_block(&mut self, b: usize, mut facts: BTreeSet<Fact>, mut moved: Vec<bool>) {
        let live = self.live_through(b);
        for (at, live) in live.iter().enumerate() {
            let span = self.span(b, at);
            let live_facts: Vec<Fact> = self.alive(&facts, live).into_iter().collect();
            for (place, role) in self.body.accesses(b, at) {
                let errors = self.errors.len();
                self.check_access(&place, role, &facts, &live_facts, (b, at));
                if self.errors.len() == errors
                    && !Self::defines(&place, role)
                    && (role != Role::Drop || !place.projection.is_empty())
                {
                    self.check_moved(place.local, &moved, span);
                }
            }
            match (
                self.body.blocks[b].statements.get(at),
                &self.body.blocks[b].terminator.kind,
            ) {
                (None, TerminatorKind::Call { function, args, .. }) => {
                    let effects = &self.effects[function.0 as usize];
                    self.check_call(effects, args, &facts, &live_facts, (b, at))
                }
                (None, TerminatorKind::Return) => self.check_return(&facts),
                _ => {}
            }
            for (loan, _) in self.transfer(&mut facts, b, at) {
                self.escaped(loan);
            }
            self.apply_moves(&mut moved, b, at);
        }
    }

    fn check_moved(&mut self, local: Local, moved: &[bool], span: Span) {
        let mut sites = self.moves_of[local.0 as usize]
            .iter()
            .copied()
            .filter(|&site| moved[site]);
        let Some(first) = sites.next() else {
            return;
        };
        let name = self.name(local).unwrap_or_default().to_string();
        let again = std::iter::once(first)
            .chain(sites)
            .any(|site| self.moves[site].1 == span);
        let moved_at = self.moves[first].1;
        let copyable = !self.has(self.body.locals[local.0 as usize].ty, Part::Drop);
        let fixed = self.sources.text(moved_at) == name;
        let error = match again {
            true => self
                .error(
                    format!("Cannot move '{name}' inside a loop: the next turn would use it again"),
                    span,
                )
                .label("moved here, once in every turn"),
            false => self
                .error(format!("'{name}' is used after it was moved"), span)
                .label("used here after the move")
                .note(format!("'{name}' is moved here"), moved_at),
        };
        match copyable {
            true if fixed => error
                .help(format!("to keep '{name}', move a copy of it instead"))
                .fix(moved_at, format!("{name}.copy()")),
            true => error.help(format!(
                "to keep '{name}', move a copy of it instead: {name}.copy()"
            )),
            false => error.help(format!(
                "'{name}' runs a drop, so it cannot be copied; use it before it is moved"
            )),
        };
    }

    fn check_access(
        &mut self,
        place: &Place,
        role: Role,
        facts: &BTreeSet<Fact>,
        live: &[Fact],
        at: (usize, usize),
    ) {
        let rule = match role {
            Role::Read => return,
            Role::Dead => return self.check_dead(place.local, live, at),
            Role::Move => Rule::Move,
            Role::Write => Rule::Variant,
            Role::Drop if !self.replaced(at.0, place) => return,
            Role::Drop | Role::Realloc => Rule::Owning,
        };
        let accessed = self.canonical(place, facts);
        self.frees(&accessed, rule);
        let span = self.span(at.0, at.1);
        self.conflict(&accessed, rule, live, at, span);
    }

    /// Note the locals behind which `rule`'s access to `accessed` may free
    /// or move a part.
    fn frees(&mut self, accessed: &[Path], rule: Rule) {
        let part = match rule {
            Rule::Variant => Part::Variant,
            _ => Part::Owning,
        };
        for path in accessed {
            if path.steps.contains(&Step::Deref) && (rule == Rule::Move || self.has(path.ty, part))
            {
                self.freed.insert(path.root);
            }
        }
    }

    /// A call may reallocate what each `&var` its arguments carry, directly
    /// or inside them, points at, when that owns memory. What it gets lent
    /// it may reach anywhere inside, so another loan an argument carries
    /// conflicts with that as a deep one. One whose `effects` free nothing
    /// keeps what an argument holds into itself.
    fn check_call(
        &mut self,
        effects: &[Effect],
        args: &[Operand],
        facts: &BTreeSet<Fact>,
        live: &[Fact],
        at: (usize, usize),
    ) {
        let carried: Vec<Vec<usize>> = args
            .iter()
            .map(|arg| {
                self.reached(self.carried_operand(arg, facts), facts, false)
                    .into_iter()
                    .map(|(loan, _)| loan)
                    .collect()
            })
            .collect();
        for (i, arg) in args.iter().enumerate() {
            let frees = effects[i].frees;
            for loan in self.writable(arg, facts) {
                let accessed: Vec<Path> = self.loans[loan]
                    .paths
                    .iter()
                    .filter(|path| self.has(path.ty, Part::Owning))
                    .cloned()
                    .collect();
                if frees {
                    self.frees(&accessed, Rule::Owning);
                }
                let shared = carried
                    .iter()
                    .enumerate()
                    .any(|(j, other)| j != i && other.contains(&loan));
                let live: Vec<Fact> = live
                    .iter()
                    .filter(|fact| fact.1 != loan || shared)
                    .filter(|fact| frees || !self.readers.values().any(|&r| r == fact.1))
                    .map(|&(holder, other, deep)| {
                        (
                            holder,
                            other,
                            deep || carried.iter().flatten().any(|&c| c == other),
                        )
                    })
                    .collect();
                let call = self.span(at.0, at.1);
                let lent = self.loans[loan].span;
                let inside = call.start < lent.start && lent.end <= call.end;
                self.conflict(
                    &accessed,
                    Rule::Owning,
                    &live,
                    at,
                    if inside { lent } else { call },
                );
            }
        }
    }

    /// Report each loan in `live` that `rule`'s access to `accessed`, at
    /// `span`, invalidates.
    fn conflict(
        &mut self,
        accessed: &[Path],
        rule: Rule,
        live: &[Fact],
        at: (usize, usize),
        span: Span,
    ) {
        for &(holder, loan, deep) in live {
            if self.reported.contains(&loan) {
                continue;
            }
            let hit = self.loans[loan].paths.iter().find_map(|path| {
                let path = inside(path, deep);
                accessed
                    .iter()
                    .find(|q| self.invalidates(q, &path, rule))
                    .cloned()
            });
            let Some(accessed) = hit else {
                continue;
            };
            let holder = self.best_holder(loan, holder, accessed.root, live);
            let what = self.describe_path(&accessed, "a temporary value");
            let named = match self.behind.get(&(holder, loan)) {
                Some(path) => self.describe_path(path, ""),
                None => self.describe(holder, ""),
            };
            let own = named == what;
            let by = match named.is_empty() {
                true => "a reference".to_string(),
                false => named,
            };
            let message = match (rule, by.as_str(), own) {
                (Rule::Move, _, true) => {
                    format!("Cannot move {what} while it holds a reference into itself")
                }
                (Rule::Move, "a reference", _) => {
                    format!("Cannot move {what} while a reference into it is still in use")
                }
                (Rule::Move, ..) => format!("Cannot move {what} while {by} still refers into it"),
                (_, _, true) => {
                    format!("Cannot change {what} while it holds a reference into itself")
                }
                (_, "a reference", _) => {
                    format!("Cannot change {what} while a reference into it is still in use")
                }
                _ => format!("Cannot change {what} while {by} still refers into it"),
            };
            let pointing = match own {
                true => "that reference".to_string(),
                false => by.clone(),
            };
            let label = match rule {
                Rule::Move => format!("this moves {what} away"),
                Rule::Variant => format!("this can change which variant {pointing} points into"),
                Rule::Owning => format!("this can free or move what {pointing} points at"),
            };
            let help = match (rule, by == "the loop") {
                (_, true) => format!(
                    "the loop walks {what}; collect the changes in another variable and make them after the loop"
                ),
                (Rule::Move, _) => {
                    format!("finish with {by} before moving {what}, or move a copy of it")
                }
                _ => format!(
                    "finish with {by} before this, or keep a copy of the value instead of a reference into {what}"
                ),
            };
            let later = self.later_use(at, holder, span);
            let borrowed = match (by.as_str(), own) {
                (_, true) => format!("{what} is given a reference into itself here"),
                ("a reference" | "the loop", _) => "borrowed here".to_string(),
                _ => format!("{by} borrows here"),
            };
            self.reported.insert(loan);
            let taken = self.loans[loan].span;
            let error = self.error(message, span).label(label).help(help);
            error.note(borrowed, taken);
            if let Some((note, at)) = later {
                error.note(note, at);
            }
        }
    }

    /// Whether `rule`'s access to `accessed` invalidates a loan on `loan`.
    /// An access reached through a reference a call derived from a `&`
    /// changes nothing, since nothing is written through a `&`, and one to a
    /// value nothing that a pointer inside it points at, unless the value can
    /// hold that.
    fn invalidates(&self, accessed: &Path, loan: &Path, rule: Rule) -> bool {
        let shared = accessed
            .anchor
            .as_ref()
            .is_some_and(|(at, _)| !self.loans[*at].mutable);
        if accessed.root != loan.root || shared {
            return false;
        }
        let (q, l, deep) = match (&accessed.anchor, &loan.anchor) {
            (Some((a, q)), Some((b, l))) if a == b && !self.loans[*a].multi => (q, l, false),
            _ => (&accessed.steps, &loan.steps, loan.deep),
        };
        let prefix = |a: &[Step], b: &[Step]| {
            a.len() <= b.len() && a.iter().zip(b).all(|(x, y)| same(*x, *y))
        };
        if rule == Rule::Move {
            return prefix(q, l) || prefix(l, q);
        }
        let step = |s: &Step| match rule {
            Rule::Variant => matches!(s, Step::Variant(_)),
            _ => matches!(s, Step::Variant(_) | Step::Index { heap: true }),
        };
        let below = match rule {
            Rule::Variant => Part::Variant,
            _ => Part::Owning,
        };
        if prefix(q, l) {
            let rest = &l[q.len()..];
            if rest.contains(&Step::Deref)
                && !deep
                && !self
                    .program
                    .stores(accessed.ty, loan.ty, false, &mut Vec::new())
            {
                return false;
            }
            return rest.iter().any(step) || (deep && self.has(loan.ty, below));
        }
        deep && prefix(l, q) && self.has(accessed.ty, below)
    }

    /// Whether the drop ending block `b`, of `place`, drops its old value
    /// for a new one: the next statement assigns it.
    fn replaced(&self, b: usize, place: &Place) -> bool {
        let TerminatorKind::Drop { target, .. } = self.body.blocks[b].terminator.kind else {
            return false;
        };
        let next = &self.body.blocks[target.0 as usize].statements;
        matches!(next.first().map(|s| &s.kind), Some(StatementKind::Assign(dest, _)) if dest == place)
    }

    /// Whether argument `at`, of type `ty`, of a call to a function that is
    /// a `method` or not, is lent for the call only: a method returns
    /// references into `self` only, so what its other `&T` arguments point
    /// at is not reached by what it returns, though the references a `T`
    /// holds may be.
    fn lent(&self, method: bool, at: usize, ty: Ty) -> bool {
        method && at > 0 && self.program.types.referent(ty).is_some()
    }

    /// A method returning a reference into an argument other than `self`
    /// that its callers lend for the call only.
    fn check_return(&mut self, facts: &BTreeSet<Fact>) {
        for (loan, _) in Self::held(facts, RETURN) {
            let lent = self.loans[loan]
                .paths
                .iter()
                .map(|path| path.root)
                .find(|root| {
                    let at = root.0 as usize;
                    (1..=self.body.arg_count).contains(&at)
                        && self.lent(self.body.method, at - 1, self.body.locals[at].ty)
                });
            let Some(param) = lent else {
                continue;
            };
            if !self.reported.insert(loan) {
                continue;
            }
            let name = self.name(param).unwrap_or_default().to_string();
            let message =
                format!("A method returns references into 'self' only, not into '{name}'");
            let span = self.loans[loan].span;
            self.error(message, span)
                .label(format!("this refers into '{name}'"))
                .help("return a reference into self, or make this a function, which may return references into any argument");
        }
    }

    /// A local that ends while a loan into its own storage is live.
    fn check_dead(&mut self, local: Local, live: &[Fact], at: (usize, usize)) {
        for &(holder, loan, _) in live {
            let inside = self.loans[loan]
                .paths
                .iter()
                .any(|path| path.root == local && !path.steps.contains(&Step::Deref));
            if holder == local || !inside || self.reported.contains(&loan) {
                continue;
            }
            let holder = self.best_holder(loan, holder, local, live);
            let dropped = holder != RETURN
                && self.next_use(at, holder).is_some_and(|(b, at)| {
                    self.body.accesses(b, at).iter().any(|(place, role)| {
                        *role == Role::Drop && place.as_local() == Some(holder)
                    })
                });
            let what = self.describe(local, "a temporary value");
            let ty = self
                .program
                .types
                .show(self.body.locals[local.0 as usize].ty);
            let (message, label, help) = match (holder == RETURN, self.name(local), self.name(holder)) {
                (true, ..) => (
                    format!("Cannot return a reference into {what}, which ends with this function"),
                    format!("this refers into {what}"),
                    Some(format!("return the {ty} itself, by value, instead of a reference into it")),
                ),
                (false, None, _) => (
                    "Cannot keep a reference into a temporary value past its statement".to_string(),
                    "this refers into a value that is freed when the statement ends".to_string(),
                    Some("store the value in a variable first, and take the reference from that variable".to_string()),
                ),
                (false, Some(name), Some(holder)) if dropped => (
                    format!("'{holder}' runs its drop after '{name}' is gone, but still refers into it"),
                    format!("this refers into '{name}'"),
                    Some(format!("declare '{name}' before '{holder}', so that '{holder}' is dropped first")),
                ),
                (false, Some(name), holder) => (
                    match holder {
                        Some(holder) => format!("'{holder}' refers into '{name}', which ends with its block first"),
                        None => format!("A reference refers into '{name}', which ends with its block first"),
                    },
                    format!("this refers into '{name}'"),
                    (!self.body.locals[local.0 as usize].looped).then(|| {
                        format!("declare '{name}' before the block, so that it lives as long as what refers into it")
                    }),
                ),
            };
            let later = match holder {
                RETURN => None,
                _ if dropped => None,
                _ => self.later_use(at, holder, self.loans[loan].span),
            };
            self.reported.insert(loan);
            let span = self.loans[loan].span;
            let error = self.error(message, span).label(label);
            if let Some(help) = help {
                error.help(help);
            }
            if let Some((note, at)) = later {
                error.note(note, at);
            }
        }
    }

    /// A loan stored behind a pointer of unknown origin, which outlives the
    /// function, while it points into the function's own storage.
    fn escaped(&mut self, loan: usize) {
        let Some(path) = self.loans[loan]
            .paths
            .iter()
            .find(|p| !p.steps.contains(&Step::Deref))
        else {
            return;
        };
        if !self.reported.insert(loan) {
            return;
        }
        let what = self.describe(path.root, "a temporary value");
        let message =
            format!("Cannot store a reference into {what}, which ends with this function");
        self.error(message, self.loans[loan].span)
            .label(format!("this refers into {what}"))
            .help("store a copy of the value, or a reference into something that outlives the function");
    }

    /// The local to name as holding `loan`: `holder`, or a named one live
    /// with it other than `root`, the local the access reaches.
    fn best_holder(&self, loan: usize, holder: Local, root: Local, live: &[Fact]) -> Local {
        if holder != root && self.name(holder).is_some() {
            return holder;
        }
        live.iter()
            .find(|fact| fact.1 == loan && fact.0 != root && self.name(fact.0).is_some())
            .map_or(holder, |fact| fact.0)
    }

    /// How a message names where `path` leads: by its local, or in a
    /// closure, by the variable it captured that the path goes through.
    fn describe_path(&self, path: &Path, unnamed: &str) -> String {
        match (path.root, &path.steps[..]) {
            (Local(1), [Step::Deref, Step::Field(at), ..]) if !self.body.captures.is_empty() => {
                format!("'{}'", self.body.captures[*at as usize])
            }
            _ => self.describe(path.root, unnamed),
        }
    }

    fn describe(&self, local: Local, unnamed: &str) -> String {
        match self.name(local) {
            Some("for") => "the loop".to_string(),
            Some(name) => format!("'{name}'"),
            None => unnamed.to_string(),
        }
    }

    /// A note at where `holder` is next used after statement `at`, which
    /// is in the next turn of a loop when it comes before `error` does.
    fn later_use(&self, at: (usize, usize), holder: Local, error: Span) -> Option<(String, Span)> {
        let (b, at) = self.next_use(at, holder)?;
        let span = self.span(b, at);
        let who = match self.name(holder) {
            Some(name) if name != "for" => format!("'{name}' is"),
            _ => "it is".to_string(),
        };
        let note = match span.end <= error.start {
            true => format!("{who} used here in the next turn of the loop"),
            false => format!("{who} used later here"),
        };
        Some((note, span))
    }

    fn next_use(&self, at: (usize, usize), holder: Local) -> Option<(usize, usize)> {
        let mut work = VecDeque::from([(at.0, at.1 + 1)]);
        let mut seen = HashSet::new();
        while let Some((b, start)) = work.pop_front() {
            let count = self.body.blocks[b].statements.len();
            let mut ended = false;
            for at in start..=count {
                let accesses = self.body.accesses(b, at);
                if accesses
                    .iter()
                    .any(|(place, role)| place.local == holder && self.uses(place, *role))
                {
                    return Some((b, at));
                }
                if accesses
                    .iter()
                    .any(|(place, role)| place.local == holder && Self::defines(place, *role))
                {
                    ended = true;
                    break;
                }
            }
            if !ended {
                for next in self.successors(b) {
                    if seen.insert(next) {
                        work.push_back((next, 0));
                    }
                }
            }
        }
        None
    }

    fn error(&mut self, message: impl Into<String>, span: Span) -> &mut ZeruError {
        self.errors.push(ZeruError::semantic(message, span));
        self.errors.last_mut().expect("pushed above")
    }
}

impl Body {
    /// The places statement `at` of block `b` (its terminator, past the
    /// statements) touches, and how, in the order it does.
    fn accesses(&self, b: usize, at: usize) -> Vec<(Place, Role)> {
        let block = &self.blocks[b];
        let mut out = Vec::new();
        let operand = |value: &Operand, out: &mut Vec<(Place, Role)>| match value {
            Operand::Copy(place) => out.push((place.clone(), Role::Read)),
            Operand::Move(place) => out.push((place.clone(), Role::Move)),
            Operand::Constant(_) => {}
        };
        let Some(statement) = block.statements.get(at) else {
            match &block.terminator.kind {
                TerminatorKind::Call { args, dest, .. } => {
                    for arg in args {
                        operand(arg, &mut out);
                    }
                    out.extend(dest.iter().map(|dest| (dest.clone(), Role::Write)));
                }
                TerminatorKind::Drop { place, .. } => out.push((place.clone(), Role::Drop)),
                TerminatorKind::SwitchInt { discr, .. } => operand(discr, &mut out),
                TerminatorKind::Assert { cond, .. } => operand(cond, &mut out),
                TerminatorKind::Return => out.push((Place::local(RETURN), Role::Read)),
                TerminatorKind::Goto(_) | TerminatorKind::Unreachable => {}
            }
            return out;
        };
        for value in statement.kind.operands() {
            operand(value, &mut out);
        }
        match &statement.kind {
            StatementKind::Assign(dest, rvalue) => {
                match rvalue {
                    Rvalue::AddressOf(place)
                    | Rvalue::DeepCopy(place)
                    | Rvalue::Len(place)
                    | Rvalue::Capacity(place)
                    | Rvalue::Discriminant(place)
                    | Rvalue::VecGet(place, _) => out.push((place.clone(), Role::Read)),
                    Rvalue::VecPop(place) | Rvalue::VecRemove(place, _) | Rvalue::Take(place) => {
                        out.push((place.clone(), Role::Realloc))
                    }
                    _ => {}
                }
                out.push((dest.clone(), Role::Write));
            }
            StatementKind::VecPush(place, _)
            | StatementKind::VecInsert(place, ..)
            | StatementKind::VecClear(place)
            | StatementKind::VecReserve(place, _)
            | StatementKind::VecExtend(place, _)
            | StatementKind::VecShrinkToFit(place) => out.push((place.clone(), Role::Realloc)),
            StatementKind::StorageDead(local) => out.push((Place::local(*local), Role::Dead)),
            StatementKind::Asm(asm) => {
                out.extend(asm.outputs.iter().map(|place| (place.clone(), Role::Write)))
            }
            StatementKind::CopyBytes(..)
            | StatementKind::SimdStore(..)
            | StatementKind::Flush
            | StatementKind::Print { .. } => {}
        }
        out
    }
}

fn same(a: Step, b: Step) -> bool {
    matches!((a, b), (Step::Index { .. }, Step::Index { .. })) || a == b
}

/// `path`, or anywhere inside it when `deep`.
fn inside(path: &Path, deep: bool) -> Path {
    match deep {
        true => Path {
            deep: true,
            anchor: None,
            ..path.clone()
        },
        false => path.clone(),
    }
}

/// `path` one step further, at a value of type `ty`, unless it stands for
/// anything inside already, though then still from its anchor short of a
/// reference.
fn extend(mut path: Path, step: Step, ty: Ty) -> Path {
    if path.deep {
        path.anchor = path
            .anchor
            .take()
            .filter(|(_, steps)| step != Step::Deref && steps.len() < EXACT)
            .map(|(loan, mut steps)| {
                steps.push(step);
                (loan, steps)
            });
        return path;
    }
    if path.steps.len() == EXACT {
        path.deep = true;
        return path;
    }
    path.steps.push(step);
    path.ty = ty;
    path
}
