//! Drop elaboration, as rustc's: a forward dataflow over the blocks finds,
//! for each local that owns something, whether it may hold a value and
//! whether it may not at each of its drops. A drop where it surely holds
//! one runs, one where it surely does not goes away, and one in between
//! tests a flag, a bool local raised as the value arrives and lowered as it
//! is moved or dropped. Unreachable blocks go first, straight-line chains
//! of blocks are joined last.

use crate::errors::Span;
use crate::tir::Program;
use crate::ty::Types;

use super::{
    BlockData, BlockId, Body, ConstValue, Constant, Local, LocalDecl, Operand, Place, RETURN,
    Statement, StatementKind, Terminator, TerminatorKind,
};

/// What each tracked local may be at a point: holding a value, and not.
#[derive(Clone, PartialEq)]
struct State {
    init: Vec<bool>,
    uninit: Vec<bool>,
}

pub(super) fn elaborate(body: &mut Body, program: &Program) {
    remove_unreachable(body);
    let tracked: Vec<Option<usize>> = {
        let mut count = 0;
        body.locals
            .iter()
            .enumerate()
            .map(|(at, decl)| {
                (at != RETURN.0 as usize && program.needs_drop(decl.ty)).then(|| {
                    count += 1;
                    count - 1
                })
            })
            .collect()
    };
    let count = tracked.iter().flatten().count();
    if count > 0 {
        let entry = entry_states(body, &tracked, count);
        let drops = classify(body, &tracked, &entry);
        add_flags(body, drops);
    }
    join_chains(body);
}

fn remove_unreachable(body: &mut Body) {
    let mut reached = vec![false; body.blocks.len()];
    let mut work = vec![BlockId(0)];
    while let Some(block) = work.pop() {
        if !std::mem::replace(&mut reached[block.0 as usize], true) {
            work.extend(body.blocks[block.0 as usize].terminator.kind.successors());
        }
    }
    let mut renumbered = vec![BlockId(u32::MAX); body.blocks.len()];
    let mut next = 0;
    for (at, &kept) in reached.iter().enumerate() {
        if kept {
            renumbered[at] = BlockId(next);
            next += 1;
        }
    }
    let blocks = std::mem::take(&mut body.blocks);
    body.blocks = blocks
        .into_iter()
        .zip(reached)
        .filter_map(|(block, kept)| kept.then_some(block))
        .collect();
    for block in &mut body.blocks {
        for target in block.terminator.kind.successors_mut() {
            *target = renumbered[target.0 as usize];
        }
    }
}

/// The local whose value a move out of `operand` takes away.
fn moved(operand: &Operand) -> Option<Local> {
    match operand {
        Operand::Move(place) => Some(place.local),
        _ => None,
    }
}

/// The locals `kind` takes values out of, and the local it gives a whole
/// value to.
fn effects(kind: &StatementKind) -> (Vec<Local>, Option<Local>) {
    let moves = kind.operands().into_iter().filter_map(moved).collect();
    let init = match kind {
        StatementKind::Assign(place, _) => place.as_local(),
        _ => None,
    };
    (moves, init)
}

/// The same for a terminator: a call's arguments and destination, and the
/// local a drop leaves empty.
fn terminator_effects(kind: &TerminatorKind) -> (Vec<Local>, Option<Local>) {
    match kind {
        TerminatorKind::Call { args, dest, .. } => (
            args.iter().filter_map(moved).collect(),
            dest.as_ref().and_then(Place::as_local),
        ),
        TerminatorKind::Drop { place, .. } => (place.as_local().into_iter().collect(), None),
        _ => (Vec::new(), None),
    }
}

impl State {
    fn set(&mut self, tracked: &[Option<usize>], local: Local, init: bool) {
        if let Some(at) = tracked[local.0 as usize] {
            self.init[at] = init;
            self.uninit[at] = !init;
        }
    }

    fn apply(&mut self, tracked: &[Option<usize>], (moves, init): (Vec<Local>, Option<Local>)) {
        for local in moves {
            self.set(tracked, local, false);
        }
        if let Some(local) = init {
            self.set(tracked, local, true);
        }
    }

    fn join(&mut self, other: &State) -> bool {
        let before = self.clone();
        for (mine, theirs) in [
            (&mut self.init, &other.init),
            (&mut self.uninit, &other.uninit),
        ] {
            for (a, b) in mine.iter_mut().zip(theirs) {
                *a |= b;
            }
        }
        *self != before
    }
}

/// The state at the start of each block.
fn entry_states(body: &Body, tracked: &[Option<usize>], count: usize) -> Vec<State> {
    let empty = State {
        init: vec![false; count],
        uninit: vec![false; count],
    };
    let mut states = vec![empty; body.blocks.len()];
    let mut start = State {
        init: vec![false; count],
        uninit: vec![true; count],
    };
    for arg in 1..=body.arg_count {
        start.set(tracked, Local(arg as u32), true);
    }
    states[0] = start;
    let mut pending = vec![true; body.blocks.len()];
    let mut work: Vec<usize> = (0..body.blocks.len()).rev().collect();
    while let Some(at) = work.pop() {
        pending[at] = false;
        let block = &body.blocks[at];
        let mut state = states[at].clone();
        for statement in &block.statements {
            state.apply(tracked, effects(&statement.kind));
        }
        state.apply(tracked, terminator_effects(&block.terminator.kind));
        for next in block.terminator.kind.successors() {
            let next = next.0 as usize;
            if states[next].join(&state) && !pending[next] {
                pending[next] = true;
                work.push(next);
            }
        }
    }
    states
}

/// What a drop of a local does.
#[derive(Clone, Copy, PartialEq)]
enum DropKind {
    Never,
    Always,
    Flagged,
}

/// How each block's closing drop of a whole tracked local goes.
fn classify(body: &Body, tracked: &[Option<usize>], entry: &[State]) -> Vec<Option<DropKind>> {
    body.blocks
        .iter()
        .zip(entry)
        .map(|(block, state)| {
            let TerminatorKind::Drop { place, .. } = &block.terminator.kind else {
                return None;
            };
            let at = tracked[place.as_local()?.0 as usize]?;
            let mut state = state.clone();
            for statement in &block.statements {
                state.apply(tracked, effects(&statement.kind));
            }
            Some(match (state.init[at], state.uninit[at]) {
                (false, _) => DropKind::Never,
                (true, false) => DropKind::Always,
                (true, true) => DropKind::Flagged,
            })
        })
        .collect()
}

fn set_flag(flag: Local, value: bool, span: Span) -> Statement {
    Statement {
        kind: StatementKind::Assign(
            Place::local(flag),
            super::Rvalue::Use(Operand::Constant(Constant {
                value: ConstValue::Bool(value),
                ty: Types::BOOL,
            })),
        ),
        span,
    }
}

/// Rewrite the drops `classify` decided, with a flag for each local that has
/// a drop in between.
fn add_flags(body: &mut Body, drops: Vec<Option<DropKind>>) {
    let mut flags: Vec<Option<Local>> = vec![None; body.locals.len()];
    for (block, kind) in body.blocks.iter().zip(&drops) {
        if *kind == Some(DropKind::Flagged) {
            let TerminatorKind::Drop { place, .. } = &block.terminator.kind else {
                unreachable!()
            };
            let local = place.local.0 as usize;
            if flags[local].is_none() {
                body.locals.push(LocalDecl {
                    ty: Types::BOOL,
                    name: None,
                });
                flags[local] = Some(Local(body.locals.len() as u32 - 1));
            }
        }
    }
    let flag_of = |local: Local| flags.get(local.0 as usize).copied().flatten();
    let span = body.span;
    let mut entry: Vec<Statement> = Vec::new();
    for (local, flag) in flags.iter().enumerate() {
        if let Some(flag) = *flag {
            let arg = (1..=body.arg_count).contains(&local);
            entry.push(set_flag(flag, arg, span));
        }
    }
    let count = body.blocks.len();
    for at in 0..count {
        let block = &mut body.blocks[at];
        let statements = std::mem::take(&mut block.statements);
        for statement in statements {
            let (moves, init) = effects(&statement.kind);
            let span = statement.span;
            block.statements.push(statement);
            for local in moves.into_iter().filter(|&l| Some(l) != init) {
                if let Some(flag) = flag_of(local) {
                    block.statements.push(set_flag(flag, false, span));
                }
            }
            if let Some(flag) = init.and_then(flag_of) {
                block.statements.push(set_flag(flag, true, span));
            }
        }
        let span = block.terminator.span;
        if let TerminatorKind::Call {
            args, dest, target, ..
        } = &block.terminator.kind
        {
            let cleared: Vec<Statement> = args
                .iter()
                .filter_map(moved)
                .filter_map(flag_of)
                .map(|flag| set_flag(flag, false, span))
                .collect();
            block.statements.extend(cleared);
            if let Some(flag) = dest.as_ref().and_then(Place::as_local).and_then(flag_of) {
                let target = target.0 as usize;
                body.blocks[target]
                    .statements
                    .insert(0, set_flag(flag, true, span));
            }
        }
    }
    body.blocks[0].statements.splice(0..0, entry);
    for (at, kind) in drops.into_iter().enumerate() {
        let Some(kind) = kind else { continue };
        let span = body.blocks[at].terminator.span;
        let taken = std::mem::replace(
            &mut body.blocks[at].terminator.kind,
            TerminatorKind::Unreachable,
        );
        let TerminatorKind::Drop { place, target } = taken else {
            unreachable!("classified as a drop")
        };
        let flag = flag_of(place.local);
        let after = match flag {
            Some(flag) => push_block(
                body,
                vec![set_flag(flag, false, span)],
                TerminatorKind::Goto(target),
                span,
            ),
            None => target,
        };
        let dropped = TerminatorKind::Drop {
            place,
            target: after,
        };
        body.blocks[at].terminator.kind = match kind {
            DropKind::Never => TerminatorKind::Goto(target),
            DropKind::Always => dropped,
            DropKind::Flagged => {
                let drop = push_block(body, Vec::new(), dropped, span);
                TerminatorKind::SwitchInt {
                    discr: Operand::Copy(Place::local(flag.unwrap())),
                    targets: vec![(0, target)],
                    otherwise: drop,
                }
            }
        };
    }
}

fn push_block(
    body: &mut Body,
    statements: Vec<Statement>,
    kind: TerminatorKind,
    span: Span,
) -> BlockId {
    body.blocks.push(BlockData {
        statements,
        terminator: Terminator { kind, span },
    });
    BlockId(body.blocks.len() as u32 - 1)
}

/// Join each block that only goes on to a block nothing else reaches with
/// that block.
fn join_chains(body: &mut Body) {
    let mut preds = vec![0usize; body.blocks.len()];
    preds[0] = 1;
    for block in &body.blocks {
        for target in block.terminator.kind.successors() {
            preds[target.0 as usize] += 1;
        }
    }
    for at in 0..body.blocks.len() {
        while let TerminatorKind::Goto(next) = body.blocks[at].terminator.kind {
            let next = next.0 as usize;
            if next == at || preds[next] != 1 {
                break;
            }
            preds[next] = 0;
            let taken = std::mem::replace(
                &mut body.blocks[next],
                BlockData {
                    statements: Vec::new(),
                    terminator: Terminator {
                        kind: TerminatorKind::Unreachable,
                        span: body.span,
                    },
                },
            );
            body.blocks[at].statements.extend(taken.statements);
            body.blocks[at].terminator = taken.terminator;
        }
    }
    remove_unreachable(body);
}
