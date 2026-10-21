//! The mid-level IR: each function is a graph
//! of basic blocks over numbered locals. Evaluation order, temporaries,
//! drops and runtime checks are all written out, so code generation only
//! translates. `build` makes it from the typed IR and `elaborate` decides
//! which drops run and which need a flag.

mod borrowck;
mod build;
mod elaborate;

use crate::errors::Span;
use crate::tir::{BinOp, FnId, Program, UnOp};
use crate::ty::{Ty, TyKind};

pub use build::build;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Local(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub u32);

/// A kind of part a type may have: a reference, a `drop` method, one an
/// owning step reaches (a Vec's elements or a variant's payload), a variant's
/// payload.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Part {
    Ref,
    Drop,
    Owning,
    Variant,
}

/// The return place, written before each `Return`.
pub const RETURN: Local = Local(0);

/// One function. Local 0 is the return place, the next `arg_count` are the
/// arguments, the rest are variables and temporaries.
pub struct Body {
    pub symbol: String,
    pub span: Span,
    pub arg_count: usize,
    pub locals: Vec<LocalDecl>,
    pub blocks: Vec<BlockData>,
}

pub struct LocalDecl {
    pub ty: Ty,
    pub name: Option<String>,
}

pub struct BlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

pub enum StatementKind {
    Assign(Place, Rvalue),
    VecPush(Place, Operand),
    VecInsert(Place, Operand, Operand),
    VecClear(Place),
    VecReserve(Place, Operand),
    VecShrinkToFit(Place),
    CopyBytes(Operand, Operand, Operand),
    Flush,
    Asm(Box<Asm>),
    StorageDead(Local),
    Print {
        stderr: bool,
        newline: bool,
        pieces: Vec<Vec<u8>>,
        values: Vec<Operand>,
    },
}

pub struct Terminator {
    pub kind: TerminatorKind,
    pub span: Span,
}

pub enum TerminatorKind {
    Goto(BlockId),
    SwitchInt {
        discr: Operand,
        targets: Vec<(i128, BlockId)>,
        otherwise: BlockId,
    },
    Return,
    Unreachable,
    Call {
        function: FnId,
        args: Vec<Operand>,
        dest: Option<Place>,
        target: BlockId,
    },
    Drop {
        place: Place,
        target: BlockId,
    },
    Assert {
        cond: Operand,
        panic: Panic,
        target: BlockId,
    },
}

/// Why a check panics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Panic {
    Null,
    Bounds,
    UnwrapNone,
    Unwrap,
    UnwrapErr,
}

#[derive(Clone, PartialEq)]
pub struct Place {
    pub local: Local,
    pub projection: Vec<Projection>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Projection {
    Deref,
    Field(u32),
    Index(Local),
    Downcast(u32),
}

#[derive(Clone)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(Constant),
}

#[derive(Clone)]
pub struct Constant {
    pub value: ConstValue,
    pub ty: Ty,
}

#[derive(Clone)]
pub enum ConstValue {
    Int(i128),
    Float(f64),
    Bool(bool),
    Str(Vec<u8>),
}

pub enum Rvalue {
    Use(Operand),
    AddressOf(Place),
    DeepCopy(Place),
    Binary {
        op: BinOp,
        left: Operand,
        right: Operand,
        checked: bool,
    },
    Unary(UnOp, Operand),
    Cast {
        value: Operand,
        to: Ty,
        checked: bool,
    },
    Len(Place),
    Capacity(Place),
    Discriminant(Place),
    Aggregate(Aggregate, Vec<Operand>),
    Repeat(Operand, u64),
    VecRepeat(Operand, u64),
    VecNew(Option<Operand>),
    VecRemove(Place, Operand),
    VecPop(Place),
    VecGet(Place, Operand),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Aggregate {
    Tuple,
    Array,
    Vec,
    Variant(u32),
    Some,
    None,
    Ok,
    Err,
}

pub struct Asm {
    pub template: String,
    pub constraints: String,
    pub outputs: Vec<Place>,
    pub inputs: Vec<Operand>,
    pub volatile: bool,
}

impl Place {
    pub fn local(local: Local) -> Self {
        Self {
            local,
            projection: Vec::new(),
        }
    }

    pub fn project(mut self, projection: Projection) -> Self {
        self.projection.push(projection);
        self
    }

    /// The local itself, with nothing projected from it.
    pub fn as_local(&self) -> Option<Local> {
        self.projection.is_empty().then_some(self.local)
    }
}

impl StatementKind {
    /// The operands it reads, in order.
    pub fn operands(&self) -> Vec<&Operand> {
        match self {
            StatementKind::Assign(_, rvalue) => rvalue.operands(),
            StatementKind::VecPush(_, item) | StatementKind::VecReserve(_, item) => vec![item],
            StatementKind::VecInsert(_, at, item) => vec![at, item],
            StatementKind::CopyBytes(to, from, count) => vec![to, from, count],
            StatementKind::Print { values, .. } => values.iter().collect(),
            StatementKind::Asm(asm) => asm.inputs.iter().collect(),
            StatementKind::VecClear(_)
            | StatementKind::VecShrinkToFit(_)
            | StatementKind::Flush
            | StatementKind::StorageDead(_) => Vec::new(),
        }
    }
}

impl Rvalue {
    pub fn operands(&self) -> Vec<&Operand> {
        match self {
            Rvalue::Use(value)
            | Rvalue::Unary(_, value)
            | Rvalue::Cast { value, .. }
            | Rvalue::Repeat(value, _)
            | Rvalue::VecRepeat(value, _)
            | Rvalue::VecRemove(_, value)
            | Rvalue::VecGet(_, value) => vec![value],
            Rvalue::Binary { left, right, .. } => vec![left, right],
            Rvalue::Aggregate(_, values) => values.iter().collect(),
            Rvalue::VecNew(capacity) => capacity.iter().collect(),
            Rvalue::AddressOf(_)
            | Rvalue::DeepCopy(_)
            | Rvalue::Len(_)
            | Rvalue::Capacity(_)
            | Rvalue::Discriminant(_)
            | Rvalue::VecPop(_) => Vec::new(),
        }
    }
}

impl TerminatorKind {
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            TerminatorKind::Goto(target)
            | TerminatorKind::Call { target, .. }
            | TerminatorKind::Drop { target, .. }
            | TerminatorKind::Assert { target, .. } => vec![*target],
            TerminatorKind::SwitchInt {
                targets, otherwise, ..
            } => targets
                .iter()
                .map(|(_, target)| *target)
                .chain([*otherwise])
                .collect(),
            TerminatorKind::Return | TerminatorKind::Unreachable => Vec::new(),
        }
    }

    pub fn successors_mut(&mut self) -> Vec<&mut BlockId> {
        match self {
            TerminatorKind::Goto(target)
            | TerminatorKind::Call { target, .. }
            | TerminatorKind::Drop { target, .. }
            | TerminatorKind::Assert { target, .. } => vec![target],
            TerminatorKind::SwitchInt {
                targets, otherwise, ..
            } => targets
                .iter_mut()
                .map(|(_, target)| target)
                .chain([otherwise])
                .collect(),
            TerminatorKind::Return | TerminatorKind::Unreachable => Vec::new(),
        }
    }
}

impl Body {
    /// The type of what `place` holds.
    pub fn place_ty(&self, program: &Program, place: &Place) -> Ty {
        program.projected(self.locals[place.local.0 as usize].ty, &place.projection)
    }

    pub fn operand_ty(&self, program: &Program, operand: &Operand) -> Ty {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => self.place_ty(program, place),
            Operand::Constant(constant) => constant.ty,
        }
    }
}

impl Program {
    /// The type `projection` reaches from a `ty`, `variant` keeping the
    /// variant a `Downcast` chose for the field after it.
    /// The type `projection` reaches from a value of `ty`.
    pub fn projected(&self, mut ty: Ty, projection: &[Projection]) -> Ty {
        let mut variant = None;
        for &step in projection {
            ty = self.project_ty(ty, step, &mut variant);
        }
        ty
    }

    pub fn project_ty(&self, ty: Ty, projection: Projection, variant: &mut Option<u32>) -> Ty {
        match (projection, self.types.kind(ty)) {
            (
                Projection::Deref,
                TyKind::Ptr(inner) | TyKind::Ref(inner) | TyKind::RefMut(inner),
            ) => *inner,
            (
                Projection::Index(_),
                TyKind::Array(elem, _) | TyKind::Vec(elem) | TyKind::Slice(elem),
            ) => *elem,
            (Projection::Downcast(at), _) => {
                *variant = Some(at);
                ty
            }
            (Projection::Field(at), _) if variant.is_some() => {
                let chosen = variant.take().unwrap();
                self.enums[&ty].variants[chosen as usize][at as usize]
            }
            (Projection::Field(at), TyKind::Tuple(parts)) => parts[at as usize],
            (Projection::Field(1), TyKind::Optional(inner) | TyKind::Result(inner, _)) => *inner,
            (Projection::Field(2), TyKind::Result(_, error)) => *error,
            (Projection::Field(at), _) => self.structs[&ty].fields[at as usize],
            _ => unreachable!("{} has no such part", self.types.show(ty)),
        }
    }

    /// Whether a value of `ty`, not looking through pointers, has a part
    /// of kind `part`.
    fn reaches(&self, ty: Ty, part: Part, seen: &mut Vec<Ty>) -> bool {
        let kind = self.types.kind(ty);
        let payloads = || {
            self.enums
                .get(&ty)
                .is_some_and(|e| e.variants.iter().any(|v| !v.is_empty()))
        };
        let found = match (part, kind) {
            (Part::Ref, TyKind::Ref(_) | TyKind::RefMut(_) | TyKind::Slice(_)) => true,
            (Part::Owning, TyKind::Vec(_)) => true,
            (Part::Owning | Part::Variant, TyKind::Optional(_) | TyKind::Result(..)) => true,
            (Part::Owning | Part::Variant, _) => payloads(),
            (Part::Drop, _) => self.structs.get(&ty).is_some_and(|s| s.drop.is_some()),
            _ => false,
        };
        if found || seen.contains(&ty) {
            return found;
        }
        seen.push(ty);
        self.parts(ty)
            .into_iter()
            .any(|inner| self.reaches(inner, part, seen))
    }

    /// The types a value of `ty` holds directly, not behind a pointer.
    fn parts(&self, ty: Ty) -> Vec<Ty> {
        match self.types.kind(ty) {
            TyKind::Vec(inner) | TyKind::Array(inner, _) | TyKind::Optional(inner) => vec![*inner],
            TyKind::Result(value, error) => vec![*value, *error],
            TyKind::Tuple(parts) => parts.clone(),
            TyKind::Adt(..) => match self.structs.get(&ty) {
                Some(layout) => layout.fields.clone(),
                None => self
                    .enums
                    .get(&ty)
                    .map_or(Vec::new(), |e| e.variants.concat()),
            },
            _ => Vec::new(),
        }
    }

    /// Whether a value of `view` may point into memory a value of `owner`
    /// holds: always through a `&` or `&var`, and through a slice only when
    /// `owner` itself stores elements of the slice's type.
    pub fn may_point_into(&self, view: Ty, owner: Ty) -> bool {
        let mut stack = vec![view];
        let mut seen = Vec::new();
        while let Some(ty) = stack.pop() {
            if seen.contains(&ty) {
                continue;
            }
            seen.push(ty);
            match self.types.kind(ty) {
                TyKind::Ref(_) | TyKind::RefMut(_) => return true,
                TyKind::Slice(elem) if self.stores(owner, *elem, &mut Vec::new()) => return true,
                TyKind::Slice(_) => {}
                _ => stack.extend(self.parts(ty)),
            }
        }
        false
    }

    /// Whether a value of `ty` is or stores elements of type `elem` itself.
    fn stores(&self, ty: Ty, elem: Ty, seen: &mut Vec<Ty>) -> bool {
        if ty == elem {
            return true;
        }
        if seen.contains(&ty) {
            return false;
        }
        seen.push(ty);
        match self.types.kind(ty) {
            TyKind::Vec(inner) | TyKind::Array(inner, _) if *inner == elem => true,
            TyKind::Ref(inner) | TyKind::RefMut(inner) => self.stores(*inner, elem, seen),
            _ => self
                .parts(ty)
                .into_iter()
                .any(|part| self.stores(part, elem, seen)),
        }
    }
}
