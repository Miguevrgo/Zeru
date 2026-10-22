//! The typed IR the checker produces and the MIR is built from. Every
//! expression has its exact type, every implicit conversion is a node of its
//! own, every call names the function instance it reaches, and every move and
//! drop the language implies is written out.

use std::collections::{HashMap, HashSet};

use crate::errors::Span;
use crate::ty::{Ty, Types};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FnId(pub u32);

pub struct Program {
    pub types: Types,
    pub functions: Vec<Function>,
    pub structs: HashMap<Ty, StructLayout>,
    pub enums: HashMap<Ty, EnumLayout>,
    pub owning: HashSet<Ty>,
    pub runtime: Runtime,
}

/// The prelude functions the generated code calls by itself.
pub struct Runtime {
    pub out_stream: Ty,
    pub write_bytes: FnId,
    pub write_int: FnId,
    pub write_uint: FnId,
    pub write_float: FnId,
    pub flush: FnId,
    pub heap: Ty,
    pub alloc: FnId,
    pub realloc: FnId,
}

pub struct StructLayout {
    pub fields: Vec<Ty>,
    pub drop: Option<FnId>,
}

pub struct EnumLayout {
    pub variants: Vec<Vec<Ty>>,
}

impl Program {
    /// Whether a value of `ty` owns memory or runs a `drop`: what is freed
    /// as it goes and duplicated by a deep copy.
    pub fn needs_drop(&self, ty: Ty) -> bool {
        self.owning.contains(&ty)
    }
}

pub struct Function {
    pub symbol: String,
    pub params: Vec<LocalId>,
    pub ret: Ty,
    pub locals: Vec<Local>,
    pub body: Block,
    pub span: Span,
}

pub struct Local {
    pub name: String,
    pub ty: Ty,
    pub view: bool,
}

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone)]
pub enum StmtKind {
    Let(LocalId, Expr),
    Expr(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
    If {
        cond: Expr,
        then: Block,
        otherwise: Option<Block>,
    },
    While {
        cond: Expr,
        body: Block,
    },
    ForRange {
        var: LocalId,
        start: Expr,
        end: Expr,
        body: Block,
    },
    ForEach {
        index: Option<LocalId>,
        var: LocalId,
        seq: Place,
        body: Block,
        within: Span,
    },
    Block(Block),
}

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone)]
pub enum ExprKind {
    Int(i128),
    Float(f64),
    Bool(bool),
    Str(Vec<u8>),
    Void,

    Read(Place),
    Move(Place),
    Copy(Place),
    AddressOf(Place),

    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>),

    Call(FnId, Vec<Expr>),
    Builtin(Builtin, Vec<Expr>),

    Tuple(Vec<Expr>),
    Struct(Vec<(u32, Expr)>),
    Array(Vec<Expr>),
    ArrayRepeat(Box<Expr>, u64),
    VecLiteral(Vec<Expr>),
    VecRepeat(Box<Expr>, u64),
    Variant(u32, Vec<Expr>),
    Some(Box<Expr>),
    None,
    Ok(Box<Expr>),
    Err(Box<Expr>),

    Assign {
        place: Place,
        value: Box<Expr>,
        drop_old: bool,
    },
    CompoundAssign {
        place: Place,
        op: BinOp,
        value: Box<Expr>,
    },

    Match {
        scrutinee: Place,
        arms: Vec<Arm>,
    },
    Try(Box<Expr>),
    Block(Block),

    Print {
        stderr: bool,
        newline: bool,
        pieces: Vec<Vec<u8>>,
        values: Vec<Expr>,
    },
    Assert {
        cond: Box<Expr>,
        pieces: Vec<Vec<u8>>,
        values: Vec<Expr>,
    },
    Asm(Box<Asm>),
}

impl Expr {
    /// Call `f` on each expression directly inside this one, those in its
    /// places included and those in the statements of a block not.
    pub fn for_each_child(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match &mut self.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Bool(_)
            | ExprKind::Str(_)
            | ExprKind::Void
            | ExprKind::None
            | ExprKind::Block(_) => {}
            ExprKind::Read(place)
            | ExprKind::Move(place)
            | ExprKind::Copy(place)
            | ExprKind::AddressOf(place) => place.for_each_expr(f),
            ExprKind::Unary(_, value)
            | ExprKind::Cast(value)
            | ExprKind::ArrayRepeat(value, _)
            | ExprKind::VecRepeat(value, _)
            | ExprKind::Some(value)
            | ExprKind::Ok(value)
            | ExprKind::Err(value)
            | ExprKind::Try(value) => f(value),
            ExprKind::Binary(_, left, right)
            | ExprKind::And(left, right)
            | ExprKind::Or(left, right) => {
                f(left);
                f(right);
            }
            ExprKind::Call(_, values)
            | ExprKind::Builtin(_, values)
            | ExprKind::Tuple(values)
            | ExprKind::Array(values)
            | ExprKind::VecLiteral(values)
            | ExprKind::Variant(_, values)
            | ExprKind::Print { values, .. } => values.iter_mut().for_each(f),
            ExprKind::Assert { cond, values, .. } => {
                f(cond);
                values.iter_mut().for_each(f);
            }
            ExprKind::Struct(fields) => fields.iter_mut().for_each(|(_, value)| f(value)),
            ExprKind::Assign { place, value, .. }
            | ExprKind::CompoundAssign { place, value, .. } => {
                place.for_each_expr(f);
                f(value);
            }
            ExprKind::Match { scrutinee, arms } => {
                scrutinee.for_each_expr(f);
                arms.iter_mut().for_each(|arm| f(&mut arm.body));
            }
            ExprKind::Asm(asm) => {
                asm.outputs
                    .iter_mut()
                    .for_each(|place| place.for_each_expr(f));
                asm.inputs.iter_mut().for_each(f);
            }
        }
    }
}

impl Place {
    /// The local it starts from, unless it starts from a pointer or a
    /// temporary.
    pub fn root(&self) -> Option<LocalId> {
        match self {
            Place::Local(local) => Some(*local),
            Place::Field(base, _) | Place::Index(base, _) => base.root(),
            Place::Deref(_) | Place::Temp(_) => None,
        }
    }

    fn for_each_expr(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match self {
            Place::Local(_) => {}
            Place::Field(base, _) => base.for_each_expr(f),
            Place::Index(base, index) => {
                base.for_each_expr(f);
                f(index);
            }
            Place::Deref(value) | Place::Temp(value) => f(value),
        }
    }
}

/// Inline assembly, kept apart since it is large and rare.
#[derive(Clone)]
pub struct Asm {
    pub template: String,
    pub constraints: String,
    pub outputs: Vec<Place>,
    pub inputs: Vec<Expr>,
    pub volatile: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    AddWrap,
    SubWrap,
    MulWrap,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    PtrAdd,
    PtrSub,
}

/// The prelude function that writes out what `print` buffered, which the
/// program calls by name and code generation defines.
pub const FLUSH_FN: &str = "__zeru_flush";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Len,
    VecCapacity,
    VecIsEmpty,
    VecPush,
    VecInsert,
    VecRemove,
    VecPop,
    VecGet,
    VecClear,
    VecReserve,
    VecShrinkToFit,
    VecNew,
    VecWithCapacity,
    IsSome,
    IsNone,
    IsOk,
    IsErr,
    Unwrap,
    UnwrapErr,
    Flush,
    CopyBytes,
    Slice,
}

/// One arm of a `match`; `span` is its pattern, where its bindings borrow
/// the scrutinee.
#[derive(Clone)]
pub struct Arm {
    pub tags: Vec<i128>,
    pub bindings: Vec<(LocalId, Vec<Payload>)>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Payload {
    SomeValue,
    OkValue,
    ErrValue,
    Variant { variant: u32, field: u32 },
}

#[derive(Clone)]
pub enum Place {
    Local(LocalId),
    Field(Box<Place>, u32),
    Index(Box<Place>, Box<Expr>),
    Deref(Box<Expr>),
    Temp(Box<Expr>),
}
