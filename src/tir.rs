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
    /// The `test("...")` runners, in the order `zeru test` numbers them.
    pub tests: Vec<FnId>,
    pub warnings: Vec<crate::errors::ZeruError>,
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
    pub exit: FnId,
    pub str_cmp: FnId,
    pub string_cmp: FnId,
    pub test_index: FnId,
}

pub struct StructLayout {
    pub fields: Vec<Ty>,
    pub drop: Option<FnId>,
}

pub struct EnumLayout {
    pub variants: Vec<Vec<Ty>>,
}

impl Program {
    /// Whether `ty` is an enum whose variants carry no data.
    pub fn is_plain_enum(&self, ty: Ty) -> bool {
        self.enums
            .get(&ty)
            .is_some_and(|e| e.variants.iter().all(Vec::is_empty))
    }

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
        inclusive: bool,
        body: Block,
    },
    ForEach {
        index: Option<LocalId>,
        walks: Vec<Walk>,
        body: Block,
        within: Span,
    },
    Block(Block),
}

/// One sequence a `for` walks: `var` is each element in turn, and
/// `unpack` names its parts when the loop takes a tuple apart.
#[derive(Clone)]
pub struct Walk {
    pub seq: Place,
    pub var: LocalId,
    pub unpack: Option<Unpack>,
}

/// Where each part of a tuple goes: a new local, an existing place, nowhere,
/// or the parts of a part that is a tuple itself.
#[derive(Clone)]
pub enum Unpack {
    Declare(LocalId),
    Store(Place),
    Skip,
    Tuple(Vec<Unpack>),
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
    VecRepeat(Box<Expr>, Box<Expr>),
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
        message: Option<Box<Expr>>,
    },
    Unpack {
        target: Unpack,
        value: Box<Expr>,
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
            | ExprKind::Some(value)
            | ExprKind::Ok(value)
            | ExprKind::Err(value)
            | ExprKind::Try(value) => f(value),
            ExprKind::Binary(_, left, right)
            | ExprKind::VecRepeat(left, right)
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
            ExprKind::Assert { cond, message } => {
                f(cond);
                message.iter_mut().for_each(|message| f(message));
            }
            ExprKind::Unpack { target, value } => {
                target.for_each_expr(f);
                f(value);
            }
            ExprKind::Struct(fields) => fields.iter_mut().for_each(|(_, value)| f(value)),
            ExprKind::Assign { place, value, .. }
            | ExprKind::CompoundAssign { place, value, .. } => {
                place.for_each_expr(f);
                f(value);
            }
            ExprKind::Match { scrutinee, arms } => {
                scrutinee.for_each_expr(f);
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        f(guard);
                    }
                    f(&mut arm.body);
                }
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

/// The bits a hash map hashes for a key; the compiler computes them.
pub const HASH_BITS_FN: &str = "__zeru_hash_bits";

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
    HashBits,
    Replace,
    Bits(BitOp),
}

/// What an integer's bit methods do: count its set bits, or the zeros
/// above or below its highest or lowest set one, or rotate its bits or
/// reverse its bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitOp {
    CountOnes,
    LeadingZeros,
    TrailingZeros,
    RotateLeft,
    RotateRight,
    SwapBytes,
}

/// One arm of a `match`: the tags it takes, each alternative a range from
/// the first to the second, none for every other; its bindings; the guard
/// that must hold too. `span` is its pattern, where its bindings borrow the
/// scrutinee.
#[derive(Clone)]
pub struct Arm {
    pub tags: Vec<(i128, i128)>,
    pub bindings: Vec<(LocalId, Vec<Payload>)>,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Payload {
    Whole,
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

impl Unpack {
    /// Call `f` on each expression inside the places the parts go to.
    fn for_each_expr(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match self {
            Unpack::Store(place) => place.for_each_expr(f),
            Unpack::Tuple(parts) => parts.iter_mut().for_each(|part| part.for_each_expr(f)),
            Unpack::Declare(_) | Unpack::Skip => {}
        }
    }
}
