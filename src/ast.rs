use crate::errors::Span;
use crate::token::Token;

pub const PRIMITIVES: [&str; 13] = [
    "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", "usize", "f32", "f64", "bool",
];

#[derive(Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: String,
    pub bound: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: String,
    pub params: Vec<(String, TypeSpec, bool)>,
    pub return_type: Option<TypeSpec>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    Named(String),
    Generic { name: String, args: Vec<TypeSpec> },
    IntLiteral(u64),
    Tuple(Vec<TypeSpec>),
    Pointer(Box<TypeSpec>),
    Optional(Box<TypeSpec>),
    Result(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    Ref(Box<TypeSpec>),
    RefMut(Box<TypeSpec>),
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Var {
        name: String,
        is_const: bool,
        value: Expression,
        type_annotation: Option<TypeSpec>,
    },
    Return(Option<Expression>),
    Break,
    Continue,
    While {
        cond: Expression,
        body: Box<Statement>,
    },
    ForIn {
        index: Option<String>,
        variable: String,
        iterable: Expression,
        body: Box<Statement>,
    },
    Expression(Expression),
    Block(Vec<Statement>),
    Function {
        name: String,
        type_params: Vec<TypeParameter>,
        params: Vec<(String, TypeSpec, bool)>,
        return_type: Option<TypeSpec>,
        body: Vec<Statement>,
    },
    Struct {
        name: String,
        type_params: Vec<TypeParameter>,
        fields: Vec<(String, TypeSpec)>,
        methods: Vec<Statement>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<TypeSpec>)>,
    },
    Trait {
        name: String,
        methods: Vec<TraitMethod>,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Import {
        path: Vec<String>,
        symbols: Option<Vec<String>>,
    },
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Int(u64),
    Float(f64),
    StringLit(Vec<u8>),
    Boolean(bool),
    None,

    Identifier(String),

    Prefix {
        operator: Token,
        right: Box<Expression>,
    },

    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },

    StructLiteral {
        name: String,
        fields: Vec<(String, Expression)>,
    },

    Call {
        function: Box<Expression>,
        type_args: Vec<TypeSpec>,
        arguments: Vec<Expression>,
    },

    Get {
        object: Box<Expression>,
        name: String,
    },
    ArrayLiteral(Vec<Expression>),
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
    },
    ArrayRepeat {
        value: Box<Expression>,
        count: Box<Expression>,
    },
    Assign {
        target: Box<Expression>,
        operator: Token,
        value: Box<Expression>,
    },
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    Slice {
        left: Box<Expression>,
        start: Option<Box<Expression>>,
        end: Option<Box<Expression>>,
    },
    Cast {
        left: Box<Expression>,
        target: TypeSpec,
    },
    Match {
        value: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },
    BorrowRef(Box<Expression>),
    BorrowRefMut(Box<Expression>),
    Dereference(Box<Expression>),
    Try {
        operator: Token,
        value: Box<Expression>,
    },
    Block(Vec<Statement>),
    Tuple(Vec<Expression>),
    InlineAsm {
        template: String,
        outputs: Vec<AsmOperand>,
        inputs: Vec<AsmOperand>,
        clobbers: Vec<String>,
        is_volatile: bool,
    },
}

/// What a `match` arm takes, with a grammar of its own.
#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Default,
    Constant(Expression),
    Binding(String),
    Payload { path: String, fields: Vec<Pattern> },
    Or(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub struct AsmOperand {
    pub constraint: String,
    pub expr: Expression,
}

impl Statement {
    pub fn new(kind: StatementKind, span: Span) -> Self {
        Self {
            kind,
            span,
            is_pub: false,
        }
    }
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Dropped down its left operand in a loop, since `a + b + c ..` and
/// `a.b.c ..` nest one level per operator with no limit on how many.
impl Drop for Expression {
    fn drop(&mut self) {
        use ExpressionKind as E;
        let mut kind = std::mem::replace(&mut self.kind, E::None);
        while let E::Infix { left, .. }
        | E::Index { left, .. }
        | E::Slice { left, .. }
        | E::Cast { left, .. }
        | E::Call { function: left, .. }
        | E::Get { object: left, .. }
        | E::Try { value: left, .. } = &mut kind
        {
            kind = std::mem::replace(&mut left.kind, E::None);
        }
    }
}

/// The text of a `print` format around each `{}`: one piece more than there
/// are values. `{{` and `}}` stand for a brace.
pub fn format_pieces(format: &[u8]) -> Result<Vec<Vec<u8>>, String> {
    let mut pieces = vec![Vec::new()];
    let mut bytes = format.iter().copied().peekable();
    while let Some(byte) = bytes.next() {
        match (byte, bytes.peek()) {
            (b'{', Some(b'}')) => {
                bytes.next();
                pieces.push(Vec::new());
            }
            (b'{', Some(b'{')) | (b'}', Some(b'}')) => {
                bytes.next();
                pieces.last_mut().unwrap().push(byte);
            }
            (b'{' | b'}', _) => {
                return Err("A brace in a format is '{}', '{{' or '}}'".to_string());
            }
            _ => pieces.last_mut().unwrap().push(byte),
        }
    }
    Ok(pieces)
}
