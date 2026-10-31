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
    Generic {
        name: String,
        args: Vec<TypeSpec>,
    },
    IntLiteral(u64),
    Tuple(Vec<TypeSpec>),
    Pointer(Box<TypeSpec>),
    Optional(Box<TypeSpec>),
    Result(Box<TypeSpec>),
    Slice(Box<TypeSpec>),
    SliceMut(Box<TypeSpec>),
    Ref(Box<TypeSpec>),
    RefMut(Box<TypeSpec>),
    /// `fn(i32) i32`: what a closure parameter takes, with no return type
    /// for one that returns nothing.
    Fn(Vec<TypeSpec>, Option<Box<TypeSpec>>),
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
    /// What `#test { }` holds, part of the program only for `zeru test` and
    /// `zeru check`.
    pub tests: Vec<Statement>,
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
    VarTuple {
        names: Vec<Names>,
        is_const: bool,
        value: Expression,
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
        variable: Names,
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
    VecLiteral(Vec<Expression>),
    VecRepeat {
        value: Box<Expression>,
        count: Box<Expression>,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,
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
        arms: Vec<MatchArm>,
    },
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
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
    Closure(Box<Closure>),
    InlineAsm {
        template: String,
        outputs: Vec<AsmOperand>,
        inputs: Vec<AsmOperand>,
        clobbers: Vec<String>,
        is_volatile: bool,
    },
}

/// A function without a name: `fn(x: i32) i32 { body }`, or `fn(x) x * 2`
/// with a body of one expression, which only a call's whole `argument`
/// may be. Each parameter may leave out its type, and may be `var`.
#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<(String, Option<TypeSpec>, bool)>,
    pub return_type: Option<TypeSpec>,
    pub body: ClosureBody,
    pub argument: bool,
}

#[derive(Debug, Clone)]
pub enum ClosureBody {
    Block(Vec<Statement>),
    Expression(Box<Expression>),
}

/// What a `var`, `const` or `for` names: one name, `_` for a part left
/// out, or a tuple taken apart, as in `(a, b)` or `((x, y), _)`. A name in
/// a `var` tuple may carry its type.
#[derive(Debug, Clone)]
pub enum Names {
    One(String, Option<TypeSpec>),
    Skip,
    Tuple(Vec<Names>),
}

/// One arm of a `match`: its pattern, the condition it may add after `if`,
/// and its body.
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
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
    Range(Expression, Expression),
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

/// How a `{:spec}` writes its value: padded with `fill` to `width`
/// characters on the side `align` names, or with zeros after its sign when
/// `zero`; a float with `precision` decimals; an integer in `radix`, its
/// letters capitals when `upper`.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Spec {
    pub fill: Option<u8>,
    pub align: Option<u8>,
    pub zero: bool,
    pub width: usize,
    pub precision: Option<usize>,
    pub radix: Option<(u64, bool)>,
}

/// The most decimals a `{:.n}` writes.
pub const MAX_PRECISION: usize = 100;

const SPEC_FORM: &str =
    "A format spec is written as in {:>8}, {:<5}, {:*^7}, {:08.3}, {:.2}, {:x}, {:X}, {:o} or {:b}";

/// A format taken apart: the text around each `{}`, and the spec of each.
pub type Pieces = (Vec<Vec<u8>>, Vec<Option<Spec>>);

/// The text of a `print` format around each `{}`: one piece more than there
/// are values, and the spec each `{}` gives, if any. `{{` and `}}` stand
/// for a brace.
pub fn format_pieces(format: &[u8]) -> Result<Pieces, String> {
    let mut pieces = vec![Vec::new()];
    let mut specs = Vec::new();
    let mut bytes = format.iter().copied().peekable();
    while let Some(byte) = bytes.next() {
        match (byte, bytes.peek()) {
            (b'{', Some(b'}')) => {
                bytes.next();
                pieces.push(Vec::new());
                specs.push(None);
            }
            (b'{', Some(b':')) => {
                bytes.next();
                let mut text = Vec::new();
                loop {
                    match bytes.next() {
                        Some(b'}') => break,
                        Some(byte) => text.push(byte),
                        None => return Err(SPEC_FORM.to_string()),
                    }
                }
                specs.push(Some(parse_spec(&text)?));
                pieces.push(Vec::new());
            }
            (b'{', Some(b'{')) | (b'}', Some(b'}')) => {
                bytes.next();
                pieces.last_mut().unwrap().push(byte);
            }
            (b'{' | b'}', _) => {
                return Err("A brace in a format is '{}', '{:spec}', '{{' or '}}'".to_string());
            }
            _ => pieces.last_mut().unwrap().push(byte),
        }
    }
    Ok((pieces, specs))
}

/// What follows the ':' of a `{:spec}`: `[[fill]align][0][width][.precision][type]`.
fn parse_spec(text: &[u8]) -> Result<Spec, String> {
    let mut spec = Spec::default();
    let mut rest = text;
    let aligns = *b"<>^";
    match rest {
        [fill, align, ..] if aligns.contains(align) && fill.is_ascii() => {
            (spec.fill, spec.align) = (Some(*fill), Some(*align));
            rest = &rest[2..];
        }
        [align, ..] if aligns.contains(align) => {
            spec.align = Some(*align);
            rest = &rest[1..];
        }
        _ => {}
    }
    if let [b'0', more @ ..] = rest {
        spec.zero = true;
        rest = more;
    }
    let number = |rest: &mut &[u8]| -> Result<Option<usize>, String> {
        let digits = rest.iter().take_while(|b| b.is_ascii_digit()).count();
        let (written, after) = rest.split_at(digits);
        *rest = after;
        match written {
            [] => Ok(None),
            _ => std::str::from_utf8(written)
                .unwrap()
                .parse()
                .map(Some)
                .map_err(|_| SPEC_FORM.to_string()),
        }
    };
    spec.width = number(&mut rest)?.unwrap_or(0);
    if let [b'.', more @ ..] = rest {
        rest = more;
        spec.precision = Some(number(&mut rest)?.ok_or(SPEC_FORM)?);
    }
    if spec.precision.is_some_and(|p| p > MAX_PRECISION) {
        return Err(format!("A format writes at most {MAX_PRECISION} decimals"));
    }
    spec.radix = match rest {
        [] => None,
        [b'x'] => Some((16, false)),
        [b'X'] => Some((16, true)),
        [b'o'] => Some((8, false)),
        [b'b'] => Some((2, false)),
        _ => return Err(SPEC_FORM.to_string()),
    };
    Ok(spec)
}
