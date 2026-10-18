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
        count: u64,
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
    /// `value?` or `value!`: the payload, or a return of the missing value.
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

/// What a `match` arm takes, with a grammar of its own, as rustc's `Pat`.
#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    /// The arm taking every value no other arm takes.
    Default,
    /// A number, possibly negative, `true`, `false` or `None`.
    Literal(Expression),
    /// A constant or a variant, by its name.
    Path(String),
    /// A name inside a payload, bound to the value there; `_` binds nothing.
    Binding(String),
    /// `Some(p)`, `Ok(p)`, `Err(p)` or `E::V(p, q)`.
    Payload { path: String, fields: Vec<Pattern> },
    /// `p | q`: any of the alternatives.
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

impl Statement {
    /// Call `seen` with every name the statement uses, nested code included.
    pub fn names(&self, seen: &mut impl FnMut(&str)) {
        let block = |statements: &[Statement], seen: &mut _| {
            statements.iter().for_each(|s| s.names(seen));
        };
        match &self.kind {
            StatementKind::Var { value, .. } => value.names(seen),
            StatementKind::Return(value) => value.iter().for_each(|v| v.names(seen)),
            StatementKind::While { cond, body } => {
                cond.names(seen);
                body.names(seen);
            }
            StatementKind::ForIn { iterable, body, .. } => {
                iterable.names(seen);
                body.names(seen);
            }
            StatementKind::Expression(expr) => expr.names(seen),
            StatementKind::Block(statements) => block(statements, seen),
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.names(seen);
                then_branch.names(seen);
                else_branch.iter().for_each(|b| b.names(seen));
            }
            _ => {}
        }
    }
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Call `seen` with every name the expression uses.
    pub fn names(&self, seen: &mut impl FnMut(&str)) {
        use ExpressionKind as E;
        match &self.kind {
            E::Identifier(name) => seen(name),
            E::Prefix { right: inner, .. }
            | E::Get { object: inner, .. }
            | E::ArrayRepeat { value: inner, .. }
            | E::Cast { left: inner, .. }
            | E::BorrowRef(inner)
            | E::BorrowRefMut(inner)
            | E::Dereference(inner)
            | E::Try { value: inner, .. } => inner.names(seen),
            E::Infix { left, right, .. }
            | E::Assign {
                target: left,
                value: right,
                ..
            }
            | E::Index { left, index: right }
            | E::Range {
                start: left,
                end: right,
            } => {
                left.names(seen);
                right.names(seen);
            }
            E::StructLiteral { fields, .. } => fields.iter().for_each(|(_, v)| v.names(seen)),
            E::Call {
                function,
                arguments,
                ..
            } => {
                function.names(seen);
                arguments.iter().for_each(|a| a.names(seen));
            }
            E::ArrayLiteral(items) | E::Tuple(items) => items.iter().for_each(|i| i.names(seen)),
            E::Match { value, arms } => {
                value.names(seen);
                for (pattern, body) in arms {
                    pattern.names(seen);
                    body.names(seen);
                }
            }
            E::Block(statements) => statements.iter().for_each(|s| s.names(seen)),
            E::InlineAsm {
                outputs, inputs, ..
            } => outputs
                .iter()
                .chain(inputs)
                .for_each(|operand| operand.expr.names(seen)),
            E::Int(_) | E::Float(_) | E::StringLit(_) | E::Boolean(_) | E::None => {}
        }
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
        | E::Cast { left, .. }
        | E::Call { function: left, .. }
        | E::Get { object: left, .. }
        | E::Try { value: left, .. } = &mut kind
        {
            kind = std::mem::replace(&mut left.kind, E::None);
        }
    }
}

impl Pattern {
    /// Call `seen` with every name the pattern uses or binds.
    pub fn names(&self, seen: &mut impl FnMut(&str)) {
        match &self.kind {
            PatternKind::Literal(expr) => expr.names(seen),
            PatternKind::Path(name) | PatternKind::Binding(name) => seen(name),
            PatternKind::Payload { path, fields } => {
                seen(path);
                fields.iter().for_each(|field| field.names(seen));
            }
            PatternKind::Or(alternatives) => alternatives.iter().for_each(|alt| alt.names(seen)),
            PatternKind::Default => {}
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
