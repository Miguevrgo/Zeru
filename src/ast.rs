use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    Named(String),
    Generic { name: String, args: Vec<TypeSpec> },
    IntLiteral(i64),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
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
        params: Vec<(String, TypeSpec)>,
        return_type: Option<TypeSpec>,
        body: Vec<Statement>,
    },
    Struct {
        name: String,
        fields: Vec<(String, TypeSpec)>,
        methods: Vec<Statement>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Import {
        #[allow(dead_code)]
        // TODO: Version 1.0.0 module system
        path: String,
        symbols: Vec<String>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Float(f64),
    StringLit(String),
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
        arguments: Vec<Expression>,
    },

    Get {
        object: Box<Expression>,
        name: String,
    },
    ArrayLiteral(Vec<Expression>),
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
        target: Box<Expression>,
    },
    Match {
        value: Box<Expression>,
        arms: Vec<(Expression, Expression)>,
    },
}
