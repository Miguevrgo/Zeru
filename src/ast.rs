use std::collections::HashMap;

use crate::errors::Span;
use crate::sema::types::Type;
use crate::token::Token;

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
    Result(Box<TypeSpec>, Option<Box<TypeSpec>>),
    Slice(Box<TypeSpec>),
    Ref(Box<TypeSpec>),
    RefMut(Box<TypeSpec>),
}

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub privates: HashMap<String, String>,
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
        ty: Option<Type>,
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
    pub ty: Option<Type>,
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
        arms: Vec<(Expression, Expression)>,
    },
    BorrowRef(Box<Expression>),
    BorrowRefMut(Box<Expression>),
    Dereference(Box<Expression>),
    Try(Box<Expression>),
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
        Self {
            kind,
            span,
            ty: None,
        }
    }

    /// The `default` arm of a `match`, which the parser writes as a name.
    pub fn is_default_pattern(&self) -> bool {
        matches!(&self.kind, ExpressionKind::Identifier(name) if name == "default")
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

/// What a walk over the tree does where it stops. Every hook does nothing by
/// default, so a visitor names only what it cares about.
pub trait Visitor {
    /// A type written in the source, as a whole.
    fn ty(&mut self, _spec: &mut TypeSpec) {}
    /// A name an expression uses: a variable, a function, a path, the type
    /// of a struct literal.
    fn name(&mut self, _name: &mut String) {}
    /// The name a top-level item declares, or a trait named in a bound.
    fn item(&mut self, _name: &mut String) {}
    /// A local the statements after it can see, until the scope it is in ends.
    fn bind(&mut self, _name: &str) {}
    /// Where the current scope starts, for `leave` to cut back to.
    fn scope(&self) -> usize {
        0
    }
    fn leave(&mut self, _scope: usize) {}
}

/// Every name a walk comes across, as a [`Visitor`] collects them.
#[derive(Default)]
pub struct Names(pub Vec<String>);

impl Visitor for Names {
    fn name(&mut self, name: &mut String) {
        self.0.push(name.clone());
    }
}

/// Walk a top-level item: a function, a struct with its methods, an enum, a
/// trait or a constant.
pub fn walk_item(v: &mut impl Visitor, statement: &mut Statement) {
    match &mut statement.kind {
        StatementKind::Var {
            name,
            value,
            type_annotation,
            ..
        } => {
            v.item(name);
            walk_expression(v, value);
            if let Some(spec) = type_annotation {
                v.ty(spec);
            }
        }
        StatementKind::Function {
            name,
            type_params,
            params,
            return_type,
            body,
        } => {
            v.item(name);
            walk_function(v, type_params, params, return_type, body);
        }
        StatementKind::Struct {
            name,
            type_params,
            fields,
            methods,
        } => {
            v.item(name);
            walk_bounds(v, type_params);
            for (_, spec) in fields.iter_mut() {
                v.ty(spec);
            }
            for method in methods {
                if let StatementKind::Function {
                    type_params,
                    params,
                    return_type,
                    body,
                    ..
                } = &mut method.kind
                {
                    walk_function(v, type_params, params, return_type, body);
                }
            }
        }
        StatementKind::Enum { name, variants } => {
            v.item(name);
            for (_, fields) in variants.iter_mut() {
                fields.iter_mut().for_each(|spec| v.ty(spec));
            }
        }
        StatementKind::Trait { name, methods } => {
            v.item(name);
            for method in methods {
                for (_, spec, _) in method.params.iter_mut() {
                    v.ty(spec);
                }
                if let Some(spec) = &mut method.return_type {
                    v.ty(spec);
                }
            }
        }
        _ => walk_statement(v, statement),
    }
}

fn walk_function(
    v: &mut impl Visitor,
    type_params: &mut [TypeParameter],
    params: &mut [(String, TypeSpec, bool)],
    return_type: &mut Option<TypeSpec>,
    body: &mut [Statement],
) {
    walk_bounds(v, type_params);
    for (_, spec, _) in params.iter_mut() {
        v.ty(spec);
    }
    if let Some(spec) = return_type {
        v.ty(spec);
    }
    let scope = v.scope();
    for (name, _, _) in params.iter() {
        v.bind(name);
    }
    walk_block(v, body);
    v.leave(scope);
}

fn walk_bounds(v: &mut impl Visitor, type_params: &mut [TypeParameter]) {
    for bound in type_params.iter_mut().filter_map(|p| p.bound.as_mut()) {
        v.item(bound);
    }
}

fn walk_block(v: &mut impl Visitor, statements: &mut [Statement]) {
    let scope = v.scope();
    for statement in statements {
        walk_statement(v, statement);
    }
    v.leave(scope);
}

fn walk_statement(v: &mut impl Visitor, statement: &mut Statement) {
    match &mut statement.kind {
        StatementKind::Var {
            name,
            value,
            type_annotation,
            ..
        } => {
            walk_expression(v, value);
            if let Some(spec) = type_annotation {
                v.ty(spec);
            }
            v.bind(name);
        }
        StatementKind::Return(value) => {
            if let Some(value) = value {
                walk_expression(v, value);
            }
        }
        StatementKind::Expression(expr) => walk_expression(v, expr),
        StatementKind::Block(body) => walk_block(v, body),
        StatementKind::While { cond, body } => {
            walk_expression(v, cond);
            walk_statement(v, body);
        }
        StatementKind::ForIn {
            variable,
            iterable,
            body,
        } => {
            walk_expression(v, iterable);
            let scope = v.scope();
            v.bind(variable);
            walk_statement(v, body);
            v.leave(scope);
        }
        StatementKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            walk_expression(v, condition);
            walk_statement(v, then_branch);
            if let Some(branch) = else_branch {
                walk_statement(v, branch);
            }
        }
        StatementKind::Function { .. }
        | StatementKind::Struct { .. }
        | StatementKind::Enum { .. }
        | StatementKind::Trait { .. } => walk_item(v, statement),
        StatementKind::Break | StatementKind::Continue | StatementKind::Import { .. } => {}
    }
}

fn walk_expression(v: &mut impl Visitor, expr: &mut Expression) {
    match &mut expr.kind {
        ExpressionKind::Identifier(name) => v.name(name),
        ExpressionKind::StructLiteral { name, fields } => {
            v.name(name);
            for (_, value) in fields.iter_mut() {
                walk_expression(v, value);
            }
        }
        ExpressionKind::Cast { left, target } => {
            walk_expression(v, left);
            v.ty(target);
        }
        ExpressionKind::Prefix { right: inner, .. }
        | ExpressionKind::ArrayRepeat { value: inner, .. }
        | ExpressionKind::Get { object: inner, .. }
        | ExpressionKind::BorrowRef(inner)
        | ExpressionKind::BorrowRefMut(inner)
        | ExpressionKind::Dereference(inner)
        | ExpressionKind::Try(inner) => walk_expression(v, inner),
        ExpressionKind::Infix { left, right, .. }
        | ExpressionKind::Assign {
            target: left,
            value: right,
            ..
        }
        | ExpressionKind::Index { left, index: right }
        | ExpressionKind::Range {
            start: left,
            end: right,
        } => {
            walk_expression(v, left);
            walk_expression(v, right);
        }
        ExpressionKind::Call {
            function,
            arguments,
        } => {
            walk_expression(v, function);
            for argument in arguments {
                walk_expression(v, argument);
            }
        }
        ExpressionKind::Match { value, arms } => {
            walk_expression(v, value);
            for (pattern, result) in arms.iter_mut() {
                let scope = v.scope();
                match &mut pattern.kind {
                    ExpressionKind::Call {
                        function,
                        arguments,
                    } => {
                        walk_expression(v, function);
                        for binding in arguments.iter() {
                            if let ExpressionKind::Identifier(name) = &binding.kind {
                                v.bind(name);
                            }
                        }
                    }
                    _ => walk_expression(v, pattern),
                }
                walk_expression(v, result);
                v.leave(scope);
            }
        }
        ExpressionKind::Block(body) => walk_block(v, body),
        ExpressionKind::ArrayLiteral(elements) | ExpressionKind::Tuple(elements) => {
            for element in elements {
                walk_expression(v, element);
            }
        }
        ExpressionKind::InlineAsm {
            outputs, inputs, ..
        } => {
            for operand in outputs.iter_mut().chain(inputs) {
                walk_expression(v, &mut operand.expr);
            }
        }
        ExpressionKind::Int(_)
        | ExpressionKind::Float(_)
        | ExpressionKind::StringLit(_)
        | ExpressionKind::Boolean(_)
        | ExpressionKind::None => {}
    }
}
