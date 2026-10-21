use crate::{
    ast::{
        AsmOperand, Expression, ExpressionKind, PRIMITIVES, Pattern, PatternKind, Program,
        Statement, StatementKind, TraitMethod, TypeParameter, TypeSpec,
    },
    errors::{Span, ZeruError},
    lexer::Lexer,
    token::Token,
};

/// A recursive descent parser over the whole token list of a file. Each
/// `parse_*` starts on the first token of what it reads and stops after its
/// last one.
pub struct Parser {
    tokens: Vec<(Token, Span)>,
    pos: usize,

    pub errors: Vec<ZeruError>,
    panic_mode: bool,

    no_struct_literal: bool,
    code_seen: bool,

    depth: usize,
    brackets: usize,
    nesting: usize,
    splits: Vec<(usize, (Token, Span))>,
    pub comments: Vec<Span>,
    pub parens: Vec<Span>,
}

/// How deep code may nest: deeper code is an
/// error, not a stack overflow in the parser or a later pass.
const MAX_NESTING: usize = 256;

/// What may follow a `match` in statement position and continue it as an
/// expression.
const POSTFIX_STARTS: [Token; 3] = [Token::Dot, Token::Question, Token::Bang];

const ITEM_STARTS: [Token; 7] = [
    Token::Fn,
    Token::Struct,
    Token::Enum,
    Token::Trait,
    Token::Const,
    Token::Import,
    Token::Pub,
];

const STATEMENT_STARTS: [Token; 10] = [
    Token::Var,
    Token::Fn,
    Token::Const,
    Token::Struct,
    Token::Enum,
    Token::If,
    Token::While,
    Token::For,
    Token::Return,
    Token::RBrace,
];

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let mut tokens = Vec::new();
        loop {
            let (token, span) = lexer.next_token();
            let end = token == Token::Eof;
            tokens.push((token, span));
            if end {
                break;
            }
        }
        let mut p = Self {
            tokens,
            pos: 0,
            errors: Vec::new(),
            panic_mode: false,
            no_struct_literal: false,
            code_seen: false,
            depth: 0,
            brackets: 0,
            nesting: 0,
            splits: Vec::new(),
            comments: std::mem::take(&mut lexer.comments),
            parens: Vec::new(),
        };
        p.report_illegal();
        p
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos].0
    }

    fn span(&self) -> Span {
        self.tokens[self.pos].1
    }

    fn prev_span(&self) -> Span {
        self.tokens[self.pos.saturating_sub(1)].1
    }

    fn at(&self, token: &Token) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(token)
    }

    fn at_any(&self, tokens: &[Token]) -> bool {
        tokens.iter().any(|t| self.at(t))
    }

    fn bump(&mut self) -> Token {
        let token = self.peek().clone();
        match token {
            Token::Eof => return token,
            Token::LBrace => self.depth += 1,
            Token::RBrace => self.depth = self.depth.saturating_sub(1),
            Token::LBracket => self.brackets += 1,
            Token::RBracket => self.brackets = self.brackets.saturating_sub(1),
            _ => {}
        }
        self.pos += 1;
        self.report_illegal();
        token
    }

    fn report_illegal(&mut self) {
        if let Token::Illegal(message) = self.peek() {
            self.errors.push(ZeruError::syntax(message, self.span()));
            self.panic_mode = true;
        }
    }

    fn eat(&mut self, token: &Token) -> bool {
        let found = self.at(token);
        if found {
            self.bump();
        }
        found
    }

    /// Run `parse` one level deeper, refusing to go past `MAX_NESTING`.
    fn nested<T>(&mut self, parse: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        if self.nesting == MAX_NESTING {
            return self.error_here(format!("Code nests more than {MAX_NESTING} levels deep"));
        }
        self.nesting += 1;
        let parsed = parse(self);
        self.nesting -= 1;
        parsed
    }

    fn expect(&mut self, token: &Token) -> Option<()> {
        if self.eat(token) {
            return Some(());
        }
        self.expected(&format!("'{token}'"))
    }

    /// Report that `what` should follow the last token read.
    fn expected<T>(&mut self, what: &str) -> Option<T> {
        let message = format!("Expected {what}, found '{}'", self.peek());
        self.error(message, self.prev_span());
        None
    }

    /// Report a problem with the next token.
    fn error_here<T>(&mut self, message: impl Into<String>) -> Option<T> {
        self.error(message, self.span());
        None
    }

    /// One error per mistake: nothing more is reported until parsing recovers,
    /// nor next to a token the lexer already rejected.
    fn error(&mut self, message: impl Into<String>, span: Span) {
        let illegal = |at: usize| matches!(self.tokens[at].0, Token::Illegal(_));
        if self.panic_mode || illegal(self.pos) || illegal(self.pos.saturating_sub(1)) {
            return;
        }
        self.panic_mode = true;
        self.errors.push(ZeruError::syntax(message, span));
    }

    /// The name that should follow the last token read.
    fn expect_identifier(&mut self) -> Option<String> {
        match self.peek() {
            Token::Identifier(_) => self.identifier("a name"),
            _ => self.expected("a name"),
        }
    }

    fn identifier(&mut self, what: &str) -> Option<String> {
        if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.bump();
            return Some(name);
        }
        self.error_here(format!("Expected {what}, found '{}'", self.peek()))
    }

    fn string(&mut self, what: &str) -> Option<String> {
        if let Token::StringLit(bytes) = self.peek() {
            let text = String::from_utf8_lossy(bytes).into_owned();
            self.bump();
            return Some(text);
        }
        self.error_here(format!("Expected {what}, found '{}'", self.peek()))
    }

    /// Whether the last error was about the next token, which recovery then
    /// skips like the rest of the broken code.
    fn blamed(&self) -> bool {
        self.errors.last().is_some_and(|e| e.span == self.span())
    }

    /// Skip the rest of a broken statement in the block whose braces hold
    /// `depth`: through its `;`, or up to where the next statement or the
    /// block's `}` starts. Braces inside the statement are skipped whole, and
    /// a `;` inside its brackets, as in `[x; n]`, does not end it.
    fn synchronize(&mut self, depth: usize, brackets: usize) {
        self.panic_mode = false;
        let mut blamed = self.blamed() && !self.at(&Token::RBrace);
        while !self.at(&Token::Eof) {
            if !blamed && self.depth == depth && self.at_any(&STATEMENT_STARTS) {
                return;
            }
            blamed = false;
            if self.bump() == Token::Semicolon
                && self.depth == depth
                && (self.brackets <= brackets || !self.at_repeat_count())
            {
                return;
            }
        }
    }

    /// Whether a count and its `]` follow, as after the `;` of `[x; n]`.
    fn at_repeat_count(&self) -> bool {
        let after = self.tokens.get(self.pos + 1).map(|(token, _)| token);
        matches!(self.peek(), Token::Int(_) | Token::Identifier(_))
            && matches!(after, Some(Token::RBracket))
    }

    /// Skip the rest of a broken top-level item: through the `}` or `;` that
    /// ends it, or up to where the next item starts.
    fn skip_item(&mut self) {
        self.panic_mode = false;
        let mut blamed = self.blamed();
        while !self.at(&Token::Eof) {
            if !blamed && self.depth == 0 && self.at_any(&ITEM_STARTS) {
                return;
            }
            blamed = false;
            let token = self.bump();
            if self.depth == 0 && matches!(token, Token::RBrace | Token::Semicolon) {
                return;
            }
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while !self.at(&Token::Eof) {
            self.panic_mode = false;
            if !self.at_any(&[Token::Import, Token::Semicolon]) {
                self.code_seen = true;
            }
            let stmt = match self.peek() {
                Token::Const => self.parse_var_statement(true),
                Token::Fn => self.parse_function_statement(),
                Token::Struct => self.parse_struct_statement(),
                Token::Enum => self.parse_enum_statement(),
                Token::Trait => self.parse_trait_statement(),
                Token::Pub => self.parse_public(),
                Token::Import => self.parse_import_statement(),
                Token::Semicolon => {
                    self.bump();
                    continue;
                }
                Token::Var => self.error_here("Global variables ('var') are not allowed. Use 'const' for constants or move 'var' inside a function"),
                token => {
                    let message = format!("Unexpected '{token}' at top level, expected fn, struct, enum, trait, const or import");
                    self.error_here(message)
                }
            };

            match stmt {
                Some(statement) => statements.push(statement),
                None => self.skip_item(),
            }
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.panic_mode = false;

        self.nested(|p| match p.peek() {
            Token::Var => p.parse_var_statement(false),
            Token::Const => p.parse_var_statement(true),
            Token::Return => p.parse_return_statement(),
            Token::Fn => p.parse_function_statement(),
            Token::If => p.parse_if_statement(),
            Token::While => p.parse_while_statement(),
            Token::For => p.parse_for_statement(),
            Token::Struct => p.parse_struct_statement(),
            Token::Enum => p.parse_enum_statement(),
            Token::Break => p.parse_jump(StatementKind::Break),
            Token::Continue => p.parse_jump(StatementKind::Continue),
            Token::LBrace => p.parse_block(),
            Token::Import => p.parse_import_statement(),
            _ => p.parse_expression_statement(),
        })
    }

    /// An expression and its `;`. A `match` ends the statement without one,
    /// unless a postfix operator continues it.
    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        let expr = match self.at(&Token::Match) {
            true => {
                let expr = self.parse_match_expression()?;
                match self.at_any(&POSTFIX_STARTS) {
                    true => self.parse_operators(expr, Precedence::Lowest)?,
                    false => expr,
                }
            }
            false => self.parse_expression(Precedence::Lowest)?,
        };
        if !matches!(expr.kind, ExpressionKind::Match { .. }) {
            self.expect(&Token::Semicolon)?;
        }
        Some(self.statement(StatementKind::Expression(expr), start))
    }

    fn statement(&self, kind: StatementKind, start: Span) -> Statement {
        Statement::new(kind, start.merge(self.prev_span()))
    }

    fn expression(&self, kind: ExpressionKind, start: Span) -> Expression {
        Expression::new(kind, start.merge(self.prev_span()))
    }

    fn parse_var_statement(&mut self, is_const: bool) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.expect_identifier()?;
        let type_annotation = match self.eat(&Token::Colon) {
            true => Some(self.parse_annotation()?),
            false => None,
        };
        self.expect(&Token::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        self.expect(&Token::Semicolon)?;

        let kind = StatementKind::Var {
            name,
            is_const,
            value,
            type_annotation,
        };
        Some(self.statement(kind, start))
    }

    fn parse_type(&mut self) -> Option<TypeSpec> {
        self.nested(|p| {
            let base = match p.peek() {
                Token::Str => {
                    p.bump();
                    TypeSpec::Slice(Box::new(TypeSpec::Named("u8".to_string())))
                }
                Token::Star => {
                    p.bump();
                    TypeSpec::Pointer(Box::new(p.parse_type()?))
                }
                Token::BitAnd => p.parse_reference_type()?,
                Token::LParen => p.parse_parenthesized_type()?,
                Token::Identifier(_) => p.parse_named_type()?,
                _ => return p.error_here("Type identifier expected"),
            };
            if p.eat(&Token::Question) {
                return Some(TypeSpec::Optional(Box::new(base)));
            }
            if p.eat(&Token::Bang) {
                return Some(TypeSpec::Result(Box::new(base)));
            }
            Some(base)
        })
    }

    /// The type of a `var` or `const`, whose `!` the lexer may have joined
    /// to the `=` after it, as in `i32!= 3`.
    fn parse_annotation(&mut self) -> Option<TypeSpec> {
        let ty = self.parse_type()?;
        let touching = self.span().start == self.prev_span().end;
        if !touching
            || !self.at(&Token::NotEq)
            || matches!(ty, TypeSpec::Optional(_) | TypeSpec::Result(_))
        {
            return Some(ty);
        }
        self.split_token(Token::Bang, Token::Assign);
        self.bump();
        Some(TypeSpec::Result(Box::new(ty)))
    }

    /// `(T)` is `T`, as in an expression; `()`, `(T,)` and `(T, U)` are tuples.
    fn parse_parenthesized_type(&mut self) -> Option<TypeSpec> {
        self.bump();
        if self.eat(&Token::RParen) {
            return Some(TypeSpec::Tuple(Vec::new()));
        }
        let first = self.parse_type()?;
        if self.eat(&Token::RParen) {
            return Some(first);
        }
        self.expect(&Token::Comma)?;
        let mut parts = vec![first];
        parts.extend(self.parse_list(&Token::RParen, Self::parse_type)?);
        Some(TypeSpec::Tuple(parts))
    }

    /// `&[T]` is a slice, `&var T` a mutable reference, `&T` a shared one.
    fn parse_reference_type(&mut self) -> Option<TypeSpec> {
        self.bump();
        if self.eat(&Token::LBracket) {
            let elem_type = self.parse_type()?;
            self.expect(&Token::RBracket)?;
            return Some(TypeSpec::Slice(Box::new(elem_type)));
        }
        if self.eat(&Token::Var) {
            return Some(TypeSpec::RefMut(Box::new(self.parse_type()?)));
        }
        Some(TypeSpec::Ref(Box::new(self.parse_type()?)))
    }

    fn parse_named_type(&mut self) -> Option<TypeSpec> {
        let name = self.parse_path(&Token::DoubleColon)?.join("::");
        if self.at(&Token::Lt) && !PRIMITIVES.contains(&name.as_str()) {
            let args = self.parse_generic_arguments()?;
            return Some(TypeSpec::Generic { name, args });
        }
        Some(TypeSpec::Named(name))
    }

    /// The `<..>` of a generic type. An argument is either a type or a length,
    /// as in `Array<i32, 4>`.
    fn parse_generic_arguments(&mut self) -> Option<Vec<TypeSpec>> {
        self.bump();
        let mut args = Vec::new();
        while !self.eat(&Token::Gt) {
            args.push(match self.peek() {
                Token::Int(length) => {
                    let length = *length;
                    self.bump();
                    TypeSpec::IntLiteral(length)
                }
                _ => self.parse_type()?,
            });
            self.split_closing_angle();
            if !self.at(&Token::Gt) && !self.eat(&Token::Comma) {
                return self.expected("',' or '>' in generic type");
            }
        }
        Some(args)
    }

    /// Split the `>` that closes a generic type off a longer token, so
    /// `Vec<Vec<i32>>` and `Vec<i32>= v` read as they look.
    fn split_closing_angle(&mut self) {
        let rest = match self.peek() {
            Token::ShiftRight => Token::Gt,
            Token::Geq => Token::Assign,
            Token::BitRShiftEq => Token::Geq,
            _ => return,
        };
        self.split_token(Token::Gt, rest);
    }

    /// Replace the next token by its one-character `first` and the `rest`.
    fn split_token(&mut self, first: Token, rest: Token) {
        let Span { start, end } = self.span();
        self.splits.push((self.pos, self.tokens[self.pos].clone()));
        self.tokens[self.pos] = (first, Span::new(start, start + 1));
        self.tokens
            .insert(self.pos + 1, (rest, Span::new(start + 1, end)));
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let value = match self.eat(&Token::Semicolon) {
            true => None,
            false => {
                let value = self.parse_expression(Precedence::Lowest)?;
                self.expect(&Token::Semicolon)?;
                Some(value)
            }
        };
        Some(self.statement(StatementKind::Return(value), start))
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.expect_identifier()?;
        let type_params = self.parse_type_parameters()?;
        self.expect(&Token::LParen)?;
        let params = self.parse_list(&Token::RParen, Self::parse_parameter)?;
        let return_type = self.parse_return_type(&[Token::LBrace])?;
        let span = start.merge(self.prev_span());
        let body = self.parse_block_statement()?;

        let kind = StatementKind::Function {
            name,
            type_params,
            params,
            return_type,
            body,
        };
        Some(Statement::new(kind, span))
    }

    fn parse_return_type(&mut self, body_starts: &[Token]) -> Option<Option<TypeSpec>> {
        if self.at_any(body_starts) {
            return Some(None);
        }
        if self.eat(&Token::Bang) {
            return Some(Some(TypeSpec::Result(Box::new(
                TypeSpec::Tuple(Vec::new()),
            ))));
        }
        self.parse_type().map(Some)
    }

    fn parse_type_parameters(&mut self) -> Option<Vec<TypeParameter>> {
        if !self.eat(&Token::Lt) {
            return Some(Vec::new());
        }
        self.parse_list(&Token::Gt, |p| {
            let name = p.identifier("type parameter name")?;
            let bound = match p.eat(&Token::Colon) {
                true => Some(p.parse_path(&Token::DoubleColon)?.join("::")),
                false => None,
            };
            Some(TypeParameter { name, bound })
        })
    }

    fn parse_parameter(&mut self) -> Option<(String, TypeSpec, bool)> {
        let is_mut = self.eat(&Token::Var);
        if self.eat(&Token::SelfTok) {
            let self_type = TypeSpec::Named("self".to_string());
            return Some(("self".to_string(), self_type, is_mut));
        }
        let name = self.identifier("parameter name")?;
        self.expect(&Token::Colon)?;
        Some((name, self.parse_type()?, is_mut))
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let condition = self.parse_condition()?;
        let then_branch = Box::new(self.parse_block()?);
        let else_branch = match self.eat(&Token::Else) {
            true if self.at(&Token::If) => Some(Box::new(self.parse_if_statement()?)),
            true => Some(Box::new(self.parse_block()?)),
            false => None,
        };

        let kind = StatementKind::If {
            condition,
            then_branch,
            else_branch,
        };
        Some(self.statement(kind, start))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let cond = self.parse_condition()?;
        let body = Box::new(self.parse_block()?);
        Some(self.statement(StatementKind::While { cond, body }, start))
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let mut variable = self.expect_identifier()?;
        let mut index = None;
        if self.eat(&Token::Comma) {
            index = Some(variable);
            variable = self.expect_identifier()?;
        }
        self.expect(&Token::In)?;

        let mut iterable = self.parse_condition()?;
        if self.eat(&Token::DotDot) {
            let end = self.parse_condition()?;
            let span = iterable.span.merge(end.span);
            let kind = ExpressionKind::Range {
                start: Box::new(iterable),
                end: Box::new(end),
            };
            iterable = Expression::new(kind, span);
        }
        let body = Box::new(self.parse_block()?);

        let kind = StatementKind::ForIn {
            index,
            variable,
            iterable,
            body,
        };
        Some(self.statement(kind, start))
    }

    fn parse_struct_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.expect_identifier()?;
        let type_params = self.parse_type_parameters()?;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut seen_method = false;

        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            if self.at_any(&[Token::Fn, Token::Pub]) {
                seen_method = true;
                let is_pub = self.eat(&Token::Pub);
                if is_pub && !self.at(&Token::Fn) {
                    return self.expected("'fn'");
                }
                if let Some(mut method) = self.parse_function_statement() {
                    method.is_pub = is_pub;
                    methods.push(method);
                }
                continue;
            }

            if seen_method {
                let message = format!("Struct '{name}': Fields must be declared before methods");
                self.error(message, self.prev_span());
                return None;
            }

            match self.parse_struct_field() {
                Some(field) => fields.push(field),
                None => {
                    self.skip_to_next_field();
                    continue;
                }
            }

            if self.at(&Token::RBrace) {
                break;
            }
            if !self.eat(&Token::Comma) {
                self.expected::<()>("','");
                self.panic_mode = false;
            }
        }

        self.expect(&Token::RBrace)?;
        let kind = StatementKind::Struct {
            name,
            type_params,
            fields,
            methods,
        };
        Some(self.statement(kind, start))
    }

    fn parse_struct_field(&mut self) -> Option<(String, TypeSpec)> {
        let name = self.identifier("field name")?;
        self.expect(&Token::Colon)?;
        Some((name, self.parse_type()?))
    }

    /// Run to the start of the next field, so parsing continues past a bad one.
    fn skip_to_next_field(&mut self) {
        while !self.at_any(&[Token::Comma, Token::RBrace, Token::Eof]) {
            self.bump();
        }
        self.eat(&Token::Comma);
    }

    fn parse_public(&mut self) -> Option<Statement> {
        self.bump();
        let mut item = match self.peek() {
            Token::Fn => self.parse_function_statement(),
            Token::Struct => self.parse_struct_statement(),
            Token::Enum => self.parse_enum_statement(),
            Token::Trait => self.parse_trait_statement(),
            Token::Const => self.parse_var_statement(true),
            _ => self.error_here("'pub' goes before fn, struct, enum, trait or const"),
        }?;
        item.is_pub = true;
        Some(item)
    }

    fn parse_enum_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.expect_identifier()?;
        self.expect(&Token::LBrace)?;
        let variants = self.parse_list(&Token::RBrace, |p| {
            let name = p.identifier("enum variant name")?;
            let fields = match p.eat(&Token::LParen) {
                true => p.parse_list(&Token::RParen, Self::parse_type)?,
                false => Vec::new(),
            };
            Some((name, fields))
        })?;
        Some(self.statement(StatementKind::Enum { name, variants }, start))
    }

    fn parse_trait_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.expect_identifier()?;
        self.expect(&Token::LBrace)?;

        let mut methods = Vec::new();
        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            self.expect(&Token::Fn)?;
            let name = self.expect_identifier()?;
            self.expect(&Token::LParen)?;
            let params = self.parse_list(&Token::RParen, Self::parse_parameter)?;
            let return_type =
                self.parse_return_type(&[Token::Semicolon, Token::RBrace, Token::Fn])?;
            self.expect(&Token::Semicolon)?;
            methods.push(TraitMethod {
                name,
                params,
                return_type,
            });
        }
        self.expect(&Token::RBrace)?;
        Some(self.statement(StatementKind::Trait { name, methods }, start))
    }

    /// A name, a qualified path like `module::Name`, or a struct literal when
    /// a brace follows the path where one may.
    fn parse_path_or_struct_literal(&mut self) -> Option<Expression> {
        let start = self.span();
        let path = self.parse_path(&Token::DoubleColon)?.join("::");
        if !self.no_struct_literal && self.at(&Token::LBrace) {
            return self.parse_struct_literal(path, start);
        }
        let name = self.expression(ExpressionKind::Identifier(path), start);
        self.parse_explicit_call(name)
    }

    /// `function<T, ..>(..)`, a call given its type arguments.
    /// What does not read as types closed by `>` and followed by `(` is a
    /// comparison, so the attempt is undone and `function` returned alone.
    fn parse_explicit_call(&mut self, function: Expression) -> Option<Expression> {
        if !self.at(&Token::Lt) {
            return Some(function);
        }
        let saved = (self.pos, self.errors.len(), self.panic_mode, self.brackets);
        let splits = self.splits.len();
        if let Some(type_args) = self.parse_generic_arguments()
            && !type_args
                .iter()
                .any(|arg| matches!(arg, TypeSpec::IntLiteral(_)))
            && self.eat(&Token::LParen)
        {
            let arguments =
                self.allowing_struct_literals(|p| p.parse_expressions(&Token::RParen))?;
            let start = function.span;
            let kind = ExpressionKind::Call {
                function: Box::new(function),
                type_args,
                arguments,
            };
            return Some(self.expression(kind, start));
        }
        for (at, token) in self.splits.drain(splits..).rev() {
            self.tokens.remove(at + 1);
            self.tokens[at] = token;
        }
        (self.pos, _, self.panic_mode, self.brackets) = saved;
        self.errors.truncate(saved.1);
        Some(function)
    }

    /// Read the condition of a construct whose body follows in braces.
    fn parse_condition(&mut self) -> Option<Expression> {
        let saved = std::mem::replace(&mut self.no_struct_literal, true);
        let condition = self.parse_expression(Precedence::Lowest);
        self.no_struct_literal = saved;
        condition
    }

    /// Read `parse` with struct literals allowed again, for a context already
    /// delimited by parentheses, brackets or commas.
    fn allowing_struct_literals<T>(&mut self, parse: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::replace(&mut self.no_struct_literal, false);
        let parsed = parse(self);
        self.no_struct_literal = saved;
        parsed
    }

    fn parse_struct_literal(&mut self, name: String, start: Span) -> Option<Expression> {
        self.bump();
        let fields = self.allowing_struct_literals(|p| {
            p.parse_list(&Token::RBrace, |p| {
                let field = p.identifier("field name")?;
                p.expect(&Token::Colon)?;
                Some((field, p.parse_expression(Precedence::Lowest)?))
            })
        })?;
        Some(self.expression(ExpressionKind::StructLiteral { name, fields }, start))
    }

    fn parse_block(&mut self) -> Option<Statement> {
        let start = self.span();
        let statements = self.parse_block_statement()?;
        Some(self.statement(StatementKind::Block(statements), start))
    }

    fn parse_import_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        if self.code_seen {
            self.error_here::<()>("Imports must come before any other code");
        }
        self.bump();
        let path = self.parse_path(&Token::Dot)?;
        let symbols = match self.eat(&Token::DoubleColon) {
            true => {
                self.expect(&Token::LBrace)?;
                Some(self.parse_list(&Token::RBrace, |p| p.identifier("imported name"))?)
            }
            false => None,
        };
        self.expect(&Token::Semicolon)?;
        Some(self.statement(StatementKind::Import { path, symbols }, start))
    }

    fn parse_path(&mut self, separator: &Token) -> Option<Vec<String>> {
        let mut path = vec![self.identifier("a name")?];
        while self.eat(separator) {
            path.push(self.expect_identifier()?);
        }
        Some(path)
    }

    fn parse_jump(&mut self, kind: StatementKind) -> Option<Statement> {
        let start = self.span();
        self.bump();
        self.expect(&Token::Semicolon)?;
        Some(self.statement(kind, start))
    }

    fn parse_block_statement(&mut self) -> Option<Vec<Statement>> {
        self.expect(&Token::LBrace)?;
        let (depth, brackets) = (self.depth, self.brackets);
        let mut block = Vec::new();

        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            match self.parse_statement() {
                Some(stmt) => block.push(stmt),
                None => self.synchronize(depth, brackets),
            }
        }

        if !self.eat(&Token::RBrace)
            && self
                .errors
                .last()
                .is_none_or(|e| e.span.end < self.span().start)
        {
            self.error_here::<()>("Expected '}', found 'end of file'");
        }
        Some(block)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        self.nested(|p| {
            let operand = p.parse_operand()?;
            p.parse_operators(operand, precedence)
        })
    }

    /// The operators after `left` that bind tighter than `precedence`.
    /// Comparisons do not chain: `a < b == c` is an error.
    fn parse_operators(
        &mut self,
        mut left: Expression,
        precedence: Precedence,
    ) -> Option<Expression> {
        let mut compared = false;
        while precedence < token_precedence(self.peek()) {
            let comparison = matches!(
                token_precedence(self.peek()),
                Precedence::Equals | Precedence::LessGreater
            );
            if comparison && (compared || precedence == Precedence::Equals) {
                return self.error_here("Comparison operators cannot be chained");
            }
            compared = comparison;
            left = self.parse_infix_expression(left)?;
        }
        Some(left)
    }

    fn parse_operand(&mut self) -> Option<Expression> {
        let start = self.span();
        let literal = match self.peek() {
            Token::Int(val) => Some(ExpressionKind::Int(*val)),
            Token::Float(val) => Some(ExpressionKind::Float(*val)),
            Token::StringLit(val) => Some(ExpressionKind::StringLit(val.clone())),
            Token::None => Some(ExpressionKind::None),
            Token::True => Some(ExpressionKind::Boolean(true)),
            Token::False => Some(ExpressionKind::Boolean(false)),
            Token::SelfTok => Some(ExpressionKind::Identifier("self".to_string())),
            _ => None,
        };
        match literal {
            Some(kind) => {
                self.bump();
                Some(Expression::new(kind, start))
            }
            None => match self.peek() {
                Token::LBracket => self.parse_array_literal(),
                Token::Identifier(_) => self.parse_path_or_struct_literal(),
                Token::LParen => self.parse_grouped_expression(),
                Token::Minus | Token::Bang => {
                    self.parse_prefix(|operator, right| ExpressionKind::Prefix { operator, right })
                }
                Token::Star => self.parse_prefix(|_, expr| ExpressionKind::Dereference(expr)),
                Token::BitAnd => self.parse_borrow_expression(),
                Token::Match => self.parse_match_expression(),
                Token::Asm => self.parse_asm_expression(),
                token => self.error_here(format!("Expected an expression, found '{token}'")),
            },
        }
    }

    /// `(a)` is `a`; `()`, `(a,)` and `(a, b)` are tuples.
    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        self.allowing_struct_literals(|p| {
            if p.eat(&Token::RParen) {
                return Some(p.expression(ExpressionKind::Tuple(vec![]), start));
            }
            let first = p.parse_expression(Precedence::Lowest)?;
            if !p.eat(&Token::Comma) {
                p.expect(&Token::RParen)?;
                p.parens.push(first.span);
                return Some(first);
            }
            let mut elements = vec![first];
            elements.extend(p.parse_expressions(&Token::RParen)?);
            Some(p.expression(ExpressionKind::Tuple(elements), start))
        })
    }

    fn parse_match_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let value = self.parse_condition()?;
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            let pattern = match self.at(&Token::Default) {
                true => {
                    let span = self.span();
                    self.bump();
                    Pattern {
                        kind: PatternKind::Default,
                        span,
                    }
                }
                false => self.parse_pattern(false)?,
            };
            self.expect(&Token::Arrow)?;

            let body = match self.at(&Token::LBrace) {
                true => {
                    let span = self.span();
                    let body = self.parse_block_statement()?;
                    self.expression(ExpressionKind::Block(body), span)
                }
                false => self.parse_expression(Precedence::Lowest)?,
            };
            let block = matches!(body.kind, ExpressionKind::Block(_));
            arms.push((pattern, body));
            if block || self.at(&Token::RBrace) {
                self.eat(&Token::Comma);
            } else {
                self.expect(&Token::Comma)?;
            }
        }
        self.expect(&Token::RBrace)?;

        let kind = ExpressionKind::Match {
            value: Box::new(value),
            arms,
        };
        Some(self.expression(kind, start))
    }

    /// Alternatives separated by `|`. A name alone is a binding inside a
    /// payload and a constant or variant elsewhere.
    fn parse_pattern(&mut self, in_payload: bool) -> Option<Pattern> {
        self.nested(|p| {
            let start = p.span();
            let first = p.parse_alternative(in_payload)?;
            if !p.at(&Token::BitOr) {
                return Some(first);
            }
            let mut alternatives = vec![first];
            while p.eat(&Token::BitOr) {
                alternatives.push(p.parse_alternative(in_payload)?);
            }
            Some(Pattern {
                kind: PatternKind::Or(alternatives),
                span: start.merge(p.prev_span()),
            })
        })
    }

    /// One alternative: `Some(p)` or `E::V(p, q)`, a name bound inside a
    /// payload, or else a constant expression stopping before `|`.
    fn parse_alternative(&mut self, in_payload: bool) -> Option<Pattern> {
        let start = self.span();
        let kind = match self.peek() {
            Token::Identifier(_) => {
                let saved = self.pos;
                let path = self.parse_path(&Token::DoubleColon)?.join("::");
                let ends = self.at_any(&[Token::Comma, Token::RParen, Token::BitOr]);
                match self.eat(&Token::LParen) {
                    true => PatternKind::Payload {
                        path,
                        fields: self.parse_list(&Token::RParen, |p| p.parse_pattern(true))?,
                    },
                    false if in_payload && ends && !path.contains("::") => {
                        PatternKind::Binding(path)
                    }
                    false => {
                        self.pos = saved;
                        PatternKind::Constant(self.parse_expression(Precedence::BitwiseOr)?)
                    }
                }
            }
            _ => PatternKind::Constant(self.parse_expression(Precedence::BitwiseOr)?),
        };
        Some(Pattern {
            kind,
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_asm_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let is_volatile = self.eat(&Token::Volatile);
        self.expect(&Token::LParen)?;
        let template = self.string("assembly template string")?;
        let outputs = self.parse_asm_section(Self::parse_asm_operand)?;
        let inputs = self.parse_asm_section(Self::parse_asm_operand)?;
        let clobbers = self.parse_asm_section(|p| p.string("clobber string"))?;
        self.expect(&Token::RParen)?;

        let kind = ExpressionKind::InlineAsm {
            template,
            outputs,
            inputs,
            clobbers,
            is_volatile,
        };
        Some(self.expression(kind, start))
    }

    /// `: item, item` of an asm statement, or nothing when the colon is absent.
    fn parse_asm_section<T>(
        &mut self,
        mut item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Vec<T>> {
        let mut items = Vec::new();
        if !self.eat(&Token::Colon) || self.at_any(&[Token::Colon, Token::RParen]) {
            return Some(items);
        }
        loop {
            items.push(item(self)?);
            if !self.eat(&Token::Comma) {
                return Some(items);
            }
        }
    }

    fn parse_asm_operand(&mut self) -> Option<AsmOperand> {
        let constraint = self.string("constraint string in assembly operand")?;
        self.expect(&Token::LParen)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect(&Token::RParen)?;
        Some(AsmOperand { constraint, expr })
    }

    /// An operator before its operand, built into an expression by `build`.
    fn parse_prefix(
        &mut self,
        build: impl FnOnce(Token, Box<Expression>) -> ExpressionKind,
    ) -> Option<Expression> {
        let start = self.span();
        let operator = self.bump();
        let operand = self.parse_expression(Precedence::Prefix)?;
        Some(self.expression(build(operator, Box::new(operand)), start))
    }

    fn parse_borrow_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let is_mutable = self.eat(&Token::Var);
        let expr = Box::new(self.parse_expression(Precedence::Prefix)?);
        let kind = match is_mutable {
            true => ExpressionKind::BorrowRefMut(expr),
            false => ExpressionKind::BorrowRef(expr),
        };
        Some(self.expression(kind, start))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let start = left.span;
        let operator = self.bump();
        let left = Box::new(left);

        let kind = match operator {
            Token::LBracket => {
                let bound = |p: &mut Self| match p.at(&Token::DotDot) || p.at(&Token::RBracket) {
                    true => Some(None),
                    false => p
                        .allowing_struct_literals(|p| p.parse_expression(Precedence::Lowest))
                        .map(|e| Some(Box::new(e))),
                };
                let start = bound(self)?;
                if self.eat(&Token::DotDot) {
                    let end = bound(self)?;
                    self.expect(&Token::RBracket)?;
                    ExpressionKind::Slice { left, start, end }
                } else {
                    let Some(index) = start else {
                        return self.error_here("Expected an index or a range");
                    };
                    self.expect(&Token::RBracket)?;
                    ExpressionKind::Index { left, index }
                }
            }
            Token::LParen => {
                let arguments =
                    self.allowing_struct_literals(|p| p.parse_expressions(&Token::RParen))?;
                ExpressionKind::Call {
                    function: left,
                    type_args: Vec::new(),
                    arguments,
                }
            }
            Token::Dot => {
                let name = match self.peek() {
                    Token::Identifier(name) => name.clone(),
                    Token::Int(index) => index.to_string(),
                    _ => return self.error_here("Expected a field name after '.'"),
                };
                self.bump();
                let get = self.expression(ExpressionKind::Get { object: left, name }, start);
                return self.parse_explicit_call(get);
            }
            Token::Question | Token::Bang => ExpressionKind::Try {
                operator,
                value: left,
            },
            Token::As => ExpressionKind::Cast {
                left,
                target: self.parse_type()?,
            },
            _ => {
                let precedence = token_precedence(&operator);
                let assigns = precedence == Precedence::Assignment;
                let right = Box::new(self.parse_expression(match assigns {
                    true => Precedence::Lowest,
                    false => precedence,
                })?);
                let span = start.merge(right.span);
                let kind = match assigns {
                    true => ExpressionKind::Assign {
                        target: left,
                        operator,
                        value: right,
                    },
                    false => ExpressionKind::Infix {
                        left,
                        operator,
                        right,
                    },
                };
                return Some(Expression::new(kind, span));
            }
        };
        Some(self.expression(kind, start))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let kind = self.allowing_struct_literals(Self::parse_array_elements)?;
        Some(self.expression(kind, start))
    }

    fn parse_array_elements(&mut self) -> Option<ExpressionKind> {
        if self.eat(&Token::RBracket) {
            return Some(ExpressionKind::ArrayLiteral(Vec::new()));
        }
        let first = self.parse_expression(Precedence::Lowest)?;

        if self.eat(&Token::Semicolon) {
            let count = self.parse_expression(Precedence::Lowest)?;
            self.expect(&Token::RBracket)?;
            return Some(ExpressionKind::ArrayRepeat {
                value: Box::new(first),
                count: Box::new(count),
            });
        }

        let mut elements = vec![first];
        if self.eat(&Token::Comma) {
            elements.extend(self.parse_expressions(&Token::RBracket)?);
        } else {
            self.expect(&Token::RBracket)?;
        }
        Some(ExpressionKind::ArrayLiteral(elements))
    }

    /// `item, item, ..` up to `close`, a trailing comma allowed. Starts after
    /// the opening token and ends after `close`.
    fn parse_list<T>(
        &mut self,
        close: &Token,
        mut item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<Vec<T>> {
        let mut items = Vec::new();
        while !self.eat(close) {
            items.push(item(self)?);
            if !self.at(close) {
                self.expect(&Token::Comma)?;
            }
        }
        Some(items)
    }

    fn parse_expressions(&mut self, close: &Token) -> Option<Vec<Expression>> {
        self.parse_list(close, |p| p.parse_expression(Precedence::Lowest))
    }
}

#[derive(PartialEq, PartialOrd)]
/// Loosest first. Bitwise operators bind tighter than comparison, so
/// `flags & MASK == 0` groups the way it reads.
enum Precedence {
    Lowest,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Equals,
    LessGreater,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Shift,
    Sum,
    Product,
    Cast,
    Prefix,
    Call,
    Index,
}

fn token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Assign
        | Token::PlusEq
        | Token::MinusEq
        | Token::StarEq
        | Token::SlashEq
        | Token::ModEq
        | Token::BitAndEq
        | Token::BitOrEq
        | Token::BitXorEq
        | Token::BitRShiftEq
        | Token::BitLShiftEq
        | Token::PlusWrapEq
        | Token::MinusWrapEq
        | Token::StarWrapEq => Precedence::Assignment,

        Token::Or => Precedence::LogicalOr,
        Token::And => Precedence::LogicalAnd,

        Token::BitOr => Precedence::BitwiseOr,
        Token::BitXor => Precedence::BitwiseXor,
        Token::BitAnd => Precedence::BitwiseAnd,

        Token::Eq | Token::NotEq => Precedence::Equals,

        Token::Gt | Token::Lt | Token::Geq | Token::Leq => Precedence::LessGreater,
        Token::ShiftLeft | Token::ShiftRight => Precedence::Shift,

        Token::Plus | Token::Minus | Token::PlusWrap | Token::MinusWrap => Precedence::Sum,
        Token::Star | Token::Slash | Token::Mod | Token::StarWrap => Precedence::Product,

        Token::As => Precedence::Cast,
        Token::LParen => Precedence::Call,
        Token::Dot | Token::LBracket | Token::Question | Token::Bang => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ExpressionKind, Program, Statement, StatementKind, TypeSpec},
        lexer::Lexer,
        parser::Parser,
        token::Token,
    };

    fn parse_input(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        program
    }

    #[test]
    fn test_generic_type_takes_a_suffix() {
        let program = parse_input("fn f() Vec<i64>? { } fn g() Vec<Vec<i64>>! { }");
        let suffixes: Vec<_> = program
            .statements
            .iter()
            .map(|stmt| match &stmt.kind {
                StatementKind::Function {
                    return_type: Some(TypeSpec::Optional(_)),
                    ..
                } => "?",
                StatementKind::Function {
                    return_type: Some(TypeSpec::Result(..)),
                    ..
                } => "!",
                other => panic!("{other:?}"),
            })
            .collect();
        assert_eq!(suffixes, ["?", "!"]);
    }

    fn check_parser_errors(parser: &Parser) {
        if parser.errors.is_empty() {
            return;
        }
        eprintln!("Parser has {} errors:", parser.errors.len());
        for err in &parser.errors {
            eprintln!("Parser error: {}", err.message);
        }
        panic!("Parser failed")
    }

    fn get_function_body(statement: &Statement) -> &Vec<Statement> {
        match &statement.kind {
            StatementKind::Function { body, .. } => body,
            _ => panic!("Expected Function statement"),
        }
    }

    /// Render an expression's shape so precedence is visible in one string.
    fn shape(expr: &crate::ast::Expression) -> String {
        match &expr.kind {
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => format!("({} {:?} {})", shape(left), operator, shape(right)),
            ExpressionKind::Int(v) => v.to_string(),
            ExpressionKind::Identifier(name) => name.clone(),
            other => format!("{other:?}"),
        }
    }

    #[test]
    fn test_trailing_comma_everywhere_a_list_ends() {
        for source in [
            "fn f(a: i32, b: i32,) { }",
            "fn main() { f(1, 2,); }",
            "fn main() { var a: Array<i32, 2> = [1, 2,]; }",
            "fn f(t: (i32, i64,)) { }",
            "struct S { a: i32, b: i32, }",
        ] {
            parse_input(source);
        }
    }

    #[test]
    fn test_import_after_code_is_rejected() {
        for source in [
            "fn f() { }\nimport std.math;",
            "fn f() { import std.math; }",
        ] {
            let mut parser = Parser::new(Lexer::new(source));
            parser.parse_program();
            let messages: Vec<_> = parser.errors.iter().map(|e| &e.message).collect();
            assert_eq!(
                messages,
                ["Imports must come before any other code"],
                "{source}"
            );
        }
    }

    #[test]
    fn test_stray_semicolons_and_generic_close_before_assign() {
        for source in [
            ";fn main() { }",
            "const A: i32 = 1;; fn main() { }",
            "fn main() { var v: Vec<i32>= x; var w: Vec<Vec<i32>>= y; }",
        ] {
            parse_input(source);
        }
    }

    #[test]
    fn test_unclosed_block_is_rejected() {
        for source in ["fn main() { f();", "fn main() { while a { a += 1; }"] {
            let mut parser = Parser::new(Lexer::new(source));
            parser.parse_program();
            let messages: Vec<_> = parser.errors.iter().map(|e| &e.message).collect();
            assert_eq!(messages, ["Expected '}', found 'end of file'"], "{source}");
        }
    }

    #[test]
    fn test_literal_beyond_every_integer_type_is_rejected() {
        let mut parser = Parser::new(Lexer::new(
            "fn main() { var a: u64 = 99999999999999999999999; }",
        ));
        parser.parse_program();
        let messages: Vec<_> = parser.errors.iter().map(|e| &e.message).collect();
        assert!(
            messages[0].contains("Integer literal '99999999999999999999999' is out of range"),
            "{messages:?}"
        );
    }

    #[test]
    fn test_cast_target_is_a_type() {
        let program =
            parse_input("fn main() { var a = x as i64 < 0; var b = p as *Array<u8, 4>; }");
        let body = get_function_body(&program.statements[0]);

        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        let ExpressionKind::Infix { left, operator, .. } = &value.kind else {
            panic!("Expected a comparison, got {:?}", value.kind);
        };
        assert_eq!(*operator, Token::Lt);
        assert!(matches!(left.kind, ExpressionKind::Cast { .. }));

        let StatementKind::Var { value, .. } = &body[1].kind else {
            panic!("Expected a var statement");
        };
        let ExpressionKind::Cast { target, .. } = &value.kind else {
            panic!("Expected a cast, got {:?}", value.kind);
        };
        let array = TypeSpec::Generic {
            name: "Array".to_string(),
            args: vec![TypeSpec::Named("u8".to_string()), TypeSpec::IntLiteral(4)],
        };
        assert_eq!(*target, TypeSpec::Pointer(Box::new(array)));
    }

    #[test]
    fn test_nested_generic_closes_on_a_single_shift_token() {
        for source in [
            "fn f(v: Vec<Vec<i32>>) { }",
            "fn f(v: Vec<Vec<Vec<i32>>>) { }",
            "fn f(v: Vec<Array<i32, 2>>) { }",
            "fn f(v: Array<Vec<i32>, 2>) { }",
        ] {
            parse_input(source);
        }
    }

    #[test]
    fn test_qualified_struct_literal() {
        let program = parse_input("fn main() { var t = shapes::Rect { w: 1, h: 2 }; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        let ExpressionKind::StructLiteral { name, fields } = &value.kind else {
            panic!("Expected a struct literal, got {:?}", value.kind);
        };
        assert_eq!(name, "shapes::Rect");
        assert_eq!(fields.len(), 2);
    }

    #[test]
    fn test_a_condition_brace_opens_the_body() {
        let program = parse_input("fn main() { if c == Color::Red { var inside = 1; } }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::If {
            condition,
            then_branch,
            ..
        } = &body[0].kind
        else {
            panic!("Expected an if statement, got {:?}", body[0].kind);
        };
        assert_eq!(shape(condition), "(c Eq Color::Red)");
        assert!(matches!(then_branch.kind, StatementKind::Block(ref b) if b.len() == 1));
    }

    #[test]
    fn test_a_delimited_brace_is_still_a_literal() {
        for input in [
            "fn main() { if take(P { x: 1 }) { } }",
            "fn main() { if list[P { x: 1 }] { } }",
            "fn main() { if (P { x: 1 }).x { } }",
        ] {
            let program = parse_input(input);
            let body = get_function_body(&program.statements[0]);
            assert!(
                matches!(body[0].kind, StatementKind::If { .. }),
                "{input} did not parse as an if"
            );
        }
    }

    #[test]
    fn test_operator_precedence() {
        for (input, expected) in [
            ("(5 > 5) == (3 < 4)", "((5 Gt 5) Eq (3 Lt 4))"),
            ("(5 < 5) != (3 > 4)", "((5 Lt 5) NotEq (3 Gt 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 Plus (4 Star 5)) Eq ((3 Star 1) Plus (4 Star 5)))",
            ),
            (
                "4 + 5 % 2 == 4 * 1 + 5 % 2",
                "((4 Plus (5 Mod 2)) Eq ((4 Star 1) Plus (5 Mod 2)))",
            ),
            ("(5 + 5) * 2", "((5 Plus 5) Star 2)"),
            ("a - b - c", "((a Minus b) Minus c)"),
            ("a >> 2", "(a ShiftRight 2)"),
            ("module::item", "module::item"),
            ("flags & mask == 0", "((flags BitAnd mask) Eq 0)"),
            ("a | b ^ c & d", "(a BitOr (b BitXor (c BitAnd d)))"),
            ("a << 2 + 1", "(a ShiftLeft (2 Plus 1))"),
            ("(42)", "42"),
        ] {
            let program = parse_input(&format!("fn main() {{ var t = {input}; }}"));
            let body = get_function_body(&program.statements[0]);
            let StatementKind::Var { value, .. } = &body[0].kind else {
                panic!("Expected a var statement");
            };
            assert_eq!(shape(value), expected, "{input}");
        }
    }

    #[test]
    fn test_single_element_with_comma_is_tuple() {
        let input = "fn main() { var t = (42,); }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Var { value, .. } => match &value.kind {
                ExpressionKind::Tuple(elements) => {
                    assert_eq!(elements.len(), 1);
                }
                _ => panic!("Expected single-element Tuple"),
            },
            _ => panic!("Expected Var statement"),
        }
    }
}
