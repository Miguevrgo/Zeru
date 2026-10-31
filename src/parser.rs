use crate::{
    ast::{
        AsmOperand, Closure, ClosureBody, Expression, ExpressionKind, MatchArm, Names, PRIMITIVES,
        Pattern, PatternKind, Program, Statement, StatementKind, TraitMethod, TypeParameter,
        TypeSpec,
    },
    errors::{Span, ZeruError},
    lexer::Lexer,
    token::Token,
};

/// A recursive descent parser over the whole token list of a file. Each
/// `parse_*` starts on the first token of what it reads and stops after its
/// last one.
/// Tokens replaced while parsing, so a rewind can put them back: where,
/// the ones taken out, and how many went in.
type Rewrite = (usize, Vec<(Token, Span)>, usize);

pub struct Parser {
    tokens: Vec<(Token, Span)>,
    pos: usize,

    pub errors: Vec<ZeruError>,
    panic_mode: bool,

    no_struct_literal: bool,
    in_pattern: bool,
    argument_at: usize,
    code_seen: bool,

    depth: usize,
    brackets: usize,
    nesting: usize,
    rewrites: Vec<Rewrite>,
    opens: Vec<Span>,
    arm_blocks: Vec<usize>,
    pub comments: Vec<Span>,
    nested: Vec<(Span, usize)>,
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
            in_pattern: false,
            argument_at: usize::MAX,
            code_seen: false,
            depth: 0,
            brackets: 0,
            nesting: 0,
            rewrites: Vec::new(),
            opens: Vec::new(),
            arm_blocks: Vec::new(),
            comments: lexer.comments,
            nested: lexer.nested,
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
            Token::LBrace => {
                self.depth += 1;
                self.opens.push(self.span());
            }
            Token::RBrace => {
                self.depth = self.depth.saturating_sub(1);
                self.opens.pop();
            }
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
        let found = self.peek().clone();
        let before = self.prev_span();
        match (token, &found) {
            (Token::RBrace, Token::Eof) => {
                let open = self.opens.last().copied();
                let error = self.error("Expected '}', found the end of the file", self.span())?;
                if let Some(open) = open {
                    error.note("this '{' is never closed", open);
                }
                None
            }
            (Token::Semicolon, _) if self.after_nested_comment(before).is_some() => {
                let inner = self.after_nested_comment(before)?;
                let error = self.error(
                    "Block comments do not nest, so this is not commented out",
                    before,
                )?;
                error
                    .note(
                        "this '/*' starts nothing; the comment ends at the first '*/'",
                        inner,
                    )
                    .help("comment out each line with // instead");
                None
            }
            (Token::Semicolon, _) => {
                let in_arm = found == Token::RBrace && self.arm_blocks.last() == Some(&self.depth);
                let error =
                    self.error(format!("Expected ';' after this, found '{found}'"), before)?;
                let starts = matches!(
                    found,
                    Token::Identifier(_)
                        | Token::Var
                        | Token::Const
                        | Token::If
                        | Token::While
                        | Token::For
                        | Token::Return
                        | Token::Match
                        | Token::Break
                        | Token::Continue
                        | Token::RBrace
                        | Token::Fn
                        | Token::Pub
                        | Token::Struct
                        | Token::Enum
                        | Token::Trait
                        | Token::Import
                );
                if starts {
                    error
                        .label("the statement ends here")
                        .help(match in_arm {
                            true => "end the statement with ; (a block gives no value: an arm that gives one is a single expression, as in 1 => b * 2)",
                            false => "end the statement with ;",
                        })
                        .fix(Span::new(before.end, before.end), ";");
                } else if found == Token::DoubleColon {
                    error.help("a module is imported first, then named by its last part, as in import std.fs; and fs::read_file(..)");
                }
                None
            }
            _ => self.expected(&format!("'{token}'")),
        }
    }

    /// The `/*` inside the block comment that ends right before the token
    /// at `span`, with nothing else between them.
    fn after_nested_comment(&self, span: Span) -> Option<Span> {
        let at = self.tokens.iter().position(|(_, s)| *s == span)?;
        let prior = at.checked_sub(1).map_or(0, |at| self.tokens[at].1.end);
        self.nested
            .iter()
            .find(|&&(inner, end)| prior <= inner.start && end <= span.start)
            .map(|&(inner, _)| inner)
    }

    /// Report a slip at the next `count` tokens, written as another
    /// language writes it, and read them as `with`, which `fix` writes.
    fn slip(&mut self, count: usize, message: &str, help: &str, fix: &str, with: Vec<Token>) {
        let span = self.span().merge(self.tokens[self.pos + count - 1].1);
        if let Some(error) = self.error(message, span) {
            error.help(help).fix(span, fix);
        }
        self.panic_mode = false;
        let with = with.into_iter().map(|token| (token, span)).collect();
        self.rewrite(self.pos, count, with);
    }

    /// The token `ahead` places after the next one.
    fn ahead(&self, ahead: usize) -> &Token {
        self.tokens
            .get(self.pos + ahead)
            .map_or(&Token::Eof, |(token, _)| token)
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
    fn error(&mut self, message: impl Into<String>, span: Span) -> Option<&mut ZeruError> {
        let illegal = |at: usize| matches!(self.tokens[at].0, Token::Illegal(_));
        if self.panic_mode || illegal(self.pos) || illegal(self.pos.saturating_sub(1)) {
            return None;
        }
        self.panic_mode = true;
        self.errors.push(ZeruError::syntax(message, span));
        self.errors.last_mut()
    }

    /// The name that should follow the last token read.
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
        let statements = self.parse_items(false);
        let mut tests = Vec::new();
        if self.eat(&Token::Tests) {
            self.code_seen = false;
            if self.expect(&Token::LBrace).is_some() {
                self.depth = 0;
                tests = self.parse_items(true);
                self.expect(&Token::RBrace);
            }
            if !self.at(&Token::Eof) {
                self.error_here::<()>("'#test' goes once, at the end of the file");
            }
        }
        Program { statements, tests }
    }

    /// The items up to the end of the file, or inside `#test { }` with
    /// `tests`, up to its `}`.
    fn parse_items(&mut self, tests: bool) -> Vec<Statement> {
        let mut statements = Vec::new();
        while !self.at_any(&[Token::Eof, Token::Tests]) && !(tests && self.at(&Token::RBrace)) {
            self.panic_mode = false;
            if !self.at_any(&[Token::Import, Token::Semicolon]) {
                self.code_seen = true;
            }
            let is_pub = self.eat(&Token::Pub);
            let stmt = match self.peek() {
                Token::Const => self.parse_var_statement(true),
                Token::Fn => self.parse_function_statement(),
                Token::Struct => self.parse_struct_statement(),
                Token::Enum => self.parse_enum_statement(),
                Token::Trait => self.parse_trait_statement(),
                _ if is_pub => self.error_here("'pub' goes before fn, struct, enum, trait or const"),
                Token::Import => self.parse_import_statement(),
                Token::Semicolon => {
                    self.bump();
                    continue;
                }
                Token::Identifier(name) if name == "test" => match tests {
                    true => self.parse_test().map(|(body, runner)| {
                        statements.push(body);
                        runner
                    }),
                    false => self.error_here("Tests go inside '#test { }', at the end of the file"),
                },
                Token::Var => self.error_here("Global variables ('var') are not allowed. Use 'const' for constants or move 'var' inside a function"),
                Token::Identifier(word)
                    if ["def", "func", "function", "fun"].contains(&word.as_str())
                        && matches!(self.ahead(1), Token::Identifier(_)) =>
                {
                    self.slip(1, "Zeru declares a function with fn", "write fn", "fn", vec![Token::Fn]);
                    self.parse_function_statement()
                }
                token => {
                    let message = format!("Unexpected '{token}' at top level, expected fn, struct, enum, trait, const or import");
                    self.error_here(message)
                }
            }
            .map(|mut stmt| {
                stmt.is_pub = is_pub;
                stmt
            });

            match stmt {
                Some(statement) => statements.push(statement),
                None => self.skip_item(),
            }
        }

        statements
    }

    /// `test("name") { body }`: the body as a function `!name` returning `!`,
    /// and `#name`, the one `zeru test` runs, unwrapping its result so an
    /// error fails the test as a panic does.
    fn parse_test(&mut self) -> Option<(Statement, Statement)> {
        let start = self.span();
        self.bump();
        self.expect(&Token::LParen)?;
        let name = self.string("the test's name")?;
        self.expect(&Token::RParen)?;
        let span = start.merge(self.prev_span());
        let function = |name: String, return_type, body| {
            let kind = StatementKind::Function {
                name,
                type_params: Vec::new(),
                params: Vec::new(),
                return_type,
                body,
            };
            Statement::new(kind, span)
        };
        let result = TypeSpec::Result(Box::new(TypeSpec::Tuple(Vec::new())));
        let body = function(
            format!("!{name}"),
            Some(result),
            self.parse_block_statement()?,
        );
        let expression = |kind| Box::new(Expression::new(kind, span));
        let call = |function| ExpressionKind::Call {
            function,
            type_args: Vec::new(),
            arguments: Vec::new(),
        };
        let unwrap = call(expression(ExpressionKind::Get {
            object: expression(call(expression(ExpressionKind::Identifier(format!(
                "!{name}"
            ))))),
            name: "unwrap".to_string(),
        }));
        let run = Statement::new(StatementKind::Expression(*expression(unwrap)), span);
        Some((body, function(format!("#{name}"), None, vec![run])))
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
            Token::Identifier(word)
                if word == "let" && matches!(p.ahead(1), Token::Identifier(_)) =>
            {
                let count = match (p.ahead(1), p.ahead(2)) {
                    (Token::Identifier(m), Token::Identifier(_)) if m == "mut" => 2,
                    _ => 1,
                };
                p.slip(
                    count,
                    "Zeru declares a variable with 'var', or 'const' when it never changes",
                    "write var",
                    "var",
                    vec![Token::Var],
                );
                p.parse_var_statement(false)
            }
            Token::Identifier(name)
                if matches!(p.ahead(1), Token::Colon) && matches!(p.ahead(2), Token::Assign) =>
            {
                let name = name.clone();
                p.slip(
                    3,
                    "Zeru declares a variable with var, not :=",
                    "write var",
                    &format!("var {name} ="),
                    vec![Token::Var, Token::Identifier(name.clone()), Token::Assign],
                );
                p.parse_var_statement(false)
            }
            Token::Identifier(word) if word == "loop" && matches!(p.ahead(1), Token::LBrace) => {
                p.slip(
                    1,
                    "Zeru writes a loop without end as while true",
                    "write while true",
                    "while true",
                    vec![Token::While, Token::True],
                );
                p.parse_while_statement()
            }
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
        let (name, type_annotation) = match self.parse_names(true)? {
            Names::Tuple(names) => {
                self.expect(&Token::Assign)?;
                let value = self.parse_expression(Precedence::Lowest)?;
                self.expect(&Token::Semicolon)?;
                let kind = StatementKind::VarTuple {
                    names,
                    is_const,
                    value,
                };
                return Some(self.statement(kind, start));
            }
            Names::One(name, ty) => (name, ty),
            Names::Skip => ("_".to_string(), None),
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
                Token::Fn => {
                    p.bump();
                    p.expect(&Token::LParen)?;
                    let params = p.parse_list(&Token::RParen, Self::parse_type)?;
                    let returns = match p.at_any(&[
                        Token::Comma,
                        Token::RParen,
                        Token::Gt,
                        Token::Semicolon,
                        Token::Assign,
                        Token::LBrace,
                        Token::Question,
                        Token::Bang,
                    ]) {
                        true => None,
                        false => Some(Box::new(p.parse_type()?)),
                    };
                    TypeSpec::Fn(params, returns)
                }
                other => return p.error_here(format!("Expected a type, found '{other}'")),
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

    /// `&[T]` is a slice and `&var [T]` one that writes, `&var T` a
    /// mutable reference, `&T` a shared one.
    fn parse_reference_type(&mut self) -> Option<TypeSpec> {
        self.bump();
        let mutable = self.eat(&Token::Var);
        if self.eat(&Token::LBracket) {
            let elem_type = Box::new(self.parse_type()?);
            self.expect(&Token::RBracket)?;
            return Some(match mutable {
                true => TypeSpec::SliceMut(elem_type),
                false => TypeSpec::Slice(elem_type),
            });
        }
        let inner = Box::new(self.parse_type()?);
        Some(match mutable {
            true => TypeSpec::RefMut(inner),
            false => TypeSpec::Ref(inner),
        })
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
        let split = vec![
            (first, Span::new(start, start + 1)),
            (rest, Span::new(start + 1, end)),
        ];
        self.rewrite(self.pos, 1, split);
    }

    /// Replace the `count` tokens from `at` by `with`, until a rewind.
    fn rewrite(&mut self, at: usize, count: usize, with: Vec<(Token, Span)>) {
        let inserted = with.len();
        let removed = self.tokens.splice(at..at + count, with).collect();
        self.rewrites.push((at, removed, inserted));
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
        let name = self.identifier("a name")?;
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
        if self.at(&Token::Minus) && matches!(self.ahead(1), Token::Gt) {
            let arrow = self.span();
            let next = self
                .tokens
                .get(self.pos + 2)
                .map_or(arrow.end, |(_, span)| span.start);
            self.slip(
                2,
                "Zeru writes the return type right after the parameters, with no ->",
                "drop the ->",
                "",
                Vec::new(),
            );
            if let Some(error) = self
                .errors
                .last_mut()
                .filter(|e| e.span.start == arrow.start)
            {
                error.fix(Span::new(arrow.start, next), "");
            }
        }
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
        self.else_if_slip();
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

    /// Read `elif`, as another language writes `else if`.
    fn else_if_slip(&mut self) {
        if matches!(self.peek(), Token::Identifier(word) if ["elif", "elsif", "elseif"].contains(&word.as_str()))
        {
            self.slip(
                1,
                "Zeru writes 'else if'",
                "write else if",
                "else if",
                vec![Token::Else, Token::If],
            );
        }
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
        if self.at(&Token::LParen) {
            let closing = self.tokens[self.pos..]
                .iter()
                .position(|(t, _)| *t == Token::RParen);
            let counted = closing.is_some_and(|end| {
                self.tokens[self.pos..self.pos + end]
                    .iter()
                    .any(|(t, _)| *t == Token::Semicolon)
            });
            if let (true, Some(end)) = (counted, closing) {
                if let Some(error) =
                    self.error("Zeru's for walks a range or a sequence", self.span())
                {
                    error.help("count with a range, as in for i in 0..n, or loop with while");
                }
                for _ in 0..=end {
                    self.bump();
                }
                self.panic_mode = false;
                self.parse_block()?;
                return Some(self.statement(StatementKind::Block(Vec::new()), start));
            }
        }
        let mut variable = self.parse_names(false)?;
        let mut index = None;
        if let Names::One(name, None) = &variable
            && self.eat(&Token::Comma)
        {
            index = Some(name.clone());
            variable = self.parse_names(false)?;
        }
        self.expect(&Token::In)?;

        let mut iterable = self.parse_condition()?;
        if self.at_any(&[Token::DotDot, Token::DotDotEq]) {
            let inclusive = self.bump() == Token::DotDotEq;
            let end = self.parse_condition()?;
            let span = iterable.span.merge(end.span);
            let kind = ExpressionKind::Range {
                start: Box::new(iterable),
                end: Box::new(end),
                inclusive,
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

    /// A name, `_`, or a parenthesized list of them, each of a `var`'s
    /// with its type when `typed`. Parentheses around one name only group
    /// it; `(a,)` is a tuple of one.
    fn parse_names(&mut self, typed: bool) -> Option<Names> {
        if !self.eat(&Token::LParen) {
            let name = self.identifier("a name")?;
            if name == "_" {
                return Some(Names::Skip);
            }
            let ty = match typed && self.eat(&Token::Colon) {
                true => Some(self.parse_annotation()?),
                false => None,
            };
            return Some(Names::One(name, ty));
        }
        let mut names = vec![self.parse_names(typed)?];
        let mut comma = false;
        while self.eat(&Token::Comma) {
            comma = true;
            if self.at(&Token::RParen) {
                break;
            }
            names.push(self.parse_names(typed)?);
        }
        self.expect(&Token::RParen)?;
        if names.len() == 1 && !comma {
            return names.pop();
        }
        Some(Names::Tuple(names))
    }

    fn parse_struct_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.identifier("a name")?;
        let type_params = self.parse_type_parameters()?;
        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut seen_method = false;

        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            if self.at(&Token::Pub)
                && matches!(self.ahead(1), Token::Identifier(_))
                && *self.ahead(2) == Token::Colon
            {
                let span = self.span();
                let next = self.tokens[self.pos + 1].1;
                if let Some(error) = self.error(
                    "A field takes no pub: every field is visible wherever its struct is",
                    span,
                ) {
                    error
                        .help("remove pub")
                        .fix(Span::new(span.start, next.start), "");
                }
                self.panic_mode = false;
                self.bump();
            }
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

    fn parse_enum_statement(&mut self) -> Option<Statement> {
        let start = self.span();
        self.bump();
        let name = self.identifier("a name")?;
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
        let name = self.identifier("a name")?;
        self.expect(&Token::LBrace)?;

        let mut methods = Vec::new();
        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            self.expect(&Token::Fn)?;
            let name = self.identifier("a name")?;
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
        let saved = self.checkpoint();
        if let Some(type_args) = self.parse_generic_arguments()
            && !type_args
                .iter()
                .any(|arg| matches!(arg, TypeSpec::IntLiteral(_)))
            && self.eat(&Token::LParen)
        {
            let arguments = self.parse_arguments()?;
            let start = function.span;
            let kind = ExpressionKind::Call {
                function: Box::new(function),
                type_args,
                arguments,
            };
            return Some(self.expression(kind, start));
        }
        self.rewind(saved);
        Some(function)
    }

    /// Where parsing is, to try one reading and [`Self::rewind`] if it fails.
    fn checkpoint(&self) -> [usize; 6] {
        [
            self.pos,
            self.errors.len(),
            usize::from(self.panic_mode),
            self.depth,
            self.brackets,
            self.rewrites.len(),
        ]
    }

    fn rewind(&mut self, [pos, errors, panic_mode, depth, brackets, rewrites]: [usize; 6]) {
        for (at, removed, inserted) in self.rewrites.drain(rewrites..).rev() {
            self.tokens.splice(at..at + inserted, removed);
        }
        (self.pos, self.panic_mode, self.depth, self.brackets) =
            (pos, panic_mode == 1, depth, brackets);
        self.opens.truncate(depth);
        self.errors.truncate(errors);
    }

    /// Read the condition of a construct whose body follows in braces.
    fn parse_condition(&mut self) -> Option<Expression> {
        self.struct_literals(false, |p| p.parse_expression(Precedence::Lowest))
    }

    /// Read `parse` with struct literals allowed or not; they are allowed
    /// again in a context already delimited by parentheses, brackets or
    /// commas.
    fn struct_literals<T>(&mut self, allowed: bool, parse: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::replace(&mut self.no_struct_literal, !allowed);
        let parsed = parse(self);
        self.no_struct_literal = saved;
        parsed
    }

    fn parse_struct_literal(&mut self, name: String, start: Span) -> Option<Expression> {
        self.bump();
        let fields = self.struct_literals(true, |p| {
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
        let here = self.span();
        if self.code_seen
            && let Some(error) = self.error("Imports must come before any other code", here)
        {
            error.help("move this import to the top of the file");
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
            path.push(self.identifier("a name")?);
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
        let open = self.prev_span();
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
            let at = self.span();
            if let Some(error) = self.error("Expected '}', found the end of the file", at) {
                error
                    .note("this '{' is never closed", open)
                    .help("close it with }");
            }
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
        loop {
            self.operator_slips();
            if precedence >= token_precedence(self.peek()) {
                break;
            }
            let comparison = matches!(
                token_precedence(self.peek()),
                Precedence::Equals | Precedence::LessGreater
            );
            if comparison && (compared || precedence == Precedence::Equals) {
                let error = self.error("Comparison operators cannot be chained", self.span())?;
                error.help("compare twice and join them, as in a < b && b < c");
                return None;
            }
            compared = comparison;
            left = self.parse_infix_expression(left)?;
        }
        Some(left)
    }

    /// Read an operator another language writes as Zeru does: `and`, `or`,
    /// `===`, `!==`, and the `++` and `--` that end a statement.
    fn operator_slips(&mut self) {
        let ends = |token: &Token| {
            matches!(
                token,
                Token::Semicolon | Token::RParen | Token::RBrace | Token::Comma
            )
        };
        match (self.peek().clone(), self.ahead(1).clone()) {
            (Token::Identifier(word), next)
                if (word == "and" || word == "or") && starts_operand(&next) =>
            {
                let (written, with) = match word.as_str() {
                    "and" => ("&&", Token::And),
                    _ => ("||", Token::Or),
                };
                let message = format!("Zeru writes '{word}' as {written}");
                self.slip(
                    1,
                    &message,
                    &format!("write {written}"),
                    written,
                    vec![with],
                );
            }
            (Token::Eq | Token::NotEq, Token::Assign) => {
                let (written, with) = match self.peek() {
                    Token::Eq => ("==", Token::Eq),
                    _ => ("!=", Token::NotEq),
                };
                let message = format!("Zeru compares with {written}, never {written}=");
                self.slip(
                    2,
                    &message,
                    &format!("write {written}"),
                    written,
                    vec![with],
                );
            }
            (Token::Minus, Token::Gt) if self.in_pattern => {
                self.slip(
                    2,
                    "A match arm is written pattern => value",
                    "write =>",
                    "=>",
                    vec![Token::Arrow],
                );
            }
            (Token::Minus, Token::Gt) if matches!(self.ahead(2), Token::Identifier(_)) => {
                self.slip(
                    2,
                    "Zeru reaches a field through a pointer with '.', not ->",
                    "write .",
                    ".",
                    vec![Token::Dot],
                );
            }
            (Token::Plus, Token::Plus) | (Token::Minus, Token::Minus) if ends(self.ahead(2)) => {
                let (written, with) = match self.peek() {
                    Token::Plus => ("+=", Token::PlusEq),
                    _ => ("-=", Token::MinusEq),
                };
                let doubled = &written[..1].repeat(2);
                let message = format!("Zeru has no {doubled}; write {written} 1");
                let fix = format!(" {written} 1");
                self.slip(
                    2,
                    &message,
                    &format!("write {written} 1"),
                    &fix,
                    vec![with, Token::Int(1)],
                );
            }
            _ => {}
        }
    }

    fn parse_operand(&mut self) -> Option<Expression> {
        if matches!(self.peek(), Token::Identifier(word) if word == "not")
            && starts_operand(self.ahead(1))
        {
            let (word, next) = (self.span(), self.tokens[self.pos + 1].1);
            self.slip(
                1,
                "Zeru writes 'not' as !",
                "write !",
                "!",
                vec![Token::Bang],
            );
            if let Some(error) = self.errors.last_mut().filter(|e| e.span == word) {
                error.fix(Span::new(word.start, next.start), "!");
            }
        }
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
                Token::Identifier(name)
                    if name == "Vec"
                        && matches!(self.tokens.get(self.pos + 1), Some((Token::LBracket, _))) =>
                {
                    self.parse_vec_literal()
                }
                Token::Identifier(_) => self.parse_path_or_struct_literal(),
                Token::LParen => self.parse_grouped_expression(),
                Token::Minus | Token::Bang => {
                    self.parse_prefix(|operator, right| ExpressionKind::Prefix { operator, right })
                }
                Token::Star => self.parse_prefix(|_, expr| ExpressionKind::Dereference(expr)),
                Token::BitAnd => self.parse_borrow_expression(),
                Token::Match => self.parse_match_expression(),
                Token::If => self.parse_if_expression(),
                Token::Fn => self.parse_closure(),
                Token::Asm => self.parse_asm_expression(),
                token => self.error_here(format!("Expected an expression, found '{token}'")),
            },
        }
    }

    /// `(a)` is `a`; `()`, `(a,)` and `(a, b)` are tuples.
    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        self.struct_literals(true, |p| {
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

    /// `if c { a } else { b }` giving a value: one expression in each
    /// branch, and an `else`, which may be another such `if`.
    fn parse_if_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let condition = Box::new(self.parse_condition()?);
        let then = Box::new(self.parse_branch()?);
        self.else_if_slip();
        if !self.eat(&Token::Else) {
            return self.error_here("An if that gives a value needs an else");
        }
        let otherwise = match self.at(&Token::If) {
            true => self.parse_if_expression()?,
            false => self.parse_branch()?,
        };
        let kind = ExpressionKind::If {
            condition,
            then,
            otherwise: Box::new(otherwise),
        };
        Some(self.expression(kind, start))
    }

    /// One branch of an `if` giving a value: `{ expression }`, or a block
    /// of statements, which the checker requires to leave.
    fn parse_branch(&mut self) -> Option<Expression> {
        let start = self.span();
        let saved = self.checkpoint();
        self.expect(&Token::LBrace)?;
        if let Some(value) = self.parse_expression(Precedence::Lowest)
            && self.eat(&Token::RBrace)
        {
            return Some(value);
        }
        self.rewind(saved);
        let body = self.parse_block_statement()?;
        Some(self.expression(ExpressionKind::Block(body), start))
    }

    fn parse_match_expression(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        let value = self.parse_condition()?;
        self.expect(&Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.at_any(&[Token::RBrace, Token::Eof]) {
            if matches!(self.peek(), Token::Identifier(word) if word == "_")
                && *self.ahead(1) == Token::Arrow
            {
                self.slip(
                    1,
                    "Zeru writes the arm for every other value as default",
                    "write default",
                    "default",
                    vec![Token::Default],
                );
            }
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
            let guard = match self.eat(&Token::If) {
                true => Some(self.parse_expression(Precedence::Lowest)?),
                false => None,
            };
            if self.at(&Token::Minus) && matches!(self.ahead(1), Token::Gt) {
                self.slip(
                    2,
                    "A match arm is written pattern => value",
                    "write =>",
                    "=>",
                    vec![Token::Arrow],
                );
            }
            self.expect(&Token::Arrow)?;

            let body = match self.at(&Token::LBrace) {
                true => {
                    let span = self.span();
                    self.arm_blocks.push(self.depth + 1);
                    let body = self.parse_block_statement();
                    self.arm_blocks.pop();
                    self.expression(ExpressionKind::Block(body?), span)
                }
                false => self.parse_expression(Precedence::Lowest)?,
            };
            let block = matches!(body.kind, ExpressionKind::Block(_));
            arms.push(MatchArm {
                pattern,
                guard,
                body,
            });
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

    /// A value, or the values from `low` to `high` written `low..=high`.
    fn parse_constant_pattern(&mut self) -> Option<PatternKind> {
        let low = self.parse_expression(Precedence::BitwiseOr)?;
        if self.at(&Token::DotDot) {
            return self.error_here("A range in a pattern takes its end in: write 'low..=high'");
        }
        if !self.eat(&Token::DotDotEq) {
            return Some(PatternKind::Constant(low));
        }
        let high = self.parse_expression(Precedence::BitwiseOr)?;
        Some(PatternKind::Range(low, high))
    }

    /// Alternatives separated by `|`. A name alone is a binding inside a
    /// payload and a constant or variant elsewhere.
    fn parse_pattern(&mut self, in_payload: bool) -> Option<Pattern> {
        let outer = std::mem::replace(&mut self.in_pattern, true);
        let pattern = self.parse_alternatives(in_payload);
        self.in_pattern = outer;
        pattern
    }

    fn parse_alternatives(&mut self, in_payload: bool) -> Option<Pattern> {
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
                        self.parse_constant_pattern()?
                    }
                }
            }
            _ => self.parse_constant_pattern()?,
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
                        .struct_literals(true, |p| p.parse_expression(Precedence::Lowest))
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
                let arguments = self.parse_arguments()?;
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
        let kind = self.struct_literals(true, Self::parse_array_elements)?;
        Some(self.expression(kind, start))
    }

    /// `Vec[a, b]` or `Vec[x; n]`: a Vec with these elements, or `n` of `x`.
    fn parse_vec_literal(&mut self) -> Option<Expression> {
        let start = self.span();
        self.bump();
        self.bump();
        let kind = match self.struct_literals(true, Self::parse_array_elements)? {
            ExpressionKind::ArrayRepeat { value, count } => {
                ExpressionKind::VecRepeat { value, count }
            }
            ExpressionKind::ArrayLiteral(items) => ExpressionKind::VecLiteral(items),
            _ => unreachable!("an array's elements"),
        };
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

    /// A call's arguments, up to its `)`. A closure that is a whole one may
    /// have a body of one expression.
    fn parse_arguments(&mut self) -> Option<Vec<Expression>> {
        self.struct_literals(true, |p| {
            p.parse_list(&Token::RParen, |p| {
                p.argument_at = p.pos;
                p.parse_expression(Precedence::Lowest)
            })
        })
    }

    /// `fn(x: i32) i32 { body }`, or with a body of one expression,
    /// `fn(x) x * 2`. After the parameters, a type followed by `{` is the
    /// return type of a block body.
    fn parse_closure(&mut self) -> Option<Expression> {
        let start = self.span();
        let argument = self.argument_at == self.pos;
        self.bump();
        self.expect(&Token::LParen)?;
        let params = self.parse_list(&Token::RParen, |p| {
            let mutable = p.eat(&Token::Var);
            let name = p.identifier("a parameter name")?;
            let ty = match p.eat(&Token::Colon) {
                true => Some(p.parse_type()?),
                false => None,
            };
            Some((name, ty, mutable))
        })?;
        let saved = self.checkpoint();
        let return_type = match self.at(&Token::LBrace) {
            true => None,
            false => self.parse_type().filter(|_| self.at(&Token::LBrace)),
        };
        if return_type.is_none() {
            self.rewind(saved);
        }
        let literal = return_type.is_some()
            && matches!(self.ahead(1), Token::Identifier(_))
            && *self.ahead(2) == Token::Colon;
        let body = match self.at(&Token::LBrace) && !literal {
            true => ClosureBody::Block(self.parse_block_statement()?),
            false if literal => {
                self.rewind(saved);
                let body = self.parse_expression(Precedence::Lowest)?;
                let name = match &return_type {
                    Some(TypeSpec::Named(name)) => name.clone(),
                    _ => "S".to_string(),
                };
                if let Some(error) = self.error(
                    "A closure body that is one struct literal goes in parentheses, or it reads as a return type and a block",
                    body.span,
                ) {
                    error.help(format!("write it as fn(..) ({name} {{ .. }})"));
                }
                ClosureBody::Expression(Box::new(body))
            }
            false => {
                self.rewind(saved);
                ClosureBody::Expression(Box::new(self.parse_expression(Precedence::Lowest)?))
            }
        };
        let closure = Closure {
            params,
            return_type,
            body,
            argument,
        };
        Some(self.expression(ExpressionKind::Closure(Box::new(closure)), start))
    }

    fn parse_expressions(&mut self, close: &Token) -> Option<Vec<Expression>> {
        self.parse_list(close, |p| p.parse_expression(Precedence::Lowest))
    }
}

/// Loosest first. Bitwise operators bind tighter than comparison, so
/// `flags & MASK == 0` groups the way it reads.
#[derive(PartialEq, PartialOrd)]
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
        Token::Assign => Precedence::Assignment,
        token if token.compound_base().is_some() => Precedence::Assignment,

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

/// Whether `token` can start an operand.
fn starts_operand(token: &Token) -> bool {
    matches!(
        token,
        Token::Identifier(_)
            | Token::Int(_)
            | Token::Float(_)
            | Token::StringLit(_)
            | Token::True
            | Token::False
            | Token::None
            | Token::SelfTok
            | Token::LParen
            | Token::Bang
            | Token::Minus
    )
}
