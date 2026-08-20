use crate::{
    ast::{
        AsmOperand, Expression, ExpressionKind, Program, Statement, StatementKind, TypeParameter,
        TypeSpec,
    },
    errors::{Span, ZeruError},
    lexer::Lexer,
    token::Token,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Token,
    current_span: Span,
    current_line: usize,
    peek_token: Token,
    peek_span: Span,
    peek_line: usize,

    pub errors: Vec<ZeruError>,
    panic_mode: bool,

    /// Set while reading the condition of `if`/`while` or the subject of
    /// `match`, where a `{` opens the body rather than a struct literal. It is
    /// cleared inside parentheses, brackets and argument lists, where there is
    /// nothing to confuse.
    no_struct_literal: bool,

    /// A `>` owed from splitting a `>>` that closed two generic levels at once.
    /// It is handed out as the next token instead of reading the lexer.
    pending_gt: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            current_token: Token::Eof,
            current_span: Span::default(),
            current_line: 1,
            peek_token: Token::Eof,
            peek_span: Span::default(),
            peek_line: 0,
            errors: Vec::new(),
            panic_mode: false,
            no_struct_literal: false,
            pending_gt: false,
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.current_span = self.peek_span;
        self.current_line = self.peek_line;

        if self.pending_gt {
            self.pending_gt = false;
            self.peek_token = Token::Gt;
            self.peek_span = Span::new(self.current_span.end, self.current_span.end + 1);
            return;
        }

        let (tok, line, span) = self.lexer.next_token();

        if let Token::Illegal(ref msg) = tok {
            self.errors.push(ZeruError::syntax(msg, span, line));
            self.panic_mode = true;
        }

        self.peek_token = tok;
        self.peek_line = line;
        self.peek_span = span;
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current_token != Token::Eof {
            if self.current_token == Token::Semicolon {
                self.next_token();
                return;
            }

            match self.peek_token {
                Token::Var
                | Token::Fn
                | Token::Const
                | Token::Struct
                | Token::Enum
                | Token::If
                | Token::While
                | Token::For
                | Token::Return
                | Token::RBrace => return,
                _ => {}
            }

            self.next_token();
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.current_token != Token::Eof {
            self.panic_mode = false;
            let stmt = match self.current_token {
                Token::Const => self.parse_var_statement::<true>(),
                Token::Fn => self.parse_function_statement(),
                Token::Struct => self.parse_struct_statement(),
                Token::Enum => self.parse_enum_statement(),
                Token::Trait => self.parse_trait_statement(),
                Token::Import => self.parse_import_statement(),
                Token::Semicolon => {
                    self.next_token();
                    None
                }
                Token::Var => {
                    self.error_current("Global variables ('var') are not allowed. Use 'const' for constants or move 'var' inside a function.");
                    self.synchronize();
                    None
                }
                _ => {
                    self.error_current(format!("Unexpected token {:?} at top level. Expected fn, struct, enum, const or import.", self.current_token).as_str());
                    self.synchronize();
                    None
                }
            };

            if let Some(statement) = stmt {
                statements.push(statement);
            } else if self.panic_mode {
                self.synchronize();
            }

            if self.peek_token_is(&Token::Eof) {
                if self.current_token != Token::Eof {
                    self.next_token();
                }
                break;
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.panic_mode = false;

        match self.current_token {
            Token::Var => self.parse_var_statement::<false>(),
            Token::Const => self.parse_var_statement::<true>(),
            Token::Return => self.parse_return_statement(),
            Token::Fn => self.parse_function_statement(),
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            Token::Struct => self.parse_struct_statement(),
            Token::Enum => self.parse_enum_statement(),
            Token::Break => self.parse_break_statement(),
            Token::Continue => self.parse_continue_statement(),
            Token::LBrace => self.parse_block_statement_wrapper(),
            Token::Import => self.parse_import_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let end_span = self.current_span;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::new(
            StatementKind::Expression(expr),
            start_span.merge(end_span),
        ))
    }

    fn parse_var_statement<const CONSTANT: bool>(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let name = self.expect_identifier()?;

        let mut type_annotation = None;
        if self.peek_token_is(&Token::Colon) {
            self.next_token();
            self.next_token();

            type_annotation = self.parse_type();
            type_annotation.as_ref()?;
        }

        if !self.expect_peek(&Token::Assign) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::Semicolon) {
            return None;
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Var {
                name,
                is_const: CONSTANT,
                value,
                type_annotation,
            },
            start_span.merge(end_span),
        ))
    }

    /// A type: a pointer, reference, slice, tuple, or a name with optional
    /// generic arguments and an optional `?` or `!` suffix.
    fn parse_type(&mut self) -> Option<TypeSpec> {
        match &self.current_token {
            // `str` is a slice of bytes.
            Token::Str => Some(TypeSpec::Slice(Box::new(TypeSpec::Named("u8".to_string())))),
            Token::Star => {
                self.next_token();
                Some(TypeSpec::Pointer(Box::new(self.parse_type()?)))
            }
            Token::BitAnd => self.parse_reference_type(),
            Token::LParen => self.parse_tuple_type(),
            Token::Identifier(_) => self.parse_named_type(),
            _ => {
                self.error_current("Type identifier expected");
                None
            }
        }
    }

    /// `&[T]` is a slice, `&var T` a mutable reference, `&T` a shared one.
    fn parse_reference_type(&mut self) -> Option<TypeSpec> {
        self.next_token();

        if self.current_token == Token::LBracket {
            self.next_token();
            let elem_type = self.parse_type()?;
            if !self.expect_peek(&Token::RBracket) {
                return None;
            }
            return Some(TypeSpec::Slice(Box::new(elem_type)));
        }

        if self.current_token == Token::Var {
            self.next_token();
            return Some(TypeSpec::RefMut(Box::new(self.parse_type()?)));
        }

        Some(TypeSpec::Ref(Box::new(self.parse_type()?)))
    }

    fn parse_tuple_type(&mut self) -> Option<TypeSpec> {
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return Some(TypeSpec::Tuple(Vec::new()));
        }

        self.next_token();
        let mut types = vec![self.parse_type()?];

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            if self.peek_token_is(&Token::RParen) {
                break;
            }
            self.next_token();
            types.push(self.parse_type()?);
        }

        if !self.expect_peek(&Token::RParen) {
            return None;
        }
        Some(TypeSpec::Tuple(types))
    }

    /// A possibly qualified name, then generic arguments or a `?`/`!` suffix.
    fn parse_named_type(&mut self) -> Option<TypeSpec> {
        let Token::Identifier(first) = &self.current_token else {
            self.error_current("Type identifier expected");
            return None;
        };
        let mut name = first.clone();

        while self.peek_token_is(&Token::DoubleColon) {
            self.next_token();
            self.next_token();
            let Token::Identifier(segment) = &self.current_token else {
                self.error_current("Expected type name after '::'");
                return None;
            };
            name.push_str("::");
            name.push_str(segment);
        }

        if self.peek_token_is(&Token::Lt) {
            let args = self.parse_generic_arguments()?;
            return Some(TypeSpec::Generic { name, args });
        }

        let named = TypeSpec::Named(name);
        if self.peek_token_is(&Token::Question) {
            self.next_token();
            return Some(TypeSpec::Optional(Box::new(named)));
        }
        if self.peek_token_is(&Token::Bang) {
            self.next_token();
            return Some(TypeSpec::Result(Box::new(named)));
        }
        Some(named)
    }

    /// The `<..>` of a generic type. An argument is either a type or a length,
    /// as in `Array<i32, 4>`.
    fn parse_generic_arguments(&mut self) -> Option<Vec<TypeSpec>> {
        self.next_token();
        self.next_token();
        let mut args = Vec::new();

        while self.current_token != Token::Gt {
            args.push(match self.current_token {
                Token::Int(length) => TypeSpec::IntLiteral(length),
                _ => self.parse_type()?,
            });

            // `Vec<Vec<i32>>` ends in a single `>>` that has to close two
            // levels: the first half is taken here, the second is left for the
            // level above.
            if self.peek_token_is(&Token::ShiftRight) {
                self.peek_token = Token::Gt;
                self.pending_gt = true;
            }

            if self.peek_token_is(&Token::Comma) {
                self.next_token();
                self.next_token();
            } else if self.peek_token_is(&Token::Gt) {
                self.next_token();
                break;
            } else {
                self.error_peek("Expected ',' or '>' in generic type");
                return None;
            }
        }

        Some(args)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        self.next_token();

        let return_value = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            self.parse_expression(Precedence::Lowest)
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Return(return_value),
            start_span.merge(end_span),
        ))
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let name = self.expect_identifier()?;

        let type_params = if self.peek_token_is(&Token::Lt) {
            self.next_token();
            self.parse_type_parameters()
        } else {
            Vec::new()
        };

        if !self.expect_peek(&Token::LParen) {
            return None;
        }

        let params = self.parse_function_parameters();
        let mut return_type = None;

        if !self.peek_token_is(&Token::LBrace) {
            self.next_token();
            return_type = self.parse_type();
            return_type.as_ref()?;
        }

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let body = self.parse_block_statement();
        let end_span = self.current_span;

        Some(Statement::new(
            StatementKind::Function {
                name,
                type_params,
                params,
                return_type,
                body,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_type_parameters(&mut self) -> Vec<TypeParameter> {
        let mut params = Vec::new();

        loop {
            self.next_token();

            if self.current_token == Token::Gt {
                break;
            }

            let name = match &self.current_token {
                Token::Identifier(n) => n.clone(),
                _ => {
                    self.error_current("Expected type parameter name");
                    break;
                }
            };

            let bound = if self.peek_token_is(&Token::Colon) {
                self.next_token();
                self.next_token();
                match &self.current_token {
                    Token::Identifier(b) => Some(b.clone()),
                    _ => {
                        self.error_current("Expected trait bound name");
                        None
                    }
                }
            } else {
                None
            };

            params.push(TypeParameter { name, bound });

            if self.peek_token_is(&Token::Comma) {
                self.next_token();
            } else if self.peek_token_is(&Token::Gt) {
                self.next_token();
                break;
            } else {
                self.error_peek("Expected ',' or '>' in type parameters");
                break;
            }
        }

        params
    }

    fn parse_function_parameters(&mut self) -> Vec<(String, TypeSpec, bool)> {
        let mut params = Vec::new();
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return params;
        }

        self.next_token();
        params.push(self.parse_parameter());

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            if self.peek_token_is(&Token::RParen) {
                break;
            }
            self.next_token();
            params.push(self.parse_parameter());
        }

        if !self.expect_peek(&Token::RParen) {
            return Vec::new();
        }
        params
    }

    fn parse_parameter(&mut self) -> (String, TypeSpec, bool) {
        let is_mut = if self.cur_token_is(&Token::Var) {
            self.next_token();
            true
        } else {
            false
        };

        if self.cur_token_is(&Token::SelfTok) {
            return (
                "self".to_string(),
                TypeSpec::Named("self".to_string()),
                is_mut,
            );
        }

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => {
                self.error_current("Expected parameter name");
                String::new()
            }
        };

        if !self.expect_peek(&Token::Colon) {
            return (name, TypeSpec::Named("Unknown".to_string()), is_mut);
        }
        self.next_token();

        let type_spec = self
            .parse_type()
            .unwrap_or(TypeSpec::Named("Unknown".to_string()));
        (name, type_spec, is_mut)
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        self.next_token();

        let condition = self.parse_condition()?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let block_start = self.current_span;
        let block_stmts = self.parse_block_statement();
        let block_end = self.current_span;
        let then_branch = Box::new(Statement::new(
            StatementKind::Block(block_stmts),
            block_start.merge(block_end),
        ));

        let else_branch = if self.peek_token_is(&Token::Else) {
            self.next_token();
            if self.peek_token_is(&Token::If) {
                self.next_token();
                Some(Box::new(self.parse_if_statement()?))
            } else {
                if !self.expect_peek(&Token::LBrace) {
                    return None;
                }
                let else_block_start = self.current_span;
                let else_block_stmts = self.parse_block_statement();
                let else_block_end = self.current_span;
                Some(Box::new(Statement::new(
                    StatementKind::Block(else_block_stmts),
                    else_block_start.merge(else_block_end),
                )))
            }
        } else {
            None
        };

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        self.next_token();

        let cond = self.parse_condition()?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let block_start = self.current_span;
        let block_stmts = self.parse_block_statement();
        let block_end = self.current_span;
        let body = Box::new(Statement::new(
            StatementKind::Block(block_stmts),
            block_start.merge(block_end),
        ));

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::While { cond, body },
            start_span.merge(end_span),
        ))
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let variable = self.expect_identifier()?;

        if !self.expect_peek(&Token::In) {
            return None;
        }
        self.next_token();

        let iterable = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let block_start = self.current_span;
        let block_stmts = self.parse_block_statement();
        let block_end = self.current_span;
        let body = Box::new(Statement::new(
            StatementKind::Block(block_stmts),
            block_start.merge(block_end),
        ));

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::ForIn {
                variable,
                iterable,
                body,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_struct_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let name = self.expect_identifier()?;

        let type_params = if self.peek_token_is(&Token::Lt) {
            self.next_token();
            self.parse_type_parameters()
        } else {
            Vec::new()
        };

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut seen_method = false;

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            if self.peek_token_is(&Token::Fn) {
                seen_method = true;
                self.next_token();
                if let Some(method) = self.parse_function_statement() {
                    methods.push(method);
                }
                continue;
            }

            if seen_method {
                self.error_current(&format!(
                    "Struct '{name}': Fields must be declared before methods."
                ));
                return None;
            }

            // One malformed field should not swallow the ones after it.
            match self.parse_struct_field() {
                Some(field) => fields.push(field),
                None => {
                    self.skip_to_next_field();
                    continue;
                }
            }

            if self.peek_token_is(&Token::RBrace) {
                break;
            }
            if self.peek_token_is(&Token::Comma) {
                self.next_token();
            } else {
                self.error_peek("Comma");
                self.panic_mode = false;
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }
        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Struct {
                name,
                type_params,
                fields,
                methods,
            },
            start_span.merge(end_span),
        ))
    }

    /// One `name: Type` field of a struct.
    fn parse_struct_field(&mut self) -> Option<(String, TypeSpec)> {
        self.next_token();

        let Token::Identifier(name) = &self.current_token else {
            self.error_current("Expected field name");
            return None;
        };
        let name = name.clone();

        if !self.expect_peek(&Token::Colon) {
            return None;
        }
        self.next_token();

        Some((name, self.parse_type()?))
    }

    /// Run to the start of the next field, so parsing continues past a bad one.
    fn skip_to_next_field(&mut self) {
        while !self.peek_token_is(&Token::Comma)
            && !self.peek_token_is(&Token::RBrace)
            && !self.peek_token_is(&Token::Eof)
        {
            self.next_token();
        }
        if self.peek_token_is(&Token::Comma) {
            self.next_token();
        }
    }

    fn parse_enum_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let name = self.expect_identifier()?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut variants = Vec::new();
        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            self.next_token();
            if let Token::Identifier(v) = &self.current_token {
                variants.push(v.clone())
            } else {
                self.error_current("Expected enum variant name");
                return None;
            }

            if self.peek_token_is(&Token::RBrace) {
                break;
            }
            if !self.expect_peek(&Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Enum { name, variants },
            start_span.merge(end_span),
        ))
    }

    fn parse_trait_statement(&mut self) -> Option<Statement> {
        use crate::ast::TraitMethod;
        let start_span = self.current_span;

        let name = self.expect_identifier()?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut methods = Vec::new();

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            if !self.expect_peek(&Token::Fn) {
                self.error_current("Expected 'fn' in trait definition");
                return None;
            }

            let method_name = self.expect_identifier()?;

            if !self.expect_peek(&Token::LParen) {
                return None;
            }

            let params = self.parse_function_parameters();
            let mut return_type = None;

            if !self.peek_token_is(&Token::Semicolon)
                && !self.peek_token_is(&Token::RBrace)
                && !self.peek_token_is(&Token::Fn)
            {
                self.next_token();
                return_type = self.parse_type();
            }

            if self.peek_token_is(&Token::Semicolon) {
                self.next_token();
            }

            methods.push(TraitMethod {
                name: method_name,
                params,
                return_type,
            });
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Trait { name, methods },
            start_span.merge(end_span),
        ))
    }

    /// A name, a qualified path like `module::Name`, or a struct literal when a
    /// path naming a type is followed by a brace.
    fn parse_path_or_struct_literal(&mut self, start_span: Span) -> Option<Expression> {
        let Token::Identifier(first) = &self.current_token else {
            return None;
        };
        let mut path = first.clone();

        // A qualified path travels as one name, which is how `Enum::Variant` and
        // `module::item` reach the analyser.
        while self.peek_token_is(&Token::DoubleColon) {
            self.next_token();
            self.next_token();
            let Token::Identifier(segment) = &self.current_token else {
                return None;
            };
            path = format!("{path}::{segment}");
        }

        let names_a_type = path
            .rsplit("::")
            .next()
            .and_then(|last| last.chars().next())
            .is_some_and(char::is_uppercase);

        if names_a_type && !self.no_struct_literal && self.peek_token_is(&Token::LBrace) {
            return self.parse_struct_literal(path, start_span);
        }

        Some(Expression::new(
            ExpressionKind::Identifier(path),
            start_span.merge(self.current_span),
        ))
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

    fn parse_struct_literal(&mut self, name: String, start_span: Span) -> Option<Expression> {
        self.next_token();
        let saved = std::mem::replace(&mut self.no_struct_literal, false);
        let literal = self.parse_struct_fields(name, start_span);
        self.no_struct_literal = saved;
        literal
    }

    fn parse_struct_fields(&mut self, name: String, start_span: Span) -> Option<Expression> {
        let mut fields = Vec::new();

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            self.next_token();
            let field_name = match &self.current_token {
                Token::Identifier(n) => n.clone(),
                _ => return None,
            };

            if !self.expect_peek(&Token::Colon) {
                return None;
            }
            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;
            fields.push((field_name, value));

            if self.peek_token_is(&Token::RBrace) {
                break;
            }

            if !self.expect_peek(&Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }

        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::StructLiteral { name, fields },
            start_span.merge(end_span),
        ))
    }

    fn parse_block_statement_wrapper(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        let stmts = self.parse_block_statement();
        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Block(stmts),
            start_span.merge(end_span),
        ))
    }

    fn parse_import_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;

        let path = self.parse_dotted_path()?;
        let symbols = if self.peek_token_is(&Token::DoubleColon) {
            self.next_token();
            Some(self.parse_import_selection()?)
        } else {
            None
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Import { path, symbols },
            start_span.merge(end_span),
        ))
    }

    /// The `a.b.c` of an import.
    fn parse_dotted_path(&mut self) -> Option<Vec<String>> {
        let mut path = vec![self.expect_identifier()?];

        while self.peek_token_is(&Token::Dot) {
            self.next_token();
            path.push(self.expect_identifier()?);
        }
        Some(path)
    }

    /// The `{a, b}` of a selective import.
    fn parse_import_selection(&mut self) -> Option<Vec<String>> {
        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut items = Vec::new();
        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            items.push(self.expect_identifier()?);

            if self.peek_token_is(&Token::RBrace) {
                break;
            }
            if !self.expect_peek(&Token::Comma) {
                return None;
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }
        Some(items)
    }

    fn parse_break_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        self.next_token();

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Break,
            start_span.merge(end_span),
        ))
    }

    fn parse_continue_statement(&mut self) -> Option<Statement> {
        let start_span = self.current_span;
        self.next_token();

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        let end_span = self.current_span;
        Some(Statement::new(
            StatementKind::Continue,
            start_span.merge(end_span),
        ))
    }

    fn parse_block_statement(&mut self) -> Vec<Statement> {
        let mut block = Vec::new();
        self.next_token();

        while !self.cur_token_is(&Token::RBrace) && !self.cur_token_is(&Token::Eof) {
            if let Some(stmt) = self.parse_statement() {
                block.push(stmt);
            }
            self.next_token();
        }

        block
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let start_span = self.current_span;
        let mut left_exp = match &self.current_token {
            Token::LBracket => self.parse_array_literal(),
            Token::Identifier(_) => self.parse_path_or_struct_literal(start_span),
            Token::Int(val) => Some(Expression::new(
                ExpressionKind::Int(*val),
                self.current_span,
            )),
            Token::Float(val) => Some(Expression::new(
                ExpressionKind::Float(*val),
                self.current_span,
            )),
            Token::StringLit(val) => Some(Expression::new(
                ExpressionKind::StringLit(val.clone()),
                self.current_span,
            )),
            Token::None => Some(Expression::new(ExpressionKind::None, self.current_span)),
            Token::True => Some(Expression::new(
                ExpressionKind::Boolean(true),
                self.current_span,
            )),
            Token::False => Some(Expression::new(
                ExpressionKind::Boolean(false),
                self.current_span,
            )),
            Token::LParen => self.parse_grouped_expression(),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            Token::Star => self.parse_dereference_expression(),
            Token::BitAnd => self.parse_borrow_expression(),
            Token::Match => self.parse_match_expression(),
            Token::SelfTok => Some(Expression::new(
                ExpressionKind::Identifier("self".to_string()),
                self.current_span,
            )),
            Token::Asm => self.parse_asm_expression(),
            _ => {
                self.error_current(
                    format!("Expected expression, found: {:?}", self.current_token).as_str(),
                );
                None
            }
        };

        left_exp.as_ref()?;

        while !self.peek_token_is(&Token::Semicolon)
            && precedence < token_precedence(&self.peek_token)
        {
            self.next_token();
            if let Some(left) = left_exp {
                left_exp = self.parse_infix_expression(left);
            } else {
                break;
            }
        }

        left_exp
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        self.next_token();
        let saved = std::mem::replace(&mut self.no_struct_literal, false);
        let grouped = self.parse_grouped_body(start_span);
        self.no_struct_literal = saved;
        grouped
    }

    fn parse_grouped_body(&mut self, start_span: Span) -> Option<Expression> {
        if self.cur_token_is(&Token::RParen) {
            return Some(Expression::new(
                ExpressionKind::Tuple(vec![]),
                start_span.merge(self.current_span),
            ));
        }

        let first = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Comma) {
            let mut elements = vec![first];

            while self.peek_token_is(&Token::Comma) {
                self.next_token();

                if self.peek_token_is(&Token::RParen) {
                    self.next_token();
                    return Some(Expression::new(
                        ExpressionKind::Tuple(elements),
                        start_span.merge(self.current_span),
                    ));
                }

                self.next_token();
                let elem = self.parse_expression(Precedence::Lowest)?;
                elements.push(elem);
            }

            if !self.expect_peek(&Token::RParen) {
                return None;
            }

            return Some(Expression::new(
                ExpressionKind::Tuple(elements),
                start_span.merge(self.current_span),
            ));
        }

        if !self.expect_peek(&Token::RParen) {
            return None;
        }
        Some(first)
    }

    fn parse_match_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        self.next_token();
        let value = self.parse_condition()?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut arms = Vec::new();

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            self.next_token();

            let pattern = if self.cur_token_is(&Token::Default) {
                Expression::new(
                    ExpressionKind::Identifier("default".to_string()),
                    self.current_span,
                )
            } else {
                self.parse_expression(Precedence::Lowest)?
            };

            if !self.expect_peek(&Token::Arrow) {
                return None;
            }
            self.next_token();

            let body = self.parse_expression(Precedence::Lowest)?;
            arms.push((pattern, body));

            if self.peek_token_is(&Token::Comma) {
                self.next_token();
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return None;
        }

        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::Match {
                value: Box::new(value),
                arms,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_asm_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        let is_volatile = if self.peek_token_is(&Token::Volatile) {
            self.next_token();
            true
        } else {
            false
        };

        if !self.expect_peek(&Token::LParen) {
            return None;
        }

        self.next_token();
        let template = match &self.current_token {
            Token::StringLit(s) => String::from_utf8(s.clone()).unwrap(),
            _ => {
                self.error_current("Expected assembly template string");
                return None;
            }
        };

        let mut outputs = Vec::new();
        if self.peek_token_is(&Token::Colon) {
            self.next_token();
            outputs = self.parse_asm_operands()?;
        }

        let mut inputs = Vec::new();
        if self.peek_token_is(&Token::Colon) {
            self.next_token();
            inputs = self.parse_asm_operands()?;
        }

        let mut clobbers = Vec::new();
        if self.peek_token_is(&Token::Colon) {
            self.next_token();
            clobbers = self.parse_asm_clobbers()?;
        }

        if !self.expect_peek(&Token::RParen) {
            return None;
        }

        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::InlineAsm {
                template,
                outputs,
                inputs,
                clobbers,
                is_volatile,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_asm_operands(&mut self) -> Option<Vec<AsmOperand>> {
        let mut operands = Vec::new();

        if self.peek_token_is(&Token::Colon) || self.peek_token_is(&Token::RParen) {
            return Some(operands);
        }

        loop {
            self.next_token();

            let constraint = match &self.current_token {
                Token::StringLit(s) => String::from_utf8(s.clone()).unwrap(),
                _ => {
                    self.error_current("Exprected constraint string in assembly operand");
                    return None;
                }
            };

            if !self.expect_peek(&Token::LParen) {
                return None;
            }
            self.next_token();

            let expr = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(&Token::RParen) {
                return None;
            }
            operands.push(AsmOperand { constraint, expr });

            if self.peek_token_is(&Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        Some(operands)
    }

    fn parse_asm_clobbers(&mut self) -> Option<Vec<String>> {
        let mut clobbers = Vec::new();

        if self.peek_token_is(&Token::RParen) {
            return Some(clobbers);
        }

        loop {
            self.next_token();

            let clobber = match &self.current_token {
                Token::StringLit(s) => String::from_utf8(s.clone()).unwrap(),
                _ => {
                    self.error_current("Expected clobber string");
                    return None;
                }
            };

            clobbers.push(clobber);

            if self.peek_token_is(&Token::Comma) {
                self.next_token();
            } else {
                break;
            }
        }

        Some(clobbers)
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        let operator = self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;
        let end_span = self.current_span;

        Some(Expression::new(
            ExpressionKind::Prefix {
                operator,
                right: Box::new(right),
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_borrow_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        self.next_token();

        let is_mutable = if self.current_token == Token::Var {
            self.next_token();
            true
        } else {
            false
        };

        let expr = self.parse_expression(Precedence::Prefix)?;
        let end_span = self.current_span;

        let kind = if is_mutable {
            ExpressionKind::BorrowRefMut(Box::new(expr))
        } else {
            ExpressionKind::BorrowRef(Box::new(expr))
        };

        Some(Expression::new(kind, start_span.merge(end_span)))
    }

    fn parse_dereference_expression(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        self.next_token();

        let expr = self.parse_expression(Precedence::Prefix)?;
        let end_span = self.current_span;

        Some(Expression::new(
            ExpressionKind::Dereference(Box::new(expr)),
            start_span.merge(end_span),
        ))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let start_span = left.span;
        let operator = self.current_token.clone();

        // These three are postfix rather than infix: they consume their own
        // brackets instead of a right-hand expression.
        match operator {
            Token::LBracket => return self.parse_index_expression(left),
            Token::LParen => return self.parse_call_expression(left),
            Token::Dot => return self.parse_get_expression(left),
            _ => {}
        }

        // Assignment is the one right associative operator: `a = b = 5` reads
        // as `a = (b = 5)`, so its right side takes everything that follows.
        let precedence = if Self::is_assignment(&operator) {
            Precedence::Lowest
        } else {
            token_precedence(&operator)
        };
        self.next_token();
        let right = self.parse_expression(precedence)?;
        let span = start_span.merge(right.span);

        let kind = match operator {
            Token::As => ExpressionKind::Cast {
                left: Box::new(left),
                target: Box::new(right),
            },
            _ if Self::is_assignment(&operator) => ExpressionKind::Assign {
                target: Box::new(left),
                operator,
                value: Box::new(right),
            },
            _ => ExpressionKind::Infix {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            },
        };

        Some(Expression::new(kind, span))
    }

    /// The operators that write to their left-hand side.
    fn is_assignment(token: &Token) -> bool {
        matches!(
            token,
            Token::Assign
                | Token::PlusEq
                | Token::MinusEq
                | Token::StarEq
                | Token::SlashEq
                | Token::ModEq
                | Token::BitAndEq
                | Token::BitOrEq
                | Token::BitXorEq
                | Token::BitLShiftEq
                | Token::BitRShiftEq
        )
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let start_span = self.current_span;
        self.next_token();
        let saved = std::mem::replace(&mut self.no_struct_literal, false);
        let literal = self.parse_array_body(start_span);
        self.no_struct_literal = saved;
        literal
    }

    fn parse_array_body(&mut self, start_span: Span) -> Option<Expression> {
        let mut elements = Vec::new();

        if self.cur_token_is(&Token::RBracket) {
            let end_span = self.current_span;
            self.next_token();
            return Some(Expression::new(
                ExpressionKind::ArrayLiteral(elements),
                start_span.merge(end_span),
            ));
        }

        let first_elem = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
            self.next_token();

            let count = match &self.current_token {
                Token::Int(n) => *n,
                _ => {
                    self.error_current("Array repeat count must be an integer literal");
                    return None;
                }
            };

            if !self.expect_peek(&Token::RBracket) {
                return None;
            }

            for _ in 0..count {
                elements.push(first_elem.clone());
            }
            let end_span = self.current_span;
            return Some(Expression::new(
                ExpressionKind::ArrayLiteral(elements),
                start_span.merge(end_span),
            ));
        }

        let mut elements = Vec::new();
        elements.push(first_elem);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            if self.peek_token_is(&Token::RBracket) {
                break;
            }
            self.next_token();
            elements.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&Token::RBracket) {
            return None;
        }
        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::ArrayLiteral(elements),
            start_span.merge(end_span),
        ))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let start_span = left.span;
        self.next_token();
        let index =
            self.allowing_struct_literals(|parser| parser.parse_expression(Precedence::Lowest))?;

        if !self.expect_peek(&Token::RBracket) {
            return None;
        }
        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::Index {
                left: Box::new(left),
                index: Box::new(index),
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let start_span = function.span;
        self.next_token();
        let arguments = self.allowing_struct_literals(Self::parse_call_arguments);
        let end_span = self.current_span;

        Some(Expression::new(
            ExpressionKind::Call {
                function: Box::new(function),
                arguments,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_get_expression(&mut self, obj: Expression) -> Option<Expression> {
        let start_span = obj.span;
        self.next_token();

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => {
                self.error_peek("Expected Identifier after '.'");
                return None;
            }
        };

        let end_span = self.current_span;
        Some(Expression::new(
            ExpressionKind::Get {
                object: Box::new(obj),
                name,
            },
            start_span.merge(end_span),
        ))
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        // Empty call: ()
        if self.cur_token_is(&Token::RParen) {
            return args;
        }

        if let Some(arg) = self.parse_expression(Precedence::Lowest) {
            args.push(arg);
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            // A trailing comma is allowed, as it already was in a struct literal.
            if self.peek_token_is(&Token::RParen) {
                break;
            }
            self.next_token();
            if let Some(arg) = self.parse_expression(Precedence::Lowest) {
                args.push(arg);
            }
        }

        if !self.expect_peek(&Token::RParen) {
            return Vec::new();
        }

        args
    }

    fn cur_token_is(&self, t: &Token) -> bool {
        std::mem::discriminant(&self.current_token) == std::mem::discriminant(t)
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        std::mem::discriminant(&self.peek_token) == std::mem::discriminant(t)
    }

    fn expect_peek(&mut self, t: &Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.error_peek(format!("{:?}", t).as_str());
            false
        }
    }

    /// Move onto the next token and hand back its name, or report what it was.
    fn expect_identifier(&mut self) -> Option<String> {
        if let Token::Identifier(name) = &self.peek_token {
            let name = name.clone();
            self.next_token();
            return Some(name);
        }
        self.error_peek("Identifier");
        None
    }

    fn error_peek(&mut self, expected: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.errors.push(ZeruError::syntax(
            format!("{expected} expected, found: {:?}", self.peek_token),
            self.current_span,
            self.current_line,
        ));
    }

    fn error_current(&mut self, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.errors.push(ZeruError::syntax(
            msg.to_string(),
            self.current_span,
            self.current_line,
        ));
    }
}

#[derive(PartialEq, PartialOrd)]
/// Loosest first. Bitwise operators bind tighter than comparison, as in Rust,
/// so `flags & MASK == 0` groups the way it reads instead of C's
/// `flags & (MASK == 0)`.
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
    // A prefix operator binds tighter than a cast, as in Rust and C: `*p as u64`
    // reads the pointer and then widens, rather than casting the pointer.
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
        | Token::BitLShiftEq => Precedence::Assignment,

        Token::Or => Precedence::LogicalOr,
        Token::And => Precedence::LogicalAnd,

        Token::BitOr => Precedence::BitwiseOr,
        Token::BitXor => Precedence::BitwiseXor,
        Token::BitAnd => Precedence::BitwiseAnd,

        Token::Eq | Token::NotEq => Precedence::Equals,

        Token::Gt | Token::Lt | Token::Geq | Token::Leq => Precedence::LessGreater,
        Token::ShiftLeft | Token::ShiftRight => Precedence::Shift,

        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Star | Token::Slash | Token::Mod => Precedence::Product,

        Token::As => Precedence::Cast,
        Token::LParen => Precedence::Call,
        Token::Dot | Token::LBracket | Token::DoubleColon => Precedence::Index,
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
        // A struct literal already allowed one, nothing else did, so
        // reformatting a call or an array across lines broke the parse.
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
    fn test_nested_generic_closes_on_a_single_shift_token() {
        // The lexer hands `>>` over as one ShiftRight, so both levels have to
        // close on it. Before, no nested generic type could be written at all.
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
    fn test_splitting_a_shift_leaves_the_operator_alone() {
        let program = parse_input("fn main() { var t = a >> 2; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        assert_eq!(shape(value), "(a ShiftRight 2)");
    }

    #[test]
    fn test_qualified_path_is_one_name() {
        let program = parse_input("fn main() { var t = module::item; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        assert_eq!(shape(value), "module::item");
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
        // `Color::Red` names a variant, so the brace after it is the `if` body.
        // Reading it as a struct literal swallowed the block.
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
        // Inside a call, brackets or another literal there is nothing to confuse,
        // so a struct literal is allowed even in condition position.
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
    fn test_bitwise_binds_tighter_than_comparison() {
        // C groups this as `flags & (mask == 0)`, which is almost never meant.
        let program = parse_input("fn main() { var t = flags & mask == 0; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        assert_eq!(shape(value), "((flags BitAnd mask) Eq 0)");
    }

    #[test]
    fn test_precedence_within_the_bitwise_group() {
        let program = parse_input("fn main() { var t = a | b ^ c & d; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        assert_eq!(shape(value), "(a BitOr (b BitXor (c BitAnd d)))");
    }

    #[test]
    fn test_shift_stays_looser_than_arithmetic() {
        let program = parse_input("fn main() { var t = a << 2 + 1; }");
        let body = get_function_body(&program.statements[0]);
        let StatementKind::Var { value, .. } = &body[0].kind else {
            panic!("Expected a var statement");
        };
        assert_eq!(shape(value), "(a ShiftLeft (2 Plus 1))");
    }

    #[test]
    fn test_declarations() {
        let input = "
        const X: u32 = 10;
        const Y: f32 = 10.0;

        fn main() {
            var x = 5;
            var y: i32 = 5;
            var z = y;
        }
    ";

        let program = parse_input(input);
        assert_eq!(program.statements.len(), 3);

        match &program.statements[0].kind {
            StatementKind::Var { name, is_const, .. } => {
                assert_eq!(name, "X");
                assert!(*is_const);
            }
            _ => panic!("Expected Const X"),
        }

        let body = get_function_body(&program.statements[2]);
        assert_eq!(body.len(), 3);
        match &body[0].kind {
            StatementKind::Var { name, is_const, .. } => {
                assert_eq!(name, "x");
                assert!(!*is_const);
            }
            _ => panic!("Expected local var x"),
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "fn test() { return 5; return 10; return; }";
        let program = parse_input(input);

        let body = get_function_body(&program.statements[0]);
        assert_eq!(body.len(), 3);
        match &body[0].kind {
            StatementKind::Return(Some(expr)) => match &expr.kind {
                ExpressionKind::Int(5) => {}
                _ => panic!("Expected return 5"),
            },
            _ => panic!("Expected return 5"),
        }
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            ("5 + 5;", "(5 + 5)"),
            ("5 - 5;", "(5 - 5)"),
            ("5 * 5;", "(5 * 5)"),
            ("5 / 5;", "(5 / 5)"),
            ("5 % 5;", "(5 % 5)"),
            ("5 > 5 == 3 < 4;", "((5 > 5) == (3 < 4))"),
            ("5 < 5 != 3 > 4;", "((5 < 5) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "4 + 5 % 2 == 4 * 1 + 5 % 2;",
                "((4 + (5 % 2)) == ((4 * 1) + (5 % 2)))",
            ),
            ("true;", "true"),
            ("false;", "false"),
            ("3 > 5 == false;", "((3 > 5) == false)"),
            ("(5 + 5) * 2;", "((5 + 5) * 2)"),
        ];

        for (input, _) in tests {
            let wrapped_input = format!("fn main() {{ {} }}", input);
            let lexer = Lexer::new(&wrapped_input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let body = get_function_body(&program.statements[0]);
            assert_eq!(body.len(), 1);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "fn main() { if (x < y) { x } else { y } }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        if let StatementKind::If {
            condition,
            then_branch: _,
            else_branch,
        } = &body[0].kind
        {
            match &condition.kind {
                ExpressionKind::Infix {
                    left,
                    operator,
                    right,
                } => {
                    assert_eq!(format!("{:?}", operator), "Lt");
                    match &left.kind {
                        ExpressionKind::Identifier(val) => assert_eq!(val, "x"),
                        _ => panic!("Left side of condition should be identifier 'x'"),
                    }

                    match &right.kind {
                        ExpressionKind::Identifier(val) => assert_eq!(val, "y"),
                        _ => panic!("Right side of condition should be identifier 'y'"),
                    }
                }
                _ => panic!("Invalid condition"),
            }
            assert!(else_branch.is_some());
        } else {
            panic!("Expected If statement");
        }
    }

    #[test]
    fn test_function_call() {
        let input = "fn main() { add(1, 2 * 3, 4 + 5); }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Call {
                    function,
                    arguments,
                } => {
                    match &function.kind {
                        ExpressionKind::Identifier(name) => assert_eq!(name, "add"),
                        _ => panic!("Expected identifier for function call"),
                    }
                    assert_eq!(arguments.len(), 3);
                }
                _ => panic!("Expected Call Expression"),
            },
            _ => panic!("Expected Call Expression"),
        }
    }

    #[test]
    fn test_while_call() {
        let input = "
            fn main() {
                var i: i32 = 0;
                while i < 60 {
                    i = i + 1; 
                }
            }
        ";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        assert_eq!(body.len(), 2);

        match &body[1].kind {
            StatementKind::While { cond, body } => {
                match &cond.kind {
                    ExpressionKind::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        assert_eq!(*operator, Token::Lt);
                        match &left.kind {
                            ExpressionKind::Identifier(name) => assert_eq!(name, "i"),
                            _ => panic!("Expected identifier 'i'"),
                        }
                        match &right.kind {
                            ExpressionKind::Int(val) => assert_eq!(*val, 60),
                            _ => panic!("Expected integer '60'"),
                        }
                    }
                    _ => panic!("Expected Infix expression"),
                }

                match &body.kind {
                    StatementKind::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                    }
                    _ => panic!("Expected Block statement"),
                }
            }
            _ => panic!("Expected While statement"),
        }
    }

    #[test]
    fn test_for_statement() {
        let input = "
        fn main() {
            for item in items {
                print(item);
            }
        }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        assert_eq!(body.len(), 1);

        match &body[0].kind {
            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => {
                assert_eq!(variable, "item");

                match &iterable.kind {
                    ExpressionKind::Identifier(name) => assert_eq!(name, "items"),
                    _ => panic!("Expected identifier 'items'"),
                }

                match &body.kind {
                    StatementKind::Block(stmts) => {
                        assert!(!stmts.is_empty());
                    }
                    _ => panic!("Expected Block statement"),
                }
            }
            _ => panic!("Expected ForIn statement"),
        }
    }

    #[test]
    fn test_structs() {
        let input = "
            struct Vector3 { x: f32, y: f32, z: f32 }
            fn main() {
                var v = Vector3 { x: 1.0, y: 2.0, z: 3.0 };
            }
        ";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 2);

        match &program.statements[0].kind {
            StatementKind::Struct { name, fields, .. } => {
                assert_eq!(name, "Vector3");
                assert_eq!(fields.len(), 3);
                assert_eq!(
                    fields[0],
                    ("x".to_string(), TypeSpec::Named("f32".to_string()))
                );
            }
            _ => panic!("Expected Struct definition"),
        }

        let body = get_function_body(&program.statements[1]);
        match &body[0].kind {
            StatementKind::Var { value, .. } => match &value.kind {
                ExpressionKind::StructLiteral { name, fields } => {
                    assert_eq!(name, "Vector3");
                    assert_eq!(fields.len(), 3);
                    assert_eq!(fields[0].0, "x");
                }
                _ => panic!("Expected StructLiteral"),
            },
            _ => panic!("Expected Var assignment"),
        }
    }

    #[test]
    fn test_advanced_expressions() {
        let input = "
        fn main() {
            var list = [1, 2, 3];
            list[0] += 5 << 1;
            var result = (a && b) || (c & d);
        }
        ";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);
        assert_eq!(body.len(), 3);

        if let StatementKind::Var { value, .. } = &body[0].kind {
            if let ExpressionKind::ArrayLiteral(elems) = &value.kind {
                assert_eq!(elems.len(), 3);
            } else {
                panic!("Expected array literal");
            }
        } else {
            panic!("Expected array literal");
        }

        match &body[1].kind {
            StatementKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Assign {
                    target,
                    operator,
                    value,
                } => {
                    assert_eq!(*operator, Token::PlusEq);
                    if let ExpressionKind::Index { .. } = &target.kind {
                    } else {
                        panic!("Expected Index");
                    }
                    if let ExpressionKind::Infix { operator, .. } = &value.kind {
                        assert_eq!(*operator, Token::ShiftLeft);
                    } else {
                        panic!("Expected Shift");
                    }
                }
                _ => panic!("Expected assign expression"),
            },
            _ => panic!("Expected assign statement"),
        }

        match &body[2].kind {
            StatementKind::Var { value, .. } => {
                if let ExpressionKind::Infix { operator, .. } = &value.kind {
                    assert_eq!(*operator, Token::Or);
                }
            }
            _ => panic!("Expected logic var"),
        }
    }

    #[test]
    fn test_bitwise_and_compound_ops() {
        let input = "
        fn main() {
            var bitwise = x & y | z ^ w; 
            var shift = x << 1 >> 2;
            x &= 1;
            x |= 2;
            x ^= 3;
            x <<= 4;
            x >>= 5;
            x *= 6;
            x /= 7;
            x %= 8;
        }
        ";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);
        assert_eq!(body.len(), 10);

        let check_assign = |index: usize, expected_op: Token| match &body[index].kind {
            StatementKind::Expression(expr) => match &expr.kind {
                ExpressionKind::Assign { operator, .. } => {
                    assert_eq!(*operator, expected_op, "Error at statement index {}", index);
                }
                _ => panic!("Expected assignment at index {}", index),
            },
            _ => panic!("Expected assignment at index {}", index),
        };

        if let StatementKind::Var { value, .. } = &body[0].kind {
            if let ExpressionKind::Infix {
                operator,
                left: _,
                right,
            } = &value.kind
            {
                assert_eq!(*operator, Token::BitOr);
                if let ExpressionKind::Infix { operator: op_r, .. } = &right.kind {
                    assert_eq!(*op_r, Token::BitXor);
                } else {
                    panic!("Right side of | should be ^");
                }
            } else {
                panic!("Expected infix for bitwise");
            }
        }

        if let StatementKind::Var { value, .. } = &body[1].kind
            && let ExpressionKind::Infix { operator, .. } = &value.kind
        {
            assert_eq!(*operator, Token::ShiftRight);
        }

        check_assign(2, Token::BitAndEq);
        check_assign(3, Token::BitOrEq);
        check_assign(4, Token::BitXorEq);
        check_assign(5, Token::BitLShiftEq);
        check_assign(6, Token::BitRShiftEq);
        check_assign(7, Token::StarEq);
        check_assign(8, Token::SlashEq);
        check_assign(9, Token::ModEq);
    }

    #[test]
    fn test_break_continue() {
        let input = "
            fn main() {
                while true {
                    if x > 10 { break; }
                    continue;
                }
            }
        ";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::While { body, .. } => match &body.kind {
                StatementKind::Block(stmts) => {
                    assert_eq!(stmts.len(), 2);
                    if let StatementKind::If { then_branch, .. } = &stmts[0].kind {
                        if let StatementKind::Block(inner_stmts) = &then_branch.kind {
                            assert!(matches!(inner_stmts[0].kind, StatementKind::Break));
                        } else {
                            panic!("Expected block in if");
                        }
                    } else {
                        panic!("Expected if");
                    }
                    assert!(matches!(stmts[1].kind, StatementKind::Continue));
                }
                _ => panic!("Expected block body"),
            },
            _ => panic!("Expected While"),
        }
    }

    #[test]
    fn test_imports() {
        let input = "
            import std.os;
            import std.math
            import std.collections::{Array, HashMap};
        ";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 3);

        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Import { .. }
        ));
        assert!(matches!(
            &program.statements[1].kind,
            StatementKind::Import { .. }
        ));
        assert!(matches!(
            &program.statements[2].kind,
            StatementKind::Import { .. }
        ));
    }

    #[test]
    fn test_unified_struct_enum_match() {
        let input = "
            enum Color { Red, Green, Blue }

            struct Player {
                name: String,
                health: i32,

                fn new(name: String) Player {
                    return Player { name: name, health: 100 };
                }

                fn take_damage(self, amount: i32) {
                    self.health -= amount;
                }
            }

            fn main() {
                var status = match x {
                    0 => \"Dead\",
                    1 => \"Alive\",
                    default => \"Unknown\"
                };
            }
        ";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 3);

        if let StatementKind::Enum { name, variants } = &program.statements[0].kind {
            assert_eq!(name, "Color");
            assert_eq!(variants.len(), 3);
        } else {
            panic!("Expected Enum");
        }

        if let StatementKind::Struct {
            name,
            fields,
            methods,
            ..
        } = &program.statements[1].kind
        {
            assert_eq!(name, "Player");
            assert_eq!(fields.len(), 2);
            assert_eq!(methods.len(), 2);

            if let StatementKind::Function { name, params, .. } = &methods[1].kind {
                assert_eq!(name, "take_damage");
                assert_eq!(params[0].0, "self");
            } else {
                panic!("Expected method take_damage");
            }
        } else {
            panic!("Expected Struct");
        }

        let body = get_function_body(&program.statements[2]);

        if let StatementKind::Var { value, .. } = &body[0].kind {
            if let ExpressionKind::Match { value: _, arms } = &value.kind {
                assert_eq!(arms.len(), 3);
                if let ExpressionKind::Identifier(p) = &arms[2].0.kind {
                    assert_eq!(p, "default");
                } else {
                    panic!("Expected default pattern");
                }
            } else {
                panic!("Expected Match expr");
            }
        } else {
            panic!("Expected Var declaration");
        }
    }

    #[test]
    fn test_arrays_syntax() {
        let input = "
        fn main() {
            var b: Array<i32, 5> = [10, 20, 30, 40, 50];
            var c = [0; 3];
        }
        ";
        let program = parse_input(input);

        check_parser_errors(&Parser::new(Lexer::new(input)));

        let body = get_function_body(&program.statements[0]);
        assert_eq!(body.len(), 2);

        match &body[0].kind {
            StatementKind::Var {
                type_annotation, ..
            } => {
                let expected = TypeSpec::Generic {
                    name: "Array".to_string(),
                    args: vec![TypeSpec::Named("i32".to_string()), TypeSpec::IntLiteral(5)],
                };
                assert_eq!(type_annotation.as_ref().unwrap(), &expected);
            }
            _ => panic!("Expected Var b"),
        }

        match &body[1].kind {
            StatementKind::Var { value, .. } => {
                if let ExpressionKind::ArrayLiteral(elements) = &value.kind {
                    assert_eq!(elements.len(), 3);
                    for expr in elements {
                        if let ExpressionKind::Int(val) = &expr.kind {
                            assert_eq!(*val, 0);
                        } else {
                            panic!("Expected Int(0)");
                        }
                    }
                } else {
                    panic!("Expected ArrayLiteral");
                }
            }
            _ => panic!("Expected Var c"),
        }
    }

    #[test]
    fn test_nested_arrays() {
        let input = "fn main() { var matrix: Array<Array<i32, 2>, 2> = [[1, 2], [3, 4]]; }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Var {
                type_annotation, ..
            } => {
                let expected = TypeSpec::Generic {
                    name: "Array".to_string(),
                    args: vec![
                        TypeSpec::Generic {
                            name: "Array".to_string(),
                            args: vec![TypeSpec::Named("i32".to_string()), TypeSpec::IntLiteral(2)],
                        },
                        TypeSpec::IntLiteral(2),
                    ],
                };
                assert_eq!(type_annotation.as_ref().unwrap(), &expected);
            }
            _ => panic!("Expected Matrix Var"),
        }
    }

    #[test]
    fn test_tuple_parsing() {
        let input = "fn main() { var t: (i32, bool) = (42, true); }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Var {
                type_annotation,
                value,
                ..
            } => {
                let expected_type = TypeSpec::Tuple(vec![
                    TypeSpec::Named("i32".to_string()),
                    TypeSpec::Named("bool".to_string()),
                ]);
                assert_eq!(type_annotation.as_ref().unwrap(), &expected_type);

                match &value.kind {
                    ExpressionKind::Tuple(elements) => {
                        assert_eq!(elements.len(), 2);
                    }
                    _ => panic!("Expected Tuple expression"),
                }
            }
            _ => panic!("Expected Var statement"),
        }
    }

    #[test]
    fn test_empty_tuple_parsing() {
        let input = "fn main() { var t: () = (); }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Var {
                type_annotation,
                value,
                ..
            } => {
                assert_eq!(type_annotation.as_ref().unwrap(), &TypeSpec::Tuple(vec![]));
                match &value.kind {
                    ExpressionKind::Tuple(elements) => {
                        assert!(elements.is_empty());
                    }
                    _ => panic!("Expected empty Tuple expression"),
                }
            }
            _ => panic!("Expected Var statement"),
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

    #[test]
    fn test_grouped_expression_not_tuple() {
        let input = "fn main() { var x = (42); }";
        let program = parse_input(input);
        let body = get_function_body(&program.statements[0]);

        match &body[0].kind {
            StatementKind::Var { value, .. } => match &value.kind {
                ExpressionKind::Int(_) => {}
                ExpressionKind::Tuple(_) => {
                    panic!("(42) should not be a tuple, use (42,) for single-element tuple")
                }
                _ => panic!("Expected integer literal from grouped expression"),
            },
            _ => panic!("Expected Var statement"),
        }
    }

    #[test]
    fn test_generic_function() {
        let input = "fn identity<T>(x: T) T { return x; }";
        let program = parse_input(input);

        if let StatementKind::Function {
            name,
            type_params,
            params,
            return_type,
            ..
        } = &program.statements[0].kind
        {
            assert_eq!(name, "identity");
            assert_eq!(type_params.len(), 1);
            assert_eq!(type_params[0].name, "T");
            assert!(type_params[0].bound.is_none());
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].0, "x");
            assert!(return_type.is_some());
        } else {
            panic!("Expected Function");
        }
    }

    #[test]
    fn test_generic_function_with_bound() {
        let input = "fn sort<T: Comparable>(arr: *T) { }";
        let program = parse_input(input);

        if let StatementKind::Function {
            name, type_params, ..
        } = &program.statements[0].kind
        {
            assert_eq!(name, "sort");
            assert_eq!(type_params.len(), 1);
            assert_eq!(type_params[0].name, "T");
            assert_eq!(type_params[0].bound, Some("Comparable".to_string()));
        } else {
            panic!("Expected Function");
        }
    }

    #[test]
    fn test_generic_function_multiple_params() {
        let input = "fn map<T, U>(x: T) U { }";
        let program = parse_input(input);

        if let StatementKind::Function { type_params, .. } = &program.statements[0].kind {
            assert_eq!(type_params.len(), 2);
            assert_eq!(type_params[0].name, "T");
            assert_eq!(type_params[1].name, "U");
        } else {
            panic!("Expected Function");
        }
    }

    #[test]
    fn test_generic_struct() {
        let input = "struct Box<T> { value: T, }";
        let program = parse_input(input);

        if let StatementKind::Struct {
            name,
            type_params,
            fields,
            ..
        } = &program.statements[0].kind
        {
            assert_eq!(name, "Box");
            assert_eq!(type_params.len(), 1);
            assert_eq!(type_params[0].name, "T");
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "value");
        } else {
            panic!("Expected Struct");
        }
    }

    #[test]
    fn test_trait_definition() {
        let input = r#"
            trait Drawable {
                fn draw(self);
                fn get_name(self) *u8;
            }
        "#;
        let program = parse_input(input);

        if let StatementKind::Trait { name, methods } = &program.statements[0].kind {
            assert_eq!(name, "Drawable");
            assert_eq!(methods.len(), 2);
            assert_eq!(methods[0].name, "draw");
            assert!(methods[0].return_type.is_none());
            assert_eq!(methods[1].name, "get_name");
            assert!(methods[1].return_type.is_some());
        } else {
            panic!("Expected Trait");
        }
    }

    #[test]
    fn test_trait_empty() {
        let input = "trait Marker { }";
        let program = parse_input(input);

        if let StatementKind::Trait { name, methods } = &program.statements[0].kind {
            assert_eq!(name, "Marker");
            assert_eq!(methods.len(), 0);
        } else {
            panic!("Expected Trait");
        }
    }
}
