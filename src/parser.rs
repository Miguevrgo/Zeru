use crate::{
    ast::{Expression, Program, Statement, TypeSpec},
    lexer::Lexer,
    token::Token,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Token,
    peek_token: Token,
    peek_line: usize,

    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            peek_line: 0,
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        let (tok, line) = self.lexer.next_token();
        self.peek_token = tok;
        self.peek_line = line;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.current_token != Token::Eof {
            let stmt = match self.current_token {
                Token::Const => self.parse_var_statement::<true>(),
                Token::Fn => self.parse_function_statement(),
                Token::Struct => self.parse_struct_statement(),
                Token::Enum => self.parse_enum_statement(),
                Token::Import => self.parse_import_statement(),
                Token::Semicolon => {
                    self.next_token();
                    None
                }
                Token::Var => {
                    self.error_current("Global variables ('var') are not allowed. Use 'const' for constants or move 'var' inside a function.");
                    self.parse_var_statement::<false>()
                }
                _ => {
                    self.error_current(format!("Unexpected token {:?} at top level. Expected fn, struct, enum, const or import.", self.current_token).as_str());
                    self.parse_var_statement::<false>()
                }
            };

            if let Some(statement) = stmt {
                statements.push(statement);
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
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Expression(expr))
    }

    fn parse_var_statement<const CONSTANT: bool>(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
        };

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

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Var {
            name,
            is_const: CONSTANT,
            value,
            type_annotation,
        })
    }

    fn parse_type(&mut self) -> Option<TypeSpec> {
        let mut name = if let Token::Identifier(n) = &self.current_token {
            n.clone()
        } else {
            self.error_current("Type identifier expected");
            return None;
        };

        while self.peek_token_is(&Token::DoubleColon) {
            self.next_token();
            self.next_token();
            if let Token::Identifier(n) = &self.current_token {
                name.push_str("::");
                name.push_str(n);
            } else {
                self.error_current("Expected type name after '::'");
                return None;
            }
        }

        if self.peek_token_is(&Token::Lt) {
            self.next_token();
            let mut args = Vec::new();

            self.next_token();

            loop {
                if self.current_token == Token::Gt {
                    break;
                }

                if let Token::Int(val) = self.current_token {
                    args.push(TypeSpec::IntLiteral(val));
                } else {
                    let sub_type = self.parse_type()?;
                    args.push(sub_type);
                }

                if self.peek_token_is(&Token::Comma) {
                    self.next_token();
                    self.next_token();
                    continue;
                } else if self.peek_token_is(&Token::Gt) {
                    self.next_token();
                    break;
                } else {
                    self.error_peek("Expected ',' or '>' in generic type");
                    return None;
                }
            }
            return Some(TypeSpec::Generic { name, args });
        }

        Some(TypeSpec::Named(name))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let return_value = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            self.parse_expression(Precedence::Lowest)
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(return_value))
    }

    fn parse_function_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
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

        Some(Statement::Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Vec<(String, TypeSpec)> {
        let mut params = Vec::new();
        if self.peek_token_is(&Token::RParen) {
            self.next_token();
            return params;
        }

        self.next_token();
        params.push(self.parse_parameter());

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            params.push(self.parse_parameter());
        }

        if !self.expect_peek(&Token::RParen) {
            return Vec::new();
        }
        params
    }

    fn parse_parameter(&mut self) -> (String, TypeSpec) {
        if self.cur_token_is(&Token::SelfTok) {
            return ("self".to_string(), TypeSpec::Named("self".to_string()));
        }

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => {
                self.errors.push("Expected param name".into());
                String::new()
            }
        };

        if !self.expect_peek(&Token::Colon) {
            return (name, TypeSpec::Named("Unknown".to_string()));
        }
        self.next_token();

        let type_spec = self
            .parse_type()
            .unwrap_or(TypeSpec::Named("Unknown".to_string()));
        (name, type_spec)
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let then_branch = Box::new(Statement::Block(self.parse_block_statement()));

        let else_branch = if self.peek_token_is(&Token::Else) {
            self.next_token();
            if self.peek_token_is(&Token::If) {
                self.next_token();
                Some(Box::new(self.parse_if_statement()?))
            } else {
                if !self.expect_peek(&Token::LBrace) {
                    return None;
                }
                Some(Box::new(Statement::Block(self.parse_block_statement())))
            }
        } else {
            None
        };

        Some(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let cond = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let body = Box::new(Statement::Block(self.parse_block_statement()));

        Some(Statement::While { cond, body })
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }
        let variable = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
        };

        if !self.expect_peek(&Token::In) {
            return None;
        }
        self.next_token();

        let iterable = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }
        let body = Box::new(Statement::Block(self.parse_block_statement()));

        Some(Statement::ForIn {
            variable,
            iterable,
            body,
        })
    }

    fn parse_struct_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }
        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
        };

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut parsing_methods = false;

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            if self.peek_token_is(&Token::Fn) {
                parsing_methods = true;
                self.next_token();

                if let Some(method) = self.parse_function_statement() {
                    methods.push(method);
                }
                continue;
            }

            if parsing_methods {
                self.errors.push(format!(
                    "Struct '{}': Fields must be declared before methods.",
                    name
                ));
                return None;
            }

            self.next_token();
            let field_name = match &self.current_token {
                Token::Identifier(n) => n.clone(),
                _ => {
                    self.errors.push("Expected field name".into());
                    return None;
                }
            };

            if !self.expect_peek(&Token::Colon) {
                return None;
            }
            self.next_token();

            let field_type = self.parse_type()?;
            fields.push((field_name, field_type));

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
        Some(Statement::Struct {
            name,
            fields,
            methods,
        })
    }

    fn parse_enum_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
        };

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut variants = Vec::new();
        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            self.next_token();
            if let Token::Identifier(v) = &self.current_token {
                variants.push(v.clone())
            } else {
                self.errors.push("Expected enum variant name".into());
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

        Some(Statement::Enum { name, variants })
    }

    fn parse_struct_literal(&mut self, name: String) -> Option<Expression> {
        self.next_token();

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

        Some(Expression::StructLiteral { name, fields })
    }

    fn parse_block_statement_wrapper(&mut self) -> Option<Statement> {
        Some(Statement::Block(self.parse_block_statement()))
    }

    fn parse_import_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek_identifier() {
            return None;
        }

        let mut path = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => unreachable!(),
        };

        while self.peek_token_is(&Token::Dot) {
            self.next_token();
            path.push('.');

            if !self.expect_peek_identifier() {
                return None;
            }

            if let Token::Identifier(n) = &self.current_token {
                path.push_str(n);
            }
        }

        let mut symbols = Vec::new();

        if self.peek_token_is(&Token::DoubleColon) {
            self.next_token();

            if !self.expect_peek(&Token::LBrace) {
                return None;
            }

            while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
                self.next_token();
                if let Token::Identifier(sym) = &self.current_token {
                    symbols.push(sym.clone());
                } else {
                    self.errors.push("Expected symbol name in import".into());
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
        }

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Import { path, symbols })
    }

    fn parse_break_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Break)
    }

    fn parse_continue_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Continue)
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
        let mut left_exp = match &self.current_token {
            Token::LBracket => self.parse_array_literal(),
            Token::Identifier(name) => {
                let starts_with_upper = name.chars().next().is_some_and(|c| c.is_uppercase());
                if starts_with_upper && self.peek_token_is(&Token::LBrace) {
                    self.parse_struct_literal(name.clone())
                } else {
                    Some(Expression::Identifier(name.clone()))
                }
            }
            Token::Int(val) => Some(Expression::Int(*val)),
            Token::Float(val) => Some(Expression::Float(*val)),
            Token::StringLit(val) => Some(Expression::StringLit(val.clone())),
            Token::None => Some(Expression::None),
            Token::True => Some(Expression::Boolean(true)),
            Token::False => Some(Expression::Boolean(false)),
            Token::LParen => self.parse_grouped_expression(),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            Token::Match => self.parse_match_expression(),
            Token::SelfTok => Some(Expression::Identifier("self".to_string())),
            _ => {
                self.error_current(
                    format!("Expected expression, found: {:?}", &self.current_token).as_str(),
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
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::RParen) {
            return None;
        }
        exp
    }

    fn parse_match_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::LBrace) {
            return None;
        }

        let mut arms = Vec::new();

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
            self.next_token();

            let pattern = if self.cur_token_is(&Token::Default) {
                Expression::Identifier("default".to_string())
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

        Some(Expression::Match {
            value: Box::new(value),
            arms,
        })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.current_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix {
            operator,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.current_token.clone();

        if operator == Token::LBracket {
            return self.parse_index_expression(left);
        }
        if operator == Token::LParen {
            return self.parse_call_expression(left);
        }
        if operator == Token::Dot {
            return self.parse_get_expression(left);
        }

        if operator == Token::As {
            let precedence = token_precedence(&self.current_token);
            self.next_token();

            let target = self.parse_expression(precedence)?;
            return Some(Expression::Cast {
                left: Box::new(left),
                target: Box::new(target),
            });
        }

        match operator {
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
            | Token::BitRShiftEq => {
                let precedence = token_precedence(&self.current_token);
                self.next_token();
                let value = self.parse_expression(precedence)?;
                return Some(Expression::Assign {
                    target: Box::new(left),
                    operator,
                    value: Box::new(value),
                });
            }
            _ => {}
        }

        let precedence = token_precedence(&self.current_token);
        self.next_token();
        let right = self.parse_expression(precedence);

        right.map(|right_val| Expression::Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right_val),
        })
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        self.next_token();
        let mut elements = Vec::new();

        if self.cur_token_is(&Token::RBracket) {
            self.next_token();
            return Some(Expression::ArrayLiteral(elements));
        }

        let first_elem = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token();
            self.next_token();

            let count = match &self.current_token {
                Token::Int(n) => *n,
                _ => {
                    self.errors
                        .push("Array repeat count must be an integer literal".into());
                    return None;
                }
            };

            if !self.expect_peek(&Token::RBracket) {
                return None;
            }

            for _ in 0..count {
                elements.push(first_elem.clone());
            }
            return Some(Expression::ArrayLiteral(elements));
        }

        let mut elements = Vec::new();
        elements.push(first_elem);

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            elements.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&Token::RBracket) {
            return None;
        }
        Some(Expression::ArrayLiteral(elements))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::RBracket) {
            return None;
        }
        Some(Expression::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        self.next_token();
        let arguments = self.parse_call_arguments();

        Some(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_get_expression(&mut self, obj: Expression) -> Option<Expression> {
        self.next_token();

        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => {
                self.error_peek("Expected Identifier after '.'");
                return None;
            }
        };

        Some(Expression::Get {
            object: Box::new(obj),
            name,
        })
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        // Empty call: ()
        if self.cur_token_is(&Token::RParen) {
            self.next_token();
            return args;
        }

        if let Some(arg) = self.parse_expression(Precedence::Lowest) {
            args.push(arg);
        }

        while self.peek_token_is(&Token::Comma) {
            self.next_token();
            self.next_token();
            if let Some(arg) = self.parse_expression(Precedence::Lowest) {
                args.push(arg);
            }
        }

        if !self.expect_peek(&Token::RParen) {
            return Vec::new();
        }
        self.next_token();

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

    fn expect_peek_identifier(&mut self) -> bool {
        match &self.peek_token {
            Token::Identifier(_) => {
                self.next_token();
                true
            }
            _ => {
                self.error_peek("Identifier");
                false
            }
        }
    }

    fn error_peek(&mut self, expected: &str) {
        self.errors.push(format!(
            "[Line {}] \x1b[31mSyntax error:\x1b[0m {expected} expected, found: {:?}",
            self.peek_line, self.current_token
        ));
    }

    fn error_current(&mut self, msg: &str) {
        self.errors.push(format!(
            "[Line {}] \x1b[31mSyntax error:\x1b[0m {}",
            self.peek_line - 1,
            msg
        ));
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equals,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Cast,
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
        ast::{Expression, Program, Statement, TypeSpec},
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
        for msg in &parser.errors {
            eprintln!("Parser error: {}", msg);
        }
        panic!("Parser failed")
    }

    fn get_function_body(statement: &Statement) -> &Vec<Statement> {
        match statement {
            Statement::Function { body, .. } => body,
            _ => panic!("Expected Function statement"),
        }
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

        match &program.statements[0] {
            Statement::Var { name, is_const, .. } => {
                assert_eq!(name, "X");
                assert!(*is_const);
            }
            _ => panic!("Expected Const X"),
        }

        let body = get_function_body(&program.statements[2]);
        assert_eq!(body.len(), 3);
        match &body[0] {
            Statement::Var { name, is_const, .. } => {
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
        match &body[0] {
            Statement::Return(Some(Expression::Int(5))) => {}
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

        if let Statement::If {
            condition,
            then_branch: _,
            else_branch,
        } = &body[0]
        {
            match condition {
                Expression::Infix {
                    left,
                    operator,
                    right,
                } => {
                    assert_eq!(format!("{:?}", operator), "Lt");
                    match left.as_ref() {
                        Expression::Identifier(val) => assert_eq!(val, "x"),
                        _ => panic!("Left side of condition should be identifier 'x'"),
                    }

                    match right.as_ref() {
                        Expression::Identifier(val) => assert_eq!(val, "y"),
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

        match &body[0] {
            Statement::Expression(Expression::Call {
                function,
                arguments,
            }) => {
                match function.as_ref() {
                    Expression::Identifier(name) => assert_eq!(name, "add"),
                    _ => panic!("Expected identifier for function call"),
                }
                assert_eq!(arguments.len(), 3);
            }
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

        match &body[1] {
            Statement::While { cond, body } => {
                match cond {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        assert_eq!(*operator, Token::Lt);
                        match left.as_ref() {
                            Expression::Identifier(name) => assert_eq!(name, "i"),
                            _ => panic!("Expected identifier 'i'"),
                        }
                        match right.as_ref() {
                            Expression::Int(val) => assert_eq!(*val, 60),
                            _ => panic!("Expected integer '60'"),
                        }
                    }
                    _ => panic!("Expected Infix expression"),
                }

                match body.as_ref() {
                    Statement::Block(stmts) => {
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

        match &body[0] {
            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                assert_eq!(variable, "item");

                match iterable {
                    Expression::Identifier(name) => assert_eq!(name, "items"),
                    _ => panic!("Expected identifier 'items'"),
                }

                match body.as_ref() {
                    Statement::Block(stmts) => {
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

        match &program.statements[0] {
            Statement::Struct { name, fields, .. } => {
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
        match &body[0] {
            Statement::Var { value, .. } => match value {
                Expression::StructLiteral { name, fields } => {
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

        if let Statement::Var {
            value: Expression::ArrayLiteral(elems),
            ..
        } = &body[0]
        {
            assert_eq!(elems.len(), 3);
        } else {
            panic!("Expected array literal");
        }

        match &body[1] {
            Statement::Expression(Expression::Assign {
                target,
                operator,
                value,
            }) => {
                assert_eq!(*operator, Token::PlusEq);
                if let Expression::Index { .. } = target.as_ref() {
                } else {
                    panic!("Expected Index");
                }
                if let Expression::Infix { operator, .. } = value.as_ref() {
                    assert_eq!(*operator, Token::ShiftLeft);
                } else {
                    panic!("Expected Shift");
                }
            }
            _ => panic!("Expected assign statement"),
        }

        match &body[2] {
            Statement::Var { value, .. } => {
                if let Expression::Infix { operator, .. } = value {
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

        let check_assign = |index: usize, expected_op: Token| match &body[index] {
            Statement::Expression(Expression::Assign { operator, .. }) => {
                assert_eq!(*operator, expected_op, "Error at statement index {}", index);
            }
            _ => panic!("Expected assignment at index {}", index),
        };

        if let Statement::Var { value, .. } = &body[0] {
            if let Expression::Infix {
                operator,
                left: _,
                right,
            } = value
            {
                assert_eq!(*operator, Token::BitOr);
                if let Expression::Infix { operator: op_r, .. } = right.as_ref() {
                    assert_eq!(*op_r, Token::BitXor);
                } else {
                    panic!("Right side of | should be ^");
                }
            } else {
                panic!("Expected infix for bitwise");
            }
        }

        if let Statement::Var {
            value: Expression::Infix { operator, .. },
            ..
        } = &body[1]
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

        match &body[0] {
            Statement::While { body, .. } => match body.as_ref() {
                Statement::Block(stmts) => {
                    assert_eq!(stmts.len(), 2);
                    if let Statement::If { then_branch, .. } = &stmts[0] {
                        if let Statement::Block(inner_stmts) = then_branch.as_ref() {
                            assert!(matches!(inner_stmts[0], Statement::Break));
                        } else {
                            panic!("Expected block in if");
                        }
                    } else {
                        panic!("Expected if");
                    }
                    assert!(matches!(stmts[1], Statement::Continue));
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

        if let Statement::Import { path, symbols } = &program.statements[0] {
            assert_eq!(path, "std.os");
            assert!(symbols.is_empty());
        } else {
            panic!("Expected import std.os");
        }

        if let Statement::Import { path, symbols } = &program.statements[1] {
            assert_eq!(path, "std.math");
            assert!(symbols.is_empty());
        } else {
            panic!("Expected import std.math");
        }

        if let Statement::Import { path, symbols } = &program.statements[2] {
            assert_eq!(path, "std.collections");
            assert_eq!(symbols.len(), 2);
            assert_eq!(symbols[0], "Array");
            assert_eq!(symbols[1], "HashMap");
        } else {
            panic!("Expected import std.collections");
        }
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

        if let Statement::Enum { name, variants } = &program.statements[0] {
            assert_eq!(name, "Color");
            assert_eq!(variants.len(), 3);
        } else {
            panic!("Expected Enum");
        }

        if let Statement::Struct {
            name,
            fields,
            methods,
        } = &program.statements[1]
        {
            assert_eq!(name, "Player");
            assert_eq!(fields.len(), 2);
            assert_eq!(methods.len(), 2);

            if let Statement::Function { name, params, .. } = &methods[1] {
                assert_eq!(name, "take_damage");
                assert_eq!(params[0].0, "self");
            } else {
                panic!("Expected method take_damage");
            }
        } else {
            panic!("Expected Struct");
        }

        let body = get_function_body(&program.statements[2]);

        if let Statement::Var { value, .. } = &body[0] {
            if let Expression::Match { value: _, arms } = value {
                assert_eq!(arms.len(), 3);
                if let Expression::Identifier(p) = &arms[2].0 {
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

        match &body[0] {
            Statement::Var {
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

        match &body[1] {
            Statement::Var { value, .. } => {
                if let Expression::ArrayLiteral(elements) = value {
                    assert_eq!(elements.len(), 3);
                    for expr in elements {
                        if let Expression::Int(val) = expr {
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

        match &body[0] {
            Statement::Var {
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
}
