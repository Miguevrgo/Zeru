use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();

        while self.current_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
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
            _ => self.parse_expression_statement(), //TODO: Hmmmmmm
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

            //TODO: Support complex types
            if let Token::Identifier(t_name) = &self.current_token {
                type_annotation = Some(t_name.clone());
            } else {
                self.error_peek("Type expected after ':'");
                return None;
            }
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
            //TODO: Complex types
            if let Token::Identifier(t) = &self.current_token {
                return_type = Some(t.clone());
            } else {
                self.error_peek("Fucntion return type or '{'");
                return None;
            }
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

    fn parse_function_parameters(&mut self) -> Vec<(String, String)> {
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

    fn parse_parameter(&mut self) -> (String, String) {
        let name = match &self.current_token {
            Token::Identifier(n) => n.clone(),
            _ => {
                self.errors
                    .push("Expected parameter name in fn definition".into());
                String::new()
            }
        };

        if !self.expect_peek(&Token::Colon) {
            return (name, String::new());
        }
        self.next_token();

        let type_name = match &self.current_token {
            Token::Identifier(t) => t.clone(),
            _ => {
                self.errors
                    .push("Expected parameter type in fn definition".into());
                String::new()
            }
        };

        (name, type_name)
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

        while !self.peek_token_is(&Token::RBrace) && !self.peek_token_is(&Token::Eof) {
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

            let field_type = match &self.current_token {
                Token::Identifier(t) => t.clone(),
                _ => {
                    self.errors.push("Expected field type".into());
                    return None;
                }
            };

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
        Some(Statement::Struct { name, fields })
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
            Token::LParen => self.parse_grouped_expression(),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            _ => {
                self.error_peek(
                    format!("No prefix parse function for {:?}", &self.current_token).as_str(),
                );
                None
            }
        };

        left_exp.as_ref()?;

        while !self.peek_token_is(&Token::Semicolon)
            && precedence < token_precedence(&self.peek_token)
        {
            self.next_token();
            left_exp = self.parse_infix_expression(left_exp.unwrap());
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

        if operator == Token::LParen {
            return self.parse_call_expression(left);
        }

        if operator == Token::Dot {
            return self.parse_get_expression(left);
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
        self.next_token();

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
            "Syntax error: {expected} expected, found: {:?}",
            self.peek_token
        ));
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

fn token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Assign | Token::Eq | Token::NotEq => Precedence::Equals,
        Token::Gt | Token::Lt => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Star | Token::Slash => Precedence::Product,
        Token::LParen => Precedence::Call,
        Token::Dot | Token::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Program, Statement},
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

    #[test]
    fn test_declarations() {
        let input = "
        var x = 5;
        var y: i32 = 5;
        const X: u32 = 10;
        const Y: f32 = 10.0;
        var z = y;
    ";

        let program = parse_input(input);
        assert_eq!(program.statements.len(), 5);

        match &program.statements[0] {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                assert_eq!(name, "x");
                assert!(!*is_const);
                assert_eq!(*type_annotation, None);

                match value {
                    Expression::Int(val) => assert_eq!(*val, 5),
                    _ => panic!("Expected Int"),
                }
            }
            _ => panic!("Expected Var statement"),
        }

        match &program.statements[1] {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                assert_eq!(name, "y");
                assert!(!*is_const);
                assert_eq!(*type_annotation, Some("i32".to_string()));

                match value {
                    Expression::Int(val) => assert_eq!(*val, 5),
                    _ => panic!("Expected Int"),
                }
            }
            _ => panic!("Expected Var statement"),
        }

        match &program.statements[2] {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                assert_eq!(name, "X");
                assert!(*is_const);
                assert_eq!(*type_annotation, Some("u32".to_string()));

                match value {
                    Expression::Int(val) => assert_eq!(*val, 10),
                    _ => panic!("Expected Int"),
                }
            }
            _ => panic!("Expected Const statement"),
        }

        match &program.statements[3] {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                assert_eq!(name, "Y");
                assert!(*is_const);
                assert_eq!(*type_annotation, Some("f32".to_string()));

                match value {
                    Expression::Float(val) => assert_eq!(*val, 10.0),
                    _ => panic!("Expected Float"),
                }
            }
            _ => panic!("Expected Const statement"),
        }

        match &program.statements[4] {
            Statement::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                assert_eq!(name, "z");
                assert!(!*is_const);
                assert_eq!(*type_annotation, None);

                match value {
                    Expression::Identifier(id) => assert_eq!(id, "y"),
                    _ => panic!("Expected Identifier"),
                }
            }
            _ => panic!("Expected Var statement"),
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "return 5; return 10; return;";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 3);

        match &program.statements[0] {
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
            ("5 > 5 == 3 < 4;", "((5 > 5) == (3 < 4))"),
            ("5 < 5 != 3 > 4;", "((5 < 5) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true;", "true"),
            ("false;", "false"),
            ("3 > 5 == false;", "((3 > 5) == false)"),
            ("(5 + 5) * 2;", "((5 + 5) * 2)"),
        ];

        for (input, _) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x } else { y }";
        let program = parse_input(input);

        match &program.statements[0] {
            Statement::If {
                condition,
                then_branch: _,
                else_branch,
            } => {
                match condition {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        assert_eq!(format!("{:?}", operator), "Lt");
                        match &**left {
                            Expression::Identifier(val) => assert_eq!(val, "x"),
                            _ => panic!("Left side of condition should be identifier 'x'"),
                        }

                        match &**right {
                            Expression::Identifier(val) => assert_eq!(val, "y"),
                            _ => panic!("Right side of condition should be identifier 'y'"),
                        }
                    }
                    _ => panic!("Invalid condition"),
                }
                assert!(else_branch.is_some());
            }
            _ => panic!("Expected If statement"),
        }
    }

    #[test]
    fn test_function_call() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let program = parse_input(input);

        match &program.statements[0] {
            Statement::Expression(Expression::Call {
                function,
                arguments,
            }) => {
                match &**function {
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
            var i: i32 = 0;
            while i < 60 {
                i = i + 1; 
            }
        ";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 2);

        match &program.statements[1] {
            Statement::While { cond, body } => {
                match cond {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                    } => {
                        assert_eq!(*operator, Token::Lt);
                        match &**left {
                            Expression::Identifier(name) => assert_eq!(name, "i"),
                            _ => panic!("Expected identifier 'i' in condition left side"),
                        }
                        match &**right {
                            Expression::Int(val) => assert_eq!(*val, 60),
                            _ => panic!("Expected integer '60' in condition right side"),
                        }
                    }
                    _ => panic!("Expected Infix expression for while condition"),
                }

                match &**body {
                    Statement::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                    }
                    _ => panic!("Expected Block statement for while body"),
                }
            }
            _ => panic!("Expected While statement"),
        }
    }

    #[test]
    fn test_for_statement() {
        let input = "
        for item in items {
            print(item);
        }";
        let program = parse_input(input);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ForIn {
                variable,
                iterable,
                body,
            } => {
                assert_eq!(variable, "item");

                match iterable {
                    Expression::Identifier(name) => assert_eq!(name, "items"),
                    _ => panic!("Expected identifier 'items' as iterable"),
                }

                match &**body {
                    Statement::Block(stmts) => {
                        assert!(!stmts.is_empty());
                    }
                    _ => panic!("Expected Block statement for for-loop body"),
                }
            }
            _ => panic!("Expected ForIn statement"),
        }
    }

    #[test]
    fn test_structs() {
        let input = "
            struct Vector3 { x: f32, y: f32, z: f32 }
            var v = Vector3 { x: 1.0, y: 2.0, z: 3.0 };
        ";
        let program = parse_input(input);
        assert_eq!(program.statements.len(), 2);

        match &program.statements[0] {
            Statement::Struct { name, fields } => {
                assert_eq!(name, "Vector3");
                assert_eq!(fields.len(), 3);
                assert_eq!(fields[0], ("x".to_string(), "f32".to_string()));
            }
            _ => panic!("Expected Struct definition"),
        }

        match &program.statements[1] {
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
}
