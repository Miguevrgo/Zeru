use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn advance(&mut self) -> Option<char> {
        self.input.next()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    self.advance();
                }
                '/' => {
                    let mut lookahead = self.input.clone();
                    lookahead.next();

                    match lookahead.peek() {
                        Some('/') => {
                            self.advance();
                            self.advance();

                            while let Some(&c) = self.peek() {
                                if c == '\n' {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        Some('*') => {
                            self.advance();
                            self.advance();

                            loop {
                                match self.advance() {
                                    Some('*') => {
                                        if self.peek() == Some(&'/') {
                                            self.advance();
                                            break;
                                        }
                                    }
                                    Some(_) => continue,
                                    None => break,
                                }
                            }
                        }
                        _ => return,
                    }
                }
                _ => return,
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = match self.advance() {
            Some(c) => c,
            None => return Token::Eof,
        };

        match ch {
            '=' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::Eq
                } else if self.peek() == Some(&'>') {
                    self.advance();
                    Token::Arrow
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '+' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::PlusEq
                } else {
                    Token::Plus
                }
            }
            '-' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::MinusEq
                } else {
                    Token::Minus
                }
            }
            '*' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::StarEq
                } else {
                    Token::Star
                }
            }
            '/' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::SlashEq
                } else {
                    Token::Slash
                }
            }
            '&' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::BitAndEq
                } else if self.peek() == Some(&'&') {
                    self.advance();
                    Token::And
                } else {
                    Token::BitAnd
                }
            }
            '|' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::BitOrEq
                } else if self.peek() == Some(&'|') {
                    self.advance();
                    Token::Or
                } else {
                    Token::BitOr
                }
            }
            '^' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::BitXorEq
                } else {
                    Token::BitXor
                }
            }
            '<' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::Leq
                } else if self.peek() == Some(&'<') {
                    self.advance();
                    if self.peek() == Some(&'=') {
                        self.advance();
                        Token::BitLShiftEq
                    } else {
                        Token::ShiftLeft
                    }
                } else {
                    Token::Lt
                }
            }
            '>' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::Geq
                } else if self.peek() == Some(&'>') {
                    self.advance();
                    if self.peek() == Some(&'=') {
                        self.advance();
                        Token::BitRShiftEq
                    } else {
                        Token::ShiftRight
                    }
                } else {
                    Token::Gt
                }
            }
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ':' => {
                if self.peek() == Some(&':') {
                    self.advance();
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '.' => Token::Dot,

            '"' => self.read_string(),

            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(ch),
            '0'..='9' => self.read_number(ch),

            _ => Token::Illegal(ch.to_string()),
        }
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut literal = String::from(ch);

        while let Some(&ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                unsafe {
                    literal.push(self.advance().unwrap_unchecked());
                }
            } else {
                break;
            }
        }

        match literal.as_str() {
            "fn" => Token::Fn,
            "var" => Token::Var,
            "const" => Token::Const,
            "if" => Token::If,
            "else" => Token::Else,
            "import" => Token::Import,
            "while" => Token::While,
            "return" => Token::Return,
            "for" => Token::For,
            "in" => Token::In,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "as" => Token::As,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "match" => Token::Match,
            "default" => Token::Default,
            "self" => Token::SelfToken,
            "None" => Token::None,

            _ => Token::Identifier(literal),
        }
    }

    fn read_number(&mut self, ch: char) -> Token {
        let mut literal = String::from(ch);
        let mut dot = false;

        while let Some(&ch) = self.peek() {
            if ch.is_ascii_digit() {
                unsafe {
                    literal.push(self.advance().unwrap_unchecked());
                }
            } else if ch == '.' && !dot {
                dot = true;
                unsafe { literal.push(self.advance().unwrap_unchecked()) }
            } else {
                break;
            }
        }

        if dot {
            Token::Float(literal.parse::<f64>().unwrap_or(0.0))
        } else {
            Token::Int(literal.parse::<i64>().unwrap_or(0))
        }
    }

    fn read_string(&mut self) -> Token {
        let mut string_lit = String::new();

        while let Some(&ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return Token::StringLit(string_lit);
                }
                '\\' => {
                    self.advance();

                    match self.advance() {
                        Some('n') => string_lit.push('\n'),
                        Some('t') => string_lit.push('\t'),
                        Some('r') => string_lit.push('\r'),
                        Some('"') => string_lit.push('"'),
                        Some('\\') => string_lit.push('\\'),
                        Some(c) => {
                            string_lit.push('\\');
                            string_lit.push(c);
                        }
                        None => return Token::Illegal("Unterminated string escape".to_string()),
                    }
                }
                _ => unsafe { string_lit.push(self.advance().unwrap_unchecked()) },
            }
        }

        Token::Illegal("Unterminated String".to_string())
    }
}
