use std::{iter::Peekable, str::Chars};

use crate::errors::Span;
use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            line: 1,
            pos: 0,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.input.next();
        if let Some(c) = ch {
            self.pos += c.len_utf8();
            if c == '\n' {
                self.line += 1;
            }
        }
        ch
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

    fn operator_or_assign(&mut self, simple: Token, compound: Token) -> Token {
        if self.peek() == Some(&'=') {
            self.advance();
            compound
        } else {
            simple
        }
    }

    pub fn next_token(&mut self) -> (Token, usize, Span) {
        self.skip_whitespace();

        let start_line = self.line;
        let start_pos = self.pos;

        let ch = match self.advance() {
            Some(c) => c,
            None => return (Token::Eof, start_line, Span::new(start_pos, self.pos)),
        };

        let token = match ch {
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
            '!' => self.operator_or_assign(Token::Bang, Token::NotEq),
            '+' => self.operator_or_assign(Token::Plus, Token::PlusEq),
            '-' => self.operator_or_assign(Token::Minus, Token::MinusEq),
            '*' => self.operator_or_assign(Token::Star, Token::StarEq),
            '/' => self.operator_or_assign(Token::Slash, Token::SlashEq),
            '%' => self.operator_or_assign(Token::Mod, Token::ModEq),
            '^' => self.operator_or_assign(Token::BitXor, Token::BitXorEq),
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
            '?' => Token::Question,

            '"' => self.read_string(),

            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(ch),
            '0'..='9' => self.read_number(ch),

            _ => Token::Illegal(ch.to_string()),
        };
        (token, start_line, Span::new(start_pos, self.pos))
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut literal = String::from(ch);

        while let Some(&ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                literal.push(self.advance().unwrap());
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
            "str" => Token::Str,
            "match" => Token::Match,
            "default" => Token::Default,
            "self" => Token::SelfTok,
            "true" => Token::True,
            "false" => Token::False,
            "asm" => Token::Asm,
            "volatile" => Token::Volatile,
            "None" => Token::None,

            _ => Token::Identifier(literal),
        }
    }

    fn read_digits(&mut self, predicate: fn(char) -> bool) -> String {
        let mut digits = String::new();
        while let Some(&ch) = self.peek() {
            if predicate(ch) {
                digits.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        digits
    }

    fn read_number(&mut self, ch: char) -> Token {
        if ch == '0'
            && let Some(&ch) = self.peek()
        {
            let (radix, predicate): (u32, fn(char) -> bool) = match ch {
                'x' => (16, |c| c.is_ascii_hexdigit()),
                'b' => (2, |c| c == '0' || c == '1'),
                'o' => (8, |c| matches!(c, '0'..='7')),
                _ => (0, |_| false),
            };
            if radix != 0 {
                self.advance();
                let digits = self.read_digits(predicate);
                return Token::Int(i64::from_str_radix(&digits, radix).unwrap_or(0));
            }
        }

        let mut literal = String::from(ch);
        let mut dot = false;

        while let Some(&ch) = self.peek() {
            if ch.is_ascii_digit() {
                literal.push(self.advance().unwrap());
            } else if ch == '.' && !dot {
                dot = true;
                literal.push(self.advance().unwrap())
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
        let mut bytes = Vec::new();

        while let Some(&ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return Token::StringLit(bytes);
                }
                '\\' => {
                    self.advance();

                    match self.advance() {
                        Some('n') => bytes.push(b'\n'),
                        Some('t') => bytes.push(b'\t'),
                        Some('r') => bytes.push(b'\r'),
                        Some('"') => bytes.push(b'"'),
                        Some('\\') => bytes.push(b'\\'),
                        Some(c) => {
                            bytes.push(b'\\');
                            bytes.push(c as u8);
                        }
                        None => return Token::Illegal("Unterminated string escape".to_string()),
                    }
                }
                _ if !ch.is_ascii() => {
                    return Token::Illegal("Non-ASCII character in string".to_string());
                }
                _ => bytes.push(self.advance().unwrap() as u8),
            }
        }

        Token::Illegal("Unterminated String".to_string())
    }
    //TODO: Read_char
}
