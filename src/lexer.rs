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
            '%' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::ModEq
                } else {
                    Token::Mod
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

    fn read_number(&mut self, ch: char) -> Token {
        let mut literal = String::from(ch);
        let mut dot = false;

        if ch == '0'
            && let Some(&ch) = self.peek()
        {
            match ch {
                'x' => {
                    literal.clear();
                    self.advance().unwrap();
                    while let Some(&n_ch) = self.peek() {
                        if n_ch.is_ascii_hexdigit() {
                            literal.push(self.advance().unwrap());
                        } else {
                            break;
                        }
                    }
                    return Token::Int(i64::from_str_radix(&literal, 16).unwrap_or(0));
                }
                'b' => {
                    literal.clear();
                    self.advance().unwrap();
                    while let Some(&n_ch) = self.peek() {
                        if n_ch == '0' || n_ch == '1' {
                            literal.push(self.advance().unwrap());
                        } else {
                            break;
                        }
                    }

                    return Token::Int(i64::from_str_radix(&literal, 2).unwrap_or(0));
                }
                'o' => {
                    literal.clear();
                    self.advance().unwrap();
                    while let Some(&n_ch) = self.peek() {
                        if n_ch as u8 >= b'0' && n_ch as u8 <= b'7' {
                            literal.push(self.advance().unwrap());
                        } else {
                            break;
                        }
                    }
                    return Token::Int(i64::from_str_radix(&literal, 8).unwrap_or(0));
                }
                _ => {}
            }
        }

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
    //TODO: Read_char
}
