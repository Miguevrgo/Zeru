use std::{iter::Peekable, num::IntErrorKind, str::Chars};

use crate::errors::Span;
use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    pos: usize,
    after_dot: bool,
    pub comments: Vec<Span>,
}

impl<'a> Lexer<'a> {
    /// Lex a slice of a larger buffer, reporting spans as offsets into that
    /// buffer, so each file can be lexed on its own and still be located in
    /// the whole program.
    pub fn at(input: &'a str, start: usize) -> Self {
        Self {
            input: input.chars().peekable(),
            pos: start,
            after_dot: false,
            comments: Vec::new(),
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.input.next();
        if let Some(c) = ch {
            self.pos += c.len_utf8();
        }
        ch
    }

    /// Skip whitespace and comments. `Err` carries where a block comment that
    /// never closes began.
    fn skip_whitespace(&mut self) -> Result<(), usize> {
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
                            let start = self.pos;
                            self.advance();
                            self.advance();

                            while let Some(&c) = self.peek() {
                                if c == '\n' {
                                    break;
                                }
                                self.advance();
                            }
                            self.comments.push(Span::new(start, self.pos));
                        }
                        Some('*') => {
                            let start = self.pos;
                            self.advance();
                            self.advance();

                            loop {
                                match self.advance() {
                                    Some('*') if self.peek() == Some(&'/') => {
                                        self.advance();
                                        self.comments.push(Span::new(start, self.pos));
                                        break;
                                    }
                                    Some(_) => {}
                                    None => return Err(start),
                                }
                            }
                        }
                        _ => return Ok(()),
                    }
                }
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    fn match_next(&mut self, expected: char, if_match: Token, default: Token) -> Token {
        if self.peek() == Some(&expected) {
            self.advance();
            if_match
        } else {
            default
        }
    }

    pub fn next_token(&mut self) -> (Token, Span) {
        if let Err(start) = self.skip_whitespace() {
            let message = "Unterminated block comment".to_string();
            return (Token::Illegal(message), Span::new(start, start + 2));
        }

        let start_pos = self.pos;

        let Some(ch) = self.advance() else {
            return (Token::Eof, Span::new(start_pos, start_pos + 1));
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
            '!' => self.match_next('=', Token::NotEq, Token::Bang),
            '+' if self.peek() == Some(&'%') => {
                self.advance();
                self.match_next('=', Token::PlusWrapEq, Token::PlusWrap)
            }
            '-' if self.peek() == Some(&'%') => {
                self.advance();
                self.match_next('=', Token::MinusWrapEq, Token::MinusWrap)
            }
            '*' if self.peek() == Some(&'%') => {
                self.advance();
                self.match_next('=', Token::StarWrapEq, Token::StarWrap)
            }
            '+' => self.match_next('=', Token::PlusEq, Token::Plus),
            '-' => self.match_next('=', Token::MinusEq, Token::Minus),
            '*' => self.match_next('=', Token::StarEq, Token::Star),
            '/' => self.match_next('=', Token::SlashEq, Token::Slash),
            '%' => self.match_next('=', Token::ModEq, Token::Mod),
            '^' => self.match_next('=', Token::BitXorEq, Token::BitXor),
            '&' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::BitAndEq
                } else {
                    self.match_next('&', Token::And, Token::BitAnd)
                }
            }
            '|' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::BitOrEq
                } else {
                    self.match_next('|', Token::Or, Token::BitOr)
                }
            }
            '<' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    Token::Leq
                } else if self.peek() == Some(&'<') {
                    self.advance();
                    self.match_next('=', Token::BitLShiftEq, Token::ShiftLeft)
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
                    self.match_next('=', Token::BitRShiftEq, Token::ShiftRight)
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
            ':' => self.match_next(':', Token::DoubleColon, Token::Colon),
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '.' => match self.match_next('.', Token::DotDot, Token::Dot) {
                Token::DotDot => self.match_next('=', Token::DotDotEq, Token::DotDot),
                dot => dot,
            },
            '?' => Token::Question,
            '#' => match self.read_identifier(ch) {
                Token::Identifier(word) => {
                    Token::Illegal(format!("Unexpected '{word}', expected '#test'"))
                }
                tests => tests,
            },

            '"' => self.read_string(),
            '\'' => self.read_char(),
            '`' => self.read_raw_string(),

            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(ch),
            '0'..='9' if self.after_dot => {
                let digits = format!("{ch}{}", self.read_digits(|c| c.is_ascii_digit()));
                Self::int_token(&digits, 10)
            }
            '0'..='9' => self.read_number(ch),

            _ => Token::Illegal(format!("Unexpected character '{ch}'")),
        };
        self.after_dot = token == Token::Dot;
        (token, Span::new(start_pos, self.pos))
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut literal = String::from(ch);

        while let Some(&ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
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
            "pub" => Token::Pub,
            "#test" => Token::Tests,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "trait" => Token::Trait,
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

    /// Digits for which `is_digit` holds, skipping the `_` that may separate
    /// them, as in `1_000_000`.
    fn read_digits(&mut self, is_digit: fn(char) -> bool) -> String {
        let mut digits = String::new();
        while let Some(&ch) = self.peek() {
            if is_digit(ch) {
                digits.push(ch);
            } else if ch != '_' {
                break;
            }
            self.advance();
        }
        digits
    }

    /// Whether the character after the next one satisfies `is`: `1.5` is a
    /// float, while `0..n` and `t.0.x` are not.
    fn second_is(&self, is: fn(char) -> bool) -> bool {
        let mut ahead = self.input.clone();
        ahead.next();
        ahead.peek().is_some_and(|&c| is(c))
    }

    fn read_number(&mut self, first: char) -> Token {
        if first == '0'
            && let Some(&ch) = self.peek()
        {
            let radix = match ch {
                'x' => 16,
                'b' => 2,
                'o' => 8,
                _ => 0,
            };
            if radix != 0 {
                self.advance();
                let digits = self.read_digits(|c| c.is_ascii_alphanumeric());
                return Self::int_token(&digits, radix);
            }
        }

        let decimal = |c: char| c.is_ascii_digit();
        let mut literal = format!("{first}{}", self.read_digits(decimal));
        let mut float = false;
        if self.peek() == Some(&'.') && self.second_is(decimal) {
            self.advance();
            literal = format!("{literal}.{}", self.read_digits(decimal));
            float = true;
        }
        if matches!(self.peek(), Some('e' | 'E'))
            && self.second_is(|c| c.is_ascii_digit() || c == '-' || c == '+')
        {
            self.advance();
            literal.push('e');
            if let Some(&sign @ ('-' | '+')) = self.peek() {
                self.advance();
                literal.push(sign);
            }
            literal += &self.read_digits(decimal);
            float = true;
        }

        if !float {
            return Self::int_token(&literal, 10);
        }
        match literal.parse::<f64>() {
            Ok(value) if value.is_finite() => Token::Float(value),
            Ok(_) => Token::Illegal(format!("Float literal '{literal}' is out of range")),
            Err(_) => Token::Illegal(format!("Malformed number '{literal}'")),
        }
    }

    /// An integer literal carries its magnitude: a minus sign is an operator,
    /// and the analyser decides whether the value suits its type.
    fn int_token(digits: &str, radix: u32) -> Token {
        match u64::from_str_radix(digits, radix) {
            Ok(value) => Token::Int(value),
            Err(e) if *e.kind() == IntErrorKind::PosOverflow => {
                Token::Illegal(format!("Integer literal '{digits}' is out of range"))
            }
            Err(_) if digits.is_empty() => {
                Token::Illegal(format!("Base {radix} literal has no digits"))
            }
            Err(_) => Token::Illegal(format!("Invalid digit in base {radix} literal '{digits}'")),
        }
    }

    fn read_string(&mut self) -> Token {
        let mut bytes = Vec::new();
        let mut problem = None;

        while let Some(&ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return match problem {
                        Some(message) => Token::Illegal(message),
                        None => Token::StringLit(bytes),
                    };
                }
                '\\' => {
                    self.advance();
                    match self.read_escape() {
                        Ok(byte) => bytes.push(byte),
                        Err(message) => {
                            problem.get_or_insert(message);
                        }
                    }
                }
                _ => {
                    self.advance();
                    bytes.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
                }
            }
        }

        Token::Illegal("Unterminated String".to_string())
    }

    fn read_escape(&mut self) -> Result<u8, String> {
        Ok(match self.advance() {
            Some('n') => b'\n',
            Some('t') => b'\t',
            Some('r') => b'\r',
            Some('0') => 0,
            Some('"') => b'"',
            Some('\'') => b'\'',
            Some('\\') => b'\\',
            Some('x') => {
                let mut value = 0;
                for _ in 0..2 {
                    let Some(digit) = self.peek().and_then(|c| c.to_digit(16)) else {
                        return Err("'\\x' takes two hex digits, as in '\\x41'".to_string());
                    };
                    self.advance();
                    value = value * 16 + digit;
                }
                if value > 0x7F {
                    return Err(format!("'\\x{value:02X}' is not ASCII"));
                }
                value as u8
            }
            Some(c) => {
                return Err(format!(
                    "Unknown escape sequence '\\{c}', expected one of \\n \\t \\r \\0 \\x \\' \\\" \\\\"
                ));
            }
            None => return Err("Unterminated escape".to_string()),
        })
    }

    /// `'a'` or `'\n'`: the value of one ASCII character, typed like any
    /// other integer literal.
    fn read_char(&mut self) -> Token {
        let value = match self.advance() {
            Some('\\') => self.read_escape(),
            Some('\'') => return Token::Illegal("Empty character literal".to_string()),
            None => return Token::Illegal("Unterminated character literal".to_string()),
            Some(c) if c.is_ascii() => Ok(c as u8),
            Some(_) => Err("Non-ASCII character literal".to_string()),
        };
        if self.peek() != Some(&'\'') {
            while let Some(c) = self.peek().copied().filter(|&c| c != '\n') {
                self.advance();
                if c == '\'' {
                    return Token::Illegal("A character literal holds one character".to_string());
                }
            }
            return Token::Illegal("Unterminated character literal".to_string());
        }
        self.advance();
        value.map_or_else(Token::Illegal, |byte| Token::Int(u64::from(byte)))
    }

    fn read_raw_string(&mut self) -> Token {
        let mut bytes = Vec::new();

        while let Some(&ch) = self.peek() {
            self.advance();
            if ch == '`' {
                return Token::StringLit(bytes);
            }
            bytes.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes());
        }

        Token::Illegal("Unterminated raw string".to_string())
    }
}
