use std::{iter::Peekable, str::Chars};

use crate::errors::Span;
use crate::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    pos: usize,
    /// The last token was a `.`, so a number is a tuple index: `t.1.0` is
    /// two of them, not `t` and the float `1.0`.
    after_dot: bool,
}

impl<'a> Lexer<'a> {
    #[cfg(test)]
    pub fn new(input: &'a str) -> Self {
        Self::at(input, 0)
    }

    /// Lex a slice of a larger buffer, reporting spans as offsets into that
    /// buffer, so each file can be lexed on its own and still be located in
    /// the whole program.
    pub fn at(input: &'a str, start: usize) -> Self {
        Self {
            input: input.chars().peekable(),
            pos: start,
            after_dot: false,
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
                            let start = self.pos;
                            self.advance();
                            self.advance();

                            loop {
                                match self.advance() {
                                    Some('*') if self.peek() == Some(&'/') => {
                                        self.advance();
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

        let ch = match self.advance() {
            Some(c) => c,
            None => return (Token::Eof, Span::new(start_pos, self.pos)),
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
            '.' => Token::Dot,
            '?' => Token::Question,

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
            let (radix, predicate): (u32, fn(char) -> bool) = match ch {
                'x' => (16, |c| c.is_ascii_hexdigit()),
                'b' => (2, |c| c == '0' || c == '1'),
                'o' => (8, |c| matches!(c, '0'..='7')),
                _ => (0, |_| false),
            };
            if radix != 0 {
                self.advance();
                let digits = self.read_digits(predicate);
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
        literal.parse().map_or_else(
            |_| Token::Illegal(format!("Malformed number '{literal}'")),
            Token::Float,
        )
    }

    /// An integer literal carries its magnitude: a minus sign is an operator,
    /// and the analyser decides whether the value suits its type.
    fn int_token(digits: &str, radix: u32) -> Token {
        match u64::from_str_radix(digits, radix) {
            Ok(value) => Token::Int(value),
            Err(_) => Token::Illegal(format!("Integer literal '{digits}' is out of range")),
        }
    }

    fn read_string(&mut self) -> Token {
        let mut bytes = Vec::new();
        // Reported once the closing quote is reached, so the rest of the
        // string is not read as code.
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
                _ if !ch.is_ascii() => {
                    self.advance();
                    problem.get_or_insert("Non-ASCII character in string".to_string());
                }
                _ => bytes.push(self.advance().unwrap() as u8),
            }
        }

        Token::Illegal("Unterminated String".to_string())
    }

    /// The byte an escape stands for, its `\\` already read.
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

    /// `'a'` or `'\\n'`: the value of one ASCII character, typed like any
    /// other integer literal.
    fn read_char(&mut self) -> Token {
        let value = match self.advance() {
            Some('\\') => self.read_escape(),
            Some('\'') | None => Err("Empty character literal".to_string()),
            Some(c) if c.is_ascii() => Ok(c as u8),
            Some(_) => Err("Non-ASCII character literal".to_string()),
        };
        if self.peek() != Some(&'\'') {
            return Token::Illegal("A character literal holds one character".to_string());
        }
        self.advance();
        value.map_or_else(Token::Illegal, |byte| Token::Int(u64::from(byte)))
    }

    fn read_raw_string(&mut self) -> Token {
        let mut bytes = Vec::new();
        let mut non_ascii = false;

        while let Some(&ch) = self.peek() {
            match ch {
                '`' => {
                    self.advance();
                    if non_ascii {
                        return Token::Illegal("Non-ASCII character in raw string".to_string());
                    }
                    return Token::StringLit(bytes);
                }
                _ if !ch.is_ascii() => {
                    self.advance();
                    non_ascii = true;
                }
                _ => bytes.push(self.advance().unwrap() as u8),
            }
        }

        Token::Illegal("Unterminated raw string".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_string_simple() {
        let mut lexer = Lexer::new("`hello world`");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "hello world");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_raw_string_with_backslashes() {
        let mut lexer = Lexer::new("`C:\\Users\\file.txt`");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "C:\\Users\\file.txt");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_raw_string_with_quotes() {
        let mut lexer = Lexer::new("`{\"name\": \"value\"}`");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "{\"name\": \"value\"}");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_raw_string_with_special_chars() {
        let mut lexer = Lexer::new("`\\d+\\.\\d+`");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "\\d+\\.\\d+");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_raw_string_multiline() {
        let mut lexer = Lexer::new("`Line 1\nLine 2\nLine 3`");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "Line 1\nLine 2\nLine 3");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_raw_string_empty() {
        let mut lexer = Lexer::new("``");
        let (token, _) = lexer.next_token();
        match token {
            Token::StringLit(bytes) => {
                assert_eq!(String::from_utf8(bytes).unwrap(), "");
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_unterminated_block_comment_is_reported() {
        let mut lexer = Lexer::new("fn /* never closed");
        assert_eq!(lexer.next_token().0, Token::Fn);
        let (token, span) = lexer.next_token();
        assert_eq!(
            token,
            Token::Illegal("Unterminated block comment".to_string())
        );
        assert_eq!(span, Span::new(3, 5));
        assert_eq!(lexer.next_token().0, Token::Eof);
    }

    #[test]
    fn test_unknown_escape_is_reported_after_the_whole_string() {
        // The string is read to its end, so `x` after it is still a name.
        let mut lexer = Lexer::new(r#""a\qb" x"#);
        let Token::Illegal(message) = lexer.next_token().0 else {
            panic!("Expected an Illegal token");
        };
        assert!(message.contains(r"'\q'"), "{message}");
        assert_eq!(lexer.next_token().0, Token::Identifier("x".to_string()));
    }

    #[test]
    fn test_non_ascii_is_reported_after_the_whole_string() {
        for source in ["\"h\u{e9}llo\" x", "`h\u{e9}llo` x"] {
            let mut lexer = Lexer::new(source);
            assert!(matches!(lexer.next_token().0, Token::Illegal(_)));
            assert_eq!(lexer.next_token().0, Token::Identifier("x".to_string()));
        }
    }

    #[test]
    fn test_a_number_after_a_dot_is_a_tuple_index() {
        let mut lexer = Lexer::new("t.1.0 1.5");
        let tokens: Vec<Token> = std::iter::from_fn(|| match lexer.next_token().0 {
            Token::Eof => None,
            token => Some(token),
        })
        .collect();
        assert_eq!(
            tokens,
            [
                Token::Identifier("t".to_string()),
                Token::Dot,
                Token::Int(1),
                Token::Dot,
                Token::Int(0),
                Token::Float(1.5),
            ]
        );
    }

    #[test]
    fn test_numbers_and_characters() {
        let mut lexer = Lexer::new("1_000 1e9 2.5E-3 0x_ff 'a' '\\x41' '\\0' 0..n");
        let tokens: Vec<Token> = std::iter::from_fn(|| match lexer.next_token().0 {
            Token::Eof => None,
            token => Some(token),
        })
        .collect();
        assert_eq!(
            tokens[..8],
            [
                Token::Int(1000),
                Token::Float(1e9),
                Token::Float(2.5e-3),
                Token::Int(255),
                Token::Int(97),
                Token::Int(65),
                Token::Int(0),
                Token::Int(0),
            ]
        );
        for bad in ["'ab'", "''", "'\\x80'", "\"\\xZ1\""] {
            assert!(
                matches!(Lexer::new(bad).next_token().0, Token::Illegal(_)),
                "{bad}"
            );
        }
    }

    #[test]
    fn test_raw_string_unterminated() {
        let mut lexer = Lexer::new("`unterminated");
        let (token, _) = lexer.next_token();
        match token {
            Token::Illegal(msg) => {
                assert_eq!(msg, "Unterminated raw string");
            }
            _ => panic!("Expected Illegal token for unterminated string"),
        }
    }
}
