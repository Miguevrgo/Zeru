#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Fn,
    Var,
    Const,
    If,
    Else,
    Import,
    While,
    Struct,
    Return,
    For,
    In,
    None,

    // Literals
    Identifier(String),
    Int(i64),
    Float(f64),
    StringLit(String),

    // Single-Character & Double tokens
    Assign, // =
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Eq,     // ==
    NotEq,  // !=
    Lt,     // <
    Gt,     // >
    Bang,   // !

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Colon,     // :
    Semicolon, // ;
    Comma,     // ,
    Dot,       // .

    // Special
    Eof,
    Illegal(String),
}
