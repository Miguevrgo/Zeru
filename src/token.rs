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
    Break,
    Continue,
    As,
    DoubleColon,

    // Literals
    Identifier(String),
    Int(i64),
    Float(f64),
    StringLit(String),

    // Logic
    And,   // &&
    Or,    // ||
    True,  // true
    False, // false

    // Bitwise
    ShiftLeft,  // <<
    ShiftRight, // >>
    BitXor,     // ^
    BitAnd,     // &
    BitOr,      // |

    // Compound Assign
    PlusEq,      // +=
    MinusEq,     // -=
    StarEq,      // *=
    SlashEq,     // /=
    ModEq,       // %=
    BitXorEq,    // ^=
    BitAndEq,    // &=
    BitOrEq,     // |=
    BitRShiftEq, // >>=
    BitLShiftEq, // <<=

    // Single-Character & Double tokens
    Assign, // =
    Plus,   // +
    Minus,  // -
    Star,   // *
    Slash,  // /
    Mod,    // %
    Eq,     // ==
    NotEq,  // !=
    Lt,     // <
    Leq,    // <=
    Gt,     // >
    Geq,    // >=
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
    Question,  // ?

    // Custom
    SelfTok, // self
    Match,   // match
    Default, // default
    Enum,    // enum
    Arrow,   // =>

    // Inline Assembly
    Asm,      // asm
    Volatile, // volatile

    // Special
    Eof,
    Illegal(String),
}
