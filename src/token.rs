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
    Int(u64),
    Float(f64),
    StringLit(Vec<u8>),

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
    PlusWrapEq,  // +%=
    MinusWrapEq, // -%=
    StarWrapEq,  // *%=

    // Wrapping arithmetic: the result taken modulo 2^bits, never a panic
    PlusWrap,  // +%
    MinusWrap, // -%
    StarWrap,  // *%

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
    DotDot,    // ..
    Question,  // ?

    // Custom
    SelfTok, // self
    Match,   // match
    Default, // default
    Enum,    // enum
    Arrow,   // =>
    Str,     // str type keyword
    Trait,   // trait keyword

    // Inline Assembly
    Asm,      // asm
    Volatile, // volatile

    // Special
    Eof,
    Illegal(String),
}

/// A token as it is written in the source, for error messages.
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let text = match self {
            Token::Identifier(name) => name,
            Token::Int(value) => return write!(f, "{value}"),
            Token::Float(value) => return write!(f, "{value}"),
            Token::StringLit(bytes) => return write!(f, "\"{}\"", String::from_utf8_lossy(bytes)),
            Token::Illegal(message) => message,
            Token::Eof => "end of file",
            Token::Fn => "fn",
            Token::Var => "var",
            Token::Const => "const",
            Token::If => "if",
            Token::Else => "else",
            Token::Import => "import",
            Token::While => "while",
            Token::Struct => "struct",
            Token::Return => "return",
            Token::For => "for",
            Token::In => "in",
            Token::None => "None",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::As => "as",
            Token::DoubleColon => "::",
            Token::And => "&&",
            Token::Or => "||",
            Token::True => "true",
            Token::False => "false",
            Token::ShiftLeft => "<<",
            Token::ShiftRight => ">>",
            Token::BitXor => "^",
            Token::BitAnd => "&",
            Token::BitOr => "|",
            Token::PlusEq => "+=",
            Token::MinusEq => "-=",
            Token::StarEq => "*=",
            Token::SlashEq => "/=",
            Token::ModEq => "%=",
            Token::BitXorEq => "^=",
            Token::BitAndEq => "&=",
            Token::BitOrEq => "|=",
            Token::BitRShiftEq => ">>=",
            Token::BitLShiftEq => "<<=",
            Token::PlusWrapEq => "+%=",
            Token::MinusWrapEq => "-%=",
            Token::StarWrapEq => "*%=",
            Token::PlusWrap => "+%",
            Token::MinusWrap => "-%",
            Token::StarWrap => "*%",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Mod => "%",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::Lt => "<",
            Token::Leq => "<=",
            Token::Gt => ">",
            Token::Geq => ">=",
            Token::Bang => "!",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::DotDot => "..",
            Token::Question => "?",
            Token::SelfTok => "self",
            Token::Match => "match",
            Token::Default => "default",
            Token::Enum => "enum",
            Token::Arrow => "=>",
            Token::Str => "str",
            Token::Trait => "trait",
            Token::Asm => "asm",
            Token::Volatile => "volatile",
        };
        f.write_str(text)
    }
}
