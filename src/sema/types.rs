#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntWidth {
    W8,
    W16,
    W32,
    W64,
    WSize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FloatWidth {
    W32,
    W64,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer {
        signed: Signedness,
        width: IntWidth,
    },
    Float(FloatWidth),
    Bool,
    String,
    Void,

    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },

    Enum {
        name: String,
        variants: Vec<String>,
    },

    Array {
        elem_type: Box<Type>,
        len: usize,
    },

    /// TODO: Pointer type for v0.2.0 | will support '*' and '&'
    #[allow(dead_code)]
    Pointer {
        elem_type: Box<Type>,
        is_mutable: bool,
    },

    Unknown,
}

impl Type {
    pub fn accepts(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (
                Type::Pointer {
                    elem_type: e1,
                    is_mutable: false,
                },
                Type::Pointer {
                    elem_type: e2,
                    is_mutable: _,
                },
            ) => e1.accepts(e2),
            (
                Type::Array {
                    elem_type: e1,
                    len: l1,
                },
                Type::Array {
                    elem_type: e2,
                    len: l2,
                },
            ) => l1 == l2 && e1.accepts(e2),
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Integer { signed, width } => {
                let s = match signed {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                };
                let w = match width {
                    IntWidth::W8 => "8",
                    IntWidth::W16 => "16",
                    IntWidth::W32 => "32",
                    IntWidth::W64 => "64",
                    IntWidth::WSize => "size",
                };
                write!(f, "{}{}", s, w)
            }
            Type::Float(w) => write!(
                f,
                "f{}",
                match w {
                    FloatWidth::W32 => "32",
                    FloatWidth::W64 => "64",
                }
            ),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "String"),
            Type::Void => write!(f, "void"),
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Array { elem_type, len } => write!(f, "[{}; {}]", elem_type, len),
            Type::Pointer {
                elem_type,
                is_mutable,
            } => {
                write!(f, "&{}{}", if *is_mutable { "mut " } else { "" }, elem_type)
            }
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}
