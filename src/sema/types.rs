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
    Void,
    Never, // noreturn type - for functions like __exit that never return

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

    Pointer(Box<Type>),
    Tuple(Vec<Type>),
    Optional(Box<Type>),
    Unknown,
}

impl Type {
    pub fn accepts(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            // Never type is compatible with anything (it never returns, so any type is fine)
            (_, Type::Never) => true,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Pointer(e1), Type::Pointer(e2)) => e1.accepts(e2),
            (Type::Optional(e1), Type::Optional(e2)) => e1.accepts(e2),
            (Type::Optional(inner), other) => inner.accepts(other),
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(a, b)| a.accepts(b))
            }

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
            Type::Void => write!(f, "void"),
            Type::Never => write!(f, "!"),
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Array { elem_type, len } => write!(f, "[{}; {}]", elem_type, len),
            Type::Pointer(elem_type) => write!(f, "*{}", elem_type),
            Type::Optional(elem_type) => write!(f, "{}?", elem_type),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}
