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

/// Represents all possible types in the Zeru type system.
///
/// Zeru's type system includes primitives (integers, floats, bool),
/// aggregate types (structs, tuples, arrays), pointers, and special
/// types like Optional for nullable values.
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer {
        signed: Signedness,
        width: IntWidth,
    },
    Float(FloatWidth),
    Bool,
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

    Pointer(Box<Type>),
    Tuple(Vec<Type>),
    Optional(Box<Type>),
    Result {
        ok_type: Box<Type>,
        err_type: Box<Type>,
    },
    Slice {
        elem_type: Box<Type>,
    },
    #[allow(clippy::enum_variant_names)]
    ParamType(String),
    Unknown,
}

impl Type {
    /// Checks if this type can accept a value of another type.
    ///
    /// This is used for type compatibility checking during semantic analysis.
    /// It's more permissive than strict equality to handle cases like:
    /// - Optional types accepting their inner type
    /// - Unknown types (used during inference)
    ///
    /// # Examples
    /// - `i32?.accepts(&i32)` → true (optional accepts inner type)
    /// - `*i32.accepts(&*i32)` → true (same pointer types)
    /// - `i32.accepts(&i64)` → false (different widths)
    pub fn accepts(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::ParamType(_), _) | (_, Type::ParamType(_)) => true,
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => n1 == n2,
            (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => n1 == n2,
            (Type::Pointer(e1), Type::Pointer(e2)) => e1.accepts(e2),
            (Type::Optional(e1), Type::Optional(e2)) => e1.accepts(e2),
            (Type::Optional(inner), other) => inner.accepts(other),
            (
                Type::Result {
                    ok_type: o1,
                    err_type: e1,
                },
                Type::Result {
                    ok_type: o2,
                    err_type: e2,
                },
            ) => o1.accepts(o2) && e1.accepts(e2),
            (Type::Slice { elem_type: e1 }, Type::Slice { elem_type: e2 }) => e1.accepts(e2),
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
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Array { elem_type, len } => write!(f, "[{}; {}]", elem_type, len),
            Type::Pointer(elem_type) => write!(f, "*{}", elem_type),
            Type::Optional(elem_type) => write!(f, "{}?", elem_type),
            Type::Result { ok_type, err_type } => write!(f, "Result<{}, {}>", ok_type, err_type),
            Type::Slice { elem_type } => write!(f, "&[{}]", elem_type),
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
            Type::ParamType(name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}
