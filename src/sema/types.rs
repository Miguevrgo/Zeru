#[derive(Debug, PartialEq, Clone)]
pub enum IntWidth {
    W8,
    W16,
    W32,
    W64,
    WSize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FloatWidth {
    W32,
    W64,
}

#[derive(Debug, PartialEq, Clone)]
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

    Pointer {
        elem_type: Box<Type>,
        is_mutable: bool,
    },

    Unknown,
}

impl Type {
    pub fn from_string(name: &str) -> Option<Type> {
        match name {
            "i8" => Some(Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W8,
            }),
            "u8" => Some(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W8,
            }),
            "i16" => Some(Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W16,
            }),
            "i32" => Some(Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W32,
            }),
            "i64" => Some(Type::Integer {
                signed: Signedness::Signed,
                width: IntWidth::W64,
            }),
            "u16" => Some(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W16,
            }),
            "u32" => Some(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W32,
            }),
            "u64" => Some(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W64,
            }),
            "usize" => Some(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::WSize,
            }),

            "f32" => Some(Type::Float(FloatWidth::W32)),
            "f64" => Some(Type::Float(FloatWidth::W64)),

            "bool" => Some(Type::Bool),
            "String" => Some(Type::String),
            "void" => Some(Type::Void),

            _ => None,
        }
    }

    pub fn accepts(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (
                Type::Pointer {
                    elem_type: e1,
                    is_mutable: false,
                },
                Type::Pointer {
                    elem_type: e2,
                    is_mutable: _,
                },
            ) => e1 == e2,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Integer { signed, width } => {
                let sign_str = match signed {
                    Signedness::Signed => "i",
                    Signedness::Unsigned => "u",
                };
                let width_str = match width {
                    IntWidth::W8 => "8",
                    IntWidth::W16 => "16",
                    IntWidth::W32 => "32",
                    IntWidth::W64 => "64",
                    IntWidth::WSize => "size",
                };
                format!("{}{}", sign_str, width_str)
            }
            Type::Float(width) => match width {
                FloatWidth::W32 => "f32".to_string(),
                FloatWidth::W64 => "f64".to_string(),
            },
            Type::Bool => "bool".to_string(),
            Type::String => "String".to_string(),
            Type::Void => "void".to_string(),

            Type::Struct { name, .. } => name.clone(),
            Type::Enum { name, .. } => name.clone(),

            Type::Array { elem_type, len } => format!("[{}; {}]", elem_type.to_string(), len),
            Type::Pointer {
                elem_type,
                is_mutable,
            } => {
                let mut_str = if *is_mutable { "mut " } else { "" };
                format!("&{}{}", mut_str, elem_type.to_string())
            }

            Type::Unknown => "<unknown>".to_string(),
        }
    }
}
