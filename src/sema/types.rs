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
        if let Some(start_angle) = name.find('<')
            && name.ends_with('>')
        {
            let base_name = &name[0..start_angle];
            let content = &name[start_angle + 1..name.len() - 1]; // "i32, 5"

            let args: Vec<&str> = content.split(',').map(|s| s.trim()).collect();

            if base_name == "Array" && args.len() == 2 {
                let type_part = args[0];
                let len_part = args[1];

                if let Ok(len) = len_part.parse::<usize>()
                    && let Some(elem_ty) = Type::from_string(type_part)
                {
                    return Some(Type::Array {
                        elem_type: Box::new(elem_ty),
                        len,
                    });
                }
            }
        }
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
