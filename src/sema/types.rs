#[derive(PartialEq)]
pub enum IntWidth {
    W8,
    W16,
    W32,
    W64,
    WSize,
}

#[derive(PartialEq)]
pub enum FloatWidth {
    W32,
    W64,
}

#[derive(PartialEq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(PartialEq)]
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
}
