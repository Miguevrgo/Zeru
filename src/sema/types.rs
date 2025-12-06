pub enum TypeWidth {
    W16,
    W32,
    W64,
    WSize,
}

pub enum Signedness {
    Signed,
    Unsigned,
}

pub enum Type {
    Integer {
        signed: Signedness,
        width: TypeWidth,
    },
    Float(TypeWidth),
    Bool,
    String,
    Void,
}
