//! Interned types. Every type is a [`Ty`], a small index into the [`Types`]
//! table, so comparing or copying one is free and a generic instance is a
//! substitution over indices instead of a copy of the source.

use std::collections::HashMap;

use crate::items::DefId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ty(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl IntTy {
    pub fn signed(self) -> bool {
        matches!(
            self,
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize
        )
    }

    pub fn bits(self) -> u32 {
        match self {
            Self::I8 | Self::U8 => 8,
            Self::I16 | Self::U16 => 16,
            Self::I32 | Self::U32 => 32,
            Self::I64 | Self::U64 | Self::Isize | Self::Usize => 64,
        }
    }

    pub fn min(self) -> i128 {
        if self.signed() {
            -(1 << (self.bits() - 1))
        } else {
            0
        }
    }

    pub fn max(self) -> i128 {
        if self.signed() {
            (1 << (self.bits() - 1)) - 1
        } else {
            (1 << self.bits()) - 1
        }
    }

    pub fn contains(self, value: i128) -> bool {
        (self.min()..=self.max()).contains(&value)
    }

    const ALL: [(IntTy, &'static str); 10] = [
        (Self::I8, "i8"),
        (Self::I16, "i16"),
        (Self::I32, "i32"),
        (Self::I64, "i64"),
        (Self::Isize, "isize"),
        (Self::U8, "u8"),
        (Self::U16, "u16"),
        (Self::U32, "u32"),
        (Self::U64, "u64"),
        (Self::Usize, "usize"),
    ];

    fn name(self) -> &'static str {
        Self::ALL.iter().find(|(ty, _)| *ty == self).unwrap().1
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    Int(IntTy),
    F32,
    F64,
    Bool,
    Void,
    /// A struct or an enum, with the arguments of its type parameters.
    Adt(DefId, Vec<Ty>),
    /// The type parameter at this position of the item being checked.
    Param(u32),
    Array(Ty, u64),
    Vec(Ty),
    Slice(Ty),
    Ptr(Ty),
    Ref(Ty),
    RefMut(Ty),
    Optional(Ty),
    /// `T!E`; a plain `T!` fails with an i32.
    Result(Ty, Ty),
    Tuple(Vec<Ty>),
    /// What an expression that already failed to check has. Every check
    /// passes it silently, so one mistake is reported once.
    Error,
}

pub struct Types {
    kinds: Vec<TyKind>,
    ids: HashMap<TyKind, Ty>,
    names: HashMap<DefId, String>,
}

impl Types {
    pub const ERROR: Ty = Ty(0);
    pub const VOID: Ty = Ty(1);
    pub const BOOL: Ty = Ty(2);
    pub const F32: Ty = Ty(3);
    pub const F64: Ty = Ty(4);
    pub const I32: Ty = Ty(7);
    pub const I64: Ty = Ty(8);
    pub const U8: Ty = Ty(10);
    pub const USIZE: Ty = Ty(14);
    pub const STR: Ty = Ty(15);

    pub fn new() -> Self {
        let mut types = Self {
            kinds: Vec::new(),
            ids: HashMap::new(),
            names: HashMap::new(),
        };
        for kind in [
            TyKind::Error,
            TyKind::Void,
            TyKind::Bool,
            TyKind::F32,
            TyKind::F64,
        ] {
            types.intern(kind);
        }
        for (int, _) in IntTy::ALL {
            types.intern(TyKind::Int(int));
        }
        types.intern(TyKind::Slice(Self::U8));
        types
    }

    pub fn intern(&mut self, kind: TyKind) -> Ty {
        if let Some(&ty) = self.ids.get(&kind) {
            return ty;
        }
        let ty = Ty(self.kinds.len() as u32);
        self.kinds.push(kind.clone());
        self.ids.insert(kind, ty);
        ty
    }

    pub fn len(&self) -> usize {
        self.kinds.len()
    }

    pub fn all(&self) -> impl Iterator<Item = Ty> + use<> {
        (0..self.kinds.len() as u32).map(Ty)
    }

    pub fn kind(&self, ty: Ty) -> &TyKind {
        &self.kinds[ty.0 as usize]
    }

    pub fn int(&mut self, int: IntTy) -> Ty {
        self.intern(TyKind::Int(int))
    }

    pub fn primitive(&mut self, name: &str) -> Option<Ty> {
        if let Some((int, _)) = IntTy::ALL.iter().find(|(_, n)| *n == name) {
            return Some(self.int(*int));
        }
        Some(match name {
            "f32" => Self::F32,
            "f64" => Self::F64,
            "bool" => Self::BOOL,
            "void" => Self::VOID,
            "str" => Self::STR,
            _ => return None,
        })
    }

    pub fn name_adt(&mut self, def: DefId, name: String) {
        self.names.insert(def, name);
    }

    pub fn adt_name(&self, def: DefId) -> &str {
        self.names.get(&def).map_or("?", String::as_str)
    }

    pub fn is_error(&self, ty: Ty) -> bool {
        ty == Self::ERROR
    }

    pub fn as_int(&self, ty: Ty) -> Option<IntTy> {
        match self.kind(ty) {
            TyKind::Int(int) => Some(*int),
            _ => None,
        }
    }

    pub fn is_float(&self, ty: Ty) -> bool {
        matches!(self.kind(ty), TyKind::F32 | TyKind::F64)
    }

    pub fn is_number(&self, ty: Ty) -> bool {
        self.as_int(ty).is_some() || self.is_float(ty)
    }

    pub fn contains_param(&self, ty: Ty) -> bool {
        match self.kind(ty) {
            TyKind::Param(_) => true,
            TyKind::Adt(_, args) | TyKind::Tuple(args) => {
                args.iter().any(|&arg| self.contains_param(arg))
            }
            TyKind::Result(ok, err) => self.contains_param(*ok) || self.contains_param(*err),
            TyKind::Array(inner, _)
            | TyKind::Vec(inner)
            | TyKind::Slice(inner)
            | TyKind::Ptr(inner)
            | TyKind::Ref(inner)
            | TyKind::RefMut(inner)
            | TyKind::Optional(inner) => self.contains_param(*inner),
            _ => false,
        }
    }

    /// How many types are nested inside one another to make `ty`.
    pub fn depth(&self, ty: Ty) -> usize {
        let deepest = |tys: &[Ty]| tys.iter().map(|&t| self.depth(t)).max().unwrap_or(0);
        1 + match self.kind(ty) {
            TyKind::Adt(_, args) | TyKind::Tuple(args) => deepest(args),
            TyKind::Result(ok, err) => deepest(&[*ok, *err]),
            TyKind::Array(inner, _)
            | TyKind::Vec(inner)
            | TyKind::Slice(inner)
            | TyKind::Ptr(inner)
            | TyKind::Ref(inner)
            | TyKind::RefMut(inner)
            | TyKind::Optional(inner) => self.depth(*inner),
            _ => 0,
        }
    }

    /// `ty` with each `Param(i)` replaced by `args[i]`.
    pub fn subst(&mut self, ty: Ty, args: &[Ty]) -> Ty {
        if args.is_empty() || !self.contains_param(ty) {
            return ty;
        }
        let kind = match self.kind(ty).clone() {
            TyKind::Param(at) => return args.get(at as usize).copied().unwrap_or(Self::ERROR),
            TyKind::Adt(def, inner) => TyKind::Adt(def, self.subst_all(&inner, args)),
            TyKind::Tuple(inner) => TyKind::Tuple(self.subst_all(&inner, args)),
            TyKind::Result(ok, err) => TyKind::Result(self.subst(ok, args), self.subst(err, args)),
            TyKind::Array(inner, len) => TyKind::Array(self.subst(inner, args), len),
            TyKind::Vec(inner) => TyKind::Vec(self.subst(inner, args)),
            TyKind::Slice(inner) => TyKind::Slice(self.subst(inner, args)),
            TyKind::Ptr(inner) => TyKind::Ptr(self.subst(inner, args)),
            TyKind::Ref(inner) => TyKind::Ref(self.subst(inner, args)),
            TyKind::RefMut(inner) => TyKind::RefMut(self.subst(inner, args)),
            TyKind::Optional(inner) => TyKind::Optional(self.subst(inner, args)),
            other => other,
        };
        self.intern(kind)
    }

    pub fn subst_all(&mut self, tys: &[Ty], args: &[Ty]) -> Vec<Ty> {
        tys.iter().map(|&ty| self.subst(ty, args)).collect()
    }

    /// The type as the programmer writes it.
    pub fn show(&self, ty: Ty) -> String {
        let list = |tys: &[Ty]| {
            tys.iter()
                .map(|&t| self.show(t))
                .collect::<Vec<_>>()
                .join(", ")
        };
        match self.kind(ty) {
            TyKind::Int(int) => int.name().to_string(),
            TyKind::F32 => "f32".into(),
            TyKind::F64 => "f64".into(),
            TyKind::Bool => "bool".into(),
            TyKind::Void => "void".into(),
            TyKind::Adt(def, args) if args.is_empty() => self.adt_name(*def).into(),
            TyKind::Adt(def, args) => format!("{}<{}>", self.adt_name(*def), list(args)),
            TyKind::Param(at) => format!("T{at}"),
            TyKind::Array(inner, len) => format!("Array<{}, {len}>", self.show(*inner)),
            TyKind::Vec(inner) => format!("Vec<{}>", self.show(*inner)),
            TyKind::Slice(inner) if *inner == Self::U8 => "str".into(),
            TyKind::Slice(inner) => format!("&[{}]", self.show(*inner)),
            TyKind::Ptr(inner) => format!("*{}", self.show(*inner)),
            TyKind::Ref(inner) => format!("&{}", self.show(*inner)),
            TyKind::RefMut(inner) => format!("&var {}", self.show(*inner)),
            TyKind::Optional(inner) => format!("{}?", self.show(*inner)),
            TyKind::Result(ok, err) if *err == Self::I32 => format!("{}!", self.show(*ok)),
            TyKind::Result(ok, err) => format!("{}!{}", self.show(*ok), self.show(*err)),
            TyKind::Tuple(items) => format!("({})", list(items)),
            TyKind::Error => "<error>".into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interning_gives_one_id_per_type() {
        let mut types = Types::new();
        let a = types.intern(TyKind::Vec(Types::I32));
        let b = types.intern(TyKind::Vec(Types::I32));
        assert_eq!(a, b);
        assert_eq!(types.show(a), "Vec<i32>");
        assert_eq!(types.primitive("usize"), Some(Types::USIZE));
        assert_eq!(types.show(Types::STR), "str");
    }

    #[test]
    fn substitution_replaces_parameters_only() {
        let mut types = Types::new();
        let param = types.intern(TyKind::Param(0));
        let optional = types.intern(TyKind::Optional(param));
        let concrete = types.subst(optional, &[Types::I64]);
        assert_eq!(types.show(concrete), "i64?");
        assert_eq!(types.subst(Types::BOOL, &[Types::I64]), Types::BOOL);
    }
}
