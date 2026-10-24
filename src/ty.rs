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

    /// The unsigned type of the same width.
    pub fn unsigned(self) -> Self {
        match self {
            Self::I8 => Self::U8,
            Self::I16 => Self::U16,
            Self::I32 => Self::U32,
            Self::I64 => Self::U64,
            Self::Isize => Self::Usize,
            other => other,
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
    Adt(DefId, Vec<Ty>),
    Param(u32),
    Infer(u32),
    Array(Ty, u64),
    Vec(Ty),
    Slice(Ty),
    SliceMut(Ty),
    Ptr(Ty),
    Ref(Ty),
    RefMut(Ty),
    Optional(Ty),
    Result(Ty, Ty),
    Tuple(Vec<Ty>),
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
    pub const U8: Ty = Ty(10);
    pub const U64: Ty = Ty(13);
    pub const USIZE: Ty = Ty(14);
    pub const STR: Ty = Ty(15);
    pub const STRING: Ty = Ty(16);

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
        types.intern(TyKind::Vec(Self::U8));
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

    /// Whether `ty` is `!`, the result of a function that returns nothing
    /// but may fail.
    pub fn is_unit_result(&self, ty: Ty) -> bool {
        matches!(self.kind(ty), TyKind::Result(ok, _) if *self.kind(*ok) == TyKind::Tuple(Vec::new()))
    }

    /// The element type of an array, a Vec or a slice.
    pub fn elem(&self, ty: Ty) -> Option<Ty> {
        match self.kind(ty) {
            TyKind::Array(elem, _)
            | TyKind::Vec(elem)
            | TyKind::Slice(elem)
            | TyKind::SliceMut(elem) => Some(*elem),
            _ => None,
        }
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

    /// The types written inside `ty`, its arguments, elements or pointee,
    /// or None when it is a leaf.
    fn args(&self, ty: Ty) -> Option<Vec<Ty>> {
        Some(match self.kind(ty) {
            TyKind::Adt(_, args) | TyKind::Tuple(args) => args.clone(),
            TyKind::Result(ok, err) => vec![*ok, *err],
            TyKind::Array(inner, _)
            | TyKind::Vec(inner)
            | TyKind::Slice(inner)
            | TyKind::SliceMut(inner)
            | TyKind::Ptr(inner)
            | TyKind::Ref(inner)
            | TyKind::RefMut(inner)
            | TyKind::Optional(inner) => vec![*inner],
            _ => return None,
        })
    }

    /// Whether `ty` has a leaf for which `leaf` holds.
    pub fn has(&self, ty: Ty, leaf: &dyn Fn(&TyKind) -> bool) -> bool {
        match self.args(ty) {
            Some(args) => args.into_iter().any(|arg| self.has(arg, leaf)),
            None => leaf(self.kind(ty)),
        }
    }

    pub fn contains_param(&self, ty: Ty) -> bool {
        self.has(ty, &|kind| matches!(kind, TyKind::Param(_)))
    }

    pub fn contains_var(&self, ty: Ty) -> bool {
        self.has(ty, &|kind| matches!(kind, TyKind::Infer(_)))
    }

    /// Whether `ty` has no type parameter and no inference variable in it.
    pub fn is_concrete(&self, ty: Ty) -> bool {
        !self.has(ty, &|kind| {
            matches!(kind, TyKind::Param(_) | TyKind::Infer(_))
        })
    }

    /// Whether `ty`, written out in full, is made of more than `size` types
    /// or nests them more than `depth` deep.
    pub fn too_big(&self, ty: Ty, size: usize, depth: usize) -> bool {
        let mut parts = vec![(ty, 1)];
        let mut count = 0;
        while let Some((ty, level)) = parts.pop() {
            count += 1;
            if count > size || level > depth {
                return true;
            }
            let args = self.args(ty).unwrap_or_default();
            parts.extend(args.into_iter().map(|arg| (arg, level + 1)));
        }
        false
    }

    /// `ty` with each `Param(i)` replaced by `args[i]`.
    pub fn subst(&mut self, ty: Ty, args: &[Ty]) -> Ty {
        if args.is_empty() || !self.contains_param(ty) {
            return ty;
        }
        self.replace(ty, &|kind| match kind {
            TyKind::Param(at) => Some(args.get(*at as usize).copied().unwrap_or(Self::ERROR)),
            _ => None,
        })
    }

    /// `ty` with each parameter or variable in it replaced by what `leaf`
    /// gives for it, if anything.
    pub fn replace(&mut self, ty: Ty, leaf: &dyn Fn(&TyKind) -> Option<Ty>) -> Ty {
        let kind = match self.kind(ty).clone() {
            TyKind::Adt(def, inner) => TyKind::Adt(def, self.replace_all(&inner, leaf)),
            TyKind::Tuple(inner) => TyKind::Tuple(self.replace_all(&inner, leaf)),
            TyKind::Result(ok, err) => {
                TyKind::Result(self.replace(ok, leaf), self.replace(err, leaf))
            }
            TyKind::Array(inner, len) => TyKind::Array(self.replace(inner, leaf), len),
            TyKind::Vec(inner) => TyKind::Vec(self.replace(inner, leaf)),
            TyKind::Slice(inner) => TyKind::Slice(self.replace(inner, leaf)),
            TyKind::SliceMut(inner) => TyKind::SliceMut(self.replace(inner, leaf)),
            TyKind::Ptr(inner) => TyKind::Ptr(self.replace(inner, leaf)),
            TyKind::Ref(inner) => TyKind::Ref(self.replace(inner, leaf)),
            TyKind::RefMut(inner) => TyKind::RefMut(self.replace(inner, leaf)),
            TyKind::Optional(inner) => TyKind::Optional(self.replace(inner, leaf)),
            other => return leaf(&other).unwrap_or(ty),
        };
        self.intern(kind)
    }

    fn replace_all(&mut self, tys: &[Ty], leaf: &dyn Fn(&TyKind) -> Option<Ty>) -> Vec<Ty> {
        tys.iter().map(|&ty| self.replace(ty, leaf)).collect()
    }

    pub fn subst_all(&mut self, tys: &[Ty], args: &[Ty]) -> Vec<Ty> {
        tys.iter().map(|&ty| self.subst(ty, args)).collect()
    }

    /// `ty` before a `?` or `!`, in parentheses when it starts with `&` or `*`,
    /// which would otherwise take the suffix in.
    fn show_operand(&self, ty: Ty) -> String {
        match self.kind(ty) {
            TyKind::Ptr(_) | TyKind::Ref(_) | TyKind::RefMut(_) => format!("({})", self.show(ty)),
            _ => self.show(ty),
        }
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
            TyKind::Infer(_) => "_".into(),
            TyKind::Array(inner, len) => format!("Array<{}, {len}>", self.show(*inner)),
            TyKind::Vec(inner) if *inner == Self::U8 => "String".into(),
            TyKind::Vec(inner) => format!("Vec<{}>", self.show(*inner)),
            TyKind::Slice(inner) if *inner == Self::U8 => "str".into(),
            TyKind::Slice(inner) => format!("&[{}]", self.show(*inner)),
            TyKind::SliceMut(inner) => format!("&var [{}]", self.show(*inner)),
            TyKind::Ptr(inner) => format!("*{}", self.show(*inner)),
            TyKind::Ref(inner) => format!("&{}", self.show(*inner)),
            TyKind::RefMut(inner) => format!("&var {}", self.show(*inner)),
            TyKind::Optional(inner) => format!("{}?", self.show_operand(*inner)),
            TyKind::Result(..) if self.is_unit_result(ty) => "!".into(),
            TyKind::Result(ok, _) => format!("{}!", self.show_operand(*ok)),
            TyKind::Tuple(items) if items.len() == 1 => format!("({},)", list(items)),
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
        let concrete = types.subst(optional, &[Types::I32]);
        assert_eq!(types.show(concrete), "i32?");
        assert_eq!(types.subst(Types::BOOL, &[Types::I32]), Types::BOOL);
    }
}
