//! Type checking. Each function instance is checked once, at its own types,
//! into the typed IR of [`crate::tir`]. A generic function or a method of a
//! generic struct becomes an instance only when something needs it, and an
//! instance is made once per set of type arguments.

mod body;
mod call;
mod consts;
mod expr;
mod pattern;
mod types;

use std::cell::Cell;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::ast::{Statement, StatementKind, TypeParameter, TypeSpec};
use crate::errors::{Span, ZeruError};
use crate::items::{DefId, ItemKind, Items, ModuleId};
use crate::tir::{self, FnId};
use crate::ty::{Ty, TyKind, Types};

/// A function's signature with its type parameters still open: struct
/// parameters first, then its own.
#[derive(Clone)]
pub struct Sig {
    pub params: Vec<Ty>,
    pub ret: Ty,
    pub receiver: Receiver,
    pub generics: Vec<TypeParameter>,
    pub own_generics: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Receiver {
    None,
    Shared,
    Mutable,
}

/// How deeply generic structs may nest in one another before checking one
/// is taken to never end.
const MAX_NESTING: usize = 32;

struct StructDef {
    fields: Vec<(String, Ty)>,
}

/// A method a trait asks for: its name, whether it takes `self`, and the
/// types of its other parameters and of what it returns.
#[derive(Clone)]
struct TraitMethod {
    name: String,
    takes_self: bool,
    params: Vec<Ty>,
    ret: Ty,
}

struct EnumDef {
    variants: Vec<(String, Vec<Ty>)>,
}

/// A global constant: checked once, at its own type, and copied where used.
enum ConstState {
    Checking,
    Done(tir::Expr),
}

struct Instance {
    def: DefId,
    args: Vec<Ty>,
    from: Span,
}

pub struct Checker<'a> {
    items: &'a Items<'a>,
    pub types: Types,
    errors: Vec<ZeruError>,

    sigs: HashMap<DefId, Option<Sig>>,
    structs: HashMap<DefId, StructDef>,
    enums: HashMap<DefId, EnumDef>,
    traits: HashMap<DefId, Vec<TraitMethod>>,
    consts: HashMap<DefId, ConstState>,

    instances: HashMap<(DefId, Vec<Ty>), FnId>,
    pending: VecDeque<FnId>,
    functions: Vec<Option<tir::Function>>,
    queued: Vec<Instance>,
    unsatisfied: HashSet<(DefId, Vec<Ty>)>,
    adts_seen: HashSet<Ty>,
    /// Where an expression failed to check without an error saying why, as
    /// rustc's delayed bugs: compiling on from there would emit broken code.
    unreported: Cell<Option<Span>>,
}

/// Where a type, a name or a call is being resolved: in which module, with
/// which type parameters in scope and what they stand for.
#[derive(Clone)]
struct Scope {
    module: ModuleId,
    generics: Vec<String>,
    args: Vec<Ty>,
}

impl<'a> Checker<'a> {
    pub fn new(items: &'a Items<'a>) -> Self {
        Self {
            items,
            types: Types::new(),
            errors: items.errors.clone(),
            sigs: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            traits: HashMap::new(),
            consts: HashMap::new(),
            instances: HashMap::new(),
            pending: VecDeque::new(),
            functions: Vec::new(),
            queued: Vec::new(),
            unsatisfied: HashSet::new(),
            adts_seen: HashSet::new(),
            unreported: Cell::new(None),
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(ZeruError::semantic(message, span));
    }

    /// Check everything reachable and return the program, or the errors.
    pub fn check(mut self) -> Result<tir::Program, Vec<ZeruError>> {
        let defs: Vec<DefId> = (0..self.items.items.len() as u32).map(DefId).collect();
        for &def in &defs {
            let item = self.items.item(def);
            match item.kind {
                ItemKind::Struct => {
                    self.struct_def(def);
                }
                ItemKind::Enum => {
                    self.enum_def(def);
                }
                ItemKind::Trait => {
                    self.trait_def(def);
                }
                ItemKind::Const => {
                    self.const_value(def, item.decl.span);
                }
                ItemKind::Fn => {
                    self.sig(def);
                }
            }
        }
        for &def in &defs {
            let item = self.items.item(def);
            if item.kind != ItemKind::Fn
                || !self.sig(def).is_some_and(|sig| sig.generics.is_empty())
            {
                continue;
            }
            self.instance(def, Vec::new(), item.decl.span);
        }
        self.check_recursive_types(&defs);
        self.drain();
        let runtime = self.runtime();
        let (structs, enums) = self.layouts();
        let owning = self
            .types
            .all()
            .filter(|&ty| !self.types.contains_param(ty))
            .collect::<Vec<_>>()
            .into_iter()
            .filter(|&ty| self.owns_heap(ty))
            .collect();

        let mut errors = self.errors;
        if let (true, Some(span)) = (errors.is_empty(), self.unreported.get()) {
            errors.push(ZeruError::semantic(
                "Internal compiler error: this expression failed to check without saying why; please report it",
                span,
            ));
        }
        if !errors.is_empty() {
            errors.sort_by(|a, b| (a.span.start, &a.message).cmp(&(b.span.start, &b.message)));
            errors.dedup_by(|a, b| a.span == b.span && a.message == b.message);
            return Err(errors);
        }
        Ok(tir::Program {
            types: self.types,
            functions: self.functions.into_iter().map(Option::unwrap).collect(),
            structs,
            enums,
            owning,
            runtime: runtime.expect("the prelude declares the runtime"),
        })
    }

    /// The layout of every struct and enum type the program names, with the
    /// `drop` method of each struct that has one. Making those methods can
    /// name more types, so this repeats until nothing new turns up.
    fn layouts(&mut self) -> (HashMap<Ty, tir::StructLayout>, HashMap<Ty, tir::EnumLayout>) {
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();
        let mut done = 0;
        while done < self.types.len() {
            let upto = self.types.len();
            for ty in self
                .types
                .all()
                .skip(done)
                .take(upto - done)
                .collect::<Vec<_>>()
            {
                let TyKind::Adt(def, args) = self.types.kind(ty).clone() else {
                    continue;
                };
                if self.types.contains_param(ty) || self.unsatisfied.contains(&(def, args.clone()))
                {
                    continue;
                }
                if self.items.item(def).kind == ItemKind::Enum {
                    let variants = self.variants_of(def).into_iter().map(|(_, f)| f).collect();
                    enums.insert(ty, tir::EnumLayout { variants });
                    continue;
                }
                let fields = self.fields_of(ty).into_iter().map(|(_, f)| f).collect();
                let drop = self
                    .items
                    .method(def, "drop")
                    .map(|method| self.instance(method, args, Span::default()));
                structs.insert(ty, tir::StructLayout { fields, drop });
            }
            done = upto;
            self.drain();
        }
        (structs, enums)
    }

    fn drain(&mut self) {
        while let Some(id) = self.pending.pop_front() {
            let instance = &self.queued[id.0 as usize];
            let (def, args) = (instance.def, instance.args.clone());
            let function = body::check_function(self, def, &args);
            self.functions[id.0 as usize] = Some(function);
        }
    }

    /// The function instance `def` at `args`, made and queued the first time.
    pub(crate) fn instance(&mut self, def: DefId, args: Vec<Ty>, from: Span) -> FnId {
        if let Some(&id) = self.instances.get(&(def, args.clone())) {
            return id;
        }
        let id = FnId(self.functions.len() as u32);
        self.functions.push(None);
        self.queued.push(Instance {
            def,
            args: args.clone(),
            from,
        });
        self.instances.insert((def, args), id);
        self.pending.push_back(id);
        id
    }

    /// Where the instance was first asked for, to point there when its body
    /// fails to check.
    fn instance_origin(&self, id: FnId) -> Span {
        self.queued[id.0 as usize].from
    }

    fn scope_of(&self, def: DefId, args: &[Ty]) -> Scope {
        let item = self.items.item(def);
        let generics = match item.kind {
            ItemKind::Fn => self.generics_of_fn(def),
            ItemKind::Struct => generics_of(item.decl).to_vec(),
            _ => Vec::new(),
        };
        Scope {
            module: item.module,
            generics: generics.into_iter().map(|g| g.name).collect(),
            args: args.to_vec(),
        }
    }

    fn generics_of_fn(&self, def: DefId) -> Vec<TypeParameter> {
        let item = self.items.item(def);
        let mut generics = item
            .owner
            .map(|owner| generics_of(self.items.item(owner).decl).to_vec())
            .unwrap_or_default();
        generics.extend(generics_of(item.decl).iter().cloned());
        generics
    }

    pub(crate) fn sig(&mut self, def: DefId) -> Option<Sig> {
        if let Some(sig) = self.sigs.get(&def) {
            return sig.clone();
        }
        self.sigs.insert(def, None);
        let sig = self.resolve_sig(def);
        self.sigs.insert(def, sig.clone());
        sig
    }

    fn resolve_sig(&mut self, def: DefId) -> Option<Sig> {
        let item = self.items.item(def);
        let StatementKind::Function {
            name,
            params,
            return_type,
            type_params,
            ..
        } = &item.decl.kind
        else {
            return None;
        };
        let span = item.decl.span;
        let full = item.name.clone();
        let generics = self.generics_of_fn(def);
        let scope = Scope {
            module: item.module,
            generics: generics.iter().map(|g| g.name.clone()).collect(),
            args: Vec::new(),
        };

        if let Some(owner) = item.owner {
            let shadowed = type_params.iter().find(|own| {
                generics_of(self.items.item(owner).decl)
                    .iter()
                    .any(|p| p.name == own.name)
            });
            if let Some(param) = shadowed {
                let owner_name = self.items.item(owner).name.clone();
                self.error(
                    format!(
                        "Type parameter '{}' of '{name}' is already a parameter of {owner_name}",
                        param.name
                    ),
                    span,
                );
                return None;
            }
        }
        let names: Vec<String> = params.iter().map(|(n, _, _)| n.clone()).collect();
        if let Some(dup) = first_duplicate(&names) {
            self.error(
                format!("Function '{full}' declares parameter '{dup}' twice"),
                span,
            );
        }
        let generic_names: Vec<String> = type_params.iter().map(|p| p.name.clone()).collect();
        if let Some(dup) = first_duplicate(&generic_names) {
            self.error(
                format!("Function '{full}' declares type parameter '{dup}' twice"),
                span,
            );
            return None;
        }
        self.check_bound_names(type_params, item.module, span);

        let mut receiver = Receiver::None;
        let mut param_tys = Vec::new();
        for (index, (param, spec, mutable)) in params.iter().enumerate() {
            if param == "self" {
                match item.owner {
                    Some(owner) if index == 0 => {
                        receiver = if *mutable {
                            Receiver::Mutable
                        } else {
                            Receiver::Shared
                        };
                        let args: Vec<Ty> = (0..generics_of(self.items.item(owner).decl).len())
                            .map(|at| self.types.intern(TyKind::Param(at as u32)))
                            .collect();
                        param_tys.push(self.types.intern(TyKind::Adt(owner, args)));
                    }
                    _ => {
                        self.error("'self' parameter allowed only in struct methods", span);
                        param_tys.push(Types::ERROR);
                    }
                }
                continue;
            }
            let ty = self.resolve_type(spec, &scope, span);
            if ty == Types::VOID {
                self.error(format!("Parameter '{param}' cannot be void"), span);
            }
            param_tys.push(ty);
        }
        let ret = match return_type {
            Some(spec) => self.resolve_type(spec, &scope, span),
            None => Types::VOID,
        };

        if self.is_main(def) {
            if !params.is_empty() {
                self.error("Function 'main' must not take arguments", span);
            }
            if ret != Types::VOID {
                let shown = self.types.show(ret);
                self.error(
                    format!("Function 'main' must return void, not {shown}"),
                    span,
                );
            }
        }
        if item.owner.is_some()
            && name == "drop"
            && (params.len() != 1 || receiver != Receiver::Mutable || return_type.is_some())
        {
            self.error(
                "A 'drop' method takes only 'var self' and returns nothing",
                span,
            );
        }
        if item.owner.is_some() && name == "copy" {
            self.error(
                "'copy' is the built-in deep copy; give this method another name",
                span,
            );
        }

        Some(Sig {
            params: param_tys,
            ret,
            receiver,
            own_generics: type_params.len(),
            generics,
        })
    }

    fn struct_def(&mut self, def: DefId) -> &StructDef {
        if !self.structs.contains_key(&def) {
            let item = self.items.item(def);
            let StatementKind::Struct {
                fields,
                type_params,
                ..
            } = &item.decl.kind
            else {
                unreachable!("a struct item is a struct")
            };
            self.structs.insert(def, StructDef { fields: Vec::new() });
            self.types.name_adt(def, item.name.clone());
            let names: Vec<String> = fields.iter().map(|(f, _)| f.clone()).collect();
            if let Some(dup) = first_duplicate(&names) {
                let message = format!("Struct '{}' declares field '{dup}' twice", item.name);
                self.error(message, item.decl.span);
            }
            let generic_names: Vec<String> = type_params.iter().map(|p| p.name.clone()).collect();
            if let Some(dup) = first_duplicate(&generic_names) {
                let message = format!(
                    "Struct '{}' declares type parameter '{dup}' twice",
                    item.name
                );
                self.error(message, item.decl.span);
            }
            self.check_bound_names(type_params, item.module, item.decl.span);
            let scope = self.scope_of(def, &[]);
            let resolved = fields
                .iter()
                .map(|(name, spec)| {
                    (
                        name.clone(),
                        self.resolve_type(spec, &scope, item.decl.span),
                    )
                })
                .collect();
            self.structs.get_mut(&def).unwrap().fields = resolved;
        }
        &self.structs[&def]
    }

    fn enum_def(&mut self, def: DefId) -> &EnumDef {
        if !self.enums.contains_key(&def) {
            let item = self.items.item(def);
            let StatementKind::Enum { variants, .. } = &item.decl.kind else {
                unreachable!("an enum item is an enum")
            };
            self.types.name_adt(def, item.name.clone());
            self.enums.insert(
                def,
                EnumDef {
                    variants: Vec::new(),
                },
            );
            let names: Vec<String> = variants.iter().map(|(v, _)| v.clone()).collect();
            if let Some(dup) = first_duplicate(&names) {
                let message = format!("Enum '{}' declares variant '{dup}' twice", item.name);
                self.error(message, item.decl.span);
            }
            let scope = self.scope_of(def, &[]);
            let resolved = variants
                .iter()
                .map(|(name, fields)| {
                    let fields = fields
                        .iter()
                        .map(|spec| self.resolve_type(spec, &scope, item.decl.span))
                        .collect();
                    (name.clone(), fields)
                })
                .collect();
            self.enums.get_mut(&def).unwrap().variants = resolved;
        }
        &self.enums[&def]
    }

    fn trait_def(&mut self, def: DefId) -> Vec<TraitMethod> {
        if let Some(methods) = self.traits.get(&def) {
            return methods.clone();
        }
        let item = self.items.item(def);
        let StatementKind::Trait { methods, .. } = &item.decl.kind else {
            unreachable!("a trait item is a trait")
        };
        let scope = self.scope_of(def, &[]);
        let span = item.decl.span;
        let mut shape = Vec::new();
        for method in methods {
            let is_self = |spec: &TypeSpec| matches!(spec, TypeSpec::Named(name) if name == "self");
            let params = method
                .params
                .iter()
                .filter(|(_, spec, _)| !is_self(spec))
                .map(|(_, spec, _)| self.resolve_type(spec, &scope, span))
                .collect();
            let ret = match &method.return_type {
                Some(spec) => self.resolve_type(spec, &scope, span),
                None => Types::VOID,
            };
            shape.push(TraitMethod {
                name: method.name.clone(),
                takes_self: method
                    .params
                    .first()
                    .is_some_and(|(_, spec, _)| is_self(spec)),
                params,
                ret,
            });
        }
        self.traits.insert(def, shape.clone());
        shape
    }

    /// Report a bound that names no trait, where the type parameter is
    /// declared.
    fn check_bound_names(&mut self, generics: &[TypeParameter], module: ModuleId, span: Span) {
        for bound in generics.iter().filter_map(|param| param.bound.as_ref()) {
            if matches!(bound.as_str(), "Eq" | "Ord" | "Num") {
                continue;
            }
            match self.items.lookup_trait(module, bound) {
                Some(def) if bound.contains("::") => self.check_visible(def, module, span),
                Some(_) => {}
                None => self.error(format!("Unknown trait '{bound}' in a bound"), span),
            }
        }
    }

    /// The fields of a struct at the given type arguments.
    pub(crate) fn fields_of(&mut self, ty: Ty) -> Vec<(String, Ty)> {
        let TyKind::Adt(def, args) = self.types.kind(ty).clone() else {
            return Vec::new();
        };
        if self.items.item(def).kind != ItemKind::Struct {
            return Vec::new();
        }
        let fields = self.struct_def(def).fields.clone();
        fields
            .into_iter()
            .map(|(name, field)| (name, self.types.subst(field, &args)))
            .collect()
    }

    pub(crate) fn variants_of(&mut self, def: DefId) -> Vec<(String, Vec<Ty>)> {
        self.enum_def(def).variants.clone()
    }

    pub(crate) fn is_enum(&self, ty: Ty) -> Option<DefId> {
        match self.types.kind(ty) {
            TyKind::Adt(def, _) if self.items.item(*def).kind == ItemKind::Enum => Some(*def),
            _ => None,
        }
    }

    pub(crate) fn is_struct(&self, ty: Ty) -> Option<DefId> {
        match self.types.kind(ty) {
            TyKind::Adt(def, _) if self.items.item(*def).kind == ItemKind::Struct => Some(*def),
            _ => None,
        }
    }

    pub(crate) fn enum_has_data(&mut self, def: DefId) -> bool {
        self.enum_def(def)
            .variants
            .iter()
            .any(|(_, f)| !f.is_empty())
    }

    /// Whether a value of `ty` owns memory or has a `drop`: what is freed
    /// when it goes, copied deeply by `.copy()`, and moved rather than copied.
    pub(crate) fn owns_heap(&mut self, ty: Ty) -> bool {
        self.owns_heap_past(ty, &mut Vec::new())
    }

    fn owns_heap_past(&mut self, ty: Ty, seen: &mut Vec<Ty>) -> bool {
        match self.types.kind(ty).clone() {
            TyKind::Vec(_) => true,
            TyKind::Adt(def, _) if !seen.contains(&ty) => {
                seen.push(ty);
                match self.items.item(def).kind {
                    ItemKind::Struct => {
                        self.items.method(def, "drop").is_some()
                            || self
                                .fields_of(ty)
                                .into_iter()
                                .any(|(_, field)| self.owns_heap_past(field, seen))
                    }
                    _ => self
                        .variants_of(def)
                        .into_iter()
                        .flat_map(|(_, fields)| fields)
                        .any(|field| self.owns_heap_past(field, seen)),
                }
            }
            TyKind::Tuple(parts) => parts
                .into_iter()
                .any(|part| self.owns_heap_past(part, seen)),
            TyKind::Array(inner, _) | TyKind::Optional(inner) | TyKind::Result(inner, _) => {
                self.owns_heap_past(inner, seen)
            }
            _ => false,
        }
    }

    /// Whether a value of `ty` holds a reference, so it borrows what that
    /// points into, and whether any it holds is a `&var`.
    pub(crate) fn holds_ref(&mut self, ty: Ty) -> Option<bool> {
        self.holds_ref_past(ty, &mut Vec::new())
    }

    fn holds_ref_past(&mut self, ty: Ty, seen: &mut Vec<Ty>) -> Option<bool> {
        let parts = match self.types.kind(ty).clone() {
            TyKind::Ref(_) => return Some(false),
            TyKind::RefMut(_) => return Some(true),
            TyKind::Adt(def, _) if !seen.contains(&ty) => {
                seen.push(ty);
                match self.items.item(def).kind {
                    ItemKind::Struct => self.fields_of(ty).into_iter().map(|(_, f)| f).collect(),
                    _ => self
                        .variants_of(def)
                        .into_iter()
                        .flat_map(|(_, fields)| fields)
                        .collect(),
                }
            }
            TyKind::Tuple(parts) => parts,
            TyKind::Result(inner, error) => vec![inner, error],
            TyKind::Array(inner, _) | TyKind::Optional(inner) | TyKind::Vec(inner) => vec![inner],
            _ => Vec::new(),
        };
        parts
            .into_iter()
            .filter_map(|part| self.holds_ref_past(part, seen))
            .reduce(|a, b| a || b)
    }

    /// Note that values of `ty` exist, so its layout and, for a generic
    /// struct, its methods are needed.
    pub(crate) fn note_adt(&mut self, ty: Ty, span: Span) {
        if self.types.contains_param(ty) || !self.adts_seen.insert(ty) {
            return;
        }
        let TyKind::Adt(def, args) = self.types.kind(ty).clone() else {
            return;
        };
        if self.types.depth(ty) > MAX_NESTING {
            let base = self.items.item(def).name.clone();
            self.error(
                format!("Checking the methods of '{base}' asks for ever deeper instances of it, without end"),
                span,
            );
            return;
        }
        for field in self.fields_of(ty).into_iter().map(|(_, ty)| ty) {
            self.note_types_in(field, span);
        }
        if self.items.item(def).kind == ItemKind::Enum {
            for field in self.variants_of(def).into_iter().flat_map(|(_, f)| f) {
                self.note_types_in(field, span);
            }
            return;
        }
        if args.is_empty() {
            return;
        }
        let methods: Vec<DefId> = self.items.methods_of(def).collect();
        for method in methods {
            if self.sig(method).is_some_and(|sig| sig.own_generics == 0) {
                self.instance(method, args.clone(), span);
            }
        }
    }

    pub(crate) fn note_types_in(&mut self, ty: Ty, span: Span) {
        match self.types.kind(ty).clone() {
            TyKind::Adt(..) => self.note_adt(ty, span),
            TyKind::Tuple(parts) => parts.into_iter().for_each(|p| self.note_types_in(p, span)),
            TyKind::Result(ok, err) => {
                self.note_types_in(ok, span);
                self.note_types_in(err, span);
            }
            TyKind::Array(inner, _)
            | TyKind::Vec(inner)
            | TyKind::Slice(inner)
            | TyKind::Optional(inner)
            | TyKind::Ptr(inner)
            | TyKind::Ref(inner)
            | TyKind::RefMut(inner) => self.note_types_in(inner, span),
            _ => {}
        }
    }

    /// Report an argument that does not meet its parameter's bound, once per
    /// item and arguments, so the complaint lands on the use, not inside.
    fn check_bounds(
        &mut self,
        def: DefId,
        generics: &[TypeParameter],
        args: &[Ty],
        span: Span,
    ) -> bool {
        if self.unsatisfied.contains(&(def, args.to_vec())) {
            return false;
        }
        let module = self.items.item(def).module;
        let mut satisfied = true;
        for (param, &arg) in generics.iter().zip(args) {
            let Some(bound) = &param.bound else {
                continue;
            };
            if self.types.contains_param(arg) || self.types.is_error(arg) {
                continue;
            }
            let ok = match bound.as_str() {
                "Eq" => match self.is_enum(arg) {
                    Some(e) => !self.enum_has_data(e),
                    None => {
                        self.types.is_number(arg)
                            || matches!(self.types.kind(arg), TyKind::Bool | TyKind::Ptr(_))
                    }
                },
                "Ord" | "Num" => self.types.is_number(arg),
                _ => match self.items.lookup_trait(module, bound) {
                    Some(trait_def) => self.has_trait_methods(arg, trait_def),
                    None => continue,
                },
            };
            if !ok {
                let (base, shown) = (self.items.item(def).name.clone(), self.types.show(arg));
                self.error(
                    format!(
                        "'{base}' asks for {}: {bound}, which {shown} does not satisfy",
                        param.name
                    ),
                    span,
                );
                satisfied = false;
            }
        }
        if !satisfied {
            self.unsatisfied.insert((def, args.to_vec()));
        }
        satisfied
    }

    /// A type meets a trait by having its methods; there is no other way to
    /// say that it does. Names and how many parameters they take are compared.
    /// A type meets a trait by having its methods, with the same parameter
    /// and return types; there is no other way to say that it does.
    fn has_trait_methods(&mut self, ty: Ty, trait_def: DefId) -> bool {
        let Some(owner) = self.is_struct(ty) else {
            return false;
        };
        let TyKind::Adt(_, args) = self.types.kind(ty).clone() else {
            return false;
        };
        self.trait_def(trait_def).into_iter().all(|wanted| {
            let Some(sig) = self
                .items
                .method(owner, &wanted.name)
                .and_then(|m| self.sig(m))
            else {
                return false;
            };
            let takes_self = sig.receiver != Receiver::None;
            let params = self
                .types
                .subst_all(&sig.params[usize::from(takes_self)..], &args);
            let ret = self.types.subst(sig.ret, &args);
            let same =
                |have: Ty, want: Ty| have == want || have == Types::ERROR || want == Types::ERROR;
            takes_self == wanted.takes_self
                && params.len() == wanted.params.len()
                && params.iter().zip(&wanted.params).all(|(&p, &w)| same(p, w))
                && same(ret, wanted.ret)
        })
    }

    fn check_visible(&mut self, def: DefId, from: ModuleId, span: Span) {
        if !self.items.visible(def, from) {
            self.errors.push(self.items.private_error(def, span));
        }
    }

    /// A struct or enum that stores itself, directly or through another, has
    /// no finite size.
    fn check_recursive_types(&mut self, defs: &[DefId]) {
        for &def in defs {
            let item = self.items.item(def);
            if !matches!(item.kind, ItemKind::Struct | ItemKind::Enum) {
                continue;
            }
            if self.stores_by_value(def, def, &mut HashSet::new()) {
                let message = format!("'{}' stores itself, so it has no finite size", item.name);
                self.error(message, item.decl.span);
            }
        }
    }

    fn stores_by_value(&mut self, from: DefId, target: DefId, seen: &mut HashSet<DefId>) -> bool {
        let fields: Vec<Ty> = match self.items.item(from).kind {
            ItemKind::Struct => self
                .struct_def(from)
                .fields
                .iter()
                .map(|(_, t)| *t)
                .collect(),
            _ => self
                .variants_of(from)
                .into_iter()
                .flat_map(|(_, fields)| fields)
                .collect(),
        };
        let mut deps = Vec::new();
        for ty in fields {
            self.inline_adts(ty, &mut deps);
        }
        deps.into_iter().any(|dep| {
            dep == target || (seen.insert(dep) && self.stores_by_value(dep, target, seen))
        })
    }

    /// The structs and enums a value of `ty` holds inline. A pointer, a
    /// slice or a Vec keeps what it holds elsewhere, which breaks a cycle.
    fn inline_adts(&self, ty: Ty, out: &mut Vec<DefId>) {
        match self.types.kind(ty) {
            TyKind::Adt(def, _) => out.push(*def),
            TyKind::Tuple(parts) => parts.iter().for_each(|&p| self.inline_adts(p, out)),
            TyKind::Optional(inner) | TyKind::Array(inner, _) => self.inline_adts(*inner, out),
            TyKind::Result(ok, err) => {
                self.inline_adts(*ok, out);
                self.inline_adts(*err, out);
            }
            _ => {}
        }
    }

    /// The prelude functions generated code calls on its own.
    /// An instance of a function the prelude defines for the compiler's use.
    pub(super) fn prelude_fn(&mut self, name: &str) -> FnId {
        let def = self
            .items
            .lookup_value(ModuleId::PRELUDE, name)
            .expect("the prelude defines it");
        self.instance(def, Vec::new(), Span::default())
    }

    fn runtime(&mut self) -> Option<tir::Runtime> {
        let global = ModuleId::PRELUDE;
        let mut prelude_struct = |name: &str| {
            let def = self.items.lookup_type(global, name)?;
            let ty = self.types.intern(TyKind::Adt(def, Vec::new()));
            self.note_adt(ty, Span::default());
            Some((def, ty))
        };
        let (stream, stream_ty) = prelude_struct("OutStream")?;
        let (heap, heap_ty) = prelude_struct("Heap")?;
        let method = |this: &mut Self, owner: DefId, name: &str| {
            let def = this.items.method(owner, name)?;
            Some(this.instance(def, Vec::new(), Span::default()))
        };
        let runtime = tir::Runtime {
            out_stream: stream_ty,
            write_bytes: method(self, stream, "write_bytes")?,
            write_int: method(self, stream, "write_int")?,
            write_uint: method(self, stream, "write_uint")?,
            write_float: method(self, stream, "write_float")?,
            flush: method(self, stream, "flush")?,
            heap: heap_ty,
            alloc: method(self, heap, "alloc")?,
            realloc: method(self, heap, "realloc")?,
        };
        self.drain();
        Some(runtime)
    }
}

fn generics_of(decl: &Statement) -> &[TypeParameter] {
    match &decl.kind {
        StatementKind::Function { type_params, .. } | StatementKind::Struct { type_params, .. } => {
            type_params
        }
        _ => &[],
    }
}

/// The program's entry point: `main` in the root file.
impl Checker<'_> {
    fn is_main(&self, def: DefId) -> bool {
        let item = self.items.item(def);
        item.owner.is_none() && item.module == ModuleId::ROOT && item.name == "main"
    }
}

fn first_duplicate(names: &[String]) -> Option<&String> {
    let mut seen = HashSet::new();
    names.iter().find(|name| !seen.insert(*name))
}
