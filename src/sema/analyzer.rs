use crate::{
    ast::{Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec},
    codegen::runtime::FLUSH_FN,
    errors::{Span, ZeruError},
    generics::{Substitutions, instantiate, mangle, map_types},
    sema::{
        symbol_table::{Moves, SymbolTable},
        types::{FloatWidth, IntWidth, Signedness, Type},
    },
};
use std::collections::{HashMap, HashSet};

type TraitMethod = (String, Vec<Type>, Option<Type>);

/// An enum variant's name, and the types of the values it carries.
pub type Variant = (String, Vec<Type>);

#[derive(PartialEq, Clone, Copy)]
enum Borrow {
    Shared,
    Mutable,
}

enum CallKind {
    Named(String),
    Method {
        method_name: String,
        is_vec_static: bool,
    },
    Unknown,
}

/// The built-in ways to write text: to stdout or stderr, with or without a
/// newline after.
pub const PRINTS: &[&str] = &["print", "println", "eprint", "eprintln"];

/// The `Vec` methods that change it, so need a receiver declared `var`.
const VEC_MUTATORS: &[&str] = &[
    "push",
    "pop",
    "clear",
    "insert",
    "remove",
    "reserve",
    "shrink_to_fit",
];

/// What a loop has to know to check the moves inside it.
struct LoopFrame {
    depth: usize,
    before: Moves,
    exits: Vec<Moves>,
}

pub struct SemanticAnalyzer {
    pub errors: Vec<ZeruError>,

    symbols: SymbolTable,
    struct_defs: HashMap<String, Vec<(String, Type)>>,
    enum_defs: HashMap<String, Vec<Variant>>,
    trait_defs: HashMap<String, Vec<TraitMethod>>,
    current_fn_return_type: Option<Type>,
    current_type_params: Vec<String>,

    mut_self_methods: HashSet<String>,

    generic_structs: HashMap<String, Statement>,
    generic_functions: HashMap<String, Statement>,
    instantiations: Vec<Statement>,

    in_instance: bool,

    loops: Vec<LoopFrame>,
    locked: Vec<String>,

    privates: HashMap<String, String>,
    current_item: String,

    constants: HashMap<String, Expression>,

    moves: HashSet<Span>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut symbols = SymbolTable::new();
        symbols.insert_fn(FLUSH_FN.to_string(), Vec::new(), Type::Void);
        Self {
            errors: Vec::new(),
            symbols,
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            current_fn_return_type: None,
            current_type_params: Vec::new(),
            mut_self_methods: HashSet::new(),
            generic_structs: HashMap::new(),
            generic_functions: HashMap::new(),
            instantiations: Vec::new(),
            in_instance: false,
            loops: Vec::new(),
            locked: Vec::new(),
            privates: HashMap::new(),
            current_item: String::new(),
            constants: HashMap::new(),
            moves: HashSet::new(),
        }
    }

    pub fn struct_fields(&self, name: &str) -> &[(String, Type)] {
        self.struct_defs.get(name).map_or(&[], Vec::as_slice)
    }

    pub fn is_moved_at(&self, span: Span) -> bool {
        self.moves.contains(&span)
    }

    /// Whether a value of `ty` owns memory or a `drop`, which makes it
    /// something to free when it goes and to duplicate when it is copied.
    pub fn owns_heap(&self, ty: &Type) -> bool {
        self.owns_heap_past(ty, &mut Vec::new())
    }

    /// `owns_heap`, entering each struct once: one that holds itself is
    /// reported already, and must not send this round in circles.
    fn owns_heap_past<'t>(&'t self, ty: &'t Type, seen: &mut Vec<&'t str>) -> bool {
        match ty {
            Type::Vec { .. } => true,
            Type::Struct(name) if !seen.contains(&name.as_str()) => {
                seen.push(name);
                self.signature(&format!("{name}::drop")).is_some()
                    || self
                        .struct_fields(name)
                        .iter()
                        .any(|(_, ty)| self.owns_heap_past(ty, seen))
            }
            Type::Tuple(types) => types.iter().any(|ty| self.owns_heap_past(ty, seen)),
            Type::Enum(name) => self.enum_variants(name).is_some_and(|variants| {
                variants
                    .iter()
                    .flat_map(|(_, fields)| fields)
                    .any(|ty| self.owns_heap_past(ty, seen))
            }),
            Type::Array {
                elem_type: inner, ..
            }
            | Type::Optional(inner)
            | Type::Result { ok_type: inner, .. } => self.owns_heap_past(inner, seen),
            _ => false,
        }
    }

    pub fn enum_variants(&self, name: &str) -> Option<&[Variant]> {
        self.enum_defs.get(name).map(Vec::as_slice)
    }

    /// Whether some variant of the enum carries values, which makes it a
    /// tag with a payload rather than a bare number.
    pub fn enum_has_data(&self, name: &str) -> bool {
        self.enum_variants(name)
            .is_some_and(|variants| variants.iter().any(|(_, fields)| !fields.is_empty()))
    }

    pub fn signature(&self, name: &str) -> Option<(&[Type], &Type)> {
        match self.symbols.lookup(name)? {
            super::symbol_table::Symbol::Function { params, ret_type } => Some((params, ret_type)),
            _ => None,
        }
    }

    pub fn analyze(&mut self, program: &mut Program) {
        self.privates = std::mem::take(&mut program.privates);
        self.take_generics(program);
        self.scan_types(&program.statements);
        self.check_recursive_structs(&program.statements);
        self.scan_functions(&program.statements);
        let generic_functions: Vec<_> = self.generic_functions.values().cloned().collect();
        self.scan_functions(&generic_functions);
        self.analyze_bodies(&mut program.statements);

        while !self.instantiations.is_empty() {
            self.expand_instantiations(program);
            self.name_instantiations(program);
        }
    }

    /// A generic struct is not a type, nor a generic function a function,
    /// until its parameters are known, so the declaration is set aside and
    /// each instantiation takes its place, checked at its own types.
    fn take_generics(&mut self, program: &mut Program) {
        program.statements.retain(|stmt| {
            let duplicate = match &stmt.kind {
                StatementKind::Struct {
                    name, type_params, ..
                } if !type_params.is_empty() => self
                    .generic_structs
                    .insert(name.clone(), stmt.clone())
                    .map(|_| format!("Type {name} is already defined")),
                StatementKind::Function {
                    name, type_params, ..
                } if !type_params.is_empty() => self
                    .generic_functions
                    .insert(name.clone(), stmt.clone())
                    .map(|_| format!("Function '{name}' is already defined")),
                _ => return true,
            };
            if let Some(message) = duplicate {
                self.error(message, stmt.span);
            }
            false
        });
    }

    /// Analyse each instantiation and put it in the program. Checking one can
    /// reach a generic struct nothing has instantiated yet, so the caller
    /// repeats this until no new instantiation turns up.
    fn expand_instantiations(&mut self, program: &mut Program) {
        self.in_instance = true;
        for mut decl in std::mem::take(&mut self.instantiations) {
            self.check_statement_top_level(&mut decl);
            program.statements.push(decl);
        }
        self.in_instance = false;
    }

    /// Register `Pair<i32>` as a struct of its own and queue its declaration.
    /// Returns the name it was given, which is what every reference uses.
    fn instantiate_struct(&mut self, base: &str, args: &[Type], span: Span) -> Option<String> {
        let decl = self.generic_structs.get(base)?.clone();
        let StatementKind::Struct { type_params, .. } = &decl.kind else {
            return None;
        };

        if type_params.len() != args.len() {
            self.error(
                format!(
                    "'{base}' takes {} type argument(s), got {}",
                    type_params.len(),
                    args.len()
                ),
                span,
            );
            return None;
        }

        for (param, arg) in type_params.iter().zip(args) {
            if let Some(bound) = &param.bound {
                self.check_bound(base, &param.name, bound, arg, span);
            }
        }

        let subs: Substitutions = type_params
            .iter()
            .zip(args)
            .map(|(param, arg)| (param.name.clone(), arg.to_spec()))
            .collect();
        let name = mangle(base, type_params, &subs);
        if self.struct_defs.contains_key(&name) {
            return Some(name);
        }

        self.struct_defs.insert(name.clone(), Vec::new());

        let concrete = instantiate(&decl, name.clone(), &subs);
        self.scan_struct_fields(&concrete);
        self.scan_functions(std::slice::from_ref(&concrete));
        self.instantiations.push(concrete);
        Some(name)
    }

    /// Report an argument that does not meet what the parameter asks of it, so
    /// the complaint lands on the bound instead of deep in a method body.
    fn check_bound(&mut self, base: &str, param: &str, bound: &str, arg: &Type, span: Span) {
        let satisfied = match bound {
            "Eq" => matches!(
                arg,
                Type::Integer { .. }
                    | Type::Float(_)
                    | Type::Bool
                    | Type::Enum(_)
                    | Type::Pointer(_)
            ),
            "Ord" | "Num" => matches!(arg, Type::Integer { .. } | Type::Float(_)),
            _ if self.trait_defs.contains_key(bound) => self.has_trait_methods(arg, bound),
            _ => {
                self.error(format!("Unknown trait '{bound}' in a bound"), span);
                return;
            }
        };

        if !satisfied {
            self.error(
                format!("'{base}' asks for {param}: {bound}, which {arg} does not satisfy"),
                span,
            );
        }
    }

    /// A type meets a trait by having its methods; there is no separate way to
    /// say that it does. Names and how many arguments they take are compared.
    fn has_trait_methods(&self, arg: &Type, bound: &str) -> bool {
        let Type::Struct(name) = arg else {
            return false;
        };
        let Some(methods) = self.trait_defs.get(bound) else {
            return false;
        };

        methods.iter().all(|(method, params, _)| {
            matches!(
                self.symbols.lookup(&format!("{name}::{method}")),
                Some(super::symbol_table::Symbol::Function { params: have, .. })
                    if have.len() == params.len()
            )
        })
    }

    /// Rewrite every written `Pair<i32>` as the name of the struct it became,
    /// so a type written out and an inferred one mean the same from here on.
    /// Each spec was resolved, and anything wrong with it reported, when it
    /// was first checked, so resolving it again here adds no errors.
    fn name_instantiations(&mut self, program: &mut Program) {
        let reported = std::mem::take(&mut self.errors);
        for stmt in &mut program.statements {
            map_types(stmt, &mut |spec| self.name_spec(spec));
        }
        self.errors = reported;
    }

    fn name_spec(&mut self, spec: &mut TypeSpec) {
        match spec {
            TypeSpec::Generic { args, .. } => {
                for arg in args.iter_mut() {
                    self.name_spec(arg);
                }
                let TypeSpec::Generic { name, .. } = &*spec else {
                    return;
                };
                if !self.generic_structs.contains_key(name) {
                    return;
                }
                if let Type::Struct(name) = self.resolve_spec(spec, Span::default()) {
                    *spec = TypeSpec::Named(name);
                }
            }
            TypeSpec::Result(ok, error) => {
                self.name_spec(ok);
                error.iter_mut().for_each(|error| self.name_spec(error));
            }
            TypeSpec::Pointer(inner)
            | TypeSpec::Optional(inner)
            | TypeSpec::Slice(inner)
            | TypeSpec::Ref(inner)
            | TypeSpec::RefMut(inner) => self.name_spec(inner),
            TypeSpec::Tuple(types) => {
                for ty in types.iter_mut() {
                    self.name_spec(ty);
                }
            }
            TypeSpec::Named(_) | TypeSpec::IntLiteral(_) => {}
        }
    }

    /// A struct or enum that stores itself, directly or through another, has
    /// no finite size. Reported here so codegen never tries to lay one out.
    fn check_recursive_structs(&mut self, stmts: &[Statement]) {
        let types: HashMap<&str, Vec<&TypeSpec>> = stmts
            .iter()
            .filter_map(|stmt| match &stmt.kind {
                StatementKind::Struct { name, fields, .. } => {
                    Some((name.as_str(), fields.iter().map(|(_, spec)| spec).collect()))
                }
                StatementKind::Enum { name, variants } => Some((
                    name.as_str(),
                    variants.iter().flat_map(|(_, f)| f).collect(),
                )),
                _ => None,
            })
            .collect();

        for stmt in stmts {
            let (StatementKind::Struct { name, .. } | StatementKind::Enum { name, .. }) =
                &stmt.kind
            else {
                continue;
            };
            if Self::stores_by_value(name, name, &types, &mut HashSet::new()) {
                self.error(
                    format!("'{name}' stores itself, so it has no finite size"),
                    stmt.span,
                );
            }
        }
    }

    fn stores_by_value(
        from: &str,
        target: &str,
        types: &HashMap<&str, Vec<&TypeSpec>>,
        seen: &mut HashSet<String>,
    ) -> bool {
        let Some(fields) = types.get(from) else {
            return false;
        };

        let mut deps = Vec::new();
        for spec in fields {
            Self::value_dependencies(spec, &mut deps);
        }
        deps.into_iter().any(|dep| {
            dep == target
                || (seen.insert(dep.clone()) && Self::stores_by_value(&dep, target, types, seen))
        })
    }

    /// Type names a field stores inline. Pointers, references, slices and `Vec`
    /// keep their payload elsewhere, so they break a cycle.
    fn value_dependencies(spec: &TypeSpec, out: &mut Vec<String>) {
        match spec {
            TypeSpec::Named(name) => out.push(name.clone()),
            TypeSpec::Tuple(types) => {
                types.iter().for_each(|t| Self::value_dependencies(t, out));
            }
            TypeSpec::Optional(inner) | TypeSpec::Result(inner, _) => {
                Self::value_dependencies(inner, out);
            }
            TypeSpec::Generic { name, args } if name == "Array" => {
                if let Some(elem) = args.first() {
                    Self::value_dependencies(elem, out);
                }
            }
            _ => {}
        }
    }

    fn scan_types(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Struct { name, .. } => {
                    if self.struct_defs.contains_key(name) || self.enum_defs.contains_key(name) {
                        self.error(format!("Type {name} is already defined"), stmt.span);
                        continue;
                    }

                    self.struct_defs.insert(name.clone(), Vec::new());
                }
                StatementKind::Enum { name, variants } => {
                    if self.enum_defs.contains_key(name) || self.struct_defs.contains_key(name) {
                        self.error(format!("Type '{name}' is already defined"), stmt.span);
                        continue;
                    }

                    let names: Vec<String> = variants.iter().map(|(v, _)| v.clone()).collect();
                    if let Some(dup) = Self::first_duplicate(&names) {
                        self.error(
                            format!("Enum '{name}' declares variant '{dup}' twice"),
                            stmt.span,
                        );
                    }

                    let variants = names.into_iter().map(|v| (v, Vec::new())).collect();
                    self.enum_defs.insert(name.clone(), variants);
                }
                StatementKind::Trait { name, methods } => {
                    if self.trait_defs.contains_key(name) {
                        self.error(format!("Trait '{name}' is already defined"), stmt.span);
                        continue;
                    }

                    let mut trait_methods = Vec::new();
                    for method in methods {
                        let param_types: Vec<Type> = method
                            .params
                            .iter()
                            .map(|(_, ty, _)| self.resolve_spec(ty, stmt.span))
                            .collect();
                        let ret_type = method
                            .return_type
                            .as_ref()
                            .map(|t| self.resolve_spec(t, stmt.span));
                        trait_methods.push((method.name.clone(), param_types, ret_type));
                    }
                    self.trait_defs.insert(name.clone(), trait_methods);
                }
                _ => {}
            }
        }

        for stmt in stmts {
            self.scan_struct_fields(stmt);
            self.scan_variant_fields(stmt);
        }
    }

    fn scan_variant_fields(&mut self, stmt: &Statement) {
        let StatementKind::Enum { name, variants } = &stmt.kind else {
            return;
        };
        let resolved: Vec<Variant> = variants
            .iter()
            .map(|(variant, fields)| {
                let fields = fields.iter().map(|spec| self.resolve_spec(spec, stmt.span));
                (variant.clone(), fields.collect())
            })
            .collect();
        if let Some(entry) = self.enum_defs.get_mut(name)
            && entry.len() == resolved.len()
        {
            *entry = resolved;
        }
    }

    fn scan_struct_fields(&mut self, stmt: &Statement) {
        let StatementKind::Struct { name, fields, .. } = &stmt.kind else {
            return;
        };

        let mut resolved: Vec<(String, Type)> = Vec::with_capacity(fields.len());
        for (field_name, spec) in fields {
            if resolved.iter().any(|(seen, _)| seen == field_name) {
                self.error(
                    format!("Struct '{name}' declares field '{field_name}' twice"),
                    stmt.span,
                );
                continue;
            }
            let ty = self.resolve_spec(spec, stmt.span);
            resolved.push((field_name.clone(), ty));
        }

        if let Some(fields) = self.struct_defs.get_mut(name) {
            *fields = resolved;
        }
    }

    fn scan_functions(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            match &stmt.kind {
                StatementKind::Function {
                    name,
                    params,
                    return_type,
                    type_params,
                    ..
                } => {
                    self.register_function(
                        name.clone(),
                        params,
                        return_type,
                        None,
                        type_params,
                        stmt.span,
                    );
                }

                StatementKind::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let StatementKind::Function {
                            name: method_name,
                            params,
                            return_type,
                            type_params,
                            ..
                        } = &method.kind
                        {
                            let full_name = format!("{struct_name}::{method_name}");
                            self.register_function(
                                full_name,
                                params,
                                return_type,
                                Some(struct_name),
                                type_params,
                                method.span,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn first_duplicate(names: &[String]) -> Option<&String> {
        let mut seen = HashSet::new();
        names.iter().find(|name| !seen.insert(*name))
    }

    fn register_function(
        &mut self,
        name: String,
        params: &Vec<(String, TypeSpec, bool)>,
        return_type: &Option<TypeSpec>,
        associated_struct: Option<&str>,
        type_params: &[crate::ast::TypeParameter],
        span: Span,
    ) {
        if self.symbols.lookup_current_scope(&name).is_some() {
            self.error(format!("Function '{name}' is already defined"), span);
            return;
        }

        if let Some(dup) =
            Self::first_duplicate(&params.iter().map(|(n, _, _)| n.clone()).collect::<Vec<_>>())
        {
            self.error(
                format!("Function '{name}' declares parameter '{dup}' twice"),
                span,
            );
        }

        if name == "main" {
            if !params.is_empty() {
                self.error("Function 'main' must not take arguments".into(), span);
            }

            if let Some(rt_spec) = return_type {
                let ret_ty = self.resolve_spec(rt_spec, span);
                if ret_ty != Type::Void {
                    self.error(
                        format!("Function 'main' must return void, not {ret_ty}"),
                        span,
                    );
                }
            }
        }

        if matches!(params.first(), Some((first, _, true)) if first == "self") {
            self.mut_self_methods.insert(name.clone());
        }
        if associated_struct.is_some()
            && name.ends_with("::drop")
            && (params.len() != 1
                || !self.mut_self_methods.contains(&name)
                || return_type.is_some())
        {
            self.error(
                "A 'drop' method takes only 'var self' and returns nothing".into(),
                span,
            );
        }

        let prev_type_params = std::mem::take(&mut self.current_type_params);
        self.current_type_params = type_params.iter().map(|tp| tp.name.clone()).collect();

        let mut param_types = Vec::new();
        for (param_name, type_spec, _is_mut) in params {
            if param_name == "self" {
                if let Some(struct_name) = associated_struct {
                    if self.struct_defs.contains_key(struct_name) {
                        param_types.push(Type::Struct(struct_name.to_string()))
                    } else {
                        param_types.push(Type::Unknown);
                        self.error("Self used in unknown struct context".into(), span);
                    }
                } else {
                    self.error(
                        "'self' parameter allowed only in struct methods".into(),
                        span,
                    );
                    param_types.push(Type::Unknown);
                }
            } else {
                let ty = self.resolve_spec(type_spec, span);
                if ty == Type::Void {
                    self.error(format!("Parameter '{param_name}' cannot be void"), span);
                }
                param_types.push(ty);
            }
        }

        let ret_ty = if let Some(rt_spec) = return_type {
            self.resolve_spec(rt_spec, span)
        } else {
            Type::Void
        };

        self.current_type_params = prev_type_params;

        self.symbols.insert_fn(name.clone(), param_types, ret_ty);
    }

    /// Globals first, each after the ones it names, so a function or a
    /// constant may use a constant declared below it.
    fn analyze_bodies(&mut self, stmts: &mut [Statement]) {
        let (mut globals, rest): (Vec<_>, Vec<_>) = stmts
            .iter_mut()
            .partition(|stmt| matches!(stmt.kind, StatementKind::Var { .. }));
        while !globals.is_empty() {
            let waiting: HashSet<String> = globals.iter().map(|g| Self::global_name(g)).collect();
            let ready = globals
                .iter_mut()
                .position(|global| {
                    let mut names = crate::ast::Names::default();
                    crate::ast::walk_item(&mut names, global);
                    names.0.iter().all(|name| !waiting.contains(name))
                })
                .unwrap_or(0);
            let global = globals.remove(ready);
            self.check_statement_top_level(global);
        }
        for stmt in rest {
            self.check_statement_top_level(stmt);
        }
    }

    fn global_name(global: &Statement) -> String {
        match &global.kind {
            StatementKind::Var { name, .. } => name.clone(),
            _ => String::new(),
        }
    }

    fn check_statement_top_level(&mut self, stmt: &mut Statement) {
        let span = stmt.span;
        if let StatementKind::Var { name, .. } = &stmt.kind {
            self.current_item = name.clone();
        }
        match &mut stmt.kind {
            StatementKind::Function {
                name,
                type_params,
                params,
                body,
                ..
            } => self.check_function_body(name, params, body, type_params, span),
            StatementKind::Struct {
                name: struct_name,
                methods,
                ..
            } => {
                for method in methods.iter_mut() {
                    let span = method.span;
                    if let StatementKind::Function {
                        name,
                        type_params,
                        params,
                        body,
                        ..
                    } = &mut method.kind
                    {
                        let full_name = format!("{struct_name}::{name}");
                        self.check_function_body(&full_name, params, body, type_params, span);
                    }
                }
            }
            StatementKind::Var { name, .. } => {
                if self.symbols.lookup_current_scope(name).is_some() {
                    self.error(format!("'{name}' is already defined"), span);
                }
                self.check_statement(stmt);
                if let StatementKind::Var { name, value, .. } = &stmt.kind {
                    if !self.is_constant(value) {
                        self.error(
                            "A global constant must be made of literals, other constants, enum variants and operators on them".into(),
                            value.span,
                        );
                    }
                    self.constants.insert(name.clone(), value.clone());
                }
            }
            _ => {}
        }
    }

    /// What a global constant may be made of. Codegen lowers its value at each
    /// use, so a call in it would run once per use instead of once.
    fn is_constant(&self, expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Int(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::Boolean(_)
            | ExpressionKind::StringLit(_) => true,
            ExpressionKind::Identifier(name) => {
                matches!(expr.ty, Some(Type::Enum(_)))
                    || matches!(
                        self.symbols.lookup(name),
                        Some(super::symbol_table::Symbol::Var { is_const: true, .. })
                    )
            }
            ExpressionKind::Prefix { right: inner, .. }
            | ExpressionKind::Cast { left: inner, .. }
            | ExpressionKind::ArrayRepeat { value: inner, .. } => self.is_constant(inner),
            ExpressionKind::Infix { left, right, .. } => {
                self.is_constant(left) && self.is_constant(right)
            }
            ExpressionKind::ArrayLiteral(items) | ExpressionKind::Tuple(items) => {
                items.iter().all(|item| self.is_constant(item))
            }
            _ => false,
        }
    }

    /// Whether control never falls off the end of `body`: every path ends in a
    /// `return`, or with `jumps`, in a `break` or `continue` too.
    /// Conservative: a loop may run zero times, so it never counts.
    fn always_leaves(body: &[Statement], jumps: bool) -> bool {
        body.iter().any(|stmt| match &stmt.kind {
            StatementKind::Return(_) => true,
            StatementKind::Break | StatementKind::Continue => jumps,
            StatementKind::Block(inner) => Self::always_leaves(inner, jumps),
            StatementKind::If {
                then_branch,
                else_branch: Some(else_branch),
                ..
            } => {
                Self::always_leaves(std::slice::from_ref(then_branch), jumps)
                    && Self::always_leaves(std::slice::from_ref(else_branch), jumps)
            }
            _ => false,
        })
    }

    /// Check a loop, `turn` being what runs on every turn and saying whether
    /// it always leaves. A variable from outside must hold a value again
    /// wherever the loop goes round, or the next turn would use it moved.
    fn check_loop(&mut self, turn: impl FnOnce(&mut Self) -> bool) {
        let before = self.symbols.moves();
        self.loops.push(LoopFrame {
            depth: self.symbols.depth(),
            before: before.clone(),
            exits: Vec::new(),
        });
        let leaves = turn(self);
        if !leaves {
            self.check_back_edge();
        }
        let frame = self.loops.pop().expect("pushed above");

        let mut after = before;
        if !leaves {
            after.extend(self.symbols.moves());
        }
        after.extend(frame.exits.into_iter().flatten());
        self.symbols.restore_moves(&after);
    }

    /// Going round again: report each outer variable moved since the loop
    /// began.
    fn check_back_edge(&mut self) {
        let Some(frame) = self.loops.last() else {
            return;
        };
        let repeated: Vec<(String, Span)> = self
            .symbols
            .moves()
            .into_iter()
            .filter(|(depth, name, _)| {
                *depth < frame.depth
                    && !frame.before.iter().any(|(d, n, _)| d == depth && n == name)
            })
            .map(|(_, name, span)| (name, span))
            .collect();
        for (name, span) in repeated {
            if !self.errors.iter().any(|error| error.span == span) {
                self.error(
                    format!("Cannot move '{name}' inside a loop: the next turn would use it again"),
                    span,
                );
            }
        }
    }

    fn check_function_body(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
        body: &mut [Statement],
        type_params: &[crate::ast::TypeParameter],
        span: Span,
    ) {
        let Some(function_symbol) = self.symbols.lookup(name).cloned() else {
            return;
        };
        self.current_item = name.to_string();

        if let super::symbol_table::Symbol::Function {
            ret_type,
            params: params_type_def,
        } = function_symbol
        {
            let prev_ret = self.current_fn_return_type.replace(ret_type);
            let prev_type_params = std::mem::take(&mut self.current_type_params);
            self.current_type_params = type_params.iter().map(|tp| tp.name.clone()).collect();
            self.symbols.enter_scope();

            for (i, (param_name, _, is_mut)) in params.iter().enumerate() {
                let ty = params_type_def.get(i).unwrap_or(&Type::Unknown).clone();
                let is_const = !is_mut;
                self.symbols
                    .insert_var(param_name.clone(), ty, is_const, param_name == "self");
            }

            for s in body.iter_mut() {
                self.check_statement(s);
            }

            if !matches!(self.current_fn_return_type, Some(Type::Void))
                && name != "main"
                && !Self::always_leaves(body, false)
            {
                let span = body.last().map_or(span, |s| s.span);
                self.error(
                    format!("Function '{name}' can finish without returning a value"),
                    span,
                );
            }

            self.symbols.exit_scope();
            self.current_type_params = prev_type_params;
            self.current_fn_return_type = prev_ret;
        }
    }

    fn resolve_spec(&mut self, spec: &TypeSpec, span: Span) -> Type {
        match spec {
            TypeSpec::Named(name) => self.resolve_named_type(name, span),
            TypeSpec::Generic { name, args } => {
                if name == "Array" && args.len() == 2 {
                    let elem_type = self.resolve_spec(&args[0], span);

                    let len = if let TypeSpec::IntLiteral(val) = args[1] {
                        val as usize
                    } else {
                        self.error("Array length must be an integer literal".into(), span);
                        0
                    };

                    return Type::Array {
                        elem_type: Box::new(elem_type),
                        len,
                    };
                }
                if name == "Vec" && args.len() == 1 {
                    let elem_type = self.resolve_spec(&args[0], span);
                    return Type::Vec {
                        elem_type: Box::new(elem_type),
                    };
                }
                if self.generic_structs.contains_key(name) {
                    let args: Vec<Type> = args.iter().map(|a| self.resolve_spec(a, span)).collect();
                    return match self.instantiate_struct(name, &args, span) {
                        Some(name) => Type::Struct(name),
                        None => Type::Unknown,
                    };
                }
                self.error(
                    format!("Unknown generic type or invalid args: {}", name),
                    span,
                );
                Type::Unknown
            }
            TypeSpec::IntLiteral(_) => {
                self.error("Unexpected integer literal in type position".into(), span);
                Type::Unknown
            }
            TypeSpec::Tuple(types) => {
                let resolved: Vec<Type> =
                    types.iter().map(|t| self.resolve_spec(t, span)).collect();
                Type::Tuple(resolved)
            }
            TypeSpec::Pointer(inner) => {
                let elem_type = self.resolve_spec(inner, span);
                Type::Pointer(Box::new(elem_type))
            }
            TypeSpec::Optional(inner) => {
                let elem_type = self.resolve_spec(inner, span);
                Type::Optional(Box::new(elem_type))
            }
            TypeSpec::Result(ok, error) => {
                let ok_type = self.resolve_spec(ok, span);
                let err_type = match error {
                    None => Self::error_type(),
                    Some(error) => match self.resolve_spec(error, span) {
                        Type::Enum(name) if self.enum_has_data(&name) => {
                            self.error(
                                format!("An error enum carries no values, and {name} does"),
                                span,
                            );
                            Type::Unknown
                        }
                        ty @ (Type::Enum(_) | Type::Unknown) => ty,
                        other => {
                            self.error(format!("An error type is an enum, not {other}"), span);
                            Type::Unknown
                        }
                    },
                };
                Type::Result {
                    ok_type: Box::new(ok_type),
                    err_type: Box::new(err_type),
                }
            }
            TypeSpec::Slice(inner) => {
                let elem_type = self.resolve_spec(inner, span);
                Type::Slice {
                    elem_type: Box::new(elem_type),
                }
            }
            TypeSpec::Ref(inner) => {
                let elem_type = self.resolve_spec(inner, span);
                Type::Ref(Box::new(elem_type))
            }
            TypeSpec::RefMut(inner) => {
                let elem_type = self.resolve_spec(inner, span);
                Type::RefMut(Box::new(elem_type))
            }
        }
    }

    fn resolve_named_type(&mut self, name: &str, span: Span) -> Type {
        if self.current_type_params.contains(&name.to_string()) {
            return Type::ParamType(name.to_string());
        }

        if self.struct_defs.contains_key(name) {
            self.check_visible(name, span);
            return Type::Struct(name.to_string());
        }
        if self.enum_defs.contains_key(name) {
            self.check_visible(name, span);
            return Type::Enum(name.to_string());
        }

        static PRIMITIVES: &[(&str, Signedness, IntWidth)] = &[
            ("i8", Signedness::Signed, IntWidth::W8),
            ("u8", Signedness::Unsigned, IntWidth::W8),
            ("i16", Signedness::Signed, IntWidth::W16),
            ("u16", Signedness::Unsigned, IntWidth::W16),
            ("i32", Signedness::Signed, IntWidth::W32),
            ("u32", Signedness::Unsigned, IntWidth::W32),
            ("i64", Signedness::Signed, IntWidth::W64),
            ("u64", Signedness::Unsigned, IntWidth::W64),
            ("isize", Signedness::Signed, IntWidth::WSize),
            ("usize", Signedness::Unsigned, IntWidth::WSize),
        ];
        for (type_name, signed, width) in PRIMITIVES {
            if name == *type_name {
                return Type::Integer {
                    signed: *signed,
                    width: *width,
                };
            }
        }

        match name {
            "f32" => return Type::Float(FloatWidth::W32),
            "f64" => return Type::Float(FloatWidth::W64),
            "bool" => return Type::Bool,
            "void" => return Type::Void,
            "self" => return Type::Unknown,
            _ => {}
        }

        let candidates: Vec<&str> = PRIMITIVES
            .iter()
            .map(|(n, _, _)| *n)
            .chain(["f32", "f64", "bool"].iter().copied())
            .chain(self.struct_defs.keys().map(|s| s.as_str()))
            .chain(self.enum_defs.keys().map(|s| s.as_str()))
            .collect();

        if let Some(suggestion) = self.find_closest_match(name, &candidates) {
            self.error(
                format!("Unknown type '{}'. Did you mean '{}'?", name, suggestion),
                span,
            );
        } else {
            self.error(format!("Unknown type '{}'", name), span);
        }
        Type::Unknown
    }

    fn find_closest_match<'a>(&self, name: &str, candidates: &[&'a str]) -> Option<&'a str> {
        candidates
            .iter()
            .map(|c| (*c, Self::levenshtein_distance(name, c)))
            .filter(|(_, dist)| *dist <= 2)
            .min_by_key(|(_, dist)| *dist)
            .map(|(name, _)| name)
    }

    fn check_var_declaration(
        &mut self,
        name: &str,
        is_const: bool,
        value: &mut Expression,
        type_annotation: &Option<TypeSpec>,
        span: Span,
    ) -> Type {
        let expected_type = type_annotation
            .as_ref()
            .map(|spec| self.resolve_spec(spec, span));

        let reported = self.errors.len();
        let value_type = self.check_expression(value, expected_type.as_ref());

        let final_type = if let Some(expected) = expected_type {
            if !expected.accepts(&value_type) && value_type != Type::Unknown {
                self.error(
                    format!(
                        "Type mismatch for variable '{name}'. Annotated as {} but got {}",
                        expected, value_type
                    ),
                    span,
                );
            }

            expected
        } else {
            if value_type == Type::Unknown && self.errors.len() == reported {
                self.error(
                    format!(
                        "Cannot infer type for variable '{}'. Please add a type annotation.",
                        name
                    ),
                    span,
                );
            }
            value_type.clone()
        };

        self.consume(value, &final_type);
        self.symbols
            .insert_var(name.to_string(), final_type.clone(), is_const, false);
        final_type
    }

    /// `for i in a..b` counts; `for x in v` reads each element of an array or
    /// a Vec; `for x in &var v` lets each be written through `x`, so `v` must
    /// not change under it meanwhile.
    fn check_for_in(&mut self, variable: &str, iterable: &mut Expression, body: &mut Statement) {
        let iter_span = iterable.span;
        let iterable_type = self.check_expression(iterable, None);

        let (walked, writable) = match iterable_type {
            Type::RefMut(inner) => (*inner, true),
            other => (other, false),
        };
        let item_type = match walked {
            Type::Integer { .. } if matches!(iterable.kind, ExpressionKind::Range { .. }) => walked,
            Type::Array { elem_type, .. } | Type::Vec { elem_type } => *elem_type,
            Type::Unknown => Type::Unknown,
            _ => {
                let shown = iterable.ty.clone().unwrap_or(Type::Unknown);
                self.error(format!("{shown} is not iterable"), iter_span);
                Type::Unknown
            }
        };

        let locked = match &iterable.kind {
            ExpressionKind::BorrowRefMut(inner) if writable => Self::place_path(inner),
            _ => None,
        };
        self.locked.extend(locked.clone());
        self.check_loop(|this| {
            this.symbols.enter_scope();
            this.symbols
                .insert_var(variable.to_string(), item_type, !writable, true);
            this.check_statement(body);
            this.symbols.exit_scope();
            Self::always_leaves(std::slice::from_ref(body), true)
        });
        if locked.is_some() {
            self.locked.pop();
        }
    }

    /// Type two operands. A number written out takes the type of the other
    /// one, on either side: `3 < x` compares at the type of `x`, as `x > 3`.
    fn check_operands(
        &mut self,
        left: &mut Expression,
        right: &mut Expression,
        expected_type: Option<&Type>,
    ) -> (Type, Type) {
        if Self::is_number_literal(left) && !Self::is_number_literal(right) {
            let r_ty = self.check_expression(right, expected_type);
            (self.check_expression(left, Some(&r_ty)), r_ty)
        } else {
            let l_ty = self.check_expression(left, expected_type);
            let r_ty = self.check_expression(right, Some(&l_ty));
            (l_ty, r_ty)
        }
    }

    fn check_range(&mut self, start: &mut Expression, end: &mut Expression, span: Span) -> Type {
        match self.check_operands(start, end, None) {
            (Type::Unknown, _) | (_, Type::Unknown) => Type::Unknown,
            (from @ Type::Integer { .. }, to) if from == to => from,
            (from, to) => {
                self.error(
                    format!("A range counts between two integers of one type, not {from} and {to}"),
                    span,
                );
                Type::Unknown
            }
        }
    }

    /// `a.b[]` for a place built of variables, fields and elements, so two
    /// places can be told to overlap.
    fn place_path(place: &Expression) -> Option<String> {
        match &place.kind {
            ExpressionKind::Identifier(name) => Some(name.clone()),
            ExpressionKind::Get { object, name } => {
                Some(format!("{}.{name}", Self::place_path(object)?))
            }
            ExpressionKind::Index { left, .. } => Some(format!("{}[]", Self::place_path(left)?)),
            _ => None,
        }
    }

    fn check_statement(&mut self, stmt: &mut Statement) {
        let span = stmt.span;
        match &mut stmt.kind {
            StatementKind::Var {
                name,
                is_const,
                value,
                type_annotation,
                ty,
            } => {
                *ty =
                    Some(self.check_var_declaration(name, *is_const, value, type_annotation, span))
            }

            StatementKind::Return(opt_expr) => {
                let expected = self.current_fn_return_type.clone();

                let expr_type = if let Some(expr) = opt_expr {
                    let ty = self.check_expression(expr, expected.as_ref());
                    self.consume(expr, &ty);
                    ty
                } else {
                    Type::Void
                };

                if let Some(expected) = expected {
                    if !expected.accepts(&expr_type) {
                        self.error(
                            format!(
                                "Invalid return type. Function expects {}, returning {}",
                                expected, expr_type
                            ),
                            span,
                        );
                    }
                } else {
                    self.error(
                        "Return statement illegal if not inside a function".into(),
                        span,
                    );
                }
            }

            StatementKind::Block(stmts) => {
                self.symbols.enter_scope();
                for s in stmts.iter_mut() {
                    self.check_statement(s);
                }
                self.symbols.exit_scope();
            }

            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_span = condition.span;
                let cond_type = self.check_expression(condition, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    self.error(
                        format!("If condition must be boolean, got {}", cond_type),
                        cond_span,
                    );
                }

                let before = self.symbols.moves();
                let mut after = Moves::new();
                self.check_statement(then_branch);
                if !Self::always_leaves(std::slice::from_ref(then_branch), true) {
                    after.extend(self.symbols.moves());
                }
                self.symbols.restore_moves(&before);
                match else_branch {
                    Some(else_stmt) => {
                        self.check_statement(else_stmt);
                        if !Self::always_leaves(std::slice::from_ref(else_stmt), true) {
                            after.extend(self.symbols.moves());
                        }
                    }
                    None => after.extend(before),
                }
                self.symbols.restore_moves(&after);
            }

            StatementKind::While { cond, body } => self.check_loop(|this| {
                let cond_span = cond.span;
                let cond_type = this.check_expression(cond, Some(&Type::Bool));
                if cond_type != Type::Bool && cond_type != Type::Unknown {
                    this.error(
                        format!("While condition must be boolean, got: {}", cond_type),
                        cond_span,
                    );
                }
                this.check_statement(body);
                Self::always_leaves(std::slice::from_ref(body), true)
            }),

            StatementKind::Break | StatementKind::Continue if self.loops.is_empty() => {
                self.error("Break/Continue can only be used inside loops".into(), span);
            }
            StatementKind::Break | StatementKind::Continue => {
                if matches!(stmt.kind, StatementKind::Continue) {
                    self.check_back_edge();
                }
                let moves = self.symbols.moves();
                if let Some(frame) = self.loops.last_mut() {
                    frame.exits.push(moves);
                }
            }

            StatementKind::Expression(expr) => {
                self.check_expression(expr, None);
            }

            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => self.check_for_in(variable, iterable, body),

            _ => {}
        }
    }

    fn check_expression(&mut self, expr: &mut Expression, expected_type: Option<&Type>) -> Type {
        let ty = self.check_expression_inner(expr, expected_type);
        expr.ty = Some(ty.clone());
        ty
    }

    fn check_identifier(&mut self, name: &str, span: Span) -> Type {
        if let Some((enum_name, variant)) = name.rsplit_once("::")
            && let Some(variants) = self.enum_defs.get(enum_name).cloned()
        {
            self.check_visible(enum_name, span);
            match variants.iter().find(|(v, _)| v == variant) {
                Some((_, fields)) if !fields.is_empty() => {
                    let message = format!(
                        "'{name}' carries {} value(s): write {name}(...)",
                        fields.len()
                    );
                    self.error(message, span);
                    return Type::Unknown;
                }
                Some(_) => return Type::Enum(enum_name.to_string()),
                None => {}
            }
            self.error(
                format!("Enum '{enum_name}' has no variant '{variant}'"),
                span,
            );
            return Type::Unknown;
        }

        if self.symbols.is_global(name) {
            self.check_visible(name, span);
        }
        if let Some(symbol) = self.symbols.lookup(name).cloned() {
            match symbol {
                super::symbol_table::Symbol::Var { ty, moved_at, .. } => {
                    if moved_at.is_some() {
                        self.error(
                                format!(
                                    "Use of moved value '{}'. Value was previously moved and is no longer valid.",
                                    name
                                ),
                                span,
                            );
                        return Type::Unknown;
                    }
                    ty
                }
                _ => {
                    self.error(format!("'{name}' is a function, not a variable"), span);
                    Type::Unknown
                }
            }
        } else {
            let suggestion = self.find_similar_variable(name);
            if let Some(similar) = suggestion {
                self.error(
                    format!(
                        "Undeclared variable '{}'. Did you mean '{}'?",
                        name, similar
                    ),
                    span,
                );
            } else {
                self.error(format!("Undeclared variable '{name}'."), span);
            }
            Type::Unknown
        }
    }

    /// Whether a write can reach `expr`. Only a place rooted in a variable or a
    /// dereference has storage; a call result or a literal is a temporary, and
    /// assigning to one would write somewhere nothing reads again.
    fn is_assignable(expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Identifier(_) | ExpressionKind::Dereference(_) => true,
            ExpressionKind::Get { object, .. } => Self::is_assignable(object),
            ExpressionKind::Index { left, .. } => Self::is_assignable(left),
            _ => false,
        }
    }

    fn check_assign(
        &mut self,
        target: &mut Expression,
        value: &mut Expression,
        span: Span,
    ) -> Type {
        if !Self::is_assignable(target) {
            self.error(
                "Cannot assign to a temporary value".to_string(),
                target.span,
            );
        }

        let target_type = if let ExpressionKind::Identifier(name) = &target.kind {
            if let Some(super::symbol_table::Symbol::Var { is_const, ty, .. }) =
                self.symbols.lookup(name).cloned()
            {
                if is_const {
                    self.error(
                        format!("Cannot reassign constant variable '{}'.", name),
                        span,
                    );
                }
                self.require_unlocked(target, span);
                Some(ty)
            } else {
                None
            }
        } else {
            None
        };

        let target_ty = match &target_type {
            Some(ty) if matches!(target.kind, ExpressionKind::Identifier(_)) => {
                target.ty = Some(ty.clone());
                ty.clone()
            }
            _ => {
                let ty = self.check_expression(target, target_type.as_ref());
                self.require_mutable(target, span);
                ty
            }
        };

        if let ExpressionKind::Index { left, .. } = &target.kind
            && matches!(left.ty, Some(Type::Slice { .. }))
        {
            self.error(
                "Cannot write through a slice, which may point at read-only memory".into(),
                span,
            );
            return Type::Void;
        }

        let resolved_target = target_type.unwrap_or(target_ty);
        let val_type = self.check_expression(value, Some(&resolved_target));

        if !resolved_target.accepts(&val_type)
            && val_type != Type::Unknown
            && resolved_target != Type::Unknown
        {
            self.error(
                format!(
                    "Type mismatch in assignment. Expected {}, got {}.",
                    resolved_target, val_type
                ),
                span,
            );
        }
        self.consume(value, &val_type);
        if let ExpressionKind::Identifier(name) = &target.kind {
            self.symbols.set_moved(name, None);
        }
        Type::Void
    }

    /// Report a write into `place` when the variable it belongs to is not
    /// declared `var`. Behind a pointer the storage is the pointee's, which a
    /// `*T` or `&var T` may write and a `&T` may not.
    fn require_mutable(&mut self, place: &Expression, span: Span) {
        self.require_unlocked(place, span);
        self.require_var_root(place, span);
    }

    fn require_var_root(&mut self, place: &Expression, span: Span) {
        if matches!(place.ty, Some(Type::Pointer(_) | Type::RefMut(_))) {
            return;
        }
        match &place.kind {
            ExpressionKind::Identifier(name) => {
                if let Some(super::symbol_table::Symbol::Var { is_const: true, .. }) =
                    self.symbols.lookup(name)
                {
                    self.error(
                        format!("Cannot modify '{name}', which is not declared 'var'"),
                        span,
                    );
                }
            }
            ExpressionKind::Get { object, .. } | ExpressionKind::Index { left: object, .. } => {
                self.require_var_root(object, span)
            }
            ExpressionKind::Dereference(inner) if matches!(inner.ty, Some(Type::Ref(_))) => {
                self.error("Cannot write through a '&' reference".into(), span)
            }
            _ => {}
        }
    }

    /// Report a change to what an enclosing `for .. in &var` loop walks, or to
    /// what holds it.
    fn require_unlocked(&mut self, place: &Expression, span: Span) {
        let Some(path) = Self::place_path(place) else {
            return;
        };
        let overlaps = |locked: &String| {
            let within = |outer: &str, inner: &str| {
                inner == outer
                    || inner.starts_with(&format!("{outer}."))
                    || inner.starts_with(&format!("{outer}["))
            };
            within(locked, &path) || within(&path, locked)
        };
        if let Some(locked) = self.locked.iter().find(|locked| overlaps(locked)).cloned() {
            self.error(
                format!(
                    "Cannot change '{path}' while a loop or match around it refers into '{locked}'"
                ),
                span,
            );
        }
    }

    /// `expr` is given away by value: to a variable, a call, a field, a
    /// collection or the caller. A variable is moved and may not be used
    /// again. A value inside a field, an element or behind a pointer cannot be
    /// moved out at all: what holds it would keep a second owner of it.
    fn consume(&mut self, expr: &Expression, ty: &Type) {
        if !self.owns_heap(ty) {
            return;
        }
        match &expr.kind {
            ExpressionKind::Identifier(name) if !self.symbols.is_global(name) => {
                if let Some(super::symbol_table::Symbol::Var {
                    is_borrowed: true, ..
                }) = self.symbols.lookup(name)
                {
                    self.error(
                        format!("Cannot move '{name}', which is borrowed; call .copy() on it"),
                        expr.span,
                    );
                }
                self.symbols.set_moved(name, Some(expr.span));
                self.moves.insert(expr.span);
            }
            ExpressionKind::Get { .. }
            | ExpressionKind::Index { .. }
            | ExpressionKind::Dereference(_) => self.error(
                "Cannot move a value out of a field, an element or a pointer; call .copy() on it"
                    .into(),
                expr.span,
            ),
            _ => {}
        }
    }

    fn check_infix(
        &mut self,
        left: &mut Expression,
        operator: &crate::token::Token,
        right: &mut Expression,
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        let operator = operator.clone();
        let (l_ty, r_ty) = self.check_operands(left, right, expected_type);

        if l_ty == Type::Unknown || r_ty == Type::Unknown {
            return Type::Unknown;
        }

        if let Type::Pointer(_) = &l_ty
            && matches!(
                operator,
                crate::token::Token::Plus | crate::token::Token::Minus
            )
            && matches!(r_ty, Type::Integer { .. })
        {
            return l_ty;
        }

        if !l_ty.accepts(&r_ty) {
            self.error(
                format!(
                    "Binary operation '{operator}' requires operands of same type. Got {} and {}.",
                    l_ty, r_ty
                ),
                span,
            );
            return Type::Unknown;
        }

        if let Type::Enum(name) = &l_ty
            && self.enum_has_data(name)
        {
            self.error(
                format!("'{operator}' does not apply to {l_ty}, whose variants carry values; use a match"),
                span,
            );
            return Type::Unknown;
        }

        if !Self::operator_applies(&operator, &l_ty) {
            self.error(
                format!("Operator '{operator}' cannot be applied to {l_ty}"),
                span,
            );
            return Type::Unknown;
        }

        match operator {
            crate::token::Token::Eq
            | crate::token::Token::NotEq
            | crate::token::Token::Lt
            | crate::token::Token::Gt
            | crate::token::Token::Leq
            | crate::token::Token::Geq => Type::Bool,

            _ => l_ty,
        }
    }

    /// Whether `operator` means something for operands of type `ty`.
    fn operator_applies(operator: &crate::token::Token, ty: &Type) -> bool {
        use crate::token::Token as T;
        let number = matches!(ty, Type::Integer { .. } | Type::Float(_));
        match operator {
            _ if matches!(ty, Type::ParamType(_)) => true,
            T::Plus | T::Minus | T::Star | T::Slash | T::Mod => number,
            T::PlusWrap | T::MinusWrap | T::StarWrap => matches!(ty, Type::Integer { .. }),
            T::BitAnd | T::BitOr | T::BitXor => matches!(ty, Type::Integer { .. } | Type::Bool),
            T::ShiftLeft | T::ShiftRight => matches!(ty, Type::Integer { .. }),
            T::And | T::Or => *ty == Type::Bool,
            T::Eq | T::NotEq => {
                number || matches!(ty, Type::Bool | Type::Enum(_) | Type::Pointer(_))
            }
            T::Lt | T::Gt | T::Leq | T::Geq => {
                number || matches!(ty, Type::Enum(_) | Type::Pointer(_))
            }
            _ => true,
        }
    }

    /// Whether `as` turns a `from` into a `to`. Nothing becomes an enum: a
    /// number outside its variants would get past an exhaustive `match`.
    fn castable(from: &Type, to: &Type) -> bool {
        use Type::*;
        matches!(
            (from, to),
            (Unknown | ParamType(_), _)
                | (_, Unknown | ParamType(_))
                | (
                    Integer { .. } | Float(_) | Bool | Enum { .. },
                    Integer { .. } | Float(_) | Bool
                )
                | (
                    Pointer(_) | Ref(_) | RefMut(_) | Slice { .. },
                    Integer { .. } | Pointer(_)
                )
                | (Integer { .. }, Pointer(_))
        )
    }

    /// A number written in the source, whose type comes from its context.
    fn is_number_literal(expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Int(_) | ExpressionKind::Float(_) => true,
            ExpressionKind::Prefix {
                operator: crate::token::Token::Minus,
                right,
            } => Self::is_number_literal(right),
            _ => false,
        }
    }

    /// What a plain `T!` fails with: an i32 code.
    fn error_type() -> Type {
        Type::Integer {
            signed: Signedness::Signed,
            width: IntWidth::W32,
        }
    }

    /// `print("x is {}", x)`: a literal format with one `{}` per argument,
    /// each a number, a bool or a `str`.
    fn check_print(&mut self, arguments: &mut [Expression], span: Span) -> Type {
        let Some((format, values)) = arguments.split_first_mut() else {
            self.error("print takes a format string first".into(), span);
            return Type::Void;
        };
        let ExpressionKind::StringLit(text) = &format.kind else {
            self.error(
                "The format must be a string literal, as in print(\"{}\", x)".into(),
                format.span,
            );
            return Type::Void;
        };
        match crate::ast::format_pieces(text) {
            Err(message) => self.error(message, format.span),
            Ok(pieces) if pieces.len() - 1 != values.len() => self.error(
                format!(
                    "The format has {} '{{}}' but {} value(s) follow it",
                    pieces.len() - 1,
                    values.len()
                ),
                span,
            ),
            Ok(_) => {}
        }
        for value in values {
            let ty = self.check_expression(value, None);
            let printable = matches!(ty, Type::Integer { .. } | Type::Float(_) | Type::Bool)
                || ty == Self::str_type()
                || ty == Type::Unknown;
            if !printable {
                self.error(
                    format!("Cannot print {ty}: print takes numbers, bools and str"),
                    value.span,
                );
            }
        }
        Type::Void
    }

    fn str_type() -> Type {
        Type::Slice {
            elem_type: Box::new(Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::W8,
            }),
        }
    }

    pub fn variant_fields(&self, path: &str) -> Option<(String, Vec<Type>)> {
        let (name, variant) = path.rsplit_once("::")?;
        let (_, fields) = self
            .enum_variants(name)?
            .iter()
            .find(|(v, _)| v == variant)?;
        Some((name.to_string(), fields.clone()))
    }

    fn check_variant(&mut self, path: &str, arguments: &mut [Expression], span: Span) -> Type {
        let Some((name, fields)) = self.variant_fields(path) else {
            return Type::Unknown;
        };
        if arguments.len() != fields.len() {
            self.error(
                format!(
                    "'{path}' carries {} value(s), got {}",
                    fields.len(),
                    arguments.len()
                ),
                span,
            );
            return Type::Enum(name);
        }
        for (argument, field) in arguments.iter_mut().zip(&fields) {
            let ty = self.check_expression(argument, Some(field));
            if !field.accepts(&ty) {
                self.error(
                    format!("'{path}' takes {field} here, got {ty}"),
                    argument.span,
                );
            }
            self.consume(argument, &ty);
        }
        Type::Enum(name)
    }

    fn check_ok_constructor(
        &mut self,
        arguments: &mut [Expression],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        if arguments.len() != 1 {
            self.error("Ok() takes exactly one argument".into(), span);
            return Type::Unknown;
        }
        let (ok_hint, err_type) = match expected_type {
            Some(Type::Result { ok_type, err_type }) => {
                (Some(ok_type.as_ref().clone()), err_type.clone())
            }
            _ => (None, Box::new(Self::error_type())),
        };
        let inner_type = self.check_expression(&mut arguments[0], ok_hint.as_ref());
        self.consume(&arguments[0], &inner_type);
        Type::Result {
            ok_type: Box::new(inner_type),
            err_type,
        }
    }

    fn check_err_constructor(
        &mut self,
        arguments: &mut [Expression],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        if arguments.len() != 1 {
            self.error("Err() takes exactly one argument, the error".into(), span);
            return Type::Unknown;
        }
        let (ok_type, err_type) = match expected_type {
            Some(Type::Result { ok_type, err_type }) => (ok_type.clone(), err_type.clone()),
            _ => (Box::new(Type::Unknown), Box::new(Self::error_type())),
        };
        let found = self.check_expression(&mut arguments[0], Some(&err_type));
        if !err_type.accepts(&found) {
            self.error(format!("Err() takes {err_type}, got {found}"), span);
        }
        Type::Result { ok_type, err_type }
    }

    /// `try value`: the payload of a `T!`, or a return with its error, which
    /// the function must be able to return.
    fn check_try(&mut self, value: &mut Expression, span: Span) -> Type {
        let ty = self.check_expression(value, None);
        let Type::Result { ok_type, err_type } = &ty else {
            if ty != Type::Unknown {
                self.error(format!("'try' takes a T! value, not {ty}"), span);
            }
            return Type::Unknown;
        };
        match &self.current_fn_return_type {
            Some(Type::Result {
                err_type: returned, ..
            }) if returned == err_type => {}
            returns => {
                let returns = returns.clone().unwrap_or(Type::Void);
                self.error(
                    format!("'try' passes its {err_type} error on, which a function returning {returns} cannot"),
                    span,
                );
            }
        }
        let ok_type = ok_type.as_ref().clone();
        self.consume(value, &ty);
        ok_type
    }

    /// `value catch fallback` for a `T!`, `value orelse fallback` for a `T?`:
    /// the payload, or the fallback, evaluated only when there is none.
    fn check_fallback(
        &mut self,
        value: &mut Expression,
        operator: &crate::token::Token,
        fallback: &mut Expression,
        span: Span,
    ) -> Type {
        let ty = self.check_expression(value, None);
        let payload = match (operator, &ty) {
            (crate::token::Token::Catch, Type::Result { ok_type, .. })
            | (crate::token::Token::Orelse, Type::Optional(ok_type)) => ok_type.as_ref().clone(),
            (_, Type::Unknown) => Type::Unknown,
            _ => {
                let wants = match operator {
                    crate::token::Token::Catch => "a T! value",
                    _ => "a T? value",
                };
                self.error(format!("'{operator}' takes {wants}, not {ty}"), span);
                return Type::Unknown;
            }
        };
        let fallback_type = self.check_expression(fallback, Some(&payload));
        if !payload.accepts(&fallback_type) {
            self.error(
                format!("The fallback must be {payload}, got {fallback_type}"),
                fallback.span,
            );
        }
        self.consume(value, &ty);
        self.consume(fallback, &fallback_type);
        payload
    }

    fn check_call_expression(
        &mut self,
        function: &mut Expression,
        arguments: &mut [Expression],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        if let ExpressionKind::Identifier(path) = &function.kind
            && let Some((owner, name)) = path.rsplit_once("::")
            && (self.struct_defs.contains_key(owner) || self.generic_structs.contains_key(owner))
        {
            self.error(
                format!("Call a struct's function with '.': {owner}.{name}()"),
                function.span,
            );
        }

        if let ExpressionKind::Get { object, name } = &function.kind
            && let ExpressionKind::Identifier(type_name) = &object.kind
            && self.symbols.lookup(type_name).is_none()
        {
            let owner = match expected_type {
                _ if self.struct_defs.contains_key(type_name) => Some(type_name.clone()),
                Some(Type::Struct(instance))
                    if self.generic_structs.contains_key(type_name)
                        && instance.starts_with(&format!("{type_name}__")) =>
                {
                    Some(instance.clone())
                }
                _ if self.generic_structs.contains_key(type_name) => {
                    self.error(
                        format!("Cannot tell what {type_name}'s type arguments are here; give the variable a type, as in var x: {type_name}<..> = {type_name}.{name}()"),
                        span,
                    );
                    return Type::Unknown;
                }
                _ => None,
            };
            if let Some(owner) = owner {
                function.kind = ExpressionKind::Identifier(format!("{owner}::{name}"));
            }
        }

        let call_kind = match &function.kind {
            ExpressionKind::Identifier(name) => CallKind::Named(name.clone()),
            ExpressionKind::Get {
                object,
                name: method_name,
            } => {
                let is_vec_static =
                    matches!(&object.kind, ExpressionKind::Identifier(n) if n == "Vec");
                CallKind::Method {
                    method_name: method_name.clone(),
                    is_vec_static,
                }
            }
            _ => CallKind::Unknown,
        };

        match call_kind {
            CallKind::Named(name) if PRINTS.contains(&name.as_str()) => {
                self.check_print(arguments, span)
            }
            CallKind::Named(name) if self.variant_fields(&name).is_some() => {
                self.check_variant(&name, arguments, span)
            }
            CallKind::Named(name) if name == "Ok" => {
                self.check_ok_constructor(arguments, expected_type, span)
            }
            CallKind::Named(name) if name == "Err" => {
                self.check_err_constructor(arguments, expected_type, span)
            }
            CallKind::Named(name) => {
                let (ty, subs) = self.check_call_mut(&name, arguments, None, span);
                if let Some(instance) = self.instantiate_function(&name, &subs, span) {
                    function.kind = ExpressionKind::Identifier(instance);
                }
                ty
            }
            CallKind::Method {
                method_name,
                is_vec_static: true,
            } => {
                let elem_type = if let Some(Type::Vec { elem_type }) = expected_type {
                    elem_type.as_ref().clone()
                } else {
                    self.error(
                        "Cannot infer Vec element type. Please add a type annotation.".into(),
                        span,
                    );
                    Type::Unknown
                };
                self.check_vec_method_mut(&method_name, &elem_type, arguments, span)
            }
            CallKind::Method {
                method_name,
                is_vec_static: false,
            } => {
                let ExpressionKind::Get { object, .. } = &mut function.kind else {
                    unreachable!()
                };
                let obj_type = self.check_expression(object, None);
                let mutates = match &obj_type {
                    Type::Vec { .. } => VEC_MUTATORS.contains(&method_name.as_str()),
                    Type::Struct(name) => self
                        .mut_self_methods
                        .contains(&format!("{name}::{method_name}")),
                    _ => false,
                };
                if mutates {
                    self.require_mutable(object, span);
                }

                if method_name == "copy" && arguments.is_empty() {
                    if obj_type != Type::Unknown && !self.owns_heap(&obj_type) && !self.in_instance
                    {
                        self.error(
                            format!("{obj_type} holds no memory of its own, so it is copied already; drop the .copy()"),
                            span,
                        );
                    }
                    return obj_type;
                }

                if let Type::Vec { elem_type } = &obj_type {
                    let elem_type = elem_type.clone();
                    return self.check_vec_method_mut(&method_name, &elem_type, arguments, span);
                }

                if let Type::Ref(inner) | Type::RefMut(inner) = &obj_type
                    && let Type::Vec { elem_type } = inner.as_ref()
                {
                    let elem_type = elem_type.clone();
                    return self.check_vec_method_mut(&method_name, &elem_type, arguments, span);
                }

                if let Type::Slice { .. } = &obj_type {
                    if method_name == "len" && arguments.is_empty() {
                        return Type::Integer {
                            signed: Signedness::Unsigned,
                            width: IntWidth::WSize,
                        };
                    }
                    self.error(format!("Slice has no method '{}'", method_name), span);
                    return Type::Unknown;
                }

                if method_name == "unwrap"
                    && matches!(obj_type, Type::Result { .. } | Type::Optional(_))
                    && self.owns_heap(&obj_type)
                {
                    self.consume(object, &obj_type);
                }

                if let Type::Result { ok_type, err_type } = &obj_type {
                    let (ok_type, err_type) = (ok_type.clone(), err_type.clone());
                    return self.check_result_method(
                        &method_name,
                        &ok_type,
                        &err_type,
                        arguments,
                        span,
                    );
                }

                if let Type::Optional(inner) = &obj_type {
                    let inner = inner.as_ref().clone();
                    return self.check_option_method(&method_name, &inner, arguments, span);
                }

                let struct_name = match &obj_type {
                    Type::Struct(name) => name.clone(),
                    Type::Pointer(elem_type) => {
                        if let Type::Struct(name) = elem_type.as_ref() {
                            name.clone()
                        } else {
                            String::new()
                        }
                    }
                    _ => String::new(),
                };

                if struct_name.is_empty() {
                    if obj_type == Type::Unknown {
                        return Type::Unknown;
                    }
                    self.error(
                        format!("Cannot call method on non-struct type {}", obj_type),
                        span,
                    );
                    return Type::Unknown;
                }
                let full_name = format!("{struct_name}::{method_name}");
                self.check_call_mut(&full_name, arguments, Some(obj_type), span)
                    .0
            }
            CallKind::Unknown => {
                self.error("Invalid call expression".into(), span);
                Type::Unknown
            }
        }
    }

    fn check_field_access(&mut self, object: &mut Expression, name: &str, span: Span) -> Type {
        let obj_type = self.check_expression(object, None);

        let actual_type = if let Type::Pointer(elem_type) = &obj_type {
            elem_type.as_ref().clone()
        } else {
            obj_type.clone()
        };

        if let Type::Tuple(types) = &actual_type {
            return match name.parse::<usize>().ok().and_then(|at| types.get(at)) {
                Some(ty) => ty.clone(),
                None => {
                    self.error(format!("{actual_type} has no field '{name}'"), span);
                    Type::Unknown
                }
            };
        }

        if let Type::Struct(struct_name) = &actual_type {
            if let Some(fields) = self.struct_defs.get(struct_name) {
                if let Some((_, ty)) = fields.iter().find(|(field, _)| field == name) {
                    return ty.clone();
                }
                self.error(
                    format!("Struct '{struct_name}' has no field '{name}'"),
                    span,
                );
            }
        } else if obj_type != Type::Unknown {
            self.error("Cannot access property on non-struct type.".into(), span);
        }
        Type::Unknown
    }

    /// The struct a generic literal stands for. An expected type settles the
    /// parameters when there is one; otherwise they are read off the values.
    fn instantiate_from_literal(
        &mut self,
        base: &str,
        fields: &mut [(String, Expression)],
        expected_type: Option<&Type>,
        span: Span,
    ) -> String {
        let Some(decl) = self.generic_structs.get(base) else {
            return base.to_string();
        };
        let StatementKind::Struct {
            type_params,
            fields: declared,
            ..
        } = &decl.kind
        else {
            return base.to_string();
        };
        let (type_params, declared) = (type_params.clone(), declared.clone());

        if let Some(Type::Struct(name)) = expected_type
            && name.starts_with(&format!("{base}__"))
        {
            return name.clone();
        }

        let mut args = Vec::with_capacity(type_params.len());
        for param in &type_params {
            let spec = TypeSpec::Named(param.name.clone());
            let value = declared
                .iter()
                .find(|(_, declared_spec)| *declared_spec == spec)
                .and_then(|(name, _)| fields.iter_mut().find(|(field, _)| field == name));

            let Some((_, value)) = value else {
                self.error(
                    format!("Cannot tell what '{}' is here, name the type", param.name),
                    span,
                );
                return base.to_string();
            };
            args.push(self.check_expression(value, None));
        }

        self.instantiate_struct(base, &args, span)
            .unwrap_or_else(|| base.to_string())
    }

    fn check_struct_literal(
        &mut self,
        name: &str,
        fields: &mut [(String, Expression)],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        let name = &self.instantiate_from_literal(name, fields, expected_type, span);

        let Some(def_fields) = self.struct_defs.get(name).cloned() else {
            self.error(format!("Unknown struct type '{name}'."), span);
            return Type::Unknown;
        };
        self.check_visible(name, span);

        for (field_name, _) in fields.iter() {
            if !def_fields.iter().any(|(n, _)| n == field_name) {
                self.error(
                    format!("Unknown field '{}' in struct '{}'", field_name, name),
                    span,
                );
            }
        }

        for (def_name, def_type) in &def_fields {
            let found = fields.iter_mut().find(|(n, _)| n == def_name);
            if let Some((_, field_expr)) = found {
                let field_span = field_expr.span;
                let expr_type = match &field_expr.ty {
                    Some(ty) => ty.clone(),
                    None => self.check_expression(field_expr, Some(def_type)),
                };
                self.consume(field_expr, &expr_type);
                if !def_type.accepts(&expr_type) && expr_type != Type::Unknown {
                    self.error(
                        format!(
                            "Type mismatch: Field '{}' in struct '{}' expected {}, got {}.",
                            def_name, name, def_type, expr_type
                        ),
                        field_span,
                    );
                }
            } else {
                self.error(
                    format!("Missing field '{def_name}' in struct literal {name}"),
                    span,
                );
            }
        }
        Type::Struct(name.clone())
    }

    fn check_match(
        &mut self,
        value: &mut Expression,
        arms: &mut [(Expression, Expression)],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        let subject = self.check_expression(value, None);
        let bindings = self.check_patterns(&subject, arms, span);
        if arms.is_empty() {
            return Type::Void;
        }

        let locked = Self::place_path(value).filter(|_| bindings.iter().any(|b| !b.is_empty()));
        self.locked.extend(locked.clone());
        let before = self.symbols.moves();
        let mut after = Moves::new();
        let mut arm_type: Option<Type> = None;
        for ((_, result), binds) in arms.iter_mut().zip(bindings) {
            self.symbols.restore_moves(&before);
            self.symbols.enter_scope();
            for (name, ty) in binds {
                self.symbols.insert_var(name, ty, true, true);
            }
            let wanted = arm_type.clone().or_else(|| expected_type.cloned());
            let ty = self.check_expression(result, wanted.as_ref());
            self.symbols.exit_scope();

            let leaves = matches!(&result.kind, ExpressionKind::Block(body) if Self::always_leaves(body, true));
            if !leaves {
                after.extend(self.symbols.moves());
            }
            match &arm_type {
                None => arm_type = Some(ty),
                Some(first) if !first.accepts(&ty) && ty != Type::Unknown => self.error(
                    format!("Every arm must give {first}, this one gives {ty}"),
                    result.span,
                ),
                Some(_) => {}
            }
        }
        self.symbols.restore_moves(&after);
        if locked.is_some() {
            self.locked.pop();
        }

        for (_, result) in arms.iter() {
            if let Some(ty) = result.ty.clone() {
                self.consume(result, &ty);
            }
        }
        arm_type.unwrap_or(Type::Void)
    }

    /// Check each pattern against the subject: none twice, and every value
    /// covered, by a `default` or by listing them all. A value no arm takes
    /// has nowhere to go at runtime. Returns the names each arm binds.
    fn check_patterns(
        &mut self,
        subject: &Type,
        arms: &mut [(Expression, Expression)],
        span: Span,
    ) -> Vec<Vec<(String, Type)>> {
        let mut bindings = vec![Vec::new(); arms.len()];
        let all: Option<Vec<String>> = match subject {
            Type::Unknown => return bindings,
            Type::Integer { .. } => None,
            Type::Bool => Some(vec!["false".into(), "true".into()]),
            Type::Optional(_) => Some(vec!["None".into(), "Some(..)".into()]),
            Type::Result { .. } => Some(vec!["Err(..)".into(), "Ok(..)".into()]),
            Type::Enum(name) => self.enum_variants(name).map(|variants| {
                variants
                    .iter()
                    .map(|(variant, fields)| match fields.is_empty() {
                        true => format!("{name}::{variant}"),
                        false => format!("{name}::{variant}(..)"),
                    })
                    .collect()
            }),
            _ => {
                self.error(
                    format!("Cannot match on {subject}, only on an integer, a bool, an enum, a T? or a T!"),
                    span,
                );
                return bindings;
            }
        };

        let mut covered = HashSet::new();
        let mut has_default = false;
        for ((pattern, _), binds) in arms.iter_mut().zip(bindings.iter_mut()) {
            if pattern.is_default_pattern() {
                if has_default {
                    self.error("A match has one 'default' arm".into(), pattern.span);
                }
                has_default = true;
                continue;
            }
            if let Some((value, names)) = self.check_pattern(pattern, subject) {
                if !covered.insert(value) {
                    self.error(
                        "This value is already matched by an earlier arm".into(),
                        pattern.span,
                    );
                }
                *binds = names;
            }
        }

        if has_default {
            return bindings;
        }
        let Some(all) = all else {
            self.error(format!("A match on {subject} needs a 'default' arm"), span);
            return bindings;
        };
        let missing: Vec<String> = (0..)
            .zip(all)
            .filter(|(at, _)| !covered.contains(at))
            .map(|(_, name)| name)
            .collect();
        if !missing.is_empty() {
            self.error(
                format!(
                    "Match does not cover {}; add an arm for each or a 'default' arm",
                    missing.join(", ")
                ),
                span,
            );
        }
        bindings
    }

    /// One pattern: a constant (a number, a bool, a constant's name, a
    /// variant), `None`, `Some(x)`, `Ok(x)`, `Err(e)` or `Shape::Circle(r)`.
    /// Returns what it stands for, to find repeats and gaps (a variant's
    /// position, `Some` and `Ok` as 1), and the names it binds.
    fn check_pattern(
        &mut self,
        pattern: &mut Expression,
        subject: &Type,
    ) -> Option<(i128, Vec<(String, Type)>)> {
        let (value, fields) = match (&pattern.kind, subject) {
            (ExpressionKind::None, Type::Optional(_)) => (0, None),
            (ExpressionKind::Call { function, .. }, _) => {
                let ExpressionKind::Identifier(path) = &function.kind else {
                    self.error("A pattern names what it matches".into(), pattern.span);
                    return None;
                };
                let fields = match (path.as_str(), subject) {
                    ("Some", Type::Optional(inner)) => Some((1, vec![*inner.clone()])),
                    ("Ok", Type::Result { ok_type, .. }) => Some((1, vec![*ok_type.clone()])),
                    ("Err", Type::Result { err_type, .. }) => Some((0, vec![*err_type.clone()])),
                    (path, Type::Enum(name)) => path
                        .rsplit_once("::")
                        .filter(|(owner, _)| owner == name)
                        .and_then(|(_, variant)| {
                            let variants = self.enum_variants(name)?;
                            let at = variants.iter().position(|(v, _)| v == variant)?;
                            Some((at as i128, variants[at].1.clone()))
                        }),
                    _ => None,
                };
                let Some((value, fields)) = fields else {
                    self.error(format!("This pattern cannot match {subject}"), pattern.span);
                    return None;
                };
                (value, Some(fields))
            }
            _ => {
                let ty = self.check_expression(pattern, Some(subject));
                if ty == Type::Unknown {
                    return None;
                }
                if !subject.accepts(&ty) {
                    self.error(
                        format!("A pattern of type {ty} cannot match a value of type {subject}"),
                        pattern.span,
                    );
                    return None;
                }
                let Some(value) = self.pattern_value(pattern, subject) else {
                    self.error(
                        "A pattern must be a literal, a constant or an enum variant".into(),
                        pattern.span,
                    );
                    return None;
                };
                return Some((value, Vec::new()));
            }
        };

        let names = match &pattern.kind {
            ExpressionKind::Call { arguments, .. } => arguments.as_slice(),
            _ => &[],
        };
        let fields = fields.unwrap_or_default();
        if names.len() != fields.len() {
            self.error(
                format!("This pattern takes {} name(s), one per value", fields.len()),
                pattern.span,
            );
            return None;
        }
        let mut binds = Vec::new();
        for (name, ty) in names.iter().zip(fields) {
            match &name.kind {
                ExpressionKind::Identifier(name) if name == "_" => {}
                ExpressionKind::Identifier(name) => binds.push((name.clone(), ty)),
                _ => {
                    self.error(
                        "A pattern binds names, as in Some(x); match the value inside again".into(),
                        name.span,
                    );
                    return None;
                }
            }
        }
        Some((value, binds))
    }

    /// What a constant pattern stands for: a number, a bool as 0 or 1, a
    /// variant's position, or a global constant's value.
    fn pattern_value(&self, pattern: &Expression, subject: &Type) -> Option<i128> {
        match (&pattern.kind, subject) {
            (ExpressionKind::Identifier(path), Type::Enum(name)) => {
                let (_, variant) = path.rsplit_once("::")?;
                let variants = self.enum_variants(name)?;
                variants
                    .iter()
                    .position(|(v, _)| v == variant)
                    .map(|at| at as i128)
            }
            _ => self.constant_value(pattern),
        }
    }

    pub fn constant_value(&self, expr: &Expression) -> Option<i128> {
        use crate::token::Token as T;
        match &expr.kind {
            ExpressionKind::Int(value) => Some(i128::from(*value)),
            ExpressionKind::Boolean(value) => Some(i128::from(*value)),
            ExpressionKind::Prefix {
                operator: T::Minus,
                right,
            } => self.constant_value(right).map(|value| -value),
            ExpressionKind::Identifier(name) => self.constant_value(self.constants.get(name)?),
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => {
                let (l, r) = (self.constant_value(left)?, self.constant_value(right)?);
                match operator {
                    T::Plus => l.checked_add(r),
                    T::Minus => l.checked_sub(r),
                    T::Star => l.checked_mul(r),
                    T::Slash => l.checked_div(r),
                    T::Mod => l.checked_rem(r),
                    T::BitAnd => Some(l & r),
                    T::BitOr => Some(l | r),
                    T::BitXor => Some(l ^ r),
                    T::ShiftLeft => l.checked_shl(u32::try_from(r).ok()?),
                    T::ShiftRight => l.checked_shr(u32::try_from(r).ok()?),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn check_prefix(
        &mut self,
        operator: &crate::token::Token,
        right: &mut Expression,
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        let operator = operator.clone();
        match &operator {
            crate::token::Token::Minus => {
                if let ExpressionKind::Int(value) = right.kind {
                    let ty = self.check_int_literal(value, true, expected_type, span);
                    right.ty = Some(ty.clone());
                    return ty;
                }

                let right_type = self.check_expression(right, expected_type);
                match &right_type {
                    Type::Integer { .. } | Type::Float(_) | Type::ParamType(_) => right_type,
                    _ => {
                        self.error(
                            format!("Cannot negate non-numeric type {}", right_type),
                            span,
                        );
                        Type::Unknown
                    }
                }
            }
            crate::token::Token::Bang => {
                let right_type = self.check_expression(right, expected_type);
                match right_type {
                    Type::Bool | Type::Integer { .. } | Type::ParamType(_) | Type::Unknown => {
                        right_type
                    }
                    _ => {
                        self.error(
                            format!("'!' applies to a bool or an integer, not {right_type}"),
                            span,
                        );
                        Type::Unknown
                    }
                }
            }
            _ => self.check_expression(right, expected_type),
        }
    }

    fn check_index(&mut self, left: &mut Expression, index: &mut Expression, span: Span) -> Type {
        let left_type = self.check_expression(left, None);
        let index_type = self.check_expression(
            index,
            Some(&Type::Integer {
                signed: Signedness::Unsigned,
                width: IntWidth::WSize,
            }),
        );

        if !matches!(index_type, Type::Integer { .. } | Type::Unknown) {
            self.error(
                format!("Index must be an integer, got {index_type}"),
                index.span,
            );
        }

        match left_type {
            Type::Array { elem_type, len } => {
                if let ExpressionKind::Int(value) = index.kind
                    && value >= len as u64
                {
                    self.error(
                        format!("Index {value} is outside the array's 0..{len}"),
                        index.span,
                    );
                }
                *elem_type
            }
            Type::Vec { elem_type } | Type::Slice { elem_type } => *elem_type,
            Type::Unknown => Type::Unknown,
            Type::Pointer(ref pointee) if matches!(pointee.as_ref(), Type::Array { .. }) => {
                self.error(
                    format!("Cannot index {left_type} directly, write (*p)[i]"),
                    span,
                );
                Type::Unknown
            }
            _ => {
                self.error(format!("Cannot index type {left_type}"), span);
                Type::Unknown
            }
        }
    }

    fn check_array_literal(
        &mut self,
        elements: &mut [Expression],
        expected_type: Option<&Type>,
    ) -> Type {
        if elements.is_empty() {
            if let Some(Type::Array { elem_type, len }) = expected_type {
                return Type::Array {
                    elem_type: elem_type.clone(),
                    len: *len,
                };
            }
            return Type::Array {
                elem_type: Box::new(Type::Unknown),
                len: 0,
            };
        }

        let elem_hint = if let Some(Type::Array { elem_type, .. }) = expected_type {
            Some(elem_type.as_ref().clone())
        } else {
            None
        };

        let first_type = self.check_expression(&mut elements[0], elem_hint.as_ref());
        self.consume(&elements[0], &first_type);
        let len = elements.len();

        for (i, elem) in elements.iter_mut().enumerate().skip(1) {
            let first = first_type.clone();
            let elem_span = elem.span;
            let elem_type = self.check_expression(elem, Some(&first));
            self.consume(elem, &elem_type);

            if !first_type.accepts(&elem_type) {
                self.error(
                    format!(
                        "Array element at index {} type mismatch. Expected {}, got {}.",
                        i, first_type, elem_type
                    ),
                    elem_span,
                );
            }
        }

        Type::Array {
            elem_type: Box::new(first_type),
            len,
        }
    }

    /// A literal fills a `Vec<T>` as readily as an array: it is checked as an
    /// array of T, then typed as the Vec.
    fn check_literal_into(
        &mut self,
        expected: Option<&Type>,
        check: impl FnOnce(&mut Self, Option<&Type>) -> Type,
    ) -> Type {
        let Some(Type::Vec { elem_type }) = expected else {
            return check(self, expected);
        };
        let hint = Type::Array {
            elem_type: elem_type.clone(),
            len: 0,
        };
        match check(self, Some(&hint)) {
            Type::Array { elem_type, .. } => Type::Vec { elem_type },
            other => other,
        }
    }

    fn check_array_repeat(
        &mut self,
        value: &mut Expression,
        count: u64,
        expected_type: Option<&Type>,
    ) -> Type {
        let elem_hint = match expected_type {
            Some(Type::Array { elem_type, .. }) => Some(elem_type.as_ref().clone()),
            _ => None,
        };
        let elem_type = self.check_expression(value, elem_hint.as_ref());
        self.consume(value, &elem_type);
        if count > 1 && matches!(value.kind, ExpressionKind::Identifier(_)) {
            self.check_expression(value, elem_hint.as_ref());
        }

        Type::Array {
            elem_type: Box::new(elem_type),
            len: count as usize,
        }
    }

    fn check_tuple(
        &mut self,
        elements: &mut [Expression],
        expected_type: Option<&Type>,
        span: Span,
    ) -> Type {
        let expected_types: Option<Vec<Type>> = if let Some(Type::Tuple(types)) = expected_type {
            Some(types.clone())
        } else {
            None
        };

        let mut result_types = Vec::with_capacity(elements.len());
        let len = elements.len();

        for (i, elem) in elements.iter_mut().enumerate() {
            let expected = expected_types.as_ref().and_then(|types| types.get(i));
            let elem_type = self.check_expression(elem, expected);
            self.consume(elem, &elem_type);
            result_types.push(elem_type);
        }

        if let Some(ref expected_types) = expected_types
            && expected_types.len() != len
        {
            self.error(
                format!(
                    "Tuple has {} elements, but expected {}",
                    len,
                    expected_types.len()
                ),
                span,
            );
        }

        Type::Tuple(result_types)
    }

    fn check_borrow(&mut self, inner: &mut Expression, kind: Borrow, span: Span) -> Type {
        let inner_kind = inner.kind.clone();
        let inner_type = self.check_expression(inner, None);

        if kind == Borrow::Mutable {
            self.require_mutable(inner, span);
        }
        if !matches!(
            inner_kind,
            ExpressionKind::Identifier(_)
                | ExpressionKind::Get { .. }
                | ExpressionKind::Index { .. }
                | ExpressionKind::Dereference(_)
        ) {
            self.error(
                match kind {
                    Borrow::Shared => "Cannot create reference to a temporary value".into(),
                    Borrow::Mutable => {
                        "Cannot create mutable reference to a temporary value".to_string()
                    }
                },
                span,
            );
        }

        match kind {
            Borrow::Shared => Type::Ref(Box::new(inner_type)),
            Borrow::Mutable => Type::RefMut(Box::new(inner_type)),
        }
    }

    fn check_expression_inner(
        &mut self,
        expr: &mut Expression,
        expected_type: Option<&Type>,
    ) -> Type {
        let span = expr.span;

        let inner_expected = match expected_type {
            Some(Type::Optional(inner)) => Some(inner.as_ref()),
            other => other,
        };

        match &mut expr.kind {
            ExpressionKind::Int(value) => {
                self.check_int_literal(*value, false, inner_expected, span)
            }
            ExpressionKind::Float(_) => match inner_expected {
                Some(Type::Float(width)) => Type::Float(*width),
                _ => Type::Float(FloatWidth::W64),
            },
            ExpressionKind::Boolean(_) => Type::Bool,
            ExpressionKind::StringLit(_) => Self::str_type(),
            ExpressionKind::None => {
                if let Some(Type::Optional(inner)) = expected_type {
                    Type::Optional(inner.clone())
                } else if let Some(exp_type) = expected_type {
                    self.error(
                        format!(
                            "'None' can only be assigned to optional types, got {}",
                            exp_type
                        ),
                        span,
                    );
                    Type::Unknown
                } else {
                    Type::Unknown
                }
            }

            ExpressionKind::Identifier(name) => self.check_identifier(name, span),

            ExpressionKind::Assign { target, value, .. } => self.check_assign(target, value, span),

            ExpressionKind::Infix {
                left,
                operator: operator @ (crate::token::Token::Catch | crate::token::Token::Orelse),
                right,
            } => {
                let operator = operator.clone();
                self.check_fallback(left, &operator, right, span)
            }
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => {
                let operator = operator.clone();
                self.check_infix(left, &operator, right, expected_type, span)
            }
            ExpressionKind::Try(value) => self.check_try(value, span),
            ExpressionKind::Block(body) => {
                self.symbols.enter_scope();
                for stmt in body.iter_mut() {
                    self.check_statement(stmt);
                }
                self.symbols.exit_scope();
                Type::Void
            }

            ExpressionKind::Call {
                function,
                arguments,
            } => self.check_call_expression(function, arguments, expected_type, span),

            ExpressionKind::Get { object, name } => {
                let name = name.clone();
                self.check_field_access(object, &name, span)
            }

            ExpressionKind::StructLiteral { name, fields } => {
                let written = name.clone();
                let ty = self.check_struct_literal(&written, fields, expected_type, span);
                if let Type::Struct(concrete) = &ty {
                    *name = concrete.clone();
                }
                ty
            }

            ExpressionKind::Match { value, arms } => {
                self.check_match(value, arms, expected_type, span)
            }

            ExpressionKind::Prefix { operator, right } => {
                let operator = operator.clone();
                self.check_prefix(&operator, right, expected_type, span)
            }

            ExpressionKind::Cast { left, target } => {
                let from = self.check_expression(left, None);
                let to = self.resolve_spec(target, span);
                let carries_data = matches!(&from, Type::Enum(name) if self.enum_has_data(name));
                if carries_data || !Self::castable(&from, &to) {
                    self.error(format!("Cannot cast {from} to {to}"), span);
                    return Type::Unknown;
                }
                to
            }
            ExpressionKind::Index { left, index } => self.check_index(left, index, span),

            ExpressionKind::ArrayLiteral(elements) => self
                .check_literal_into(inner_expected, |this, hint| {
                    this.check_array_literal(elements, hint)
                }),
            ExpressionKind::Range { start, end } => self.check_range(start, end, span),
            ExpressionKind::ArrayRepeat { value, count } => self
                .check_literal_into(inner_expected, |this, hint| {
                    this.check_array_repeat(value, *count, hint)
                }),

            ExpressionKind::BorrowRef(inner) => self.check_borrow(inner, Borrow::Shared, span),
            ExpressionKind::BorrowRefMut(inner) => self.check_borrow(inner, Borrow::Mutable, span),
            ExpressionKind::Dereference(inner) => {
                let inner_type = self.check_expression(inner, None);

                match inner_type {
                    Type::Pointer(elem_type) => *elem_type,
                    Type::Ref(elem_type) => *elem_type,
                    Type::RefMut(elem_type) => *elem_type,
                    Type::Unknown => Type::Unknown,
                    _ => {
                        self.error(format!("Cannot dereference type {}", inner_type), span);
                        Type::Unknown
                    }
                }
            }
            ExpressionKind::Tuple(elements) => self.check_tuple(elements, expected_type, span),

            ExpressionKind::InlineAsm {
                outputs, inputs, ..
            } => {
                for operand in inputs.iter_mut() {
                    self.check_expression(&mut operand.expr, None);
                }

                for operand in outputs.iter_mut() {
                    let operand_span = operand.expr.span;
                    let is_ident = matches!(operand.expr.kind, ExpressionKind::Identifier(_));
                    let ty = self.check_expression(&mut operand.expr, None);
                    if !is_ident {
                        self.error(
                            "Inline assembly output must be a variable".to_string(),
                            operand_span,
                        );
                    }
                    let _ = ty;
                }
                Type::Integer {
                    signed: Signedness::Signed,
                    width: IntWidth::W64,
                }
            }
        }
    }

    fn expect_arity(
        &mut self,
        name: &str,
        arguments: &[Expression],
        arity: usize,
        span: Span,
    ) -> bool {
        if arguments.len() == arity {
            return true;
        }
        let plural = if arity == 1 { "" } else { "s" };
        self.error(
            format!("{name}() takes exactly {arity} argument{plural}"),
            span,
        );
        false
    }

    fn check_vec_method_mut(
        &mut self,
        method_name: &str,
        elem_type: &Type,
        arguments: &mut [Expression],
        span: Span,
    ) -> Type {
        let usize_type = Type::Integer {
            signed: Signedness::Unsigned,
            width: IntWidth::WSize,
        };
        let vec_type = || Type::Vec {
            elem_type: Box::new(elem_type.clone()),
        };
        let full_name = format!("Vec::{method_name}");

        let arity = match method_name {
            "with_capacity" | "push" | "get" | "remove" | "reserve" => 1,
            "insert" => 2,
            _ => 0,
        };
        let arity_ok = self.expect_arity(&full_name, arguments, arity, span);

        if arity_ok && method_name != "push" && arity > 0 {
            let arg_type = self.check_expression(&mut arguments[0], Some(&usize_type));
            if !usize_type.accepts(&arg_type) {
                self.error(format!("{full_name}() index or count must be usize"), span);
            }
        }
        if arity_ok && matches!(method_name, "push" | "insert") {
            let item = arguments.last_mut().unwrap();
            let arg_type = self.check_expression(item, Some(elem_type));
            self.consume(item, &arg_type);
            if !elem_type.accepts(&arg_type) {
                self.error(
                    format!("{full_name}() expects {elem_type}, got {arg_type}"),
                    span,
                );
            }
        }

        match method_name {
            "new" | "with_capacity" => vec_type(),
            "push" | "insert" | "reserve" | "shrink_to_fit" | "clear" => Type::Void,
            "get" if self.owns_heap(elem_type) => {
                self.error(
                    format!("Vec::get() cannot copy out a {elem_type}; index it and call .copy()"),
                    span,
                );
                Type::Unknown
            }
            "get" => Type::Optional(Box::new(elem_type.clone())),
            "remove" => elem_type.clone(),
            "pop" => Type::Optional(Box::new(elem_type.clone())),
            "len" | "capacity" => usize_type,
            "is_empty" => Type::Bool,
            _ => {
                self.error(format!("Vec<T> has no method '{method_name}'"), span);
                Type::Unknown
            }
        }
    }

    fn check_option_method(
        &mut self,
        method_name: &str,
        inner: &Type,
        arguments: &mut [Expression],
        span: Span,
    ) -> Type {
        self.expect_arity(method_name, arguments, 0, span);

        match method_name {
            "is_some" | "is_none" => Type::Bool,
            "unwrap" => inner.clone(),
            _ => {
                self.error(format!("Optional has no method '{method_name}'"), span);
                Type::Unknown
            }
        }
    }

    fn check_result_method(
        &mut self,
        method_name: &str,
        ok_type: &Type,
        err_type: &Type,
        arguments: &mut [Expression],
        span: Span,
    ) -> Type {
        self.expect_arity(method_name, arguments, 0, span);

        match method_name {
            "is_ok" | "is_err" => Type::Bool,
            "unwrap" => ok_type.clone(),
            "unwrap_err" => err_type.clone(),
            _ => {
                self.error(format!("Result type has no method '{method_name}'"), span);
                Type::Unknown
            }
        }
    }

    /// Check a call against the callee's signature. Also returns what each
    /// type parameter of a generic callee turned out to be.
    fn check_call_mut(
        &mut self,
        name: &str,
        args: &mut [Expression],
        implicit_self: Option<Type>,
        call_span: Span,
    ) -> (Type, HashMap<String, Type>) {
        self.check_visible(name, call_span);
        if let Some(super::symbol_table::Symbol::Function { params, ret_type }) =
            self.symbols.lookup(name).cloned()
        {
            let mut expected_args = params.clone();
            let mut substitutions: HashMap<String, Type> = HashMap::new();

            if let Some(self_type) = implicit_self
                && !expected_args.is_empty()
            {
                let self_type = match self_type {
                    Type::Pointer(pointee) | Type::Ref(pointee) | Type::RefMut(pointee) => *pointee,
                    other => other,
                };
                if !expected_args[0].accepts(&self_type) {
                    self.error(
                        format!(
                            "Method '{name}' called on wrong type. Expected {}, got {}",
                            expected_args[0], self_type
                        ),
                        call_span,
                    );
                }
                expected_args.remove(0);
            }

            if args.len() != expected_args.len() {
                self.error(
                    format!(
                        "Function '{name}' expects {} arguments, got {}",
                        expected_args.len(),
                        args.len()
                    ),
                    call_span,
                );
            } else {
                for i in 0..args.len() {
                    let expected = Self::substitute_params(&expected_args[i], &substitutions);
                    let arg_span = args[i].span;
                    let arg_type = self.check_expression(&mut args[i], Some(&expected));
                    Self::bind_params(&expected_args[i], &arg_type, &mut substitutions);

                    if !expected.accepts(&arg_type) {
                        self.error(format!("Argument {} type mismatch.", i + 1), arg_span);
                    }

                    if !matches!(expected, Type::Ref(_) | Type::RefMut(_)) {
                        self.consume(&args[i], &arg_type);
                    }
                }
            }
            return (
                Self::substitute_params(&ret_type, &substitutions),
                substitutions,
            );
        }

        self.error(format!("Function '{name}' not defined."), call_span);
        (Type::Unknown, HashMap::new())
    }

    /// Record what each type parameter in `param` stands for, read off the
    /// argument's type in the same position: `*T` given a `*i64` makes T i64.
    fn bind_params(param: &Type, arg: &Type, subs: &mut HashMap<String, Type>) {
        match (param, arg) {
            (Type::ParamType(name), _) => {
                subs.entry(name.clone()).or_insert_with(|| arg.clone());
            }
            (Type::Pointer(p), Type::Pointer(a) | Type::Ref(a) | Type::RefMut(a))
            | (Type::Ref(p), Type::Ref(a) | Type::RefMut(a))
            | (Type::RefMut(p), Type::RefMut(a))
            | (Type::Optional(p), Type::Optional(a))
            | (Type::Slice { elem_type: p }, Type::Slice { elem_type: a })
            | (Type::Vec { elem_type: p }, Type::Vec { elem_type: a })
            | (Type::Array { elem_type: p, .. }, Type::Array { elem_type: a, .. })
            | (Type::Result { ok_type: p, .. }, Type::Result { ok_type: a, .. }) => {
                Self::bind_params(p, a, subs)
            }
            (Type::Tuple(params), Type::Tuple(args)) => {
                for (p, a) in params.iter().zip(args) {
                    Self::bind_params(p, a, subs);
                }
            }
            _ => {}
        }
    }

    /// Register `largest<i64>` as a function of its own, its body queued to be
    /// checked at those types, and return the name the call now goes to.
    /// `None` when `name` is not generic or an argument could not be typed.
    fn instantiate_function(
        &mut self,
        name: &str,
        subs: &HashMap<String, Type>,
        span: Span,
    ) -> Option<String> {
        let decl = self.generic_functions.get(name)?.clone();
        let StatementKind::Function { type_params, .. } = &decl.kind else {
            return None;
        };

        let mut specs = Substitutions::new();
        for param in type_params {
            match subs.get(&param.name) {
                Some(Type::Unknown) => return None,
                Some(ty) => {
                    if let Some(bound) = &param.bound {
                        self.check_bound(name, &param.name, bound, ty, span);
                    }
                    specs.insert(param.name.clone(), ty.to_spec());
                }
                None => {
                    self.error(
                        format!(
                            "Cannot tell what '{}' is in this call to '{name}'",
                            param.name
                        ),
                        span,
                    );
                    return None;
                }
            }
        }

        let instance = mangle(name, type_params, &specs);
        if self.symbols.lookup(&instance).is_none() {
            let concrete = instantiate(&decl, instance.clone(), &specs);
            self.scan_functions(std::slice::from_ref(&concrete));
            self.instantiations.push(concrete);
        }
        Some(instance)
    }

    /// Replace the type parameters of a generic signature with what the call
    /// site inferred, so the caller sees a concrete type.
    fn substitute_params(ty: &Type, subs: &HashMap<String, Type>) -> Type {
        let boxed = |inner: &Type| Box::new(Self::substitute_params(inner, subs));

        match ty {
            Type::ParamType(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Pointer(inner) => Type::Pointer(boxed(inner)),
            Type::Ref(inner) => Type::Ref(boxed(inner)),
            Type::RefMut(inner) => Type::RefMut(boxed(inner)),
            Type::Optional(inner) => Type::Optional(boxed(inner)),
            Type::Slice { elem_type } => Type::Slice {
                elem_type: boxed(elem_type),
            },
            Type::Vec { elem_type } => Type::Vec {
                elem_type: boxed(elem_type),
            },
            Type::Array { elem_type, len } => Type::Array {
                elem_type: boxed(elem_type),
                len: *len,
            },
            Type::Result { ok_type, err_type } => Type::Result {
                ok_type: boxed(ok_type),
                err_type: boxed(err_type),
            },
            Type::Tuple(types) => Type::Tuple(
                types
                    .iter()
                    .map(|t| Self::substitute_params(t, subs))
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    fn check_visible(&mut self, name: &str, span: Span) {
        let declared = match name.rsplit_once("::") {
            Some((owner, member)) if owner.contains("__") => {
                format!("{}::{member}", owner.split("__").next().unwrap_or(owner))
            }
            _ => name.to_string(),
        };
        if let Some(module) = self.privates.get(&declared)
            && !self.current_item.starts_with(&format!("{module}::"))
        {
            let message = format!("'{declared}' is private to module '{module}'; mark it pub");
            self.error(message, span);
        }
    }

    fn error(&mut self, msg: String, span: Span) {
        self.errors.push(ZeruError::semantic(msg, span));
    }

    /// The type of an integer literal, negated when `negative`: the integer
    /// type its context expects, or i32 when nothing does. A value that does
    /// not fit is an error, not a quiet wrap.
    fn check_int_literal(
        &mut self,
        value: u64,
        negative: bool,
        expected: Option<&Type>,
        span: Span,
    ) -> Type {
        let expected = match expected {
            Some(Type::Optional(inner)) => Some(inner.as_ref()),
            other => other,
        };
        if let Some(Type::Float(width)) = expected {
            let exact = match width {
                FloatWidth::W32 => (value as f32) as u64 == value,
                FloatWidth::W64 => (value as f64) as u64 == value,
            };
            if !exact {
                self.error(
                    format!("Literal {value} has no exact {} value", Type::Float(*width)),
                    span,
                );
            }
            return Type::Float(*width);
        }
        let (signed, width, by_default) = match expected {
            Some(Type::Integer { signed, width }) => (*signed, *width, false),
            _ => (Signedness::Signed, IntWidth::W32, true),
        };
        let ty = Type::Integer { signed, width };

        if !Self::fits_in_int(value, negative, width, signed) {
            let sign = if negative { "-" } else { "" };
            let hint = if by_default {
                ", the type a literal takes when nothing says otherwise; annotate a wider type"
            } else {
                ""
            };
            self.error(
                format!("Literal {sign}{value} does not fit in {ty}{hint}"),
                span,
            );
        }
        ty
    }

    fn fits_in_int(value: u64, negative: bool, width: IntWidth, signed: Signedness) -> bool {
        let bits = match width {
            IntWidth::W8 => 8,
            IntWidth::W16 => 16,
            IntWidth::W32 => 32,
            IntWidth::W64 | IntWidth::WSize => 64,
        };
        let value = u128::from(value);
        match (signed, negative) {
            (Signedness::Unsigned, false) => value < 1 << bits,
            (Signedness::Unsigned, true) => value == 0,
            (Signedness::Signed, false) => value < 1 << (bits - 1),
            (Signedness::Signed, true) => value <= 1 << (bits - 1),
        }
    }

    fn find_similar_variable(&self, name: &str) -> Option<String> {
        let mut best_match: Option<String> = None;
        let mut min_dist = usize::MAX;

        for scope in self.symbols.get_all_scopes() {
            for (var_name, symbol) in scope {
                if let super::symbol_table::Symbol::Var { .. } = symbol {
                    let dist = Self::levenshtein_distance(name, var_name);
                    if dist < min_dist && dist <= 2 {
                        min_dist = dist;
                        best_match = Some(var_name.clone());
                    }
                }
            }
        }

        best_match
    }

    /// Edit distance, one row at a time: `row[j]` is the distance between the
    /// prefix of `a` read so far and the first `j` characters of `b`.
    fn levenshtein_distance(a: &str, b: &str) -> usize {
        let b: Vec<char> = b.chars().collect();
        let mut row: Vec<usize> = (0..=b.len()).collect();

        for (i, a_char) in a.chars().enumerate() {
            let mut diagonal = row[0];
            row[0] = i + 1;
            for (j, &b_char) in b.iter().enumerate() {
                let above = row[j + 1];
                row[j + 1] = (above + 1)
                    .min(row[j] + 1)
                    .min(diagonal + usize::from(a_char != b_char));
                diagonal = above;
            }
        }
        row[b.len()]
    }
}
