//! Every declaration in the program gets a [`DefId`], and every module a scope
//! mapping names to them. Names are resolved by looking them up, never by
//! rewriting the source, so a local can shadow an item and two modules can
//! both declare `min`. A module sees its own items, what it
//! imports and, last, the prelude; never the items of the file importing it.

use std::collections::HashMap;

use crate::ast::{Program, Statement, StatementKind};
use crate::errors::{Sources, Span, ZeruError};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl ModuleId {
    /// The prelude, part of every program: what it marks `pub` every module
    /// sees, and the rest only the standard library.
    pub const PRELUDE: ModuleId = ModuleId(0);
    /// The file being compiled.
    pub const ROOT: ModuleId = ModuleId(1);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ItemKind {
    Fn,
    Struct,
    Enum,
    Trait,
    Const,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Namespace {
    Type,
    Trait,
    Value,
}

const NAMESPACES: [Namespace; 3] = [Namespace::Type, Namespace::Trait, Namespace::Value];

impl ItemKind {
    fn namespace(self) -> Namespace {
        match self {
            Self::Struct | Self::Enum => Namespace::Type,
            Self::Trait => Namespace::Trait,
            Self::Fn | Self::Const => Namespace::Value,
        }
    }
}

pub struct Item<'a> {
    pub name: String,
    pub kind: ItemKind,
    pub module: ModuleId,
    pub is_pub: bool,
    pub decl: &'a Statement,
    pub owner: Option<DefId>,
}

/// A parsed file and the name its importers call it by. The prelude comes
/// first and the root file second; neither has a name.
pub struct SourceModule {
    pub name: Option<String>,
    pub program: Program,
    pub std: bool,
}

struct Module {
    name: Option<String>,
    std: bool,
    scope: HashMap<(Namespace, String), DefId>,
    imported: HashMap<String, (ModuleId, String)>,
    modules: HashMap<String, ModuleId>,
}

pub struct Items<'a> {
    pub items: Vec<Item<'a>>,
    sources: &'a Sources,
    modules: Vec<Module>,
    by_name: HashMap<String, ModuleId>,
    methods: HashMap<(DefId, String), DefId>,
    pub errors: Vec<ZeruError>,
}

impl<'a> Items<'a> {
    pub fn collect(sources: &'a [SourceModule], text: &'a Sources) -> Self {
        let mut items = Self {
            items: Vec::new(),
            sources: text,
            modules: Vec::new(),
            by_name: HashMap::new(),
            methods: HashMap::new(),
            errors: Vec::new(),
        };
        for source in sources {
            let id = ModuleId(items.modules.len() as u32);
            items.modules.push(Module {
                name: source.name.clone(),
                std: source.std,
                scope: HashMap::new(),
                imported: HashMap::new(),
                modules: HashMap::new(),
            });
            if let Some(name) = &source.name {
                items.by_name.insert(name.clone(), id);
            }
            for statement in &source.program.statements {
                items.declare(id, statement);
            }
        }
        for (at, source) in sources.iter().enumerate() {
            items.import(ModuleId(at as u32), &source.program);
        }
        items
    }

    fn declare(&mut self, module: ModuleId, statement: &'a Statement) {
        let (kind, name) = match &statement.kind {
            StatementKind::Function { name, .. } => (ItemKind::Fn, name),
            StatementKind::Struct { name, .. } => (ItemKind::Struct, name),
            StatementKind::Enum { name, .. } => (ItemKind::Enum, name),
            StatementKind::Trait { name, .. } => (ItemKind::Trait, name),
            StatementKind::Var {
                name,
                is_const: true,
                ..
            } => (ItemKind::Const, name),
            _ => return,
        };
        let shown = self.qualified(module, name);
        let key = (kind.namespace(), name.clone());
        if let Some(&first) = self.modules[module.0 as usize].scope.get(&key) {
            let message = match kind {
                _ if name.starts_with('#') => return,
                ItemKind::Fn if name.starts_with('!') => {
                    format!("Test '{}' is already defined", &name[1..])
                }
                ItemKind::Struct | ItemKind::Enum => format!("Type '{shown}' is already defined"),
                ItemKind::Trait => format!("Trait '{shown}' is already defined"),
                ItemKind::Fn => format!("Function '{shown}' is already defined"),
                ItemKind::Const => format!("'{shown}' is already defined"),
            };
            let first = self.sources.first_line(self.item(first).decl.span);
            let mut error = ZeruError::semantic(message, self.sources.first_line(statement.span));
            error
                .note("first defined here", first)
                .help("rename one of them");
            self.errors.push(error);
            return;
        }
        let def = self.push(Item {
            name: shown.clone(),
            kind,
            module,
            is_pub: statement.is_pub,
            decl: statement,
            owner: None,
        });
        self.modules[module.0 as usize].scope.insert(key, def);

        let StatementKind::Struct { methods, .. } = &statement.kind else {
            return;
        };
        for method in methods {
            let StatementKind::Function { name, .. } = &method.kind else {
                continue;
            };
            let full = format!("{shown}::{name}");
            if let Some(&first) = self.methods.get(&(def, name.clone())) {
                let first = self.sources.first_line(self.item(first).decl.span);
                let mut error = ZeruError::semantic(
                    format!("Function '{full}' is already defined"),
                    self.sources.first_line(method.span),
                );
                error
                    .note("first defined here", first)
                    .help("rename one of them");
                self.errors.push(error);
                continue;
            }
            let method_def = self.push(Item {
                name: full,
                kind: ItemKind::Fn,
                module,
                is_pub: method.is_pub,
                decl: method,
                owner: Some(def),
            });
            self.methods.insert((def, name.clone()), method_def);
        }
    }

    fn push(&mut self, item: Item<'a>) -> DefId {
        self.items.push(item);
        DefId(self.items.len() as u32 - 1)
    }

    fn qualified(&self, module: ModuleId, name: &str) -> String {
        match &self.modules[module.0 as usize].name {
            Some(module) => format!("{module}::{name}"),
            None => name.to_string(),
        }
    }

    /// `import a.b;` lets the importer say `b::x`, and `import a.b::{x, y}`
    /// also `x` for `b::x`.
    fn import(&mut self, module: ModuleId, program: &Program) {
        for statement in &program.statements {
            let StatementKind::Import { path, symbols } = &statement.kind else {
                continue;
            };
            let short = path.last().cloned().unwrap_or_default();
            let Some(&target) = self.by_name.get(&short) else {
                continue;
            };
            self.modules[module.0 as usize]
                .modules
                .insert(short.clone(), target);
            for symbol in symbols.iter().flatten() {
                let found: Vec<DefId> = NAMESPACES
                    .into_iter()
                    .filter_map(|ns| {
                        self.modules[target.0 as usize]
                            .scope
                            .get(&(ns, symbol.clone()))
                            .copied()
                    })
                    .collect();
                if found.is_empty() {
                    let at = self.sources.word_in(statement.span, symbol);
                    let names: Vec<String> = self.modules[target.0 as usize]
                        .scope
                        .iter()
                        .filter(|(_, def)| self.item(**def).is_pub)
                        .map(|((_, name), _)| name.clone())
                        .collect();
                    let mut error = ZeruError::semantic(
                        format!("Module '{short}' has no '{symbol}'"),
                        at.unwrap_or(statement.span),
                    );
                    crate::check::suggest::did_you_mean(&mut error, symbol, &names, at);
                    self.errors.push(error);
                    continue;
                }
                let declared_here = NAMESPACES.into_iter().any(|ns| {
                    self.modules[module.0 as usize]
                        .scope
                        .contains_key(&(ns, symbol.clone()))
                });
                if declared_here {
                    self.errors.push(ZeruError::semantic(
                        format!("'{symbol}' is imported from '{short}' and also declared here"),
                        statement.span,
                    ));
                    continue;
                }
                if let Some((other, _)) = self.modules[module.0 as usize].imported.get(symbol) {
                    let other = self.module_name(*other).unwrap_or_default();
                    self.errors.push(ZeruError::semantic(
                        format!("'{symbol}' is already imported from '{other}'"),
                        statement.span,
                    ));
                    continue;
                }
                let private: Vec<ZeruError> = found
                    .into_iter()
                    .filter(|&def| !self.visible(def, module))
                    .map(|def| self.private_error(def, statement.span))
                    .collect();
                self.errors.extend(private);
                self.modules[module.0 as usize]
                    .imported
                    .insert(symbol.clone(), (target, symbol.clone()));
            }
        }
    }

    pub fn item(&self, def: DefId) -> &Item<'a> {
        &self.items[def.0 as usize]
    }

    /// Whether `def` is declared in the standard library.
    pub fn is_std(&self, def: DefId) -> bool {
        self.modules[self.item(def).module.0 as usize].std
    }

    /// The name of `def` as a program calls it: the prelude's functions
    /// behind the methods of `str`, `Vec` and `String` by those methods.
    pub fn shown(&self, def: DefId) -> String {
        let name = &self.item(def).name;
        let owners = [
            ("__zeru_str_", "str"),
            ("__zeru_vec_", "Vec"),
            ("__zeru_string_", "String"),
        ];
        owners
            .iter()
            .find_map(|(prefix, owner)| Some(format!("{owner}.{}", name.strip_prefix(prefix)?)))
            .unwrap_or_else(|| name.clone())
    }

    pub fn method(&self, owner: DefId, name: &str) -> Option<DefId> {
        self.methods.get(&(owner, name.to_string())).copied()
    }

    pub fn methods_of(&self, owner: DefId) -> impl Iterator<Item = DefId> + '_ {
        let StatementKind::Struct { methods, .. } = &self.item(owner).decl.kind else {
            unreachable!("only a struct has methods")
        };
        methods.iter().filter_map(move |method| match &method.kind {
            StatementKind::Function { name, .. } => self.method(owner, name),
            _ => None,
        })
    }

    pub fn module_name(&self, module: ModuleId) -> Option<&str> {
        self.modules[module.0 as usize].name.as_deref()
    }

    /// The item `path` names from inside `from`: `x`, or `module::x` of a
    /// module `from` imports. A name is its own item, else one it imports,
    /// else the prelude's.
    fn lookup(&self, from: ModuleId, namespace: Namespace, path: &str) -> Option<DefId> {
        let here = &self.modules[from.0 as usize];
        let in_module = |module: ModuleId, name: &str| {
            self.modules[module.0 as usize]
                .scope
                .get(&(namespace, name.to_string()))
                .copied()
        };
        if let Some((module, name)) = path.split_once("::") {
            return in_module(*here.modules.get(module)?, name);
        }
        if let Some(def) = in_module(from, path) {
            return Some(def);
        }
        if let Some((module, name)) = here.imported.get(path) {
            return in_module(*module, name);
        }
        in_module(ModuleId::PRELUDE, path).filter(|&def| self.visible(def, from))
    }

    /// The module code in `from` imports as `name`.
    pub fn imported_module(&self, from: ModuleId, name: &str) -> Option<ModuleId> {
        self.modules[from.0 as usize].modules.get(name).copied()
    }

    /// The names of the items of `kind` code in `from` names unqualified,
    /// or with `public`, the public ones `from` itself declares.
    pub fn names_of(&self, from: ModuleId, kind: ItemKind, public: bool) -> Vec<String> {
        let mut names: Vec<String> = self
            .items
            .iter()
            .enumerate()
            .filter(|(_, item)| item.kind == kind && item.owner.is_none())
            .filter(|&(at, item)| match public {
                true => item.module == from && item.is_pub,
                false => {
                    item.module == from
                        || item.module == ModuleId::PRELUDE && self.visible(DefId(at as u32), from)
                }
            })
            .map(|(_, item)| item.name.clone())
            .collect();
        if !public {
            names.extend(self.modules[from.0 as usize].imported.keys().cloned());
        }
        names
    }

    pub fn lookup_type(&self, from: ModuleId, path: &str) -> Option<DefId> {
        self.lookup(from, Namespace::Type, path)
    }

    pub fn lookup_value(&self, from: ModuleId, path: &str) -> Option<DefId> {
        self.lookup(from, Namespace::Value, path)
    }

    pub fn lookup_trait(&self, from: ModuleId, path: &str) -> Option<DefId> {
        self.lookup(from, Namespace::Trait, path)
    }

    /// Whether code in `from` may name `def`: anything public, anything of
    /// its own module or the root file, and the prelude's private items from
    /// a module of the standard library.
    pub fn visible(&self, def: DefId, from: ModuleId) -> bool {
        let item = self.item(def);
        item.is_pub
            || item.module == from
            || item.module == ModuleId::ROOT
            || item.module == ModuleId::PRELUDE && self.modules[from.0 as usize].std
    }

    /// The error for using `def` where it is not visible, with the `pub`
    /// that makes it visible unless the standard library keeps it private.
    pub fn private_error(&self, def: DefId, span: Span) -> ZeruError {
        let item = self.item(def);
        let module = self.module_name(item.module).unwrap_or_default();
        let mut error = ZeruError::semantic(
            format!("'{}' is private to module '{module}'", item.name),
            span,
        );
        let decl = self.sources.first_line(item.decl.span);
        error.note("declared here, without pub", decl);
        if !self.modules[item.module.0 as usize].std {
            error
                .help("mark it pub to use it outside its module")
                .fix(Span::new(decl.start, decl.start), "pub ");
        }
        error
    }
}
