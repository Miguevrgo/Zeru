//! Every declaration in the program gets a [`DefId`], and every module a scope
//! mapping names to them. Names are resolved by looking them up, never by
//! rewriting the source, so a local can shadow an item and two modules can
//! both declare `min`.

use std::collections::HashMap;

use crate::ast::{Program, Statement, StatementKind};
use crate::errors::{Span, ZeruError};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl ModuleId {
    /// The prelude, part of every program: what it marks `pub` every module
    /// sees, and the rest only the standard library.
    pub const PRELUDE: ModuleId = ModuleId(0);
    /// The file being compiled, whose items every module sees.
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
    /// As messages show it: `Circle`, `shapes::Circle`, `Circle::area`.
    pub name: String,
    pub kind: ItemKind,
    pub module: ModuleId,
    pub is_pub: bool,
    pub decl: &'a Statement,
    /// The struct a method belongs to.
    pub owner: Option<DefId>,
}

/// A parsed file and the name its importers call it by. The prelude comes
/// first and the root file second; neither has a name.
pub struct SourceModule {
    pub name: Option<String>,
    pub program: Program,
    /// Part of the standard library, which may use the prelude's own items.
    pub std: bool,
}

struct Module {
    name: Option<String>,
    std: bool,
    scope: HashMap<(Namespace, String), DefId>,
    imported: HashMap<String, (ModuleId, String)>,
}

pub struct Items<'a> {
    pub items: Vec<Item<'a>>,
    modules: Vec<Module>,
    by_name: HashMap<String, ModuleId>,
    methods: HashMap<(DefId, String), DefId>,
    pub errors: Vec<ZeruError>,
}

impl<'a> Items<'a> {
    pub fn collect(sources: &'a [SourceModule]) -> Self {
        let mut items = Self {
            items: Vec::new(),
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
        if self.modules[module.0 as usize].scope.contains_key(&key) {
            let message = match kind {
                ItemKind::Struct | ItemKind::Enum => format!("Type '{shown}' is already defined"),
                ItemKind::Trait => format!("Trait '{shown}' is already defined"),
                ItemKind::Fn => format!("Function '{shown}' is already defined"),
                ItemKind::Const => format!("'{shown}' is already defined"),
            };
            self.errors
                .push(ZeruError::semantic(message, statement.span));
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
            if self.methods.contains_key(&(def, name.clone())) {
                self.errors.push(ZeruError::semantic(
                    format!("Function '{full}' is already defined"),
                    method.span,
                ));
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

    /// `import a.b::{x, y}` lets the importer say `x` for `b::x`.
    fn import(&mut self, module: ModuleId, program: &Program) {
        for statement in &program.statements {
            let StatementKind::Import {
                path,
                symbols: Some(symbols),
            } = &statement.kind
            else {
                continue;
            };
            let short = path.last().cloned().unwrap_or_default();
            let Some(&target) = self.by_name.get(&short) else {
                continue;
            };
            for symbol in symbols {
                let found: Vec<DefId> = [Namespace::Type, Namespace::Trait, Namespace::Value]
                    .into_iter()
                    .filter_map(|ns| {
                        self.modules[target.0 as usize]
                            .scope
                            .get(&(ns, symbol.clone()))
                            .copied()
                    })
                    .collect();
                if found.is_empty() {
                    self.errors.push(ZeruError::semantic(
                        format!("Module '{short}' has no '{symbol}'"),
                        statement.span,
                    ));
                    continue;
                }
                let declared_here = [Namespace::Type, Namespace::Trait, Namespace::Value]
                    .into_iter()
                    .any(|ns| {
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

    /// The item `path` names from inside `from`: `x`, or `module::x`.
    fn lookup(&self, from: ModuleId, namespace: Namespace, path: &str) -> Option<DefId> {
        if let Some((module, name)) = path.split_once("::") {
            let module = *self.by_name.get(module)?;
            return self.modules[module.0 as usize]
                .scope
                .get(&(namespace, name.to_string()))
                .copied();
        }
        let key = (namespace, path.to_string());
        let here = &self.modules[from.0 as usize];
        if let Some(&def) = here.scope.get(&key) {
            return Some(def);
        }
        if let Some((module, name)) = here.imported.get(path) {
            return self.modules[module.0 as usize]
                .scope
                .get(&(namespace, name.clone()))
                .copied();
        }
        [ModuleId::ROOT, ModuleId::PRELUDE]
            .into_iter()
            .filter_map(|module| self.modules[module.0 as usize].scope.get(&key).copied())
            .find(|&def| self.visible(def, from))
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

    /// Whether code in `from` may name `def`: anything public, anything in
    /// the prelude or the root file, and anything of its own module.
    pub fn visible(&self, def: DefId, from: ModuleId) -> bool {
        let item = self.item(def);
        item.is_pub
            || item.module == from
            || item.module == ModuleId::ROOT
            || item.module == ModuleId::PRELUDE && self.modules[from.0 as usize].std
    }

    /// The error for using `def` where it is not visible.
    pub fn private_error(&self, def: DefId, span: Span) -> ZeruError {
        let item = self.item(def);
        let module = self.module_name(item.module).unwrap_or_default();
        ZeruError::semantic(
            format!(
                "'{}' is private to module '{module}'; mark it pub",
                item.name
            ),
            span,
        )
    }
}
