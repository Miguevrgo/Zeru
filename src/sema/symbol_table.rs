use std::collections::HashMap;

use super::types::Type;
use crate::errors::Span;

/// Which variables are moved at some point of a body: the scope each lives
/// in, its name, and where it was moved.
pub type Moves = Vec<(usize, String, Span)>;

#[derive(Clone)]
pub enum Symbol {
    Var {
        ty: Type,
        is_const: bool,
        /// Where its value was given away, while it holds none.
        moved_at: Option<Span>,
        /// A view of a value something else owns: `self`, or a `for` loop's
        /// element. It can be read but not moved.
        is_borrowed: bool,
    },
    Function {
        params: Vec<Type>,
        ret_type: Type,
    },
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Compiler Error: cannot exit global scope")
        }
    }

    pub fn insert_var(&mut self, name: String, ty: Type, is_const: bool, is_borrowed: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Symbol::Var {
                    ty,
                    is_const,
                    moved_at: None,
                    is_borrowed,
                },
            );
        }
    }

    /// Whether `name` resolves to a global, which no local hides.
    pub fn is_global(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rposition(|scope| scope.contains_key(name))
            == Some(0)
    }

    /// A function is always global, wherever the declaration was reached from.
    pub fn insert_fn(&mut self, name: String, params: Vec<Type>, ret_type: Type) {
        self.scopes[0].insert(name, Symbol::Function { params, ret_type });
    }

    /// Record that `name` gave its value away at `span`, or with `None`, that
    /// it holds one again.
    pub fn set_moved(&mut self, name: &str, span: Option<Span>) {
        let found = self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name));
        if let Some(Symbol::Var { moved_at, .. }) = found {
            *moved_at = span;
        }
    }

    /// The variables moved right now.
    pub fn moves(&self) -> Moves {
        let mut moves = Moves::new();
        for (depth, scope) in self.scopes.iter().enumerate() {
            for (name, symbol) in scope {
                if let Symbol::Var {
                    moved_at: Some(span),
                    ..
                } = symbol
                {
                    moves.push((depth, name.clone(), *span));
                }
            }
        }
        moves
    }

    /// Make exactly the variables in `moves` the moved ones.
    pub fn restore_moves(&mut self, moves: &[(usize, String, Span)]) {
        for (depth, scope) in self.scopes.iter_mut().enumerate() {
            for (name, symbol) in scope.iter_mut() {
                if let Symbol::Var { moved_at, .. } = symbol {
                    *moved_at = moves
                        .iter()
                        .find(|(at, moved, _)| *at == depth && moved == name)
                        .map(|(_, _, span)| *span);
                }
            }
        }
    }

    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn get_all_scopes(&self) -> &[HashMap<String, Symbol>] {
        &self.scopes
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    pub fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.scopes.last().and_then(|scope| scope.get(name))
    }
}
