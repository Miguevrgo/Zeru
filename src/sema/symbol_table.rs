use std::collections::HashMap;

use super::types::Type;

#[derive(Clone)]
pub enum Symbol {
    Var {
        ty: Type,
        is_const: bool,
        is_moved: bool,
        /// How many loops enclose the declaration. Moving the variable from a
        /// deeper loop would move it again on every turn.
        loop_depth: usize,
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

    pub fn insert_var(&mut self, name: String, ty: Type, is_const: bool, loop_depth: usize) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Symbol::Var {
                    ty,
                    is_const,
                    is_moved: false,
                    loop_depth,
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

    pub fn mark_moved(&mut self, name: &str) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::Var { is_moved, .. }) = scope.get_mut(name) {
                *is_moved = true;
                return true;
            }
        }
        false
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
