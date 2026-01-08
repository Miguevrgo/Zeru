use std::collections::HashMap;

use super::types::Type;

#[derive(Clone)]
pub enum Symbol {
    Var {
        ty: Type,
        is_const: bool,
        is_moved: bool,
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

    pub fn insert_var(&mut self, name: String, ty: Type, is_const: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                Symbol::Var {
                    ty,
                    is_const,
                    is_moved: false,
                },
            );
        }
    }

    pub fn insert_fn(&mut self, name: String, params: Vec<Type>, ret_type: Type) {
        // NOTE: Currently there is only support for global functions so insertion is
        // performed in the current scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Symbol::Function { params, ret_type });
        }
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

    pub fn get_all_scopes(&self) -> &Vec<HashMap<String, Symbol>> {
        &self.scopes
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        if let Some(scope) = self.scopes.last() {
            scope.get(name)
        } else {
            None
        }
    }
}
