use std::collections::HashMap;

use super::types::Type;

#[derive(Clone)]
pub enum Symbol {
    Var { ty: Type, is_const: bool },
    Function { params: Vec<Type>, ret_type: Type },
}

impl Symbol {
    pub fn type_name(&self) -> String {
        match self {
            Symbol::Var { ty, .. } => ty.to_string(),
            Symbol::Function { params, ret_type } => {
                let params_str = params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("fn({}) {}", params_str, ret_type.to_string())
            }
        }
    }
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
            scope.insert(name, Symbol::Var { ty, is_const });
        }
    }

    pub fn insert_fn(&mut self, name: String, params: Vec<Type>, ret_type: Type) {
        //NOTE: Currently there is only support for global functions so insertion is
        // performed in the current scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Symbol::Function { params, ret_type });
        }
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
