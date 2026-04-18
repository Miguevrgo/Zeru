//! Monomorphization of generic functions.

use std::collections::HashMap;

use inkwell::values::FunctionValue;

use crate::{
    ast::{Expression, TypeSpec},
    codegen::compiler::Compiler,
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn monomorphize_call(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> FunctionValue<'ctx> {
        let generic_def = self.generic_functions.get(name).cloned().unwrap();

        let mut type_substitutions: HashMap<String, TypeSpec> = HashMap::new();
        for (i, (_, param_type, _)) in generic_def.params.iter().enumerate() {
            if let TypeSpec::Named(type_param_name) = param_type
                && generic_def
                    .type_params
                    .iter()
                    .any(|tp| &tp.name == type_param_name)
                && let Some(arg) = arguments.get(i)
            {
                let inferred = self.infer_type_from_expression(arg);
                type_substitutions.insert(type_param_name.clone(), inferred);
            }
        }

        let mangled_name =
            self.mangle_generic_name(name, &type_substitutions, &generic_def.type_params);

        if let Some(func) = self.monomorphized.get(&mangled_name) {
            return *func;
        }

        if let Some(func) = self.module.get_function(&mangled_name) {
            return func;
        }

        let substituted_params: Vec<(String, TypeSpec, bool)> = generic_def
            .params
            .iter()
            .map(|(name, ty, is_mut)| {
                (
                    name.clone(),
                    self.substitute_type_spec(ty, &type_substitutions),
                    *is_mut,
                )
            })
            .collect();

        let substituted_return = generic_def
            .return_type
            .as_ref()
            .map(|ty| self.substitute_type_spec(ty, &type_substitutions));

        let func =
            self.compile_fn_prototype(&mangled_name, &substituted_params, &substituted_return);
        self.monomorphized.insert(mangled_name.clone(), func);

        let saved_block = self.builder.get_insert_block();
        let saved_fn = self.current_fn;
        let saved_vars = std::mem::take(&mut self.variables);
        let saved_ptr_elems = std::mem::take(&mut self.pointer_elem_types);
        let saved_subs = std::mem::take(&mut self.current_type_substitutions);

        self.current_type_substitutions = type_substitutions;
        self.compile_fn_body(&mangled_name, &substituted_params, &generic_def.body);

        self.variables = saved_vars;
        self.pointer_elem_types = saved_ptr_elems;
        self.current_type_substitutions = saved_subs;
        self.current_fn = saved_fn;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        func
    }

    pub(super) fn substitute_type_spec(
        &self,
        ty: &TypeSpec,
        subs: &HashMap<String, TypeSpec>,
    ) -> TypeSpec {
        match ty {
            TypeSpec::Named(name) => {
                if let Some(replacement) = subs.get(name) {
                    replacement.clone()
                } else {
                    ty.clone()
                }
            }
            TypeSpec::Pointer(inner) => {
                TypeSpec::Pointer(Box::new(self.substitute_type_spec(inner, subs)))
            }
            TypeSpec::Tuple(elems) => TypeSpec::Tuple(
                elems
                    .iter()
                    .map(|e| self.substitute_type_spec(e, subs))
                    .collect(),
            ),
            TypeSpec::Optional(inner) => {
                TypeSpec::Optional(Box::new(self.substitute_type_spec(inner, subs)))
            }
            TypeSpec::Result(inner) => {
                TypeSpec::Result(Box::new(self.substitute_type_spec(inner, subs)))
            }
            TypeSpec::Generic { name, args } => TypeSpec::Generic {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|a| self.substitute_type_spec(a, subs))
                    .collect(),
            },
            TypeSpec::IntLiteral(_) | TypeSpec::Slice(_) => ty.clone(),
            TypeSpec::Ref(inner) => TypeSpec::Ref(Box::new(self.substitute_type_spec(inner, subs))),
            TypeSpec::RefMut(inner) => {
                TypeSpec::RefMut(Box::new(self.substitute_type_spec(inner, subs)))
            }
        }
    }

    pub(super) fn mangle_generic_name(
        &self,
        base_name: &str,
        subs: &HashMap<String, TypeSpec>,
        type_params: &[crate::ast::TypeParameter],
    ) -> String {
        let mut mangled = base_name.to_string();
        mangled.push_str("__");
        for tp in type_params {
            if let Some(concrete) = subs.get(&tp.name) {
                mangled.push_str(&self.type_spec_to_mangled(concrete));
                mangled.push('_');
            }
        }
        mangled
    }

    pub(super) fn type_spec_to_mangled(&self, ty: &TypeSpec) -> String {
        match ty {
            TypeSpec::Named(name) => name.clone(),
            TypeSpec::Pointer(inner) => format!("ptr_{}", self.type_spec_to_mangled(inner)),
            TypeSpec::Tuple(elems) => {
                let inner: Vec<_> = elems.iter().map(|e| self.type_spec_to_mangled(e)).collect();
                format!("tuple_{}", inner.join("_"))
            }
            TypeSpec::Optional(inner) => format!("opt_{}", self.type_spec_to_mangled(inner)),
            TypeSpec::Result(inner) => format!("res_{}", self.type_spec_to_mangled(inner)),
            TypeSpec::Generic { name, args } => {
                let inner: Vec<_> = args.iter().map(|a| self.type_spec_to_mangled(a)).collect();
                format!("{}_{}", name, inner.join("_"))
            }
            TypeSpec::IntLiteral(n) => format!("lit{}", n),
            TypeSpec::Slice(inner) => format!("slice_{}", self.type_spec_to_mangled(inner)),
            TypeSpec::Ref(inner) => format!("ref_{}", self.type_spec_to_mangled(inner)),
            TypeSpec::RefMut(inner) => format!("refmut_{}", self.type_spec_to_mangled(inner)),
        }
    }
}
