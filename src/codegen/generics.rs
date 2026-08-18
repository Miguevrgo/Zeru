//! Monomorphization of generic functions.

use inkwell::values::FunctionValue;

use crate::{
    ast::{Expression, TypeSpec},
    codegen::compiler::Compiler,
    generics::{Substitutions, mangle, substitute},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Emit (or reuse) the instantiation of a generic function for the concrete
    /// argument types at this call site.
    pub(super) fn monomorphize_call(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> Option<FunctionValue<'ctx>> {
        let generic = self.generic_functions.get(name).cloned()?;

        let mut subs = Substitutions::new();
        for (i, (_, param_type, _)) in generic.params.iter().enumerate() {
            if let TypeSpec::Named(type_param) = param_type
                && generic.type_params.iter().any(|tp| &tp.name == type_param)
                && let Some(arg) = arguments.get(i)
            {
                subs.insert(type_param.clone(), self.infer_type_from_expression(arg));
            }
        }

        let mangled = mangle(name, &generic.type_params, &subs);
        if let Some(existing) = self
            .monomorphized
            .get(&mangled)
            .copied()
            .or_else(|| self.module.get_function(&mangled))
        {
            return Some(existing);
        }

        let params: Vec<(String, TypeSpec, bool)> = generic
            .params
            .iter()
            .map(|(name, ty, is_mut)| (name.clone(), substitute(ty, &subs), *is_mut))
            .collect();
        let return_type = generic.return_type.as_ref().map(|ty| substitute(ty, &subs));

        let func = self.compile_fn_prototype(&mangled, &params, &return_type);
        // Registered before the body is emitted so a recursive call terminates.
        self.monomorphized.insert(mangled.clone(), func);

        // The instantiation is a separate function, so it must not see the
        // caller's locals or enclosing loops.
        let saved_block = self.builder.get_insert_block();
        let saved_fn = self.current_fn;
        let saved_vars = std::mem::take(&mut self.variables);
        let saved_ptr_elems = std::mem::take(&mut self.pointer_elem_types);
        let saved_loops = std::mem::take(&mut self.loop_stack);
        let saved_subs = std::mem::replace(&mut self.current_type_substitutions, subs);

        self.compile_fn_body(&mangled, &params, &generic.body);

        self.variables = saved_vars;
        self.pointer_elem_types = saved_ptr_elems;
        self.loop_stack = saved_loops;
        self.current_type_substitutions = saved_subs;
        self.current_fn = saved_fn;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Some(func)
    }
}
