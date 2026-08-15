//! Monomorphization of generic functions.

use std::collections::HashMap;

use inkwell::values::FunctionValue;

use crate::{
    ast::{Expression, TypeParameter, TypeSpec},
    codegen::compiler::Compiler,
};

type Substitutions = HashMap<String, TypeSpec>;

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

        let mangled = Self::mangle_generic_name(name, &subs, &generic.type_params);
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
            .map(|(name, ty, is_mut)| (name.clone(), Self::substitute(ty, &subs), *is_mut))
            .collect();
        let return_type = generic
            .return_type
            .as_ref()
            .map(|ty| Self::substitute(ty, &subs));

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

    fn substitute(ty: &TypeSpec, subs: &Substitutions) -> TypeSpec {
        let boxed = |inner: &TypeSpec| Box::new(Self::substitute(inner, subs));
        let all = |types: &[TypeSpec]| types.iter().map(|t| Self::substitute(t, subs)).collect();

        match ty {
            TypeSpec::Named(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
            TypeSpec::Pointer(inner) => TypeSpec::Pointer(boxed(inner)),
            TypeSpec::Optional(inner) => TypeSpec::Optional(boxed(inner)),
            TypeSpec::Result(inner) => TypeSpec::Result(boxed(inner)),
            TypeSpec::Slice(inner) => TypeSpec::Slice(boxed(inner)),
            TypeSpec::Ref(inner) => TypeSpec::Ref(boxed(inner)),
            TypeSpec::RefMut(inner) => TypeSpec::RefMut(boxed(inner)),
            TypeSpec::Tuple(elems) => TypeSpec::Tuple(all(elems)),
            TypeSpec::Generic { name, args } => TypeSpec::Generic {
                name: name.clone(),
                args: all(args),
            },
            TypeSpec::IntLiteral(_) => ty.clone(),
        }
    }

    fn mangle_generic_name(
        base_name: &str,
        subs: &Substitutions,
        type_params: &[TypeParameter],
    ) -> String {
        let mut mangled = format!("{base_name}__");
        for tp in type_params {
            if let Some(concrete) = subs.get(&tp.name) {
                mangled.push_str(&Self::mangle_type(concrete));
                mangled.push('_');
            }
        }
        mangled
    }

    fn mangle_type(ty: &TypeSpec) -> String {
        let inner = Self::mangle_type;
        let joined = |types: &[TypeSpec]| {
            types
                .iter()
                .map(Self::mangle_type)
                .collect::<Vec<_>>()
                .join("_")
        };

        match ty {
            TypeSpec::Named(name) => name.clone(),
            TypeSpec::IntLiteral(n) => format!("lit{n}"),
            TypeSpec::Pointer(t) => format!("ptr_{}", inner(t)),
            TypeSpec::Optional(t) => format!("opt_{}", inner(t)),
            TypeSpec::Result(t) => format!("res_{}", inner(t)),
            TypeSpec::Slice(t) => format!("slice_{}", inner(t)),
            TypeSpec::Ref(t) => format!("ref_{}", inner(t)),
            TypeSpec::RefMut(t) => format!("refmut_{}", inner(t)),
            TypeSpec::Tuple(elems) => format!("tuple_{}", joined(elems)),
            TypeSpec::Generic { name, args } => format!("{name}_{}", joined(args)),
        }
    }
}
