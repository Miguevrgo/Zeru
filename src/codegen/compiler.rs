//! LLVM IR code generator core.
//!
//! This module owns the [`Compiler`] struct and the high-level
//! [`Compiler::compile_program`] orchestrator. The actual LLVM IR
//! emission logic is split across sibling modules:
//!
//! * [`super::body`]    — statements and expression lowering.
//! * [`super::types`]   — LLVM type mapping, struct/enum layout, signedness queries.
//! * [`super::generics`] — monomorphization of generic functions.
//! * [`super::runtime`] — LLVM/libc glue, builtin streams, `Vec`/`Result` runtime,
//!   inline asm and other rarely-touched plumbing.

use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, StructType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{Program, Statement, StatementKind, TypeSpec},
    codegen::SafetyMode,
    errors::ZeruError,
};

/// LLVM IR code generator for Zeru.
///
/// This compiler takes a semantically-validated AST and generates LLVM IR.
/// It manages LLVM contexts, types, functions, and generates optimized code
/// based on the selected safety mode (Debug, ReleaseSafe, or ReleaseFast).
///
/// # Lifetimes
/// * `'a` - Lifetime of the LLVM builder
/// * `'ctx` - Lifetime of the LLVM context (must outlive the builder)
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    pub(super) variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool)>,
    pub(super) pointer_elem_types: HashMap<String, BasicTypeEnum<'ctx>>,
    pub(super) constants: HashMap<String, BasicValueEnum<'ctx>>,
    pub(super) struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    pub(super) enum_defs: HashMap<String, Vec<String>>,
    pub(super) current_fn: Option<FunctionValue<'ctx>>,

    pub(super) current_struct_context: Option<String>,
    pub(super) loop_stack: Vec<LoopContext<'ctx>>,
    pub(super) safety_mode: SafetyMode,
    pub(super) panic_fn: Option<FunctionValue<'ctx>>,

    pub(super) stdout_stream: Option<PointerValue<'ctx>>,
    pub(super) stderr_stream: Option<PointerValue<'ctx>>,

    pub(super) generic_functions: HashMap<String, GenericFunctionDef>,
    pub(super) monomorphized: HashMap<String, FunctionValue<'ctx>>,
    pub(super) current_type_substitutions: HashMap<String, TypeSpec>,

    pub(super) scope_stack: Vec<Vec<String>>,

    pub errors: Vec<ZeruError>,
}

#[derive(Clone)]
pub(super) struct GenericFunctionDef {
    pub(super) type_params: Vec<crate::ast::TypeParameter>,
    pub(super) params: Vec<(String, TypeSpec, bool)>,
    pub(super) return_type: Option<TypeSpec>,
    pub(super) body: Vec<Statement>,
}

pub(super) struct LoopContext<'ctx> {
    pub(super) continue_block: BasicBlock<'ctx>,
    pub(super) break_block: BasicBlock<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        safety_mode: SafetyMode,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            variables: HashMap::new(),
            pointer_elem_types: HashMap::new(),
            constants: HashMap::new(),
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            current_fn: None,
            current_struct_context: None,
            loop_stack: Vec::new(),
            safety_mode,
            panic_fn: None,
            stdout_stream: None,
            stderr_stream: None,
            generic_functions: HashMap::new(),
            monomorphized: HashMap::new(),
            current_type_substitutions: HashMap::new(),
            scope_stack: vec![Vec::new()], // Global scope
            errors: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct {
                name, type_params, ..
            } = &stmt.kind
                && type_params.is_empty()
            {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs
                    .insert(name.clone(), (struct_type, HashMap::new()));
            }
            if let StatementKind::Enum { name, variants } = &stmt.kind {
                self.enum_defs.insert(name.clone(), variants.clone());
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Var {
                name,
                is_const: true,
                value,
                type_annotation,
            } = &stmt.kind
            {
                let const_val = self.compile_const_expr(value, type_annotation.as_ref());
                self.constants.insert(name.clone(), const_val);
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Struct {
                name,
                fields,
                type_params,
                ..
            } = &stmt.kind
                && type_params.is_empty()
            {
                self.current_struct_context = Some(name.clone());
                self.compile_struct_body(name, fields, stmt.span);
                self.current_struct_context = None;
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Function {
                name,
                type_params,
                params,
                return_type,
                body,
                ..
            } = &stmt.kind
                && !type_params.is_empty()
            {
                self.generic_functions.insert(
                    name.clone(),
                    GenericFunctionDef {
                        type_params: type_params.clone(),
                        params: params.clone(),
                        return_type: return_type.clone(),
                        body: body.clone(),
                    },
                );
            }
        }

        self.init_builtin_streams();

        for stmt in &program.statements {
            if let StatementKind::Function {
                name,
                params,
                return_type,
                type_params,
                ..
            } = &stmt.kind
                && type_params.is_empty()
            {
                self.compile_fn_prototype(name, params, return_type);
            }
            if let StatementKind::Struct {
                name: struct_name,
                methods,
                ..
            } = &stmt.kind
            {
                self.current_struct_context = Some(struct_name.clone());
                for method in methods {
                    if let StatementKind::Function {
                        name: method_name,
                        params,
                        return_type,
                        type_params,
                        ..
                    } = &method.kind
                        && type_params.is_empty()
                    {
                        let mangled_name = format!("{}::{}", struct_name, method_name);
                        self.compile_fn_prototype(&mangled_name, params, return_type);
                    }
                }
                self.current_struct_context = None;
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Function {
                name,
                params,
                body,
                type_params,
                ..
            } = &stmt.kind
                && type_params.is_empty()
            {
                self.compile_fn_body(name, params, body);
            }

            if let StatementKind::Struct {
                name: struct_name,
                methods,
                ..
            } = &stmt.kind
            {
                self.current_struct_context = Some(struct_name.clone());
                for method in methods {
                    if let StatementKind::Function {
                        name: method_name,
                        params,
                        body,
                        type_params,
                        ..
                    } = &method.kind
                        && type_params.is_empty()
                    {
                        let mangled_name = format!("{}::{}", struct_name, method_name);
                        self.compile_fn_body(&mangled_name, params, body);
                    }
                }
                self.current_struct_context = None;
            }
        }

        self.create_builtin_cleanup();
    }
}
