//! The [`Compiler`] state and the [`Compiler::compile_program`] pass pipeline.
//! IR emission itself lives in [`super::body`], [`super::types`],
//! [`super::layout`] and [`super::runtime`].

use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
};

use crate::{
    ast::{Expression, Program, Statement, StatementKind, TypeSpec},
    codegen::SafetyMode,
    errors::ZeruError,
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    pub(super) variables: HashMap<String, VarBinding<'ctx>>,
    pub(super) pointer_elem_types: HashMap<String, BasicTypeEnum<'ctx>>,
    /// Each global constant's value and declared type. It is lowered wherever
    /// the constant is used, inside a function, where LLVM folds it.
    pub(super) constants: HashMap<String, (Expression, Option<BasicTypeEnum<'ctx>>)>,
    pub(super) struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    pub(super) enum_defs: HashMap<String, Vec<String>>,
    pub(super) current_fn: Option<FunctionValue<'ctx>>,

    pub(super) current_struct_context: Option<String>,
    pub(super) loop_stack: Vec<LoopContext<'ctx>>,
    pub(super) safety_mode: SafetyMode,
    pub(super) panic_fn: Option<FunctionValue<'ctx>>,

    pub(super) stdout_stream: Option<PointerValue<'ctx>>,
    pub(super) stderr_stream: Option<PointerValue<'ctx>>,

    pub(super) scope_stack: Vec<Vec<(String, Option<VarBinding<'ctx>>)>>,

    pub errors: Vec<ZeruError>,
}

/// Where a variable lives, its LLVM type, and whether that type is unsigned.
pub(super) type VarBinding<'ctx> = (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool);

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
            scope_stack: vec![Vec::new()],
            errors: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        self.declare_nominal_types(program);
        self.collect_global_constants(program);
        self.lay_out_structs(program);

        self.init_builtin_streams();

        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_prototype(&f.name, f.params, f.return_type);
        });
        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_body(&f.name, f.params, f.body);
        });

        self.create_builtin_cleanup();
    }

    fn declare_nominal_types(&mut self, program: &Program) {
        for stmt in &program.statements {
            match &stmt.kind {
                StatementKind::Struct {
                    name, type_params, ..
                } if type_params.is_empty() => {
                    let struct_type = self.context.opaque_struct_type(name);
                    self.struct_defs
                        .insert(name.clone(), (struct_type, HashMap::new()));
                }
                StatementKind::Enum { name, variants } => {
                    self.enum_defs.insert(name.clone(), variants.clone());
                }
                _ => {}
            }
        }
    }

    fn collect_global_constants(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Var {
                name,
                is_const: true,
                value,
                type_annotation,
            } = &stmt.kind
            {
                let ty = type_annotation
                    .as_ref()
                    .and_then(|spec| self.get_llvm_type(spec));
                self.constants.insert(name.clone(), (value.clone(), ty));
            }
        }
    }

    fn lay_out_structs(&mut self, program: &Program) {
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
    }

    /// Run `emit` over every non-generic function: free functions, then each
    /// struct's methods with `current_struct_context` set so `self` resolves.
    fn for_each_concrete_fn(
        &mut self,
        program: &Program,
        emit: impl Fn(&mut Self, &ConcreteFn<'_>),
    ) {
        for stmt in &program.statements {
            match &stmt.kind {
                StatementKind::Function { .. } => {
                    if let Some(f) = ConcreteFn::from_statement(&stmt.kind, None) {
                        emit(self, &f);
                    }
                }
                StatementKind::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    self.current_struct_context = Some(struct_name.clone());
                    for method in methods {
                        if let Some(f) = ConcreteFn::from_statement(&method.kind, Some(struct_name))
                        {
                            emit(self, &f);
                        }
                    }
                    self.current_struct_context = None;
                }
                _ => {}
            }
        }
    }
}

struct ConcreteFn<'s> {
    name: String,
    params: &'s [(String, TypeSpec, bool)],
    return_type: &'s Option<TypeSpec>,
    body: &'s [Statement],
}

impl<'s> ConcreteFn<'s> {
    fn from_statement(kind: &'s StatementKind, owner: Option<&str>) -> Option<Self> {
        let StatementKind::Function {
            name,
            type_params,
            params,
            return_type,
            body,
        } = kind
        else {
            return None;
        };
        if !type_params.is_empty() {
            return None;
        }
        Some(Self {
            name: match owner {
                Some(struct_name) => format!("{struct_name}::{name}"),
                None => name.clone(),
            },
            params,
            return_type,
            body,
        })
    }
}
