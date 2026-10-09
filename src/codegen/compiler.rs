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
    sema::analyzer::SemanticAnalyzer,
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    /// What the analyser resolved: struct fields, enum variants, signatures.
    pub(super) types: &'a SemanticAnalyzer,

    pub(super) variables: HashMap<String, VarBinding<'ctx>>,
    /// Each global constant's value and declared type. It is lowered wherever
    /// the constant is used, inside a function, where LLVM folds it.
    pub(super) constants: HashMap<String, (Expression, Option<BasicTypeEnum<'ctx>>)>,
    pub(super) struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    pub(super) current_fn: Option<FunctionValue<'ctx>>,

    pub(super) loop_stack: Vec<LoopContext<'ctx>>,
    pub(super) safety_mode: SafetyMode,
    pub(super) panic_fn: Option<FunctionValue<'ctx>>,

    pub(super) stdout_stream: Option<PointerValue<'ctx>>,
    pub(super) stderr_stream: Option<PointerValue<'ctx>>,

    pub(super) scope_stack: Vec<Vec<(String, Option<VarBinding<'ctx>>)>>,

    pub errors: Vec<ZeruError>,
}

/// Where a variable lives, and its LLVM type.
pub(super) type VarBinding<'ctx> = (PointerValue<'ctx>, BasicTypeEnum<'ctx>);

pub(super) struct LoopContext<'ctx> {
    pub(super) continue_block: BasicBlock<'ctx>,
    pub(super) break_block: BasicBlock<'ctx>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        types: &'a SemanticAnalyzer,
        safety_mode: SafetyMode,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            types,
            variables: HashMap::new(),
            constants: HashMap::new(),
            struct_defs: HashMap::new(),
            current_fn: None,
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
        self.declare_structs(program);
        self.collect_global_constants(program);
        self.lay_out_structs(program);

        self.init_builtin_streams();

        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_prototype(&f.name, f.params);
        });
        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_body(&f.name, f.params, f.body);
        });

        self.create_builtin_cleanup();
    }

    fn declare_structs(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct { name, .. } = &stmt.kind {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs
                    .insert(name.clone(), (struct_type, HashMap::new()));
            }
        }
    }

    fn collect_global_constants(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Var {
                name,
                is_const: true,
                value,
                ty,
                ..
            } = &stmt.kind
            {
                let ty = ty.as_ref().and_then(|ty| self.llvm_type_of(ty));
                self.constants.insert(name.clone(), (value.clone(), ty));
            }
        }
    }

    fn lay_out_structs(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct { name, .. } = &stmt.kind {
                self.compile_struct_body(name);
            }
        }
    }

    /// Run `emit` over every function: free functions, then struct methods.
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
                    for method in methods {
                        if let Some(f) = ConcreteFn::from_statement(&method.kind, Some(struct_name))
                        {
                            emit(self, &f);
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

struct ConcreteFn<'s> {
    name: String,
    params: &'s [(String, TypeSpec, bool)],
    body: &'s [Statement],
}

impl<'s> ConcreteFn<'s> {
    fn from_statement(kind: &'s StatementKind, owner: Option<&str>) -> Option<Self> {
        let StatementKind::Function {
            name,
            type_params,
            params,
            body,
            ..
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
            body,
        })
    }
}
