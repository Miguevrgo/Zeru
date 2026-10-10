//! The [`Compiler`] state and the [`Compiler::compile_program`] pass pipeline.
//! IR emission itself lives in [`super::body`], [`super::types`],
//! [`super::layout`] and [`super::runtime`].

use std::collections::HashMap;

use inkwell::{
    OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{BasicTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
};

use crate::{
    ast::{Expression, Program, Statement, StatementKind, TypeSpec},
    codegen::SafetyMode,
    errors::{Sources, Span, ZeruError},
    sema::{analyzer::SemanticAnalyzer, types::Type},
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub(super) types: &'a SemanticAnalyzer,
    pub(super) sources: &'a Sources,
    pub(super) current_span: Span,

    pub(super) variables: HashMap<String, VarBinding<'ctx>>,
    pub(super) constants: HashMap<String, (Expression, Option<BasicTypeEnum<'ctx>>)>,
    pub(super) struct_defs: HashMap<String, StructType<'ctx>>,
    pub(super) target: TargetData,
    pub(super) current_fn: Option<FunctionValue<'ctx>>,

    pub(super) loop_stack: Vec<LoopContext<'ctx>>,
    pub(super) safety_mode: SafetyMode,
    pub(super) panic_fn: Option<FunctionValue<'ctx>>,

    pub(super) stdout_stream: Option<PointerValue<'ctx>>,
    pub(super) stderr_stream: Option<PointerValue<'ctx>>,

    pub(super) scope_stack: Vec<Scope<'ctx>>,
    pub(super) temporaries: Vec<Owned<'ctx>>,
    pub(super) debug: Option<super::debug::Debug<'ctx>>,

    pub errors: Vec<ZeruError>,
}

/// Where a variable lives, and its LLVM type.
pub(super) type VarBinding<'ctx> = (PointerValue<'ctx>, BasicTypeEnum<'ctx>);

/// Build for the machine compiling, and take its sizes and alignments.
fn host_layout(module: &Module) -> TargetData {
    Target::initialize_native(&InitializationConfig::default()).expect("a native target");
    let triple = TargetMachine::get_default_triple();
    let machine = Target::from_triple(&triple)
        .ok()
        .and_then(|target| {
            target.create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
        })
        .expect("a target machine for the host");
    let layout = machine.get_target_data();
    module.set_triple(&triple);
    module.set_data_layout(&layout.get_data_layout());
    layout
}

pub(super) struct LoopContext<'ctx> {
    pub(super) continue_block: BasicBlock<'ctx>,
    pub(super) break_block: BasicBlock<'ctx>,
    pub(super) scope_depth: usize,
    pub(super) temporaries: usize,
}

/// One block's bindings: what each name shadowed, and the values it owns.
#[derive(Default)]
pub(super) struct Scope<'ctx> {
    pub(super) shadowed: Vec<(String, Option<VarBinding<'ctx>>)>,
    pub(super) owned: Vec<Owned<'ctx>>,
}

/// A value to drop: where it lives, the flag saying it still owns what it
/// holds (a move lowers it), and its type.
#[derive(Clone)]
pub(super) struct Owned<'ctx> {
    pub(super) slot: PointerValue<'ctx>,
    pub(super) flag: PointerValue<'ctx>,
    pub(super) ty: Type,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        types: &'a SemanticAnalyzer,
        sources: &'a Sources,
        safety_mode: SafetyMode,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            types,
            sources,
            current_span: Span::default(),
            variables: HashMap::new(),
            constants: HashMap::new(),
            struct_defs: HashMap::new(),
            target: host_layout(module),
            current_fn: None,
            loop_stack: Vec::new(),
            safety_mode,
            panic_fn: None,
            stdout_stream: None,
            stderr_stream: None,
            scope_stack: vec![Scope::default()],
            temporaries: Vec::new(),
            debug: None,
            errors: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        self.init_debug_info();
        self.declare_structs(program);
        self.collect_global_constants(program);

        self.init_builtin_streams();

        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_prototype(&f.name, f.params);
        });
        self.create_flush();
        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_body(&f.name, f.params, f.body, f.span);
            this.leave_debug_scope();
        });

        self.finish_debug_info();
    }

    fn declare_structs(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct { name, .. } = &stmt.kind {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs.insert(name.clone(), struct_type);
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

    fn for_each_concrete_fn(
        &mut self,
        program: &Program,
        emit: impl Fn(&mut Self, &ConcreteFn<'_>),
    ) {
        for stmt in &program.statements {
            match &stmt.kind {
                StatementKind::Function { .. } => {
                    if let Some(f) = ConcreteFn::from_statement(stmt, None) {
                        emit(self, &f);
                    }
                }
                StatementKind::Struct {
                    name: struct_name,
                    methods,
                    ..
                } => {
                    for method in methods {
                        if let Some(f) = ConcreteFn::from_statement(method, Some(struct_name)) {
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
    span: Span,
}

impl<'s> ConcreteFn<'s> {
    fn from_statement(stmt: &'s Statement, owner: Option<&str>) -> Option<Self> {
        let StatementKind::Function {
            name,
            type_params,
            params,
            body,
            ..
        } = &stmt.kind
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
            span: stmt.span,
        })
    }
}
