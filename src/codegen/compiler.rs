//! The [`Compiler`] state and the [`Compiler::compile_program`] pass pipeline.
//! IR emission itself lives in [`super::body`], [`super::types`],
//! [`super::layout`], [`super::generics`] and [`super::runtime`].

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

/// Lowers a semantically-validated AST to LLVM IR.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    pub(super) variables: HashMap<String, VarBinding<'ctx>>,
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

    /// One entry per open block, recording what each declaration shadowed so
    /// the outer binding comes back when the block ends.
    pub(super) scope_stack: Vec<Vec<(String, Option<VarBinding<'ctx>>)>>,

    pub errors: Vec<ZeruError>,
}

/// Where a variable lives, its LLVM type, and whether that type is unsigned.
pub(super) type VarBinding<'ctx> = (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool);

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
            scope_stack: vec![Vec::new()],
            errors: Vec::new(),
        }
    }

    /// Lower a whole program. Each pass depends on the one before it: struct
    /// bodies can name other structs, signatures any struct, bodies any function.
    pub fn compile_program(&mut self, program: &Program) {
        self.declare_nominal_types(program);
        self.eval_global_constants(program);
        self.lay_out_structs(program);
        self.collect_generic_functions(program);

        // `print` writes through globals that must exist before any caller.
        self.init_builtin_streams();

        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_prototype(&f.name, f.params, f.return_type);
        });
        self.for_each_concrete_fn(program, |this, f| {
            this.compile_fn_body(&f.name, f.params, f.body);
        });

        self.create_builtin_cleanup();
    }

    /// Opaque LLVM type per struct plus the enum variant lists. Bodies come
    /// later so structs can reference each other.
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

    fn eval_global_constants(&mut self, program: &Program) {
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

    /// Stash generic bodies; [`Compiler::monomorphize_call`] emits them once
    /// concrete type arguments are known.
    fn collect_generic_functions(&mut self, program: &Program) {
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

/// A non-generic function with its name already mangled.
struct ConcreteFn<'s> {
    name: String,
    params: &'s [(String, TypeSpec, bool)],
    return_type: &'s Option<TypeSpec>,
    body: &'s [Statement],
}

impl<'s> ConcreteFn<'s> {
    /// `None` for anything that is not a function, or that is generic.
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
