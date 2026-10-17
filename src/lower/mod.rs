//! Lowering of the typed IR to LLVM. Scalars are SSA values; every other
//! value lives in memory and is handled through a pointer to it, copied with
//! `memcpy`. Every function but `main` is internal, so the calling convention
//! is ours: an aggregate argument is a pointer to a copy the callee owns, and
//! an aggregate result is written through a pointer passed first.

mod debug;
mod expr;
mod glue;
mod runtime;
mod types;

use std::collections::HashMap;

use inkwell::{
    IntPredicate, OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::errors::{Sources, Span};
use crate::tir::{Block, Expr, ExprKind, FnId, Function, LocalId, Place, Program, Stmt, StmtKind};
use crate::ty::{Ty, TyKind, Types};

/// A lowered value: nothing, an SSA scalar, or the address of an aggregate.
/// An address may point into a place; whoever keeps the value copies it.
#[derive(Clone, Copy)]
pub(super) enum Val<'ctx> {
    Unit,
    Imm(BasicValueEnum<'ctx>),
    Mem(PointerValue<'ctx>),
}

/// A value something owns: dropped through its flag when its owner goes.
#[derive(Clone, Copy)]
struct Owned<'ctx> {
    slot: PointerValue<'ctx>,
    flag: PointerValue<'ctx>,
    ty: Ty,
}

struct LoopTarget<'ctx> {
    next: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
    scopes: usize,
    temporaries: usize,
}

pub struct Lower<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    program: &'a Program,
    sources: &'a Sources,
    mode: SafetyMode,
    target: TargetData,
    llvm_types: HashMap<Ty, BasicTypeEnum<'ctx>>,
    functions: Vec<FunctionValue<'ctx>>,
    streams: Option<[PointerValue<'ctx>; 2]>,
    flush: Option<FunctionValue<'ctx>>,
    panic: Option<FunctionValue<'ctx>>,
    debug: Option<debug::Debug<'ctx>>,

    current: Option<FunctionValue<'ctx>>,
    function: Option<&'a Function>,
    addresses: Vec<Option<PointerValue<'ctx>>>,
    flags: Vec<Option<PointerValue<'ctx>>>,
    sret: Option<PointerValue<'ctx>>,
    scopes: Vec<Vec<Owned<'ctx>>>,
    temporaries: Vec<Owned<'ctx>>,
    loops: Vec<LoopTarget<'ctx>>,
    span: Span,
}

/// Fill `module` with `program`.
pub fn lower<'ctx>(
    program: &Program,
    context: &'ctx Context,
    module: &Module<'ctx>,
    sources: &Sources,
    mode: SafetyMode,
) {
    let mut lower = Lower {
        context,
        module,
        builder: context.create_builder(),
        program,
        sources,
        mode,
        target: host_layout(module),
        llvm_types: HashMap::new(),
        functions: Vec::new(),
        streams: None,
        flush: None,
        panic: None,
        debug: None,
        current: None,
        function: None,
        addresses: Vec::new(),
        flags: Vec::new(),
        sret: None,
        scopes: Vec::new(),
        temporaries: Vec::new(),
        loops: Vec::new(),
        span: Span::default(),
    };
    lower.init_debug();
    for function in &program.functions {
        let declared = lower.declare(function);
        lower.functions.push(declared);
    }
    lower.init_streams();
    lower.create_flush();
    for at in 0..program.functions.len() {
        lower.define(FnId(at as u32));
    }
    lower.finish_debug();
}

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

impl<'a, 'ctx> Lower<'a, 'ctx> {
    fn types(&self) -> &'a Types {
        &self.program.types
    }

    fn signature(&mut self, function: &Function) -> FunctionType<'ctx> {
        let ptr = self.ptr_type().into();
        let mut params: Vec<BasicMetadataTypeEnum> = Vec::new();
        if self.in_memory(function.ret) && !function.exported {
            params.push(ptr);
        }
        for &param in &function.params {
            let local = &function.locals[param.0 as usize];
            params.push(match local.view || !self.is_scalar(local.ty) {
                true => ptr,
                false => self.llvm_ty(local.ty).into(),
            });
        }
        if function.exported {
            return self.context.i32_type().fn_type(&params, false);
        }
        match function.ret {
            ret if ret == Types::VOID || self.in_memory(ret) => {
                self.context.void_type().fn_type(&params, false)
            }
            ret => self.llvm_ty(ret).fn_type(&params, false),
        }
    }

    /// Whether a value of `ty` is returned through a pointer.
    fn in_memory(&self, ty: Ty) -> bool {
        ty != Types::VOID && !self.is_scalar(ty)
    }

    fn declare(&mut self, function: &Function) -> FunctionValue<'ctx> {
        let fn_type = self.signature(function);
        let linkage = (!function.exported).then_some(Linkage::Internal);
        self.module.add_function(&function.symbol, fn_type, linkage)
    }

    fn define(&mut self, id: FnId) {
        let function = &self.program.functions[id.0 as usize];
        let value = self.functions[id.0 as usize];
        self.current = Some(value);
        self.function = Some(function);
        self.addresses = vec![None; function.locals.len()];
        self.flags = vec![None; function.locals.len()];
        self.scopes = vec![Vec::new()];
        self.temporaries.clear();
        self.loops.clear();

        let entry = self.context.append_basic_block(value, "entry");
        self.builder.position_at_end(entry);
        self.span = function.span;
        self.enter_debug_scope(value, function.span);

        let mut args = value.get_param_iter();
        self.sret = (self.in_memory(function.ret) && !function.exported)
            .then(|| args.next().unwrap().into_pointer_value());
        for (&param, arg) in function.params.iter().zip(args) {
            let local = &function.locals[param.0 as usize];
            let slot = if local.view || !self.is_scalar(local.ty) {
                arg.into_pointer_value()
            } else {
                let slot = self.alloca(local.ty, &local.name);
                self.builder.build_store(slot, arg).unwrap();
                slot
            };
            self.addresses[param.0 as usize] = Some(slot);
            if !local.view {
                self.own(param);
            }
        }

        self.stmts(&function.body.stmts);
        if self.is_open() && (function.ret == Types::VOID || function.exported) {
            self.drop_scopes_from(0);
            self.build_return(None);
        } else if self.is_open() {
            self.builder.build_unreachable().unwrap();
        }
        self.leave_debug_scope();
    }

    fn build_return(&mut self, value: Option<BasicValueEnum<'ctx>>) {
        let function = self.function.unwrap();
        if function.exported {
            let zero = self.context.i32_type().const_zero();
            self.builder.build_return(Some(&zero)).unwrap();
        } else if let Some(value) = value {
            self.builder.build_return(Some(&value)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }

    /// The local's slot owns what it holds, if that owns anything.
    fn own(&mut self, local: LocalId) {
        let ty = self.function.unwrap().locals[local.0 as usize].ty;
        if !self.program.needs_drop(ty) {
            return;
        }
        let slot = self.addresses[local.0 as usize].unwrap();
        let owned = self.raise_flag(slot, ty);
        self.flags[local.0 as usize] = Some(owned.flag);
        self.scopes.last_mut().unwrap().push(owned);
    }

    fn raise_flag(&mut self, slot: PointerValue<'ctx>, ty: Ty) -> Owned<'ctx> {
        let flag = self.entry_flag();
        let raised = self.context.bool_type().const_int(1, false);
        self.builder.build_store(flag, raised).unwrap();
        Owned { slot, flag, ty }
    }

    /// Hand `slot` to the current statement, which drops it as it ends.
    fn adopt_temporary(&mut self, slot: PointerValue<'ctx>, ty: Ty) {
        if self.program.needs_drop(ty) {
            let owned = self.raise_flag(slot, ty);
            self.temporaries.push(owned);
        }
    }

    fn drop_owned(&mut self, owned: Owned<'ctx>) {
        if !self.is_open() {
            return;
        }
        let bool_type = self.context.bool_type();
        let live = self
            .builder
            .build_load(bool_type, owned.flag, "owns")
            .unwrap()
            .into_int_value();
        self.if_then(live, |this| {
            this.call_drop(owned.slot, owned.ty);
            this.builder
                .build_store(owned.flag, bool_type.const_zero())
                .unwrap();
        });
    }

    fn drop_scopes_from(&mut self, depth: usize) {
        let owned: Vec<Owned> = self.scopes[depth..]
            .iter()
            .flatten()
            .rev()
            .copied()
            .collect();
        for owned in owned {
            self.drop_owned(owned);
        }
    }

    fn leave_temporaries(&mut self, mark: usize) {
        let owned: Vec<Owned> = self.temporaries[mark..].iter().rev().copied().collect();
        for owned in owned {
            self.drop_owned(owned);
        }
    }

    fn drop_temporaries(&mut self, mark: usize) {
        self.leave_temporaries(mark);
        self.temporaries.truncate(mark);
    }

    fn drop_everything(&mut self) {
        self.leave_temporaries(0);
        self.drop_scopes_from(0);
    }

    fn is_open(&self) -> bool {
        self.builder
            .get_insert_block()
            .is_some_and(|block| block.get_terminator().is_none())
    }

    fn branch_if_open(&self, target: BasicBlock<'ctx>) {
        if self.is_open() {
            self.builder.build_unconditional_branch(target).unwrap();
        }
    }

    fn block(&mut self, block: &'a Block) {
        self.scopes.push(Vec::new());
        self.stmts(&block.stmts);
        self.drop_scopes_from(self.scopes.len() - 1);
        self.scopes.pop();
    }

    /// Statements after one that leaves are never reached and not lowered.
    fn stmts(&mut self, stmts: &'a [Stmt]) {
        for stmt in stmts {
            if !self.is_open() {
                return;
            }
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &'a Stmt) {
        let outer = std::mem::replace(&mut self.span, stmt.span);
        self.set_debug_location();
        let mark = self.temporaries.len();
        match &stmt.kind {
            StmtKind::Let(local, value) => {
                let ty = self.function.unwrap().locals[local.0 as usize].ty;
                let slot = self.alloca(ty, &self.function.unwrap().locals[local.0 as usize].name);
                self.write(value, slot);
                self.addresses[local.0 as usize] = Some(slot);
                self.own(*local);
            }
            StmtKind::Expr(expr) => {
                let value = self.expr(expr);
                let fresh = !matches!(expr.kind, ExprKind::Read(_) | ExprKind::Move(_));
                if let (Val::Mem(slot), true) = (value, fresh) {
                    self.adopt_temporary(slot, expr.ty);
                }
            }
            StmtKind::Return(value) => self.lower_return(value.as_ref()),
            StmtKind::Break | StmtKind::Continue => {
                let target = self.loops.last().expect("a jump is inside a loop");
                let (scopes, temporaries) = (target.scopes, target.temporaries);
                let to = match stmt.kind {
                    StmtKind::Break => target.exit,
                    _ => target.next,
                };
                self.leave_temporaries(temporaries);
                self.drop_scopes_from(scopes);
                self.builder.build_unconditional_branch(to).unwrap();
            }
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond = self.condition(cond);
                let then_bb = self.append("then");
                let merge = self.append("merge");
                let else_bb = otherwise.as_ref().map_or(merge, |_| self.append("else"));
                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb)
                    .unwrap();
                self.builder.position_at_end(then_bb);
                self.block(then);
                self.branch_if_open(merge);
                if let Some(otherwise) = otherwise {
                    self.builder.position_at_end(else_bb);
                    self.block(otherwise);
                    self.branch_if_open(merge);
                }
                self.builder.position_at_end(merge);
            }
            StmtKind::While { cond, body } => {
                let cond_bb = self.append("loop_cond");
                let body_bb = self.append("loop_body");
                let after = self.append("after_loop");
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                self.builder.position_at_end(cond_bb);
                let cond = self.condition(cond);
                self.builder
                    .build_conditional_branch(cond, body_bb, after)
                    .unwrap();
                self.builder.position_at_end(body_bb);
                self.loop_body(body, cond_bb, after);
                self.branch_if_open(cond_bb);
                self.builder.position_at_end(after);
            }
            StmtKind::ForRange {
                var,
                start,
                end,
                body,
            } => self.for_range(*var, start, end, body),
            StmtKind::ForEach { var, seq, body } => self.for_each(*var, seq, body),
            StmtKind::Block(block) => self.block(block),
        }
        self.drop_temporaries(mark);
        self.span = outer;
        self.set_debug_location();
    }

    fn lower_return(&mut self, value: Option<&'a Expr>) {
        let returned = match value {
            Some(value) if self.sret.is_some() => {
                let sret = self.sret.unwrap();
                self.write(value, sret);
                None
            }
            Some(value) => match self.expr(value) {
                Val::Imm(value) => Some(value),
                _ => None,
            },
            None => None,
        };
        self.drop_everything();
        self.build_return(returned);
    }

    /// A condition, whose temporaries are done with once it is decided.
    fn condition(&mut self, cond: &'a Expr) -> IntValue<'ctx> {
        let mark = self.temporaries.len();
        let value = self.imm(cond).into_int_value();
        self.drop_temporaries(mark);
        value
    }

    fn loop_body(&mut self, body: &'a Block, next: BasicBlock<'ctx>, exit: BasicBlock<'ctx>) {
        self.loops.push(LoopTarget {
            next,
            exit,
            scopes: self.scopes.len(),
            temporaries: self.temporaries.len(),
        });
        self.block(body);
        self.loops.pop();
    }

    fn for_range(&mut self, var: LocalId, start: &'a Expr, end: &'a Expr, body: &'a Block) {
        let ty = self.function.unwrap().locals[var.0 as usize].ty;
        let first = self.imm(start).into_int_value();
        let bound = self.imm(end).into_int_value();
        let slot = self.alloca(ty, "for_index");
        self.builder.build_store(slot, first).unwrap();
        self.addresses[var.0 as usize] = Some(slot);

        let cond_bb = self.append("for_cond");
        let body_bb = self.append("for_body");
        let incr_bb = self.append("for_incr");
        let after = self.append("after_for");
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let index = self
            .builder
            .build_load(first.get_type(), slot, "index")
            .unwrap()
            .into_int_value();
        let signed = self.types().as_int(ty).is_some_and(|int| int.signed());
        let below = if signed {
            IntPredicate::SLT
        } else {
            IntPredicate::ULT
        };
        let more = self
            .builder
            .build_int_compare(below, index, bound, "for_cond")
            .unwrap();
        self.builder
            .build_conditional_branch(more, body_bb, after)
            .unwrap();

        self.builder.position_at_end(body_bb);
        self.loop_body(body, incr_bb, after);
        self.branch_if_open(incr_bb);
        self.builder.position_at_end(incr_bb);
        let next = self
            .builder
            .build_int_add(index, first.get_type().const_int(1, false), "next_index")
            .unwrap();
        self.builder.build_store(slot, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(after);
    }

    fn for_each(&mut self, var: LocalId, seq: &'a Place, body: &'a Block) {
        let local = &self.function.unwrap().locals[var.0 as usize];
        let seq_ty = self.place_ty(seq);
        let container = self.place(seq);
        let usize_type = self.usize_type();
        let (elem, count) = match self.types().kind(seq_ty).clone() {
            TyKind::Array(elem, len) => (elem, usize_type.const_int(len, false)),
            TyKind::Vec(elem) => (elem, self.load_field(container, seq_ty, 1).into_int_value()),
            _ => unreachable!("only arrays and Vecs are walked"),
        };
        let index_slot = self.alloca(Types::USIZE, "for_index");
        self.builder
            .build_store(index_slot, usize_type.const_zero())
            .unwrap();
        let copy_slot = (!local.view).then(|| self.alloca(elem, &local.name));

        let cond_bb = self.append("for_cond");
        let body_bb = self.append("for_body");
        let incr_bb = self.append("for_incr");
        let after = self.append("after_for");
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let index = self
            .builder
            .build_load(usize_type, index_slot, "index")
            .unwrap()
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, count, "for_cond")
            .unwrap();
        self.builder
            .build_conditional_branch(more, body_bb, after)
            .unwrap();

        self.builder.position_at_end(body_bb);
        let element = self.element_ptr(container, seq_ty, index);
        let address = match copy_slot {
            Some(slot) => {
                self.copy_bits(slot, element, elem);
                slot
            }
            None => element,
        };
        self.addresses[var.0 as usize] = Some(address);
        self.loop_body(body, incr_bb, after);
        self.branch_if_open(incr_bb);
        self.builder.position_at_end(incr_bb);
        let next = self
            .builder
            .build_int_add(index, usize_type.const_int(1, false), "next_index")
            .unwrap();
        self.builder.build_store(index_slot, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(after);
    }

    fn append(&self, name: &str) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(self.current.expect("inside a function"), name)
    }

    /// A slot for a `ty` at the top of the entry block.
    fn alloca(&mut self, ty: Ty, name: &str) -> PointerValue<'ctx> {
        let llvm = self.llvm_ty(ty);
        self.entry_builder().build_alloca(llvm, name).unwrap()
    }

    /// A bool slot, false in the entry block before anything else runs.
    fn entry_flag(&self) -> PointerValue<'ctx> {
        let bool_type = self.context.bool_type();
        let builder = self.entry_builder();
        let flag = builder.build_alloca(bool_type, "owns").unwrap();
        builder.build_store(flag, bool_type.const_zero()).unwrap();
        flag
    }

    fn entry_builder(&self) -> Builder<'ctx> {
        let builder = self.context.create_builder();
        let entry = self
            .current
            .and_then(|f| f.get_first_basic_block())
            .expect("a function with an entry");
        match entry.get_first_instruction() {
            Some(first) => builder.position_before(&first),
            None => builder.position_at_end(entry),
        }
        builder
    }

    fn if_then(&mut self, condition: IntValue<'ctx>, then: impl FnOnce(&mut Self)) {
        let then_bb = self.append("then");
        let done = self.append("done");
        self.builder
            .build_conditional_branch(condition, then_bb, done)
            .unwrap();
        self.builder.position_at_end(then_bb);
        then(self);
        self.branch_if_open(done);
        self.builder.position_at_end(done);
    }

    /// Emit the body of `function` through `build`, then return to where the
    /// builder was.
    fn in_helper(&mut self, function: FunctionValue<'ctx>, build: impl FnOnce(&mut Self)) {
        let block = self.builder.get_insert_block();
        let outer = self.current.replace(function);
        let outer_scope = self.swap_debug_scope(None);
        self.builder.unset_current_debug_location();
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        build(self);
        self.current = outer;
        self.swap_debug_scope(outer_scope);
        if let Some(block) = block {
            self.builder.position_at_end(block);
        }
        self.set_debug_location();
    }
}

impl<'ctx> Val<'ctx> {
    fn ptr(self) -> PointerValue<'ctx> {
        match self {
            Val::Mem(ptr) => ptr,
            Val::Imm(value) => value.into_pointer_value(),
            Val::Unit => unreachable!("a void value has no address"),
        }
    }

    fn imm(self) -> BasicValueEnum<'ctx> {
        match self {
            Val::Imm(value) => value,
            _ => unreachable!("an aggregate is not an immediate"),
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub enum SafetyMode {
    #[default]
    Debug,
    ReleaseSafe,
    ReleaseFast,
}

impl SafetyMode {
    pub const fn emit_safety_checks(&self) -> bool {
        matches!(self, SafetyMode::Debug | SafetyMode::ReleaseSafe)
    }

    pub const fn from_flags(release_fast: bool, release_safe: bool) -> Self {
        if release_fast {
            Self::ReleaseFast
        } else if release_safe {
            Self::ReleaseSafe
        } else {
            Self::Debug
        }
    }

    pub const fn clang_flags(&self) -> &[&str] {
        match self {
            Self::Debug => &["-O0", "-g"],
            Self::ReleaseSafe => &["-O2"],
            Self::ReleaseFast => &["-O3"],
        }
    }
}

impl std::fmt::Display for SafetyMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Debug => "unoptimized + debug info",
            Self::ReleaseSafe => "safely optimized",
            Self::ReleaseFast => "optimized",
        })
    }
}
