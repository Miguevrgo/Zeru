//! Lowering of the MIR to LLVM, one block at a time, deciding nothing the
//! MIR has not. Scalars are SSA values; every other value lives in memory
//! and is handled through a pointer to it, copied with `memcpy`. Every
//! function is internal, so the calling convention is ours: an
//! aggregate argument is a pointer to a copy the callee owns, and an
//! aggregate result is written through a pointer passed first.

mod debug;
mod glue;
mod runtime;
mod rvalue;
mod types;

use std::collections::HashMap;

use inkwell::{
    OptimizationLevel,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, ValueKind,
    },
};

use crate::errors::{Sources, Span};
use crate::mir::{
    BlockId, Body, ConstValue, Constant, Local, Operand, Panic, Place, Projection, RETURN, Rvalue,
    StatementKind, TerminatorKind,
};
use crate::tir::{FnId, Program};
use crate::ty::{Ty, Types};

/// A lowered value: nothing, an SSA scalar, or the address of an aggregate.
/// An address may point into a place; whoever keeps the value copies it.
#[derive(Clone, Copy)]
pub(super) enum Val<'ctx> {
    Unit,
    Imm(BasicValueEnum<'ctx>),
    Mem(PointerValue<'ctx>),
}

/// Where a local lives: an SSA value once it is assigned, or a slot.
#[derive(Clone, Copy)]
enum Slot<'ctx> {
    Ssa(Option<BasicValueEnum<'ctx>>),
    Mem(PointerValue<'ctx>),
}

pub struct Lower<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    program: &'a Program,
    bodies: &'a [Body],
    sources: &'a Sources,
    mode: SafetyMode,
    target: TargetData,
    llvm_types: HashMap<Ty, BasicTypeEnum<'ctx>>,
    functions: Vec<FunctionValue<'ctx>>,
    streams: Option<[PointerValue<'ctx>; 2]>,
    heap: Option<PointerValue<'ctx>>,
    flush: Option<FunctionValue<'ctx>>,
    panic: Option<FunctionValue<'ctx>>,
    debug: Option<debug::Debug<'ctx>>,

    current: Option<FunctionValue<'ctx>>,
    body: Option<&'a Body>,
    locals: Vec<Slot<'ctx>>,
    fixed: Vec<bool>,
    blocks: Vec<Option<BasicBlock<'ctx>>>,
    span: Span,
}

/// Fill `module` with the functions of `program`, whose MIR is `bodies`.
pub fn lower<'ctx>(
    program: &Program,
    bodies: &[Body],
    context: &'ctx Context,
    module: &Module<'ctx>,
    sources: &Sources,
    mode: SafetyMode,
    machine: &TargetMachine,
) {
    let target = machine.get_target_data();
    module.set_triple(&machine.get_triple());
    module.set_data_layout(&target.get_data_layout());
    let mut lower = Lower {
        context,
        module,
        builder: context.create_builder(),
        program,
        bodies,
        sources,
        mode,
        target,
        llvm_types: HashMap::new(),
        functions: Vec::new(),
        streams: None,
        heap: None,
        flush: None,
        panic: None,
        debug: None,
        current: None,
        body: None,
        locals: Vec::new(),
        fixed: Vec::new(),
        blocks: Vec::new(),
        span: Span::default(),
    };
    lower.init_debug();
    for body in bodies {
        let declared = lower.declare(body);
        lower.functions.push(declared);
    }
    lower.init_globals();
    lower.create_flush();
    lower.create_start();
    let reached = reached(program, bodies);
    for (at, &reached) in reached.iter().enumerate() {
        match reached {
            true => lower.define(FnId(at as u32)),
            false => unsafe { lower.functions[at].delete() },
        }
    }
    lower.finish_debug();
}

/// The temporaries that only ever hold the constant parts given to them
/// once, which are read where the constant lives rather than copied.
fn fixed_locals(body: &Body) -> Vec<Option<&Constant>> {
    let mut found = vec![None; body.locals.len()];
    let mut touched = vec![0u32; body.locals.len()];
    for block in &body.blocks {
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Assign(place, rvalue) => {
                    touched[place.local.0 as usize] += 1;
                    match rvalue {
                        Rvalue::Use(Operand::Constant(constant))
                            if place.projection.is_empty()
                                && matches!(constant.value, ConstValue::Parts(_)) =>
                        {
                            found[place.local.0 as usize] = Some(constant);
                        }
                        Rvalue::AddressOf(target) | Rvalue::Take(target) => {
                            touched[target.local.0 as usize] += 2;
                        }
                        _ => {}
                    }
                }
                StatementKind::VecPush(place, _)
                | StatementKind::VecInsert(place, ..)
                | StatementKind::VecClear(place)
                | StatementKind::VecReserve(place, _)
                | StatementKind::VecShrinkToFit(place) => touched[place.local.0 as usize] += 2,
                StatementKind::Asm(asm) => {
                    for place in &asm.outputs {
                        touched[place.local.0 as usize] += 2;
                    }
                }
                _ => {}
            }
        }
        match &block.terminator.kind {
            TerminatorKind::Call {
                dest: Some(place), ..
            }
            | TerminatorKind::Drop { place, .. } => {
                touched[place.local.0 as usize] += 2;
            }
            _ => {}
        }
    }
    found
        .into_iter()
        .zip(touched)
        .zip(&body.locals)
        .enumerate()
        .map(|(at, ((found, touched), decl))| {
            found.filter(|_| at > body.arg_count && touched == 1 && decl.name.is_none())
        })
        .collect()
}

/// Which functions the program can call: those `main` or the tests reach,
/// and the ones the generated code calls by itself, the runtime's and the
/// `drop`s.
/// The others, most of the prelude's in a small program, are not built.
fn reached(program: &Program, bodies: &[Body]) -> Vec<bool> {
    let runtime = &program.runtime;
    let mut work: Vec<FnId> = [
        runtime.write_bytes,
        runtime.write_int,
        runtime.write_uint,
        runtime.write_float,
        runtime.flush,
        runtime.alloc,
        runtime.realloc,
        runtime.str_cmp,
        runtime.string_cmp,
    ]
    .into_iter()
    .chain(program.structs.values().filter_map(|layout| layout.drop))
    .chain(program.tests.iter().copied())
    .chain((!program.tests.is_empty()).then_some(runtime.test_index))
    .chain(
        bodies
            .iter()
            .position(|body| body.symbol == "main")
            .map(|at| FnId(at as u32)),
    )
    .collect();
    let mut reached = vec![false; bodies.len()];
    while let Some(id) = work.pop() {
        if std::mem::replace(&mut reached[id.0 as usize], true) {
            continue;
        }
        for block in &bodies[id.0 as usize].blocks {
            if let TerminatorKind::Call { function, .. } = block.terminator.kind {
                work.push(function);
            }
        }
    }
    reached
}

/// The target the program is built for: Linux on x86-64-v2, with SSE4.2
/// and POPCNT, which every x86-64 processor since about 2009 has.
pub fn host_machine(mode: SafetyMode) -> TargetMachine {
    Target::initialize_native(&InitializationConfig::default()).expect("a native target");
    let triple = TargetMachine::get_default_triple();
    Target::from_triple(&triple)
        .ok()
        .and_then(|target| {
            target.create_target_machine(
                &triple,
                "x86-64-v2",
                "",
                mode.opt_level(),
                RelocMode::PIC,
                CodeModel::Default,
            )
        })
        .expect("a target machine for the host")
}

impl<'a, 'ctx> Lower<'a, 'ctx> {
    fn types(&self) -> &'a Types {
        &self.program.types
    }

    fn body(&self) -> &'a Body {
        self.body.expect("inside a function")
    }

    fn local_ty(&self, local: Local) -> Ty {
        self.body().locals[local.0 as usize].ty
    }

    fn signature(&mut self, body: &Body) -> FunctionType<'ctx> {
        let ptr = self.ptr_type().into();
        let ret = body.locals[0].ty;
        let mut params: Vec<BasicMetadataTypeEnum> = Vec::new();
        if self.in_memory(ret) {
            params.push(ptr);
        }
        for decl in &body.locals[1..=body.arg_count] {
            params.push(match self.is_scalar(decl.ty) {
                true => self.llvm_ty(decl.ty).into(),
                false => ptr,
            });
        }
        match ret {
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

    /// A function named as one of start.s takes another symbol, since the
    /// runtime's are global.
    fn declare(&mut self, body: &Body) -> FunctionValue<'ctx> {
        let fn_type = self.signature(body);
        let symbol = match runtime::runtime_symbol(&body.symbol) {
            true => format!("{}.zeru", body.symbol),
            false => body.symbol.clone(),
        };
        self.module
            .add_function(&symbol, fn_type, Some(Linkage::Internal))
    }

    fn define(&mut self, id: FnId) {
        let body = &self.bodies[id.0 as usize];
        let value = self.functions[id.0 as usize];
        self.current = Some(value);
        self.body = Some(body);
        let entry = self.context.append_basic_block(value, "entry");
        self.builder.position_at_end(entry);
        self.span = body.span;
        self.enter_debug_scope(value, body.span);

        let ssa = self.ssa_locals(body);
        let fixed = fixed_locals(body);
        self.fixed = fixed.iter().map(Option::is_some).collect();
        let mut args = value.get_param_iter();
        let ret = body.locals[0].ty;
        let sret = self
            .in_memory(ret)
            .then(|| args.next().unwrap().into_pointer_value());
        self.locals = Vec::with_capacity(body.locals.len());
        for (at, decl) in body.locals.iter().enumerate() {
            let name = decl.name.as_deref().unwrap_or("tmp");
            let arg = (1..=body.arg_count)
                .contains(&at)
                .then(|| args.next().unwrap());
            let slot = match (at, arg) {
                (0, _) if sret.is_some() => Slot::Mem(sret.unwrap()),
                (_, None) if let Some(constant) = fixed[at] => match self.constant(constant) {
                    Val::Mem(global) => Slot::Mem(global),
                    _ => unreachable!("parts are kept in memory"),
                },
                (_, None) if decl.ty == Types::VOID || ssa[at] => Slot::Ssa(None),
                (_, None) => Slot::Mem(self.alloca(decl.ty, name)),
                (_, Some(arg)) if self.is_scalar(decl.ty) => {
                    let slot = self.alloca(decl.ty, name);
                    self.builder.build_store(slot, arg).unwrap();
                    Slot::Mem(slot)
                }
                (_, Some(arg)) => Slot::Mem(arg.into_pointer_value()),
            };
            self.locals.push(slot);
        }

        let order = reverse_postorder(body);
        self.blocks = vec![None; body.blocks.len()];
        self.blocks[0] = Some(entry);
        for &block in &order[1..] {
            self.blocks[block.0 as usize] = Some(self.append("bb"));
        }
        for block in order {
            self.lower_block(block);
        }
        self.leave_debug_scope();
    }

    /// Which locals are SSA values rather than slots: temporaries of a
    /// scalar type assigned once and never borrowed.
    fn ssa_locals(&self, body: &Body) -> Vec<bool> {
        let mut defs = vec![0u32; body.locals.len()];
        let mut borrowed = vec![false; body.locals.len()];
        let mut define = |place: &Place| {
            if let Some(local) = place.as_local() {
                defs[local.0 as usize] += 1;
            }
        };
        for block in &body.blocks {
            for statement in &block.statements {
                match &statement.kind {
                    StatementKind::Assign(place, rvalue) => {
                        define(place);
                        if let Rvalue::AddressOf(place) = rvalue
                            && place.projection.first() != Some(&Projection::Deref)
                        {
                            borrowed[place.local.0 as usize] = true
                        }
                    }
                    StatementKind::Asm(asm) => asm.outputs.iter().for_each(&mut define),
                    _ => {}
                }
            }
            if let TerminatorKind::Call {
                dest: Some(dest), ..
            } = &block.terminator.kind
            {
                define(dest);
            }
        }
        body.locals
            .iter()
            .enumerate()
            .map(|(at, decl)| {
                decl.name.is_none() && defs[at] == 1 && !borrowed[at] && self.is_scalar(decl.ty)
            })
            .collect()
    }

    fn lower_block(&mut self, block: BlockId) {
        let data = &self.body().blocks[block.0 as usize];
        self.builder
            .position_at_end(self.blocks[block.0 as usize].unwrap());
        for statement in &data.statements {
            self.span = statement.span;
            self.set_debug_location();
            self.statement(&statement.kind);
        }
        self.span = data.terminator.span;
        self.set_debug_location();
        self.terminator(&data.terminator.kind);
    }

    fn block_of(&self, block: BlockId) -> BasicBlock<'ctx> {
        self.blocks[block.0 as usize].expect("a reachable block")
    }

    fn statement(&mut self, kind: &'a StatementKind) {
        match kind {
            StatementKind::Assign(place, rvalue) => self.assign(place, rvalue),
            StatementKind::VecPush(vec, item) => self.vec_insert(vec, None, item),
            StatementKind::VecInsert(vec, at, item) => self.vec_insert(vec, Some(at), item),
            StatementKind::VecClear(vec) => self.vec_clear(vec),
            StatementKind::VecReserve(vec, more) => self.vec_reserve(vec, more),
            StatementKind::VecShrinkToFit(vec) => self.vec_shrink(vec),
            StatementKind::CopyBytes(to, from, count) => {
                let to = self.operand(to).imm().into_pointer_value();
                let from = self.operand(from).imm().into_pointer_value();
                let count = self.operand(count).imm().into_int_value();
                self.builder.build_memmove(to, 1, from, 1, count).unwrap();
            }
            StatementKind::StorageDead(_) => {}
            StatementKind::Flush => {
                let flush = self.flush.expect("the prelude declares the streams");
                self.builder.build_call(flush, &[], "").unwrap();
            }
            StatementKind::Print {
                stderr,
                newline,
                pieces,
                values,
            } => self.print(*stderr, *newline, pieces, values),
            StatementKind::Asm(asm) => self.asm(asm),
        }
    }

    fn terminator(&mut self, kind: &'a TerminatorKind) {
        match kind {
            TerminatorKind::Goto(target) => {
                self.builder
                    .build_unconditional_branch(self.block_of(*target))
                    .unwrap();
            }
            TerminatorKind::SwitchInt {
                discr,
                targets,
                otherwise,
            } => {
                let value = self.operand(discr).imm().into_int_value();
                let otherwise = self.block_of(*otherwise);
                match targets.as_slice() {
                    [(0, zero)] if value.get_type().get_bit_width() == 1 => {
                        let zero = self.block_of(*zero);
                        self.builder
                            .build_conditional_branch(value, otherwise, zero)
                            .unwrap();
                    }
                    _ => {
                        let cases: Vec<(IntValue, BasicBlock)> = targets
                            .iter()
                            .map(|(at, target)| {
                                (
                                    Self::int_const(value.get_type(), *at),
                                    self.block_of(*target),
                                )
                            })
                            .collect();
                        self.builder.build_switch(value, otherwise, &cases).unwrap();
                    }
                }
            }
            TerminatorKind::Return => {
                let body = self.body();
                let ret = body.locals[0].ty;
                if ret == Types::VOID || self.in_memory(ret) {
                    self.builder.build_return(None).unwrap();
                } else {
                    let value = self.read(&Place::local(RETURN)).imm();
                    self.builder.build_return(Some(&value)).unwrap();
                }
            }
            TerminatorKind::Unreachable => {
                self.builder.build_unreachable().unwrap();
            }
            TerminatorKind::Call {
                function,
                args,
                dest,
                target,
            } => {
                self.call(*function, args, dest.as_ref());
                self.builder
                    .build_unconditional_branch(self.block_of(*target))
                    .unwrap();
            }
            TerminatorKind::Drop { place, target } => {
                let ty = self.place_ty(place);
                let address = self.place(place);
                self.call_drop(address, ty);
                self.builder
                    .build_unconditional_branch(self.block_of(*target))
                    .unwrap();
            }
            TerminatorKind::Assert {
                cond,
                panic,
                target,
            } => {
                let holds = self.operand(cond).imm().into_int_value();
                let panic = match panic {
                    Panic::Abort => self.abort_block(""),
                    Panic::Null => self.panic_block("null pointer dereference"),
                    Panic::Bounds => self.panic_block("index out of bounds"),
                    Panic::UnwrapNone => self.panic_block("unwrap on None"),
                    Panic::Unwrap => self.panic_block("unwrap on Err"),
                    Panic::UnwrapErr => self.panic_block("unwrap_err on Ok"),
                };
                self.builder
                    .build_conditional_branch(holds, self.block_of(*target), panic)
                    .unwrap();
            }
        }
    }

    /// A call: an aggregate result is written straight into `dest`, an
    /// aggregate argument passed as a pointer to a copy, or to the local
    /// itself when it is moved.
    fn call(&mut self, function: FnId, args: &'a [Operand], dest: Option<&'a Place>) {
        let callee = self.functions[function.0 as usize];
        let ret = self.bodies[function.0 as usize].locals[0].ty;
        let mut lowered: Vec<BasicMetadataValueEnum> = Vec::new();
        let sret = self
            .in_memory(ret)
            .then(|| self.place(dest.expect("a result goes somewhere")));
        if let Some(sret) = sret {
            lowered.push(sret.into());
        }
        for arg in args {
            let ty = self.operand_ty(arg);
            if self.is_scalar(ty) {
                lowered.push(self.operand(arg).imm().into());
                continue;
            }
            let address = match arg {
                Operand::Move(place) if self.is_slot(place) => self.place(place),
                _ => {
                    let value = self.operand(arg);
                    let copy = self.alloca(ty, "arg");
                    self.store_val(copy, value, ty);
                    copy
                }
            };
            lowered.push(address.into());
        }
        let result = self.builder.build_call(callee, &lowered, "call").unwrap();
        if let (None, ValueKind::Basic(value), Some(dest)) =
            (sret, result.try_as_basic_value(), dest)
        {
            self.store(dest, Val::Imm(value), ret);
        }
    }

    /// Whether `place` is a whole local in a slot of its own.
    fn is_slot(&self, place: &Place) -> bool {
        place
            .as_local()
            .is_some_and(|local| matches!(self.locals[local.0 as usize], Slot::Mem(_)))
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
        self.builder.build_unconditional_branch(done).unwrap();
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

/// The reachable blocks of `body`, each after every block that dominates it.
fn reverse_postorder(body: &Body) -> Vec<BlockId> {
    let mut seen = vec![false; body.blocks.len()];
    let mut order = Vec::new();
    let mut stack = vec![(BlockId(0), 0)];
    seen[0] = true;
    while let Some((block, next)) = stack.pop() {
        let successors = body.blocks[block.0 as usize].terminator.kind.successors();
        match successors.get(next) {
            Some(&successor) => {
                stack.push((block, next + 1));
                if !std::mem::replace(&mut seen[successor.0 as usize], true) {
                    stack.push((successor, 0));
                }
            }
            None => order.push(block),
        }
    }
    order.reverse();
    order
}

impl<'ctx> Val<'ctx> {
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

    pub const fn opt_level(&self) -> OptimizationLevel {
        match self {
            Self::Debug => OptimizationLevel::None,
            Self::ReleaseSafe => OptimizationLevel::Default,
            Self::ReleaseFast => OptimizationLevel::Aggressive,
        }
    }

    /// The LLVM pipeline at -O0, which also drops functions nothing calls,
    /// -O2 and -O3.
    pub const fn passes(&self) -> &str {
        match self {
            Self::Debug => "default<O0>,globaldce",
            Self::ReleaseSafe => "default<O2>",
            Self::ReleaseFast => "default<O3>",
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
