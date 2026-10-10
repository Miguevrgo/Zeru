//! LLVM/libc glue: allocator declarations, panics, inline asm, builtin print
//! streams, and the `Vec`/`T!` runtime.

use inkwell::{
    AddressSpace, IntPredicate,
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    intrinsics::Intrinsic,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, PointerType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
        ValueKind,
    },
};

use crate::{
    ast::{AsmOperand, Expression, ExpressionKind, format_pieces},
    codegen::{
        compiler::Compiler,
        layout::{
            OPTION_TAG, OPTION_VALUE, RESULT_ERR, RESULT_TAG, RESULT_VALUE, SLICE_LEN, SLICE_PTR,
            VEC_CAP, VEC_LEN, VEC_PTR,
        },
    },
    errors::{Span, ZeruError},
    sema::types::{Signedness, Type},
    token::Token,
};

const ALLOC_FN: &str = "__zeru_alloc";
const REALLOC_FN: &str = "__zeru_realloc";
const MEMMOVE_FN: &str = "__zeru_memmove";
pub const FLUSH_FN: &str = "__zeru_flush";

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(ZeruError::semantic(message, span));
    }

    /// Fallback value returned after an error is recorded, so lowering can continue.
    pub(super) fn dummy_val(&self) -> BasicValueEnum<'ctx> {
        self.context.i32_type().const_int(0, false).into()
    }

    /// A read-only global holding `bytes` and a terminating NUL, which a
    /// syscall taking a C string needs; a `\0` inside is kept as data.
    pub(super) fn const_bytes(&self, bytes: &[u8]) -> PointerValue<'ctx> {
        let value = self.context.const_string(bytes, true);
        let global = self.module.add_global(value.get_type(), None, "str");
        global.set_initializer(&value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.as_pointer_value()
    }

    pub(super) fn ptr_type(&self) -> PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    pub(super) fn usize_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    pub(super) fn load(
        &self,
        ty: impl BasicType<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        self.builder.build_load(ty, ptr, name).unwrap()
    }

    pub(super) fn load_int(
        &self,
        ty: IntType<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> IntValue<'ctx> {
        self.load(ty, ptr, name).into_int_value()
    }

    fn load_ptr(&self, ptr: PointerValue<'ctx>, name: &str) -> PointerValue<'ctx> {
        self.load(self.ptr_type(), ptr, name).into_pointer_value()
    }

    pub(super) fn extract(
        &self,
        agg: StructValue<'ctx>,
        field: u32,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        self.builder.build_extract_value(agg, field, name).unwrap()
    }

    fn extern_fn(&self, name: &str, fn_type: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        self.module.get_function(name).unwrap_or_else(|| {
            self.module
                .add_function(name, fn_type, Some(Linkage::External))
        })
    }

    fn call_ptr(
        &self,
        callee: FunctionValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
        name: &str,
    ) -> PointerValue<'ctx> {
        match self
            .builder
            .build_call(callee, args, name)
            .unwrap()
            .try_as_basic_value()
        {
            ValueKind::Basic(v) => v.into_pointer_value(),
            _ => self.ptr_type().const_null(),
        }
    }

    pub(super) fn vec_field_ptr(
        &self,
        vec_ptr: PointerValue<'ctx>,
        field: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_struct_gep(self.vec_type(), vec_ptr, field, name)
            .unwrap()
    }

    pub(super) fn vec_elem_ptr(
        &self,
        data_ptr: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        unsafe {
            self.builder
                .build_gep(elem_type, data_ptr, &[index], "elem_ptr")
                .unwrap()
        }
    }

    /// Bytes taken by `count` elements, as an LLVM constant expression.
    pub(super) fn bytes_for(
        &self,
        elem_type: BasicTypeEnum<'ctx>,
        count: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let stride = elem_type
            .size_of()
            .unwrap_or(self.usize_type().const_int(1, false));
        self.builder
            .build_int_mul(count, stride, "byte_size")
            .unwrap()
    }

    pub(super) fn zero_value_for(&self, ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match ty {
            BasicTypeEnum::IntType(t) => t.const_int(0, false).into(),
            BasicTypeEnum::FloatType(t) => t.const_float(0.0).into(),
            BasicTypeEnum::PointerType(t) => t.const_null().into(),
            BasicTypeEnum::StructType(t) => t.get_undef().into(),
            BasicTypeEnum::ArrayType(t) => t.get_undef().into(),
            BasicTypeEnum::VectorType(t) => t.get_undef().into(),
            BasicTypeEnum::ScalableVectorType(t) => t.get_undef().into(),
        }
    }

    /// Append `func` to `llvm.global_ctors` or `llvm.global_dtors`.
    fn register_global_array(&mut self, array_name: &str, func: FunctionValue<'ctx>) {
        let i32_type = self.context.i32_type();
        let ptr_type = self.ptr_type();
        let entry_type = self
            .context
            .struct_type(&[i32_type.into(), ptr_type.into(), ptr_type.into()], false);

        let entry = entry_type.const_named_struct(&[
            i32_type.const_int(65535, false).into(),
            func.as_global_value().as_pointer_value().into(),
            ptr_type.const_null().into(),
        ]);

        let entries = entry_type.const_array(&[entry]);
        let global = self.module.add_global(
            entries.get_type(),
            Some(AddressSpace::default()),
            array_name,
        );
        global.set_linkage(Linkage::Appending);
        global.set_initializer(&entries);
    }

    /// Emit the body of `function` through `build`, then put the builder
    /// back where it was.
    pub(super) fn in_helper(
        &mut self,
        function: FunctionValue<'ctx>,
        build: impl FnOnce(&mut Self),
    ) {
        let block = self.builder.get_insert_block();
        let outer_fn = self.current_fn.replace(function);
        let outer_scope = self.swap_debug_scope(None);
        self.builder.unset_current_debug_location();

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        build(self);

        self.current_fn = outer_fn;
        self.swap_debug_scope(outer_scope);
        if let Some(block) = block {
            self.builder.position_at_end(block);
        }
        self.set_debug_location();
    }

    /// What every failed check calls: flush what was printed, write the
    /// message to stderr, abort.
    fn panic_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.panic_fn {
            return f;
        }
        let void = self.context.void_type();
        let f = self.module.add_function(
            "__zeru_panic",
            void.fn_type(&[self.ptr_type().into(), self.usize_type().into()], false),
            Some(Linkage::Internal),
        );
        for attribute in ["noreturn", "cold"] {
            let kind = Attribute::get_named_enum_kind_id(attribute);
            f.add_attribute(
                AttributeLoc::Function,
                self.context.create_enum_attribute(kind, 0),
            );
        }
        let abort_fn = self.extern_fn("abort", void.fn_type(&[], false));

        self.in_helper(f, |this| {
            if let (Some(err), Some(write), Some(flush)) = (
                this.stderr_stream,
                this.module.get_function("OutStream::write_bytes"),
                this.module.get_function(FLUSH_FN),
            ) {
                let (text, len) = (f.get_nth_param(0).unwrap(), f.get_nth_param(1).unwrap());
                let b = this.builder;
                b.build_call(write, &[err.into(), text.into(), len.into()], "")
                    .unwrap();
                b.build_call(flush, &[], "").unwrap();
            }
            this.builder.build_call(abort_fn, &[], "").unwrap();
            this.builder.build_unreachable().unwrap();
        });
        self.panic_fn = Some(f);
        f
    }

    /// Report what went wrong and where, then abort.
    fn build_panic(&mut self, from: inkwell::basic_block::BasicBlock<'ctx>, label: &str) {
        let what = match label {
            "null" => "null pointer dereference",
            "bounds" => "index out of bounds",
            "div" => "division by zero, or MIN / -1",
            "shift" => "shift by the operand's width or more",
            "overflow" => "arithmetic overflow",
            "unwrap_none" => "unwrap on None",
            "unwrap" => "unwrap on Err",
            "unwrap_err" => "unwrap_err on Ok",
            _ => label,
        };
        let message = match self.sources.position(self.current_span) {
            Some((file, line, column)) => format!("panic at {file}:{line}:{column}: {what}\n"),
            None => format!("panic: {what}\n"),
        };

        let panic_fn = self.panic_fn();
        self.builder.position_at_end(from);
        let text = self.const_bytes(message.as_bytes());
        let len = self.usize_type().const_int(message.len() as u64, false);
        self.builder
            .build_call(panic_fn, &[text.into(), len.into()], "")
            .unwrap();
        self.builder.build_unreachable().unwrap();
    }

    /// Abort when `condition` holds, then carry on in a fresh block. Every
    /// safety check funnels through here.
    fn emit_trap_if(&mut self, condition: IntValue<'ctx>, label: &str) {
        let Some(current_fn) = self.current_fn else {
            return;
        };
        let panic_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_panic"));
        let ok_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_ok"));

        self.builder
            .build_conditional_branch(condition, panic_bb, ok_bb)
            .unwrap();

        self.build_panic(panic_bb, label);
        self.builder.position_at_end(ok_bb);
    }

    pub(super) fn emit_null_check(&mut self, ptr: PointerValue<'ctx>) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }
        let is_null = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                ptr,
                self.ptr_type().const_null(),
                "is_null",
            )
            .unwrap();
        self.emit_trap_if(is_null, "null");
    }

    /// Trap when `index` falls outside `0..len`. One unsigned compare covers
    /// both ends: a negative index wraps to a value above any length.
    pub(super) fn emit_bounds_check(&mut self, index: IntValue<'ctx>, len: u64, unsigned: bool) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }
        // A constant index needs no check; the analyser already rejected the
        // ones that do not fit.
        if let Some(constant) = index.get_sign_extended_constant()
            && (0..len as i64).contains(&constant)
        {
            return;
        }

        let len = self.usize_type().const_int(len, false);
        self.emit_bounds_check_against(index, len, unsigned);
    }

    /// The same check where the length is only known once it runs, as for a Vec.
    pub(super) fn emit_bounds_check_against(
        &mut self,
        index: IntValue<'ctx>,
        len: IntValue<'ctx>,
        unsigned: bool,
    ) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }

        let usize_type = self.usize_type();
        let index = match index
            .get_type()
            .get_bit_width()
            .cmp(&usize_type.get_bit_width())
        {
            std::cmp::Ordering::Less if unsigned => self
                .builder
                .build_int_z_extend(index, usize_type, "idx")
                .unwrap(),
            std::cmp::Ordering::Less => self
                .builder
                .build_int_s_extend(index, usize_type, "idx")
                .unwrap(),
            _ => index,
        };

        let out_of_range = self
            .builder
            .build_int_compare(IntPredicate::UGE, index, len, "out_of_range")
            .unwrap();
        self.emit_trap_if(out_of_range, "bounds");
    }

    /// Trap on a zero divisor, and on `MIN / -1`, whose result has no
    /// representation and which LLVM leaves undefined.
    pub(super) fn emit_division_check(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        signed: bool,
    ) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }
        let int_type = rhs.get_type();
        let mut invalid = self
            .builder
            .build_int_compare(IntPredicate::EQ, rhs, int_type.const_zero(), "div_zero")
            .unwrap();

        if signed {
            let min = int_type.const_int(1 << (int_type.get_bit_width() - 1), false);
            let lhs_is_min = self
                .builder
                .build_int_compare(IntPredicate::EQ, lhs, min, "lhs_is_min")
                .unwrap();
            let rhs_is_neg_one = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    rhs,
                    int_type.const_all_ones(),
                    "rhs_is_neg_one",
                )
                .unwrap();
            let overflows = self
                .builder
                .build_and(lhs_is_min, rhs_is_neg_one, "div_overflow")
                .unwrap();
            invalid = self
                .builder
                .build_or(invalid, overflows, "div_invalid")
                .unwrap();
        }

        self.emit_trap_if(invalid, "div");
    }

    /// LLVM leaves a shift by the operand width or more undefined.
    pub(super) fn emit_shift_check(&mut self, amount: IntValue<'ctx>) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }
        let int_type = amount.get_type();
        let width = int_type.const_int(int_type.get_bit_width() as u64, false);
        let too_wide = self
            .builder
            .build_int_compare(IntPredicate::UGE, amount, width, "shift_wide")
            .unwrap();
        self.emit_trap_if(too_wide, "shift");
    }

    /// `+`, `-` and `*` through LLVM's overflow intrinsic, trapping when it
    /// reports one. `None` if the intrinsic is unavailable, so the caller can
    /// fall back to the plain operation.
    pub(super) fn build_checked_int_arith(
        &mut self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        op: &Token,
        signed: bool,
    ) -> Option<BasicValueEnum<'ctx>> {
        let name = match (op, signed) {
            (Token::Plus, true) => "llvm.sadd.with.overflow",
            (Token::Plus, false) => "llvm.uadd.with.overflow",
            (Token::Minus, true) => "llvm.ssub.with.overflow",
            (Token::Minus, false) => "llvm.usub.with.overflow",
            (Token::Star, true) => "llvm.smul.with.overflow",
            (Token::Star, false) => "llvm.umul.with.overflow",
            _ => return None,
        };

        let declaration =
            Intrinsic::find(name)?.get_declaration(self.module, &[lhs.get_type().into()])?;
        let ValueKind::Basic(BasicValueEnum::StructValue(pair)) = self
            .builder
            .build_call(declaration, &[lhs.into(), rhs.into()], "arith")
            .unwrap()
            .try_as_basic_value()
        else {
            return None;
        };

        let overflowed = self.extract(pair, 1, "overflowed").into_int_value();
        self.emit_trap_if(overflowed, "overflow");
        Some(self.extract(pair, 0, "arith_val"))
    }

    /// Emit `body` once per index in `0..count`, leaving the builder after
    /// the loop.
    pub(super) fn build_counted_loop(
        &mut self,
        count: IntValue<'ctx>,
        mut body: impl FnMut(&mut Self, IntValue<'ctx>),
    ) {
        let Some(function) = self.current_fn else {
            return;
        };
        let usize_type = self.usize_type();
        let index_ptr = self.create_entry_block_alloca(function, "i", usize_type.into());
        self.builder
            .build_store(index_ptr, usize_type.const_zero())
            .unwrap();

        let cond_bb = self.context.append_basic_block(function, "count_cond");
        let body_bb = self.context.append_basic_block(function, "count_body");
        let after_bb = self.context.append_basic_block(function, "count_after");
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(cond_bb);
        let index = self.load_int(usize_type, index_ptr, "i");
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, count, "more")
            .unwrap();
        self.builder
            .build_conditional_branch(more, body_bb, after_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        body(self, index);
        let next = self
            .builder
            .build_int_add(index, usize_type.const_int(1, false), "next")
            .unwrap();
        self.builder.build_store(index_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(after_bb);
    }

    /// A bool slot, set to false in the entry block before anything else runs.
    pub(super) fn create_entry_flag(&self, function: FunctionValue<'ctx>) -> PointerValue<'ctx> {
        let bool_type = self.context.bool_type();
        let builder = self.entry_builder(function);
        let flag = builder.build_alloca(bool_type, "owns").unwrap();
        builder.build_store(flag, bool_type.const_zero()).unwrap();
        flag
    }

    /// A buffer for `count` elements of `elem_type`.
    pub(super) fn alloc_buffer(
        &self,
        elem_type: BasicTypeEnum<'ctx>,
        count: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let usize_type = self.usize_type();
        let alloc_fn = self.extern_fn(
            ALLOC_FN,
            self.ptr_type().fn_type(&[usize_type.into()], false),
        );
        let size = self.bytes_for(elem_type, count);
        self.call_ptr(alloc_fn, &[size.into()], "buffer")
    }

    /// Give back a buffer that held `capacity` elements.
    pub(super) fn free_buffer(
        &self,
        data: PointerValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
        capacity: IntValue<'ctx>,
    ) {
        let size = self.bytes_for(elem_type, capacity);
        // Resizing to nothing is how the allocator frees.
        self.resize_buffer(data, size, self.usize_type().const_zero());
    }

    fn resize_buffer(
        &self,
        data: PointerValue<'ctx>,
        old_size: IntValue<'ctx>,
        new_size: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let ptr_type = self.ptr_type();
        let usize_type = self.usize_type();
        let realloc_fn = self.extern_fn(
            REALLOC_FN,
            ptr_type.fn_type(
                &[ptr_type.into(), usize_type.into(), usize_type.into()],
                false,
            ),
        );
        self.call_ptr(
            realloc_fn,
            &[data.into(), old_size.into(), new_size.into()],
            "resized",
        )
    }

    /// Copy `count` elements from `src` to `dst`; the two may overlap.
    pub(super) fn move_bytes(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
        count: IntValue<'ctx>,
    ) {
        let ptr_type = self.ptr_type();
        let memmove_fn = self.extern_fn(
            MEMMOVE_FN,
            self.context.void_type().fn_type(
                &[ptr_type.into(), ptr_type.into(), self.usize_type().into()],
                false,
            ),
        );
        let size = self.bytes_for(elem_type, count);
        self.builder
            .build_call(memmove_fn, &[dst.into(), src.into(), size.into()], "")
            .unwrap();
    }

    pub(super) fn create_entry_block_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        self.entry_builder(function).build_alloca(ty, name).unwrap()
    }

    /// A builder at the top of `function`'s entry block, so what it emits
    /// comes before anything else.
    fn entry_builder(&self, function: FunctionValue<'ctx>) -> Builder<'ctx> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        builder
    }

    /// Lower an `asm` block: build the constraint string, call the inline asm
    /// value, then write each output back to its lvalue.
    pub(super) fn compile_inline_asm(
        &mut self,
        template: &str,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
        clobbers: &[String],
        is_volatile: bool,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let constraints: Vec<String> = outputs
            .iter()
            .chain(inputs)
            .map(|op| op.constraint.clone())
            .chain(clobbers.iter().map(|c| format!("~{{{c}}}")))
            .collect();

        let input_values: Vec<BasicValueEnum<'ctx>> = inputs
            .iter()
            .map(|inp| self.compile_expression(&inp.expr, None))
            .collect();

        let word = self.usize_type().as_basic_type_enum();
        let output_type = match outputs.len() {
            0 => word,
            1 => expected_type.unwrap_or(word),
            n => self.context.struct_type(&vec![word; n], false).into(),
        };

        let param_types: Vec<_> = input_values.iter().map(|v| v.get_type().into()).collect();
        let asm_fn_type = match output_type {
            BasicTypeEnum::IntType(t) => t.fn_type(&param_types, false),
            BasicTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            BasicTypeEnum::StructType(t) => t.fn_type(&param_types, false),
            _ => self.usize_type().fn_type(&param_types, false),
        };

        let asm_val = self.context.create_inline_asm(
            asm_fn_type,
            template.to_string(),
            constraints.join(","),
            is_volatile,
            false,
            None,
            false,
        );

        let args: Vec<BasicMetadataValueEnum<'ctx>> =
            input_values.iter().map(|v| (*v).into()).collect();
        let result = match self
            .builder
            .build_indirect_call(asm_fn_type, asm_val, &args, "asm_result")
            .unwrap()
            .try_as_basic_value()
        {
            ValueKind::Basic(value) => value,
            ValueKind::Instruction(_) => self.usize_type().const_zero().into(),
        };

        for (i, out) in outputs.iter().enumerate() {
            let Some((ptr, _)) = self.compile_lvalue(&out.expr) else {
                continue;
            };
            let val = if outputs.len() == 1 {
                result
            } else {
                self.extract(result.into_struct_value(), i as u32, "asm_out")
            };
            self.builder.build_store(ptr, val).unwrap();
        }

        result
    }

    pub(super) fn compile_vec_static_method(
        &mut self,
        method_name: &str,
        arguments: &[Expression],
        elem_type: BasicTypeEnum<'ctx>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        let usize_type = self.usize_type();
        let zero = usize_type.const_zero();

        let (data, cap) = match method_name {
            "new" => (self.ptr_type().const_null(), zero),
            "with_capacity" => {
                let cap = match arguments.first() {
                    Some(arg) => self
                        .compile_expression(arg, Some(usize_type.into()))
                        .into_int_value(),
                    None => zero,
                };
                (self.alloc_buffer(elem_type, cap), cap)
            }
            _ => {
                self.error(
                    format!("Unknown Vec static method '{method_name}'"),
                    call_span,
                );
                return self.dummy_val();
            }
        };
        self.build_vec(data, zero, cap)
    }

    /// A Vec header over `data`.
    pub(super) fn build_vec(
        &self,
        data: PointerValue<'ctx>,
        len: IntValue<'ctx>,
        cap: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.build_struct(
            self.vec_type(),
            &[data.into(), len.into(), cap.into()],
            "vec",
        )
        .into()
    }

    /// A method on the Vec at `vec_ptr`. `elem` is the element type, which
    /// `clear` needs to drop what it removes.
    pub(super) fn compile_vec_method(
        &mut self,
        method_name: &str,
        vec_ptr: PointerValue<'ctx>,
        arguments: &[Expression],
        elem: &Type,
    ) -> Option<BasicValueEnum<'ctx>> {
        let elem_type = self.llvm_type_of(elem)?;
        let usize_type = self.usize_type();
        let one = usize_type.const_int(1, false);
        let unit = self.dummy_val();
        let len_field = self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field");
        let cap_field = self.vec_field_ptr(vec_ptr, VEC_CAP, "cap_field");
        let ptr_field = self.vec_field_ptr(vec_ptr, VEC_PTR, "ptr_field");
        let index_arg = |this: &mut Self| -> Option<IntValue<'ctx>> {
            let index = this.compile_expression(arguments.first()?, Some(usize_type.into()));
            Some(index.into_int_value())
        };
        let item_arg = |this: &mut Self| -> Option<BasicValueEnum<'ctx>> {
            Some(this.compile_expression(arguments.last()?, Some(elem_type)))
        };

        match method_name {
            "len" => Some(self.load(usize_type, len_field, "len")),
            "capacity" => Some(self.load(usize_type, cap_field, "cap")),
            "is_empty" => {
                let len = self.load_int(usize_type, len_field, "len");
                let zero = usize_type.const_zero();
                let is_empty =
                    self.builder
                        .build_int_compare(IntPredicate::EQ, len, zero, "is_empty");
                Some(is_empty.unwrap().into())
            }
            "push" | "insert" => {
                let at: Option<IntValue> = match method_name {
                    "insert" => Some(index_arg(self)?),
                    _ => None,
                };
                let item: BasicValueEnum = item_arg(self)?;
                let len = self.load_int(usize_type, len_field, "len");
                let grown = self.builder.build_int_add(len, one, "grown").unwrap();
                let at = match at {
                    // At the end is a place to insert, past it is not.
                    Some(at) => {
                        self.emit_bounds_check_against(at, grown, true);
                        at
                    }
                    None => len,
                };
                self.build_vec_reserve(vec_ptr, grown, elem_type);

                let data = self.load_ptr(ptr_field, "data");
                let slot = self.vec_elem_ptr(data, at, elem_type);
                if method_name == "insert" {
                    let next = self.builder.build_int_add(at, one, "next").unwrap();
                    let after = self.vec_elem_ptr(data, next, elem_type);
                    let tail = self.builder.build_int_sub(len, at, "tail").unwrap();
                    self.move_bytes(after, slot, elem_type, tail);
                }
                self.builder.build_store(slot, item).unwrap();
                self.builder.build_store(len_field, grown).unwrap();
                Some(unit)
            }
            "remove" => {
                let at: IntValue = index_arg(self)?;
                let len = self.load_int(usize_type, len_field, "len");
                self.emit_bounds_check_against(at, len, true);
                let data = self.load_ptr(ptr_field, "data");
                let slot = self.vec_elem_ptr(data, at, elem_type);
                let removed = self.load(elem_type, slot, "removed");
                let next = self.builder.build_int_add(at, one, "next").unwrap();
                let after = self.vec_elem_ptr(data, next, elem_type);
                let tail = self.builder.build_int_sub(len, next, "tail").unwrap();
                self.move_bytes(slot, after, elem_type, tail);
                let shrunk = self.builder.build_int_sub(len, one, "shrunk").unwrap();
                self.builder.build_store(len_field, shrunk).unwrap();
                Some(removed)
            }
            "reserve" => {
                let more: IntValue = index_arg(self)?;
                let len = self.load_int(usize_type, len_field, "len");
                let needed = self.builder.build_int_add(len, more, "needed").unwrap();
                self.build_vec_reserve(vec_ptr, needed, elem_type);
                Some(unit)
            }
            "shrink_to_fit" => {
                let len = self.load_int(usize_type, len_field, "len");
                let cap = self.load_int(usize_type, cap_field, "cap");
                let data = self.load_ptr(ptr_field, "data");
                let old_size = self.bytes_for(elem_type, cap);
                let new_size = self.bytes_for(elem_type, len);
                let resized = self.resize_buffer(data, old_size, new_size);
                self.builder.build_store(ptr_field, resized).unwrap();
                self.builder.build_store(cap_field, len).unwrap();
                Some(unit)
            }
            "pop" => {
                let len = self.load_int(usize_type, len_field, "len");
                let has_elem = self
                    .builder
                    .build_int_compare(IntPredicate::NE, len, usize_type.const_zero(), "has_elem")
                    .unwrap();

                Some(self.build_optional_elem_read(
                    "pop",
                    has_elem,
                    move |this| {
                        let new_len = this.builder.build_int_sub(len, one, "new_len").unwrap();
                        this.builder.build_store(len_field, new_len).unwrap();
                        new_len
                    },
                    vec_ptr,
                    elem_type,
                ))
            }
            "get" => {
                let idx: IntValue = index_arg(self)?;
                let len = self.load_int(usize_type, len_field, "len");
                let in_bounds = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, idx, len, "in_bounds")
                    .unwrap();

                Some(self.build_optional_elem_read("get", in_bounds, |_| idx, vec_ptr, elem_type))
            }
            "clear" => {
                if self.types.owns_heap(elem) {
                    let len = self.load_int(usize_type, len_field, "len");
                    let data = self.load_ptr(ptr_field, "data");
                    self.build_counted_loop(len, |this, at| {
                        let item = this.vec_elem_ptr(data, at, elem_type);
                        this.call_drop(item, elem);
                    });
                }
                self.builder
                    .build_store(len_field, usize_type.const_zero())
                    .unwrap();
                Some(unit)
            }
            _ => None,
        }
    }

    /// Make room for `needed` elements: half as many again plus eight, or
    /// `needed` itself if that is more.
    fn build_vec_reserve(
        &mut self,
        vec_ptr: PointerValue<'ctx>,
        needed: IntValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
    ) {
        let usize_type = self.usize_type();
        let cap_field = self.vec_field_ptr(vec_ptr, VEC_CAP, "cap_field");
        let cap = self.load_int(usize_type, cap_field, "cap");
        let short = self
            .builder
            .build_int_compare(IntPredicate::UGT, needed, cap, "short")
            .unwrap();

        self.if_then(short, |this| {
            let b = this.builder;
            let half = b
                .build_int_unsigned_div(cap, usize_type.const_int(2, false), "half")
                .unwrap();
            let grown = b.build_int_add(cap, half, "grown").unwrap();
            let grown = b
                .build_int_add(grown, usize_type.const_int(8, false), "grown")
                .unwrap();
            let enough = b
                .build_int_compare(IntPredicate::UGT, needed, grown, "enough")
                .unwrap();
            let new_cap = b
                .build_select(enough, needed, grown, "new_cap")
                .unwrap()
                .into_int_value();

            let ptr_field = this.vec_field_ptr(vec_ptr, VEC_PTR, "ptr_field");
            let data = this.load_ptr(ptr_field, "data");
            let old_size = this.bytes_for(elem_type, cap);
            let new_size = this.bytes_for(elem_type, new_cap);
            let resized = this.resize_buffer(data, old_size, new_size);
            this.builder.build_store(ptr_field, resized).unwrap();
            this.builder.build_store(cap_field, new_cap).unwrap();
        });
    }

    /// Branch on `guard`; read element `index_of(..)` as `Some` on the taken
    /// side, yield `None` on the other, and merge through a phi.
    fn build_optional_elem_read(
        &mut self,
        label: &str,
        guard: IntValue<'ctx>,
        index_of: impl FnOnce(&mut Self) -> IntValue<'ctx>,
        vec_ptr: PointerValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let Some(current_fn) = self.current_fn else {
            return self.dummy_val();
        };

        let some_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_some"));
        let none_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_none"));
        let merge_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_merge"));

        self.builder
            .build_conditional_branch(guard, some_bb, none_bb)
            .unwrap();

        self.builder.position_at_end(none_bb);
        let none_val = self.build_option_none(elem_type);
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let none_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(some_bb);
        let index = index_of(self);
        let data_ptr = self.load_ptr(
            self.vec_field_ptr(vec_ptr, VEC_PTR, "ptr_field"),
            "data_ptr",
        );
        let elem_ptr = self.vec_elem_ptr(data_ptr, index, elem_type);
        let elem_val = self.load(elem_type, elem_ptr, "elem_val");
        let some_val = self.build_option_some(elem_val);
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let some_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(none_val.get_type(), &format!("{label}_result"))
            .unwrap();
        phi.add_incoming(&[(&none_val, none_end), (&some_val, some_end)]);
        phi.as_basic_value()
    }

    /// Declare the `stdout`/`stderr` globals and the ctor that opens them.
    pub(super) fn init_builtin_streams(&mut self) {
        let Some(BasicTypeEnum::StructType(stream_type)) =
            self.llvm_type_of(&Type::Struct("OutStream".into()))
        else {
            return;
        };
        let field = |name: &str| {
            let fields = self.types.struct_fields("OutStream");
            fields
                .iter()
                .position(|(field, _)| field == name)
                .map(|at| at as u32)
        };
        let (Some(fd_index), Some(index_index)) = (field("fd"), field("index")) else {
            return;
        };

        let init_fn = self.module.add_function(
            "__init_builtin_streams",
            self.context.void_type().fn_type(&[], false),
            None,
        );
        let entry = self.context.append_basic_block(init_fn, "entry");
        self.builder.position_at_end(entry);

        let mut stream_ptrs = Vec::with_capacity(2);
        for (name, fd) in [("__stdout_stream", 1), ("__stderr_stream", 2)] {
            let global = self
                .module
                .add_global(stream_type, Some(AddressSpace::default()), name);
            global.set_initializer(&stream_type.const_zero());
            let ptr = global.as_pointer_value();

            let fd_ptr = self
                .builder
                .build_struct_gep(stream_type, ptr, fd_index, "fd_ptr")
                .unwrap();
            self.builder
                .build_store(fd_ptr, self.context.i32_type().const_int(fd, false))
                .unwrap();

            let index_ptr = self
                .builder
                .build_struct_gep(stream_type, ptr, index_index, "index_ptr")
                .unwrap();
            self.builder
                .build_store(index_ptr, self.usize_type().const_zero())
                .unwrap();

            stream_ptrs.push(ptr);
        }
        self.builder.build_return(None).unwrap();

        self.stdout_stream = Some(stream_ptrs[0]);
        self.stderr_stream = Some(stream_ptrs[1]);
        self.register_global_array("llvm.global_ctors", init_fn);
    }

    /// `__zeru_flush`, which writes out what both builtin streams hold: run
    /// as the program ends, by `exit`, and before a panic aborts.
    pub(super) fn create_flush(&mut self) {
        let (Some(stdout_ptr), Some(stderr_ptr), Some(flush_fn)) = (
            self.stdout_stream,
            self.stderr_stream,
            self.module.get_function("OutStream::flush"),
        ) else {
            return;
        };

        let dtor_fn = self.module.add_function(
            FLUSH_FN,
            self.context.void_type().fn_type(&[], false),
            Some(Linkage::Internal),
        );
        let entry = self.context.append_basic_block(dtor_fn, "entry");
        self.builder.position_at_end(entry);

        for stream in [stdout_ptr, stderr_ptr] {
            self.builder
                .build_call(flush_fn, &[stream.into()], "")
                .unwrap();
        }
        self.builder.build_return(None).unwrap();

        self.register_global_array("llvm.global_dtors", dtor_fn);
    }

    /// `print("x is {}", x)`: the format's text around each `{}` is written
    /// as it is, each value by the `OutStream` method for its kind.
    pub(super) fn compile_builtin_print(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> BasicValueEnum<'ctx> {
        let unit = self.dummy_val();
        let stream = if name.starts_with('e') {
            self.stderr_stream
        } else {
            self.stdout_stream
        };
        let (Some(stream), Some((format, values))) = (stream, arguments.split_first()) else {
            return unit;
        };
        let ExpressionKind::StringLit(text) = &format.kind else {
            return unit;
        };

        let pieces = format_pieces(text).unwrap_or_default();
        for (at, piece) in pieces.iter().enumerate() {
            if !piece.is_empty() {
                let text = self.const_bytes(piece);
                let len = self.usize_type().const_int(piece.len() as u64, false);
                self.call_stream("write_bytes", stream, &[text.into(), len.into()]);
            }
            if let Some(value) = values.get(at) {
                self.print_value(stream, value);
            }
        }

        if name.ends_with("ln") {
            let newline = self.const_bytes(b"\n");
            let one = self.usize_type().const_int(1, false);
            self.call_stream("write_bytes", stream, &[newline.into(), one.into()]);
            self.call_stream("flush", stream, &[]);
        }
        unit
    }

    /// Write one value: a `str` as it is, a bool as `true` or `false`, a
    /// number in decimal.
    fn print_value(&mut self, stream: PointerValue<'ctx>, value: &Expression) {
        let compiled = self.compile_expression(value, None);
        let i64_type = self.context.i64_type();
        match (&value.ty, compiled) {
            (Some(Type::Bool), BasicValueEnum::IntValue(flag)) => {
                let (yes, no) = (self.const_bytes(b"true"), self.const_bytes(b"false"));
                let text = self
                    .builder
                    .build_select(flag, yes, no, "bool_text")
                    .unwrap();
                let lens = (i64_type.const_int(4, false), i64_type.const_int(5, false));
                let len = self
                    .builder
                    .build_select(flag, lens.0, lens.1, "bool_len")
                    .unwrap();
                self.call_stream("write_bytes", stream, &[text.into(), len.into()]);
            }
            (Some(Type::Integer { signed, .. }), BasicValueEnum::IntValue(number)) => {
                let signed = *signed == Signedness::Signed;
                let wide = self
                    .builder
                    .build_int_cast_sign_flag(number, i64_type, signed, "wide")
                    .unwrap();
                let method = if signed { "write_int" } else { "write_uint" };
                self.call_stream(method, stream, &[wide.into()]);
            }
            (Some(Type::Float(_)), BasicValueEnum::FloatValue(number)) => {
                let wide = self
                    .builder
                    .build_float_ext(number, self.context.f64_type(), "wide")
                    .unwrap();
                self.call_stream("write_float", stream, &[wide.into()]);
            }
            (_, BasicValueEnum::StructValue(text)) => {
                let ptr = self.extract(text, SLICE_PTR, "str_ptr");
                let len = self.extract(text, SLICE_LEN, "str_len");
                self.call_stream("write_bytes", stream, &[ptr.into(), len.into()]);
            }
            _ => {}
        }
    }

    /// Call `OutStream::<method>` on `stream`, if the prelude defines it.
    fn call_stream(
        &self,
        method: &str,
        stream: PointerValue<'ctx>,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) {
        let Some(function) = self.module.get_function(&format!("OutStream::{method}")) else {
            return;
        };
        let mut all = vec![stream.into()];
        all.extend_from_slice(args);
        self.builder.build_call(function, &all, "").unwrap();
    }

    /// `Ok(value)` or `Err(code)`, laid out as the `T!` the analyser typed the
    /// call as.
    pub(super) fn compile_result_constructor(
        &mut self,
        arguments: &[Expression],
        call: &Expression,
        ok: bool,
    ) -> BasicValueEnum<'ctx> {
        let ([argument], Some(BasicTypeEnum::StructType(result_type))) = (
            arguments,
            call.ty.as_ref().and_then(|ty| self.llvm_type_of(ty)),
        ) else {
            self.error("'Err()' requires a known Result type context", call.span);
            return self.dummy_val();
        };

        let ok_type = result_type.get_field_type_at_index(RESULT_VALUE).unwrap();
        let code_type = self.context.i32_type();
        let (value, code) = if ok {
            let value = self.compile_expression(argument, Some(ok_type));
            (value, code_type.const_zero())
        } else {
            let code = self.compile_expression(argument, Some(code_type.into()));
            (self.zero_value_for(ok_type), code.into_int_value())
        };
        let tag = self.context.bool_type().const_int(u64::from(ok), false);
        self.build_struct(result_type, &[tag.into(), value, code.into()], "result")
            .into()
    }

    /// `T?` queries, mirroring the ones on `T!`.
    pub(super) fn compile_option_method(
        &mut self,
        method_name: &str,
        option: StructValue<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let tag = self.extract(option, OPTION_TAG, "opt_tag").into_int_value();

        match method_name {
            "is_some" => Some(tag.into()),
            "is_none" => Some(self.builder.build_not(tag, "opt_is_none").unwrap().into()),
            "unwrap" => {
                self.emit_trap_if(
                    self.builder.build_not(tag, "opt_empty").unwrap(),
                    "unwrap_none",
                );
                Some(self.extract(option, OPTION_VALUE, "opt_val"))
            }
            _ => None,
        }
    }

    pub(super) fn compile_result_method(
        &mut self,
        method_name: &str,
        result_val: StructValue<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let tag = || {
            self.extract(result_val, RESULT_TAG, "res_tag")
                .into_int_value()
        };

        match method_name {
            "is_ok" => Some(tag().into()),
            "is_err" => Some(self.builder.build_not(tag(), "res_is_err").unwrap().into()),
            "unwrap" => {
                let is_err = self.builder.build_not(tag(), "res_is_err").unwrap();
                self.emit_trap_if(is_err, "unwrap");
                Some(self.extract(result_val, RESULT_VALUE, "unwrap_val"))
            }
            "unwrap_err" => {
                self.emit_trap_if(tag(), "unwrap_err");
                Some(self.extract(result_val, RESULT_ERR, "unwrap_err_val"))
            }
            _ => None,
        }
    }
}
