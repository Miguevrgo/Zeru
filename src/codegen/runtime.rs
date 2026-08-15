//! LLVM/libc glue: allocator declarations, panics, inline asm, builtin print
//! streams, and the `Vec`/`T!` runtime.

use inkwell::{
    AddressSpace, IntPredicate,
    intrinsics::Intrinsic,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, PointerType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue,
        ValueKind,
    },
};

use crate::{
    ast::{AsmOperand, Expression},
    codegen::{
        compiler::Compiler,
        layout::{
            OPTION_TAG, OPTION_VALUE, RESULT_ERR, RESULT_TAG, RESULT_VALUE, SLICE_PTR, VEC_CAP,
            VEC_LEN, VEC_PTR,
        },
    },
    errors::{Span, ZeruError},
    token::Token,
};

const ALLOC_FN: &str = "__zeru_alloc";
const REALLOC_FN: &str = "__zeru_realloc";
const MEMCPY_FN: &str = "__zeru_memcpy";
const GEN_ALLOC_FN: &str = "mem::gen_alloc";

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub(super) fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(ZeruError::semantic(message, span, 0));
    }

    /// Fallback value returned after an error is recorded, so lowering can continue.
    pub(super) fn dummy_val(&self) -> BasicValueEnum<'ctx> {
        self.context.i32_type().const_int(0, false).into()
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

    fn load_int(&self, ty: IntType<'ctx>, ptr: PointerValue<'ctx>, name: &str) -> IntValue<'ctx> {
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

    fn vec_field_ptr(
        &self,
        vec_ptr: PointerValue<'ctx>,
        field: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_struct_gep(self.vec_type(), vec_ptr, field, name)
            .unwrap()
    }

    fn vec_elem_ptr(
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
    fn bytes_for(&self, elem_type: BasicTypeEnum<'ctx>, count: IntValue<'ctx>) -> IntValue<'ctx> {
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

    fn panic_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.panic_fn {
            return f;
        }
        let f = self.extern_fn("abort", self.context.void_type().fn_type(&[], false));
        self.panic_fn = Some(f);
        f
    }

    /// Abort the process, leaving the builder in a fresh unreachable-free block.
    fn build_panic(&mut self, from: inkwell::basic_block::BasicBlock<'ctx>) {
        self.builder.position_at_end(from);
        let abort_fn = self.panic_fn();
        self.builder.build_call(abort_fn, &[], "").unwrap();
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

        self.build_panic(panic_bb);
        self.builder.position_at_end(ok_bb);
    }

    pub(super) fn emit_null_check(&mut self, ptr: PointerValue<'ctx>, _error_msg: &str) {
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
            .build_int_compare(
                IntPredicate::UGE,
                index,
                usize_type.const_int(len, false),
                "out_of_range",
            )
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

    pub(super) fn create_entry_block_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).unwrap()
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
        let vec_type = self.vec_type();
        let zero = usize_type.const_zero();

        match method_name {
            "new" => self
                .build_struct(
                    vec_type,
                    &[
                        self.ptr_type().const_null().into(),
                        zero.into(),
                        zero.into(),
                    ],
                    "vec_new",
                )
                .into(),
            "with_capacity" => {
                let cap = match arguments.first() {
                    Some(arg) => self
                        .compile_expression(arg, Some(usize_type.into()))
                        .into_int_value(),
                    None => zero,
                };

                let alloc_size = self.bytes_for(elem_type, cap);
                let alloc_fn = self.extern_fn(
                    GEN_ALLOC_FN,
                    self.ptr_type().fn_type(&[usize_type.into()], false),
                );
                let data = self.call_ptr(alloc_fn, &[alloc_size.into()], "vec_alloc");

                self.build_struct(
                    vec_type,
                    &[data.into(), zero.into(), cap.into()],
                    "vec_with_cap",
                )
                .into()
            }
            _ => {
                self.error(
                    format!("Unknown Vec static method '{method_name}'"),
                    call_span,
                );
                self.dummy_val()
            }
        }
    }

    pub(super) fn compile_vec_method(
        &mut self,
        method_name: &str,
        vec_struct: StructValue<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match method_name {
            "len" => Some(self.extract(vec_struct, VEC_LEN, "vec_len")),
            "capacity" => Some(self.extract(vec_struct, VEC_CAP, "vec_cap")),
            "is_empty" => {
                let len = self
                    .extract(vec_struct, VEC_LEN, "vec_len")
                    .into_int_value();
                let is_empty = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        len,
                        len.get_type().const_zero(),
                        "is_empty",
                    )
                    .unwrap();
                Some(is_empty.into())
            }
            _ => None,
        }
    }

    pub(super) fn compile_vec_method_mut(
        &mut self,
        method_name: &str,
        vec_ptr: PointerValue<'ctx>,
        arguments: &[Expression],
        elem_type: BasicTypeEnum<'ctx>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let usize_type = self.usize_type();
        let unit = self.dummy_val();

        match method_name {
            "push" => {
                self.build_vec_push(vec_ptr, arguments.first()?, elem_type);
                Some(unit)
            }
            "pop" => {
                let len_field = self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field");
                let len = self.load_int(usize_type, len_field, "len");
                let has_elem = self
                    .builder
                    .build_int_compare(IntPredicate::NE, len, usize_type.const_zero(), "has_elem")
                    .unwrap();

                Some(self.build_optional_elem_read(
                    "pop",
                    has_elem,
                    move |this| {
                        let new_len = this
                            .builder
                            .build_int_sub(len, usize_type.const_int(1, false), "new_len")
                            .unwrap();
                        this.builder.build_store(len_field, new_len).unwrap();
                        new_len
                    },
                    vec_ptr,
                    elem_type,
                ))
            }
            "get" => {
                let idx = self
                    .compile_expression(arguments.first()?, Some(usize_type.into()))
                    .into_int_value();
                let len_field = self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field");
                let len = self.load_int(usize_type, len_field, "len");
                let in_bounds = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, idx, len, "in_bounds")
                    .unwrap();

                Some(self.build_optional_elem_read("get", in_bounds, |_| idx, vec_ptr, elem_type))
            }
            "clear" => {
                let len_field = self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field");
                self.builder
                    .build_store(len_field, usize_type.const_zero())
                    .unwrap();
                Some(unit)
            }
            "copy" => Some(self.build_vec_copy(vec_ptr, elem_type)),
            _ => None,
        }
    }

    /// Grow the buffer if full, then append one element and bump the length.
    fn build_vec_push(
        &mut self,
        vec_ptr: PointerValue<'ctx>,
        item: &Expression,
        elem_type: BasicTypeEnum<'ctx>,
    ) {
        let usize_type = self.usize_type();
        let item_val = self.compile_expression(item, Some(elem_type));
        let Some(current_fn) = self.current_fn else {
            return;
        };

        let ptr_field = self.vec_field_ptr(vec_ptr, VEC_PTR, "ptr_field");
        let len_field = self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field");
        let cap_field = self.vec_field_ptr(vec_ptr, VEC_CAP, "cap_field");

        let data_ptr = self.load_ptr(ptr_field, "data_ptr");
        let len = self.load_int(usize_type, len_field, "len");
        let cap = self.load_int(usize_type, cap_field, "cap");

        let needs_grow = self
            .builder
            .build_int_compare(IntPredicate::UGE, len, cap, "needs_grow")
            .unwrap();

        let grow_bb = self.context.append_basic_block(current_fn, "vec_grow");
        let store_bb = self.context.append_basic_block(current_fn, "vec_store");
        self.builder
            .build_conditional_branch(needs_grow, grow_bb, store_bb)
            .unwrap();

        self.builder.position_at_end(grow_bb);
        let min_cap = usize_type.const_int(8, false);
        let half = self
            .builder
            .build_int_unsigned_div(cap, usize_type.const_int(2, false), "half_cap")
            .unwrap();
        let grown = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_add(cap, half, "cap_plus_half")
                    .unwrap(),
                min_cap,
                "grown_cap",
            )
            .unwrap();
        let cap_is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, cap, usize_type.const_zero(), "cap_zero")
            .unwrap();
        let new_cap = self
            .builder
            .build_select(cap_is_zero, min_cap, grown, "new_cap")
            .unwrap()
            .into_int_value();

        let old_size = self.bytes_for(elem_type, cap);
        let new_size = self.bytes_for(elem_type, new_cap);

        let ptr_type = self.ptr_type();
        let realloc_fn = self.extern_fn(
            REALLOC_FN,
            ptr_type.fn_type(
                &[ptr_type.into(), usize_type.into(), usize_type.into()],
                false,
            ),
        );
        let new_ptr = self.call_ptr(
            realloc_fn,
            &[data_ptr.into(), old_size.into(), new_size.into()],
            "new_ptr",
        );

        self.builder.build_store(ptr_field, new_ptr).unwrap();
        self.builder.build_store(cap_field, new_cap).unwrap();
        self.builder.build_unconditional_branch(store_bb).unwrap();

        self.builder.position_at_end(store_bb);
        let final_ptr = self.load_ptr(ptr_field, "final_ptr");
        let final_len = self.load_int(usize_type, len_field, "final_len");
        let elem_ptr = self.vec_elem_ptr(final_ptr, final_len, elem_type);
        self.builder.build_store(elem_ptr, item_val).unwrap();

        let new_len = self
            .builder
            .build_int_add(final_len, usize_type.const_int(1, false), "new_len")
            .unwrap();
        self.builder.build_store(len_field, new_len).unwrap();
    }

    /// Allocate a compacted duplicate of the buffer (capacity trimmed to length).
    fn build_vec_copy(
        &mut self,
        vec_ptr: PointerValue<'ctx>,
        elem_type: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let usize_type = self.usize_type();
        let ptr_type = self.ptr_type();

        let src_ptr = self.load_ptr(self.vec_field_ptr(vec_ptr, VEC_PTR, "ptr_field"), "src_ptr");
        let len = self.load_int(
            usize_type,
            self.vec_field_ptr(vec_ptr, VEC_LEN, "len_field"),
            "len",
        );
        let alloc_size = self.bytes_for(elem_type, len);

        let alloc_fn = self.extern_fn(ALLOC_FN, ptr_type.fn_type(&[usize_type.into()], false));
        let new_ptr = self.call_ptr(alloc_fn, &[alloc_size.into()], "new_ptr");

        let memcpy_fn = self.extern_fn(
            MEMCPY_FN,
            self.context.void_type().fn_type(
                &[ptr_type.into(), ptr_type.into(), usize_type.into()],
                false,
            ),
        );
        self.builder
            .build_call(
                memcpy_fn,
                &[new_ptr.into(), src_ptr.into(), alloc_size.into()],
                "",
            )
            .unwrap();

        self.build_struct(
            self.vec_type(),
            &[new_ptr.into(), len.into(), len.into()],
            "vec_copy",
        )
        .into()
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
        let Some((stream_type, fields)) = self.struct_defs.get("OutStream") else {
            return;
        };
        let stream_type = *stream_type;
        let (Some(&fd_index), Some(&index_index)) = (fields.get("fd"), fields.get("index")) else {
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

    /// Emit the dtor that flushes both builtin streams at exit.
    pub(super) fn create_builtin_cleanup(&mut self) {
        let (Some(stdout_ptr), Some(stderr_ptr), Some(flush_fn)) = (
            self.stdout_stream,
            self.stderr_stream,
            self.module.get_function("OutStream::flush"),
        ) else {
            return;
        };

        let dtor_fn = self.module.add_function(
            "__cleanup_builtin_streams",
            self.context.void_type().fn_type(&[], false),
            None,
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

    pub(super) fn compile_builtin_print(
        &mut self,
        name: &str,
        arguments: &[Expression],
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        let [argument] = arguments else {
            self.error(format!("'{name}()' expects exactly 1 argument"), call_span);
            return self.dummy_val();
        };

        let to_stderr = name.starts_with('e');
        let newline = name.ends_with("ln");
        let (Some(stream), Some(write_str_fn)) = (
            if to_stderr {
                self.stderr_stream
            } else {
                self.stdout_stream
            },
            self.module.get_function("OutStream::write_str"),
        ) else {
            return self.dummy_val();
        };

        let str_ptr: BasicMetadataValueEnum = match self.compile_expression(argument, None) {
            BasicValueEnum::StructValue(slice) => self.extract(slice, SLICE_PTR, "str_ptr").into(),
            BasicValueEnum::PointerValue(ptr) => ptr.into(),
            _ => {
                self.error(format!("'{name}()' expects a string argument"), call_span);
                return self.dummy_val();
            }
        };

        self.builder
            .build_call(write_str_fn, &[stream.into(), str_ptr], "")
            .unwrap();

        if newline {
            let nl = self
                .builder
                .build_global_string_ptr("\n", "newline")
                .unwrap();
            self.builder
                .build_call(
                    write_str_fn,
                    &[stream.into(), nl.as_pointer_value().into()],
                    "",
                )
                .unwrap();
            if let Some(flush_fn) = self.module.get_function("OutStream::flush") {
                self.builder
                    .build_call(flush_fn, &[stream.into()], "")
                    .unwrap();
            }
        }

        self.dummy_val()
    }

    pub(super) fn compile_ok_constructor(
        &mut self,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        let [argument] = arguments else {
            self.error("'Ok()' expects exactly 1 argument", call_span);
            return self.dummy_val();
        };

        let result_type = match expected_type {
            Some(BasicTypeEnum::StructType(st)) if self.is_result_layout(st) => st,
            _ => {
                let inner_type = self.compile_expression(argument, None).get_type();
                self.context.struct_type(
                    &[
                        self.context.bool_type().into(),
                        inner_type,
                        self.context.i32_type().into(),
                    ],
                    false,
                )
            }
        };

        let inner_type = result_type.get_field_type_at_index(RESULT_VALUE).unwrap();
        let inner_val = self.compile_expression(argument, Some(inner_type));
        let is_ok = self.context.bool_type().const_int(1, false);
        let no_error = self.context.i32_type().const_zero();

        self.build_struct(
            result_type,
            &[is_ok.into(), inner_val, no_error.into()],
            "res_ok",
        )
        .into()
    }

    pub(super) fn compile_err_constructor(
        &mut self,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        let [argument] = arguments else {
            self.error("'Err()' expects exactly 1 argument", call_span);
            return self.dummy_val();
        };

        let Some(BasicTypeEnum::StructType(result_type)) = expected_type
            .filter(|ty| matches!(ty, BasicTypeEnum::StructType(st) if self.is_result_layout(*st)))
        else {
            self.error("'Err()' requires a known Result type context", call_span);
            return self.dummy_val();
        };

        let error_code = self
            .compile_expression(argument, Some(self.context.i32_type().into()))
            .into_int_value();
        let is_ok = self.context.bool_type().const_zero();
        let inner_type = result_type.get_field_type_at_index(RESULT_VALUE).unwrap();
        let unused_ok = self.zero_value_for(inner_type);

        self.build_struct(
            result_type,
            &[is_ok.into(), unused_ok, error_code.into()],
            "res_err",
        )
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
                let tag = tag();
                self.abort_unless_tag_is(tag, true, "unwrap");
                Some(self.extract(result_val, RESULT_VALUE, "unwrap_val"))
            }
            "unwrap_err" => {
                let tag = tag();
                self.abort_unless_tag_is(tag, false, "unwrap_err");
                Some(self.extract(result_val, RESULT_ERR, "unwrap_err_val"))
            }
            _ => None,
        }
    }

    /// Abort when the `T!` tag is not the wanted variant, leaving the builder
    /// in the surviving block.
    fn abort_unless_tag_is(&mut self, tag: IntValue<'ctx>, want_ok: bool, label: &str) {
        let Some(current_fn) = self.current_fn else {
            return;
        };

        let keep_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_ok"));
        let panic_bb = self
            .context
            .append_basic_block(current_fn, &format!("{label}_panic"));

        let (on_true, on_false) = if want_ok {
            (keep_bb, panic_bb)
        } else {
            (panic_bb, keep_bb)
        };
        self.builder
            .build_conditional_branch(tag, on_true, on_false)
            .unwrap();

        self.build_panic(panic_bb);
        self.builder.position_at_end(keep_bb);
    }
}
