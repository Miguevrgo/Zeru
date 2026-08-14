//! Low-level LLVM/libc glue and built-in runtime helpers.
//!
//! This is the "obscure" file you should rarely need to touch. It contains:
//!
//! * Allocator/memcpy/realloc external function declarations.
//! * Panic and null-check intrinsics.
//! * Inline assembly lowering.
//! * Built-in print stream initialization and cleanup constructors.
//! * `Vec` and `Result` runtime constructor/method implementations.
//! * Small generic helpers (`error`, `dummy_val`, `create_entry_block_alloca`).

use inkwell::{
    IntPredicate,
    types::{BasicTypeEnum, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{AsmOperand, Expression},
    codegen::compiler::Compiler,
    errors::{Span, ZeruError},
};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Record a codegen error. Compilation continues so further errors can be found.
    pub(super) fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(ZeruError::semantic(message, span, 0));
    }

    /// Produce a safe dummy `i32 0` value used as a fallback after an error is recorded.
    pub(super) fn dummy_val(&self) -> BasicValueEnum<'ctx> {
        self.context.i32_type().const_int(0, false).into()
    }

    /// The `{ *mut u8, usize, usize }` LLVM struct shape used for built-in `Vec`.
    fn vec_struct_type(&self) -> StructType<'ctx> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        self.context.struct_type(
            &[ptr_type.into(), usize_type.into(), usize_type.into()],
            false,
        )
    }

    /// The `{ bool, T }` LLVM struct shape used for `Option<T>` returned from
    /// runtime helpers (e.g. `Vec::pop`).
    fn option_struct_type(&self, elem_ty: BasicTypeEnum<'ctx>) -> StructType<'ctx> {
        self.context
            .struct_type(&[self.context.bool_type().into(), elem_ty], false)
    }

    /// Produce the canonical "zero" value for any LLVM basic type.
    ///
    /// Used to populate the payload slot of `None` and `Err(_)` values
    /// where the inner value is logically absent but LLVM still requires
    /// a concrete bit pattern.
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

    /// Register `func` in the given LLVM appending-array global
    /// (`llvm.global_ctors` or `llvm.global_dtors`) at default priority.
    fn register_global_array(&mut self, array_name: &str, func: FunctionValue<'ctx>) {
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let entry_struct_type = self
            .context
            .struct_type(&[i32_type.into(), ptr_type.into(), ptr_type.into()], false);

        let entry = entry_struct_type.const_named_struct(&[
            i32_type.const_int(65535, false).into(),
            func.as_global_value().as_pointer_value().into(),
            ptr_type.const_null().into(),
        ]);

        let entries_array: inkwell::values::ArrayValue = entry_struct_type.const_array(&[entry]);
        let global = self.module.add_global(
            entries_array.get_type(),
            Some(inkwell::AddressSpace::default()),
            array_name,
        );
        global.set_linkage(inkwell::module::Linkage::Appending);
        global.set_initializer(&entries_array);
    }

    fn get_or_create_panic_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.panic_fn {
            return f;
        }

        let void_type = self.context.void_type();
        let abort_fn_type = void_type.fn_type(&[], false);
        let abort_fn = self.module.add_function(
            "abort",
            abort_fn_type,
            Some(inkwell::module::Linkage::External),
        );

        self.panic_fn = Some(abort_fn);
        abort_fn
    }

    pub(super) fn emit_null_check(&mut self, ptr: PointerValue<'ctx>, error_msg: &str) {
        if !self.safety_mode.emit_safety_checks() {
            return;
        }

        let current_fn = self
            .current_fn
            .expect("emit_null_check called outside function");

        let null_ptr = self
            .context
            .ptr_type(inkwell::AddressSpace::default())
            .const_null();
        let is_null = self
            .builder
            .build_int_compare(IntPredicate::EQ, ptr, null_ptr, "is_null")
            .unwrap();

        let panic_block = self.context.append_basic_block(current_fn, "null_panic");
        let continue_block = self.context.append_basic_block(current_fn, "null_ok");

        self.builder
            .build_conditional_branch(is_null, panic_block, continue_block)
            .unwrap();

        self.builder.position_at_end(panic_block);

        let _msg = self
            .builder
            .build_global_string_ptr(error_msg, "panic_msg")
            .unwrap();

        let abort_fn = self.get_or_create_panic_fn();
        self.builder.build_call(abort_fn, &[], "").unwrap();
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(continue_block);
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

    pub(super) fn compile_inline_asm(
        &mut self,
        template: &str,
        outputs: &[AsmOperand],
        inputs: &[AsmOperand],
        clobbers: &[String],
        is_volatile: bool,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let mut constraints = Vec::new();

        for out in outputs {
            constraints.push(out.constraint.clone());
        }

        for inp in inputs {
            constraints.push(inp.constraint.clone());
        }

        for clob in clobbers {
            constraints.push(format!("~{{{}}}", clob));
        }

        let constraint_str = constraints.join(",");
        let mut input_values: Vec<BasicValueEnum<'ctx>> = Vec::new();
        for inp in inputs {
            let val = self.compile_expression(&inp.expr, None);
            input_values.push(val);
        }

        let output_type = if outputs.is_empty() {
            self.context.i64_type().into()
        } else if outputs.len() == 1 {
            expected_type.unwrap_or_else(|| self.context.i64_type().into())
        } else {
            let types: Vec<BasicTypeEnum<'ctx>> = outputs
                .iter()
                .map(|_| self.context.i64_type().into())
                .collect();
            self.context.struct_type(&types, false).into()
        };

        let input_types: Vec<BasicTypeEnum<'ctx>> =
            input_values.iter().map(|v| v.get_type()).collect();

        let asm_fn_type = match output_type {
            BasicTypeEnum::IntType(t) => t.fn_type(
                &input_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            ),
            BasicTypeEnum::FloatType(t) => t.fn_type(
                &input_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            ),
            BasicTypeEnum::StructType(t) => t.fn_type(
                &input_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            ),
            _ => self.context.i64_type().fn_type(
                &input_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                false,
            ),
        };

        let asm_val = self.module.get_context().create_inline_asm(
            asm_fn_type,
            template.to_string(),
            constraint_str,
            is_volatile,
            false,
            None,
            false,
        );

        let args: Vec<BasicMetadataValueEnum<'ctx>> =
            input_values.iter().map(|v| (*v).into()).collect();

        let call_site = self
            .builder
            .build_indirect_call(asm_fn_type, asm_val, &args, "asm_result")
            .unwrap();

        let result = match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(value) => value,
            inkwell::values::ValueKind::Instruction(_) => {
                self.context.i64_type().const_int(0, false).into()
            }
        };

        if !outputs.is_empty() {
            for (i, out) in outputs.iter().enumerate() {
                if let Some((ptr, _ty)) = self.compile_lvalue(&out.expr) {
                    let val_to_store = if outputs.len() == 1 {
                        result
                    } else {
                        self.builder
                            .build_extract_value(result.into_struct_value(), i as u32, "asm_out")
                            .unwrap()
                    };
                    self.builder.build_store(ptr, val_to_store).unwrap();
                }
            }
        }

        result
    }

    pub(super) fn compile_vec_static_method(
        &mut self,
        method_name: &str,
        arguments: &[Expression],
        _expected_type: Option<BasicTypeEnum<'ctx>>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let vec_type = self.vec_struct_type();

        match method_name {
            "new" => {
                let null_ptr = ptr_type.const_null();
                let zero = usize_type.const_int(0, false);

                let mut vec_val = vec_type.get_undef();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, null_ptr, 0, "vec_ptr")
                    .unwrap()
                    .into_struct_value();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, zero, 1, "vec_len")
                    .unwrap()
                    .into_struct_value();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, zero, 2, "vec_cap")
                    .unwrap()
                    .into_struct_value();

                vec_val.into()
            }
            "with_capacity" => {
                // Vec::with_capacity(cap) - allocate memory upfront
                let cap_val = if !arguments.is_empty() {
                    self.compile_expression(&arguments[0], Some(usize_type.into()))
                        .into_int_value()
                } else {
                    usize_type.const_int(0, false)
                };

                let elem_size: u64 = 8;

                let elem_size_val = usize_type.const_int(elem_size, false);
                let alloc_size = self
                    .builder
                    .build_int_mul(cap_val, elem_size_val, "alloc_size")
                    .unwrap();

                let alloc_fn = self.module.get_function("mem::gen_alloc").or_else(|| {
                    let fn_type = ptr_type.fn_type(&[usize_type.into()], false);
                    Some(self.module.add_function(
                        "mem::gen_alloc",
                        fn_type,
                        Some(inkwell::module::Linkage::External),
                    ))
                });

                let ptr_val = if let Some(alloc_fn) = alloc_fn {
                    match self
                        .builder
                        .build_call(alloc_fn, &[alloc_size.into()], "vec_alloc")
                        .unwrap()
                        .try_as_basic_value()
                    {
                        inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
                        _ => ptr_type.const_null(),
                    }
                } else {
                    ptr_type.const_null()
                };

                let zero = usize_type.const_int(0, false);
                let mut vec_val = vec_type.get_undef();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, ptr_val, 0, "vec_ptr")
                    .unwrap()
                    .into_struct_value();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, zero, 1, "vec_len")
                    .unwrap()
                    .into_struct_value();
                vec_val = self
                    .builder
                    .build_insert_value(vec_val, cap_val, 2, "vec_cap")
                    .unwrap()
                    .into_struct_value();

                vec_val.into()
            }
            _ => {
                self.error(
                    format!("Unknown Vec static method '{}'", method_name),
                    call_span,
                );
                self.dummy_val()
            }
        }
    }

    pub(super) fn compile_vec_method(
        &mut self,
        method_name: &str,
        vec_struct: inkwell::values::StructValue<'ctx>,
        _arguments: &[Expression],
        _object: &Expression,
    ) -> Option<BasicValueEnum<'ctx>> {
        let usize_type = self.context.i64_type();

        match method_name {
            "len" => {
                let len = self
                    .builder
                    .build_extract_value(vec_struct, 1, "vec_len")
                    .unwrap();
                Some(len)
            }
            "capacity" => {
                let cap = self
                    .builder
                    .build_extract_value(vec_struct, 2, "vec_cap")
                    .unwrap();
                Some(cap)
            }
            "is_empty" => {
                let len = self
                    .builder
                    .build_extract_value(vec_struct, 1, "vec_len")
                    .unwrap()
                    .into_int_value();
                let zero = usize_type.const_int(0, false);
                let is_empty = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, len, zero, "is_empty")
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
        elem_size: u64,
    ) -> Option<BasicValueEnum<'ctx>> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let vec_type = self.vec_struct_type();

        match method_name {
            "push" => {
                let item_val = self.compile_expression(&arguments[0], None);
                let current_fn = self.current_fn.unwrap();

                let ptr_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 0, "ptr_field")
                    .unwrap();
                let len_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 1, "len_field")
                    .unwrap();
                let cap_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 2, "cap_field")
                    .unwrap();

                let data_ptr = self
                    .builder
                    .build_load(ptr_type, ptr_field_ptr, "data_ptr")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_load(usize_type, len_field_ptr, "len")
                    .unwrap()
                    .into_int_value();
                let cap = self
                    .builder
                    .build_load(usize_type, cap_field_ptr, "cap")
                    .unwrap()
                    .into_int_value();

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

                let zero = usize_type.const_int(0, false);
                let eight = usize_type.const_int(8, false);
                let two = usize_type.const_int(2, false);
                let cap_is_zero = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, cap, zero, "cap_zero")
                    .unwrap();

                let half_cap = self
                    .builder
                    .build_int_unsigned_div(cap, two, "half_cap")
                    .unwrap();
                let grown_cap = self
                    .builder
                    .build_int_add(
                        self.builder
                            .build_int_add(cap, half_cap, "cap_plus_half")
                            .unwrap(),
                        eight,
                        "grown_cap",
                    )
                    .unwrap();
                let new_cap = self
                    .builder
                    .build_select(cap_is_zero, eight, grown_cap, "new_cap")
                    .unwrap()
                    .into_int_value();

                let elem_size_val = usize_type.const_int(elem_size, false);
                let new_size = self
                    .builder
                    .build_int_mul(new_cap, elem_size_val, "new_size")
                    .unwrap();

                let realloc_fn = self.get_or_create_realloc_fn();
                let old_size = self
                    .builder
                    .build_int_mul(cap, elem_size_val, "old_size")
                    .unwrap();
                let new_ptr = match self
                    .builder
                    .build_call(
                        realloc_fn,
                        &[data_ptr.into(), old_size.into(), new_size.into()],
                        "new_ptr",
                    )
                    .unwrap()
                    .try_as_basic_value()
                {
                    inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
                    _ => ptr_type.const_null(),
                };

                self.builder.build_store(ptr_field_ptr, new_ptr).unwrap();
                self.builder.build_store(cap_field_ptr, new_cap).unwrap();

                self.builder.build_unconditional_branch(store_bb).unwrap();

                self.builder.position_at_end(store_bb);

                let final_ptr = self
                    .builder
                    .build_load(ptr_type, ptr_field_ptr, "final_ptr")
                    .unwrap()
                    .into_pointer_value();
                let final_len = self
                    .builder
                    .build_load(usize_type, len_field_ptr, "final_len")
                    .unwrap()
                    .into_int_value();

                let offset = self
                    .builder
                    .build_int_mul(final_len, usize_type.const_int(elem_size, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), final_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                self.builder.build_store(elem_ptr, item_val).unwrap();

                let new_len = self
                    .builder
                    .build_int_add(final_len, usize_type.const_int(1, false), "new_len")
                    .unwrap();
                self.builder.build_store(len_field_ptr, new_len).unwrap();

                Some(self.context.i32_type().const_int(0, false).into())
            }
            "pop" => {
                let current_fn = self.current_fn.unwrap();

                let len_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 1, "len_field")
                    .unwrap();
                let ptr_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 0, "ptr_field")
                    .unwrap();

                let len = self
                    .builder
                    .build_load(usize_type, len_field_ptr, "len")
                    .unwrap()
                    .into_int_value();

                let zero = usize_type.const_int(0, false);
                let is_empty = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, len, zero, "is_empty")
                    .unwrap();

                let empty_bb = self.context.append_basic_block(current_fn, "pop_empty");
                let pop_bb = self.context.append_basic_block(current_fn, "pop_do");
                let merge_bb = self.context.append_basic_block(current_fn, "pop_merge");

                self.builder
                    .build_conditional_branch(is_empty, empty_bb, pop_bb)
                    .unwrap();

                self.builder.position_at_end(empty_bb);
                let elem_type = self.context.i64_type();
                let opt_type = self.option_struct_type(elem_type.into());
                let none_val = {
                    let mut v = opt_type.get_undef();
                    v = self
                        .builder
                        .build_insert_value(
                            v,
                            self.context.bool_type().const_int(0, false),
                            0,
                            "none_tag",
                        )
                        .unwrap()
                        .into_struct_value();
                    v = self
                        .builder
                        .build_insert_value(v, elem_type.const_int(0, false), 1, "none_val")
                        .unwrap()
                        .into_struct_value();
                    v
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let empty_end_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(pop_bb);
                let new_len = self
                    .builder
                    .build_int_sub(len, usize_type.const_int(1, false), "new_len")
                    .unwrap();
                self.builder.build_store(len_field_ptr, new_len).unwrap();

                let data_ptr = self
                    .builder
                    .build_load(ptr_type, ptr_field_ptr, "data_ptr")
                    .unwrap()
                    .into_pointer_value();
                let offset = self
                    .builder
                    .build_int_mul(new_len, usize_type.const_int(elem_size, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };
                let elem_val = self
                    .builder
                    .build_load(elem_type, elem_ptr, "elem_val")
                    .unwrap();

                let some_val = {
                    let mut v = opt_type.get_undef();
                    v = self
                        .builder
                        .build_insert_value(
                            v,
                            self.context.bool_type().const_int(1, false),
                            0,
                            "some_tag",
                        )
                        .unwrap()
                        .into_struct_value();
                    v = self
                        .builder
                        .build_insert_value(v, elem_val, 1, "some_val")
                        .unwrap()
                        .into_struct_value();
                    v
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let pop_end_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(opt_type, "pop_result").unwrap();
                phi.add_incoming(&[(&none_val, empty_end_bb), (&some_val, pop_end_bb)]);

                Some(phi.as_basic_value())
            }
            "get" => {
                let current_fn = self.current_fn.unwrap();
                let idx = self
                    .compile_expression(&arguments[0], Some(usize_type.into()))
                    .into_int_value();

                let len_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 1, "len_field")
                    .unwrap();
                let ptr_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 0, "ptr_field")
                    .unwrap();

                let len = self
                    .builder
                    .build_load(usize_type, len_field_ptr, "len")
                    .unwrap()
                    .into_int_value();

                let in_bounds = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, idx, len, "in_bounds")
                    .unwrap();

                let oob_bb = self.context.append_basic_block(current_fn, "get_oob");
                let valid_bb = self.context.append_basic_block(current_fn, "get_valid");
                let merge_bb = self.context.append_basic_block(current_fn, "get_merge");

                self.builder
                    .build_conditional_branch(in_bounds, valid_bb, oob_bb)
                    .unwrap();

                self.builder.position_at_end(oob_bb);
                let elem_type = self.context.i64_type();
                let opt_type = self.option_struct_type(elem_type.into());
                let none_val = {
                    let mut v = opt_type.get_undef();
                    v = self
                        .builder
                        .build_insert_value(
                            v,
                            self.context.bool_type().const_int(0, false),
                            0,
                            "none_tag",
                        )
                        .unwrap()
                        .into_struct_value();
                    v = self
                        .builder
                        .build_insert_value(v, elem_type.const_int(0, false), 1, "none_val")
                        .unwrap()
                        .into_struct_value();
                    v
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let oob_end_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(valid_bb);
                let data_ptr = self
                    .builder
                    .build_load(ptr_type, ptr_field_ptr, "data_ptr")
                    .unwrap()
                    .into_pointer_value();
                let offset = self
                    .builder
                    .build_int_mul(idx, usize_type.const_int(elem_size, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };
                let elem_val = self
                    .builder
                    .build_load(elem_type, elem_ptr, "elem_val")
                    .unwrap();

                let some_val = {
                    let mut v = opt_type.get_undef();
                    v = self
                        .builder
                        .build_insert_value(
                            v,
                            self.context.bool_type().const_int(1, false),
                            0,
                            "some_tag",
                        )
                        .unwrap()
                        .into_struct_value();
                    v = self
                        .builder
                        .build_insert_value(v, elem_val, 1, "some_val")
                        .unwrap()
                        .into_struct_value();
                    v
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let valid_end_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(opt_type, "get_result").unwrap();
                phi.add_incoming(&[(&none_val, oob_end_bb), (&some_val, valid_end_bb)]);

                Some(phi.as_basic_value())
            }
            "clear" => {
                let len_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 1, "len_field")
                    .unwrap();
                let zero = usize_type.const_int(0, false);
                self.builder.build_store(len_field_ptr, zero).unwrap();
                Some(self.context.i32_type().const_int(0, false).into())
            }
            "copy" => {
                let ptr_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 0, "ptr_field")
                    .unwrap();
                let len_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 1, "len_field")
                    .unwrap();
                let cap_field_ptr = self
                    .builder
                    .build_struct_gep(vec_type, vec_ptr, 2, "cap_field")
                    .unwrap();

                let src_ptr = self
                    .builder
                    .build_load(ptr_type, ptr_field_ptr, "src_ptr")
                    .unwrap()
                    .into_pointer_value();
                let len = self
                    .builder
                    .build_load(usize_type, len_field_ptr, "len")
                    .unwrap()
                    .into_int_value();
                let _cap = self
                    .builder
                    .build_load(usize_type, cap_field_ptr, "cap")
                    .unwrap()
                    .into_int_value();

                let elem_size_val = usize_type.const_int(elem_size, false);
                let alloc_size = self
                    .builder
                    .build_int_mul(len, elem_size_val, "alloc_size")
                    .unwrap();

                let alloc_fn = self.get_or_create_alloc_fn();
                let new_ptr = match self
                    .builder
                    .build_call(alloc_fn, &[alloc_size.into()], "new_ptr")
                    .unwrap()
                    .try_as_basic_value()
                {
                    inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
                    _ => ptr_type.const_null(),
                };

                let memcpy_fn = self.get_or_create_memcpy_fn();
                self.builder
                    .build_call(
                        memcpy_fn,
                        &[new_ptr.into(), src_ptr.into(), alloc_size.into()],
                        "",
                    )
                    .unwrap();

                let mut new_vec = vec_type.get_undef();
                new_vec = self
                    .builder
                    .build_insert_value(new_vec, new_ptr, 0, "copy_ptr")
                    .unwrap()
                    .into_struct_value();
                new_vec = self
                    .builder
                    .build_insert_value(new_vec, len, 1, "copy_len")
                    .unwrap()
                    .into_struct_value();
                new_vec = self
                    .builder
                    .build_insert_value(new_vec, len, 2, "copy_cap")
                    .unwrap()
                    .into_struct_value();

                Some(new_vec.into())
            }
            _ => None,
        }
    }

    fn get_or_create_alloc_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function("__zeru_alloc") {
            return f;
        }
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let fn_type = ptr_type.fn_type(&[usize_type.into()], false);
        self.module.add_function(
            "__zeru_alloc",
            fn_type,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn get_or_create_realloc_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function("__zeru_realloc") {
            return f;
        }
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let fn_type = ptr_type.fn_type(
            &[ptr_type.into(), usize_type.into(), usize_type.into()],
            false,
        );
        self.module.add_function(
            "__zeru_realloc",
            fn_type,
            Some(inkwell::module::Linkage::External),
        )
    }

    fn get_or_create_memcpy_fn(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function("__zeru_memcpy") {
            return f;
        }
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let fn_type = self.context.void_type().fn_type(
            &[ptr_type.into(), ptr_type.into(), usize_type.into()],
            false,
        );
        self.module.add_function(
            "__zeru_memcpy",
            fn_type,
            Some(inkwell::module::Linkage::External),
        )
    }

    pub(super) fn init_builtin_streams(&mut self) {
        let (outstream_type, field_indices) =
            if let Some((st, indices)) = self.struct_defs.get("OutStream") {
                (*st, indices.clone())
            } else {
                return;
            };

        let fd_index = *field_indices.get("fd").expect("OutStream missing fd field");
        let index_index = *field_indices
            .get("index")
            .expect("OutStream missing index field");

        let stdout_global = self.module.add_global(
            outstream_type,
            Some(inkwell::AddressSpace::default()),
            "__stdout_stream",
        );
        let stderr_global = self.module.add_global(
            outstream_type,
            Some(inkwell::AddressSpace::default()),
            "__stderr_stream",
        );

        stdout_global.set_initializer(&outstream_type.const_zero());
        stderr_global.set_initializer(&outstream_type.const_zero());

        let void_type = self.context.void_type();
        let init_fn_type = void_type.fn_type(&[], false);
        let init_fn = self
            .module
            .add_function("__init_builtin_streams", init_fn_type, None);

        let entry_block = self.context.append_basic_block(init_fn, "entry");
        self.builder.position_at_end(entry_block);

        let stdout_ptr = stdout_global.as_pointer_value();
        let fd_ptr = self
            .builder
            .build_struct_gep(outstream_type, stdout_ptr, fd_index, "stdout_fd_ptr")
            .expect("Failed to GEP stdout fd");
        self.builder
            .build_store(fd_ptr, self.context.i32_type().const_int(1, false))
            .unwrap();
        let index_ptr = self
            .builder
            .build_struct_gep(outstream_type, stdout_ptr, index_index, "stdout_index_ptr")
            .expect("Failed to GEP stdout index");
        self.builder
            .build_store(index_ptr, self.context.i64_type().const_zero())
            .unwrap();

        let stderr_ptr = stderr_global.as_pointer_value();
        let fd_ptr_err = self
            .builder
            .build_struct_gep(outstream_type, stderr_ptr, fd_index, "stderr_fd_ptr")
            .expect("Failed to GEP stderr fd");
        self.builder
            .build_store(fd_ptr_err, self.context.i32_type().const_int(2, false))
            .unwrap();
        let index_ptr_err = self
            .builder
            .build_struct_gep(outstream_type, stderr_ptr, index_index, "stderr_index_ptr")
            .expect("Failed to GEP stderr index");
        self.builder
            .build_store(index_ptr_err, self.context.i64_type().const_zero())
            .unwrap();

        self.builder.build_return(None).unwrap();

        self.stdout_stream = Some(stdout_ptr);
        self.stderr_stream = Some(stderr_ptr);

        self.register_global_array("llvm.global_ctors", init_fn);
    }

    pub(super) fn create_builtin_cleanup(&mut self) {
        let stdout_ptr = if let Some(ptr) = self.stdout_stream {
            ptr
        } else {
            return;
        };
        let stderr_ptr = self.stderr_stream.expect("stderr_stream not initialized");

        let void_type = self.context.void_type();
        let dtor_fn_type = void_type.fn_type(&[], false);
        let dtor_fn = self
            .module
            .add_function("__cleanup_builtin_streams", dtor_fn_type, None);

        let dtor_entry = self.context.append_basic_block(dtor_fn, "entry");
        self.builder.position_at_end(dtor_entry);

        let flush_fn = self
            .module
            .get_function("OutStream::flush")
            .expect("OutStream::flush not found");
        self.builder
            .build_call(flush_fn, &[stdout_ptr.into()], "")
            .unwrap();

        self.builder
            .build_call(flush_fn, &[stderr_ptr.into()], "")
            .unwrap();

        self.builder.build_return(None).unwrap();

        self.register_global_array("llvm.global_dtors", dtor_fn);
    }

    pub(super) fn compile_builtin_print(
        &mut self,
        name: &str,
        arguments: &[Expression],
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        if arguments.len() != 1 {
            self.error(
                format!("'{}()' expects exactly 1 argument", name),
                call_span,
            );
            return self.dummy_val();
        }

        let stream_ptr = match name {
            "print" | "println" => {
                if let Some(ptr) = self.stdout_stream {
                    ptr
                } else {
                    return self.context.i32_type().const_int(0, false).into();
                }
            }
            "eprint" | "eprintln" => {
                if let Some(ptr) = self.stderr_stream {
                    ptr
                } else {
                    return self.context.i32_type().const_int(0, false).into();
                }
            }
            _ => unreachable!(),
        };

        let string_arg = self.compile_expression(&arguments[0], None);

        let str_ptr = if let BasicValueEnum::StructValue(str_slice) = string_arg {
            self.builder
                .build_extract_value(str_slice, 0, "str_ptr")
                .unwrap()
        } else if let BasicValueEnum::PointerValue(ptr) = string_arg {
            ptr.into()
        } else {
            self.error(format!("'{}()' expects a string argument", name), call_span);
            return self.dummy_val();
        };

        let write_str_fn = self
            .module
            .get_function("OutStream::write_str")
            .expect("OutStream::write_str not found");

        self.builder
            .build_call(write_str_fn, &[stream_ptr.into(), str_ptr.into()], "")
            .unwrap();

        if name == "println" || name == "eprintln" {
            let newline = self
                .builder
                .build_global_string_ptr("\n", "newline")
                .unwrap();
            self.builder
                .build_call(
                    write_str_fn,
                    &[stream_ptr.into(), newline.as_pointer_value().into()],
                    "",
                )
                .unwrap();

            let flush_fn = self
                .module
                .get_function("OutStream::flush")
                .expect("OutStream::flush not found");
            self.builder
                .build_call(flush_fn, &[stream_ptr.into()], "")
                .unwrap();
        }

        self.context.i32_type().const_int(0, false).into()
    }

    pub(super) fn compile_ok_constructor(
        &mut self,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        if arguments.len() != 1 {
            self.error("'Ok()' expects exactly 1 argument", call_span);
            return self.dummy_val();
        }

        let result_type = if let Some(BasicTypeEnum::StructType(st)) = expected_type {
            st
        } else {
            let inner_val = self.compile_expression(&arguments[0], None);
            let inner_type = inner_val.get_type();
            let tag_type = self.context.bool_type().into();
            let error_code_type = self.context.i32_type().into();
            self.context
                .struct_type(&[tag_type, inner_type, error_code_type], false)
        };

        let inner_type = result_type.get_field_type_at_index(1).unwrap();
        let inner_val = self.compile_expression(&arguments[0], Some(inner_type));

        let is_ok = self.context.bool_type().const_int(1, false); // true = Ok
        let zero_error = self.context.i32_type().const_int(0, false);

        let mut result_val = result_type.get_undef();
        result_val = self
            .builder
            .build_insert_value(result_val, is_ok, 0, "res_tag")
            .unwrap()
            .into_struct_value();
        result_val = self
            .builder
            .build_insert_value(result_val, inner_val, 1, "res_val")
            .unwrap()
            .into_struct_value();
        result_val = self
            .builder
            .build_insert_value(result_val, zero_error, 2, "res_err")
            .unwrap()
            .into_struct_value();

        result_val.into()
    }

    pub(super) fn compile_err_constructor(
        &mut self,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call_span: Span,
    ) -> BasicValueEnum<'ctx> {
        if arguments.len() != 1 {
            self.error("'Err()' expects exactly 1 argument", call_span);
            return self.dummy_val();
        }

        let result_type = if let Some(BasicTypeEnum::StructType(st)) = expected_type {
            st
        } else {
            self.error("'Err()' requires a known Result type context", call_span);
            return self.dummy_val();
        };

        let error_code = self
            .compile_expression(&arguments[0], Some(self.context.i32_type().into()))
            .into_int_value();

        let is_ok = self.context.bool_type().const_int(0, false); // false = Err
        let inner_type = result_type.get_field_type_at_index(1).unwrap();
        let zero_val = self.zero_value_for(inner_type);

        let mut result_val = result_type.get_undef();
        result_val = self
            .builder
            .build_insert_value(result_val, is_ok, 0, "res_tag")
            .unwrap()
            .into_struct_value();
        result_val = self
            .builder
            .build_insert_value(result_val, zero_val, 1, "res_val")
            .unwrap()
            .into_struct_value();
        result_val = self
            .builder
            .build_insert_value(result_val, error_code, 2, "res_err")
            .unwrap()
            .into_struct_value();

        result_val.into()
    }

    /// Compile a method call on a Result value (`is_ok`, `is_err`, `unwrap`, `unwrap_err`).
    pub(super) fn compile_result_method(
        &mut self,
        method_name: &str,
        result_val: inkwell::values::StructValue<'ctx>,
        _span: Span,
    ) -> Option<BasicValueEnum<'ctx>> {
        match method_name {
            "is_ok" => {
                let tag = self
                    .builder
                    .build_extract_value(result_val, 0, "res_tag")
                    .unwrap();
                Some(tag)
            }
            "is_err" => {
                let tag = self
                    .builder
                    .build_extract_value(result_val, 0, "res_tag")
                    .unwrap()
                    .into_int_value();
                let negated = self.builder.build_not(tag, "res_is_err").unwrap();
                Some(negated.into())
            }
            "unwrap" => {
                let tag = self
                    .builder
                    .build_extract_value(result_val, 0, "res_tag")
                    .unwrap()
                    .into_int_value();

                let current_fn = self.current_fn.expect("unwrap called outside function");
                let ok_bb = self.context.append_basic_block(current_fn, "unwrap_ok");
                let panic_bb = self.context.append_basic_block(current_fn, "unwrap_panic");

                self.builder
                    .build_conditional_branch(tag, ok_bb, panic_bb)
                    .unwrap();

                // Panic path: abort on Err
                self.builder.position_at_end(panic_bb);
                let abort_fn = self.get_or_create_panic_fn();
                self.builder.build_call(abort_fn, &[], "").unwrap();
                self.builder.build_unreachable().unwrap();

                // Ok path: extract the value
                self.builder.position_at_end(ok_bb);
                let val = self
                    .builder
                    .build_extract_value(result_val, 1, "unwrap_val")
                    .unwrap();
                Some(val)
            }
            "unwrap_err" => {
                let tag = self
                    .builder
                    .build_extract_value(result_val, 0, "res_tag")
                    .unwrap()
                    .into_int_value();

                let current_fn = self.current_fn.expect("unwrap_err called outside function");
                let ok_bb = self
                    .context
                    .append_basic_block(current_fn, "unwrap_err_panic");
                let err_bb = self.context.append_basic_block(current_fn, "unwrap_err_ok");

                self.builder
                    .build_conditional_branch(tag, ok_bb, err_bb)
                    .unwrap();

                // Panic path: abort on Ok
                self.builder.position_at_end(ok_bb);
                let abort_fn = self.get_or_create_panic_fn();
                self.builder.build_call(abort_fn, &[], "").unwrap();
                self.builder.build_unreachable().unwrap();

                // Err path: extract the error code
                self.builder.position_at_end(err_bb);
                let err_code = self
                    .builder
                    .build_extract_value(result_val, 2, "unwrap_err_val")
                    .unwrap();
                Some(err_code)
            }
            _ => None,
        }
    }
}
