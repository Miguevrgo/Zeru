//! Statement and expression lowering.
//!
//! This is where most day-to-day codegen work happens: control flow,
//! variable declarations, function bodies, arithmetic, calls, casts,
//! pattern matching, and so on.

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{Expression, ExpressionKind, Statement, StatementKind, TypeSpec},
    codegen::compiler::{Compiler, LoopContext},
    errors::Span,
    token::Token,
};

/// Outcome of resolving a method call on a receiver expression.
///
/// Some receivers (vector built-ins, `copy()`, slice `.len()`) produce a
/// value directly; others resolve to a real LLVM function plus the
/// implicit `self` argument, which is then dispatched through the shared
/// call-site emission code.
enum MethodCallOutcome<'ctx> {
    Done(BasicValueEnum<'ctx>),
    Resolved(FunctionValue<'ctx>, Vec<BasicMetadataValueEnum<'ctx>>),
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Build a `{ *u8, usize }` string slice from raw bytes.
    ///
    /// The bytes are emitted as a global string and packed into the
    /// 2-field struct that string literals lower to.
    fn build_str_slice(&mut self, s: &[u8]) -> BasicValueEnum<'ctx> {
        let str_val = std::str::from_utf8(s).unwrap();
        let global_str = self
            .builder
            .build_global_string_ptr(str_val, "str")
            .unwrap();
        let str_ptr = global_str.as_pointer_value();
        let str_len = self.context.i64_type().const_int(s.len() as u64, false);

        let ptr_type = self
            .context
            .ptr_type(inkwell::AddressSpace::default())
            .into();
        let len_type = self.context.i64_type().into();
        let str_type = self.context.struct_type(&[ptr_type, len_type], false);

        let mut str_slice = str_type.get_undef();
        str_slice = self
            .builder
            .build_insert_value(str_slice, str_ptr, 0, "str_ptr")
            .unwrap()
            .into_struct_value();
        str_slice = self
            .builder
            .build_insert_value(str_slice, str_len, 1, "str_len")
            .unwrap()
            .into_struct_value();
        str_slice.into()
    }
    pub(super) fn compile_fn_prototype(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
        return_type: &Option<TypeSpec>,
    ) -> FunctionValue<'ctx> {
        let ret_type = if name == "main" && return_type.is_none() {
            Some(self.context.i32_type().as_basic_type_enum())
        } else {
            match return_type {
                Some(spec) => self.get_llvm_type(spec),
                None => None,
            }
        };

        let mut param_types = Vec::new();
        for (param_name, type_spec, is_mut) in params {
            if param_name == "self" {
                if let Some(struct_name) = &self.current_struct_context
                    && let Some((st, _)) = self.struct_defs.get(struct_name)
                {
                    if *is_mut {
                        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                        param_types.push(ptr_type.into());
                    } else {
                        param_types.push(st.as_basic_type_enum().into());
                    }
                    continue;
                }
                if let Some((struct_name, _)) = name.split_once("::")
                    && let Some((st, _)) = self.struct_defs.get(struct_name)
                {
                    if *is_mut {
                        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                        param_types.push(ptr_type.into());
                    } else {
                        param_types.push(st.as_basic_type_enum().into());
                    }
                    continue;
                }
                self.error(
                    format!(
                        "'self' parameter used outside of struct context in function '{}'",
                        name
                    ),
                    Span::default(),
                );
                continue;
            }

            if let Some(ty) = self.get_llvm_type(type_spec) {
                param_types.push(ty.into());
            } else {
                self.error(
                    format!("Function parameter '{}' cannot be void", param_name),
                    Span::default(),
                );
            }
        }

        let fn_type = match ret_type {
            Some(basic_ty) => basic_ty.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };

        self.module.add_function(name, fn_type, None)
    }

    pub(super) fn compile_fn_body(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
        body: &[Statement],
    ) {
        let function = self.module.get_function(name).unwrap();
        self.current_fn = Some(function);

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.variables.clear();

        for (i, arg) in function.get_param_iter().enumerate() {
            let (param_name, param_spec, is_mut) = &params[i];

            if param_name == "self" {
                if *is_mut {
                    if let Some(struct_name) = &self.current_struct_context {
                        let struct_type = self
                            .struct_defs
                            .get(struct_name)
                            .unwrap()
                            .0
                            .as_basic_type_enum();
                        let ptr_type = self
                            .context
                            .ptr_type(inkwell::AddressSpace::default())
                            .as_basic_type_enum();
                        let alloca = self.create_entry_block_alloca(function, param_name, ptr_type);
                        self.builder.build_store(alloca, arg).unwrap();
                        self.variables
                            .insert(param_name.clone(), (alloca, ptr_type, false));

                        self.pointer_elem_types
                            .insert(param_name.clone(), struct_type);
                        continue;
                    }
                } else if let Some(struct_name) = &self.current_struct_context {
                    let struct_type = self
                        .struct_defs
                        .get(struct_name)
                        .unwrap()
                        .0
                        .as_basic_type_enum();
                    let alloca = self.create_entry_block_alloca(function, param_name, struct_type);
                    self.builder.build_store(alloca, arg).unwrap();
                    self.variables
                        .insert(param_name.clone(), (alloca, struct_type, false));
                    continue;
                }

                self.error("'self' used in non-struct context body", Span::default());
                return;
            }

            let arg_type = match self.get_llvm_type(param_spec) {
                Some(t) => t,
                None => {
                    self.error(
                        format!("Parameter '{}' has invalid type", param_name),
                        Span::default(),
                    );
                    return;
                }
            };

            let alloca = self.create_entry_block_alloca(function, param_name, arg_type);
            self.builder.build_store(alloca, arg).unwrap();

            let is_unsigned = Self::is_unsigned_type(param_spec);

            if let TypeSpec::Pointer(inner_spec) = param_spec
                && let Some(elem_type) = self.get_llvm_type(inner_spec)
            {
                self.pointer_elem_types
                    .insert(param_name.clone(), elem_type);
            }

            self.variables
                .insert(param_name.clone(), (alloca, arg_type, is_unsigned));
        }

        for stmt in body {
            self.compile_statement(stmt);
        }

        let current_block = self.builder.get_insert_block().unwrap();

        if current_block.get_terminator().is_none() {
            let ret_opt = function.get_type().get_return_type();

            if ret_opt.is_none() {
                self.builder.build_return(None).unwrap();
            } else if name == "main" {
                let zero = self.context.i32_type().const_zero();
                self.builder.build_return(Some(&zero)).unwrap();
            } else {
                self.builder.build_unreachable().unwrap();
            }
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        match &stmt.kind {
            StatementKind::Var {
                name,
                value,
                type_annotation,
                ..
            } => {
                let is_unsigned = type_annotation.as_ref().is_some_and(Self::is_unsigned_type);

                let target_type = if let Some(spec) = type_annotation {
                    match self.get_llvm_type(spec) {
                        Some(ty) => Some(ty),
                        None => {
                            self.error(
                                format!("Variable '{}' cannot have void type", name),
                                stmt.span,
                            );
                            return;
                        }
                    }
                } else {
                    None
                };

                if let Some(TypeSpec::Pointer(inner_spec)) = type_annotation
                    && let Some(elem_type) = self.get_llvm_type(inner_spec)
                {
                    self.pointer_elem_types.insert(name.clone(), elem_type);
                }

                let initial_val = self.compile_expression(value, target_type);
                let final_type = target_type.unwrap_or_else(|| initial_val.get_type());

                let function = self.current_fn.unwrap();
                let alloca = self.create_entry_block_alloca(function, name, final_type);

                self.builder.build_store(alloca, initial_val).unwrap();
                self.variables
                    .insert(name.clone(), (alloca, final_type, is_unsigned));

                if let Some(scope) = self.scope_stack.last_mut() {
                    scope.push(name.clone());
                }
            }
            StatementKind::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    let ret_hint = self.current_fn.and_then(|f| f.get_type().get_return_type());
                    let val = self.compile_expression(expr, ret_hint);
                    self.builder.build_return(Some(&val)).unwrap();
                } else {
                    let is_main = self
                        .current_fn
                        .map(|f| f.get_name().to_str().unwrap_or("") == "main")
                        .unwrap_or(false);
                    if is_main {
                        let zero = self.context.i32_type().const_zero();
                        self.builder.build_return(Some(&zero)).unwrap();
                    } else {
                        self.builder.build_return(None).unwrap();
                    }
                }
            }
            StatementKind::Expression(expr) => {
                self.compile_expression(expr, None);
            }
            StatementKind::Block(stmts) => {
                self.scope_stack.push(Vec::new());

                for statement in stmts {
                    self.compile_statement(statement);
                }

                self.scope_stack.pop();
            }
            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let parent_fn = self.current_fn.unwrap();

                let cond_val =
                    self.compile_expression(condition, Some(self.context.bool_type().into()));
                let cond_bool = cond_val.into_int_value();

                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "merge");
                if else_branch.is_some() {
                    self.builder
                        .build_conditional_branch(cond_bool, then_bb, else_bb)
                        .unwrap();
                } else {
                    self.builder
                        .build_conditional_branch(cond_bool, then_bb, merge_bb)
                        .unwrap();
                }

                self.builder.position_at_end(then_bb);
                self.compile_statement(then_branch);
                let then_end_bb = self.builder.get_insert_block().unwrap();
                if then_end_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                if let Some(else_stmt) = else_branch {
                    self.builder.position_at_end(else_bb);
                    self.compile_statement(else_stmt);
                    let else_end_bb = self.builder.get_insert_block().unwrap();
                    if else_end_bb.get_terminator().is_none() {
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                } else {
                    self.builder.position_at_end(else_bb);
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                self.builder.position_at_end(merge_bb);
            }

            StatementKind::While { cond, body } => {
                let parent_fn = self.current_fn.unwrap();

                let loop_cond_bb = self.context.append_basic_block(parent_fn, "loop_cond");
                let loop_body_bb = self.context.append_basic_block(parent_fn, "loop_body");
                let after_loop_bb = self.context.append_basic_block(parent_fn, "after_loop");

                self.builder
                    .build_unconditional_branch(loop_cond_bb)
                    .unwrap();

                self.builder.position_at_end(loop_cond_bb);
                let cond_val = self.compile_expression(cond, Some(self.context.bool_type().into()));
                let cond_bool = cond_val.into_int_value();

                self.builder
                    .build_conditional_branch(cond_bool, loop_body_bb, after_loop_bb)
                    .unwrap();

                self.loop_stack.push(LoopContext {
                    continue_block: loop_cond_bb,
                    break_block: after_loop_bb,
                });

                self.builder.position_at_end(loop_body_bb);
                self.compile_statement(body);

                self.loop_stack.pop();

                let current_bb = self.builder.get_insert_block().unwrap();
                if current_bb.get_terminator().is_none() {
                    self.builder
                        .build_unconditional_branch(loop_cond_bb)
                        .unwrap();
                }

                self.builder.position_at_end(after_loop_bb);
            }
            StatementKind::Break => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    self.builder
                        .build_unconditional_branch(loop_ctx.break_block)
                        .unwrap();
                }
            }
            StatementKind::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    self.builder
                        .build_unconditional_branch(loop_ctx.continue_block)
                        .unwrap();
                }
            }

            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => {
                let parent_fn = self.current_fn.unwrap();

                let (arr_ptr, arr_type) = match self.compile_lvalue(iterable) {
                    Some((ptr, BasicTypeEnum::ArrayType(arr_ty))) => (ptr, arr_ty),
                    _ => {
                        println!("Codegen: ForIn requires an array type");
                        return;
                    }
                };

                let array_len = arr_type.len() as u64;
                let elem_type = arr_type.get_element_type();
                let i64_type = self.context.i64_type();

                let index_ptr = self.builder.build_alloca(i64_type, "for_index").unwrap();
                self.builder
                    .build_store(index_ptr, i64_type.const_zero())
                    .unwrap();

                let elem_ptr = self.builder.build_alloca(elem_type, variable).unwrap();
                self.variables
                    .insert(variable.clone(), (elem_ptr, elem_type, false));

                let loop_cond_bb = self.context.append_basic_block(parent_fn, "for_cond");
                let loop_body_bb = self.context.append_basic_block(parent_fn, "for_body");
                let after_loop_bb = self.context.append_basic_block(parent_fn, "after_for");

                self.builder
                    .build_unconditional_branch(loop_cond_bb)
                    .unwrap();

                self.builder.position_at_end(loop_cond_bb);
                let index_val = self
                    .builder
                    .build_load(i64_type, index_ptr, "index")
                    .unwrap()
                    .into_int_value();
                let len_val = i64_type.const_int(array_len, false);
                let cond = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, index_val, len_val, "for_cond")
                    .unwrap();
                self.builder
                    .build_conditional_branch(cond, loop_body_bb, after_loop_bb)
                    .unwrap();

                self.builder.position_at_end(loop_body_bb);

                let zero = i64_type.const_zero();
                let elem_gep = unsafe {
                    self.builder
                        .build_in_bounds_gep(arr_type, arr_ptr, &[zero, index_val], "elem_gep")
                        .unwrap()
                };
                let elem_val = self
                    .builder
                    .build_load(elem_type, elem_gep, "elem_val")
                    .unwrap();
                self.builder.build_store(elem_ptr, elem_val).unwrap();

                let loop_incr_bb = self.context.append_basic_block(parent_fn, "for_incr");
                self.loop_stack.push(LoopContext {
                    continue_block: loop_incr_bb,
                    break_block: after_loop_bb,
                });

                self.compile_statement(body);
                self.loop_stack.pop();

                let current_bb = self.builder.get_insert_block().unwrap();
                if current_bb.get_terminator().is_none() {
                    self.builder
                        .build_unconditional_branch(loop_incr_bb)
                        .unwrap();
                }

                self.builder.position_at_end(loop_incr_bb);
                let next_index = self
                    .builder
                    .build_int_add(index_val, i64_type.const_int(1, false), "next_index")
                    .unwrap();
                self.builder.build_store(index_ptr, next_index).unwrap();
                self.builder
                    .build_unconditional_branch(loop_cond_bb)
                    .unwrap();

                self.builder.position_at_end(after_loop_bb);

                self.variables.remove(variable);
            }

            _ => println!("Codegen: Unimplemented statement: {:?}", stmt.kind),
        }
    }

    pub(super) fn compile_const_expr(
        &mut self,
        expr: &Expression,
        type_annotation: Option<&TypeSpec>,
    ) -> BasicValueEnum<'ctx> {
        match &expr.kind {
            ExpressionKind::Int(val) => {
                let int_type = match type_annotation {
                    Some(ts) => match self.get_llvm_type(ts) {
                        Some(BasicTypeEnum::IntType(t)) => t,
                        _ => self.context.i32_type(),
                    },
                    None => self.context.i32_type(),
                };
                int_type.const_int(*val as u64, false).into()
            }
            ExpressionKind::Float(val) => {
                let float_type = match type_annotation {
                    Some(ts) => match self.get_llvm_type(ts) {
                        Some(BasicTypeEnum::FloatType(t)) => t,
                        _ => self.context.f32_type(),
                    },
                    None => self.context.f32_type(),
                };
                float_type.const_float(*val).into()
            }
            ExpressionKind::Boolean(val) => self
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into(),
            ExpressionKind::StringLit(s) => self.build_str_slice(s),
            _ => {
                self.error(
                    format!("Unsupported constant expression: {:?}", expr.kind),
                    expr.span,
                );
                self.dummy_val()
            }
        }
    }

    pub(super) fn compile_lvalue(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if let Some((ptr, ty, _)) = self.variables.get(name) {
                    if name == "self" && matches!(ty, BasicTypeEnum::PointerType(_)) {
                        let ptr_val = self.builder.build_load(*ty, *ptr, "self_ptr").unwrap();
                        if let BasicValueEnum::PointerValue(actual_ptr) = ptr_val
                            && let Some(elem_type) = self.pointer_elem_types.get(name)
                        {
                            return Some((actual_ptr, *elem_type));
                        }

                        return None;
                    }
                    return Some((*ptr, *ty));
                }
                None
            }
            ExpressionKind::Get { object, name } => {
                let (ptr, val_type) = self.compile_lvalue(object)?;

                if let BasicTypeEnum::StructType(struct_ty) = val_type {
                    let struct_name = struct_ty
                        .get_name()
                        .expect("Anonymous struct in field access")
                        .to_str()
                        .unwrap();

                    if let Some((_, indices)) = self.struct_defs.get(struct_name)
                        && let Some(&index) = indices.get(name)
                    {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, ptr, index, "field_ptr")
                            .ok()?;

                        let field_type = struct_ty.get_field_type_at_index(index)?;
                        return Some((field_ptr, field_type));
                    }
                }
                None
            }

            ExpressionKind::Index { left, index } => {
                let (ptr, array_type) = self.compile_lvalue(left)?;

                if let BasicTypeEnum::ArrayType(array_ty) = array_type {
                    let index_val = self
                        .compile_expression(index, Some(self.context.i64_type().into()))
                        .into_int_value();

                    let zero = self.context.i64_type().const_zero();
                    let elem_ptr = unsafe {
                        self.builder
                            .build_in_bounds_gep(array_ty, ptr, &[zero, index_val], "elem_ptr")
                            .ok()?
                    };

                    let elem_type = array_ty.get_element_type();
                    return Some((elem_ptr, elem_type));
                }
                None
            }

            ExpressionKind::Dereference(inner) => {
                let ptr_val = self.compile_expression(inner, None);

                if let BasicValueEnum::PointerValue(ptr) = ptr_val {
                    self.emit_null_check(ptr, "null pointer dereference in assignment");

                    let elem_type = self.context.i64_type().into();
                    Some((ptr, elem_type))
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    pub(super) fn compile_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let result = self.compile_expression_inner(expr, expected_type);

        if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type
            && opt_type.count_fields() == 2
            && opt_type.get_field_type_at_index(0) == Some(self.context.bool_type().into())
            && !matches!(result, BasicValueEnum::StructValue(s) if s.get_type() == opt_type)
        {
            let has_value = self.context.bool_type().const_int(1, false);
            let mut opt_val = opt_type.get_undef();
            opt_val = self
                .builder
                .build_insert_value(opt_val, has_value, 0, "opt_tag")
                .unwrap()
                .into_struct_value();
            opt_val = self
                .builder
                .build_insert_value(opt_val, result, 1, "opt_val")
                .unwrap()
                .into_struct_value();
            return opt_val.into();
        }

        result
    }

    fn compile_expression_inner(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let inner_expected = if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type {
            if opt_type.count_fields() == 2
                && opt_type.get_field_type_at_index(0) == Some(self.context.bool_type().into())
            {
                opt_type.get_field_type_at_index(1)
            } else {
                expected_type
            }
        } else {
            expected_type
        };

        match &expr.kind {
            ExpressionKind::Int(val) => {
                let int_type = match inner_expected {
                    Some(BasicTypeEnum::IntType(t)) => t,
                    _ => self.context.i32_type(),
                };

                int_type.const_int(*val as u64, false).into()
            }
            ExpressionKind::Float(val) => {
                let float_type = match inner_expected {
                    Some(BasicTypeEnum::FloatType(t)) => t,
                    _ => self.context.f32_type(),
                };

                float_type.const_float(*val).into()
            }
            ExpressionKind::Identifier(name) => {
                if let Some(const_val) = self.constants.get(name) {
                    return *const_val;
                }

                if let Some((ptr, ty, _)) = self.variables.get(name) {
                    let load_name = format!("{}_load", name);
                    return self.builder.build_load(*ty, *ptr, &load_name).unwrap();
                }

                self.error(format!("Unknown identifier '{}'", name), expr.span);
                self.dummy_val()
            }
            ExpressionKind::Get { .. } | ExpressionKind::Index { .. } => {
                if let ExpressionKind::Get { object, name } = &expr.kind
                    && let ExpressionKind::Identifier(enum_name) = &object.kind
                    && let Some(variants) = self.enum_defs.get(enum_name)
                    && let Some(index) = variants.iter().position(|v| v == name)
                {
                    return self
                        .context
                        .i32_type()
                        .const_int(index as u64, false)
                        .into();
                }

                if let Some((ptr, ty)) = self.compile_lvalue(expr) {
                    let load_name = match &expr.kind {
                        ExpressionKind::Get { name, .. } => format!("{}_load", name),
                        ExpressionKind::Index { .. } => "elem_load".to_string(),
                        ExpressionKind::Identifier(name) => format!("{}_load", name),
                        _ => "field_load".to_string(),
                    };
                    return self.builder.build_load(ty, ptr, &load_name).unwrap();
                }

                if let ExpressionKind::Get { object, name } = &expr.kind {
                    let obj_val = self.compile_expression(object, None);
                    if let BasicValueEnum::StructValue(struct_val) = obj_val {
                        let struct_ty = struct_val.get_type();
                        let struct_name = struct_ty
                            .get_name()
                            .expect("Anonymous struct")
                            .to_str()
                            .unwrap();

                        if let Some((_, indices)) = self.struct_defs.get(struct_name)
                            && let Some(&index) = indices.get(name)
                        {
                            return self
                                .builder
                                .build_extract_value(struct_val, index, "extracttmp")
                                .unwrap();
                        }
                    }
                }

                self.error(
                    format!("Failed to load expression: {:?}", expr.kind),
                    expr.span,
                );
                self.dummy_val()
            }
            ExpressionKind::StructLiteral { name, fields } => {
                let (struct_ty, field_tasks) =
                    if let Some((st, indices)) = self.struct_defs.get(name) {
                        let st = *st;
                        let mut tasks = Vec::new();
                        for (field_name, field_expr) in fields {
                            if let Some(&index) = indices.get(field_name) {
                                let field_type = st.get_field_type_at_index(index).unwrap();
                                tasks.push((index, field_type, field_expr));
                            } else {
                                self.error(
                                    format!("Unknown field '{}' in struct '{}'", field_name, name),
                                    field_expr.span,
                                );
                                return self.dummy_val();
                            }
                        }
                        (st, tasks)
                    } else {
                        self.error(format!("Unknown struct type '{}'", name), expr.span);
                        return self.dummy_val();
                    };

                let mut struct_val = struct_ty.get_undef();
                for (index, field_type, field_expr) in field_tasks {
                    let val = self.compile_expression(field_expr, Some(field_type));
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, val, index, "inserttmp")
                        .unwrap()
                        .into_struct_value();
                }
                struct_val.into()
            }
            ExpressionKind::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    if let Some(BasicTypeEnum::ArrayType(arr_ty)) = expected_type {
                        return arr_ty.get_undef().into();
                    }
                    self.error("Cannot infer type from an empty array literal", expr.span);
                    return self.dummy_val();
                }

                let elem_type = if let Some(BasicTypeEnum::ArrayType(arr_ty)) = expected_type {
                    arr_ty.get_element_type()
                } else {
                    self.compile_expression(&elements[0], None).get_type()
                };

                let array_type = elem_type.array_type(elements.len() as u32);
                let mut array_val = array_type.get_undef();

                for (i, elem_expr) in elements.iter().enumerate() {
                    let elem_val = self.compile_expression(elem_expr, Some(elem_type));
                    array_val = self
                        .builder
                        .build_insert_value(array_val, elem_val, i as u32, "arr_insert")
                        .unwrap()
                        .into_array_value();
                }
                array_val.into()
            }
            ExpressionKind::Assign {
                target,
                operator,
                value,
            } => {
                let (ptr, ty) = self
                    .compile_lvalue(target)
                    .expect("Codegen: Assignment target invalid");

                let final_val = if *operator == Token::Assign {
                    self.compile_expression(value, Some(ty))
                } else {
                    let current_val = {
                        let load_name = match &target.kind {
                            ExpressionKind::Identifier(name) => format!("{}_cur", name),
                            ExpressionKind::Get { name, .. } => format!("{}_cur", name),
                            _ => "cur_val".to_string(),
                        };
                        self.builder.build_load(ty, ptr, &load_name).unwrap()
                    };
                    let rhs = self.compile_expression(value, Some(ty));

                    self.apply_compound_op(current_val, rhs, operator, expr.span)
                };

                self.builder.build_store(ptr, final_val).unwrap();
                final_val
            }
            ExpressionKind::Call {
                function,
                arguments,
            } => self.lower_call(function, arguments, expected_type, expr.span),
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => self.lower_infix(left, operator, right, expected_type, expr),
            ExpressionKind::Boolean(val) => self
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into(),

            ExpressionKind::StringLit(s) => self.build_str_slice(s),
            ExpressionKind::Prefix { operator, right } => {
                let operand = self.compile_expression(right, expected_type);
                match operator {
                    Token::Minus => match operand {
                        BasicValueEnum::IntValue(v) => {
                            self.builder.build_int_neg(v, "negtmp").unwrap().into()
                        }
                        BasicValueEnum::FloatValue(v) => {
                            self.builder.build_float_neg(v, "fnegtmp").unwrap().into()
                        }
                        _ => {
                            self.error("Cannot negate a non-numeric type", right.span);
                            self.dummy_val()
                        }
                    },
                    Token::Bang => {
                        if let BasicValueEnum::IntValue(v) = operand {
                            self.builder.build_not(v, "nottmp").unwrap().into()
                        } else {
                            self.error("Cannot apply '!' to a non-integer type", right.span);
                            self.dummy_val()
                        }
                    }
                    _ => {
                        self.error(
                            format!("Prefix operator '{:?}' is not implemented", operator),
                            expr.span,
                        );
                        self.dummy_val()
                    }
                }
            }

            ExpressionKind::Cast { left, target } => self.lower_cast(left, target, expr.span),
            ExpressionKind::AddressOf(inner) => {
                if let Some((ptr, _ty)) = self.compile_lvalue(inner) {
                    ptr.into()
                } else {
                    self.error("Cannot take address of non-lvalue expression", inner.span);
                    self.dummy_val()
                }
            }

            ExpressionKind::BorrowRef(inner) | ExpressionKind::BorrowRefMut(inner) => {
                if let Some((ptr, _ty)) = self.compile_lvalue(inner) {
                    ptr.into()
                } else {
                    self.error(
                        "Cannot create reference to non-lvalue expression",
                        inner.span,
                    );
                    self.dummy_val()
                }
            }

            ExpressionKind::Dereference(inner) => {
                let elem_type = if let ExpressionKind::Identifier(name) = &inner.kind {
                    self.pointer_elem_types.get(name).copied()
                } else {
                    None
                };

                let ptr_val = self.compile_expression(inner, None);

                if let BasicValueEnum::PointerValue(ptr) = ptr_val {
                    self.emit_null_check(ptr, "null pointer dereference");

                    let load_type = expected_type
                        .or(elem_type)
                        .unwrap_or_else(|| self.context.i64_type().into());
                    self.builder.build_load(load_type, ptr, "deref").unwrap()
                } else {
                    self.error("Cannot dereference a non-pointer value", inner.span);
                    self.dummy_val()
                }
            }

            ExpressionKind::Tuple(elements) => {
                let mut field_types: Vec<BasicTypeEnum<'ctx>> = Vec::with_capacity(elements.len());
                let mut field_values: Vec<BasicValueEnum<'ctx>> =
                    Vec::with_capacity(elements.len());

                for elem in elements {
                    let val = self.compile_expression(elem, None);
                    field_types.push(val.get_type());
                    field_values.push(val);
                }

                let tuple_type = self.context.struct_type(&field_types, false);
                let mut tuple_val = tuple_type.get_undef();

                for (i, val) in field_values.into_iter().enumerate() {
                    tuple_val = self
                        .builder
                        .build_insert_value(tuple_val, val, i as u32, "tuple_insert")
                        .unwrap()
                        .into_struct_value();
                }

                tuple_val.into()
            }
            ExpressionKind::Match { value, arms } => self.lower_match(value, arms, expected_type),
            ExpressionKind::None => {
                if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type {
                    let has_value = self.context.bool_type().const_int(0, false);
                    let inner_type = opt_type.get_field_type_at_index(1).unwrap();
                    let zero_val = self.zero_value_for(inner_type);
                    let mut opt_val = opt_type.get_undef();
                    opt_val = self
                        .builder
                        .build_insert_value(opt_val, has_value, 0, "opt_tag")
                        .unwrap()
                        .into_struct_value();
                    opt_val = self
                        .builder
                        .build_insert_value(opt_val, zero_val, 1, "opt_val")
                        .unwrap()
                        .into_struct_value();
                    opt_val.into()
                } else {
                    self.error("'None' requires a known optional type context", expr.span);
                    self.dummy_val()
                }
            }
            ExpressionKind::InlineAsm {
                template,
                outputs,
                inputs,
                clobbers,
                is_volatile,
            } => self.compile_inline_asm(
                template,
                outputs,
                inputs,
                clobbers,
                *is_volatile,
                expected_type,
            ),
        }
    }

    fn apply_compound_op(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        operator: &Token,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        // Map the compound operator to its plain binary equivalent, then reuse
        // the arithmetic helpers shared with `lower_infix`.
        let plain = match operator {
            Token::PlusEq => Token::Plus,
            Token::MinusEq => Token::Minus,
            Token::StarEq => Token::Star,
            Token::SlashEq => Token::Slash,
            Token::ModEq => Token::Mod,
            Token::BitAndEq => Token::BitAnd,
            Token::BitOrEq => Token::BitOr,
            Token::BitXorEq => Token::BitXor,
            Token::BitLShiftEq => Token::ShiftLeft,
            Token::BitRShiftEq => Token::ShiftRight,
            _ => {
                self.error(format!("Unknown compound operator '{:?}'", operator), span);
                return self.dummy_val();
            }
        };

        match (lhs, rhs) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                self.apply_int_arith(l, r, &plain, true).unwrap_or_else(|| {
                    self.error(format!("Unknown compound operator '{:?}'", operator), span);
                    self.dummy_val()
                })
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                self.apply_float_arith(l, r, &plain).unwrap_or_else(|| {
                    self.error(
                        format!(
                            "Compound operator '{:?}' is not supported for floats",
                            operator
                        ),
                        span,
                    );
                    self.dummy_val()
                })
            }
            _ => {
                self.error("Type mismatch in compound assignment", span);
                self.dummy_val()
            }
        }
    }

    /// Build an integer arithmetic/bitwise op. Returns `None` for unsupported
    /// operators so the caller can choose its own error message.
    fn apply_int_arith(
        &self,
        l: inkwell::values::IntValue<'ctx>,
        r: inkwell::values::IntValue<'ctx>,
        op: &Token,
        signed: bool,
    ) -> Option<BasicValueEnum<'ctx>> {
        let v: BasicValueEnum<'ctx> = match op {
            Token::Plus => self.builder.build_int_add(l, r, "addtmp").unwrap().into(),
            Token::Minus => self.builder.build_int_sub(l, r, "subtmp").unwrap().into(),
            Token::Star => self.builder.build_int_mul(l, r, "multmp").unwrap().into(),
            Token::Slash => {
                if signed {
                    self.builder
                        .build_int_signed_div(l, r, "divtmp")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_div(l, r, "udivtmp")
                        .unwrap()
                        .into()
                }
            }
            Token::Mod => {
                if signed {
                    self.builder
                        .build_int_signed_rem(l, r, "modtmp")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_rem(l, r, "umodtmp")
                        .unwrap()
                        .into()
                }
            }
            Token::BitAnd => self.builder.build_and(l, r, "andtmp").unwrap().into(),
            Token::BitOr => self.builder.build_or(l, r, "ortmp").unwrap().into(),
            Token::BitXor => self.builder.build_xor(l, r, "xortmp").unwrap().into(),
            Token::ShiftLeft => self
                .builder
                .build_left_shift(l, r, "shltmp")
                .unwrap()
                .into(),
            Token::ShiftRight => self
                .builder
                .build_right_shift(l, r, signed, "shrtmp")
                .unwrap()
                .into(),
            _ => return None,
        };
        Some(v)
    }

    /// Build a float arithmetic op. Returns `None` for unsupported operators.
    fn apply_float_arith(
        &self,
        l: inkwell::values::FloatValue<'ctx>,
        r: inkwell::values::FloatValue<'ctx>,
        op: &Token,
    ) -> Option<BasicValueEnum<'ctx>> {
        let v: BasicValueEnum<'ctx> = match op {
            Token::Plus => self
                .builder
                .build_float_add(l, r, "faddtmp")
                .unwrap()
                .into(),
            Token::Minus => self
                .builder
                .build_float_sub(l, r, "fsubtmp")
                .unwrap()
                .into(),
            Token::Star => self
                .builder
                .build_float_mul(l, r, "fmultmp")
                .unwrap()
                .into(),
            Token::Slash => self
                .builder
                .build_float_div(l, r, "fdivtmp")
                .unwrap()
                .into(),
            Token::Mod => self
                .builder
                .build_float_rem(l, r, "fmodtmp")
                .unwrap()
                .into(),
            _ => return None,
        };
        Some(v)
    }

    /// Lower an `ExpressionKind::Call`.
    ///
    /// Resolves the callee (method, free function, generic instantiation,
    /// or built-in) and dispatches through the shared call-site emission.
    fn lower_call(
        &mut self,
        function: &Expression,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let (fn_val, implicit_args) = if let ExpressionKind::Get {
            object,
            name: method_name,
        } = &function.kind
        {
            match self.compile_method_call(object, method_name, arguments, expected_type, span) {
                MethodCallOutcome::Done(v) => return v,
                MethodCallOutcome::Resolved(func, args) => (func, args),
            }
        } else if let ExpressionKind::Identifier(name) = &function.kind {
            if matches!(name.as_str(), "print" | "println" | "eprint" | "eprintln") {
                return self.compile_builtin_print(name, arguments, span);
            }

            if name == "Ok" {
                return self.compile_ok_constructor(arguments, expected_type, span);
            }

            if name == "Err" {
                return self.compile_err_constructor(arguments, expected_type, span);
            }

            if self.generic_functions.contains_key(name) {
                let func = self.monomorphize_call(name, arguments);
                (func, Vec::new())
            } else {
                let func = match self.module.get_function(name) {
                    Some(f) => f,
                    None => {
                        self.error(format!("Unknown function '{}'", name), span);
                        return self.dummy_val();
                    }
                };
                (func, Vec::new())
            }
        } else {
            self.error("Indirect function calls are not yet supported", span);
            return self.dummy_val();
        };

        let mut compiled_args: Vec<BasicMetadataValueEnum> = implicit_args;
        let param_types: Vec<_> = fn_val.get_type().get_param_types();
        let param_offset = compiled_args.len();

        for (i, arg) in arguments.iter().enumerate() {
            let expected: Option<BasicTypeEnum> =
                param_types.get(i + param_offset).and_then(|t| match t {
                    inkwell::types::BasicMetadataTypeEnum::ArrayType(t) => Some((*t).into()),
                    inkwell::types::BasicMetadataTypeEnum::FloatType(t) => Some((*t).into()),
                    inkwell::types::BasicMetadataTypeEnum::IntType(t) => Some((*t).into()),
                    inkwell::types::BasicMetadataTypeEnum::PointerType(t) => Some((*t).into()),
                    inkwell::types::BasicMetadataTypeEnum::StructType(t) => Some((*t).into()),
                    inkwell::types::BasicMetadataTypeEnum::VectorType(t) => Some((*t).into()),
                    _ => None,
                });
            compiled_args.push(self.compile_expression(arg, expected).into());
        }

        let call_site = self
            .builder
            .build_call(fn_val, &compiled_args, "call_res")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(value) => value,
            inkwell::values::ValueKind::Instruction(_) => {
                self.context.i32_type().const_int(0, false).into()
            }
        }
    }

    /// Resolve a method call on `object`.
    ///
    /// Handles built-in fast paths (`copy`, `Vec::*`, slice `.len()`) by
    /// returning [`MethodCallOutcome::Done`] with the result. Otherwise
    /// resolves the user-defined method and returns it together with the
    /// implicit `self` argument as [`MethodCallOutcome::Resolved`].
    fn compile_method_call(
        &mut self,
        object: &Expression,
        method_name: &str,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        span: Span,
    ) -> MethodCallOutcome<'ctx> {
        if method_name == "copy" && arguments.is_empty() {
            return MethodCallOutcome::Done(self.compile_expression(object, expected_type));
        }

        if let ExpressionKind::Identifier(type_name) = &object.kind
            && type_name == "Vec"
        {
            return MethodCallOutcome::Done(self.compile_vec_static_method(
                method_name,
                arguments,
                expected_type,
                span,
            ));
        }

        if let ExpressionKind::Identifier(var_name) = &object.kind
            && let Some((ptr, ty, _)) = self.variables.get(var_name).cloned()
            && let BasicTypeEnum::StructType(st) = ty
            && st.count_fields() == 3
        {
            let elem_size: u64 = 8;
            if let Some(result) =
                self.compile_vec_method_mut(method_name, ptr, arguments, elem_size)
            {
                return MethodCallOutcome::Done(result);
            }
        }

        let obj_val = self.compile_expression(object, None);

        if let BasicValueEnum::StructValue(vec_struct) = obj_val {
            let struct_type = vec_struct.get_type();
            if struct_type.count_fields() == 3
                && let Some(result) =
                    self.compile_vec_method(method_name, vec_struct, arguments, object)
            {
                return MethodCallOutcome::Done(result);
            }
            // Check if this is a Result type (field 0 is i1/bool tag)
            if struct_type.count_fields() == 3 {
                if let Some(inkwell::types::BasicTypeEnum::IntType(it)) =
                    struct_type.get_field_type_at_index(0)
                {
                    if it.get_bit_width() == 1 {
                        if let Some(result) =
                            self.compile_result_method(method_name, vec_struct, span)
                        {
                            return MethodCallOutcome::Done(result);
                        }
                    }
                }
            }
            if struct_type.count_fields() == 2 && method_name == "len" {
                let len = self
                    .builder
                    .build_extract_value(vec_struct, 1, "slice_len")
                    .unwrap();
                return MethodCallOutcome::Done(len);
            }
        }

        let struct_name_result = if let ExpressionKind::Identifier(var_name) = &object.kind {
            self.variables.get(var_name).and_then(|(_, ty, _)| {
                if let BasicTypeEnum::StructType(st) = ty {
                    Some(st.get_name().unwrap().to_str().unwrap().to_string())
                } else if let BasicTypeEnum::PointerType(_) = ty {
                    self.pointer_elem_types.get(var_name).and_then(|elem_ty| {
                        if let BasicTypeEnum::StructType(st) = elem_ty {
                            Some(st.get_name().unwrap().to_str().unwrap().to_string())
                        } else {
                            None
                        }
                    })
                } else {
                    None
                }
            })
        } else {
            None
        };

        let struct_name = if let Some(name) = struct_name_result {
            name
        } else {
            let obj_val = self.compile_expression(object, None);
            match obj_val.get_type() {
                BasicTypeEnum::StructType(st) => {
                    st.get_name().unwrap().to_str().unwrap().to_string()
                }
                _ => {
                    self.error("Method call on non-struct value", span);
                    return MethodCallOutcome::Done(self.dummy_val());
                }
            }
        };

        let mangled = format!("{}::{}", struct_name, method_name);
        let func = match self.module.get_function(&mangled) {
            Some(f) => f,
            None => {
                self.error(format!("Method '{}' not found", mangled), span);
                return MethodCallOutcome::Done(self.dummy_val());
            }
        };

        let param_types = func.get_type().get_param_types();
        let first_param_is_ptr = param_types
            .first()
            .map(|t| matches!(t, inkwell::types::BasicMetadataTypeEnum::PointerType(_)))
            .unwrap_or(false);

        let args: Vec<BasicMetadataValueEnum> = if first_param_is_ptr {
            if let ExpressionKind::Identifier(var_name) = &object.kind {
                if let Some((ptr, _, _)) = self.variables.get(var_name) {
                    if self.pointer_elem_types.contains_key(var_name) {
                        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                        let loaded_ptr = self
                            .builder
                            .build_load(ptr_type, *ptr, "self_loaded")
                            .unwrap();
                        vec![loaded_ptr.into()]
                    } else {
                        vec![(*ptr).into()]
                    }
                } else {
                    self.error("Cannot get pointer to object for method call", span);
                    return MethodCallOutcome::Done(self.dummy_val());
                }
            } else {
                self.error("'var self' method requires an identifier as receiver", span);
                return MethodCallOutcome::Done(self.dummy_val());
            }
        } else {
            let obj_val = self.compile_expression(object, None);
            vec![obj_val.into()]
        };

        MethodCallOutcome::Resolved(func, args)
    }

    /// Lower an `ExpressionKind::Infix`.
    ///
    /// Dispatches the operator to the appropriate LLVM builder based on
    /// the operand category (integer, float, pointer arithmetic, pointer
    /// comparison) and handles the special-cased `::`, `&&` and `||`.
    fn lower_infix(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
        expr: &Expression,
    ) -> BasicValueEnum<'ctx> {
        if *operator == Token::DoubleColon
            && let (ExpressionKind::Identifier(enum_name), ExpressionKind::Identifier(variant_name)) =
                (&left.kind, &right.kind)
        {
            if let Some(variants) = self.enum_defs.get(enum_name)
                && let Some(index) = variants.iter().position(|v| v == variant_name)
            {
                return self
                    .context
                    .i32_type()
                    .const_int(index as u64, false)
                    .into();
            }

            self.error("Invalid '::' expression", expr.span);
            return self.dummy_val();
        }

        if *operator == Token::And || *operator == Token::Or {
            return self.lower_short_circuit(left, operator, right, expr.span);
        }

        let is_comparison = matches!(
            operator,
            Token::Eq | Token::NotEq | Token::Lt | Token::Leq | Token::Gt | Token::Geq
        );

        let operand_hint = if is_comparison { None } else { expected_type };

        let lhs = self.compile_expression(left, operand_hint);
        let rhs = self.compile_expression(right, Some(lhs.get_type()));

        match (lhs, rhs) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                // Comparisons are signed/unsigned-aware via the LHS expr type.
                let unsigned = Self::is_unsigned_expr(left);
                let int_cmp = |signed: IntPredicate, unsigned_pred: IntPredicate, name: &str| {
                    let pred = if unsigned { unsigned_pred } else { signed };
                    self.builder
                        .build_int_compare(pred, l, r, name)
                        .unwrap()
                        .into()
                };

                match operator {
                    Token::Eq => self
                        .builder
                        .build_int_compare(IntPredicate::EQ, l, r, "eqtmp")
                        .unwrap()
                        .into(),
                    Token::NotEq => self
                        .builder
                        .build_int_compare(IntPredicate::NE, l, r, "netmp")
                        .unwrap()
                        .into(),
                    Token::Lt => int_cmp(IntPredicate::SLT, IntPredicate::ULT, "lttmp"),
                    Token::Leq => int_cmp(IntPredicate::SLE, IntPredicate::ULE, "letmp"),
                    Token::Gt => int_cmp(IntPredicate::SGT, IntPredicate::UGT, "gttmp"),
                    Token::Geq => int_cmp(IntPredicate::SGE, IntPredicate::UGE, "getmp"),
                    _ => {
                        // For arithmetic/bitwise the signedness of the *result* (or
                        // the LHS for shifts) drives the LLVM op choice.
                        let signed = if matches!(operator, Token::ShiftRight) {
                            self.is_signed_integer(left).unwrap_or(true)
                        } else {
                            self.is_signed_integer(expr).unwrap_or(true)
                        };
                        self.apply_int_arith(l, r, operator, signed)
                            .unwrap_or_else(|| {
                                self.error(
                                    format!("Integer operator '{:?}' is not implemented", operator),
                                    expr.span,
                                );
                                self.dummy_val()
                            })
                    }
                }
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => match operator {
                Token::Eq => self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "feqtmp")
                    .unwrap()
                    .into(),
                Token::NotEq => self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "fnetmp")
                    .unwrap()
                    .into(),
                Token::Lt => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "flttmp")
                    .unwrap()
                    .into(),
                Token::Leq => self
                    .builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fletmp")
                    .unwrap()
                    .into(),
                Token::Gt => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fgttmp")
                    .unwrap()
                    .into(),
                Token::Geq => self
                    .builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fgetmp")
                    .unwrap()
                    .into(),
                _ => self.apply_float_arith(l, r, operator).unwrap_or_else(|| {
                    self.error(
                        format!("Float operator '{:?}' is not implemented", operator),
                        expr.span,
                    );
                    self.dummy_val()
                }),
            },
            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(offset)) => {
                let off = match operator {
                    Token::Plus => offset,
                    Token::Minus => self.builder.build_int_neg(offset, "neg").unwrap(),
                    _ => {
                        self.error(
                            "Only '+' and '-' are supported for pointer arithmetic",
                            expr.span,
                        );
                        return self.dummy_val();
                    }
                };
                unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), ptr, &[off], "ptr")
                        .unwrap()
                        .into()
                }
            }
            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                // Pointer comparison - convert to int and compare
                let usize_type = self.context.i64_type();
                let l_int = self
                    .builder
                    .build_ptr_to_int(l, usize_type, "ptr_l")
                    .unwrap();
                let r_int = self
                    .builder
                    .build_ptr_to_int(r, usize_type, "ptr_r")
                    .unwrap();
                match operator {
                    Token::Eq => self
                        .builder
                        .build_int_compare(IntPredicate::EQ, l_int, r_int, "ptr_eq")
                        .unwrap()
                        .into(),
                    Token::NotEq => self
                        .builder
                        .build_int_compare(IntPredicate::NE, l_int, r_int, "ptr_ne")
                        .unwrap()
                        .into(),
                    Token::Lt => self
                        .builder
                        .build_int_compare(IntPredicate::ULT, l_int, r_int, "ptr_lt")
                        .unwrap()
                        .into(),
                    Token::Leq => self
                        .builder
                        .build_int_compare(IntPredicate::ULE, l_int, r_int, "ptr_le")
                        .unwrap()
                        .into(),
                    Token::Gt => self
                        .builder
                        .build_int_compare(IntPredicate::UGT, l_int, r_int, "ptr_gt")
                        .unwrap()
                        .into(),
                    Token::Geq => self
                        .builder
                        .build_int_compare(IntPredicate::UGE, l_int, r_int, "ptr_ge")
                        .unwrap()
                        .into(),
                    _ => {
                        self.error(
                            "Only comparison operators are supported for pointer-pointer operations",
                            expr.span,
                        );
                        self.dummy_val()
                    }
                }
            }
            _ => {
                self.error("Type mismatch in binary operation", expr.span);
                self.dummy_val()
            }
        }
    }

    /// Lower the short-circuit operators `&&` and `||` with proper
    /// branching and a phi node, leaving the builder at the merge block.
    fn lower_short_circuit(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let bool_type = self.context.bool_type();
        let current_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let to_bool = |v: BasicValueEnum<'ctx>, this: &Self| -> inkwell::values::IntValue<'ctx> {
            match v {
                BasicValueEnum::IntValue(v) => {
                    if v.get_type().get_bit_width() == 1 {
                        v
                    } else {
                        this.builder
                            .build_int_compare(
                                IntPredicate::NE,
                                v,
                                v.get_type().const_zero(),
                                "tobool",
                            )
                            .unwrap()
                    }
                }
                _ => this.context.bool_type().const_zero(),
            }
        };

        let lhs = self.compile_expression(left, Some(bool_type.into()));
        if !matches!(lhs, BasicValueEnum::IntValue(_)) {
            self.error("'&&' and '||' require boolean or integer operands", span);
        }
        let lhs_bool = to_bool(lhs, self);

        let entry_block = self.builder.get_insert_block().unwrap();
        let rhs_block = self.context.append_basic_block(current_fn, "rhs_eval");
        let merge_block = self.context.append_basic_block(current_fn, "merge");

        if operator == &Token::And {
            self.builder
                .build_conditional_branch(lhs_bool, rhs_block, merge_block)
                .unwrap();
        } else {
            self.builder
                .build_conditional_branch(lhs_bool, merge_block, rhs_block)
                .unwrap();
        }

        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_expression(right, Some(bool_type.into()));
        if !matches!(rhs, BasicValueEnum::IntValue(_)) {
            self.error("'&&' and '||' require boolean or integer operands", span);
        }
        let rhs_bool = to_bool(rhs, self);
        let rhs_end_block = self.builder.get_insert_block().unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "result").unwrap();

        if operator == &Token::And {
            phi.add_incoming(&[
                (&bool_type.const_zero(), entry_block),
                (&rhs_bool, rhs_end_block),
            ]);
        } else {
            phi.add_incoming(&[
                (&bool_type.const_all_ones(), entry_block),
                (&rhs_bool, rhs_end_block),
            ]);
        }

        phi.as_basic_value()
    }

    /// Lower an `ExpressionKind::Cast` to the appropriate LLVM conversion.
    fn lower_cast(
        &mut self,
        left: &Expression,
        target: &Expression,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let src_val = self.compile_expression(left, None);

        let target_typespec = match Self::expr_to_typespec(target) {
            Some(t) => t,
            None => {
                self.error(
                    format!("Cannot use '{:?}' as a cast target type", target.kind),
                    target.span,
                );
                return self.dummy_val();
            }
        };
        let target_type = match self.get_llvm_type(&target_typespec) {
            Some(t) => t,
            None => {
                self.error("Cast to void type is not allowed", target.span);
                return self.dummy_val();
            }
        };

        match (src_val, target_type) {
            (BasicValueEnum::IntValue(v), BasicTypeEnum::IntType(t)) => {
                let src_bits = v.get_type().get_bit_width();
                let dst_bits = t.get_bit_width();
                if src_bits == dst_bits {
                    v.into()
                } else if src_bits < dst_bits {
                    self.builder
                        .build_int_z_extend(v, t, "zexttmp")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_truncate(v, t, "trunctmp")
                        .unwrap()
                        .into()
                }
            }
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::IntType(t)) => self
                .builder
                .build_float_to_signed_int(v, t, "fptosi")
                .unwrap()
                .into(),
            (BasicValueEnum::IntValue(v), BasicTypeEnum::FloatType(t)) => self
                .builder
                .build_signed_int_to_float(v, t, "sitofp")
                .unwrap()
                .into(),
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::FloatType(t)) => {
                if v.get_type() == t {
                    v.into()
                } else {
                    let src_is_f32 = v.get_type() == self.context.f32_type();
                    let dst_is_f32 = t == self.context.f32_type();
                    if src_is_f32 && !dst_is_f32 {
                        self.builder.build_float_ext(v, t, "fext").unwrap().into()
                    } else {
                        self.builder
                            .build_float_trunc(v, t, "ftrunc")
                            .unwrap()
                            .into()
                    }
                }
            }
            (BasicValueEnum::PointerValue(v), BasicTypeEnum::IntType(t)) => self
                .builder
                .build_ptr_to_int(v, t, "ptrtoint")
                .unwrap()
                .into(),
            (BasicValueEnum::IntValue(v), BasicTypeEnum::PointerType(t)) => self
                .builder
                .build_int_to_ptr(v, t, "inttoptr")
                .unwrap()
                .into(),
            // Pointer-to-pointer cast (no-op with opaque pointers)
            (BasicValueEnum::PointerValue(v), BasicTypeEnum::PointerType(_t)) => v.into(),
            // str (struct {ptr, len}) → *u8: extract the pointer field
            (BasicValueEnum::StructValue(v), BasicTypeEnum::PointerType(_t)) => {
                let struct_ty = v.get_type();
                if struct_ty.count_fields() == 2 {
                    self.builder
                        .build_extract_value(v, 0, "str_ptr")
                        .unwrap()
                } else {
                    self.error("Unsupported cast combination", span);
                    self.dummy_val()
                }
            }
            // str (struct {ptr, len}) → i64: extract ptr then ptr_to_int
            (BasicValueEnum::StructValue(v), BasicTypeEnum::IntType(t)) => {
                let struct_ty = v.get_type();
                if struct_ty.count_fields() == 2 {
                    let ptr_val = self
                        .builder
                        .build_extract_value(v, 0, "str_ptr")
                        .unwrap();
                    if let BasicValueEnum::PointerValue(p) = ptr_val {
                        self.builder
                            .build_ptr_to_int(p, t, "str_ptrtoint")
                            .unwrap()
                            .into()
                    } else {
                        self.error("Unsupported cast combination", span);
                        self.dummy_val()
                    }
                } else {
                    self.error("Unsupported cast combination", span);
                    self.dummy_val()
                }
            }
            _ => {
                self.error("Unsupported cast combination", span);
                self.dummy_val()
            }
        }
    }

    /// Lower an `ExpressionKind::Match` into a `switch` plus a result phi.
    fn lower_match(
        &mut self,
        value: &Expression,
        arms: &[(Expression, Expression)],
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let parent_fn = self.current_fn.unwrap();
        let match_val = self.compile_expression(value, None).into_int_value();

        let merge_bb = self.context.append_basic_block(parent_fn, "match_merge");

        let mut arm_blocks: Vec<(BasicBlock<'ctx>, BasicValueEnum<'ctx>)> = Vec::new();
        let mut default_block: Option<BasicBlock<'ctx>> = None;
        let mut cases: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>)> = Vec::new();

        for (pattern, _) in arms {
            let block = self.context.append_basic_block(parent_fn, "match_arm");

            if let ExpressionKind::Identifier(name) = &pattern.kind
                && name == "default"
            {
                default_block = Some(block);
                continue;
            }

            let pattern_val = self.compile_expression(pattern, None).into_int_value();
            cases.push((pattern_val, block));
        }

        let default_bb = default_block.unwrap_or_else(|| {
            let bb = self.context.append_basic_block(parent_fn, "match_default");
            self.builder.position_at_end(bb);
            self.builder.build_unreachable().unwrap();
            bb
        });

        let entry_bb = self.builder.get_insert_block().unwrap();
        self.builder.position_at_end(entry_bb);

        let switch = self
            .builder
            .build_switch(match_val, default_bb, &cases)
            .unwrap();
        let _ = switch;

        let mut arm_idx = 0;
        for (pattern, result) in arms {
            let is_default = if let ExpressionKind::Identifier(name) = &pattern.kind {
                name == "default"
            } else {
                false
            };

            let block = if is_default {
                default_block.unwrap()
            } else {
                let (_, block) = cases[arm_idx];
                arm_idx += 1;
                block
            };

            self.builder.position_at_end(block);
            let result_val = self.compile_expression(result, expected_type);
            let current_bb = self.builder.get_insert_block().unwrap();
            if current_bb.get_terminator().is_none() {
                self.builder.build_unconditional_branch(merge_bb).unwrap();
            }
            arm_blocks.push((self.builder.get_insert_block().unwrap(), result_val));
        }

        self.builder.position_at_end(merge_bb);

        if arm_blocks.is_empty() {
            return self.context.i32_type().const_int(0, false).into();
        }

        let result_type = arm_blocks[0].1.get_type();
        let phi = self.builder.build_phi(result_type, "match_result").unwrap();

        for (block, val) in &arm_blocks {
            phi.add_incoming(&[(val, *block)]);
        }

        phi.as_basic_value()
    }
}
