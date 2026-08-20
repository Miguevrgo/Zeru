//! Statement and expression lowering: control flow, declarations, function
//! bodies, arithmetic, calls, casts and pattern matching.

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    types::{BasicType, BasicTypeEnum, StructType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, ValueKind,
    },
};

use crate::{
    ast::{Expression, ExpressionKind, Statement, StatementKind, TypeSpec},
    codegen::{
        compiler::{Compiler, LoopContext, VarBinding},
        layout::{OPTION_VALUE, SLICE_LEN, SLICE_PTR, VEC_LEN, VEC_PTR},
    },
    errors::Span,
    sema::types::Type,
    token::Token,
};

enum MethodCallOutcome<'ctx> {
    Done(BasicValueEnum<'ctx>),
    Resolved(FunctionValue<'ctx>, Vec<BasicMetadataValueEnum<'ctx>>),
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Emit `s` as a global string and pack it into a `{ *u8, usize }` slice.
    fn build_str_slice(&mut self, s: &[u8]) -> BasicValueEnum<'ctx> {
        let text = std::str::from_utf8(s).unwrap_or_default();
        let global = self.builder.build_global_string_ptr(text, "str").unwrap();
        let len = self.usize_type().const_int(s.len() as u64, false);
        self.build_struct(
            self.slice_type(),
            &[global.as_pointer_value().into(), len.into()],
            "str_slice",
        )
        .into()
    }

    /// The struct a method belongs to, taken from the current struct context or
    /// from the `Struct::method` name.
    fn self_struct_type(&self, fn_name: &str) -> Option<StructType<'ctx>> {
        let owner = self
            .current_struct_context
            .as_deref()
            .or_else(|| fn_name.rsplit_once("::").map(|(owner, _)| owner))?;
        self.struct_defs.get(owner).map(|(st, _)| *st)
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
            return_type
                .as_ref()
                .and_then(|spec| self.get_llvm_type(spec))
        };

        let mut param_types = Vec::with_capacity(params.len());
        for (param_name, type_spec, is_mut) in params {
            let param_type = if param_name == "self" {
                match self.self_struct_type(name) {
                    // `var self` comes in by pointer so writes reach the caller.
                    Some(_) if *is_mut => self.ptr_type().as_basic_type_enum(),
                    Some(st) => st.as_basic_type_enum(),
                    None => {
                        self.error(
                            format!("'self' parameter outside of a struct in '{name}'"),
                            Span::default(),
                        );
                        continue;
                    }
                }
            } else {
                match self.get_llvm_type(type_spec) {
                    Some(ty) => ty,
                    None => {
                        self.error(
                            format!("Function parameter '{param_name}' cannot be void"),
                            Span::default(),
                        );
                        continue;
                    }
                }
            };
            param_types.push(param_type.into());
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
        // Missing means the prototype pass already reported why.
        let Some(function) = self.module.get_function(name) else {
            return;
        };
        self.current_fn = Some(function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();
        self.pointer_elem_types.clear();
        self.scope_stack.clear();
        self.scope_stack.push(Vec::new());

        for (arg, (param_name, param_spec, is_mut)) in function.get_param_iter().zip(params) {
            let (slot_type, pointee) = if param_name == "self" {
                let Some(st) = self.self_struct_type(name) else {
                    self.error("'self' used outside of a struct", Span::default());
                    return;
                };
                if *is_mut {
                    (
                        self.ptr_type().as_basic_type_enum(),
                        Some(st.as_basic_type_enum()),
                    )
                } else {
                    (st.as_basic_type_enum(), None)
                }
            } else {
                let Some(ty) = self.get_llvm_type(param_spec) else {
                    self.error(
                        format!("Parameter '{param_name}' has invalid type"),
                        Span::default(),
                    );
                    return;
                };
                let pointee = match param_spec {
                    TypeSpec::Pointer(inner) => self.get_llvm_type(inner),
                    _ => None,
                };
                (ty, pointee)
            };

            let alloca = self.create_entry_block_alloca(function, param_name, slot_type);
            self.builder.build_store(alloca, arg).unwrap();
            self.variables.insert(
                param_name.clone(),
                (alloca, slot_type, Self::is_unsigned_type(param_spec)),
            );
            if let Some(pointee) = pointee {
                self.pointer_elem_types.insert(param_name.clone(), pointee);
            }
        }

        for stmt in body {
            self.compile_statement(stmt);
        }

        let Some(current_block) = self.builder.get_insert_block() else {
            return;
        };
        if current_block.get_terminator().is_some() {
            return;
        }

        match function.get_type().get_return_type() {
            None => self.builder.build_return(None).unwrap(),
            Some(_) if name == "main" => {
                let zero = self.context.i32_type().const_zero();
                self.builder.build_return(Some(&zero)).unwrap()
            }
            Some(_) => self.builder.build_unreachable().unwrap(),
        };
    }

    /// Branch to `target` unless the current block already ends in a terminator.
    fn branch_if_open(&self, target: BasicBlock<'ctx>) {
        if let Some(block) = self.builder.get_insert_block()
            && block.get_terminator().is_none()
        {
            self.builder.build_unconditional_branch(target).unwrap();
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        let Some(parent_fn) = self.current_fn else {
            return;
        };

        match &stmt.kind {
            StatementKind::Var {
                name,
                value,
                type_annotation,
                ..
            } => self.declare_variable(parent_fn, name, value, type_annotation, stmt.span),

            StatementKind::Return(Some(expr)) => {
                let ret_hint = parent_fn.get_type().get_return_type();
                let val = self.compile_expression(expr, ret_hint);
                self.builder.build_return(Some(&val)).unwrap();
            }
            // `main` returns an implicit exit status even on a bare `return`.
            StatementKind::Return(None) => {
                if parent_fn.get_name().to_str() == Ok("main") {
                    let zero = self.context.i32_type().const_zero();
                    self.builder.build_return(Some(&zero)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
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
                self.pop_scope();
            }

            StatementKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(parent_fn, condition, then_branch, else_branch.as_deref()),

            StatementKind::While { cond, body } => self.compile_while(parent_fn, cond, body),

            StatementKind::ForIn {
                variable,
                iterable,
                body,
            } => self.compile_for_in(parent_fn, variable, iterable, body),

            StatementKind::Break | StatementKind::Continue => {
                let is_break = matches!(stmt.kind, StatementKind::Break);
                let Some(ctx) = self.loop_stack.last() else {
                    self.error("'break' and 'continue' need an enclosing loop", stmt.span);
                    return;
                };
                let target = if is_break {
                    ctx.break_block
                } else {
                    ctx.continue_block
                };
                self.builder.build_unconditional_branch(target).unwrap();
            }

            // Accepted by the parser and analyser but never lowered: skipping it
            // would emit a binary that quietly does less than the source says.
            _ => self.error(
                format!("Statement is not implemented in codegen: {:?}", stmt.kind),
                stmt.span,
            ),
        }
    }

    fn declare_variable(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        name: &str,
        value: &Expression,
        type_annotation: &Option<TypeSpec>,
        span: Span,
    ) {
        let annotated = match type_annotation {
            Some(spec) => match self.get_llvm_type(spec) {
                Some(ty) => Some(ty),
                None => {
                    self.error(format!("Variable '{name}' cannot have void type"), span);
                    return;
                }
            },
            None => None,
        };

        if let Some(TypeSpec::Pointer(inner_spec)) = type_annotation
            && let Some(elem_type) = self.get_llvm_type(inner_spec)
        {
            self.pointer_elem_types.insert(name.to_string(), elem_type);
        }

        let initial = self.compile_expression(value, annotated);
        let slot_type = annotated.unwrap_or_else(|| initial.get_type());
        let alloca = self.create_entry_block_alloca(parent_fn, name, slot_type);
        self.builder.build_store(alloca, initial).unwrap();

        let is_unsigned = type_annotation.as_ref().is_some_and(Self::is_unsigned_type);
        self.bind_variable(name, (alloca, slot_type, is_unsigned));
    }

    /// Bind `name` in the innermost scope, remembering what it shadowed.
    fn bind_variable(&mut self, name: &str, binding: VarBinding<'ctx>) {
        let shadowed = self.variables.insert(name.to_string(), binding);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.push((name.to_string(), shadowed));
        }
    }

    /// Close the innermost scope, putting back whatever each name meant outside it.
    fn pop_scope(&mut self) {
        let Some(scope) = self.scope_stack.pop() else {
            return;
        };
        for (name, shadowed) in scope.into_iter().rev() {
            match shadowed {
                Some(outer) => self.variables.insert(name, outer),
                None => self.variables.remove(&name),
            };
        }
    }

    fn compile_if(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) {
        let cond = self.compile_bool(condition);

        let then_bb = self.context.append_basic_block(parent_fn, "then");
        let merge_bb = self.context.append_basic_block(parent_fn, "merge");
        // Without an `else` the false edge goes straight to the merge block.
        let else_bb = match else_branch {
            Some(_) => self.context.append_basic_block(parent_fn, "else"),
            None => merge_bb,
        };

        self.builder
            .build_conditional_branch(cond, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        self.compile_statement(then_branch);
        self.branch_if_open(merge_bb);

        if let Some(else_stmt) = else_branch {
            self.builder.position_at_end(else_bb);
            self.compile_statement(else_stmt);
            self.branch_if_open(merge_bb);
        }

        self.builder.position_at_end(merge_bb);
    }

    fn compile_while(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        cond: &Expression,
        body: &Statement,
    ) {
        let cond_bb = self.context.append_basic_block(parent_fn, "loop_cond");
        let body_bb = self.context.append_basic_block(parent_fn, "loop_body");
        let after_bb = self.context.append_basic_block(parent_fn, "after_loop");

        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let cond_val = self.compile_bool(cond);
        self.builder
            .build_conditional_branch(cond_val, body_bb, after_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        self.compile_loop_body(body, cond_bb, after_bb);
        self.branch_if_open(cond_bb);

        self.builder.position_at_end(after_bb);
    }

    /// Walk a fixed-size array, copying each element into the loop variable.
    fn compile_for_in(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        variable: &str,
        iterable: &Expression,
        body: &Statement,
    ) {
        let Some((container, shape)) = self.compile_lvalue(iterable) else {
            self.error("'for .. in' requires an array or a Vec", iterable.span);
            return;
        };
        let usize_type = self.usize_type();

        // How many turns is settled before the first one, so pushing inside the
        // body cannot extend the loop. A Vec's buffer is fetched again each turn
        // because a push may have moved it.
        let (elem_type, count) = match shape {
            BasicTypeEnum::ArrayType(array_type) => (
                array_type.get_element_type(),
                usize_type.const_int(array_type.len() as u64, false),
            ),
            BasicTypeEnum::StructType(st) if self.is_vec_layout(st) => {
                let len_field = self.vec_field_ptr(container, VEC_LEN, "len_field");
                (
                    self.element_type_of(iterable),
                    self.load_int(usize_type, len_field, "len"),
                )
            }
            _ => {
                self.error("'for .. in' requires an array or a Vec", iterable.span);
                return;
            }
        };

        // Entry-block allocas: a nested loop must not grow the stack per iteration.
        let index_ptr = self.create_entry_block_alloca(parent_fn, "for_index", usize_type.into());
        let elem_slot = self.create_entry_block_alloca(parent_fn, variable, elem_type);
        self.builder
            .build_store(index_ptr, usize_type.const_zero())
            .unwrap();

        self.scope_stack.push(Vec::new());
        self.bind_variable(variable, (elem_slot, elem_type, false));

        let cond_bb = self.context.append_basic_block(parent_fn, "for_cond");
        let body_bb = self.context.append_basic_block(parent_fn, "for_body");
        let incr_bb = self.context.append_basic_block(parent_fn, "for_incr");
        let after_bb = self.context.append_basic_block(parent_fn, "after_for");

        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let index = self.load(usize_type, index_ptr, "index").into_int_value();
        let in_range = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, count, "for_cond")
            .unwrap();
        self.builder
            .build_conditional_branch(in_range, body_bb, after_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        let elem_gep = match shape {
            BasicTypeEnum::ArrayType(array_type) => unsafe {
                self.builder
                    .build_in_bounds_gep(
                        array_type,
                        container,
                        &[usize_type.const_zero(), index],
                        "elem_gep",
                    )
                    .unwrap()
            },
            _ => {
                let data_field = self.vec_field_ptr(container, VEC_PTR, "data_field");
                let ptr_type = self.ptr_type();
                let data = self.load(ptr_type, data_field, "data").into_pointer_value();
                self.vec_elem_ptr(data, index, elem_type)
            }
        };
        let elem_val = self.load(elem_type, elem_gep, "elem_val");
        self.builder.build_store(elem_slot, elem_val).unwrap();

        self.compile_loop_body(body, incr_bb, after_bb);
        self.branch_if_open(incr_bb);

        self.builder.position_at_end(incr_bb);
        let next = self
            .builder
            .build_int_add(index, usize_type.const_int(1, false), "next_index")
            .unwrap();
        self.builder.build_store(index_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(after_bb);
        self.pop_scope();
    }

    fn compile_bool(&mut self, expr: &Expression) -> IntValue<'ctx> {
        let bool_type = self.context.bool_type();
        match self.compile_expression(expr, Some(bool_type.into())) {
            BasicValueEnum::IntValue(v) => v,
            _ => {
                self.error("Condition must be a boolean", expr.span);
                bool_type.const_zero()
            }
        }
    }

    fn compile_loop_body(
        &mut self,
        body: &Statement,
        continue_block: BasicBlock<'ctx>,
        break_block: BasicBlock<'ctx>,
    ) {
        self.loop_stack.push(LoopContext {
            continue_block,
            break_block,
        });
        self.compile_statement(body);
        self.loop_stack.pop();
    }

    pub(super) fn compile_const_expr(
        &mut self,
        expr: &Expression,
        type_annotation: Option<&TypeSpec>,
    ) -> BasicValueEnum<'ctx> {
        let annotated = type_annotation.and_then(|ts| self.get_llvm_type(ts));

        match &expr.kind {
            ExpressionKind::Int(val) => {
                let int_type = match annotated {
                    Some(BasicTypeEnum::IntType(t)) => t,
                    _ => self.context.i32_type(),
                };
                int_type.const_int(*val as u64, false).into()
            }
            ExpressionKind::Float(val) => {
                let float_type = match annotated {
                    Some(BasicTypeEnum::FloatType(t)) => t,
                    _ => self.context.f64_type(),
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

    /// Integer tag of an `Enum::Variant` path, numbered by declaration order.
    fn enum_variant_tag(&self, qualified_name: &str) -> Option<BasicValueEnum<'ctx>> {
        let (enum_name, variant_name) = qualified_name.rsplit_once("::")?;
        let index = self
            .enum_defs
            .get(enum_name)?
            .iter()
            .position(|v| v == variant_name)?;
        Some(
            self.context
                .i32_type()
                .const_int(index as u64, false)
                .into(),
        )
    }

    pub(super) fn compile_lvalue(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                let (ptr, ty, _) = self.variables.get(name)?;
                Some((*ptr, *ty))
            }

            ExpressionKind::Get { object, name } => {
                let (ptr, struct_ty) = self.struct_place(object)?;
                let index = self.struct_field_index(struct_ty, name)?;
                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, ptr, index, "field_ptr")
                    .ok()?;
                Some((field_ptr, struct_ty.get_field_type_at_index(index)?))
            }

            ExpressionKind::Index { left, index } => {
                let (ptr, container) = self.place_of(left)?;
                let usize_type = self.usize_type();
                let unsigned = Self::is_unsigned_expr(index);
                let offset = self
                    .compile_expression(index, Some(usize_type.into()))
                    .into_int_value();

                match container {
                    BasicTypeEnum::ArrayType(array_ty) => {
                        self.emit_bounds_check(offset, array_ty.len() as u64, unsigned);
                        let elem_ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    array_ty,
                                    ptr,
                                    &[usize_type.const_zero(), offset],
                                    "elem_ptr",
                                )
                                .ok()?
                        };
                        Some((elem_ptr, array_ty.get_element_type()))
                    }

                    // A Vec and a slice both keep their elements elsewhere, and
                    // how many there are is only known once it runs.
                    BasicTypeEnum::StructType(shape)
                        if self.is_vec_layout(shape) || self.is_slice_layout(shape) =>
                    {
                        let (data_at, len_at) = if self.is_vec_layout(shape) {
                            (VEC_PTR, VEC_LEN)
                        } else {
                            (SLICE_PTR, SLICE_LEN)
                        };
                        let elem_type = self.element_type_of(left);
                        let ptr_type = self.ptr_type();
                        let data_field = self.field_ptr(shape, ptr, data_at, "data_field");
                        let data = self.load(ptr_type, data_field, "data").into_pointer_value();
                        let len_field = self.field_ptr(shape, ptr, len_at, "len_field");
                        let len = self.load_int(usize_type, len_field, "len");
                        self.emit_bounds_check_against(offset, len, unsigned);
                        Some((self.vec_elem_ptr(data, offset, elem_type), elem_type))
                    }

                    _ => None,
                }
            }

            ExpressionKind::Dereference(inner) => {
                // The pointee type sets the width of a store through this lvalue.
                // Assuming i64 makes `*p = x` on a `*i32` write eight bytes.
                let elem_type = self
                    .pointee_type_of(inner)
                    .unwrap_or_else(|| self.usize_type().into());

                let BasicValueEnum::PointerValue(ptr) = self.compile_expression(inner, None) else {
                    return None;
                };
                self.emit_null_check(ptr, "null pointer dereference in assignment");
                Some((ptr, elem_type))
            }

            _ => None,
        }
    }

    /// Where `expr` lives. A temporary has no place of its own, so it is spilled
    /// to a slot; the analyser already refuses to write to one, so this only
    /// serves reads like `f()[0]` and `f().field`.
    fn place_of(&mut self, expr: &Expression) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        if let Some(place) = self.compile_lvalue(expr) {
            return Some(place);
        }

        let function = self.current_fn?;
        let value = self.compile_expression(expr, None);
        let slot = self.create_entry_block_alloca(function, "temp", value.get_type());
        self.builder.build_store(slot, value).unwrap();
        Some((slot, value.get_type()))
    }

    /// Where the struct `expr` denotes lives, following one pointer hop so
    /// `p.field` and `self.field` read through the pointer.
    fn struct_place(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, StructType<'ctx>)> {
        match self.place_of(expr)? {
            (ptr, BasicTypeEnum::StructType(st)) => Some((ptr, st)),
            (ptr, BasicTypeEnum::PointerType(_)) => {
                let BasicTypeEnum::StructType(st) = self.pointee_type_of(expr)? else {
                    return None;
                };
                let ptr_type = self.ptr_type();
                Some((self.load(ptr_type, ptr, "deref").into_pointer_value(), st))
            }
            _ => None,
        }
    }

    /// Index of field `name` within a named user struct.
    fn struct_field_index(&self, struct_ty: StructType<'ctx>, name: &str) -> Option<u32> {
        let struct_name = struct_ty.get_name()?.to_str().ok()?;
        self.struct_defs.get(struct_name)?.1.get(name).copied()
    }

    /// What `expr` points at, from the type the analyser resolved. A computed
    /// pointer such as `p + 1` has no name to look up, and guessing a width
    /// there reads the wrong number of bytes.
    fn pointee_type_of(&self, expr: &Expression) -> Option<BasicTypeEnum<'ctx>> {
        if let Some(Type::Pointer(pointee) | Type::Ref(pointee) | Type::RefMut(pointee)) = &expr.ty
        {
            return self.llvm_type_of(pointee);
        }

        match &expr.kind {
            ExpressionKind::Identifier(name) => self.pointer_elem_types.get(name).copied(),
            _ => None,
        }
    }

    /// Lower an expression, wrapping the result in `Some(_)` when the context
    /// wants a `T?` but the expression produced a bare `T`.
    pub(super) fn compile_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let result = self.compile_expression_inner(expr, expected_type);

        if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type
            && self.is_option_layout(opt_type)
            && !matches!(result, BasicValueEnum::StructValue(s) if s.get_type() == opt_type)
        {
            return self.build_option_some(result).into();
        }

        result
    }

    fn compile_expression_inner(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        // Under a `T?` context the sub-expression is typed as the payload `T`.
        let inner_expected = match expected_type {
            Some(BasicTypeEnum::StructType(opt_type)) if self.is_option_layout(opt_type) => {
                opt_type.get_field_type_at_index(OPTION_VALUE)
            }
            other => other,
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
                    _ => self.context.f64_type(),
                };

                float_type.const_float(*val).into()
            }
            ExpressionKind::Identifier(name) => self.lower_identifier(name, expr.span),
            ExpressionKind::Get { .. } | ExpressionKind::Index { .. } => {
                self.lower_place_read(expr)
            }
            ExpressionKind::StructLiteral { name, fields } => {
                self.lower_struct_literal(name, fields, expr.span)
            }
            ExpressionKind::ArrayLiteral(elements) => {
                self.lower_array_literal(elements, expected_type, expr.span)
            }
            ExpressionKind::Assign {
                target,
                operator,
                value,
            } => self.lower_assign(target, operator, value, expr.span),
            ExpressionKind::Call {
                function,
                arguments,
            } => self.lower_call(function, arguments, expected_type, expr),
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
                match (operator, operand) {
                    (Token::Minus, BasicValueEnum::IntValue(v)) => {
                        self.builder.build_int_neg(v, "neg").unwrap().into()
                    }
                    (Token::Minus, BasicValueEnum::FloatValue(v)) => {
                        self.builder.build_float_neg(v, "fneg").unwrap().into()
                    }
                    (Token::Bang, BasicValueEnum::IntValue(v)) => {
                        self.builder.build_not(v, "not").unwrap().into()
                    }
                    _ => self.unsupported_operator(operator, expr.span),
                }
            }

            ExpressionKind::Cast { left, target } => self.lower_cast(left, target, expr.span),

            // `&x`, `&var x` and `ptr(x)` all lower to the address of the lvalue.
            ExpressionKind::AddressOf(inner)
            | ExpressionKind::BorrowRef(inner)
            | ExpressionKind::BorrowRefMut(inner) => match self.compile_lvalue(inner) {
                Some((ptr, _)) => ptr.into(),
                None => {
                    self.error("Cannot take the address of a temporary", inner.span);
                    self.dummy_val()
                }
            },

            ExpressionKind::Dereference(inner) => {
                let pointee = self.pointee_type_of(inner);
                let BasicValueEnum::PointerValue(ptr) = self.compile_expression(inner, None) else {
                    self.error("Cannot dereference a non-pointer value", inner.span);
                    return self.dummy_val();
                };

                self.emit_null_check(ptr, "null pointer dereference");
                let load_type = expected_type
                    .or(pointee)
                    .unwrap_or_else(|| self.usize_type().into());
                self.load(load_type, ptr, "deref")
            }

            ExpressionKind::Tuple(elements) => {
                let values: Vec<_> = elements
                    .iter()
                    .map(|elem| self.compile_expression(elem, None))
                    .collect();
                let field_types: Vec<_> = values.iter().map(|v| v.get_type()).collect();
                let tuple_type = self.context.struct_type(&field_types, false);

                self.build_struct(tuple_type, &values, "tuple").into()
            }
            ExpressionKind::Match { value, arms } => self.lower_match(value, arms, expected_type),
            ExpressionKind::None => {
                if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type
                    && let Some(inner_type) = opt_type.get_field_type_at_index(OPTION_VALUE)
                {
                    self.build_option_none(inner_type).into()
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

    fn lower_identifier(&mut self, name: &str, span: Span) -> BasicValueEnum<'ctx> {
        if let Some(const_val) = self.constants.get(name) {
            return *const_val;
        }
        if let Some((ptr, ty, _)) = self.variables.get(name) {
            return self.load(*ty, *ptr, &format!("{name}_load"));
        }
        // The parser folds `Enum::Variant` into one qualified identifier.
        if let Some(tag) = self.enum_variant_tag(name) {
            return tag;
        }

        self.error(format!("Unknown identifier '{name}'"), span);
        self.dummy_val()
    }

    /// Read a field or element, as an enum tag or through its storage.
    fn lower_place_read(&mut self, expr: &Expression) -> BasicValueEnum<'ctx> {
        if let ExpressionKind::Get { object, name } = &expr.kind
            && let ExpressionKind::Identifier(enum_name) = &object.kind
            && let Some(tag) = self.enum_variant_tag(&format!("{enum_name}::{name}"))
        {
            return tag;
        }

        if let Some((ptr, ty)) = self.compile_lvalue(expr) {
            let label = match &expr.kind {
                ExpressionKind::Get { name, .. } => name.as_str(),
                _ => "elem",
            };
            return self.load(ty, ptr, &format!("{label}_load"));
        }

        self.error(format!("Cannot read {:?}", expr.kind), expr.span);
        self.dummy_val()
    }

    fn lower_assign(
        &mut self,
        target: &Expression,
        operator: &Token,
        value: &Expression,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let Some((ptr, ty)) = self.compile_lvalue(target) else {
            self.error("Invalid assignment target", target.span);
            return self.dummy_val();
        };

        let stored = if *operator == Token::Assign {
            self.compile_expression(value, Some(ty))
        } else {
            let current = self.load(ty, ptr, "cur_val");
            let rhs = self.compile_expression(value, Some(ty));
            let signed = self.is_signed_integer(target).unwrap_or(true);
            self.apply_compound_op(current, rhs, operator, signed, span)
        };

        self.builder.build_store(ptr, stored).unwrap();
        stored
    }

    fn lower_struct_literal(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let Some(struct_ty) = self.struct_defs.get(name).map(|(st, _)| *st) else {
            self.error(format!("Unknown struct type '{name}'"), span);
            return self.dummy_val();
        };

        let mut struct_val = struct_ty.get_undef();
        for (field_name, field_expr) in fields {
            let Some(index) = self.struct_field_index(struct_ty, field_name) else {
                self.error(
                    format!("Unknown field '{field_name}' in struct '{name}'"),
                    field_expr.span,
                );
                return self.dummy_val();
            };
            let field_type = struct_ty.get_field_type_at_index(index).unwrap();
            let val = self.compile_expression(field_expr, Some(field_type));
            struct_val = self
                .builder
                .build_insert_value(struct_val, val, index, "field")
                .unwrap()
                .into_struct_value();
        }
        struct_val.into()
    }

    fn lower_array_literal(
        &mut self,
        elements: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        // The element type comes from the annotation, or from the first element.
        let annotated = match expected_type {
            Some(BasicTypeEnum::ArrayType(arr_ty)) => Some(arr_ty.get_element_type()),
            _ => None,
        };
        let Some(elem_type) =
            annotated.or_else(|| Some(self.compile_expression(elements.first()?, None).get_type()))
        else {
            self.error("Cannot infer type from an empty array literal", span);
            return self.dummy_val();
        };

        let mut array_val = elem_type.array_type(elements.len() as u32).get_undef();
        for (i, elem) in elements.iter().enumerate() {
            let val = self.compile_expression(elem, Some(elem_type));
            array_val = self
                .builder
                .build_insert_value(array_val, val, i as u32, "elem")
                .unwrap()
                .into_array_value();
        }
        array_val.into()
    }

    /// `x += y` and friends: the plain operator applied to the current value.
    fn apply_compound_op(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        operator: &Token,
        signed: bool,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
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
            _ => return self.unsupported_operator(operator, span),
        };

        self.apply_arith(lhs, rhs, &plain, signed)
            .unwrap_or_else(|| self.unsupported_operator(operator, span))
    }

    /// Apply an arithmetic or bitwise operator to a matching pair of operands.
    /// `None` when the operator does not apply, so callers word their own error.
    fn apply_arith(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        op: &Token,
        signed: bool,
    ) -> Option<BasicValueEnum<'ctx>> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                self.apply_int_arith(l, r, op, signed)
            }
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                let b = self.builder;
                Some(match op {
                    Token::Plus => b.build_float_add(l, r, "fadd").unwrap().into(),
                    Token::Minus => b.build_float_sub(l, r, "fsub").unwrap().into(),
                    Token::Star => b.build_float_mul(l, r, "fmul").unwrap().into(),
                    Token::Slash => b.build_float_div(l, r, "fdiv").unwrap().into(),
                    Token::Mod => b.build_float_rem(l, r, "frem").unwrap().into(),
                    _ => return None,
                })
            }
            _ => None,
        }
    }

    /// The operations that can go wrong emit a check first: `+ - *` trap on
    /// overflow, `/ %` on a zero divisor, and shifts on an oversized amount.
    /// All three are skipped in ReleaseFast.
    fn apply_int_arith(
        &mut self,
        l: IntValue<'ctx>,
        r: IntValue<'ctx>,
        op: &Token,
        signed: bool,
    ) -> Option<BasicValueEnum<'ctx>> {
        match op {
            Token::Plus | Token::Minus | Token::Star if self.safety_mode.emit_safety_checks() => {
                if let Some(checked) = self.build_checked_int_arith(l, r, op, signed) {
                    return Some(checked);
                }
            }
            Token::Slash | Token::Mod => self.emit_division_check(l, r, signed),
            Token::ShiftLeft | Token::ShiftRight => self.emit_shift_check(r),
            _ => {}
        }

        let b = self.builder;
        Some(match op {
            Token::Plus => b.build_int_add(l, r, "add").unwrap().into(),
            Token::Minus => b.build_int_sub(l, r, "sub").unwrap().into(),
            Token::Star => b.build_int_mul(l, r, "mul").unwrap().into(),
            Token::Slash if signed => b.build_int_signed_div(l, r, "div").unwrap().into(),
            Token::Slash => b.build_int_unsigned_div(l, r, "udiv").unwrap().into(),
            Token::Mod if signed => b.build_int_signed_rem(l, r, "rem").unwrap().into(),
            Token::Mod => b.build_int_unsigned_rem(l, r, "urem").unwrap().into(),
            Token::BitAnd => b.build_and(l, r, "and").unwrap().into(),
            Token::BitOr => b.build_or(l, r, "or").unwrap().into(),
            Token::BitXor => b.build_xor(l, r, "xor").unwrap().into(),
            Token::ShiftLeft => b.build_left_shift(l, r, "shl").unwrap().into(),
            Token::ShiftRight => b.build_right_shift(l, r, signed, "shr").unwrap().into(),
            _ => return None,
        })
    }

    /// Resolve the callee (method, builtin, generic instantiation or free
    /// function) and emit the call.
    fn lower_call(
        &mut self,
        function: &Expression,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call: &Expression,
    ) -> BasicValueEnum<'ctx> {
        let span = call.span;
        let (fn_val, implicit_args) = match &function.kind {
            ExpressionKind::Get {
                object,
                name: method_name,
            } => {
                match self.compile_method_call(object, method_name, arguments, expected_type, call)
                {
                    MethodCallOutcome::Done(v) => return v,
                    MethodCallOutcome::Resolved(func, args) => (func, args),
                }
            }
            ExpressionKind::Identifier(name) => match name.as_str() {
                "print" | "println" | "eprint" | "eprintln" => {
                    return self.compile_builtin_print(name, arguments, span);
                }
                "Ok" => return self.compile_ok_constructor(arguments, expected_type, span),
                "Err" => return self.compile_err_constructor(arguments, expected_type, span),
                _ => {
                    let resolved = match self.generic_functions.contains_key(name) {
                        true => self.monomorphize_call(name, arguments),
                        false => self.module.get_function(name),
                    };
                    match resolved {
                        Some(func) => (func, Vec::new()),
                        None => {
                            self.error(format!("Unknown function '{name}'"), span);
                            return self.dummy_val();
                        }
                    }
                }
            },
            _ => {
                self.error("Indirect function calls are not yet supported", span);
                return self.dummy_val();
            }
        };

        // Parameters are read off the function value so each argument is typed
        // by the slot it lands in.
        let param_offset = implicit_args.len() as u32;
        let mut compiled_args: Vec<BasicMetadataValueEnum> = implicit_args;
        for (i, arg) in arguments.iter().enumerate() {
            let expected = fn_val
                .get_nth_param(i as u32 + param_offset)
                .map(|param| param.get_type());
            compiled_args.push(self.compile_expression(arg, expected).into());
        }

        match self
            .builder
            .build_call(fn_val, &compiled_args, "call_res")
            .unwrap()
            .try_as_basic_value()
        {
            ValueKind::Basic(value) => value,
            ValueKind::Instruction(_) => self.dummy_val(),
        }
    }

    /// Resolve a method call: builtin fast paths return a value directly, a
    /// user method resolves to an LLVM function plus its `self` argument.
    fn compile_method_call(
        &mut self,
        object: &Expression,
        method_name: &str,
        arguments: &[Expression],
        expected_type: Option<BasicTypeEnum<'ctx>>,
        call: &Expression,
    ) -> MethodCallOutcome<'ctx> {
        let span = call.span;

        if method_name == "copy" && arguments.is_empty() {
            return MethodCallOutcome::Done(self.compile_expression(object, expected_type));
        }

        if let ExpressionKind::Identifier(type_name) = &object.kind
            && type_name == "Vec"
        {
            // No receiver to ask, so the element type comes from the call's
            // own resolved type.
            let elem_type = self.element_type_of(call);
            return MethodCallOutcome::Done(self.compile_vec_static_method(
                method_name,
                arguments,
                elem_type,
                span,
            ));
        }

        // Mutating methods need the storage, not a loaded copy of the header.
        let elem_type = self.element_type_of(object);
        if let Some(vec_ptr) = self.vec_storage_of(object)
            && let Some(result) =
                self.compile_vec_method_mut(method_name, vec_ptr, arguments, elem_type)
        {
            return MethodCallOutcome::Done(result);
        }

        // Compiled once: `make_point().len()` must not evaluate `make_point()` twice.
        let receiver = self.compile_expression(object, None);

        if let BasicValueEnum::StructValue(sv) = receiver {
            let shape = sv.get_type();

            if self.is_vec_layout(shape)
                && let Some(result) = self.compile_vec_method(method_name, sv)
            {
                return MethodCallOutcome::Done(result);
            }
            if self.is_result_layout(shape)
                && let Some(result) = self.compile_result_method(method_name, sv)
            {
                return MethodCallOutcome::Done(result);
            }
            if self.is_option_layout(shape)
                && let Some(result) = self.compile_option_method(method_name, sv)
            {
                return MethodCallOutcome::Done(result);
            }
            if self.is_slice_layout(shape) && method_name == "len" {
                return MethodCallOutcome::Done(self.extract(sv, SLICE_LEN, "slice_len"));
            }
        }

        let Some(struct_name) = self.receiver_struct_name(object, receiver) else {
            self.error("Method call on non-struct value", span);
            return MethodCallOutcome::Done(self.dummy_val());
        };

        let mangled = format!("{struct_name}::{method_name}");
        let Some(func) = self.module.get_function(&mangled) else {
            self.error(format!("Method '{mangled}' not found"), span);
            return MethodCallOutcome::Done(self.dummy_val());
        };

        // `var self` takes the receiver by pointer, plain `self` by value.
        let takes_self_by_pointer =
            matches!(func.get_nth_param(0), Some(BasicValueEnum::PointerValue(_)));

        let self_arg: Option<BasicMetadataValueEnum> = match (takes_self_by_pointer, receiver) {
            (true, _) => self.self_pointer_for(object).map(Into::into),
            // A pointer receiver is loaded for a by-value `self`.
            (false, BasicValueEnum::PointerValue(_)) => self
                .struct_place(object)
                .map(|(ptr, struct_ty)| self.load(struct_ty, ptr, "self_val").into()),
            (false, value) => Some(value.into()),
        };

        let Some(self_arg) = self_arg else {
            self.error("Method receiver must be a variable", span);
            return MethodCallOutcome::Done(self.dummy_val());
        };

        MethodCallOutcome::Resolved(func, vec![self_arg])
    }

    /// Element type of the `Vec` or slice `expr` denotes, taken from the type
    /// the analyser resolved. Falls back to a word when nothing said otherwise.
    fn element_type_of(&self, expr: &Expression) -> BasicTypeEnum<'ctx> {
        match &expr.ty {
            Some(Type::Vec { elem_type } | Type::Slice { elem_type }) => {
                self.llvm_type_of(elem_type)
            }
            _ => None,
        }
        .unwrap_or_else(|| self.usize_type().into())
    }

    /// Address of one field of an aggregate that lives at `ptr`.
    fn field_ptr(
        &self,
        shape: StructType<'ctx>,
        ptr: PointerValue<'ctx>,
        field: u32,
        name: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_struct_gep(shape, ptr, field, name)
            .unwrap()
    }

    /// Storage behind `object` when it holds a builtin `Vec`. A temporary has
    /// no storage, so a mutating method has nothing to reach through.
    fn vec_storage_of(&mut self, object: &Expression) -> Option<PointerValue<'ctx>> {
        let (ptr, ty) = self.compile_lvalue(object)?;
        match ty {
            BasicTypeEnum::StructType(st) if self.is_vec_layout(st) => Some(ptr),
            _ => None,
        }
    }

    /// Name of the user struct a call dispatches on, looking through a pointer
    /// receiver.
    fn receiver_struct_name(
        &self,
        object: &Expression,
        receiver: BasicValueEnum<'ctx>,
    ) -> Option<String> {
        let named = |ty: &BasicTypeEnum<'ctx>| match ty {
            BasicTypeEnum::StructType(st) => Some(st.get_name()?.to_str().ok()?.to_string()),
            _ => None,
        };

        if let ExpressionKind::Identifier(var_name) = &object.kind
            && let Some((_, ty, _)) = self.variables.get(var_name)
        {
            return match ty {
                BasicTypeEnum::PointerType(_) => named(self.pointer_elem_types.get(var_name)?),
                _ => named(ty),
            };
        }

        named(&receiver.get_type())
    }

    /// Pointer to pass as `self` to a `var self` method. A receiver that is
    /// itself a pointer (a forwarded `self`) is loaded first.
    fn self_pointer_for(&mut self, object: &Expression) -> Option<PointerValue<'ctx>> {
        let ExpressionKind::Identifier(var_name) = &object.kind else {
            return None;
        };
        let (ptr, _, _) = *self.variables.get(var_name)?;

        if self.pointer_elem_types.contains_key(var_name) {
            let ptr_type = self.ptr_type();
            return Some(self.load(ptr_type, ptr, "self_loaded").into_pointer_value());
        }
        Some(ptr)
    }

    /// Dispatch a binary operator on the operand category (integer, float,
    /// pointer arithmetic, pointer comparison), plus `::`, `&&` and `||`.
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
            if let Some(tag) = self.enum_variant_tag(&format!("{enum_name}::{variant_name}")) {
                return tag;
            }

            self.error("Invalid '::' expression", expr.span);
            return self.dummy_val();
        }

        if matches!(operator, Token::And | Token::Or) {
            return self.lower_short_circuit(left, operator, right, expr.span);
        }

        let comparison = Self::compare_predicates(operator);
        // A comparison's operands carry their own type, not the boolean result.
        let operand_hint = if comparison.is_some() {
            None
        } else {
            expected_type
        };
        let lhs = self.compile_expression(left, operand_hint);
        let rhs = self.compile_expression(right, Some(lhs.get_type()));

        match (lhs, rhs) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                if let Some((signed, unsigned, _)) = comparison {
                    let pred = if Self::is_unsigned_expr(left) {
                        unsigned
                    } else {
                        signed
                    };
                    return self
                        .builder
                        .build_int_compare(pred, l, r, "cmp")
                        .unwrap()
                        .into();
                }

                // Shifts follow the left operand, everything else the result type.
                let source = if *operator == Token::ShiftRight {
                    left
                } else {
                    expr
                };
                let signed = self.is_signed_integer(source).unwrap_or(true);
                self.apply_arith(l.into(), r.into(), operator, signed)
                    .unwrap_or_else(|| self.unsupported_operator(operator, expr.span))
            }

            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                if let Some((_, _, pred)) = comparison {
                    return self
                        .builder
                        .build_float_compare(pred, l, r, "fcmp")
                        .unwrap()
                        .into();
                }
                self.apply_arith(l.into(), r.into(), operator, true)
                    .unwrap_or_else(|| self.unsupported_operator(operator, expr.span))
            }

            (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(offset)) => {
                let step = match operator {
                    Token::Plus => offset,
                    Token::Minus => self.builder.build_int_neg(offset, "neg").unwrap(),
                    _ => {
                        self.error("Pointer arithmetic supports only '+' and '-'", expr.span);
                        return self.dummy_val();
                    }
                };
                // A step is one element wide, as in C: `p + 1` on a *i32 moves
                // four bytes. Without the pointee type it would move one, and
                // land inside the element it started on.
                let elem = self
                    .pointee_type_of(left)
                    .unwrap_or_else(|| self.context.i8_type().into());
                unsafe {
                    self.builder
                        .build_gep(elem, ptr, &[step], "ptr")
                        .unwrap()
                        .into()
                }
            }

            (BasicValueEnum::PointerValue(l), BasicValueEnum::PointerValue(r)) => {
                let Some((_, pred, _)) = comparison else {
                    self.error("Pointers support only comparison operators", expr.span);
                    return self.dummy_val();
                };
                let usize_type = self.usize_type();
                let l_int = self
                    .builder
                    .build_ptr_to_int(l, usize_type, "ptr_l")
                    .unwrap();
                let r_int = self
                    .builder
                    .build_ptr_to_int(r, usize_type, "ptr_r")
                    .unwrap();
                self.builder
                    .build_int_compare(pred, l_int, r_int, "ptr_cmp")
                    .unwrap()
                    .into()
            }

            _ => {
                self.error("Type mismatch in binary operation", expr.span);
                self.dummy_val()
            }
        }
    }

    /// LLVM predicates for a comparison token, as `(signed, unsigned, float)`.
    fn compare_predicates(op: &Token) -> Option<(IntPredicate, IntPredicate, FloatPredicate)> {
        use FloatPredicate as F;
        use IntPredicate as I;
        Some(match op {
            Token::Eq => (I::EQ, I::EQ, F::OEQ),
            Token::NotEq => (I::NE, I::NE, F::ONE),
            Token::Lt => (I::SLT, I::ULT, F::OLT),
            Token::Leq => (I::SLE, I::ULE, F::OLE),
            Token::Gt => (I::SGT, I::UGT, F::OGT),
            Token::Geq => (I::SGE, I::UGE, F::OGE),
            _ => return None,
        })
    }

    fn unsupported_operator(&mut self, operator: &Token, span: Span) -> BasicValueEnum<'ctx> {
        self.error(format!("Operator '{operator:?}' is not implemented"), span);
        self.dummy_val()
    }

    /// Lower `&&`/`||` as a branch plus a phi, leaving the builder at the merge.
    fn lower_short_circuit(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let bool_type = self.context.bool_type();
        let Some(current_fn) = self.current_fn else {
            return self.dummy_val();
        };
        let is_and = *operator == Token::And;

        let lhs = self.truthy(left, span);
        let entry_block = self.builder.get_insert_block().unwrap();
        let rhs_block = self.context.append_basic_block(current_fn, "rhs_eval");
        let merge_block = self.context.append_basic_block(current_fn, "merge");

        // `&&` only needs the right side when the left is true, `||` when false.
        let (on_true, on_false) = if is_and {
            (rhs_block, merge_block)
        } else {
            (merge_block, rhs_block)
        };
        self.builder
            .build_conditional_branch(lhs, on_true, on_false)
            .unwrap();

        self.builder.position_at_end(rhs_block);
        let rhs = self.truthy(right, span);
        let rhs_end_block = self.builder.get_insert_block().unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "result").unwrap();
        let short_circuit_value = if is_and {
            bool_type.const_zero()
        } else {
            bool_type.const_all_ones()
        };
        phi.add_incoming(&[(&short_circuit_value, entry_block), (&rhs, rhs_end_block)]);

        phi.as_basic_value()
    }

    /// Lower `expr` to an `i1`, comparing wider integers against zero.
    fn truthy(&mut self, expr: &Expression, span: Span) -> IntValue<'ctx> {
        let bool_type = self.context.bool_type();
        let BasicValueEnum::IntValue(v) = self.compile_expression(expr, Some(bool_type.into()))
        else {
            self.error("'&&' and '||' require boolean or integer operands", span);
            return bool_type.const_zero();
        };

        if v.get_type().get_bit_width() == 1 {
            return v;
        }
        self.builder
            .build_int_compare(IntPredicate::NE, v, v.get_type().const_zero(), "tobool")
            .unwrap()
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

        // An i1 must never sign-extend, or `true as i32` would come out as -1.
        let src_signed =
            |v: IntValue<'ctx>, signed: bool| v.get_type().get_bit_width() > 1 && signed;
        let left_signed = self.is_signed_integer(left).unwrap_or(true);
        let f32_type = self.context.f32_type();

        match (src_val, target_type) {
            (BasicValueEnum::IntValue(v), BasicTypeEnum::IntType(t)) => {
                let (src_bits, dst_bits) = (v.get_type().get_bit_width(), t.get_bit_width());
                // Widening follows the source signedness: always zero-extending
                // would turn `-1 as i64` into 4294967295.
                match (src_bits.cmp(&dst_bits), src_signed(v, left_signed)) {
                    (std::cmp::Ordering::Equal, _) => v.into(),
                    (std::cmp::Ordering::Less, true) => self
                        .builder
                        .build_int_s_extend(v, t, "sexttmp")
                        .unwrap()
                        .into(),
                    (std::cmp::Ordering::Less, false) => self
                        .builder
                        .build_int_z_extend(v, t, "zexttmp")
                        .unwrap()
                        .into(),
                    (std::cmp::Ordering::Greater, _) => self
                        .builder
                        .build_int_truncate(v, t, "trunctmp")
                        .unwrap()
                        .into(),
                }
            }
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::IntType(t)) => {
                if Self::is_unsigned_type(&target_typespec) {
                    self.builder
                        .build_float_to_unsigned_int(v, t, "fptoui")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_float_to_signed_int(v, t, "fptosi")
                        .unwrap()
                        .into()
                }
            }
            (BasicValueEnum::IntValue(v), BasicTypeEnum::FloatType(t)) => {
                if src_signed(v, left_signed) {
                    self.builder
                        .build_signed_int_to_float(v, t, "sitofp")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_unsigned_int_to_float(v, t, "uitofp")
                        .unwrap()
                        .into()
                }
            }
            (BasicValueEnum::FloatValue(v), BasicTypeEnum::FloatType(t)) => {
                match (v.get_type() == t, v.get_type() == f32_type) {
                    (true, _) => v.into(),
                    (false, true) => self.builder.build_float_ext(v, t, "fext").unwrap().into(),
                    (false, false) => self
                        .builder
                        .build_float_trunc(v, t, "ftrunc")
                        .unwrap()
                        .into(),
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
            // A no-op with opaque pointers.
            (BasicValueEnum::PointerValue(v), BasicTypeEnum::PointerType(_)) => v.into(),
            // `str`/slice keeps its data pointer and drops the length.
            (BasicValueEnum::StructValue(v), BasicTypeEnum::PointerType(_))
                if self.is_slice_layout(v.get_type()) =>
            {
                self.extract(v, SLICE_PTR, "str_ptr")
            }
            (BasicValueEnum::StructValue(v), BasicTypeEnum::IntType(t))
                if self.is_slice_layout(v.get_type()) =>
            {
                let ptr = self.extract(v, SLICE_PTR, "str_ptr").into_pointer_value();
                self.builder
                    .build_ptr_to_int(ptr, t, "str_ptrtoint")
                    .unwrap()
                    .into()
            }
            _ => {
                self.error("Unsupported cast combination", span);
                self.dummy_val()
            }
        }
    }

    /// Lower a `match` into a `switch` over the arm blocks plus a result phi.
    fn lower_match(
        &mut self,
        value: &Expression,
        arms: &[(Expression, Expression)],
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let Some(parent_fn) = self.current_fn else {
            return self.dummy_val();
        };
        let BasicValueEnum::IntValue(subject) = self.compile_expression(value, None) else {
            self.error("'match' requires an integer or enum value", value.span);
            return self.dummy_val();
        };

        let merge_bb = self.context.append_basic_block(parent_fn, "match_merge");

        let mut cases = Vec::with_capacity(arms.len());
        let mut default_bb = None;
        let mut arm_bodies = Vec::with_capacity(arms.len());

        for (pattern, result) in arms {
            let block = self.context.append_basic_block(parent_fn, "match_arm");
            arm_bodies.push((block, result));

            if Self::is_default_pattern(pattern) {
                default_bb = Some(block);
            } else if let BasicValueEnum::IntValue(tag) = self.compile_expression(pattern, None) {
                cases.push((tag, block));
            } else {
                self.error(
                    "'match' patterns must be integers or enum variants",
                    pattern.span,
                );
            }
        }

        // The switch goes in whichever block the patterns left behind, captured
        // before the synthesised default moves the builder.
        let switch_bb = self.builder.get_insert_block().unwrap();

        // An exhaustive match has no `default` arm, so one is synthesised for the
        // switch to fall back to.
        let default_bb = default_bb.unwrap_or_else(|| {
            let bb = self.context.append_basic_block(parent_fn, "match_default");
            self.builder.position_at_end(bb);
            self.builder.build_unreachable().unwrap();
            bb
        });

        self.builder.position_at_end(switch_bb);
        self.builder
            .build_switch(subject, default_bb, &cases)
            .unwrap();

        // Only arms that fall through to the merge block feed the phi; one that
        // returns or breaks is not a predecessor.
        let mut incoming: Vec<(BasicBlock<'ctx>, BasicValueEnum<'ctx>)> = Vec::new();
        for (block, result) in arm_bodies {
            self.builder.position_at_end(block);
            let value = self.compile_expression(result, expected_type);
            let end_bb = self.builder.get_insert_block().unwrap();
            if end_bb.get_terminator().is_none() {
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                incoming.push((end_bb, value));
            }
        }

        self.builder.position_at_end(merge_bb);

        let Some((_, first)) = incoming.first() else {
            self.builder.build_unreachable().unwrap();
            return self.dummy_val();
        };

        let phi = self
            .builder
            .build_phi(first.get_type(), "match_result")
            .unwrap();
        for (block, value) in &incoming {
            phi.add_incoming(&[(value, *block)]);
        }
        phi.as_basic_value()
    }

    fn is_default_pattern(pattern: &Expression) -> bool {
        matches!(&pattern.kind, ExpressionKind::Identifier(name) if name == "default")
    }
}
