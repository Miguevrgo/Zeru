//! Statement and expression lowering: control flow, declarations, function
//! bodies, arithmetic, calls, casts and pattern matching.

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    module::Linkage,
    types::{BasicType, BasicTypeEnum, IntType, StructType},
    values::{
        BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue, ValueKind,
    },
};

use crate::{
    ast::{Expression, ExpressionKind, Statement, StatementKind, TypeSpec},
    codegen::{
        compiler::{Compiler, LoopContext, Scope, VarBinding},
        layout::{
            OPTION_TAG, OPTION_VALUE, RESULT_ERR, RESULT_TAG, RESULT_VALUE, SLICE_LEN, SLICE_PTR,
            VEC_LEN, VEC_PTR,
        },
    },
    errors::Span,
    sema::{analyzer::PRINTS, types::Type},
    token::Token,
};

enum MethodCallOutcome<'ctx> {
    Done(BasicValueEnum<'ctx>),
    Resolved(FunctionValue<'ctx>, Vec<BasicMetadataValueEnum<'ctx>>),
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Emit `s` as a global string and pack it into a `{ *u8, usize }` slice.
    fn build_str_slice(&mut self, s: &[u8]) -> BasicValueEnum<'ctx> {
        let data = self.const_bytes(s);
        let len = self.usize_type().const_int(s.len() as u64, false);
        self.build_struct(self.slice_type(), &[data.into(), len.into()], "str_slice")
            .into()
    }

    pub(super) fn compile_fn_prototype(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
    ) -> FunctionValue<'ctx> {
        let types = self.types;
        let (param_types, ret) = types.signature(name).unwrap_or((&[], &Type::Void));

        // `main` gives the process an exit status even when it returns nothing.
        let ret_type = match self.llvm_type_of(ret) {
            None if name == "main" => Some(self.context.i32_type().into()),
            other => other,
        };

        let param_types: Vec<_> = params
            .iter()
            .zip(param_types)
            .filter_map(|((param_name, _, _), ty)| match ty {
                // `self` comes in by pointer: borrowed, not copied, and
                // `var self` writes reach the caller.
                Type::Struct(_) if param_name == "self" => Some(self.ptr_type().into()),
                _ => self.llvm_type_of(ty).map(Into::into),
            })
            .collect();

        let fn_type = match ret_type {
            Some(basic_ty) => basic_ty.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };

        // Only `main` is seen from outside, so LLVM may drop what nothing calls.
        let linkage = (name != "main").then_some(Linkage::Internal);
        self.module.add_function(name, fn_type, linkage)
    }

    pub(super) fn compile_fn_body(
        &mut self,
        name: &str,
        params: &[(String, TypeSpec, bool)],
        body: &[Statement],
        span: Span,
    ) {
        // Missing means the prototype pass already reported why.
        let Some(function) = self.module.get_function(name) else {
            return;
        };
        self.current_fn = Some(function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.enter_debug_scope(function, span);
        self.variables.clear();
        self.scope_stack.clear();
        self.scope_stack.push(Scope::default());

        let types = self.types;
        let param_types = types.signature(name).map_or(&[][..], |(params, _)| params);
        for ((arg, (param_name, _, _)), ty) in
            function.get_param_iter().zip(params).zip(param_types)
        {
            // `self` already points at where its struct lives.
            if let (Type::Struct(_), true) = (ty, param_name == "self")
                && let Some(struct_type) = self.llvm_type_of(ty)
            {
                self.variables
                    .insert(param_name.clone(), (arg.into_pointer_value(), struct_type));
                continue;
            }
            let slot_type = arg.get_type();
            let alloca = self.create_entry_block_alloca(function, param_name, slot_type);
            self.builder.build_store(alloca, arg).unwrap();
            self.variables
                .insert(param_name.clone(), (alloca, slot_type));
            // Passed by value, so the callee owns it.
            self.own(alloca, ty);
        }

        for stmt in body {
            self.compile_statement(stmt);
        }

        if !self.block_is_open() {
            return;
        }
        self.drop_scopes_from(0);

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
        let outer_span = std::mem::replace(&mut self.current_span, stmt.span);
        self.set_debug_location();
        self.lower_statement(stmt);
        self.drop_temporaries();
        self.current_span = outer_span;
        self.set_debug_location();
    }

    fn lower_statement(&mut self, stmt: &Statement) {
        let Some(parent_fn) = self.current_fn else {
            return;
        };

        match &stmt.kind {
            StatementKind::Var {
                name, value, ty, ..
            } => self.declare_variable(parent_fn, name, value, ty.as_ref()),

            StatementKind::Return(Some(expr)) => {
                let ret_hint = parent_fn.get_type().get_return_type();
                let val = self.compile_expression(expr, ret_hint);
                self.drop_all_owned();
                self.builder.build_return(Some(&val)).unwrap();
            }
            // `main` returns an implicit exit status even on a bare `return`.
            StatementKind::Return(None) => {
                self.drop_scopes_from(0);
                if parent_fn.get_name().to_str() == Ok("main") {
                    let zero = self.context.i32_type().const_zero();
                    self.builder.build_return(Some(&zero)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }

            StatementKind::Expression(expr) => {
                let value = self.compile_expression(expr, None);
                // A value nobody takes is dropped with the statement.
                if expr.ty.as_ref().is_some_and(|ty| self.types.owns_heap(ty)) {
                    self.adopt_temporary(value, expr.ty.as_ref());
                }
            }

            StatementKind::Block(stmts) => {
                self.scope_stack.push(Scope::default());
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
                self.drop_scopes_from(ctx.scope_depth);
                self.builder.build_unconditional_branch(target).unwrap();
            }

            // Accepted by the parser and analyser but never lowered: skipping it
            // would emit a binary that quietly does less than the source says.
            _ => self.error(
                "A declaration inside a function body is not supported",
                stmt.span,
            ),
        }
    }

    fn declare_variable(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        name: &str,
        value: &Expression,
        ty: Option<&Type>,
    ) {
        let declared = ty.and_then(|ty| self.llvm_type_of(ty));
        let initial = self.compile_expression(value, declared);
        let slot_type = declared.unwrap_or_else(|| initial.get_type());
        let alloca = self.create_entry_block_alloca(parent_fn, name, slot_type);
        self.builder.build_store(alloca, initial).unwrap();

        self.bind_variable(name, (alloca, slot_type));
        if let Some(ty) = ty {
            self.own(alloca, ty);
        }
    }

    /// Bind `name` in the innermost scope, remembering what it shadowed.
    fn bind_variable(&mut self, name: &str, binding: VarBinding<'ctx>) {
        let shadowed = self.variables.insert(name.to_string(), binding);
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.shadowed.push((name.to_string(), shadowed));
        }
    }

    /// Close the innermost scope: drop what it owns, and put back whatever
    /// each name meant outside it.
    fn pop_scope(&mut self) {
        self.drop_scopes_from(self.scope_stack.len().saturating_sub(1));
        let Some(scope) = self.scope_stack.pop() else {
            return;
        };
        for (name, shadowed) in scope.shadowed.into_iter().rev() {
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

    /// A `for` loop: an index from `first` up to `bound`. Over a range the
    /// index is the loop variable; over an array or a Vec it picks each
    /// turn's element, which the variable is a copy of, or behind `&var`, is.
    fn compile_for_in(
        &mut self,
        parent_fn: FunctionValue<'ctx>,
        variable: &str,
        iterable: &Expression,
        body: &Statement,
    ) {
        let usize_type = self.usize_type();
        let (first, bound, walked) = if let ExpressionKind::Range { start, end } = &iterable.kind {
            let first = self.compile_expression(start, None).into_int_value();
            let bound = self
                .compile_expression(end, Some(first.get_type().into()))
                .into_int_value();
            (first, bound, None)
        } else {
            let (place, by_ref) = match &iterable.kind {
                ExpressionKind::BorrowRefMut(inner) => (inner.as_ref(), true),
                _ => (iterable, false),
            };
            // How many turns is settled before the first one, so pushing
            // inside the body cannot extend the loop.
            let (container, shape, count) = match self.compile_lvalue(place) {
                Some((container, BasicTypeEnum::ArrayType(array))) => {
                    let len = usize_type.const_int(array.len() as u64, false);
                    (container, Some(array), len)
                }
                Some((container, _)) if matches!(place.ty, Some(Type::Vec { .. })) => {
                    let len_field = self.vec_field_ptr(container, VEC_LEN, "len_field");
                    (container, None, self.load_int(usize_type, len_field, "len"))
                }
                _ => {
                    self.error("'for .. in' requires an array or a Vec", iterable.span);
                    return;
                }
            };
            let walked = (container, shape, self.element_type_of(place), by_ref);
            (usize_type.const_zero(), count, Some(walked))
        };

        // Entry-block allocas: a nested loop must not grow the stack per turn.
        let index_type = first.get_type();
        let index_ptr = self.create_entry_block_alloca(parent_fn, "for_index", index_type.into());
        self.builder.build_store(index_ptr, first).unwrap();
        self.scope_stack.push(Scope::default());

        let cond_bb = self.context.append_basic_block(parent_fn, "for_cond");
        let body_bb = self.context.append_basic_block(parent_fn, "for_body");
        let incr_bb = self.context.append_basic_block(parent_fn, "for_incr");
        let after_bb = self.context.append_basic_block(parent_fn, "after_for");

        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);
        let index = self.load_int(index_type, index_ptr, "index");
        let below = match walked.is_none() && !Self::is_unsigned_expr(iterable) {
            true => IntPredicate::SLT,
            false => IntPredicate::ULT,
        };
        let in_range = self
            .builder
            .build_int_compare(below, index, bound, "for_cond")
            .unwrap();
        self.builder
            .build_conditional_branch(in_range, body_bb, after_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        match walked {
            None => self.bind_variable(variable, (index_ptr, index_type.into())),
            Some((container, shape, elem_type, by_ref)) => {
                let element = match shape {
                    Some(array) => unsafe {
                        self.builder
                            .build_in_bounds_gep(
                                array,
                                container,
                                &[usize_type.const_zero(), index],
                                "element",
                            )
                            .unwrap()
                    },
                    // Fetched each turn: a push in the body may have moved it.
                    None => {
                        let data_field = self.vec_field_ptr(container, VEC_PTR, "data_field");
                        let data = self.load(self.ptr_type(), data_field, "data");
                        self.vec_elem_ptr(data.into_pointer_value(), index, elem_type)
                    }
                };
                if by_ref {
                    self.bind_variable(variable, (element, elem_type));
                } else {
                    let slot = self.create_entry_block_alloca(parent_fn, variable, elem_type);
                    let value = self.load(elem_type, element, "element");
                    self.builder.build_store(slot, value).unwrap();
                    self.bind_variable(variable, (slot, elem_type));
                }
            }
        }

        self.compile_loop_body(body, incr_bb, after_bb);
        self.branch_if_open(incr_bb);

        self.builder.position_at_end(incr_bb);
        let next = self
            .builder
            .build_int_add(index, index_type.const_int(1, false), "next_index")
            .unwrap();
        self.builder.build_store(index_ptr, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(after_bb);
        self.pop_scope();
    }

    /// A condition, whose temporaries are done with once it is decided.
    fn compile_bool(&mut self, expr: &Expression) -> IntValue<'ctx> {
        let bool_type = self.context.bool_type();
        let value = self.compile_expression(expr, Some(bool_type.into()));
        self.drop_temporaries();
        match value {
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
            scope_depth: self.scope_stack.len(),
        });
        self.compile_statement(body);
        self.loop_stack.pop();
    }

    pub(super) fn compile_lvalue(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                let (ptr, ty) = self.variables.get(name)?;
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
                    BasicTypeEnum::StructType(shape) => {
                        let (data_at, len_at) = match &left.ty {
                            Some(Type::Vec { .. }) => (VEC_PTR, VEC_LEN),
                            Some(Type::Slice { .. }) => (SLICE_PTR, SLICE_LEN),
                            _ => return None,
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
                self.emit_null_check(ptr);
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

        let value = self.compile_expression(expr, None);
        let slot = self.adopt_temporary(value, expr.ty.as_ref());
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
        // A tuple has no name and no field names: the number is the index.
        if let Ok(index) = name.parse::<u32>() {
            return (index < struct_ty.count_fields()).then_some(index);
        }
        let struct_name = struct_ty.get_name()?.to_str().ok()?;
        let fields = self.types.struct_fields(struct_name);
        fields
            .iter()
            .position(|(field, _)| field == name)
            .map(|at| at as u32)
    }

    /// What `expr` points at, from the type the analyser resolved.
    fn pointee_type_of(&self, expr: &Expression) -> Option<BasicTypeEnum<'ctx>> {
        match &expr.ty {
            Some(Type::Pointer(pointee) | Type::Ref(pointee) | Type::RefMut(pointee)) => {
                self.llvm_type_of(pointee)
            }
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

        if let Some(BasicTypeEnum::StructType(expected)) = expected_type
            && expected == self.option_type(result.get_type())
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
        // The type the analyser settled, which for a literal can come from the
        // other operand of a comparison, something `expected_type` does not carry.
        let settled_type = || expr.ty.as_ref().and_then(|ty| self.llvm_type_of(ty));

        match &expr.kind {
            ExpressionKind::Int(val) => match settled_type() {
                Some(BasicTypeEnum::FloatType(t)) => t.const_float(*val as f64).into(),
                Some(BasicTypeEnum::IntType(t)) => t.const_int(*val, false).into(),
                _ => self.context.i32_type().const_int(*val, false).into(),
            },
            ExpressionKind::Float(val) => {
                let float_type = match settled_type() {
                    Some(BasicTypeEnum::FloatType(t)) => t,
                    _ => self.context.f64_type(),
                };

                float_type.const_float(*val).into()
            }
            ExpressionKind::Identifier(name) => {
                let value = self.lower_identifier(name, expr.span);
                // Given away here, so the variable no longer drops it.
                if self.types.is_moved_at(expr.span)
                    && let Some((slot, _)) = self.variables.get(name)
                {
                    self.release(*slot);
                }
                value
            }
            ExpressionKind::Get { .. } | ExpressionKind::Index { .. } => {
                self.lower_place_read(expr)
            }
            ExpressionKind::StructLiteral { name, fields } => {
                self.lower_struct_literal(name, fields, expr.span)
            }
            ExpressionKind::ArrayLiteral(_) | ExpressionKind::ArrayRepeat { .. }
                if matches!(expr.ty, Some(Type::Vec { .. })) =>
            {
                self.lower_vec_literal(expr)
            }
            ExpressionKind::ArrayLiteral(elements) => {
                self.lower_array_literal(elements, expected_type, expr.span)
            }
            ExpressionKind::ArrayRepeat { value, count } => {
                self.lower_array_repeat(value, *count, expr)
            }
            ExpressionKind::Try(value) => self.lower_try(value),
            ExpressionKind::Block(body) => {
                self.scope_stack.push(Scope::default());
                for statement in body {
                    self.compile_statement(statement);
                }
                self.pop_scope();
                self.dummy_val()
            }
            // Only a `for` loop takes one, and lowers it itself.
            ExpressionKind::Range { .. } => {
                self.error("A range only goes in a 'for' loop", expr.span);
                self.dummy_val()
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

            ExpressionKind::Cast { left, .. } => self.lower_cast(left, expr),

            // `&x` and `&var x` both lower to the address of the lvalue.
            ExpressionKind::BorrowRef(inner) | ExpressionKind::BorrowRefMut(inner) => {
                match self.compile_lvalue(inner) {
                    Some((ptr, _)) => ptr.into(),
                    None => {
                        self.error("Cannot take the address of a temporary", inner.span);
                        self.dummy_val()
                    }
                }
            }

            ExpressionKind::Dereference(inner) => {
                let pointee = self.pointee_type_of(inner);
                let BasicValueEnum::PointerValue(ptr) = self.compile_expression(inner, None) else {
                    self.error("Cannot dereference a non-pointer value", inner.span);
                    return self.dummy_val();
                };

                self.emit_null_check(ptr);
                let load_type = expected_type
                    .or(pointee)
                    .unwrap_or_else(|| self.usize_type().into());
                self.load(load_type, ptr, "deref")
            }

            ExpressionKind::Tuple(elements) => {
                // Each element takes its type from the tuple being built, or an
                // i64 field holding a small literal comes out as an i32.
                let wanted = match expected_type {
                    Some(BasicTypeEnum::StructType(shape))
                        if shape.count_fields() as usize == elements.len() =>
                    {
                        Some(shape)
                    }
                    _ => None,
                };

                let mut values = Vec::with_capacity(elements.len());
                for (at, elem) in elements.iter().enumerate() {
                    let field = wanted.and_then(|shape| shape.get_field_type_at_index(at as u32));
                    values.push(self.compile_expression(elem, field));
                }

                let tuple_type = wanted.unwrap_or_else(|| {
                    let field_types: Vec<_> = values.iter().map(|v| v.get_type()).collect();
                    self.context.struct_type(&field_types, false)
                });
                self.build_struct(tuple_type, &values, "tuple").into()
            }
            ExpressionKind::Match { value, arms } => self.lower_match(value, arms, expected_type),
            ExpressionKind::None => {
                if let Some(BasicTypeEnum::StructType(opt_type)) = settled_type()
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
        // A local first: it may shadow a constant of the same name.
        if let Some((ptr, ty)) = self.variables.get(name) {
            return self.load(*ty, *ptr, &format!("{name}_load"));
        }
        if let Some((value, ty)) = self.constants.get(name).cloned() {
            return self.compile_expression(&value, ty);
        }
        // The parser folds `Enum::Variant` into one qualified identifier.
        if let Some(variant) = self.build_variant(name, &[]) {
            return variant;
        }

        self.error(format!("Unknown identifier '{name}'"), span);
        self.dummy_val()
    }

    /// Read a field or element, as an enum tag or through its storage.
    fn lower_place_read(&mut self, expr: &Expression) -> BasicValueEnum<'ctx> {
        if let Some((ptr, ty)) = self.compile_lvalue(expr) {
            let label = match &expr.kind {
                ExpressionKind::Get { name, .. } => name.as_str(),
                _ => "elem",
            };
            return self.load(ty, ptr, &format!("{label}_load"));
        }

        self.error("Cannot read this expression", expr.span);
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
            let stored = self.compile_expression(value, Some(ty));
            self.drop_overwritten(target, ptr);
            stored
        } else {
            let current = self.load(ty, ptr, "cur_val");
            let rhs = self.compile_expression(value, Some(ty));
            let signed = !Self::is_unsigned_expr(target);
            self.apply_compound_op(current, rhs, operator, signed, span)
        };

        self.builder.build_store(ptr, stored).unwrap();
        stored
    }

    /// Drop the value an assignment is about to replace. A variable may have
    /// given its value away, so its flag decides. A field, an element, or what
    /// `self` or a `for .. in &var` name stands for still holds its own. What
    /// a raw pointer points at may never have been set.
    fn drop_overwritten(&mut self, target: &Expression, ptr: PointerValue<'ctx>) {
        if let Some(owned) = self.owned_at(ptr) {
            self.drop_owned(&owned);
            let raised = self.context.bool_type().const_int(1, false);
            self.builder.build_store(owned.flag, raised).unwrap();
            return;
        }
        if !matches!(target.kind, ExpressionKind::Dereference(_))
            && let Some(ty) = target.ty.as_ref().filter(|ty| self.types.owns_heap(ty))
        {
            self.call_drop(ptr, ty);
        }
    }

    fn lower_struct_literal(
        &mut self,
        name: &str,
        fields: &[(String, Expression)],
        span: Span,
    ) -> BasicValueEnum<'ctx> {
        let Some(BasicTypeEnum::StructType(struct_ty)) =
            self.llvm_type_of(&Type::Struct(name.to_string()))
        else {
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

    /// `[a, b]` or `[value; count]` where a Vec is wanted: a buffer that
    /// fits the elements, filled in place.
    fn lower_vec_literal(&mut self, expr: &Expression) -> BasicValueEnum<'ctx> {
        let elem_type = self.element_type_of(expr);
        let usize_type = self.usize_type();
        let count = match &expr.kind {
            ExpressionKind::ArrayLiteral(elements) => elements.len() as u64,
            ExpressionKind::ArrayRepeat { count, .. } => *count,
            _ => 0,
        };
        let count = usize_type.const_int(count, false);
        let data = self.alloc_buffer(elem_type, count);

        let store = |this: &mut Self, at: IntValue<'ctx>, element: &Expression| {
            let value = this.compile_expression(element, Some(elem_type));
            let slot = this.vec_elem_ptr(data, at, elem_type);
            this.builder.build_store(slot, value).unwrap();
        };
        match &expr.kind {
            ExpressionKind::ArrayLiteral(elements) => {
                for (at, element) in (0..).zip(elements) {
                    store(self, usize_type.const_int(at, false), element);
                }
            }
            ExpressionKind::ArrayRepeat { value, .. } => {
                self.build_counted_loop(count, |this, at| store(this, at, value));
            }
            _ => {}
        }
        self.build_vec(data, count, count)
    }

    /// `[value; count]`: a loop evaluating `value` into each element.
    fn lower_array_repeat(
        &mut self,
        value: &Expression,
        count: u64,
        expr: &Expression,
    ) -> BasicValueEnum<'ctx> {
        let (Some(function), Some(BasicTypeEnum::ArrayType(array_type))) = (
            self.current_fn,
            expr.ty.as_ref().and_then(|ty| self.llvm_type_of(ty)),
        ) else {
            self.error("Cannot lay out this array", expr.span);
            return self.dummy_val();
        };
        let elem_type = array_type.get_element_type();
        let slot = self.create_entry_block_alloca(function, "repeat", array_type.into());

        let usize_type = self.usize_type();
        self.build_counted_loop(usize_type.const_int(count, false), |this, index| {
            let elem = this.compile_expression(value, Some(elem_type));
            let zero = usize_type.const_zero();
            let elem_ptr = unsafe {
                this.builder
                    .build_in_bounds_gep(array_type, slot, &[zero, index], "elem_ptr")
                    .unwrap()
            };
            this.builder.build_store(elem_ptr, elem).unwrap();
        });
        self.load(array_type, slot, "repeat")
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
            Token::PlusWrapEq => Token::PlusWrap,
            Token::MinusWrapEq => Token::MinusWrap,
            Token::StarWrapEq => Token::StarWrap,
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
            // LLVM's own add, sub and mul wrap; only the checks above trap.
            Token::Plus | Token::PlusWrap => b.build_int_add(l, r, "add").unwrap().into(),
            Token::Minus | Token::MinusWrap => b.build_int_sub(l, r, "sub").unwrap().into(),
            Token::Star | Token::StarWrap => b.build_int_mul(l, r, "mul").unwrap().into(),
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
                _ if PRINTS.contains(&name.as_str()) => {
                    return self.compile_builtin_print(name, arguments);
                }
                "Ok" | "Err" => {
                    return self.compile_result_constructor(arguments, call, name == "Ok");
                }
                _ if self.types.variant_fields(name).is_some() => {
                    return self
                        .build_variant(name, arguments)
                        .unwrap_or_else(|| self.dummy_val());
                }
                _ => match self.module.get_function(name) {
                    Some(func) => (func, Vec::new()),
                    None => {
                        self.error(format!("Unknown function '{name}'"), span);
                        return self.dummy_val();
                    }
                },
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
            let copy = match object.ty.as_ref().filter(|ty| self.types.owns_heap(ty)) {
                // What it owns is duplicated too, so the two stay apart.
                Some(ty) => self
                    .place_of(object)
                    .and_then(|(place, _)| self.copy_value(place, ty)),
                None => Some(self.compile_expression(object, expected_type)),
            };
            return MethodCallOutcome::Done(copy.unwrap_or_else(|| self.dummy_val()));
        }

        // `unwrap` takes the payload out, so the receiver is read as a value:
        // a variable gives it away instead of dropping it later.
        if method_name == "unwrap"
            && let Some(ty @ (Type::Optional(_) | Type::Result { .. })) = &object.ty
        {
            let value = self.compile_expression(object, None).into_struct_value();
            let payload = match ty {
                Type::Optional(_) => self.compile_option_method(method_name, value),
                _ => self.compile_result_method(method_name, value),
            };
            return MethodCallOutcome::Done(payload.unwrap_or_else(|| self.dummy_val()));
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

        // Every method works on where its receiver lives. A temporary gets a
        // slot of its own, so `make_point().len()` evaluates it once.
        let Some((place, place_type)) = self.place_of(object) else {
            self.error("Method receiver must be a variable", span);
            return MethodCallOutcome::Done(self.dummy_val());
        };
        let loaded = |this: &mut Self| this.load(place_type, place, "receiver").into_struct_value();

        let builtin = match &object.ty {
            Some(Type::Vec { elem_type }) => {
                self.compile_vec_method(method_name, place, arguments, elem_type)
            }
            Some(Type::Result { .. }) => {
                let result = loaded(self);
                self.compile_result_method(method_name, result)
            }
            Some(Type::Optional(_)) => {
                let option = loaded(self);
                self.compile_option_method(method_name, option)
            }
            Some(Type::Slice { .. }) if method_name == "len" => {
                let slice = loaded(self);
                Some(self.extract(slice, SLICE_LEN, "slice_len"))
            }
            _ => None,
        };
        if let Some(result) = builtin {
            return MethodCallOutcome::Done(result);
        }

        // A user struct, possibly behind a pointer.
        let struct_name = match &object.ty {
            Some(Type::Struct(name)) => name.clone(),
            Some(Type::Pointer(inner) | Type::Ref(inner) | Type::RefMut(inner))
                if matches!(inner.as_ref(), Type::Struct(_)) =>
            {
                inner.to_string()
            }
            _ => {
                self.error("Method call on non-struct value", span);
                return MethodCallOutcome::Done(self.dummy_val());
            }
        };

        let mangled = format!("{struct_name}::{method_name}");
        let Some(func) = self.module.get_function(&mangled) else {
            self.error(format!("Method '{mangled}' not found"), span);
            return MethodCallOutcome::Done(self.dummy_val());
        };

        // Through a pointer, `self` is what it points at.
        let self_ptr = match place_type {
            BasicTypeEnum::PointerType(ptr_type) => {
                self.load(ptr_type, place, "deref").into_pointer_value()
            }
            _ => place,
        };
        MethodCallOutcome::Resolved(func, vec![self_ptr.into()])
    }

    /// Element type of the `Vec` or slice `expr` denotes, taken from the type
    /// the analyser resolved. Falls back to a word when nothing said otherwise.
    fn element_type_of(&self, expr: &Expression) -> BasicTypeEnum<'ctx> {
        match &expr.ty {
            Some(
                Type::Vec { elem_type } | Type::Slice { elem_type } | Type::Array { elem_type, .. },
            ) => self.llvm_type_of(elem_type),
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

    /// Dispatch a binary operator on the operand category (integer, float,
    /// pointer arithmetic, pointer comparison), plus `&&` and `||`.
    fn lower_infix(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
        expr: &Expression,
    ) -> BasicValueEnum<'ctx> {
        if matches!(operator, Token::And | Token::Or) {
            return self.lower_short_circuit(left, operator, right, expr.span);
        }
        if matches!(operator, Token::Catch | Token::Orelse) {
            return self.lower_fallback(left, right);
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
                let signed = !Self::is_unsigned_expr(source);
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
            // Unordered: NaN differs from everything, itself included.
            Token::NotEq => (I::NE, I::NE, F::UNE),
            Token::Lt => (I::SLT, I::ULT, F::OLT),
            Token::Leq => (I::SLE, I::ULE, F::OLE),
            Token::Gt => (I::SGT, I::UGT, F::OGT),
            Token::Geq => (I::SGE, I::UGE, F::OGE),
            _ => return None,
        })
    }

    fn unsupported_operator(&mut self, operator: &Token, span: Span) -> BasicValueEnum<'ctx> {
        self.error(format!("Operator '{operator}' is not implemented"), span);
        self.dummy_val()
    }

    /// `try value`: on an error, return it from the function, dropping what
    /// the function owns; otherwise go on with the payload.
    fn lower_try(&mut self, value: &Expression) -> BasicValueEnum<'ctx> {
        let (Some(function), BasicValueEnum::StructValue(result)) =
            (self.current_fn, self.compile_expression(value, None))
        else {
            return self.dummy_val();
        };
        let Some(BasicTypeEnum::StructType(returned)) = function.get_type().get_return_type()
        else {
            return self.dummy_val();
        };
        let failed_bb = self.context.append_basic_block(function, "try_failed");
        let ok_bb = self.context.append_basic_block(function, "try_ok");
        let is_ok = self.extract(result, RESULT_TAG, "is_ok").into_int_value();
        self.builder
            .build_conditional_branch(is_ok, ok_bb, failed_bb)
            .unwrap();

        self.builder.position_at_end(failed_bb);
        let error = self.extract(result, RESULT_ERR, "error");
        let payload = returned.get_field_type_at_index(RESULT_VALUE).unwrap();
        let failure = self.build_struct(
            returned,
            &[
                self.context.bool_type().const_zero().into(),
                self.zero_value_for(payload),
                error,
            ],
            "failure",
        );
        self.drop_all_owned();
        self.builder.build_return(Some(&failure)).unwrap();

        self.builder.position_at_end(ok_bb);
        self.extract(result, RESULT_VALUE, "payload")
    }

    /// `value catch fallback` or `value orelse fallback`: the payload of a
    /// `T!` or `T?` when it has one, else the fallback, evaluated only then.
    fn lower_fallback(
        &mut self,
        value: &Expression,
        fallback: &Expression,
    ) -> BasicValueEnum<'ctx> {
        let (Some(function), BasicValueEnum::StructValue(held)) =
            (self.current_fn, self.compile_expression(value, None))
        else {
            return self.dummy_val();
        };
        // Both keep the tag first and the payload second.
        let has_payload = self
            .extract(held, OPTION_TAG, "has_payload")
            .into_int_value();
        let payload = self.extract(held, OPTION_VALUE, "payload");
        let start_bb = self.builder.get_insert_block().unwrap();
        let fallback_bb = self.context.append_basic_block(function, "fallback");
        let merge_bb = self.context.append_basic_block(function, "fallback_merge");
        self.builder
            .build_conditional_branch(has_payload, merge_bb, fallback_bb)
            .unwrap();

        self.builder.position_at_end(fallback_bb);
        let replacement = self.compile_expression(fallback, Some(payload.get_type()));
        let fallback_end = self.builder.get_insert_block().unwrap();
        let falls_through = self.block_is_open();
        self.branch_if_open(merge_bb);

        self.builder.position_at_end(merge_bb);
        let phi = self.builder.build_phi(payload.get_type(), "or").unwrap();
        phi.add_incoming(&[(&payload, start_bb)]);
        if falls_through {
            phi.add_incoming(&[(&replacement, fallback_end)]);
        }
        phi.as_basic_value()
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
    fn lower_cast(&mut self, left: &Expression, cast: &Expression) -> BasicValueEnum<'ctx> {
        let span = cast.span;
        let src_val = self.compile_expression(left, None);

        let Some(target_type) = cast.ty.as_ref().and_then(|ty| self.llvm_type_of(ty)) else {
            self.error("Cast to void type is not allowed", span);
            return self.dummy_val();
        };

        // An i1 must never sign-extend, or `true as i32` would come out as -1.
        let src_signed =
            |v: IntValue<'ctx>, signed: bool| v.get_type().get_bit_width() > 1 && signed;
        let left_signed = !Self::is_unsigned_expr(left);
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
                if Self::is_unsigned_expr(cast) {
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
            // A `str` or other slice, the only aggregate the analyser lets
            // through, keeps its data pointer and drops the length.
            (BasicValueEnum::StructValue(v), BasicTypeEnum::PointerType(_)) => {
                self.extract(v, SLICE_PTR, "str_ptr")
            }
            (BasicValueEnum::StructValue(v), BasicTypeEnum::IntType(t)) => {
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
        // Where the subject lives, for arms that bind what it holds; a
        // `T?`, a `T!` or an enum carrying values is told apart by its tag.
        let subject_type = value.ty.clone().unwrap_or(Type::Unknown);
        let Some((place, shape)) = self.place_of(value) else {
            return self.dummy_val();
        };
        let tag = match self.load(shape, place, "subject") {
            BasicValueEnum::IntValue(tag) => tag,
            BasicValueEnum::StructValue(held) => self.extract(held, 0, "tag").into_int_value(),
            _ => {
                self.error(
                    "'match' requires an integer, an enum, a T? or a T!",
                    value.span,
                );
                return self.dummy_val();
            }
        };

        let merge_bb = self.context.append_basic_block(parent_fn, "match_merge");

        let mut cases = Vec::with_capacity(arms.len());
        let mut default_bb = None;
        let mut arm_bodies = Vec::with_capacity(arms.len());

        for (pattern, result) in arms {
            let block = self.context.append_basic_block(parent_fn, "match_arm");
            arm_bodies.push((block, pattern, result));

            if pattern.is_default_pattern() {
                default_bb = Some(block);
            } else if let Some(case) = self.pattern_tag(pattern, tag.get_type()) {
                cases.push((case, block));
            } else {
                self.error("This pattern has no constant value", pattern.span);
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
        self.builder.build_switch(tag, default_bb, &cases).unwrap();

        // Only arms that fall through to the merge block feed the phi; one that
        // returns or breaks is not a predecessor.
        let mut incoming: Vec<(BasicBlock<'ctx>, BasicValueEnum<'ctx>)> = Vec::new();
        for (block, pattern, result) in arm_bodies {
            self.builder.position_at_end(block);
            self.scope_stack.push(Scope::default());
            self.bind_pattern(pattern, &subject_type, place, shape);
            let value = self.compile_expression(result, expected_type);
            self.pop_scope();
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

    /// The tag an arm's pattern switches on: 1 for `Some` and `Ok`, 0 for
    /// `None` and `Err`, a variant's position, or a constant's value.
    fn pattern_tag(
        &mut self,
        pattern: &Expression,
        tag_type: IntType<'ctx>,
    ) -> Option<IntValue<'ctx>> {
        let path = match &pattern.kind {
            ExpressionKind::None => return Some(tag_type.const_zero()),
            ExpressionKind::Call { function, .. } => match &function.kind {
                ExpressionKind::Identifier(path) => path.as_str(),
                _ => return None,
            },
            ExpressionKind::Identifier(path) if self.variant_index(path).is_some() => path.as_str(),
            // A literal or a constant, as the analyser worked it out.
            _ => {
                let value = self.types.constant_value(pattern)?;
                return Some(tag_type.const_int(value as u64, true));
            }
        };
        let tag = match path {
            "Some" | "Ok" => 1,
            "Err" => 0,
            path => self.variant_index(path)?,
        };
        Some(tag_type.const_int(tag, false))
    }

    /// Bind the names a payload pattern gives, each to where its value sits
    /// inside the subject: a view of it, which the analyser keeps unchanged.
    fn bind_pattern(
        &mut self,
        pattern: &Expression,
        subject_type: &Type,
        place: PointerValue<'ctx>,
        shape: BasicTypeEnum<'ctx>,
    ) {
        let (
            ExpressionKind::Call {
                function,
                arguments,
            },
            BasicTypeEnum::StructType(shape),
        ) = (&pattern.kind, shape)
        else {
            return;
        };
        let ExpressionKind::Identifier(path) = &function.kind else {
            return;
        };
        let field_at = |this: &Self, at: u32, ty: &Type| {
            let ptr = this.field_ptr(shape, place, at, "bound");
            this.llvm_type_of(ty).map(|ty| (ptr, ty))
        };
        let values: Vec<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> =
            match (path.as_str(), subject_type) {
                ("Some", Type::Optional(inner)) => {
                    field_at(self, OPTION_VALUE, inner).into_iter().collect()
                }
                ("Ok", Type::Result { ok_type, .. }) => {
                    field_at(self, RESULT_VALUE, ok_type).into_iter().collect()
                }
                ("Err", Type::Result { err_type, .. }) => {
                    field_at(self, RESULT_ERR, err_type).into_iter().collect()
                }
                (path, Type::Enum(_)) => {
                    let Some((_, fields)) = self.types.variant_fields(path) else {
                        return;
                    };
                    let Some(payload) = self.payload_type(&fields) else {
                        return;
                    };
                    let area = self.field_ptr(shape, place, 1, "payload");
                    (0..)
                        .zip(&fields)
                        .filter_map(|(at, ty)| {
                            let ptr = self.field_ptr(payload, area, at, "bound");
                            self.llvm_type_of(ty).map(|ty| (ptr, ty))
                        })
                        .collect()
                }
                _ => return,
            };
        for (binding, value) in arguments.iter().zip(values) {
            if let ExpressionKind::Identifier(name) = &binding.kind
                && name != "_"
            {
                self.bind_variable(name, value);
            }
        }
    }

    /// A variant's position among its enum's.
    fn variant_index(&self, path: &str) -> Option<u64> {
        let (enum_name, variant) = path.rsplit_once("::")?;
        let variants = self.types.enum_variants(enum_name)?;
        variants
            .iter()
            .position(|(v, _)| v == variant)
            .map(|at| at as u64)
    }

    /// `Enum::Variant` or `Enum::Variant(values)`: a bare tag, or for an enum
    /// whose variants carry values, the tag with them in its payload area.
    fn build_variant(&mut self, path: &str, values: &[Expression]) -> Option<BasicValueEnum<'ctx>> {
        let tag = self
            .context
            .i32_type()
            .const_int(self.variant_index(path)?, false);
        let (enum_name, fields) = self.types.variant_fields(path)?;
        if !self.types.enum_has_data(&enum_name) {
            return Some(tag.into());
        }
        let BasicTypeEnum::StructType(shape) = self.llvm_type_of(&Type::Enum(enum_name))? else {
            return None;
        };
        let slot = self.create_entry_block_alloca(self.current_fn?, "variant", shape.into());
        let tag_field = self.field_ptr(shape, slot, 0, "tag");
        self.builder.build_store(tag_field, tag).unwrap();
        if let Some(payload) = self.payload_type(&fields) {
            let area = self.field_ptr(shape, slot, 1, "payload");
            for ((at, value), ty) in (0..).zip(values).zip(&fields) {
                let compiled = self.compile_expression(value, self.llvm_type_of(ty));
                let field = self.field_ptr(payload, area, at, "value");
                self.builder.build_store(field, compiled).unwrap();
            }
        }
        Some(self.load(shape, slot, "variant"))
    }
}
