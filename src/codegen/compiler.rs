use std::collections::HashMap;

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec},
    codegen::SafetyMode,
    token::Token,
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool)>,
    struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    current_fn: Option<FunctionValue<'ctx>>,

    current_struct_context: Option<String>,
    loop_stack: Vec<LoopContext<'ctx>>,
    safety_mode: SafetyMode,
    panic_fn: Option<FunctionValue<'ctx>>,
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
            struct_defs: HashMap::new(),
            current_fn: None,
            current_struct_context: None,
            loop_stack: Vec::new(),
            safety_mode,
            panic_fn: None,
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct { name, .. } = &stmt.kind {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs
                    .insert(name.clone(), (struct_type, HashMap::new()));
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Struct { name, fields, .. } = &stmt.kind {
                self.current_struct_context = Some(name.clone());
                self.compile_struct_body(name, fields);
                self.current_struct_context = None;
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Function {
                name,
                params,
                return_type,
                ..
            } = &stmt.kind
            {
                self.compile_fn_prototype(name, params, return_type);
            }
            if let StatementKind::Struct {
                name: struct_name,
                methods,
                ..
            } = &stmt.kind
            {
                self.current_struct_context = Some(struct_name.clone());
                for method in methods {
                    if let StatementKind::Function {
                        name: method_name,
                        params,
                        return_type,
                        ..
                    } = &method.kind
                    {
                        let mangled_name = format!("{}::{}", struct_name, method_name);
                        self.compile_fn_prototype(&mangled_name, params, return_type);
                    }
                }
                self.current_struct_context = None;
            }
        }

        for stmt in &program.statements {
            if let StatementKind::Function {
                name, params, body, ..
            } = &stmt.kind
            {
                self.compile_fn_body(name, params, body);
            }

            if let StatementKind::Struct {
                name: struct_name,
                methods,
                ..
            } = &stmt.kind
            {
                self.current_struct_context = Some(struct_name.clone());
                for method in methods {
                    if let StatementKind::Function {
                        name: method_name,
                        params,
                        body,
                        ..
                    } = &method.kind
                    {
                        let mangled_name = format!("{}::{}", struct_name, method_name);
                        self.compile_fn_body(&mangled_name, params, body);
                    }
                }
                self.current_struct_context = None;
            }
        }
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

    fn emit_null_check(&mut self, ptr: PointerValue<'ctx>, error_msg: &str) {
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

        // TODO: For now, just call abort.
        let abort_fn = self.get_or_create_panic_fn();
        self.builder.build_call(abort_fn, &[], "").unwrap();
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(continue_block);
    }

    fn compile_struct_body(&mut self, name: &str, fields: &[(String, TypeSpec)]) {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();

        for (i, (field_name, field_spec)) in fields.iter().enumerate() {
            if let Some(ty) = self.get_llvm_type(field_spec) {
                field_types.push(ty);
                field_indices.insert(field_name.clone(), i as u32);
            } else {
                panic!("Compiler: Unknown type in struct field {}", field_name);
            }
        }

        if let Some((struct_type, indices)) = self.struct_defs.get_mut(name) {
            struct_type.set_body(&field_types, false);
            *indices = field_indices;
        }
    }

    fn compile_fn_prototype(
        &self,
        name: &str,
        params: &[(String, TypeSpec)],
        return_type: &Option<TypeSpec>,
    ) -> FunctionValue<'ctx> {
        let ret_type = match return_type {
            Some(spec) => self.get_llvm_type(spec),
            None => None,
        };

        let mut param_types = Vec::new();
        for (param_name, type_spec) in params {
            if param_name == "self" {
                if let Some(struct_name) = &self.current_struct_context
                    && let Some((st, _)) = self.struct_defs.get(struct_name)
                {
                    param_types.push(st.as_basic_type_enum().into());
                    continue;
                }
                if let Some((struct_name, _)) = name.split_once("::")
                    && let Some((st, _)) = self.struct_defs.get(struct_name)
                {
                    param_types.push(st.as_basic_type_enum().into());
                    continue;
                }
                panic!(
                    "'self' parameter used outside of struct context in function '{}'",
                    name
                );
            }

            if let Some(ty) = self.get_llvm_type(type_spec) {
                param_types.push(ty.into());
            } else {
                panic!("Function parameter '{}' cannot be void", param_name);
            }
        }

        //FIX: Support for i32 return in main will be eliminated as soon as there
        // is an implementation for exit | change src/analyzer@register_function
        if name == "main" && ret_type.is_none() {
            let i32_type = self.context.i32_type();
            let fn_type = i32_type.fn_type(&param_types, false);
            return self.module.add_function(name, fn_type, None);
        }

        let fn_type = match ret_type {
            Some(basic_ty) => basic_ty.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };

        self.module.add_function(name, fn_type, None)
    }

    fn compile_fn_body(&mut self, name: &str, params: &[(String, TypeSpec)], body: &[Statement]) {
        let function = self.module.get_function(name).unwrap();
        self.current_fn = Some(function);

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        self.variables.clear();

        for (i, arg) in function.get_param_iter().enumerate() {
            let (param_name, param_spec) = &params[i];
            let arg_type = if param_name == "self" {
                if let Some(struct_name) = &self.current_struct_context {
                    self.struct_defs
                        .get(struct_name)
                        .unwrap()
                        .0
                        .as_basic_type_enum()
                } else {
                    panic!("self in non-struct context body");
                }
            } else {
                self.get_llvm_type(param_spec).expect("Invalid param type")
            };

            let alloca = self.create_entry_block_alloca(function, param_name, arg_type);
            self.builder.build_store(alloca, arg).unwrap();

            let is_unsigned = if let TypeSpec::Named(t_name) = param_spec {
                t_name.starts_with('u')
            } else {
                false
            };

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
                // FIX: Delete support for this
                let zero = self.context.i32_type().const_int(0, false);
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
                // NOTE: We are using a bool in HashMap + a string + an starts_with just to know if
                // the type is unsigned or not, also it only works for simple type, is this the
                // right way?
                let is_unsigned = if let Some(TypeSpec::Named(t_name)) = type_annotation {
                    t_name.starts_with('u')
                } else {
                    false
                };

                let target_type = if let Some(spec) = type_annotation {
                    match self.get_llvm_type(spec) {
                        Some(ty) => Some(ty),
                        None => panic!("Codegen: Variable '{}' cannot be void", name),
                    }
                } else {
                    None
                };
                let initial_val = self.compile_expression(value, target_type);
                let final_type = target_type.unwrap_or_else(|| initial_val.get_type());

                let function = self.current_fn.unwrap();
                let alloca = self.create_entry_block_alloca(function, name, final_type);

                self.builder.build_store(alloca, initial_val).unwrap();
                self.variables
                    .insert(name.clone(), (alloca, final_type, is_unsigned));
            }
            StatementKind::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    let ret_hint = self.current_fn.and_then(|f| f.get_type().get_return_type());
                    let val = self.compile_expression(expr, ret_hint);
                    self.builder.build_return(Some(&val)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            StatementKind::Expression(expr) => {
                self.compile_expression(expr, None);
            }
            StatementKind::Block(stmts) => {
                for statement in stmts {
                    self.compile_statement(statement);
                }
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
            _ => println!("Codegen: Unimplemented statement: {:?}", stmt.kind),
        }
    }

    fn compile_lvalue(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                self.variables.get(name).map(|(ptr, ty, _)| (*ptr, *ty))
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

    fn compile_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match &expr.kind {
            ExpressionKind::Int(val) => {
                let int_type = match expected_type {
                    Some(BasicTypeEnum::IntType(t)) => t,
                    _ => self.context.i32_type(),
                };

                int_type.const_int(*val as u64, false).into()
            }
            ExpressionKind::Float(val) => {
                let float_type = match expected_type {
                    Some(BasicTypeEnum::FloatType(t)) => t,
                    _ => self.context.f32_type(),
                };

                float_type.const_float(*val).into()
            }
            ExpressionKind::Identifier(_)
            | ExpressionKind::Get { .. }
            | ExpressionKind::Index { .. } => {
                if let Some((ptr, ty)) = self.compile_lvalue(expr) {
                    return self.builder.build_load(ty, ptr, "loadtmp").unwrap();
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

                panic!("Codegen: Failed to load identifier/field {:?}", expr);
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
                                panic!("Unknown field {} in struct {}", field_name, name);
                            }
                        }
                        (st, tasks)
                    } else {
                        panic!("Unknown struct type {}", name);
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
                    panic!("Cannot infer type from an empty array literal");
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
                    let current_val = self.builder.build_load(ty, ptr, "loadtmp").unwrap();
                    let rhs = self.compile_expression(value, Some(ty));

                    self.apply_compound_op(current_val, rhs, operator)
                };

                self.builder.build_store(ptr, final_val).unwrap();
                final_val
            }
            ExpressionKind::Call {
                function,
                arguments,
            } => {
                let (fn_val, implicit_args) = if let ExpressionKind::Get {
                    object,
                    name: method_name,
                } = &function.kind
                {
                    let obj_val = self.compile_expression(object, None);
                    let struct_name = match obj_val.get_type() {
                        BasicTypeEnum::StructType(st) => {
                            st.get_name().unwrap().to_str().unwrap().to_string()
                        }
                        _ => panic!("Method call on non-struct"),
                    };
                    let mangled = format!("{}::{}", struct_name, method_name);
                    let func = self
                        .module
                        .get_function(&mangled)
                        .expect("Method not found");
                    let args: Vec<BasicMetadataValueEnum> = vec![obj_val.into()];
                    (func, args)
                } else if let ExpressionKind::Identifier(name) = &function.kind {
                    let func = self.module.get_function(name).expect("Function not found");
                    (func, Vec::new())
                } else {
                    panic!("Indirect calls not implemented");
                };

                let mut compiled_args: Vec<BasicMetadataValueEnum> = implicit_args;
                for arg in arguments {
                    compiled_args.push(self.compile_expression(arg, None).into());
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
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => {
                let is_comparison = matches!(
                    operator,
                    Token::Eq | Token::NotEq | Token::Lt | Token::Leq | Token::Gt | Token::Geq
                );

                let operand_hint = if is_comparison { None } else { expected_type };

                let lhs = self.compile_expression(left, operand_hint);
                let rhs = self.compile_expression(right, Some(lhs.get_type()));

                match (lhs, rhs) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match operator {
                        Token::Plus => self.builder.build_int_add(l, r, "addtmp").unwrap().into(),
                        Token::Minus => self.builder.build_int_sub(l, r, "subtmp").unwrap().into(),
                        Token::Star => self.builder.build_int_mul(l, r, "multmp").unwrap().into(),
                        Token::Slash => {
                            let is_signed = self.is_signed_integer(expr).unwrap_or(true);
                            if is_signed {
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
                            let is_signed = self.is_signed_integer(expr).unwrap_or(true);
                            if is_signed {
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
                        Token::Lt => self
                            .builder
                            .build_int_compare(IntPredicate::SLT, l, r, "lttmp")
                            .unwrap()
                            .into(),
                        Token::Leq => self
                            .builder
                            .build_int_compare(IntPredicate::SLE, l, r, "letmp")
                            .unwrap()
                            .into(),
                        Token::Gt => self
                            .builder
                            .build_int_compare(IntPredicate::SGT, l, r, "gttmp")
                            .unwrap()
                            .into(),
                        Token::Geq => self
                            .builder
                            .build_int_compare(IntPredicate::SGE, l, r, "getmp")
                            .unwrap()
                            .into(),
                        Token::BitAnd => self.builder.build_and(l, r, "andtmp").unwrap().into(),
                        Token::BitOr => self.builder.build_or(l, r, "ortmp").unwrap().into(),
                        Token::BitXor => self.builder.build_xor(l, r, "xortmp").unwrap().into(),
                        Token::ShiftLeft => self
                            .builder
                            .build_left_shift(l, r, "shltmp")
                            .unwrap()
                            .into(),
                        Token::ShiftRight => {
                            let is_signed = self.is_signed_integer(left).unwrap_or(true);
                            self.builder
                                .build_right_shift(l, r, is_signed, "shrtmp")
                                .unwrap()
                                .into()
                        }
                        _ => panic!("Op int not implemented"),
                    },
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        match operator {
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
                            _ => panic!("Op float not implemented"),
                        }
                    }
                    _ => panic!("Type mismatch in binary operation"),
                }
            }
            ExpressionKind::Boolean(val) => self
                .context
                .bool_type()
                .const_int(*val as u64, false)
                .into(),
            ExpressionKind::StringLit(s) => {
                let string_val = self.builder.build_global_string_ptr(s, "str").unwrap();
                string_val.as_pointer_value().into()
            }
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
                        _ => panic!("Codegen: Cannot negate non-numeric type"),
                    },
                    Token::Bang => {
                        if let BasicValueEnum::IntValue(v) = operand {
                            self.builder.build_not(v, "nottmp").unwrap().into()
                        } else {
                            panic!("Codegen: Cannot apply '!' operator to non-integer type");
                        }
                    }
                    _ => panic!("Codegen: Unimplemented prefix operator: {operator:?}"),
                }
            }
            ExpressionKind::Cast { left, target } => {
                let src_val = self.compile_expression(left, None);

                if let ExpressionKind::Identifier(type_name) = &target.kind {
                    let target_type = self
                        .get_llvm_type(&TypeSpec::Named(type_name.clone()))
                        .expect("Cast to void not allowed");

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
                        _ => panic!("Codegen: Unsupported cast combination"),
                    }
                } else {
                    panic!("Codegen: Cast target must be a type name");
                }
            }
            ExpressionKind::AddressOf(inner) => {
                if let Some((ptr, _ty)) = self.compile_lvalue(inner) {
                    ptr.into()
                } else {
                    panic!("Codegen: Cannot take address of non-lvalue expression");
                }
            }
            ExpressionKind::Dereference(inner) => {
                let ptr_val = self.compile_expression(inner, None);

                if let BasicValueEnum::PointerValue(ptr) = ptr_val {
                    self.emit_null_check(ptr, "null pointer dereference");

                    let load_type = expected_type.unwrap_or_else(|| self.context.i64_type().into());
                    self.builder.build_load(load_type, ptr, "deref").unwrap()
                } else {
                    panic!("Codegen: Cannot dereference non-pointer value");
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
            _ => panic!("Codegen: Unimplemented expression {:?}", expr.kind),
        }
    }

    fn is_signed_integer(&self, expr: &Expression) -> Option<bool> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if let Some((_, _, is_unsigned)) = self.variables.get(name) {
                    Some(!is_unsigned)
                } else {
                    None
                }
            }
            //TODO: Binary operations should be handled?
            _ => None,
        }
    }

    fn create_entry_block_alloca(
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

    fn apply_compound_op(
        &self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        operator: &Token,
    ) -> BasicValueEnum<'ctx> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match operator {
                Token::PlusEq => self.builder.build_int_add(l, r, "addtmp").unwrap().into(),
                Token::MinusEq => self.builder.build_int_sub(l, r, "subtmp").unwrap().into(),
                Token::StarEq => self.builder.build_int_mul(l, r, "multmp").unwrap().into(),
                Token::SlashEq => self
                    .builder
                    .build_int_signed_div(l, r, "divtmp")
                    .unwrap()
                    .into(),
                Token::ModEq => self
                    .builder
                    .build_int_signed_rem(l, r, "modtmp")
                    .unwrap()
                    .into(),
                Token::BitAndEq => self.builder.build_and(l, r, "andtmp").unwrap().into(),
                Token::BitOrEq => self.builder.build_or(l, r, "ortmp").unwrap().into(),
                Token::BitXorEq => self.builder.build_xor(l, r, "xortmp").unwrap().into(),
                Token::BitLShiftEq => self
                    .builder
                    .build_left_shift(l, r, "shltmp")
                    .unwrap()
                    .into(),
                Token::BitRShiftEq => self
                    .builder
                    .build_right_shift(l, r, true, "shrtmp")
                    .unwrap()
                    .into(),
                _ => panic!("Codegen: Unknown compound operator {:?}", operator),
            },
            (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => match operator {
                Token::PlusEq => self
                    .builder
                    .build_float_add(l, r, "faddtmp")
                    .unwrap()
                    .into(),
                Token::MinusEq => self
                    .builder
                    .build_float_sub(l, r, "fsubtmp")
                    .unwrap()
                    .into(),
                Token::StarEq => self
                    .builder
                    .build_float_mul(l, r, "fmultmp")
                    .unwrap()
                    .into(),
                Token::SlashEq => self
                    .builder
                    .build_float_div(l, r, "fdivtmp")
                    .unwrap()
                    .into(),
                Token::ModEq => self
                    .builder
                    .build_float_rem(l, r, "fmodtmp")
                    .unwrap()
                    .into(),
                _ => panic!(
                    "Codegen: Compound operator {:?} not supported for floats",
                    operator
                ),
            },
            _ => panic!("Codegen: Type mismatch in compound assignment"),
        }
    }

    fn get_llvm_type(&self, spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        match spec {
            TypeSpec::Named(name) => match name.as_str() {
                "i8" | "u8" => Some(self.context.i8_type().into()),
                "i16" | "u16" => Some(self.context.i16_type().into()),
                "i32" | "u32" => Some(self.context.i32_type().into()),
                "i64" | "u64" => Some(self.context.i64_type().into()),
                "isize" | "usize" => Some(self.context.i64_type().into()),
                "f32" => Some(self.context.f32_type().into()),
                "f64" => Some(self.context.f64_type().into()),
                "bool" => Some(self.context.bool_type().into()),
                "void" => None,
                "self" => {
                    if let Some(struct_name) = &self.current_struct_context {
                        if let Some((struct_ty, _)) = self.struct_defs.get(struct_name) {
                            Some(struct_ty.as_basic_type_enum())
                        } else {
                            panic!("Self used in unknown struct: {}", struct_name);
                        }
                    } else {
                        panic!("Self used outside struct context");
                    }
                }
                _ => {
                    if let Some((struct_ty, _)) = self.struct_defs.get(name) {
                        Some(struct_ty.as_basic_type_enum())
                    } else {
                        panic!("Codegen: Unknown type {name}");
                    }
                }
            },
            TypeSpec::Generic { name, args } => {
                if name == "Array" && args.len() == 2 {
                    let elem_type = self.get_llvm_type(&args[0])?;
                    let len = if let TypeSpec::IntLiteral(val) = args[1] {
                        val as u32
                    } else {
                        panic!("Array length must be an integer type");
                    };

                    return Some(elem_type.array_type(len).into());
                }
                panic!("Codegen: Unknown generic type {name}");
            }
            TypeSpec::IntLiteral(_) => {
                panic!("Codegen: Unexpected integer literal in type position")
            }
            TypeSpec::Tuple(types) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> =
                    types.iter().filter_map(|t| self.get_llvm_type(t)).collect();
                Some(self.context.struct_type(&field_types, false).into())
            }
            TypeSpec::Pointer(inner) => {
                let _inner_type = self.get_llvm_type(inner);
                Some(
                    self.context
                        .ptr_type(inkwell::AddressSpace::default())
                        .into(),
                )
            }
        }
    }
}

struct LoopContext<'ctx> {
    continue_block: BasicBlock<'ctx>,
    break_block: BasicBlock<'ctx>,
}
