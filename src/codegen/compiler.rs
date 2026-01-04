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
    ast::{AsmOperand, Expression, ExpressionKind, Program, Statement, StatementKind, TypeSpec},
    codegen::SafetyMode,
    token::Token,
};

/// LLVM IR code generator for Zeru.
///
/// This compiler takes a semantically-validated AST and generates LLVM IR.
/// It manages LLVM contexts, types, functions, and generates optimized code
/// based on the selected safety mode (Debug, ReleaseSafe, or ReleaseFast).
///
/// # Lifetimes
/// * `'a` - Lifetime of the LLVM builder
/// * `'ctx` - Lifetime of the LLVM context (must outlive the builder)
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool)>,
    pointer_elem_types: HashMap<String, BasicTypeEnum<'ctx>>,
    constants: HashMap<String, BasicValueEnum<'ctx>>,
    struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    enum_defs: HashMap<String, Vec<String>>,
    current_fn: Option<FunctionValue<'ctx>>,

    current_struct_context: Option<String>,
    loop_stack: Vec<LoopContext<'ctx>>,
    safety_mode: SafetyMode,
    panic_fn: Option<FunctionValue<'ctx>>,

    stdout_stream: Option<PointerValue<'ctx>>,
    stderr_stream: Option<PointerValue<'ctx>>,
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
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let StatementKind::Struct { name, .. } = &stmt.kind {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs
                    .insert(name.clone(), (struct_type, HashMap::new()));
            }
            if let StatementKind::Enum { name, variants } = &stmt.kind {
                self.enum_defs.insert(name.clone(), variants.clone());
            }
        }

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

        for stmt in &program.statements {
            if let StatementKind::Struct { name, fields, .. } = &stmt.kind {
                self.current_struct_context = Some(name.clone());
                self.compile_struct_body(name, fields);
                self.current_struct_context = None;
            }
        }

        self.init_builtin_streams();

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

        self.create_builtin_cleanup();
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

        let fn_type = match ret_type {
            Some(basic_ty) => basic_ty.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };

        self.module.add_function(name, fn_type, None)
    }

    fn compile_fn_body(
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

                panic!("self in non-struct context body");
            }

            let arg_type = self.get_llvm_type(param_spec).expect("Invalid param type");

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
                        None => panic!("Codegen: Variable '{}' cannot be void", name),
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

    fn compile_const_expr(
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
            ExpressionKind::StringLit(s) => {
                let str_val = std::str::from_utf8(s).unwrap();
                let string_val = self
                    .builder
                    .build_global_string_ptr(str_val, "str")
                    .unwrap();
                string_val.as_pointer_value().into()
            }
            _ => panic!("Codegen: Unsupported constant expression: {:?}", expr.kind),
        }
    }

    fn compile_lvalue(
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

    fn compile_expression(
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
                    return self.builder.build_load(*ty, *ptr, "loadtmp").unwrap();
                }

                panic!("Codegen: Unknown identifier '{}'", name);
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
                    let struct_name_result = if let ExpressionKind::Identifier(var_name) =
                        &object.kind
                    {
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
                            _ => panic!("Method call on non-struct"),
                        }
                    };

                    let mangled = format!("{}::{}", struct_name, method_name);
                    let func = self
                        .module
                        .get_function(&mangled)
                        .expect("Method not found");

                    let param_types = func.get_type().get_param_types();
                    let first_param_is_ptr = param_types
                        .first()
                        .map(|t| matches!(t, inkwell::types::BasicMetadataTypeEnum::PointerType(_)))
                        .unwrap_or(false);

                    let args: Vec<BasicMetadataValueEnum> = if first_param_is_ptr {
                        if let ExpressionKind::Identifier(var_name) = &object.kind {
                            if let Some((ptr, _, _)) = self.variables.get(var_name) {
                                if self.pointer_elem_types.contains_key(var_name) {
                                    let ptr_type =
                                        self.context.ptr_type(inkwell::AddressSpace::default());
                                    let loaded_ptr = self
                                        .builder
                                        .build_load(ptr_type, *ptr, "self_loaded")
                                        .unwrap();
                                    vec![loaded_ptr.into()]
                                } else {
                                    vec![(*ptr).into()]
                                }
                            } else {
                                panic!("Cannot get pointer to object for method call");
                            }
                        } else {
                            panic!("var self method requires identifier object");
                        }
                    } else {
                        let obj_val = self.compile_expression(object, None);
                        vec![obj_val.into()]
                    };

                    (func, args)
                } else if let ExpressionKind::Identifier(name) = &function.kind {
                    if matches!(name.as_str(), "print" | "println" | "eprint" | "eprintln") {
                        return self.compile_builtin_print(name, arguments);
                    }

                    if name == "exit" {
                        return self.compile_builtin_exit(arguments);
                    }

                    let func = self.module.get_function(name).expect("Function not found");
                    (func, Vec::new())
                } else {
                    panic!("Indirect calls not implemented");
                };

                let mut compiled_args: Vec<BasicMetadataValueEnum> = implicit_args;
                let param_types: Vec<_> = fn_val.get_type().get_param_types();
                let param_offset = compiled_args.len();

                for (i, arg) in arguments.iter().enumerate() {
                    let expected: Option<BasicTypeEnum> =
                        param_types.get(i + param_offset).and_then(|t| match t {
                            inkwell::types::BasicMetadataTypeEnum::ArrayType(t) => {
                                Some((*t).into())
                            }
                            inkwell::types::BasicMetadataTypeEnum::FloatType(t) => {
                                Some((*t).into())
                            }
                            inkwell::types::BasicMetadataTypeEnum::IntType(t) => Some((*t).into()),
                            inkwell::types::BasicMetadataTypeEnum::PointerType(t) => {
                                Some((*t).into())
                            }
                            inkwell::types::BasicMetadataTypeEnum::StructType(t) => {
                                Some((*t).into())
                            }
                            inkwell::types::BasicMetadataTypeEnum::VectorType(t) => {
                                Some((*t).into())
                            }
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
            ExpressionKind::Infix {
                left,
                operator,
                right,
            } => {
                if *operator == Token::DoubleColon
                    && let (
                        ExpressionKind::Identifier(enum_name),
                        ExpressionKind::Identifier(variant_name),
                    ) = (&left.kind, &right.kind)
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

                    panic!("Codegen: Invalid :: expression");
                }

                if *operator == Token::And || *operator == Token::Or {
                    let bool_type = self.context.bool_type();
                    let current_fn = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();

                    let lhs = self.compile_expression(left, Some(bool_type.into()));
                    let lhs_bool = match lhs {
                        BasicValueEnum::IntValue(v) => {
                            if v.get_type().get_bit_width() == 1 {
                                v
                            } else {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::NE,
                                        v,
                                        v.get_type().const_zero(),
                                        "tobool",
                                    )
                                    .unwrap()
                            }
                        }
                        _ => panic!("&& and || require boolean/integer operands"),
                    };

                    let entry_block = self.builder.get_insert_block().unwrap();
                    let rhs_block = self.context.append_basic_block(current_fn, "rhs_eval");
                    let merge_block = self.context.append_basic_block(current_fn, "merge");

                    if *operator == Token::And {
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
                    let rhs_bool = match rhs {
                        BasicValueEnum::IntValue(v) => {
                            if v.get_type().get_bit_width() == 1 {
                                v
                            } else {
                                self.builder
                                    .build_int_compare(
                                        IntPredicate::NE,
                                        v,
                                        v.get_type().const_zero(),
                                        "tobool",
                                    )
                                    .unwrap()
                            }
                        }
                        _ => panic!("&& and || require boolean/integer operands"),
                    };
                    let rhs_end_block = self.builder.get_insert_block().unwrap();
                    self.builder
                        .build_unconditional_branch(merge_block)
                        .unwrap();

                    self.builder.position_at_end(merge_block);
                    let phi = self.builder.build_phi(bool_type, "result").unwrap();

                    if *operator == Token::And {
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

                    return phi.as_basic_value();
                }

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
                    (BasicValueEnum::PointerValue(ptr), BasicValueEnum::IntValue(offset)) => {
                        let off = match operator {
                            Token::Plus => offset,
                            Token::Minus => self.builder.build_int_neg(offset, "neg").unwrap(),
                            _ => panic!("Only +/- supported for pointer arithmetic"),
                        };
                        unsafe {
                            self.builder
                                .build_gep(self.context.i8_type(), ptr, &[off], "ptr")
                                .unwrap()
                                .into()
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
                let str_val = std::str::from_utf8(s).unwrap();
                let string_val = self
                    .builder
                    .build_global_string_ptr(str_val, "str")
                    .unwrap();
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

                let target_typespec = Self::expr_to_typespec(target);
                let target_type = self
                    .get_llvm_type(&target_typespec)
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
                    _ => panic!("Codegen: Unsupported cast combination"),
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
            ExpressionKind::Match { value, arms } => {
                let parent_fn = self.current_fn.unwrap();
                let match_val = self.compile_expression(value, None).into_int_value();

                let merge_bb = self.context.append_basic_block(parent_fn, "match_merge");

                let mut arm_blocks: Vec<(BasicBlock<'ctx>, BasicValueEnum<'ctx>)> = Vec::new();
                let mut default_block: Option<BasicBlock<'ctx>> = None;
                let mut cases: Vec<(inkwell::values::IntValue<'ctx>, BasicBlock<'ctx>)> =
                    Vec::new();

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
            ExpressionKind::None => {
                if let Some(BasicTypeEnum::StructType(opt_type)) = expected_type {
                    let has_value = self.context.bool_type().const_int(0, false);
                    let inner_type = opt_type.get_field_type_at_index(1).unwrap();
                    let zero_val: BasicValueEnum = match inner_type {
                        BasicTypeEnum::IntType(t) => t.const_int(0, false).into(),
                        BasicTypeEnum::FloatType(t) => t.const_float(0.0).into(),
                        BasicTypeEnum::PointerType(t) => t.const_null().into(),
                        BasicTypeEnum::StructType(t) => t.get_undef().into(),
                        BasicTypeEnum::ArrayType(t) => t.get_undef().into(),
                        BasicTypeEnum::VectorType(t) => t.get_undef().into(),
                        BasicTypeEnum::ScalableVectorType(t) => t.get_undef().into(),
                    };
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
                    panic!("Codegen: None requires known optional type context");
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

    fn compile_inline_asm(
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

    fn is_signed_integer(&self, expr: &Expression) -> Option<bool> {
        match &expr.kind {
            ExpressionKind::Identifier(name) => {
                if let Some((_, _, is_unsigned)) = self.variables.get(name) {
                    Some(!is_unsigned)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn is_unsigned_type(spec: &TypeSpec) -> bool {
        match spec {
            TypeSpec::Named(name) => {
                matches!(name.as_str(), "u8" | "u16" | "u32" | "u64" | "usize")
            }
            TypeSpec::Pointer(inner) => Self::is_unsigned_type(inner),
            TypeSpec::Tuple(_) | TypeSpec::Optional(_) => false,
            TypeSpec::Generic { args, .. } => {
                args.first().map(Self::is_unsigned_type).unwrap_or(false)
            }
            TypeSpec::IntLiteral(_) => false,
            TypeSpec::Slice(_) => false,
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

    fn expr_to_typespec(expr: &Expression) -> TypeSpec {
        match &expr.kind {
            ExpressionKind::Identifier(name) => TypeSpec::Named(name.clone()),
            ExpressionKind::Dereference(inner) => {
                TypeSpec::Pointer(Box::new(Self::expr_to_typespec(inner)))
            }
            _ => panic!(
                "Codegen: Cannot convert expression to type: {:?}",
                expr.kind
            ),
        }
    }

    fn get_llvm_type(&self, spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        match spec {
            TypeSpec::Named(name) => self.get_named_llvm_type(name),
            TypeSpec::Generic { name, args } => {
                if name == "Array" && args.len() == 2 {
                    let elem_type = self.get_llvm_type(&args[0])?;
                    let len = match &args[1] {
                        TypeSpec::IntLiteral(val) => *val as u32,
                        _ => panic!("Array length must be an integer literal"),
                    };
                    return Some(elem_type.array_type(len).into());
                }
                if name == "Result" && args.len() == 2 {
                    let ok_type = self
                        .get_llvm_type(&args[0])
                        .unwrap_or(self.context.i8_type().into());
                    let err_type = self
                        .get_llvm_type(&args[1])
                        .unwrap_or(self.context.i8_type().into());
                    let tag_type = self.context.bool_type().into();
                    let data_type = if self.type_size(ok_type) >= self.type_size(err_type) {
                        ok_type
                    } else {
                        err_type
                    };
                    return Some(
                        self.context
                            .struct_type(&[tag_type, data_type], false)
                            .into(),
                    );
                }
                panic!("Codegen: Unknown generic type {name}");
            }
            TypeSpec::IntLiteral(_) => {
                panic!("Codegen: Unexpected integer literal in type position")
            }
            TypeSpec::Tuple(types) => {
                let field_types: Vec<_> =
                    types.iter().filter_map(|t| self.get_llvm_type(t)).collect();
                Some(self.context.struct_type(&field_types, false).into())
            }
            TypeSpec::Pointer(_) => Some(
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ),
            TypeSpec::Optional(inner) => {
                let inner_type = self.get_llvm_type(inner)?;
                let tag_type = self.context.bool_type().into();
                Some(
                    self.context
                        .struct_type(&[tag_type, inner_type], false)
                        .into(),
                )
            }
            TypeSpec::Slice(_) => {
                let ptr_type = self
                    .context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into();
                let len_type = self.context.i64_type().into();
                Some(
                    self.context
                        .struct_type(&[ptr_type, len_type], false)
                        .into(),
                )
            }
        }
    }

    fn type_size(&self, ty: BasicTypeEnum<'ctx>) -> u64 {
        match ty {
            BasicTypeEnum::IntType(t) => t.get_bit_width() as u64,
            BasicTypeEnum::FloatType(t) => {
                if t == self.context.f32_type() {
                    32
                } else {
                    64
                }
            }
            BasicTypeEnum::PointerType(_) => 64,
            BasicTypeEnum::ArrayType(t) => t.len() as u64 * self.type_size(t.get_element_type()),
            BasicTypeEnum::StructType(t) => {
                t.get_field_types().iter().map(|f| self.type_size(*f)).sum()
            }
            BasicTypeEnum::VectorType(t) => t.get_size() as u64,
            BasicTypeEnum::ScalableVectorType(_) => 64,
        }
    }

    fn get_named_llvm_type(&self, name: &str) -> Option<BasicTypeEnum<'ctx>> {
        let int_type = match name {
            "i8" | "u8" => Some(self.context.i8_type()),
            "i16" | "u16" => Some(self.context.i16_type()),
            "i32" | "u32" => Some(self.context.i32_type()),
            "i64" | "u64" | "isize" | "usize" => Some(self.context.i64_type()),
            _ => None,
        };
        if let Some(ty) = int_type {
            return Some(ty.into());
        }

        match name {
            "f32" => return Some(self.context.f32_type().into()),
            "f64" => return Some(self.context.f64_type().into()),
            "bool" => return Some(self.context.bool_type().into()),
            "void" => return None,
            "self" => {
                return self
                    .current_struct_context
                    .as_ref()
                    .and_then(|struct_name| self.struct_defs.get(struct_name))
                    .map(|(st, _)| st.as_basic_type_enum())
                    .or_else(|| panic!("Self used outside struct context"));
            }
            _ => {}
        }

        if let Some((struct_ty, _)) = self.struct_defs.get(name) {
            return Some(struct_ty.as_basic_type_enum());
        }
        if self.enum_defs.contains_key(name) {
            return Some(self.context.i32_type().into());
        }

        panic!("Codegen: Unknown type {name}");
    }

    fn init_builtin_streams(&mut self) {
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

        let ctor_struct_type = self.context.struct_type(
            &[
                self.context.i32_type().into(),
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ],
            false,
        );

        let ctor_element = ctor_struct_type.const_named_struct(&[
            self.context.i32_type().const_int(65535, false).into(),
            init_fn.as_global_value().as_pointer_value().into(),
            self.context
                .ptr_type(inkwell::AddressSpace::default())
                .const_null()
                .into(),
        ]);

        let ctors_array: inkwell::values::ArrayValue =
            ctor_struct_type.const_array(&[ctor_element]);
        let global_ctors = self.module.add_global(
            ctors_array.get_type(),
            Some(inkwell::AddressSpace::default()),
            "llvm.global_ctors",
        );
        global_ctors.set_linkage(inkwell::module::Linkage::Appending);
        global_ctors.set_initializer(&ctors_array);
    }

    fn create_builtin_cleanup(&mut self) {
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

        let ctor_struct_type = self.context.struct_type(
            &[
                self.context.i32_type().into(),
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ],
            false,
        );

        let dtor_element = ctor_struct_type.const_named_struct(&[
            self.context.i32_type().const_int(65535, false).into(),
            dtor_fn.as_global_value().as_pointer_value().into(),
            self.context
                .ptr_type(inkwell::AddressSpace::default())
                .const_null()
                .into(),
        ]);

        let dtors_array: inkwell::values::ArrayValue =
            ctor_struct_type.const_array(&[dtor_element]);
        let global_dtors = self.module.add_global(
            dtors_array.get_type(),
            Some(inkwell::AddressSpace::default()),
            "llvm.global_dtors",
        );
        global_dtors.set_linkage(inkwell::module::Linkage::Appending);
        global_dtors.set_initializer(&dtors_array);
    }

    fn compile_builtin_print(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> BasicValueEnum<'ctx> {
        if arguments.len() != 1 {
            panic!("{}() expects exactly 1 argument", name);
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

        let string_arg = self.compile_expression(
            &arguments[0],
            Some(
                self.context
                    .ptr_type(inkwell::AddressSpace::default())
                    .into(),
            ),
        );

        let write_str_fn = self
            .module
            .get_function("OutStream::write_str")
            .expect("OutStream::write_str not found");

        self.builder
            .build_call(write_str_fn, &[stream_ptr.into(), string_arg.into()], "")
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

    fn compile_builtin_exit(&mut self, arguments: &[Expression]) -> BasicValueEnum<'ctx> {
        if arguments.len() != 1 {
            panic!("exit() expects exactly 1 argument");
        }

        if let Some(stdout_ptr) = self.stdout_stream {
            let flush_fn = self
                .module
                .get_function("OutStream::flush")
                .expect("OutStream::flush not found");
            self.builder
                .build_call(flush_fn, &[stdout_ptr.into()], "")
                .unwrap();
        }

        if let Some(stderr_ptr) = self.stderr_stream {
            let flush_fn = self
                .module
                .get_function("OutStream::flush")
                .expect("OutStream::flush not found");
            self.builder
                .build_call(flush_fn, &[stderr_ptr.into()], "")
                .unwrap();
        }

        let exit_code = self
            .compile_expression(&arguments[0], Some(self.context.i32_type().into()))
            .into_int_value();

        let syscall1_fn = self
            .module
            .get_function("syscall1")
            .expect("syscall1 not found");
        let sys_exit = self.context.i64_type().const_int(60, false);
        let exit_code_i64 = self
            .builder
            .build_int_s_extend(exit_code, self.context.i64_type(), "exit_code_i64")
            .unwrap();

        self.builder
            .build_call(syscall1_fn, &[sys_exit.into(), exit_code_i64.into()], "")
            .unwrap();

        self.builder.build_unreachable().unwrap();
        self.context.i32_type().const_int(0, false).into()
    }
}

struct LoopContext<'ctx> {
    continue_block: BasicBlock<'ctx>,
    break_block: BasicBlock<'ctx>,
}
