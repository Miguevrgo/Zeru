use std::collections::HashMap;

use inkwell::{
    FloatPredicate, IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, StructType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use crate::{
    ast::{Expression, Program, Statement, TypeSpec},
    token::Token,
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>, bool)>,
    struct_defs: HashMap<String, (StructType<'ctx>, HashMap<String, u32>)>,
    current_fn: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            variables: HashMap::new(),
            struct_defs: HashMap::new(),
            current_fn: None,
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let Statement::Struct { name, .. } = stmt {
                let struct_type = self.context.opaque_struct_type(name);
                self.struct_defs
                    .insert(name.clone(), (struct_type, HashMap::new()));
            }
        }

        for stmt in &program.statements {
            if let Statement::Struct { name, fields, .. } = stmt {
                self.compile_struct_body(name, fields);
            }
        }

        for stmt in &program.statements {
            if let Statement::Function {
                name,
                params,
                return_type,
                ..
            } = stmt
            {
                self.compile_fn_prototype(name, params, return_type);
            }
        }

        for stmt in &program.statements {
            if let Statement::Function {
                name, params, body, ..
            } = stmt
            {
                self.compile_fn_body(name, params, body);
            }
        }
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
        for (_, type_spec) in params {
            if let Some(ty) = self.get_llvm_type(type_spec) {
                param_types.push(ty.into());
            } else {
                panic!("Function parameter cannot be void");
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
            let arg_type = arg.get_type();

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
        match stmt {
            Statement::Var {
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
            Statement::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    let ret_hint = self.current_fn.and_then(|f| f.get_type().get_return_type());
                    let val = self.compile_expression(expr, ret_hint);
                    self.builder.build_return(Some(&val)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            Statement::Expression(expr) => {
                self.compile_expression(expr, None);
            }
            Statement::Block(stmts) => {
                for statement in stmts {
                    self.compile_statement(statement);
                }
            }
            Statement::If {
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
                if then_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                if let Some(else_stmt) = else_branch {
                    self.builder.position_at_end(else_bb);
                    self.compile_statement(else_stmt);
                    if else_bb.get_terminator().is_none() {
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                } else {
                    self.builder.position_at_end(else_bb);
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                self.builder.position_at_end(merge_bb);
            }

            Statement::While { cond, body } => {
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

                self.builder.position_at_end(loop_body_bb);
                self.compile_statement(body);

                if loop_body_bb.get_terminator().is_none() {
                    self.builder
                        .build_unconditional_branch(loop_cond_bb)
                        .unwrap();
                }

                self.builder.position_at_end(after_loop_bb);
            }
            _ => println!("Codegen: Unimplemented statement: {stmt:?}"),
        }
    }

    fn compile_lvalue(
        &mut self,
        expr: &Expression,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>)> {
        match expr {
            Expression::Identifier(name) => {
                self.variables.get(name).map(|(ptr, ty, _)| (*ptr, *ty))
            }
            Expression::Get { object, name } => {
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

            //TODO: Array indexing
            _ => None,
        }
    }

    fn compile_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<BasicTypeEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match expr {
            Expression::Int(val) => {
                let int_type = match expected_type {
                    Some(BasicTypeEnum::IntType(t)) => t,
                    _ => self.context.i32_type(),
                };

                int_type.const_int(*val as u64, false).into()
            }
            Expression::Float(val) => {
                let float_type = match expected_type {
                    Some(BasicTypeEnum::FloatType(t)) => t,
                    _ => self.context.f32_type(),
                };

                float_type.const_float(*val).into()
            }
            Expression::Identifier(_) | Expression::Get { .. } => {
                if let Some((ptr, ty)) = self.compile_lvalue(expr) {
                    return self.builder.build_load(ty, ptr, "loadtmp").unwrap();
                }

                if let Expression::Get { object, name } = expr {
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
            Expression::StructLiteral { name, fields } => {
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
            Expression::Assign { target, value, .. } => {
                let hint = if let Some((_, ty)) = self.compile_lvalue(target) {
                    Some(ty)
                } else {
                    None
                };

                let val = self.compile_expression(value, hint);

                if let Some((ptr, _)) = self.compile_lvalue(target) {
                    self.builder.build_store(ptr, val).unwrap();
                    return val;
                }

                panic!("Codegen: Assignment target invalid");
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let fn_name = if let Expression::Identifier(name) = &**function {
                    name
                } else {
                    panic!("[Error] Indirect function calls not implemented");
                };

                let function = match self.module.get_function(fn_name) {
                    Some(f) => f,
                    None => {
                        panic!("[Error] Call to undefined function '{}'", fn_name);
                    }
                };

                let mut compiled_args = Vec::new();
                for arg in arguments {
                    compiled_args.push(self.compile_expression(arg, None).into());
                }

                let call_site = self
                    .builder
                    .build_call(function, &compiled_args, "call_res")
                    .unwrap();

                use inkwell::values::ValueKind;
                match call_site.try_as_basic_value() {
                    ValueKind::Basic(value) => value,
                    ValueKind::Instruction(_) => self.context.i32_type().const_int(0, false).into(),
                }
            }
            Expression::Infix {
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
                        Token::Slash => self
                            .builder
                            .build_int_signed_div(l, r, "divtmp")
                            .unwrap()
                            .into(),

                        Token::Mod => self
                            .builder
                            .build_int_signed_rem(l, r, "modtmp")
                            .unwrap()
                            .into(),

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
                        //FIX: Preserving sign (optimize for unsigned)
                        Token::ShiftRight => {
                            let is_unsigned = if let Expression::Identifier(name) = &**left {
                                if let Some((_, _, is_u)) = self.variables.get(name) {
                                    *is_u
                                } else {
                                    false
                                }
                            } else {
                                false
                            };
                            self.builder
                                .build_right_shift(l, r, !is_unsigned, "shrtmp")
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
            _ => panic!("Codegen: Unimplemented expression {:?}", expr),
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

    fn get_llvm_type(&self, spec: &TypeSpec) -> Option<BasicTypeEnum<'ctx>> {
        match spec {
            TypeSpec::Named(name) => match name.as_str() {
                "i8" | "u8" => Some(self.context.i8_type().into()),
                "i16" | "u16" => Some(self.context.i16_type().into()),
                "i32" | "u32" => Some(self.context.i32_type().into()),
                "i64" | "u64" => Some(self.context.i64_type().into()),
                "f32" => Some(self.context.f32_type().into()),
                "f64" => Some(self.context.f64_type().into()),
                "bool" => Some(self.context.bool_type().into()),
                "void" => None,
                _ => {
                    if let Some((struct_ty, _)) = self.struct_defs.get(name) {
                        Some(struct_ty.as_basic_type_enum())
                    } else {
                        panic!("Codegen: Unknown type {name}");
                    }
                }
            },
            _ => panic!("Codegen: Complex types not implemented yet"),
        }
    }
}
