use std::collections::HashMap;

use inkwell::{
    FloatPredicate, IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
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

    variables: HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
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
            current_fn: None,
        }
    }

    pub fn compile_program(&mut self, program: &Program) {
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
            let (param_name, _) = &params[i];
            let arg_type = arg.get_type();

            let alloca = self.create_entry_block_alloca(function, param_name, arg_type);
            self.builder.build_store(alloca, arg).unwrap();

            self.variables
                .insert(param_name.clone(), (alloca, arg_type));
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
                self.variables.insert(name.clone(), (alloca, final_type));
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
            Expression::Identifier(name) => match self.variables.get(name) {
                Some((ptr, ty)) => self.builder.build_load(*ty, *ptr, name).unwrap(),
                None => panic!("Codegen: Undefined variable {}", name),
            },
            Expression::Assign { target, value, .. } => {
                let hint = if let Expression::Identifier(name) = &**target {
                    self.variables.get(name).map(|(_, ty)| *ty)
                } else {
                    None
                };

                let val = self.compile_expression(value, hint);

                if let Expression::Identifier(name) = &**target
                    && let Some((ptr, _)) = self.variables.get(name)
                {
                    self.builder.build_store(*ptr, val).unwrap();
                    return val;
                }
                panic!("Codegen: Assignment target invalid");
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
                        Token::ShiftRight => self
                            .builder
                            .build_right_shift(l, r, false, "shrtmp")
                            .unwrap()
                            .into(),
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
                _ => panic!("Codegen: Unknown type {}", name),
            },
            _ => panic!("Codegen: Complex types not implemented yet"),
        }
    }
}
