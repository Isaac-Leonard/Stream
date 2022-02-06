pub mod compile {
    use crate::shared::shared::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;

    use inkwell::module::{Linkage, Module};
    use inkwell::passes::PassManager;

    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
    };
    use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, FunctionType};
    use inkwell::values::{
        BasicMetadataValueEnum, BasicValueEnum, FloatMathValue, IntMathValue, PointerValue,
    };
    /// Some parts of this file have been directly taken from the collider scope example from inkwell
    use inkwell::values::{BasicValue, FunctionValue};
    use inkwell::OptimizationLevel;
    use std::collections::HashMap;
    use std::path::Path;

    fn get_value<'a, 'ctx>(
        val: &CompData,
        compiler: &Compiler<'a, 'ctx>,
        fn_val: Option<&FunctionValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match val {
            CompData::Int(int) => compiler
                .context
                .i32_type()
                .const_int(*int as u64, false)
                .as_basic_value_enum(),
            CompData::Bool(bool) => compiler
                .context
                .bool_type()
                .const_int(*bool as u64, false)
                .as_basic_value_enum(),
            CompData::Float(float) => compiler
                .context
                .f32_type()
                .const_float(*float as f64)
                .as_basic_value_enum(),
            CompData::Null => compiler
                .context
                .custom_width_int_type(1)
                .const_int(0, false)
                .as_basic_value_enum(),
            CompData::Str(str) => {
                let var = compiler.add_variable_to_block(
                    &"string".to_string(),
                    val.get_type().get_compiler_type(compiler.context),
                    fn_val.unwrap(),
                );
                let string = compiler
                    .context
                    .i8_type()
                    .const_array(
                        &str.chars()
                            .chain(['\0'])
                            .map(|x| compiler.context.i8_type().const_int(x as u64, false))
                            .collect::<Vec<_>>()
                            .as_slice(),
                    )
                    .as_basic_value_enum();
                compiler.builder.build_store(var, string);
                var.as_basic_value_enum()
            }
            CompData::Func(_) => panic!("Functions should be retrieved seperately"),
        }
    }

    fn comp_bin_op<'a, 'ctx>(
        op: &Op,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        compiler: &Compiler<'a, 'ctx>,
        fn_val: Option<&FunctionValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        use BasicValueEnum::*;
        match (lhs, rhs) {
            (IntValue(a), IntValue(b)) => comp_bin_op_int(op, a, b, &compiler.builder),
            (FloatValue(a), FloatValue(b)) => comp_bin_op_float(op, a, b, compiler.builder),
            (PointerValue(a), PointerValue(b)) => match (
                a.get_type().get_element_type(),
                b.get_type().get_element_type(),
            ) {
                (AnyTypeEnum::ArrayType(_), AnyTypeEnum::ArrayType(_)) => {
                    comp_bin_op_str(op, a, b, compiler, fn_val.unwrap())
                }
                _ => panic!(
                    "Binary operations for {:?} and {:?} type cannot be compiled at this time",
                    a, b
                ),
            },
            (a, b) => panic!(
                "Binary operations for {:?} and {:?} type cannot be compiled at this time",
                a, b
            ),
        }
    }

    fn comp_bin_op_float<'ctx, T: FloatMathValue<'ctx>>(
        op: &Op,
        lhs: T,
        rhs: T,
        builder: &Builder<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        use Op::*;
        match op {
            Add => builder
                .build_float_add(lhs, rhs, "adding")
                .as_basic_value_enum(),
            Sub => builder
                .build_float_sub(lhs, rhs, "subtracting")
                .as_basic_value_enum(),
            Mult => builder
                .build_float_mul(lhs, rhs, "mult")
                .as_basic_value_enum(),
            Div => builder
                .build_float_div(lhs, rhs, "div")
                .as_basic_value_enum(),
            Eq => builder
                .build_float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs, "equal")
                .as_basic_value_enum(),
            Le => builder
                .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
        }
    }

    fn comp_bin_op_int<'ctx, T: IntMathValue<'ctx>>(
        op: &Op,
        lhs: T,
        rhs: T,
        builder: &Builder<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        use Op::*;
        match op {
            Add => builder
                .build_int_add(lhs, rhs, "adding")
                .as_basic_value_enum(),
            Sub => builder
                .build_int_sub(lhs, rhs, "subtracting")
                .as_basic_value_enum(),
            Mult => builder
                .build_int_mul(lhs, rhs, "mult")
                .as_basic_value_enum(),
            Div => builder
                .build_int_signed_div(lhs, rhs, "div")
                .as_basic_value_enum(),
            Eq => builder
                .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "equal")
                .as_basic_value_enum(),
            Le => builder
                .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
        }
    }

    fn comp_bin_op_str<'a, 'ctx>(
        op: &Op,
        lhs: PointerValue<'ctx>,
        rhs: PointerValue<'ctx>,
        compiler: &Compiler<'a, 'ctx>,
        fn_val: &FunctionValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        use Op::*;
        let (len_1, len_2) = match (
            lhs.get_type().get_element_type(),
            rhs.get_type().get_element_type(),
        ) {
            (AnyTypeEnum::ArrayType(a), AnyTypeEnum::ArrayType(b)) => (a.len(), b.len()),
            _ => panic!("comp_bin_op_str called with wrong values"),
        };

        match op {
            Add => {
                let strcat = if let Some(strcat) = compiler.module.get_function("strcat") {
                    strcat
                } else {
                    let fn_type = compiler.create_function_shape(&CompType::Callible(
                        vec![CompType::Ptr, CompType::Ptr],
                        Box::new(CompType::Int),
                    ));
                    compiler.module.add_function(
                        "strcat",
                        fn_type,
                        Some(Linkage::AvailableExternally),
                    )
                };
                let target_type = compiler
                    .context
                    .i8_type()
                    .array_type(len_1 + len_2 - 1)
                    .as_basic_type_enum();
                let target = compiler.add_variable_to_block("strcat_cpy", target_type, fn_val);

                let cast_type = compiler
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::Generic);
                compiler
                    .builder
                    .build_store(target, target_type.const_zero());

                let lhs_ptr = compiler.builder.build_bitcast(lhs, cast_type, "cast");
                let rhs_ptr = compiler.builder.build_bitcast(rhs, cast_type, "cast");
                let target_ptr = compiler.builder.build_bitcast(target, cast_type, "cast");

                compiler.builder.build_call(
                    strcat,
                    &[
                        BasicMetadataValueEnum::from(target_ptr),
                        BasicMetadataValueEnum::from(lhs_ptr),
                    ],
                    "str_cpy",
                );

                compiler.builder.build_call(
                    strcat,
                    &[
                        BasicMetadataValueEnum::from(target_ptr),
                        BasicMetadataValueEnum::from(rhs_ptr),
                    ],
                    "str_cpy",
                );
                target.as_basic_value_enum()
            }
            x => panic!("Operator '{}' is not supported for strings", x.get_str()),
        }
    }

    pub struct Compiler<'a, 'ctx> {
        pub context: &'ctx Context,
        pub builder: &'a Builder<'ctx>,
        pub fpm: &'a PassManager<FunctionValue<'ctx>>,
        pub module: &'a Module<'ctx>,
    }
    impl<'a, 'ctx> Compiler<'a, 'ctx> {
        fn compile_expression(
            &self,
            exp: &CompExpression,
            variables: &HashMap<String, PointerValue<'ctx>>,
            parent: Option<&FunctionValue<'ctx>>,
        ) -> BasicValueEnum<'ctx> {
            match exp {
                CompExpression::Call(var, args) => {
                    let func = self.module.get_function(&var.name).unwrap();
                    let compiled_args = args
                        .iter()
                        .map(|arg| {
                            let val = self.compile_expression(arg, variables, parent);
                            match val {
                                BasicValueEnum::PointerValue(x) => self.builder.build_bitcast(
                                    x,
                                    self.context
                                        .i8_type()
                                        .ptr_type(inkwell::AddressSpace::Generic),
                                    "cast",
                                ),
                                x => x,
                            }
                        })
                        .collect::<Vec<BasicValueEnum>>();
                    let argv = compiled_args
                        .iter()
                        .map(|x| BasicMetadataValueEnum::from(*x))
                        .collect::<Vec<BasicMetadataValueEnum>>();
                    match self
                        .builder
                        .build_call(func, argv.as_slice(), &var.name)
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => value,
                        None => panic!("Invalid call produced."),
                    }
                }
                CompExpression::BinOp(op, left, right) => {
                    let lhs = self.compile_expression(left, variables, parent);
                    let rhs = self.compile_expression(right, variables, parent);
                    comp_bin_op(op, lhs, rhs, self, parent)
                }
                CompExpression::Read(var) => {
                    let var = variables.get(&var.name).unwrap();
                    self.builder.build_load(*var, "load").as_basic_value_enum()
                }
                CompExpression::Assign(var, exp) => match exp.as_ref() {
                    CompExpression::Value(CompData::Func(func)) => {
                        self.create_function(func, &var.name);
                        self.context
                            .custom_width_int_type(1)
                            .const_int(0, false)
                            .as_basic_value_enum()
                    }
                    exp => {
                        let val = self.compile_expression(exp, variables, parent);
                        let var_ptr = *variables.get(&var.name).unwrap();
                        self.builder.build_store(var_ptr, val);
                        val
                    }
                },
                CompExpression::Value(val) => get_value(val, self, parent),
                CompExpression::IfElse {
                    cond,
                    then,
                    otherwise,
                } => {
                    let cond = self
                        .compile_expression(cond, variables, parent)
                        .into_int_value();

                    let then_bb = self.context.append_basic_block(*parent.unwrap(), "then");
                    let else_bb = self.context.append_basic_block(*parent.unwrap(), "else");
                    let cont_bb = self.context.append_basic_block(*parent.unwrap(), "ifcont");

                    self.builder
                        .build_conditional_branch(cond, then_bb, else_bb);

                    // build then block
                    self.builder.position_at_end(then_bb);
                    let then_val = self.compile_expression(then, variables, parent);
                    self.builder.build_unconditional_branch(cont_bb);

                    let then_bb = self.builder.get_insert_block().unwrap();

                    // build else block
                    self.builder.position_at_end(else_bb);
                    let else_val = self.compile_expression(otherwise, variables, parent);
                    self.builder.build_unconditional_branch(cont_bb);

                    let else_bb = self.builder.get_insert_block().unwrap();

                    // emit merge block
                    self.builder.position_at_end(cont_bb);

                    let phi = self
                        .builder
                        .build_phi(self.context.custom_width_int_type(32), "iftmp");

                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                    phi.as_basic_value()
                }
                CompExpression::List(expressions) => expressions
                    .iter()
                    .map(|x| self.compile_expression(x, variables, parent))
                    .last()
                    .unwrap(),
                CompExpression::Prog(prog) => {
                    self.compile_expression(&prog.body, variables, parent)
                }
                other => panic!("Not implemented '{:?}'", other),
            }
        }
        fn create_function(&'a self, func: &FunctionAst, name: &String) -> FunctionValue<'ctx> {
            let fn_val = self.create_function_shape(&CompType::Callible(
                func.arguments.iter().map(|x| x.typing.clone()).collect(),
                Box::new(func.return_type.clone()),
            ));
            if func.body == None {
                return self
                    .module
                    .add_function(name, fn_val, Some(Linkage::AvailableExternally));
            }
            let fn_val = self.module.add_function(name, fn_val, None);

            let entry = self.context.append_basic_block(fn_val, "entry");

            self.builder.position_at_end(entry);

            // build variables map
            let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();

            for (i, arg) in fn_val.get_param_iter().enumerate() {
                let arg_name = func.arguments[i].name.as_str();
                let ty = func.arguments[i].typing.get_compiler_type(self.context);
                let var = self.add_variable_to_block(arg_name, ty, &fn_val);
                self.builder.build_store(var, arg);
                variables.insert(arg_name.to_string(), var);
            }
            let arg_names = variables.clone().keys().cloned().collect::<Vec<_>>();
            let str_name = &"string".to_string();
            func.body
                .clone()
                .unwrap()
                .scope
                .variables
                .iter()
                .filter(|x| !arg_names.contains(x.0))
                .for_each(|x| {
                    let ty = x.1.typing.clone();
                    let comp_type = ty.get_compiler_type(self.context);
                    let name = x.0.to_string();
                    if ty.is_str() {
                        let string = self.add_variable_to_block(str_name, comp_type, &fn_val);
                        let ptr_ty = comp_type.ptr_type(inkwell::AddressSpace::Generic);
                        let var = self.add_variable_to_block(&name, ptr_ty, &fn_val);
                        self.builder.build_store(var, string);
                        variables.insert(name, var);
                    } else {
                        let var = self.add_variable_to_block(&name, comp_type, &fn_val);
                        variables.insert(name, var);
                    }
                });
            // compile body
            let body = self.compile_expression(
                &func.body.clone().unwrap().body,
                &variables,
                Some(&fn_val),
            );
            match func.return_type {
                CompType::Null => self.builder.build_return(Some(
                    &self
                        .context
                        .custom_width_int_type(1)
                        .const_int(0, false)
                        .as_basic_value_enum(),
                )),
                _ => self.builder.build_return(Some(&body)),
            };

            // return the whole thing after verification and optimization
            if !fn_val.verify(true) {
                self.module.print_to_stderr();
                panic!("Invalid generated function.")
            }
            self.fpm.run_on(&fn_val);
            fn_val
        }

        pub fn add_variable_to_block(
            &self,
            name: &str,
            ty: impl BasicType<'ctx>,
            fn_val: &FunctionValue<'ctx>,
        ) -> PointerValue<'ctx> {
            let builder = self.context.create_builder();

            let entry = fn_val.get_first_basic_block().unwrap();

            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(entry),
            }

            builder.build_alloca(ty, name)
        }
        pub fn create_function_shape(&self, func: &CompType) -> FunctionType<'ctx> {
            if let CompType::Callible(arguments, return_type) = func {
                let args_types = arguments
                    .iter()
                    .map(|x| {
                        match x.get_compiler_type(self.context) {
                            inkwell::types::BasicTypeEnum::ArrayType(x) => x
                                .ptr_type(inkwell::AddressSpace::Generic)
                                .as_basic_type_enum(),
                            x => x,
                        }
                        .into()
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();
                let args_types = args_types.as_slice();

                return_type
                    .get_compiler_type(self.context)
                    .fn_type(args_types, false)
            } else {
                panic!("Must be a function, not '{}'", func.get_str())
            }
        }
    }

    pub fn compile(ast: &Program, settings: Settings) {
        let ctx = Context::create();
        let module = ctx.create_module("repl");
        let builder = ctx.create_builder();

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();
        // make module
        let module = ctx.create_module("module");
        let compiler = Compiler {
            context: &ctx,
            builder: &builder,
            fpm: &fpm,
            module: &module,
        };
        compiler.compile_expression(&ast.body, &HashMap::new(), None);

        Target::initialize_x86(&InitializationConfig::default());
        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let path = Path::new("./example-compile.o");
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-apple-darwin"),
                "x86-64",
                "+avx2",
                opt,
                reloc,
                model,
            )
            .unwrap();
        target_machine
            .write_to_file(&compiler.module, FileType::Object, &path)
            .unwrap();
        if settings.print_llvm {
            module.print_to_stderr();
        }
    }
}
