pub mod compile {
    use crate::shared::shared::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;

    use inkwell::module::{Linkage, Module};
    use inkwell::passes::PassManager;

    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
    };
    use inkwell::types::{BasicMetadataTypeEnum, BasicType, FunctionType};
    use inkwell::values::{
        BasicMetadataValueEnum, BasicValueEnum, FloatMathValue, IntMathValue, PointerValue,
    };
    /// Some parts of this file have been directly taken from the collider scope example from inkwell
    use inkwell::values::{BasicValue, FunctionValue};
    use inkwell::OptimizationLevel;
    use std::collections::HashMap;
    use std::path::Path;

    fn get_value<'a, 'ctx>(val: &CompData, compiler: &Compiler<'a, 'ctx>) -> BasicValueEnum<'ctx> {
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
            CompData::Str(str) => compiler
                .builder
                .build_global_string_ptr(&str, "String")
                .as_basic_value_enum(),
            CompData::Func(_) => panic!("Functions should be retrieved seperately"),
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
            parent: Option<&FunctionValue>,
        ) -> BasicValueEnum<'ctx> {
            match exp {
                CompExpression::Call(var, args) => {
                    let func = self.module.get_function(&var.name).unwrap();
                    let compiled_args = args
                        .iter()
                        .map(|arg| {
                            let val = self.compile_expression(arg, variables, parent);
                            match val {
                                BasicValueEnum::PointerValue(_x) => {
                                    println!("Pointer value found");
                                    val
                                }
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
                    use BasicValueEnum::*;
                    let lhs = self.compile_expression(left, variables, parent);
                    let rhs = self.compile_expression(right, variables, parent);
                    match (lhs, rhs) {
                        (IntValue(a), IntValue(b)) => comp_bin_op_int(op, a, b, &self.builder),
                        (FloatValue(a), FloatValue(b)) => comp_bin_op_float(op, a, b, self.builder),
                        _ => panic!(
                            "binary operations for this type cannot be compiled at this time"
                        ),
                    }
                }
                CompExpression::Read(var) => self
                    .builder
                    .build_load(*variables.get(&var.name).unwrap(), &var.name),
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
                        let var = *variables.get(&var.name).unwrap();
                        let _ptr = self.builder.build_store(var, val);
                        val
                    }
                },
                CompExpression::Value(val) => get_value(val, self),
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
                let alloca = self.add_variable_to_block(arg_name, ty, &fn_val);

                self.builder.build_store(alloca, arg);

                variables.insert(arg_name.to_string(), alloca);
            }
            let arg_names = variables.clone().keys().cloned().collect::<Vec<_>>();
            func.body
                .clone()
                .unwrap()
                .scope
                .variables
                .iter()
                .filter(|x| !arg_names.contains(x.0))
                .for_each(|x| {
                    variables.insert(
                        x.0.to_string(),
                        self.add_variable_to_block(
                            &x.0.to_string(),
                            x.1.typing.get_compiler_type(self.context),
                            &fn_val,
                        ),
                    );
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
                fn_val.print_to_stderr();
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
                    .map(|x| x.get_compiler_type(self.context).into())
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

    pub fn compile(ast: &Program) {
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
    }
}
