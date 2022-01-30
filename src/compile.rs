#[path = "shared.rs"]
mod shared;
pub mod compile {
    use crate::shared::*;
    use inkwell::builder::Builder;
    use inkwell::context::{self, Context};
    use inkwell::execution_engine::ExecutionEngine;
    use inkwell::module::{Linkage, Module};
    use inkwell::passes::PassManager;
    use inkwell::targets;
    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    };
    use inkwell::types::{BasicMetadataTypeEnum, BasicType, FunctionType, IntType, VoidType};
    use inkwell::values::{
        BasicMetadataValueEnum, BasicValueEnum, FloatMathValue, FloatValue, IntMathValue, IntValue,
        PointerValue,
    };
    /// Some parts of this file have been directly taken from the collider scope example from inkwell
    use inkwell::values::{BasicValue, FunctionValue};
    use inkwell::OptimizationLevel;
    use std::collections::HashMap;
    use std::io;
    use std::io::Write;
    use std::ops::RangeBounds;
    use std::path::Path;
    // macro used to print & flush without printing a new line
    macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}
    /// Convenience type alias for the `sum` function.
    ///
    /// Calling this is innately `unsafe` because there's no guarantee it doesn't
    /// do `unsafe` operations internally.

    fn get_value<'ctx>(val: &CompData, ctx: &'ctx Context) -> BasicValueEnum<'ctx> {
        match val {
            CompData::Int(int) => ctx
                .i32_type()
                .const_int(*int as u64, false)
                .as_basic_value_enum(),
            CompData::Bool(bool) => ctx
                .bool_type()
                .const_int(*bool as u64, false)
                .as_basic_value_enum(),
            CompData::Float(float) => ctx
                .f32_type()
                .const_float(*float as f64)
                .as_basic_value_enum(),
            CompData::Null => ctx
                .custom_width_int_type(1)
                .const_int(0, false)
                .as_basic_value_enum(),
            CompData::Str(str) => ctx
                .i8_type()
                .const_array(
                    &str.chars()
                        .map(|x| ctx.i8_type().const_int(x as u64, false))
                        .collect::<Vec<_>>(),
                )
                .as_basic_value_enum(),
            CompData::Func(_) => panic!("Functions should be retrieved seperately"),
            _ => panic!(
                "Only integer, float and null types are currently supported, not '{:?}'",
                val
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

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    enum ReturnType<'ctx> {
        Int(IntType<'ctx>),
        Void(VoidType<'ctx>),
    }
    impl<'ctx> ReturnType<'ctx> {
        pub fn fn_type(
            self,
            param_types: &[BasicMetadataTypeEnum<'ctx>],
            is_var_args: bool,
        ) -> FunctionType<'ctx> {
            match self {
                Self::Void(void) => void.fn_type(param_types, is_var_args),
                Self::Int(int) => int.fn_type(param_types, is_var_args),
            }
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
        ) -> BasicValueEnum<'ctx> {
            match exp {
                CompExpression::Call(var, args) => {
                    let func = self.module.get_function(&var.name).unwrap();
                    let compiled_args = args
                        .iter()
                        .map(|arg| self.compile_expression(arg, variables).into())
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
                    let lhs = self.compile_expression(left, variables);
                    let rhs = self.compile_expression(right, variables);
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
                        let val = self.compile_expression(exp, variables);
                        let var = *variables.get(&var.name).unwrap();
                        let ptr = self.builder.build_store(var, val);
                        val
                    }
                },
                CompExpression::Value(val) => get_value(val, self.context),
                CompExpression::List(expressions) => expressions
                    .iter()
                    .map(|x| self.compile_expression(x, variables))
                    .last()
                    .unwrap(),
                _ => panic!("Not implemented"),
            }
        }
        fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
            self.module.get_function(name)
        }
        pub fn create_function(&'a self, func: &FunctionAst, name: &String) -> FunctionValue<'ctx> {
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
            println!(
                "variables: {:?}",
                func.body.clone().unwrap().scope.variables
            );
            println!("Args: {:?}", arg_names);
            func.body
                .clone()
                .unwrap()
                .scope
                .variables
                .iter()
                .filter(|x| !arg_names.contains(x.0))
                .for_each(|x| {
                    println!("{}", x.0);
                    variables.insert(
                        x.0.to_string(),
                        self.add_variable_to_block(
                            &x.0.to_string(),
                            x.1.typing.get_compiler_type(self.context),
                            &fn_val,
                        ),
                    );
                });
            println!("variables: {:?}", variables);
            // compile body
            let body = self.compile_expression(&func.body.clone().unwrap().body, &variables);
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
                println!("{}", func.get_str());
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
        let ret = ctx.i32_type();
        compiler.module.add_function(
            "putchar",
            ret.fn_type(&[BasicMetadataTypeEnum::IntType(ret)], false),
            Some(Linkage::AvailableExternally),
        );
        compiler.compile_expression(&ast.body, &HashMap::new());
        compiler.module.print_to_stderr();
        println!("Before segfault");

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

    pub fn repl() {
        loop {
            println!();
            print_flush!("?> ");

            // Read input from stdin
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Could not read from standard input.");

            if input.starts_with("exit") || input.starts_with("quit") {
                break;
            } else if input.chars().all(char::is_whitespace) {
                continue;
            }
        }
    }
}
