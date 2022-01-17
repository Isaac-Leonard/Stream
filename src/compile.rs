#[path = "shared.rs"]
mod shared;
pub mod compile {
    use crate::shared::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::execution_engine::ExecutionEngine;
    use inkwell::module::{Linkage, Module};
    use inkwell::passes::PassManager;
    use inkwell::targets;
    use inkwell::targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    };
    use inkwell::types::{BasicMetadataTypeEnum, FunctionType, IntType, VoidType};
    use inkwell::values::{BasicMetadataValueEnum, IntValue, PointerValue};
    /// Some parts of this file have been directly taken from the collider scope example from inkwell
    use inkwell::values::{BasicValue, FunctionValue};
    use inkwell::OptimizationLevel;
    use std::collections::HashMap;
    use std::io;
    use std::io::Write;
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

    fn get_value<'ctx>(val: &RawData, ctx: &'ctx Context) -> IntValue<'ctx> {
        match val {
            RawData::Int(int) => ctx.i32_type().const_int(*int as u64, false),
            RawData::Bool(bool) => ctx.bool_type().const_int(*bool as u64, false),
            _ => panic!("Only integer types are currently supported"),
        }
    }

    fn compile_instr<'a, 'ctx>(
        statement: &'a Instr,
        ctx: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
        function: &FunctionValue<'ctx>,
    ) -> IntValue<'ctx> {
        match statement {
            Instr::LoneExpression(exp) => compile_expression(exp, ctx, builder, module, variables),
            Instr::Assign(name, exp) => {
                let val = compile_expression(exp, ctx, builder, module, variables);
                let var = variables.get(name.as_str()).expect("Undefined variable.");

                builder.build_store(*var, val);
                val
            }
            Instr::InitAssign(_, name, _, exp) => {
                let val = compile_expression(exp, ctx, builder, module, variables);
                let builder2 = ctx.create_builder();

                let entry = function.get_first_basic_block().unwrap();

                match entry.get_first_instruction() {
                    Some(first_instr) => builder2.position_before(&first_instr),
                    None => builder2.position_at_end(entry),
                }

                let alloca = builder2.build_alloca(ctx.i32_type(), name);
                builder.build_store(alloca, val);

                variables.insert(name.to_string(), alloca);
                val
            }
            Instr::Loop(_, _) => {
                panic!("Looping not supported when compiling")
            }
            Instr::IfElse(_, _, _) => {
                panic!("Branching is not supported when compiling")
            }
            _ => {
                panic!("Invalid parsing")
            }
        }
    }

    fn compile_expression<'a, 'ctx>(
        exp: &'a Expression,
        ctx: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        variables: &HashMap<String, PointerValue<'ctx>>,
    ) -> IntValue<'ctx> {
        match exp {
            Expression::FuncCall(name, args) => {
                let func = module.get_function(name).unwrap();
                let compiled_args = args
                    .iter()
                    .map(|arg| compile_expression(arg, ctx, builder, module, variables))
                    .collect::<Vec<IntValue>>();
                let argv = compiled_args
                    .iter()
                    .map(|x| BasicMetadataValueEnum::IntValue(*x))
                    .collect::<Vec<BasicMetadataValueEnum>>();
                match builder
                    .build_call(func, argv.as_slice(), name)
                    .try_as_basic_value()
                    .left()
                {
                    Some(value) => value.into_int_value(),
                    None => panic!("Invalid call produced."),
                }
            }
            Expression::Addition(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_add(lhs, rhs, "adding")
            }
            Expression::Subtraction(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_sub(lhs, rhs, "subtracting")
            }
            Expression::Multiplication(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_mul(lhs, rhs, "mult")
            }
            Expression::Division(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_signed_div(lhs, rhs, "div")
            }
            Expression::Equal(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "equal")
            }
            Expression::LessThan(left, right) => {
                let lhs = compile_expression(left, ctx, builder, module, variables);
                let rhs = compile_expression(right, ctx, builder, module, variables);
                builder.build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "Lessthan")
            }
            Expression::Terminal(val) => match val {
                Symbol::Data(data) => get_value(data, &ctx),
                Symbol::Identifier(name) => builder
                    .build_load(*variables.get(name).unwrap(), name)
                    .into_int_value(),
            },
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
    pub fn compile(ast: &Vec<Instr>) {
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
        let ret = ctx.i32_type();
        module.add_function(
            "putchar",
            ret.fn_type(&[BasicMetadataTypeEnum::IntType(ret)], false),
            Some(Linkage::AvailableExternally),
        );
        for stat in ast {
            match stat {
                Instr::InitAssign(_, name, _, exp) => match exp {
                    Expression::Terminal(Symbol::Data(RawData::Func(fun))) => {
                        let return_type = fun.return_type[0].clone();
                        let i32_type = ctx.i32_type();
                        let ret = match return_type.as_str() {
                            "int" | "null" => ReturnType::Int(i32_type),
                            "void" => ReturnType::Void(ctx.void_type()),
                            x => panic!("Cannot process return type of '{}' at this time", x),
                        };
                        let args = ret.fn_type(
                            std::iter::repeat(i32_type)
                                .take(fun.args.len())
                                .map(|f| f.into())
                                .collect::<Vec<BasicMetadataTypeEnum>>()
                                .as_slice(),
                            false,
                        );
                        let function = module.add_function(name, args, None);
                        // set arguments names
                        for (i, arg) in function.get_param_iter().enumerate() {
                            arg.into_int_value().set_name(&fun.args[i].0);
                        }
                        let entry = ctx.append_basic_block(function, "entry");

                        builder.position_at_end(entry);

                        // build variables map
                        let mut variables: HashMap<String, PointerValue> = HashMap::new();
                        variables.reserve(fun.args.len());

                        for (i, arg) in function.get_param_iter().enumerate() {
                            let arg_name = fun.args[i].0.as_str();
                            let builder2 = ctx.create_builder();

                            let entry = function.get_first_basic_block().unwrap();

                            match entry.get_first_instruction() {
                                Some(first_instr) => builder2.position_before(&first_instr),
                                None => builder2.position_at_end(entry),
                            }

                            let alloca = builder2.build_alloca(ctx.i32_type(), name);

                            builder.build_store(alloca, arg);

                            variables.insert(fun.args[i].0.clone(), alloca);
                        }

                        // compile body
                        let mut last_body = None;
                        for exp in fun.body.iter() {
                            last_body = Some(compile_instr(
                                &exp,
                                &ctx,
                                &builder,
                                &module,
                                &mut variables,
                                &function,
                            ))
                        }
                        let final_value: &dyn BasicValue = &last_body.unwrap();
                        let ret_value: Option<&dyn BasicValue> = match ret {
                            ReturnType::Int(_) => Some(final_value),
                            ReturnType::Void(_) => None,
                        };
                        builder.build_return(ret_value);

                        // return the whole thing after verification and optimization
                        if function.verify(true) {
                            fpm.run_on(&function);
                        } else {
                            panic!("Compiler claims function is invalid")
                        }
                    }
                    _ => {
                        panic!("Cannot have non function declarations at a global level")
                    }
                },
                _ => {
                    panic!("Can only have function declarations at a global level")
                }
            }
        }
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
            .write_to_file(&module, FileType::Object, &path)
            .unwrap();

        let ee = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>("main") };
        match maybe_fn {
            Ok(f) => unsafe {
                println!("=> {}", f.call());
            },
            Err(err) => {
                println!("!> Error during execution: {:?}", err);
            }
        };
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
