#[path = "shared.rs"]
mod shared;
pub mod compile {
    use crate::shared::*;
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::execution_engine::ExecutionEngine;
    use inkwell::module::Module;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicMetadataTypeEnum;
    use inkwell::values::{BasicMetadataValueEnum, IntValue, PointerValue};
    /// Some parts of this file have been directly taken from the collider scope example from inkwell
    use inkwell::values::{BasicValue, FunctionValue};
    use std::collections::HashMap;
    use std::io;
    use std::io::Write;
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
        for stat in ast {
            match stat {
                Instr::InitAssign(_, name, _, exp) => match exp {
                    Expression::Terminal(Symbol::Data(RawData::Func(fun))) => {
                        let ret = ctx.i32_type();
                        let args = ret.fn_type(
                            std::iter::repeat(ret)
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

                        builder.build_return(Some(&last_body.unwrap()));

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
