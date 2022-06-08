use crate::settings::Settings;
use crate::{ast::*, map_vec};
use fxhash::hash32;
use inkwell::builder::Builder;
use inkwell::context::Context;

use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{
    AnyTypeEnum, ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AggregateValue, BasicMetadataValueEnum, BasicValueEnum, FloatMathValue, IntMathValue, IntValue,
    PointerValue,
};
/// Some parts of this file have been directly taken from the collider scope example from inkwell
use inkwell::values::{BasicValue, FunctionValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

impl CompType {
    fn get_discriminant(&self) -> u32 {
        hash32(self)
    }

    fn get_compiler_type<'ctx>(
        &self,
        context: &'ctx Context,
    ) -> Result<BasicTypeEnum<'ctx>, String> {
        use CompType::*;
        Ok(match self.clone() {
            Array(ty, len) => ty
                .get_compiler_type(context)?
                .array_type(len as u32)
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Type => context.i32_type().as_basic_type_enum(),
            Int => context.i32_type().as_basic_type_enum(),
            Float => context.f32_type().as_basic_type_enum(),
            Null => context.custom_width_int_type(1).as_basic_type_enum(),
            Bool => context.custom_width_int_type(1).as_basic_type_enum(),
            Str(len) => context
                .i8_type()
                .array_type(len + 1)
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Struct(keys) => context
                .struct_type(
                    keys.iter()
                        .map(|(_, ty)| ty.get_compiler_type(context))
                        .collect::<Result<Vec<_>, _>>()?
                        .as_slice(),
                    false,
                )
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Ptr => context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Union(_) => context
                .struct_type(
                    &[
                        context.i32_type().as_basic_type_enum(),
                        context.i32_type().as_basic_type_enum(),
                    ],
                    false,
                )
                .as_basic_type_enum(),
            Constant(data) => data.widen().get_compiler_type(context)?,
            _ => {
                return Err(format!(
                    "get_compiler_type not implemented for type '{}'",
                    self.get_str()
                ))
            }
        })
    }
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
}
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn i32(&self, val: i32) -> BasicValueEnum<'ctx> {
        (self.context.i32_type().const_int(val as u64, false)).as_basic_value_enum()
    }

    fn i64(&self, val: i64) -> BasicValueEnum<'ctx> {
        (self.context.i64_type().const_int(val as u64, false)).as_basic_value_enum()
    }

    fn custom_int(&self, width: u8, val: i8) -> BasicValueEnum<'ctx> {
        (self.context.custom_width_int_type(width as u32))
            .const_int(val as u64, false)
            .as_basic_value_enum()
    }

    fn i8(&self, val: i8) -> BasicValueEnum<'ctx> {
        (self.context.i8_type().const_int(val as u64, false)).as_basic_value_enum()
    }

    fn f32(&self, val: f32) -> BasicValueEnum<'ctx> {
        (self.context.f32_type().const_float(val as f64)).as_basic_value_enum()
    }

    fn alloc_heap(&self, bytes: usize) -> Result<PointerValue<'ctx>, String> {
        let malloc = self.module.get_function("malloc").unwrap_or_else(|| {
            let fn_val = self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic)
                .fn_type(
                    &[BasicMetadataTypeEnum::from(self.context.i64_type())],
                    false,
                );
            self.module
                .add_function("malloc", fn_val, Some(Linkage::External))
        });
        self.builder
            .build_call(
                malloc,
                &[BasicMetadataValueEnum::from(self.i64(bytes as i64))],
                "malloc",
            )
            .try_as_basic_value()
            .left()
            .ok_or_else(|| "Invalid return from function".to_string())
            .map(|x| x.into_pointer_value())
    }

    fn cast_to_i32(&self, int: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_int_cast(int, self.context.i32_type(), "int_cast")
    }

    fn cast_to_i8(&self, int: IntValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_int_cast(int, self.context.i8_type(), "int_cast")
    }

    fn load_variable(
        &self,
        variables: &HashMap<String, PointerValue<'ctx>>,
        var: &str,
    ) -> BasicValueEnum<'ctx> {
        let error = format!("Tried to load nonexistent variable {}", var);
        let var = variables.get(var).expect(&error);
        self.builder.build_load(*var, "load")
    }

    fn extract_element(&self, agg: impl AggregateValue<'ctx>, i: u32) -> BasicValueEnum<'ctx> {
        self.builder
            .build_extract_value(agg, i, "extract_element")
            .expect("Tried to extract non existent element from struct")
    }

    fn calc_pos(
        &self,
        arr: &ExpEnvironment,
        index: &ExpEnvironment,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
        parent: Option<&FunctionValue<'ctx>>,
    ) -> Result<PointerValue<'ctx>, String> {
        let comp_arr = self
            .compile_expression(arr, variables, parent)?
            .into_pointer_value();
        let index = self.compile_expression(index, variables, parent)?;
        Ok(unsafe {
            self.builder.build_in_bounds_gep(
                comp_arr,
                &[self.i32(0).into_int_value(), index.into_int_value()],
                "calc_pos",
            )
        })
    }

    fn comp_bin_op(
        &self,
        op: &Op,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        fn_val: Option<&FunctionValue<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        use BasicValueEnum::*;
        Ok(match (lhs, rhs) {
            (IntValue(a), IntValue(b)) => self.comp_bin_op_int(op, a, b),
            (FloatValue(a), FloatValue(b)) => self.comp_bin_op_float(op, a, b),
            (PointerValue(a), PointerValue(b)) => match (
                a.get_type().get_element_type(),
                b.get_type().get_element_type(),
            ) {
                (AnyTypeEnum::ArrayType(_), AnyTypeEnum::ArrayType(_)) => self.comp_bin_op_str(
                    op,
                    a,
                    b,
                    fn_val.ok_or_else(|| "No function provided".to_string())?,
                )?,
                _ => {
                    return Err(format!(
                        "Binary operations for {:?} and {:?} type cannot be compiled at this time",
                        a, b
                    ))
                }
            },
            (a, b) => {
                return Err(format!(
                    "Binary operations for {:?} and {:?} type cannot be compiled at this time",
                    a, b
                ))
            }
        })
    }

    fn comp_bin_op_float<T: FloatMathValue<'ctx>>(
        &self,
        op: &Op,
        lhs: T,
        rhs: T,
    ) -> BasicValueEnum<'ctx> {
        let builder = self.builder;
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
            Neq => builder
                .build_float_compare(inkwell::FloatPredicate::ONE, lhs, rhs, "equal")
                .as_basic_value_enum(),
            Le => builder
                .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
            Ge => builder
                .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
        }
    }

    fn comp_bin_op_int<T: IntMathValue<'ctx>>(
        &self,
        op: &Op,
        lhs: T,
        rhs: T,
    ) -> BasicValueEnum<'ctx> {
        let builder = self.builder;
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
            Neq => builder
                .build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "equal")
                .as_basic_value_enum(),
            Le => builder
                .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
            Ge => builder
                .build_int_compare(inkwell::IntPredicate::SGT, lhs, rhs, "Lessthan")
                .as_basic_value_enum(),
        }
    }

    fn comp_bin_op_str(
        &self,
        op: &Op,
        lhs: PointerValue<'ctx>,
        rhs: PointerValue<'ctx>,
        fn_val: &FunctionValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        use Op::*;
        let (len_1, len_2) = match (
            lhs.get_type().get_element_type(),
            rhs.get_type().get_element_type(),
        ) {
            (AnyTypeEnum::ArrayType(a), AnyTypeEnum::ArrayType(b)) => (a.len(), b.len()),
            _ => return Err("comp_bin_op_str called with wrong values".to_string()),
        };

        Ok(match op {
            Add => {
                let strcat = self.module.get_function("strcat").unwrap_or({
                    let fn_type = self.create_function_shape(&CompType::Callible(
                        vec![CompType::Ptr, CompType::Ptr],
                        Box::new(CompType::Int),
                    ))?;
                    self.module
                        .add_function("strcat", fn_type, Some(Linkage::AvailableExternally))
                });
                let target_type = self
                    .context
                    .i8_type()
                    .array_type(len_1 + len_2 - 1)
                    .as_basic_type_enum();
                let target = self.add_variable_to_block("strcat_cpy", target_type, fn_val);

                let cast_type = self
                    .context
                    .i8_type()
                    .ptr_type(inkwell::AddressSpace::Generic);
                self.builder.build_store(target, target_type.const_zero());

                let lhs_ptr = self.builder.build_bitcast(lhs, cast_type, "cast");
                let rhs_ptr = self.builder.build_bitcast(rhs, cast_type, "cast");
                let target_ptr = self.builder.build_bitcast(target, cast_type, "cast");

                self.builder.build_call(
                    strcat,
                    &[
                        BasicMetadataValueEnum::from(target_ptr),
                        BasicMetadataValueEnum::from(lhs_ptr),
                    ],
                    "str_cpy",
                );

                self.builder.build_call(
                    strcat,
                    &[
                        BasicMetadataValueEnum::from(target_ptr),
                        BasicMetadataValueEnum::from(rhs_ptr),
                    ],
                    "str_cpy",
                );
                target.as_basic_value_enum()
            }
            x => {
                return Err(format!(
                    "Operator '{}' is not supported for strings",
                    x.get_str()
                ))
            }
        })
    }

    fn get_value(&self, val: &CompData) -> Result<BasicValueEnum<'ctx>, String> {
        Ok(match val {
            CompData::Int(int) => self.i32(*int),
            CompData::Bool(bool) => self.custom_int(1, *bool as i8),
            CompData::Float(float) => self.f32(*float),
            CompData::Null => self.custom_int(1, 0),
            CompData::Str(str) => self
                .create_array(
                    self.context.i8_type().array_type(str.len() as u32 + 1),
                    (str.chars().chain(['\0']))
                        .map(|x| self.i8(x as i8))
                        .collect::<Vec<_>>(),
                )?
                .as_basic_value_enum(),
            CompData::Func(_) => return Err("Functions should be retrieved seperately".to_string()),
        })
    }

    fn create_array(
        &self,
        arr_ty: ArrayType<'ctx>,
        elements: Vec<BasicValueEnum<'ctx>>,
    ) -> Result<PointerValue<'ctx>, String> {
        let elements = elements.iter();
        let mem = match arr_ty.get_element_type() {
            BasicTypeEnum::IntType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * el.get_bit_width() as usize)?,
                el.array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::FloatType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * 32)?,
                el.array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::PointerType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * 64)?,
                el.array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::ArrayType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::StructType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::VectorType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(elements.len() * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(elements.len() as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
        };
        for (i, element) in elements.enumerate() {
            let ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    mem,
                    &[
                        self.i64(0).into_int_value(),
                        self.i32(i as i32).into_int_value(),
                    ],
                    "array_initialise",
                )
            };
            self.builder.build_store(ptr, *element);
        }
        Ok(mem)
    }

    fn compile_expression(
        &self,
        exp: &ExpEnvironment,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
        parent: Option<&FunctionValue<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        Ok(match exp.expression.as_ref() {
            CompExpression::Call(var, args) => {
                let func = self.module.get_function(&var.name).unwrap();
                let compiled_args = map_vec!(args, |arg| {
                    let val = self
                        .compile_expression(arg, variables, parent)
                        .expect("Cannot convert to result yet");
                    if arg.result_type.is_str() {
                        self.builder
                            .build_pointer_cast(
                                val.into_pointer_value(),
                                self.context
                                    .i8_type()
                                    .ptr_type(inkwell::AddressSpace::Generic),
                                "cast",
                            )
                            .as_basic_value_enum()
                    } else {
                        val
                    }
                });
                let argv = map_vec!(compiled_args, |x| BasicMetadataValueEnum::from(*x));
                self.builder
                    .build_call(func, argv.as_slice(), &var.name)
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| "Invalid function call produced".to_string())?
            }
            CompExpression::BinOp(op, left, right) => {
                let lhs = self.compile_expression(left, variables, parent)?;
                let rhs = self.compile_expression(right, variables, parent)?;
                self.comp_bin_op(op, lhs, rhs, parent)?
            }
            CompExpression::Read(var) => {
                if var.typing.is_primitive() {
                    self.load_variable(variables, &var.name)
                } else {
                    variables.get(&var.name).unwrap().as_basic_value_enum()
                }
            }
            CompExpression::Assign(var, exp) => match var.expression.as_ref() {
                CompExpression::Read(var) => match exp.expression.as_ref() {
                    CompExpression::Value(CompData::Func(func)) => {
                        self.create_function(func, &var.name)?;
                        // TODO: Is this needed
                        self.custom_int(1, 0)
                    }
                    _ => {
                        let val = self.compile_expression(exp, variables, parent)?;
                        let var_ptr = *variables.get(&var.name).unwrap();
                        if val.is_pointer_value() && !var.typing.is_union() {
                            variables.insert(var.name.clone(), val.into_pointer_value());
                            return Ok(val);
                        }
                        if var.typing.is_union() {
                            let ty = &exp.result_type;
                            if !ty.is_union() {
                                let discriminant = self.i32(ty.get_discriminant() as i32);
                                let val = if val.is_pointer_value() {
                                    self.builder
                                        .build_ptr_to_int(
                                            val.into_pointer_value(),
                                            self.context.i32_type(),
                                            "",
                                        )
                                        .as_basic_value_enum()
                                } else {
                                    self.builder.build_bitcast(
                                        val,
                                        self.context.i32_type().as_basic_type_enum(),
                                        "",
                                    )
                                };
                                self.builder.build_store(
                                    self.builder.build_struct_gep(var_ptr, 0, "").unwrap(),
                                    discriminant,
                                );
                                self.builder.build_store(
                                    self.builder.build_struct_gep(var_ptr, 1, "").unwrap(),
                                    val,
                                );
                            } else {
                                self.builder.build_store(var_ptr, val);
                            }
                        } else {
                            self.builder.build_store(var_ptr, val);
                        }
                        val
                    }
                },
                CompExpression::Index(arr, index) => match *exp.expression {
                    CompExpression::Value(CompData::Func(_)) => {
                        return Err("Cannot yet store functions in an array".to_string())
                    }
                    _ => {
                        let mut val = self.compile_expression(exp, variables, parent)?;
                        if arr.result_type.is_str() {
                            val = self.cast_to_i8(val.into_int_value()).as_basic_value_enum();
                        };
                        let index_ptr = self.calc_pos(arr, index, variables, parent)?;
                        self.builder.build_store(index_ptr, val);
                        val
                    }
                },
                _ => return Err(format!("{:?} not supported on lhs of assignment", var)),
            },
            CompExpression::Value(val) => self.get_value(val)?,
            CompExpression::IfElse {
                cond,
                then,
                otherwise,
            } => {
                let cond = self
                    .compile_expression(cond, variables, parent)?
                    .into_int_value();

                let then_bb = self.context.append_basic_block(*parent.unwrap(), "then");
                let else_bb = self.context.append_basic_block(*parent.unwrap(), "else");
                let cont_bb = self.context.append_basic_block(*parent.unwrap(), "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expression(then, variables, parent)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expression(otherwise, variables, parent)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value()
            }
            CompExpression::WhileLoop { cond, body } => {
                // go from current block to loop block
                let loop_bb = self.context.append_basic_block(*parent.unwrap(), "loop");

                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                // emit body
                self.compile_expression(body, variables, parent)?;

                // compile end condition
                let end_cond = self
                    .compile_expression(cond, variables, parent)?
                    .into_int_value();

                let after_bb = self
                    .context
                    .append_basic_block(*parent.unwrap(), "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);
                self.i64(0)
            }
            CompExpression::List(expressions) => {
                let mut last = None;
                for x in expressions {
                    last = Some(self.compile_expression(x, variables, parent)?);
                }
                last.unwrap_or_else(|| self.i32(0))
            }
            CompExpression::Prog(prog) => self.compile_expression(&prog.body, variables, parent)?,
            CompExpression::Index(arr, index) => {
                let ptr = self.calc_pos(arr, index, variables, parent)?;
                let val = self.builder.build_load(ptr, "indexing");
                if arr.result_type.is_str() {
                    self.cast_to_i32(val.into_int_value()).as_basic_value_enum()
                } else {
                    val
                }
            }
            CompExpression::DotAccess(val, key) => {
                let keys = match &val.result_type {
                    CompType::Struct(data) => data,
                    _ => unreachable!(),
                };
                let index = keys.clone().iter().position(|x| x.0 == key.0).unwrap();
                let val = self.compile_expression(val, variables, parent)?;
                let ptr = self
                    .builder
                    .build_struct_gep(val.into_pointer_value(), index as u32, "")
                    .unwrap();
                self.builder.build_load(ptr, "")
            }
            CompExpression::Typeof(exp) => {
                // TODO: Optimise away once we introduce purity specifiers
                let res_exp = self.compile_expression(exp, variables, parent)?;
                if exp.result_type.is_union() {
                    self.extract_element(res_exp.into_struct_value(), 0)
                } else {
                    self.i32(exp.result_type.get_discriminant() as i32)
                }
            }
            CompExpression::Struct(data) => {
                let mem = self.builder.build_pointer_cast(
                    // TODO: Actually calculate it instead of guessing
                    self.alloc_heap(data.len() * 4)?,
                    exp.result_type
                        .get_compiler_type(self.context)?
                        .into_pointer_type(),
                    "",
                );
                if let CompType::Struct(keys) = &exp.result_type {
                    let raw_data: Vec<BasicValueEnum<'ctx>> = keys
                        .iter()
                        .map(|(k, _)| {
                            self.compile_expression(
                                &data.iter().find(|x| &x.0 .0 == k).unwrap().1,
                                variables,
                                parent,
                            )
                        })
                        .collect::<Result<_, _>>()?;
                    for (i, element) in raw_data.iter().enumerate() {
                        let ptr = self
                            .builder
                            .build_struct_gep(mem, i as u32, "struct_initialise")
                            .unwrap();
                        self.builder.build_store(ptr, *element);
                    }
                    mem.as_basic_value_enum()
                } else {
                    unreachable!()
                }
            }
            CompExpression::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|x| self.compile_expression(x, variables, parent))
                    .collect::<Result<_, _>>()?;
                let arr_ty = exp
                    .result_type
                    .get_compiler_type(self.context)?
                    .into_pointer_type()
                    .get_element_type()
                    .into_array_type();
                self.create_array(arr_ty, elements)?.as_basic_value_enum()
            }
            other => return Err(format!("Not implemented '{:?}'", other)),
        })
    }

    fn create_function(
        &self,
        func: &FunctionAst,
        name: &str,
    ) -> Result<FunctionValue<'ctx>, String> {
        let error = format!("Expected fn {} to have been added before compiling", name);
        let fn_val = if let Some(val) = self.module.get_function(name) {
            val
        } else {
            return Err(error);
        };
        if func.body == None {
            return Ok(fn_val);
        }

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        // build variables map
        let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let arg_name = func.arguments[i].name.as_str();
            let ty = func.arguments[i].typing.get_compiler_type(self.context)?;
            let var = self.add_variable_to_block(arg_name, ty, &fn_val);
            if func.arguments[i].typing.is_primitive() {
                self.builder.build_store(var, arg);
                variables.insert(arg_name.to_string(), var);
            } else {
                variables.insert(arg_name.to_string(), arg.into_pointer_value());
            }
        }
        let arg_names = variables.clone().keys().cloned().collect::<Vec<_>>();
        for x in
            (func.body.clone().unwrap().scope.variables.iter()).filter(|x| !arg_names.contains(x.0))
        {
            let ty = x.1.typing.clone();
            let comp_type = ty.as_ref().unwrap().get_compiler_type(self.context)?;
            let name = x.0.to_string();
            let var = self.add_variable_to_block(&name, comp_type, &fn_val);
            variables.insert(name, var);
        }
        // compile body
        let body = self.compile_expression(
            &func.body.clone().unwrap().body,
            &mut variables,
            Some(&fn_val),
        )?;
        match func.return_type {
            CompType::Null => self.builder.build_return(Some(&self.custom_int(1, 0))),
            _ => self.builder.build_return(Some(&body)),
        };

        // return the whole thing after verification and optimization

        if !fn_val.verify(true) {
            self.module.print_to_stderr();
            return Err("Invalid generated function.".to_string());
        }
        self.fpm.run_on(&fn_val);
        Ok(fn_val)
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

    pub fn create_function_shape(&self, func: &CompType) -> Result<FunctionType<'ctx>, String> {
        if let CompType::Callible(arguments, return_type) = func {
            let args_types: Vec<BasicMetadataTypeEnum> = arguments
                .iter()
                .map(|x| x.get_compiler_type(self.context).map(|x| x.into()))
                .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?;
            let args_types = args_types.as_slice();

            Ok(return_type
                .get_compiler_type(self.context)?
                .fn_type(args_types, false))
        } else {
            Err(format!("Must be a function, not '{}'", func.get_str()))
        }
    }
}

pub fn compile(ast: &Program, settings: Settings) -> Result<(), String> {
    let ctx = Context::create();
    let module = ctx.create_module("repl");
    let builder = ctx.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);
    if settings.skip_optimizations {
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
    };
    fpm.initialize();
    // make module
    let module = ctx.create_module("module");
    let compiler = Compiler {
        context: &ctx,
        builder: &builder,
        fpm: &fpm,
        module: &module,
    };
    for (name, var) in ast.scope.clone().variables {
        if var.typing.as_ref().unwrap().is_callable() {
            let fn_val = compiler.create_function_shape(&var.typing.clone().unwrap())?;
            compiler.module.add_function(&name, fn_val, None);
        }
    }

    compiler
        .compile_expression(&ast.body, &mut HashMap::new(), None)
        .unwrap();

    Target::initialize_native(&InitializationConfig::default())?;
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let path = Path::new(&settings.object_name);
    let target =
        Target::from_name("x86-64").ok_or_else(|| "Could not find target from name".to_string())?;
    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .ok_or_else(|| "Couldnot make target_machine".to_string())?;

    target_machine
        .write_to_file(compiler.module, FileType::Object, path)
        .map_err(|x| x.to_string())?;
    if settings.print_llvm {
        module.print_to_stderr();
    }
    Ok(())
}
