use crate::settings::Settings;
use crate::{ast::*, map_vec};
use fxhash::hash32;
use inkwell::builder::Builder;
use inkwell::context::Context;

use inkwell::debug_info::{AsDIScope, DICompileUnit, DebugInfoBuilder};
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{
    AnyTypeEnum, ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{
    AggregateValue, BasicMetadataValueEnum, BasicValueEnum, CallableValue, FloatMathValue,
    IntMathValue, IntValue, PointerValue,
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
            Callible(args, ret) => {
                let args_types: Vec<BasicMetadataTypeEnum> = args
                    .iter()
                    .map(|x| x.get_compiler_type(context).map(|x| x.into()))
                    .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?;
                let args_types = args_types.as_slice();

                ret.get_compiler_type(context)?
                    .fn_type(args_types, false)
                    .ptr_type(inkwell::AddressSpace::Generic)
                    .as_basic_type_enum()
            }
            Array(ty, len) => ty
                .get_compiler_type(context)?
                .array_type(len as u32)
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Type => context.i32_type().as_basic_type_enum(),
            Int => context.i32_type().as_basic_type_enum(),
            Char => context.i8_type().as_basic_type_enum(),
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
    pub dibuilder: DebugInfoBuilder<'ctx>,
    pub compile_unit: DICompileUnit<'ctx>,
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
        if let Some(var) = variables.get(var) {
            self.builder.build_load(*var, "load")
        } else if let Some(func) = self.module.get_function(var) {
            func.as_global_value().as_basic_value_enum()
        } else {
            panic!("Tried to load nonexistent variable {}", var)
        }
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
                (AnyTypeEnum::PointerType(_), AnyTypeEnum::PointerType(_))
                | (AnyTypeEnum::ArrayType(_), AnyTypeEnum::ArrayType(_))
                | (AnyTypeEnum::PointerType(_), AnyTypeEnum::ArrayType(_))
                | (AnyTypeEnum::ArrayType(_), AnyTypeEnum::PointerType(_)) => self
                    .comp_bin_op_str(
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
                .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, "greaterthan")
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
            CompData::Str(str) => {
                unsafe { self.builder.build_global_string(str, "string_ptr") }.as_basic_value_enum()
            }
            CompData::Func(_) => return Err("Functions should be retrieved seperately".to_string()),
        })
    }

    fn create_array(
        &self,
        arr_ty: ArrayType<'ctx>,
        len: usize,
    ) -> Result<PointerValue<'ctx>, String> {
        let mem = match arr_ty.get_element_type() {
            BasicTypeEnum::IntType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * el.get_bit_width() as usize)?,
                el.array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::FloatType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * 32)?,
                el.array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::PointerType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * 64)?,
                el.array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::ArrayType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::StructType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
            BasicTypeEnum::VectorType(el) => self.builder.build_pointer_cast(
                self.alloc_heap(len * 64)?,
                el.ptr_type(inkwell::AddressSpace::Generic)
                    .array_type(len as u32)
                    .ptr_type(inkwell::AddressSpace::Generic),
                "",
            ),
        };
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
                let func = self
                    .load_variable(variables, &var.get_name())
                    .into_pointer_value();
                self.builder
                    .build_call(
                        CallableValue::try_from(func).unwrap(),
                        argv.as_slice(),
                        &var.get_name(),
                    )
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| "Invalid function call produced".to_string())?
            }
            CompExpression::BinOp(op, left, right) => {
                let lhs = self.compile_expression(left, variables, parent)?;
                let rhs = self.compile_expression(right, variables, parent)?;
                self.comp_bin_op(op, lhs, rhs, parent)?
            }
            CompExpression::Read(var) => self.load_variable(variables, &var.get_name()),
            CompExpression::Conversion(exp, ty) => {
                let val = self.compile_expression(exp, variables, parent)?;
                // Assume only int and char conversions for now
                // TODO: implement properly
                if ty.is_int() {
                    self.cast_to_i32(val.into_int_value())
                } else {
                    self.cast_to_i8(val.into_int_value())
                }
                .as_basic_value_enum()
            }
            CompExpression::Assign(mem, exp) => {
                if mem.accessing.is_empty() {
                    if let CompExpression::Value(CompData::Func(func)) = exp.expression.as_ref() {
                        self.create_function(func, &mem.variable.get_name())?;
                        // TODO: Is this needed
                        return Ok(self.custom_int(1, 0));
                    }
                } else {
                    if let CompExpression::Value(CompData::Func(_)) = *exp.expression {
                        return Err("Cannot yet store functions in an array".to_string());
                    };
                }
                let val = self.compile_expression(exp, variables, parent)?;
                let mut mem_ptr = *variables.get(&mem.variable.get_name()).unwrap();
                let mut mem_ty = mem.variable.get_type();
                for (access, ty) in &mem.accessing {
                    let ptr = self
                        .builder
                        .build_load(mem_ptr, "load")
                        .into_pointer_value();
                    mem_ty = ty.clone();
                    match access {
                        IndexOption::Index(index) => {
                            let index = self.compile_expression(index, variables, parent)?;
                            mem_ptr = unsafe {
                                self.builder.build_in_bounds_gep(
                                    ptr,
                                    &[self.i32(0).into_int_value(), index.into_int_value()],
                                    "calc_pos",
                                )
                            };
                        }
                        IndexOption::Dot(prop) => {
                            let index = if let CompType::Struct(data) = &mem_ty {
                                data.iter()
                                    .position(|x| &x.0 == prop)
                                    .ok_or_else(|| "Failed to get prop of struct".to_string())?
                            } else {
                                return Err("attempted to get prop of non-struct".to_string());
                            };
                            mem_ptr = self
                                .builder
                                .build_struct_gep(ptr, index as u32, "calc_pos")
                                .map_err(|_| "Failed to assign struct".to_string())?;
                        }
                    }
                }

                if mem_ty.is_union() {
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
                            self.builder.build_struct_gep(mem_ptr, 0, "").unwrap(),
                            discriminant,
                        );
                        self.builder.build_store(
                            self.builder.build_struct_gep(mem_ptr, 1, "").unwrap(),
                            val,
                        );
                    } else {
                        self.builder.build_store(mem_ptr, val);
                    }
                } else {
                    self.builder.build_store(mem_ptr, val);
                }
                val
            }
            CompExpression::Value(val) => self.get_value(val)?,
            CompExpression::IfElse(if_exp) => {
                let cond = self
                    .compile_expression(&if_exp.cond, variables, parent)?
                    .into_int_value();

                let then_bb = self.context.append_basic_block(*parent.unwrap(), "then");
                let else_bb = self.context.append_basic_block(*parent.unwrap(), "else");
                let cont_bb = self.context.append_basic_block(*parent.unwrap(), "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expression(&if_exp.then, variables, parent)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expression(&if_exp.otherwise, variables, parent)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value()
            }
            CompExpression::WhileLoop { cond, body } => {
                let loop_bb = self.context.append_basic_block(*parent.unwrap(), "loop");
                // compile end condition
                let end_cond = self
                    .compile_expression(cond, variables, parent)?
                    .into_int_value();
                let after_bb = self
                    .context
                    .append_basic_block(*parent.unwrap(), "afterloop");
                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);

                self.builder.position_at_end(loop_bb);

                // emit body
                self.compile_expression(body, variables, parent)?;
                // compile end condition
                let end_cond = self
                    .compile_expression(cond, variables, parent)?
                    .into_int_value();

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
            CompExpression::Index(arr, index) => {
                let ptr = self.calc_pos(arr, index, variables, parent)?;
                self.builder.build_load(ptr, "indexing")
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
        let prog = if let Some(prog) = func.body.as_ref() {
            prog
        } else {
            return Ok(fn_val);
        };
        let ditype = self
            .dibuilder
            .create_basic_type(
                &format!("{}", func.return_type),
                0_u64,
                0x00,
                inkwell::debug_info::DIFlagsConstants::PUBLIC,
            )
            .unwrap();
        let subroutine_type = self.dibuilder.create_subroutine_type(
            self.compile_unit.get_file(),
            /* return type */ Some(ditype.as_type()),
            /* parameter types */ &[],
            inkwell::debug_info::DIFlagsConstants::PUBLIC,
        );
        let func_scope = self.dibuilder.create_function(
            self.compile_unit.as_debug_info_scope(),
            name,
            None,
            self.compile_unit.get_file(),
            0,
            /* DIType */ subroutine_type,
            /* is_local_to_unit */ true,
            /* is_definition */ true,
            /* scope_line */ 0,
            /* flags */ inkwell::debug_info::DIFlagsConstants::PUBLIC,
            /* is_optimized */ false,
        );
        fn_val.set_subprogram(func_scope);
        let lexical_block = self.dibuilder.create_lexical_block(
            /* scope */ func_scope.as_debug_info_scope(),
            /* file */ self.compile_unit.get_file(),
            /* line_no */ 0,
            /* column_no */ 0,
        );

        let loc = self.dibuilder.create_debug_location(
            &self.context,
            /* line */ 0,
            /* column */ 0,
            /* current_scope */ lexical_block.as_debug_info_scope(),
            /* inlined_at */ None,
        );
        self.builder.set_current_debug_location(&self.context, loc);

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        // build variables map
        let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let arg_name = func.arguments[i].get_name();
            let ty = func.arguments[i]
                .get_type()
                .get_compiler_type(self.context)?;
            let var = self.add_variable_to_block(&arg_name, ty, &fn_val);
            self.builder.build_store(var, arg);
            variables.insert(arg_name.to_string(), var);
        }
        for x in prog.body.get_all_written_variables() {
            if !variables.contains_key(x.get_name().as_str()) {
                let ty = x.get_type();
                let comp_type = ty.get_compiler_type(self.context)?;
                let name = x.get_name();
                let var = self.add_variable_to_block(&name, comp_type, &fn_val);
                variables.insert(name, var);
            }
        }

        for x in prog.body.get_all_written_variables() {
            if x.get_name().starts_with(".array") {
                let ptr = *variables.get(&x.get_name()).unwrap();
                let ty = x.get_type();
                let comp_ty = ty
                    .get_compiler_type(self.context)
                    .unwrap()
                    .into_pointer_type()
                    .get_element_type()
                    .into_array_type();
                let len = if let CompType::Array(_, len) = ty {
                    len
                } else {
                    panic!("Exprected array here, wtf")
                };
                let val = self.create_array(comp_ty, len as usize)?;
                self.builder.build_store(ptr, val);
            }
        }
        // compile body
        let body = self.compile_expression(&prog.body, &mut variables, Some(&fn_val))?;
        match func.return_type {
            CompType::Null => self.builder.build_return(Some(&self.custom_int(1, 0))),
            _ => self.builder.build_return(Some(&body)),
        };

        // return the whole thing after verification and optimization
        self.dibuilder.finalize();
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
    let debug_metadata_version = ctx.i32_type().const_int(3, false);
    module.add_basic_value_flag(
        "Debug Info Version",
        inkwell::module::FlagBehavior::Warning,
        debug_metadata_version,
    );
    let (dibuilder, compile_unit) = module.create_debug_info_builder(
        true,
        /* language */ inkwell::debug_info::DWARFSourceLanguage::C,
        /* filename */ &settings.input_name,
        /* directory */ ".",
        /* producer */ "Stream compiler",
        /* is_optimized */ !settings.skip_optimizations,
        /* compiler command line flags */ "Fill in later",
        /* runtime_ver */ 0,
        /* split_name */ "",
        /* kind */ inkwell::debug_info::DWARFEmissionKind::Full,
        /* dwo_id */ 0,
        /* split_debug_inling */ false,
        /* debug_info_for_profiling */ false,
        // Leaving these empty
        // they're not mentioned in the docs and only exist for later versions of llvm so not sure what to provide
        "",
        "",
    );

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
        dibuilder,
        compile_unit,
    };

    for (name, var) in ast.scope.clone().variables {
        if var.get_type().is_callable() {
            let fn_val = compiler.create_function_shape(&var.get_type().clone())?;
            compiler.module.add_function(&name, fn_val, None);
        }
    }

    compiler.compile_expression(&ast.body, &mut HashMap::new(), None)?;

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
