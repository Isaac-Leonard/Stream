use crate::ast1::*;
use crate::ast2::*;
use crate::errors::CompError;
use crate::map_vec;

use std::hash::Hasher;

#[derive(Debug, PartialEq, Clone)]
pub struct Accesses {
	pub variable: CompVariable,
	pub read: u32,
	pub write: u32,
	pub capture: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRVariable {
	ty: CompType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Readable {
	Variable(IRVariable),
	Value(ConstantData),
	Property(IRVariable, u32),
	Index(IRVariable, IRVariable),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignable {
	Variable(IRVariable),
	Property(IRVariable, u32),
	Index(IRVariable, IRVariable),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRExpression {
	Typeof(Readable),
	BinOp(Op, Readable, Readable),
	Call(Readable, Vec<Readable>),
	Assign(Assignable, Readable),
	IfElse(Readable, Vec<Self>, Vec<Self>),
	WhileLoop(Readable, Vec<Self>),
	Conversion(Readable, CompType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IndexOption {
	Index(ExpEnvironment),
	Dot(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemoryLocation {
	pub variable: CompVariable,
	pub accessing: Vec<(IndexOption, CompType)>,
}
fn count_max_references(func: &FunctionAst) -> Vec<Accesses> {
	if let Some(env) = &func.body {
		let mut accesses = Vec::new();
		count_max_references_in_env(&env.body, &mut accesses);
		accesses
	} else {
		let args = func.arguments.clone();
		map_vec!(args, |variable| Accesses {
			variable: variable.clone(),
			read: 0,
			write: 0,
			capture: 1,
		})
	}
}
fn count_max_references_in_env(env: &ExpEnvironment, accesses: &mut Vec<Accesses>) {
	use CompExpression::*;
	match env.expression.as_ref() {
		List(envs) => {
			for env in envs {
				count_max_references_in_env(env, accesses)
			}
		}
		IfElse(ifelse) => {
			count_max_references_in_env(&ifelse.cond, accesses);
			let mut then = Vec::new();
			count_max_references_in_env(&ifelse.then, &mut then);
			let mut otherwise = Vec::new();
			count_max_references_in_env(&ifelse.otherwise, &mut otherwise);
			for access in then.into_iter().chain(otherwise) {
				let existing = accesses
					.iter_mut()
					.find(|access2| access2.variable == access.variable);
				if let Some(access2) = existing {
					access2.capture = std::cmp::max(access.capture, access2.capture);
					access2.write = std::cmp::max(access.write, access2.write);
					access2.read = std::cmp::max(access.read, access2.read);
				} else {
					accesses.push(access.clone())
				}
			}
		}
		Read(var) => {
			let exists = accesses.iter_mut().find(|access| &access.variable == var);
			if var.get_type().is_primitive() {
				if let Some(access) = exists {
					access.read += 1;
				} else {
					accesses.push(Accesses {
						variable: var.clone(),
						read: 1,
						write: 0,
						capture: 0,
					})
				}
			} else if let Some(access) = exists {
				access.capture += 1;
			} else {
				accesses.push(Accesses {
					variable: var.clone(),
					read: 0,
					write: 0,
					capture: 1,
				})
			}
		}
		WhileLoop { cond, body } => {
			count_max_references_in_env(cond, accesses);
			count_max_references_in_env(body, accesses)
		}
		BinOp(_, a, b) => {
			count_max_references_in_env(a, accesses);
			count_max_references_in_env(b, accesses)
		}
		OneOp(_, exp) => count_max_references_in_env(exp, accesses),
		Typeof(exp) => count_max_references_in_env(exp, accesses),
		Conversion(exp, _) => count_max_references_in_env(exp, accesses),
		DotAccess(exp, _) => count_max_references_in_env(exp, accesses),
		Index(arr, idx) => {
			count_max_references_in_env(arr, accesses);
			count_max_references_in_env(idx, accesses)
		}
		Array(envs) => {
			for env in envs {
				count_max_references_in_env(env, accesses)
			}
		}
		Struct(envs) => {
			for env in envs {
				count_max_references_in_env(&env.1 .1, accesses)
			}
		}
		Call(func, _, envs) => {
			if let Some(access) = accesses.iter_mut().find(|access| &access.variable == func) {
				access.read += 1;
			} else {
				accesses.push(Accesses {
					variable: func.clone(),
					read: 1,
					write: 0,
					capture: 0,
				})
			}
			for env in envs {
				count_max_references_in_env(env, accesses)
			}
		}
		Value(CompData::Func(_)) => panic!("Cannot reference count closures at this time"),
		Value(_) => {}
		Assign(lvalue, rhs) => {
			count_max_references_in_env(rhs, accesses);
			for access in &lvalue.accessing {
				if let IndexOption::Index(env) = &access.0 {
					count_max_references_in_env(env, accesses)
				}
			}
			if let Some(access) = accesses
				.iter_mut()
				.find(|access| access.variable == lvalue.variable)
			{
				access.write += 1;
			} else {
				accesses.push(Accesses {
					variable: lvalue.variable.clone(),
					read: 0,
					write: 1,
					capture: 0,
				})
			}
		}
	}
}

pub fn resolve_memory(
	mut exp: &SpannedExpression,
	env: &ExpEnvironment,
	scope: &mut Scope,
	file: &str,
) -> (Option<MemoryLocation>, Vec<CompError>) {
	let mut accesses = Vec::new();
	let mut errs = Vec::new();
	loop {
		match &exp.1 {
			Expression::Index(left, index) => {
				accesses.push(IndexOption::Index(
					crate::ast2::transform_exp(index, env, scope, file).0,
				));
				exp = left.as_ref();
			}
			Expression::DotAccess(left, prop) => {
				accesses.push(IndexOption::Dot(prop.0.clone()));
				exp = left.as_ref();
			}
			Expression::Terminal(Symbol::Identifier(ref name)) => {
				let var = scope.get_variable(name);
				return if let Ok(var) = var {
					accesses.reverse();
					(
						Some(MemoryLocation {
							variable: var,
							accessing: map_vec!(accesses, |x| (x.clone(), CompType::Unknown)),
						}),
						errs,
					)
				} else {
					errs.push(CompError::CannotFindVariable(name.clone(), exp.0.clone()));
					(None, errs)
				};
			}
			_ => return (None, errs),
		}
	}
}

impl FunctionAst {
	#[warn(clippy::unimplemented)]
	fn replace_all_constant_generic_calls(&mut self) {
		if let Some(prog) = self.body.as_mut() {
			prog.body.map_inplace(&mut |x| match x.expression.as_ref() {
				CompExpression::Call(_name, _generics, _args) => Some(x.clone()),
				_ => None,
			})
		};
	}
}
