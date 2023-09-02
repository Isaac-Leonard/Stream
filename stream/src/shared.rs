use crate::ast1::*;
use crate::ast2::*;
use crate::ast3::*;
use crate::errors::*;
use crate::map_vec;
use crate::settings::Settings;

use std::ops::Range;

pub fn transform_type(ty: &CustomType, scope: &Scope) -> (CompType, Vec<CompError>) {
	let mut errors = Vec::new();
	macro_rules! get_type {
		($ty:expr) => {{
			let mut res = transform_type($ty, &scope);
			errors.append(&mut res.1);
			res.0
		}};
	}

	let ty = match ty {
		CustomType::Struct(data) => CompType::Struct(
			data.iter()
				.map(|(key, value)| (key.clone(), get_type!(value)))
				.collect(),
		),
		CustomType::Array(el_ty, len) => {
			let el_ty = get_type!(el_ty);
			let len = get_type!(len);
			// TODO: replace len with an actual type and check it resolves to an Int
			CompType::Array(el_ty.boxed(), len.boxed())
		}
		CustomType::Union(sub_types) => {
			CompType::Union(map_vec!(sub_types, |x| get_type!(x))).flatten()
		}

		CustomType::Callible(generics, args, ret) => {
			let generics = map_vec!(generics, |(name, ty)| (
				name,
				ty.as_ref()
					.map(|ty| get_type!(ty))
					.unwrap_or(CompType::Unknown)
			));
			let mut scope = scope.clone();
			for (pos, (name, ty)) in generics.into_iter().enumerate() {
				eprint!("{}", name);
				scope.add_type(name.to_string(), CompType::Generic(pos, ty.boxed()));
			}
			let args = map_vec!(args, |x| get_type!(x));
			let ret = get_type!(ret);
			CompType::Callible(args, Box::new(ret))
		}
		CustomType::Lone(ty) => {
			let found_type = scope.get_type(&ty.name);
			if let Ok(found_ty) = found_type {
				let generics = map_vec!(ty.generics, |x| get_type!(x));
				// We want to preserve lone generics
				// As in we only want to substitute generics that are part of another type
				// Generics found here are ones that get substituted in themselves
				if !matches!(found_ty, CompType::Generic(_, _)) {
					let (ty, mut errs) = found_ty.substitute_generics(&generics);
					errors.append(&mut errs);
					ty
				} else {
					found_ty
				}
			} else {
				errors.push(CompError::CannotFindType(ty.clone().name, 0..0));
				CompType::Unknown
			}
		}
		CustomType::Constant(data) => CompType::Constant(data.clone()),
	};
	(ty, errors)
}

fn bin_exp(
	op: &Op,
	left: &SpannedExpression,
	right: &SpannedExpression,
	env: &ExpEnvironment,
	scope: &mut Scope,
	file: &str,
) -> (CompExpression, Vec<CompError>) {
	let left = transform_exp(left, env, scope, file);
	let right = transform_exp(right, env, scope, file);
	(
		CompExpression::BinOp(op.clone(), left.0, right.0),
		left.1.into_iter().chain(right.1).collect(),
	)
}

pub fn transform_exp(
	(loc, exp): &SpannedExpression,
	env: &ExpEnvironment,
	scope: &mut Scope,
	file: &str,
) -> (ExpEnvironment, Vec<CompError>) {
	use Expression::*;
	let mut errs = Vec::new();
	macro_rules! get_exp {
		($exp:expr, $env:expr, $scope:expr) => {{
			let mut res = transform_exp($exp, $env, $scope, file);
			errs.append(&mut res.1);
			res.0
		}};
	}

	macro_rules! resolve_memory_location {
		($exp:expr, $env:expr, $scope:expr) => {{
			let mut res = crate::ast3::resolve_memory($exp, $env, $scope, file);
			errs.append(&mut res.1);
			res.0
		}};
	}

	let expression = match exp {
		Struct(data) => CompExpression::Struct(
			data.iter()
				.map(|(key, value)| (key.0.clone(), (key.1.clone(), get_exp!(value, env, scope))))
				.collect(),
		),
		Array(elements) => CompExpression::Array(map_vec!(elements, |x| get_exp!(x, env, scope))),
		DotAccess(val, key) => CompExpression::DotAccess(get_exp!(val, env, scope), key.clone()),
		Conversion(exp, ty) => {
			let (ty, mut errors) = transform_type(ty, scope);
			errs.append(&mut errors);
			CompExpression::Conversion(get_exp!(exp, env, scope), ty)
		}
		Typeof(exp) => CompExpression::Typeof(get_exp!(exp, env, scope)),
		Index(arr, index) => {
			CompExpression::Index(get_exp!(arr, env, scope), get_exp!(index, env, scope))
		}
		TypeDeclaration(_, _, _) => CompExpression::List(Vec::new()),
		InitAssign(_, _, name, _, exp) => {
			if scope.variable_initialised(&name.0) {
				errs.push(CompError::RedeclareInSameScope(name.0.clone(), loc.clone()));
			}

			let exp = get_exp!(exp, env, scope);
			let exp_ty = exp.result_type.clone();
			let has_type = scope.variable_has_type(&name.0);
			if !has_type {
				scope.set_variable_type(&name.0, &exp_ty);
			}
			scope.set_variable_initialised(&name.0);
			let var = scope.get_variable(&name.0);
			let var = if let Ok(var) = var {
				var
			} else {
				errs.push(CompError::CannotFindVariable(name.0.clone(), loc.clone()));
				CompVariable {
					name: name.0.clone(),
					typing: exp_ty.clone(),
					constant: false,
					external: false,
					declared_at: None,
					initialised: false,
				}
			};

			let mem = MemoryLocation {
				variable: var,
				accessing: Vec::new(),
			};
			CompExpression::Assign(mem, exp)
		}
		Assign(name, exp) => {
			let lhs = resolve_memory_location!(name, env, scope);
			let exp = get_exp!(exp, env, scope);
			if let Some(lhs) = lhs {
				CompExpression::Assign(lhs, exp)
			} else {
				errs.push(CompError::InvalidLeftHandForAssignment(
					name.1.clone(),
					name.0.clone(),
				));
				CompExpression::List(Vec::new())
			}
		}
		IfElse(cond, left, right) => {
			let cond = get_exp!(cond, env, scope);
			let then = get_exp!(left, env, scope);
			let otherwise = get_exp!(right, env, scope);
			CompExpression::IfElse(crate::ast2::IfElse {
				cond,
				then,
				otherwise,
			})
		}
		Expression::Loop(exp, body) => {
			let cond = get_exp!(exp, env, scope);
			let body = get_exp!(body, env, scope);
			CompExpression::WhileLoop { cond, body }
		}
		Expression::Block(expressions) => {
			let mut env = env.clone();
			let mut oks = Vec::new();
			for exp in expressions {
				env = get_exp!(exp, &env, scope);
				oks.push(env.clone());
			}
			CompExpression::List(oks)
		}
		Expression::BinOp(op, l, r) => {
			let mut exp = bin_exp(op, l.as_ref(), r, env, scope, file);
			errs.append(&mut exp.1);
			exp.0
		}
		Expression::FuncCall(name, generics, arguments) => {
			let mut args = Vec::new();
			let mut env = env.clone();
			for exp in arguments {
				env = get_exp!(exp, &env, scope);
				args.push(env.clone());
			}
			let func = if let Ok(var) = scope.get_variable(name) {
				var
			} else {
				errs.push(CompError::CannotFindVariable(name.clone(), loc.clone()));
				CompVariable {
					name: name.clone(),
					constant: false,
					external: false,
					typing: CompType::Unknown,
					declared_at: None,
					initialised: false,
				}
			};
			let generics = map_vec!(generics, |x| {
				let (ty, mut errors) = transform_type(x, scope);
				errs.append(&mut errors);
				ty
			});
			CompExpression::Call(func, generics, args)
		}
		Expression::Terminal(sym) => match sym {
			Symbol::Identifier(name) => {
				let var = if let Ok(var) = scope.get_variable(name) {
					var
				} else {
					errs.push(CompError::CannotFindVariable(name.clone(), loc.clone()));
					CompVariable {
						name: name.clone(),
						constant: false,
						external: false,
						typing: CompType::Unknown,
						declared_at: None,
						initialised: false,
					}
				};
				CompExpression::Read(var)
			}
			Symbol::Data(data) => CompExpression::Value(match data.clone() {
				RawData::Int(val) => CompData::Int(val),
				RawData::Float(val) => CompData::Float(val),
				RawData::Str(val) => CompData::Str(val),
				RawData::Bool(val) => CompData::Bool(val),
				RawData::Null => CompData::Null,
				RawData::Func(func) => {
					let (func, mut errors) = transform_function(&func, scope, file);
					errs.append(&mut errors);
					CompData::Func(func)
				}
			}),
		},
		Expression::Invalid => panic!("invalid {:?}", loc),
	};
	get_env(expression, env, loc.clone(), errs)
}

fn resolve_scope<'a>(
	(_, ast): &SpannedExpression,
	scope: &'a mut Scope,
	file: &str,
) -> &'a mut Scope {
	// TODO: Sort out errors and stuff here
	match ast {
		Expression::TypeDeclaration(name, generics, declared_type) => {
			if scope.get_type(name).is_err() {
				let mut subscope = scope.clone();
				for (pos, (name, constraint)) in generics.iter().enumerate() {
					let constraint = match constraint {
						Some(ty) => {
							// TODO: Hacky error reporting until we clean this part up
							let (ty, errs) = transform_type(ty, scope);
							if !errs.is_empty() {
								eprintln!("{:?}", errs);
							}
							ty
						}
						None => CompType::Unknown,
					};
					subscope.add_type(name.clone(), CompType::Generic(pos, constraint.boxed()));
				}
				let (ty, errors) = transform_type(declared_type, &subscope);
				if !errors.is_empty() {
					eprintln!("{:?}", errors);
				}
				scope.add_type(name.clone(), ty);
			}
			scope
		}
		Expression::InitAssign(external, constant, name, declared_type, _exp) => {
			if scope.variable_exists(&name.0) {
				scope
			} else {
				let typing = match declared_type {
					None => CompType::Unknown,
					Some(x) => transform_type(x, scope).0,
				};
				scope.add_variable(CompVariable {
					name: name.0.clone(),
					constant: *constant,
					typing,
					initialised: false,
					external: *external,
					declared_at: Some((file.to_string(), name.1.clone())),
				})
			}
		}
		Expression::Block(expressions) => {
			for exp in expressions {
				resolve_scope(exp, scope, file);
			}
			scope
		}
		_x => scope,
	}
}

fn get_env_from_scope(_scope: &Scope) -> ExpEnvironment {
	ExpEnvironment {
		result_type: CompType::Null,
		located: 0..0,
		expression: Box::new(CompExpression::List(Vec::new())),
	}
}

fn transform_ast(
	ast: &SpannedExpression,
	scope: &mut Scope,
	file: &str,
) -> (Program, Vec<CompError>) {
	let env = get_env_from_scope(scope);
	let mut expression = transform_exp(ast, &env, scope, file);
	let mut prog = Program {
		scope: scope.clone(),
		body: expression.0,
	};
	prog.body.replace_arrays();
	let functions = prog.body.get_functions();
	for func in functions {
		if let Some(prog2) = &func.body {
			let variables = prog2.body.get_all_mentioned_variables();
			let functions = prog2.body.get_functions();
			for sub_func in functions {
				if let Some(sub_prog) = &sub_func.body {
					let sub_variables = sub_prog.body.get_all_mentioned_variables();
					if sub_variables.iter().any(|x| variables.contains(x)) {
						expression.1.push(CompError::NotImplemented(
							"Closures that capture variables are not currently implemented"
								.to_string(),
							0..0,
						))
					}
				}
			}
		}
	}
	(prog, expression.1)
}

pub fn create_program(
	ast: &SpannedExpression,
	scope: &mut Scope,
	settings: &Settings,
) -> (Program, Vec<CompError>) {
	resolve_scope(ast, scope, &settings.input_name);
	transform_ast(ast, scope, &settings.input_name)
}

fn get_top_type(types: &[CompType]) -> CompType {
	use CompType::*;
	let mut highest_types = Vec::new();
	for ty in types {
		highest_types.push(match ty {
			Int | Constant(ConstantData::Int(_)) => Int,
			Float | Constant(ConstantData::Float(_)) => Float,
			Null | Constant(ConstantData::Null) => Null,
			Bool | Constant(ConstantData::Bool(_)) => Bool,
			Constant(ConstantData::Str(str)) => {
				Str(ConstantData::Int(str.len() as i32).to_type().boxed())
			}
			x => x.clone(),
		});
	}
	Union(highest_types).flatten()
}

pub fn get_env(
	mut exp: CompExpression,
	env: &ExpEnvironment,
	located: Range<usize>,
	errs: Vec<CompError>,
) -> (ExpEnvironment, Vec<CompError>) {
	let (ty, errs) = resolve_type(&mut exp, env, located.clone(), errs);
	(
		ExpEnvironment {
			expression: Box::new(exp.clone()),
			result_type: ty,
			located,
		},
		errs,
	)
}

pub fn get_shared_access_types(types: &[CompType], key: &str) -> Vec<CompType> {
	types
		.iter()
		.filter_map(|x| match x {
			CompType::Struct(fields) => fields.get(key).cloned(),
			_ => None,
		})
		.collect::<Vec<_>>()
}

pub fn resolve_type(
	exp: &mut CompExpression,
	_env: &ExpEnvironment,
	located: Range<usize>,
	mut errs: Vec<CompError>,
) -> (CompType, Vec<CompError>) {
	use CompExpression::*;
	match exp {
		Conversion(_exp, ty) => (ty.clone(), errs),
		DotAccess(val, (key, _)) => {
			if let CompType::Union(types) = &val.result_type {
				let union = get_shared_access_types(types, key);
				let result_type = if union.len() != types.len() {
					errs.push(CompError::MissingPropertyInUnion(
						key.clone(),
						val.result_type.clone(),
						located,
					));
					CompType::Unknown
				} else {
					CompType::Union(union)
				};
				(result_type, errs)
			} else if let CompType::Struct(fields) = &val.result_type {
				let result_type = if let Some(ty) = fields.get(key) {
					ty.clone()
				} else {
					errs.push(CompError::PropertyDoesNotExistOnType(
						key.clone(),
						val.result_type.clone(),
						located,
					));
					CompType::Unknown
				};
				(result_type, errs)
			} else if let CompType::Array(_, len) = &val.result_type {
				let result_type = if key == "length" {
					len.as_ref().clone()
				} else {
					errs.push(CompError::PropertyDoesNotExistOnType(
						key.clone(),
						val.result_type.clone(),
						located,
					));
					CompType::Unknown
				};
				(result_type, errs)
			} else {
				errs.push(CompError::PropertyDoesNotExistOnType(
					key.clone(),
					val.result_type.clone(),
					located,
				));
				(CompType::Unknown, errs)
			}
		}
		Struct(fields) => {
			let result_type = CompType::Struct(
				fields
					.iter()
					.map(|(k, v)| (k.clone(), v.1.result_type.clone()))
					.collect(),
			)
			.flatten();
			(result_type, errs)
		}
		Array(elements) => {
			let el_types = map_vec!(elements, |el| el.result_type.widen());
			// TODO: Fix this as its hacky and will cause a crash
			(
				CompType::Array(
					el_types
						.first()
						.cloned()
						.unwrap_or(CompType::Unknown)
						.boxed(),
					CompType::Constant(ConstantData::Int(elements.len() as i32)).boxed(),
				),
				errs,
			)
		}

		Typeof(_) => (CompType::Type, errs),
		List(exps) => (
			exps.last()
				.map(|x| x.result_type.clone())
				.unwrap_or(CompType::Null),
			errs,
		),
		WhileLoop { cond, body } => {
			if !cond.result_type.is_bool() {
				errs.push(CompError::BoolInWhile(
					cond.result_type.clone(),
					cond.located.clone(),
				));
			}
			(body.result_type.clone(), errs)
		}
		IfElse(if_exp) => {
			if !if_exp.cond.result_type.is_bool() {
				errs.push(CompError::BoolInIf(
					if_exp.cond.result_type.clone(),
					if_exp.cond.located.clone(),
				));
			}
			let then_ty = if_exp.then.result_type.clone();
			let other_ty = if_exp.otherwise.result_type.clone();
			(CompType::Union(vec![then_ty, other_ty]).flatten(), errs)
		}
		Value(data) => (data.get_type(), errs),
		Index(arr, i) => {
			let arr_ty = arr.result_type.clone();
			let i_ty = i.result_type.clone();
			// TODO: Move to use an is_indexable method
			let result_type = if !arr_ty.is_str() && !arr_ty.is_array() {
				errs.push(CompError::CannotIndexType(arr_ty, i.located.clone()));
				CompType::Unknown
			} else if !i_ty.is_int() {
				errs.push(CompError::InvalidIndexType(i_ty, i.located.clone()));
				CompType::Unknown
			} else if let CompType::Array(elements, _) = arr_ty {
				*elements
			} else if arr_ty.is_str() {
				CompType::Char
			} else {
				CompType::Unknown
			};
			(result_type, errs)
		}
		Assign(var, lhs) => {
			let exp_ty = lhs.result_type.clone();
			let mut var_ty = var.variable.get_type();
			let mut accesses = Vec::new();
			for access in &var.accessing {
				use IndexOption::*;
				match &access.0 {
					Dot(prop) => {
						if let CompType::Struct(fields) = &var_ty {
							accesses.push((Dot(prop.clone()), var_ty.clone()));
							var_ty = fields.get(prop).unwrap().clone();
						} else {
							errs.push(CompError::CannotIndexType(var_ty.clone(), 0..0));
							accesses.push(access.clone());
						}
					}
					Index(index) => {
						if !index.result_type.is_int() {
							var_ty = CompType::Unknown;
							errs.push(CompError::InvalidIndexType(
								index.result_type.clone(),
								index.located.clone(),
							));
							accesses.push((Index(index.clone()), CompType::Unknown))
						} else if let CompType::Array(el_ty, _len) = var_ty {
							var_ty = el_ty.as_ref().clone();
							accesses.push((Index(index.clone()), var_ty.clone()));
						} else {
							errs.push(CompError::CannotIndexType(
								var_ty.clone(),
								index.located.clone(),
							));
							var_ty = CompType::Unknown;
						};
					}
				}
			}
			let exp_ty = if let CompType::Touple(elements) = exp_ty {
				CompType::Array(
					get_top_type(&elements).boxed(),
					CompType::Constant(ConstantData::Int(elements.len() as i32)).boxed(),
				)
			} else {
				exp_ty
			};
			if !var_ty.super_type_of(&exp_ty) {
				errs.push(CompError::InvalidAssignment(
					var_ty.clone(),
					exp_ty.clone(),
					located,
				));
			}
			var.accessing = accesses;
			(exp_ty, errs)
		}
		BinOp(op, a, b) => {
			let result_type = match op.resulting_type(&a.result_type, &b.result_type) {
				Ok(ty) => ty,
				Err(err) => {
					errs.push(err);
					CompType::Unknown
				}
			};
			(result_type, errs)
		}
		OneOp(_, val) => (val.result_type.clone(), errs),
		Read(var) => (var.get_type(), errs),
		Call(var, generics, args) => {
			let var = var.clone();
			let result_type = if let CompType::Callible(arg_types, ret) = var.get_type() {
				if arg_types.len() != args.len() {
					errs.push(CompError::WrongArgumentsCount(
						var.get_name(),
						args.len(),
						arg_types.len(),
						located,
					))
				}
				for (x, y) in arg_types.iter().zip(args) {
					let (x, mut errors) = x.substitute_generics(generics);
					errs.append(&mut errors);
					if !x.super_type_of(&y.result_type) {
						errs.push(CompError::InvalidAssignment(
							y.result_type.clone(),
							x.clone(),
							y.located.clone(),
						))
					}
				}
				let (ret, mut errors) = ret.substitute_generics(generics);
				errs.append(&mut errors);
				ret
			} else {
				errs.push(CompError::NonfunctionCall(
					var.get_name(),
					var.get_type(),
					located,
				));
				CompType::Unknown
			};
			(result_type, errs)
		}
	}
}

fn calc_type_from_not(from: &CompType, not: &CompType) -> CompType {
	use CompType::*;
	if let Union(types) = from {
		if types.contains(not) {
			Union(types.iter().filter(|ty| ty != &not).cloned().collect()).flatten()
		} else {
			from.clone()
		}
	} else if not.is_primitive() && from.is_primitive() {
		if not == from {
			Union(Vec::new())
		} else {
			from.clone()
		}
	} else {
		// ToDO: Actually match all cases, this should not be reached if correct code is written but still technically reachable and so needs to be handled properly
		NEVER
	}
}

fn transform_function(
	func: &Function,
	scope: &mut Scope,
	file: &str,
) -> (FunctionAst, Vec<CompError>) {
	let mut errs = Vec::new();
	for (pos, (name, constraint)) in func.generics.iter().enumerate() {
		let constraint = match constraint {
			Some(ty) => {
				// TODO: Hacky error reporting until we clean this part up
				let (ty, mut errors) = transform_type(ty, scope);
				errs.append(&mut errors);
				ty
			}
			None => CompType::Unknown,
		};
		scope.add_type(name.clone(), CompType::Generic(pos, constraint.boxed()));
	}

	let mut arguments = Vec::new();
	for arg in &func.args {
		let arg_ty = if let Some(ty) = &arg.1 {
			let (ty, mut errors) = transform_type(ty, scope);
			errs.append(&mut errors);
			ty
		} else {
			CompType::Unknown
		};
		arguments.push(CompVariable {
			name: arg.0 .0.clone(),
			constant: true,
			typing: arg_ty,
			external: false,
			declared_at: Some((file.to_string(), arg.0 .1.clone())),
			initialised: true,
		});
	}
	let (return_type, mut errors) = transform_type(&func.return_type, scope);
	errs.append(&mut errors);
	let arguments = arguments;
	let func = match &func.body {
		Some(body) => {
			let mut local_scope = scope.create_child(arguments.clone());
			let local_scope = resolve_scope(body.as_ref(), &mut local_scope, file);
			let (body, mut errors) = transform_ast(body.as_ref(), local_scope, file);
			errs.append(&mut errors);
			FunctionAst {
				arguments,
				return_type,
				body: Some(Box::new(body)),
			}
		}
		None => FunctionAst {
			arguments,
			return_type,
			body: None,
		},
	};
	(func, errs)
}
