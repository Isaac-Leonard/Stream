use crate::ast1::*;
use crate::ast3::*;
use crate::errors::CompError;
use crate::map_vec;
use crate::settings::Settings;
use crate::utils::WithErrors;
use std::cell::Ref;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter};
use std::sync::Arc;
use std::{collections::HashMap, ops::Range};

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum CompType {
	Unknown,
	Not(Box<Self>),
	Callible(Vec<Self>, Box<Self>),
	Union(Vec<Self>),
	Array(Box<CompType>, Box<CompType>),
	Struct(BTreeMap<String, CompType>),
	Null,
	Bool,
	Int,
	Float,
	Str(Box<CompType>),
	Ptr,
	IntPtr,
	/// Stores the position of the generic and the type it extends
	Generic(usize, Box<CompType>),
	Type,
	Constant(ConstantData),
	Touple(Vec<Self>),
	Char,
}

impl CompType {
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

	pub fn boxed(self) -> Box<Self> {
		Box::new(self)
	}

	pub fn contains_generic(&self) -> bool {
		use CompType::*;
		match self {
			Generic(_, _) => true,
			IntPtr | Type | Char | Unknown | Bool | Ptr | Int | Float | Null | Constant(_)
			| Not(_) => false,
			Array(element_ty, _) => element_ty.contains_generic(),
			Touple(elements) => elements.iter().any(|x| x.contains_generic()),
			Struct(elements) => elements.iter().any(|x| x.1.contains_generic()),
			Union(tys) => tys.iter().any(|x| x.contains_generic()),
			Callible(args, ret) => {
				args.iter().any(|x| x.contains_generic()) || ret.contains_generic()
			}
			Str(len) => len.contains_generic(),
		}
	}

	pub fn is_primitive(&self) -> bool {
		use CompType::*;
		!matches!(self, Str(_) | Array(_, _) | Struct(_))
	}

	pub fn widen(&self) -> Self {
		match self {
			Self::Constant(data) => data.widen(),
			Self::Array(el_ty, len) => Self::Array(el_ty.widen().boxed(), len.clone()),
			x => x.clone(),
		}
	}

	pub fn get_str(&self) -> String {
		format!("{}", self)
	}

	pub fn get_variants(&self) -> Vec<CompType> {
		if let CompType::Union(types) = self {
			types.clone()
		} else {
			vec![self.clone()]
		}
	}

	pub fn super_type_of(&self, sub_type: &CompType) -> bool {
		if self == sub_type {
			true
		} else if let CompType::Generic(_, extends_type) = sub_type {
			self.super_type_of(extends_type)
		} else if let CompType::Generic(_, extends)=self&& let CompType::Generic(_, extends_type) = sub_type {
			extends.super_type_of(extends_type)
		} else if self.is_ptr() && sub_type.is_str() {
			true
		} else if let CompType::Union(types) = self {
			// TODO: Do deeper checking here
			types.contains(sub_type)
		} else if let CompType::Constant(data) = sub_type {
			match (self, data) {
				(CompType::Str(len), ConstantData::Str(str)) => match len.as_ref() {
					CompType::Constant(ConstantData::Int(len)) => *len == str.len() as i32,
					CompType::Int => true,
					_ => false,
				},
				(CompType::Int, ConstantData::Int(_)) => true,
				(CompType::Float, ConstantData::Float(_)) => true,
				(CompType::Bool, ConstantData::Bool(_)) => true,
				(CompType::Null, ConstantData::Null) => true,
				_ => false,
			}
		} else if let (Self::Struct(data), Self::Struct(sub)) = (self, sub_type) {
			for (a, b) in data.iter().zip(sub) {
				if !a.1.super_type_of(b.1) {
					return false;
				}
			}
			true
		} else if self == &CompType::Unknown || sub_type == &CompType::Unknown {
			// Return true if either is unknown
			// Fix to make work properly with generics
			true
		} else {
			false
		}
	}

	/// Recursively examines itself returning a new copy of itself with generic types substituted where they are found
	/// TODO: Work out a way to determine if too many generics were provided
	pub fn substitute_generics(
		&self,
		generics: &[CompType],
		location: Range<usize>,
	) -> WithErrors<CompType> {
		use CompType::*;
		let mut errors = Vec::new();

		let ty = match self {
			IntPtr | Ptr | Type | Not(_) | Unknown | Char | Bool | Constant(_) | Null | Int
			| Float => self.clone(),
			Str(len) => Str(len
				.substitute_generics(generics, location.clone())
				.collect_errors_into(&mut errors)
				.boxed()),
			Array(el_ty, len) => Array(
				el_ty
					.substitute_generics(generics, location.clone())
					.collect_errors_into(&mut errors)
					.boxed(),
				len.substitute_generics(generics, location.clone())
					.collect_errors_into(&mut errors)
					.boxed(),
			),
			Struct(data) => Struct(
				data.iter()
					.map(|(key, ty)| {
						(
							key.clone(),
							ty.substitute_generics(generics, location.clone())
								.collect_errors_into(&mut errors),
						)
					})
					.collect(),
			),
			Union(types) => Union(map_vec!(types, |ty| ty
				.substitute_generics(generics, location.clone())
				.collect_errors_into(&mut errors))),
			Touple(elements) => Touple(map_vec!(elements, |ty| ty
				.substitute_generics(generics, location.clone())
				.collect_errors_into(&mut errors))),
			Callible(args, ret) => Callible(
				map_vec!(args, |x| x
					.substitute_generics(generics, location.clone())
					.collect_errors_into(&mut errors)),
				ret.substitute_generics(generics, location.clone())
					.collect_errors_into(&mut errors)
					.boxed(),
			),
			Generic(position, extends_type) => {
				let substitute_type = generics.get(*position);
				if let Some(substitute) = substitute_type {
					if extends_type.super_type_of(substitute) {
						substitute.clone()
					} else {
						errors.push(CompError::MismatchedGenericConstraint(
							substitute.clone(),
							extends_type.as_ref().clone(),
							location,
						));
						CompType::Unknown
					}
				} else {
					// Pass it Straight through
					self.clone()
				}
			} // TODO: Hack till I can fill in the rest (I'm tired and its late)
		};
		WithErrors::new(ty.flatten(), errors)
	}

	pub fn match_generics<'a, 'b>(&'a self, ty: &'b Self) -> Vec<(&'a Self, &'b Self)> {
		use CompType::*;
		let mut matches = Vec::new();
		if matches!(self, Generic(_, _)) {
			matches.push((self, ty))
		};
		let mut sub_matches = match (self, ty) {
			(Generic(_, sub), ty) => sub.match_generics(ty),
			(Type, Type)
				| (Char, Char)
				| (Unknown, _)
				| (Bool, Bool)
				| (Ptr, Ptr)
				| (Int, Int)
				| (Float, Float)
				| (Null, Null)
			// TODO: Match on types here
				| (Constant(_), Constant(_))
				| (Not(_), Not(_)) => Vec::new(),
			(Array(a, a_len),Array(b, b_len)) => a.match_generics(b.as_ref()).into_iter().chain(a_len.match_generics(b_len.as_ref())).collect(),
			(Touple(a), Touple(b)) => a.iter().zip(b).flat_map(|x| x.0.match_generics(x.1)).collect(),
			(Struct(a), Struct(b)) => a.iter().zip(b).flat_map(|x| x.0.1.match_generics(x.1.1)).collect(),
			// TODO: Handling here is wrong and needs to be fixed
			(Union(a), Union(b)) => a.iter().zip(b).flat_map(|x| x.0.match_generics(x.1)).collect(),
			(Callible(a_args, a_ret), Callible(b_args, b_ret)) => {eprintln!("Matching function generics");
				let mut matches:Vec<_>= a_args.iter().zip(b_args).flat_map(|x| x.0.match_generics(x.1)).collect();
				matches.append(&mut  a_ret.match_generics(b_ret));
				matches
			}
			(Str(a),Str(b)) => a.match_generics(b),
			(_, _)=>panic!("attempted to find generics of mismatched types: {}, {}", self,ty)		};
		matches.append(&mut sub_matches);
		matches
	}

	pub fn is_bool(&self) -> bool {
		matches!(
			*self,
			CompType::Bool | CompType::Constant(ConstantData::Bool(_))
		)
	}

	fn is_null(&self) -> bool {
		self == &CompType::Null
	}

	pub fn is_int(&self) -> bool {
		matches!(
			*self,
			CompType::Int | CompType::Constant(ConstantData::Int(_))
		)
	}

	pub fn is_int_ptr(&self) -> bool {
		matches!(*self, CompType::IntPtr)
	}

	pub fn is_ptr(&self) -> bool {
		matches!(*self, CompType::Ptr)
	}

	pub fn is_str(&self) -> bool {
		matches!(
			self,
			CompType::Str(_) | CompType::Constant(ConstantData::Str(_))
		)
	}

	pub fn is_array(&self) -> bool {
		matches!(self, CompType::Array(_, _))
	}

	pub fn is_union(&self) -> bool {
		matches!(self, CompType::Union(_))
	}

	pub fn is_callable(&self) -> bool {
		matches!(self, CompType::Callible(_, _))
	}

	pub fn flatten(&self) -> CompType {
		use CompType::*;
		match self {
			Char => Char,
			IntPtr => IntPtr,
			Touple(elements) => Touple(map_vec!(elements, |el| el.flatten())),
			Constant(data) => Constant(data.clone()),
			Struct(fields) => {
				let keys = fields
					.iter()
					.map(|(k, v)| (k.clone(), v.flatten()))
					.collect();
				Struct(keys)
			}
			Not(ty) => {
				if let Not(ty) = &**ty {
					ty.flatten()
				} else {
					Not(Box::new(ty.flatten()))
				}
			}
			Type => Type,
			Unknown => Unknown,
			Ptr => Ptr,
			Array(ty, len) => Array(ty.clone(), len.clone()),
			Int => Int,
			Float => Float,
			Str(len) => Str(len.clone()),
			Bool => Bool,
			Null => Null,
			Callible(args, ret) => Callible(
				args.iter().map(|x| x.flatten()).collect(),
				Box::new(ret.flatten()),
			),
			Union(types) => {
				let types = types.iter().map(|x| x.flatten()).collect::<Vec<_>>();
				match types.len() {
					0 => Null,
					1 => types[0].clone(),
					_ => {
						let mut uniques = Vec::new();
						for ty in types {
							if !uniques.contains(&ty) {
								uniques.push(ty)
							}
						}
						if uniques.len() == 1 {
							uniques[0].clone()
						} else {
							Union(uniques)
						}
					}
				}
			}
			Generic(pos, name) => Generic(*pos, name.clone()),
		}
	}
}

impl Display for CompType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use CompType::*;
		match self {
			Char => write!(f, "Char"),
			IntPtr => write!(f, "IntPtr"),
			Touple(elements) => write!(
				f,
				"[{}]",
				elements
					.iter()
					.map(CompType::get_str)
					.reduce(|a, b| format!("{}, {}", a, b))
					.unwrap()
			),
			Constant(data) => write!(f, "{:?}", data),
			Unknown => write!(f, "unknown"),
			Struct(keys) => write!(
				f,
				"{{{}}}",
				keys.iter()
					.map(|(k, v)| format!("{}: {};", k, v))
					.fold(String::from(""), |a, b| format!("{};\n{}", a, b))
			),
			Not(ty) => write!(f, "Not<{}>", ty),
			Type => write!(f, "Type"),
			Array(ty, len) => write!(f, "[{}, {}]", ty, len),
			Generic(_pos, name) => write!(f, "{}", name),
			Ptr => write!(f, "Ptr"),
			Int => write!(f, "Int"),
			Null => write!(f, "Null"),
			Str(len) => write!(f, "Str<{}>", len),
			Bool => write!(f, "Bool"),
			Float => write!(f, "Float"),
			Union(types) => write!(
				f,
				"{}",
				types
					.iter()
					.map(CompType::get_str)
					.reduce(|a, b| format!("{} | {}", a, b))
					.unwrap()
			),
			Callible(args, ret) => write!(
				f,
				"({}): {}",
				args.iter()
					.map(CompType::get_str)
					.reduce(|a, b| format!("{}, {}", a, b))
					.unwrap_or_else(String::new),
				ret.get_str()
			),
		}
	}
}

pub const NEVER: CompType = CompType::Union(Vec::new());

#[derive(Debug, PartialEq, Clone)]
pub enum CompExpression {
	/// Accessing an objects properties via dot notation
	DotAccess(ExpEnvironment, (String, Range<usize>)),
	/// A literal value used in code
	Value(CompData),
	/// The list of expressions that make up an literal array
	Array(Vec<ExpEnvironment>),
	/// Getting the type of an expression
	Typeof(ExpEnvironment),
	/// Declaring a struct with its properties and expressions assigned to them
	/// We must attach the key location to the value to allow for accessing without knowing the location it was declared at while still keeping track of the location
	Struct(BTreeMap<String, (Range<usize>, ExpEnvironment)>),
	/// A binary operation
	BinOp(Op, ExpEnvironment, ExpEnvironment),
	/// Reading the value of a variable
	Read(CompVariable),
	/// An unary operation
	OneOp(Prefix, ExpEnvironment),
	/// Calling a function
	Call(CompVariable, Vec<CompType>, Vec<ExpEnvironment>),
	/// Assigning to something
	Assign(MemoryLocation, ExpEnvironment),
	/// Branching code based on some condition
	IfElse(IfElse),
	/// repeat some code while some condition is true
	WhileLoop {
		cond: ExpEnvironment,
		body: ExpEnvironment,
	},
	/// Read a value at an array index
	Index(ExpEnvironment, ExpEnvironment),
	/// A list of expressions
	List(Vec<ExpEnvironment>),
	/// Converting the return type of an expression to a specified type
	Conversion(ExpEnvironment, CompType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpEnvironment {
	/// The code that makes up this environment
	// Probably should be boxing where recursion actually occurs but this reduces the amount of code
	pub expression: Box<CompExpression>,
	/// The type this environment returns
	pub result_type: CompType,
	/// The span in the file of the section of code that makes up this environment
	pub located: Range<usize>,
}

impl ExpEnvironment {
	pub fn is_if_else(&self) -> bool {
		matches!(self.expression.as_ref(), CompExpression::IfElse(_))
	}

	pub fn is_while_loop(&self) -> bool {
		matches!(
			self.expression.as_ref(),
			CompExpression::WhileLoop { cond: _, body: _ }
		)
	}

	pub fn is_read(&self) -> bool {
		matches!(self.expression.as_ref(), CompExpression::Read(_))
	}

	pub fn is_call(&self) -> bool {
		matches!(self.expression.as_ref(), CompExpression::Call(_, _, _))
	}

	pub fn find_map<'a, T: 'a, F>(&'a self, matcher: &mut F) -> Option<T>
	where
		F: FnMut(&'a Self) -> Option<T>,
	{
		let current = matcher(self);
		if current.is_some() {
			return current;
		};
		match self.expression.as_ref() {
			CompExpression::List(exps)
			| CompExpression::Array(exps)
			| CompExpression::Call(_, _, exps) => exps.iter().find_map(|x| x.find_map(matcher)),
			CompExpression::Struct(fields) => fields.iter().find_map(|x| x.1 .1.find_map(matcher)),
			CompExpression::IfElse(ifelse) => {
				let cond = ifelse.cond.find_map(matcher);
				if cond.is_some() {
					return cond;
				}
				let then = ifelse.then.find_map(matcher);
				if then.is_some() {
					return then;
				}
				let otherwise = ifelse.otherwise.find_map(matcher);
				if otherwise.is_some() {
					return otherwise;
				}
				None
			}
			CompExpression::WhileLoop { cond: a, body: b }
			| CompExpression::BinOp(_, a, b)
			| CompExpression::Index(a, b) => {
				let a = a.find_map(matcher);
				if a.is_some() {
					return a;
				}
				let b = b.find_map(matcher);
				if b.is_some() {
					return b;
				}
				None
			}
			CompExpression::OneOp(_, exp)
			| CompExpression::Typeof(exp)
			| CompExpression::Conversion(exp, _)
			| CompExpression::DotAccess(exp, _) => exp.find_map(matcher),
			CompExpression::Assign(lvalue, rhs) => {
				let rhs = rhs.find_map(matcher);
				if rhs.is_some() {
					return rhs;
				}
				for access in &lvalue.accessing {
					if let IndexOption::Index(index) = &access.0 {
						let access = index.find_map(matcher);
						if access.is_some() {
							return access;
						}
					}
				}
				None
			}
			CompExpression::Read(_) | CompExpression::Value(_) => None,
		}
	}

	pub fn find(&self, matcher: fn(&Self) -> bool) -> Option<&Self> {
		self.find_map(&mut |x| if matcher(x) { Some(x) } else { None })
	}

	pub fn has(&self, matcher: fn(&Self) -> bool) -> bool {
		self.find(matcher).is_some()
	}

	/// Branches directly in the expression, not including function calls
	pub fn contains_direct_branches(&self) -> bool {
		self.has(|x| x.is_if_else() || x.is_while_loop())
	}

	pub fn contains_branches(&self) -> bool {
		self.has(|x| x.is_if_else() || x.is_while_loop() || x.is_call())
	}

	pub fn map_each<'a, T: 'a, F>(&'a self, mapper: &mut F) -> Vec<T>
	where
		F: FnMut(&'a Self) -> Vec<T>,
	{
		let mut current = mapper(self);
		match self.expression.as_ref() {
			CompExpression::List(exps)
			| CompExpression::Array(exps)
			| CompExpression::Call(_, _, exps) => {
				current.append(&mut exps.iter().flat_map(|x| x.map_each(mapper)).collect());
			}
			CompExpression::Struct(fields) => {
				current.append(
					&mut fields
						.iter()
						.flat_map(|x| x.1 .1.map_each(mapper))
						.collect(),
				);
			}
			CompExpression::IfElse(ifelse) => {
				current.append(&mut ifelse.cond.map_each(mapper));
				current.append(&mut ifelse.then.map_each(mapper));
				current.append(&mut ifelse.otherwise.map_each(mapper));
			}
			CompExpression::WhileLoop { cond: a, body: b }
			| CompExpression::BinOp(_, a, b)
			| CompExpression::Index(a, b) => {
				current.append(&mut a.map_each(mapper));
				current.append(&mut b.map_each(mapper));
			}
			CompExpression::OneOp(_, exp)
			| CompExpression::Typeof(exp)
			| CompExpression::Conversion(exp, _)
			| CompExpression::DotAccess(exp, _) => {
				current.append(&mut exp.map_each(mapper));
			}
			CompExpression::Assign(lvalue, rhs) => {
				current.append(&mut rhs.map_each(mapper));
				for access in &lvalue.accessing {
					if let IndexOption::Index(index) = &access.0 {
						current.append(&mut index.map_each(mapper));
					}
				}
			}
			CompExpression::Read(_) | CompExpression::Value(_) => {}
		}
		current
	}

	/// Written as in modified during execution
	pub fn get_all_written_variables(&self) -> Vec<CompVariable> {
		self.map_each(&mut |x| match x.expression.as_ref() {
			CompExpression::Assign(lvalue, _) => vec![lvalue.variable.clone()],
			_ => Vec::new(),
		})
	}

	pub fn get_all_mentioned_variables(&self) -> Vec<&CompVariable> {
		self.map_each(&mut |x| match x.expression.as_ref() {
			CompExpression::Assign(lvalue, _) => vec![&lvalue.variable],
			CompExpression::Read(var) => vec![var],
			_ => Vec::new(),
		})
	}

	pub fn map_inplace(&mut self, mapper: &mut dyn FnMut(&Self) -> Option<Self>) {
		if let Some(current) = mapper(self) {
			*self = current;
		};
		match self.expression.as_mut() {
			CompExpression::List(exps)
			| CompExpression::Array(exps)
			| CompExpression::Call(_, _, exps) => exps.iter_mut().for_each(|x| x.map_inplace(mapper)),
			CompExpression::Struct(fields) => {
				fields.iter_mut().for_each(|x| x.1 .1.map_inplace(mapper))
			}
			CompExpression::IfElse(ifelse) => {
				ifelse.cond.map_inplace(mapper);
				ifelse.then.map_inplace(mapper);
				ifelse.otherwise.map_inplace(mapper);
			}
			CompExpression::WhileLoop { cond: a, body: b }
			| CompExpression::BinOp(_, a, b)
			| CompExpression::Index(a, b) => {
				a.map_inplace(mapper);
				b.map_inplace(mapper);
			}
			CompExpression::OneOp(_, exp)
			| CompExpression::Typeof(exp)
			| CompExpression::Conversion(exp, _)
			| CompExpression::DotAccess(exp, _) => {
				exp.map_inplace(mapper);
			}
			CompExpression::Assign(lvalue, rhs) => {
				rhs.map_inplace(mapper);
				for access in &mut lvalue.accessing {
					if let IndexOption::Index(index) = &mut access.0 {
						index.map_inplace(mapper)
					}
				}
			}
			CompExpression::Read(_) | CompExpression::Value(_) => {}
		}
	}

	pub fn replace_arrays(&mut self) {
		let mut count = 0;
		self.map_inplace(&mut |x| match x.expression.as_ref() {
			CompExpression::Array(elements) => {
				let var = CompVariable {
					name: ".array".to_string() + &count.to_string(),
					initialised: false,
					declared_at: None,
					typing: x.result_type.clone(),
					constant: false,
					external: false,
				};
				count += 1;
				let mut list: Vec<ExpEnvironment> = elements
					.iter()
					.enumerate()
					.map(|(i, element)| ExpEnvironment {
						expression: Box::new(CompExpression::Assign(
							MemoryLocation {
								variable: var.clone(),
								accessing: vec![(
									IndexOption::Index(ExpEnvironment {
										expression: Box::new(CompExpression::Value(CompData::Int(
											i as i32,
										))),
										result_type: CompType::Int,
										located: 0..0,
									}),
									element.result_type.clone(),
								)],
							},
							element.clone(),
						)),
						result_type: element.result_type.clone(),
						located: 0..0,
					})
					.collect();

				list.push(ExpEnvironment {
					expression: Box::new(CompExpression::Read(var)),
					result_type: x.result_type.clone(),
					located: x.located.clone(),
				});
				Some(ExpEnvironment {
					expression: Box::new(CompExpression::List(list)),
					result_type: x.result_type.clone(),
					located: x.located.clone(),
				})
			}
			_ => None,
		})
	}

	pub fn get_final_expressions(&self) -> Vec<&ExpEnvironment> {
		match self.expression.as_ref() {
			CompExpression::List(exps) => exps.last().unwrap().get_final_expressions(),
			CompExpression::WhileLoop { cond: _, body: _ } => Vec::new(),
			CompExpression::IfElse(ifelse) => ifelse
				.then
				.get_final_expressions()
				.into_iter()
				.chain(ifelse.otherwise.get_final_expressions())
				.collect(),
			CompExpression::Array(_)
			| CompExpression::Struct(_)
			| CompExpression::Read(_)
			| CompExpression::Typeof(_)
			| CompExpression::Value(_)
			| CompExpression::Index(_, _)
			| CompExpression::OneOp(_, _)
			| CompExpression::BinOp(_, _, _)
			| CompExpression::Assign(_, _)
			| CompExpression::Call(_, _, _)
			| CompExpression::Conversion(_, _)
			| CompExpression::DotAccess(_, _) => {
				vec![self]
			}
		}
	}

	pub fn get_references_of(&self, var: &CompVariable) -> Vec<&ExpEnvironment> {
		self.map_each(&mut |x| match x.expression.as_ref() {
			CompExpression::Read(variable) => {
				if var == variable {
					vec![x]
				} else {
					Vec::new()
				}
			}
			CompExpression::Assign(lvalue, _) => {
				if var == &lvalue.variable {
					vec![x]
				} else {
					Vec::new()
				}
			}
			_ => Vec::new(),
		})
	}

	pub fn get_functions(&self) -> Vec<&FunctionAst> {
		self.map_each(&mut |x| match x.expression.as_ref() {
			CompExpression::Value(CompData::Func(func)) => vec![func],
			_ => Vec::new(),
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
	pub scope: Scope,
	pub body: ExpEnvironment,
}

impl Program {
	pub fn get_exported(&self) -> Vec<CompVariable> {
		self.scope.get_exported()
	}
}

#[derive(Debug, PartialEq, Default)]
struct InnerScope {
	parent: Option<Scope>,
	types: HashMap<String, CompType>,
	variables: HashMap<String, CompVariable>,
	preset_variables: HashMap<String, CompVariable>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope(Arc<RefCell<InnerScope>>);

impl PartialEq for Scope {
	fn eq(&self, other: &Self) -> bool {
		*self.0.borrow() == *other.0.borrow()
	}
}

impl Scope {
	fn resolve_scope<'a>(
		&'a mut self,
		(loc, ast): &SpannedExpression,
		file: &str,
	) -> &'a mut Scope {
		// TODO: Sort out errors and stuff here
		match ast {
			Expression::TypeDeclaration(name, generics, declared_type) => {
				if self.get_type(name).is_err() {
					let mut subscope = self.create_child(Vec::new());
					for (pos, (name, constraint)) in generics.iter().enumerate() {
						let constraint = match constraint {
							Some(ty) => {
								// TODO: Hacky error reporting until we clean this part up
								let WithErrors { data: ty, errors } =
									transform_type(ty, &subscope, loc.clone());
								if !errors.is_empty() {
									eprintln!("{:?}", errors);
								}
								ty
							}
							None => CompType::Unknown,
						};
						subscope.add_type(name.clone(), CompType::Generic(pos, constraint.boxed()));
					}
					let WithErrors { data: ty, errors } =
						transform_type(declared_type, &subscope, loc.clone());
					if !errors.is_empty() {
						eprintln!("{:?}", errors);
					}
					self.add_type(name.clone(), ty);
				}
				self
			}
			Expression::InitAssign(external, constant, name, declared_type, _exp) => {
				if self.variable_exists(&name.0) {
					self
				} else {
					let typing = match declared_type {
						None => CompType::Unknown,
						Some(x) => {
							let WithErrors { data: ty, errors } =
								transform_type(x, self, loc.clone());
							for err in errors {
								eprintln!("{}", err.get_msg_without_lines());
							}
							ty
						}
					};
					self.add_variable(CompVariable {
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
					self.resolve_scope(exp, file);
				}
				self
			}
			_x => self,
		}
	}

	fn get_inner(&self) -> Ref<'_, InnerScope> {
		self.0.borrow()
	}

	/// Returns an option referencing the parent scope if it exists
	pub fn get_parent(&self) -> Option<Self> {
		self.0.borrow().parent.clone()
	}

	pub fn get_variables(&self) -> HashMap<String, CompVariable> {
		self.get_inner().variables.clone()
	}

	pub fn get_types(&self) -> HashMap<String, CompType> {
		self.get_inner().types.clone()
	}

	/// Takes in an active scope and a list of varables this scope will have that the parent doesn't
	pub fn create_child(&self, preset_variables: Vec<CompVariable>) -> Self {
		Self(Arc::new(RefCell::new(InnerScope {
			parent: Some(self.clone()),
			types: HashMap::new(),
			preset_variables: preset_variables
				.into_iter()
				.map(|var| (var.get_name(), var))
				.collect(),
			variables: HashMap::new(),
		})))
	}

	pub fn get_exported(&self) -> Vec<CompVariable> {
		self.0
			.borrow()
			.variables
			.iter()
			.filter(|x| x.1.is_extern())
			.map(|x| x.1.clone())
			.collect()
	}

	pub fn variable_exists(&self, name: &String) -> bool {
		if self.0.borrow().variables.contains_key(name)
			|| self.0.borrow().preset_variables.contains_key(name)
		{
			true
		} else {
			match self.get_parent() {
				Some(parent) => parent.variable_exists(name),
				None => false,
			}
		}
	}

	pub fn add_variable(&mut self, var: CompVariable) -> &mut Scope {
		self.0.borrow_mut().variables.insert(var.get_name(), var);
		self
	}

	pub fn add_type(&mut self, name: String, ty: CompType) -> &mut Scope {
		self.0.borrow_mut().types.insert(name, ty);
		self
	}

	pub fn set_variable_initialised(&mut self, name: &String) {
		if let Some(var) = self.0.borrow_mut().variables.get_mut(name) {
			var.set_initialised();
		}
	}

	pub fn set_variable_type<'a>(&'a mut self, name: &String, ty: &CompType) -> &'a mut Scope {
		if let Some(v) = self.0.borrow_mut().variables.get_mut(name) {
			v.set_type(ty.clone());
		}
		self
	}

	pub fn get_variable(&self, name: &str) -> Result<CompVariable, String> {
		if let Some(var) = self.0.borrow().variables.get(name) {
			if name == "mapIntArr" {
				eprintln!("NewArray: {:?}", var);
			}
			Ok(var.clone())
		} else if let Some(var) = self.0.borrow().preset_variables.get(name) {
			Ok(var.clone())
		} else {
			match self.get_parent() {
				Some(parent) => parent.get_variable(name),
				None => Err(format!("Cannot find variable '{}'", name)),
			}
		}
	}

	pub fn get_type(&self, name: &String) -> Result<CompType, CompError> {
		if let Some(ty) = self.0.borrow().types.get(name) {
			Ok(ty.clone())
		} else {
			match self.get_parent() {
				Some(parent) => parent.get_type(name),
				None => Err(CompError::CannotFindType(name.clone(), 0..0)),
			}
		}
	}

	pub fn constant_exists(&self, name: &String) -> bool {
		if let Some(var) = self.0.borrow().variables.get(name) {
			var.is_const()
		} else {
			match self.get_parent() {
				Some(parent) => parent.constant_exists(name),
				None => false,
			}
		}
	}

	pub fn variable_initialised(&self, name: &String) -> bool {
		if let Some(var) = self.0.borrow().variables.get(name) {
			var.is_initialised()
		} else {
			match self.get_parent() {
				Some(parent) => parent.constant_exists(name),
				None => false,
			}
		}
	}

	pub fn variable_has_type(&self, name: &String) -> bool {
		if let Some(var) = self.0.borrow().variables.get(name) {
			var.get_type() != CompType::Unknown
		} else {
			match self.get_parent() {
				Some(parent) => parent.constant_exists(name),
				None => false,
			}
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompData {
	Null,
	Bool(bool),
	Int(i32),
	Float(f32),
	Str(String),
	Func(FunctionAst),
}

impl CompData {
	pub fn get_type(&self) -> CompType {
		use CompData::*;
		match self {
			Null => CompType::Null,
			Bool(b) => CompType::Constant(ConstantData::Bool(*b)),
			Int(i) => CompType::Constant(ConstantData::Int(*i)),
			Float(f) => CompType::Constant(ConstantData::Float(*f)),
			Str(content) => CompType::Str(Box::new(CompType::Constant(ConstantData::Int(
				content.len() as i32,
			)))),
			Func(ast) => ast.as_generic_type(),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfElse {
	pub cond: ExpEnvironment,
	pub then: ExpEnvironment,
	pub otherwise: ExpEnvironment,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompVariable {
	pub name: String,
	pub typing: CompType,
	pub initialised: bool,
	pub constant: bool,
	pub external: bool,
	pub declared_at: Option<(String, Range<usize>)>,
}

impl CompVariable {
	pub fn get_name(&self) -> String {
		self.name.clone()
	}

	pub fn get_type(&self) -> CompType {
		// TODO: implement a version that returns a reference if possible
		self.typing.clone()
	}

	pub fn set_type(&mut self, ty: CompType) {
		self.typing = ty;
	}

	pub fn is_const(&self) -> bool {
		self.constant
	}

	pub fn is_extern(&self) -> bool {
		self.external
	}

	pub fn get_declaration_location(&self) -> Option<(String, Range<usize>)> {
		self.declared_at.clone()
	}

	pub fn is_initialised(&self) -> bool {
		self.initialised
	}

	pub fn set_initialised(&mut self) {
		self.initialised = true;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionAst {
	pub arguments: Vec<CompVariable>,
	pub generic_arguments: Vec<CompVariable>,
	pub return_type: CompType,
	pub generic_return_type: CompType,
	pub body: Option<Box<Program>>,
}

impl FunctionAst {
	pub fn as_type(&self) -> CompType {
		CompType::Callible(
			self.arguments.iter().map(|x| x.get_type()).collect(),
			Box::new(self.return_type.clone()),
		)
	}

	/// We need this so the compiler can work with fully defined types but this can be called generically
	/// TODO: Rewrite to make this unneeded
	pub fn as_generic_type(&self) -> CompType {
		CompType::Callible(
			self.generic_arguments
				.iter()
				.map(|x| x.get_type())
				.collect(),
			Box::new(self.generic_return_type.clone()),
		)
	}

	pub fn with_generics(&self, generics: &[CompType]) -> Self {
		let body = if let Some(ref body) = self.body {
			body
		} else {
			return self.clone();
		};
		let prog = Program {
			scope: body.scope.create_child(Vec::new()),
			body: body.body.replace_generics(generics),
		};
		FunctionAst {
			body: Some(Box::new(prog)),
			..self.clone()
		}
	}
	pub fn get_all_written_variables(&self) -> Vec<CompVariable> {
		let mut vars = self.arguments.clone();
		if let Some(body) = &self.body {
			vars.append(&mut body.body.get_all_written_variables())
		}
		vars
	}

	pub fn returns_allocated(&self) -> bool {
		!self.return_type.is_primitive()
	}

	pub fn allocates(&self) -> bool {
		use CompExpression::*;
		if let Some(body) = &self.body {
			body.body
				.has(|x| matches!(x.expression.as_ref(), Struct(_) | Array(_)))
		} else {
			false
		}
	}

	pub fn find_allocates_targets(&self) -> Vec<MemoryLocation> {
		use CompExpression::*;
		if let Some(body) = &self.body {
			body.body.map_each(&mut |x| match x.expression.as_ref() {
				Assign(lvalue, rhs) => {
					if matches!(rhs.expression.as_ref(), Struct(_) | Array(_)) {
						vec![lvalue.clone()]
					} else {
						Vec::new()
					}
				}
				_ => Vec::new(),
			})
		} else {
			Vec::new()
		}
	}

	pub fn get_returns(&self) -> Vec<&ExpEnvironment> {
		if self.return_type.is_null() {
			Vec::new()
		} else if let Some(prog) = &self.body {
			prog.body.get_final_expressions()
		} else {
			Vec::new()
		}
	}
}

impl Op {
	pub fn resulting_type(&self, a: &CompType, b: &CompType) -> Result<CompType, CompError> {
		use CompType::*;
		use Op::*;
		match self {
			Eq | Neq => Ok(Bool),
			Le | Ge => match (a, b) {
				(Float, Float) | (Int, Int) => Ok(Bool),
				(Float, Constant(ConstantData::Float(_)))
				| (Constant(ConstantData::Float(_)), Float) => Ok(Bool),
				(Int, Constant(ConstantData::Int(_))) | (Constant(ConstantData::Int(_)), Int) => {
					Ok(Bool)
				}
				(Constant(ConstantData::Float(a)), Constant(ConstantData::Float(b))) => {
					Ok(Constant(ConstantData::Bool(if self == &Le {
						a < b
					} else {
						a > b
					})))
				}
				(Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
					Ok(Constant(ConstantData::Bool(if self == &Le {
						a < b
					} else {
						a > b
					})))
				}
				(Int, Generic(_, ty)) | (Generic(_, ty), Int) => {
					if ty.is_int() {
						Ok(Bool)
					} else {
						Err(self.invalid_comparison_msg(a, b))
					}
				}
				_ => Err(self.invalid_comparison_msg(a, b)),
			},
			Add => Ok(match (a, b) {
				// TODO: Make this recursive and allow for unknown or generic length strings
				(Str(a), Str(b)) => match (a.as_ref(), b.as_ref()) {
					(Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
						Str(ConstantData::Int(*a + *b).to_type().boxed())
					}
					_ => Unknown,
				},
				(Str(a), Constant(ConstantData::Str(b))) => match a.as_ref() {
					Constant(ConstantData::Int(a)) => {
						Str(ConstantData::Int(*a + b.len() as i32).to_type().boxed())
					}
					_ => Unknown,
				},
				(Constant(ConstantData::Str(a)), Str(b)) => match b.as_ref() {
					Constant(ConstantData::Int(b)) => Str(Box::new(CompType::Constant(
						ConstantData::Int(a.len() as i32 + *b),
					))),
					_ => CompType::Str(Box::new(CompType::Int)),
				},
				(Constant(ConstantData::Str(a)), Constant(ConstantData::Str(b))) => {
					Constant(ConstantData::Str(a.clone() + b))
				}
				(Bool, Bool) | (Null, Null) | (Bool, Null) | (Null, Bool) => {
					return Err(self.invalid_comparison_msg(a, b))
				}
				(Int, Int) => Int,
				(Int, Constant(ConstantData::Int(_))) => Int,
				(Constant(ConstantData::Int(_)), Int) => Int,
				(Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
					Constant(ConstantData::Int(a + b))
				}
				(Float, Float) => Float,
				(Float, Constant(ConstantData::Float(_))) => Float,
				(Constant(ConstantData::Float(_)), Float) => Float,
				(Constant(ConstantData::Float(a)), Constant(ConstantData::Float(b))) => {
					Constant(ConstantData::Float(a + b))
				}
				_ => return Err(self.invalid_comparison_msg(a, b)),
			}),
			Mult => match (a, b) {
				(Int, Int) => Ok(Int),
				(Int, Constant(ConstantData::Int(_)))
				| (Constant(ConstantData::Int(_)), Int)
				| (Generic(_, _), Constant(ConstantData::Int(_))) => Ok(Int),
				(Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
					Ok(Constant(ConstantData::Int(a * b)))
				}
				(Float, Float) => Ok(Float),
				(Float, Constant(ConstantData::Float(_)))
				| (Constant(ConstantData::Float(_)), Float) => Ok(Float),
				(Constant(ConstantData::Float(a)), Constant(ConstantData::Float(b))) => {
					Ok(Constant(ConstantData::Float(a * b)))
				}
				_ => Err(self.invalid_comparison_msg(a, b)),
			},
			Sub | Div => match (a, b) {
				(Int, Int) => Ok(Int),
				(Float, Float) => Ok(Float),
				(Float, Constant(ConstantData::Float(_)))
				| (Constant(ConstantData::Float(_)), Float) => Ok(Float),
				(Int, Constant(ConstantData::Int(_))) | (Constant(ConstantData::Int(_)), Int) => {
					Ok(Int)
				}
				(Constant(ConstantData::Float(a)), Constant(ConstantData::Float(b))) => {
					Ok(Constant(ConstantData::Float(if self == &Sub {
						a - b
					} else if self == &Mult {
						a * b
					} else {
						a / b
					})))
				}
				(Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
					Ok(Constant(ConstantData::Int(if self == &Sub {
						a - b
					} else if self == &Mult {
						a * b
					} else {
						a / b
					})))
				}
				_ => Err(self.invalid_comparison_msg(a, b)),
			},
		}
	}
	fn invalid_comparison_msg(&self, a: &CompType, b: &CompType) -> CompError {
		CompError::InvalidComparison(self.clone(), a.clone(), b.clone(), 0..0)
	}
}

impl Display for Op {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		use Op::*;
		write!(
			f,
			"{}",
			match self {
				Add => "+",
				Sub => "-",
				Mult => "*",
				Div => "/",
				Eq => "==",
				Neq => "!=",
				Le => "<",
				Ge => ">",
			}
		)
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
	Neg,
}

impl Prefix {
	pub fn get_str(&self) -> &str {
		match self {
			Self::Neg => "-",
		}
	}
}

impl ConstantData {
	pub fn to_comp_data(self) -> CompData {
		match self {
			Self::Int(val) => CompData::Int(val),
			Self::Float(val) => CompData::Float(val),
			Self::Bool(val) => CompData::Bool(val),
			Self::Str(val) => CompData::Str(val),
			Self::Null => CompData::Null,
		}
	}
	pub fn to_type(self) -> CompType {
		CompType::Constant(self)
	}

	pub fn widen(&self) -> CompType {
		use ConstantData::*;
		match self {
			Str(data) => CompType::Str(Box::new(CompType::Constant(ConstantData::Int(
				data.len() as i32
			)))),
			Int(_) => CompType::Int,
			Float(_) => CompType::Float,
			Bool(_) => CompType::Bool,
			Null => CompType::Null,
		}
	}
}

pub fn transform_exp(
	(loc, exp): &SpannedExpression,
	env: &ExpEnvironment,
	scope: &mut Scope,
	file: &str,
) -> WithErrors<ExpEnvironment> {
	use Expression::*;
	let mut errs = Vec::new();

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
				.map(|(key, value)| {
					(
						key.0.clone(),
						(
							key.1.clone(),
							transform_exp(value, env, scope, file).collect_errors_into(&mut errs),
						),
					)
				})
				.collect(),
		),
		Array(elements) => {
			CompExpression::Array(map_vec!(elements, |x| transform_exp(x, env, scope, file)
				.collect_errors_into(&mut errs)))
		}
		DotAccess(val, key) => CompExpression::DotAccess(
			transform_exp(val, env, scope, file).collect_errors_into(&mut errs),
			key.clone(),
		),
		Conversion(exp, ty) => CompExpression::Conversion(
			transform_exp(exp, env, scope, file).collect_errors_into(&mut errs),
			transform_type(ty, scope, loc.clone()).collect_errors_into(&mut errs),
		),
		Typeof(exp) => CompExpression::Typeof(
			transform_exp(exp, env, scope, file).collect_errors_into(&mut errs),
		),
		Index(arr, index) => CompExpression::Index(
			transform_exp(arr, env, scope, file).collect_errors_into(&mut errs),
			transform_exp(index, env, scope, file).collect_errors_into(&mut errs),
		),
		TypeDeclaration(_, _, _) => CompExpression::List(Vec::new()),
		InitAssign(_, _, name, _, exp) => {
			if scope.variable_initialised(&name.0) {
				errs.push(CompError::RedeclareInSameScope(name.0.clone(), loc.clone()));
			}

			let exp = transform_exp(exp, env, scope, file).collect_errors_into(&mut errs);
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
			let exp = transform_exp(exp, env, scope, file).collect_errors_into(&mut errs);
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
			let cond = transform_exp(cond, env, scope, file).collect_errors_into(&mut errs);
			let then = transform_exp(left, env, scope, file).collect_errors_into(&mut errs);
			let otherwise = transform_exp(right, env, scope, file).collect_errors_into(&mut errs);
			CompExpression::IfElse(crate::ast2::IfElse {
				cond,
				then,
				otherwise,
			})
		}
		Expression::Loop(exp, body) => {
			let cond = transform_exp(exp, env, scope, file).collect_errors_into(&mut errs);
			let body = transform_exp(body, env, scope, file).collect_errors_into(&mut errs);
			CompExpression::WhileLoop { cond, body }
		}
		Expression::Block(expressions) => {
			let mut env = env.clone();
			let mut oks = Vec::new();
			for exp in expressions {
				env = transform_exp(exp, &env, scope, file).collect_errors_into(&mut errs);
				oks.push(env.clone());
			}
			CompExpression::List(oks)
		}
		Expression::BinOp(op, left, right) => {
			let left = transform_exp(left, env, scope, file).collect_errors_into(&mut errs);
			let right = transform_exp(right, env, scope, file).collect_errors_into(&mut errs);
			CompExpression::BinOp(op.clone(), left, right)
		}
		Expression::FuncCall(name, generics, arguments) => {
			let mut args = Vec::new();
			let mut env = env.clone();
			for exp in arguments {
				env = transform_exp(exp, &env, scope, file).collect_errors_into(&mut errs);
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
				transform_type(x, scope, loc.clone()).collect_errors_into(&mut errs)
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
			Symbol::Data(data) => {
				CompExpression::Value(data.transform(scope, file).collect_errors_into(&mut errs))
			}
		},
		Expression::Invalid => panic!("invalid {:?}", loc),
	};
	get_env(expression, env, loc.clone(), errs)
}

fn transform_ast(ast: &SpannedExpression, scope: &mut Scope, file: &str) -> WithErrors<Program> {
	let mut errors = Vec::new();
	let env = get_env_from_scope(scope);
	let expression = transform_exp(ast, &env, scope, file).collect_errors_into(&mut errors);
	let mut prog = Program {
		scope: scope.clone(),
		body: expression,
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
						errors.push(CompError::NotImplemented(
							"Closures that capture variables are not currently implemented"
								.to_string(),
							0..0,
						))
					}
				}
			}
		}
	}
	WithErrors::new(prog, errors)
}

pub fn create_program(
	ast: &SpannedExpression,
	scope: &mut Scope,
	settings: &Settings,
) -> WithErrors<Program> {
	scope.resolve_scope(ast, &settings.input_name);
	transform_ast(ast, scope, &settings.input_name)
}

pub fn transform_type(
	ty: &CustomType,
	scope: &Scope,
	location: Range<usize>,
) -> WithErrors<CompType> {
	let mut errors = Vec::new();
	let ty = match ty {
		CustomType::Struct(data) => CompType::Struct(
			data.iter()
				.map(|(key, value)| {
					(
						key.clone(),
						transform_type(value, scope, location.clone())
							.collect_errors_into(&mut errors),
					)
				})
				.collect(),
		),
		CustomType::Array(el_ty, len) => {
			let el_ty =
				transform_type(el_ty, scope, location.clone()).collect_errors_into(&mut errors);
			let len = transform_type(len, scope, location.clone()).collect_errors_into(&mut errors);
			CompType::Array(el_ty.boxed(), len.boxed())
		}
		CustomType::Union(sub_types) => CompType::Union(map_vec!(sub_types, |x| {
			transform_type(x, scope, location.clone()).collect_errors_into(&mut errors)
		}))
		.flatten(),

		CustomType::Callible(generics, args, ret) => {
			let mut scope = scope.create_child(Vec::new());
			for (pos, (name, ty)) in generics.iter().enumerate() {
				let ty = if let Some(ty) = ty {
					transform_type(ty, &scope, location.clone()).collect_errors_into(&mut errors)
				} else {
					CompType::Unknown
				};
				scope.add_type(name.to_string(), CompType::Generic(pos, ty.clone().boxed()));
			}
			let args = map_vec!(args, |x| transform_type(x, &scope, location.clone())
				.collect_errors_into(&mut errors));
			let ret =
				transform_type(ret, &scope, location.clone()).collect_errors_into(&mut errors);
			CompType::Callible(args, ret.boxed())
		}
		CustomType::Lone(referenced_type) => {
			let found_type = scope.get_type(&referenced_type.name);
			if let Ok(found_type) = found_type {
				let generics = map_vec!(referenced_type.generics, |x| {
					transform_type(x, scope, location.clone()).collect_errors_into(&mut errors)
				});
				// We want to preserve lone generics
				// As in we only want to substitute generics that are part of another type
				// Generics found here are ones that get substituted in themselves
				if !matches!(found_type, CompType::Generic(_, _)) {
					found_type
						.substitute_generics(&generics, location)
						.collect_errors_into(&mut errors)
				} else {
					found_type
				}
			} else {
				errors.push(CompError::CannotFindType(
					referenced_type.clone().name,
					0..0,
				));
				CompType::Unknown
			}
		}
		CustomType::Constant(data) => CompType::Constant(data.clone()),
	};
	WithErrors::new(ty, errors)
}

impl Function {
	fn transform_function(&self, scope: &mut Scope, file: &str) -> WithErrors<FunctionAst> {
		if self.generics
			== vec![(
				"T".to_string(),
				Some(CustomType::Lone(UseType {
					name: "Int".to_string(),
					generics: Vec::new(),
				})),
			)] {
			return self.transform_function_single_constant_int(scope, file);
		}

		let mut errs = Vec::new();
		for (pos, (name, constraint)) in self.generics.iter().enumerate() {
			let constraint = match constraint {
				Some(ty) => transform_type(ty, scope, 0..0).collect_errors_into(&mut errs),
				None => CompType::Unknown,
			};
			scope.add_type(name.clone(), CompType::Generic(pos, constraint.boxed()));
		}

		let mut arguments = Vec::new();
		for arg in &self.args {
			let arg_ty = if let Some(ty) = &arg.1 {
				transform_type(ty, scope, arg.0 .1.clone()).collect_errors_into(&mut errs)
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
		let return_type = transform_type(&self.return_type.0, scope, self.return_type.1.clone())
			.collect_errors_into(&mut errs);
		let arguments = arguments;
		let func = match &self.body {
			Some(body) => {
				let mut local_scope = scope.create_child(arguments.clone());
				let local_scope = local_scope.resolve_scope(body.as_ref(), file);
				let body =
					transform_ast(body.as_ref(), local_scope, file).collect_errors_into(&mut errs);
				FunctionAst {
					generic_arguments: arguments.clone(),
					arguments,
					generic_return_type: return_type.clone(),
					return_type,
					body: Some(Box::new(body)),
				}
			}
			None => FunctionAst {
				generic_arguments: arguments.clone(),
				arguments,
				generic_return_type: return_type.clone(),
				return_type,
				body: None,
			},
		};
		WithErrors::new(func, errs)
	}

	fn transform_function_single_constant_int(
		&self,
		scope: &Scope,
		file: &str,
	) -> WithErrors<FunctionAst> {
		let mut errs = Vec::new();
		let (name, _constraint) = self.generics.first().unwrap();
		let mut local_scope = scope.create_child(Vec::new());
		local_scope.add_type(name.clone(), CompType::Int);

		let mut arguments = Vec::new();
		for arg in &self.args {
			let arg_ty = if let Some(ty) = &arg.1 {
				transform_type(ty, &local_scope, arg.0 .1.clone()).collect_errors_into(&mut errs)
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
		let return_type = transform_type(
			&self.return_type.0,
			&local_scope,
			self.return_type.1.clone(),
		)
		.collect_errors_into(&mut errs);

		let arguments = arguments;
		let (generic_arguments, generic_return_type) = {
			let mut generic_scope = scope.create_child(Vec::new());
			for (pos, (name, constraint)) in self.generics.iter().enumerate() {
				let constraint = if let Some(ty) = constraint {
					transform_type(ty, &generic_scope, 0..0).collect_errors_into(&mut errs)
				} else {
					CompType::Unknown
				};
				generic_scope.add_type(name.clone(), CompType::Generic(pos, constraint.boxed()));
			}

			let mut arguments = Vec::new();
			for arg in &self.args {
				let arg_ty = if let Some(ty) = &arg.1 {
					transform_type(ty, &generic_scope, arg.0 .1.clone())
						.collect_errors_into(&mut errs)
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
			let return_type =
				transform_type(&self.return_type.0, scope, self.return_type.1.clone())
					.collect_errors_into(&mut errs);
			(arguments, return_type)
		};

		let func = match &self.body {
			Some(body) => {
				let mut local_scope = scope.create_child(arguments.clone());
				let local_scope = local_scope.resolve_scope(body.as_ref(), file);
				let body =
					transform_ast(body.as_ref(), local_scope, file).collect_errors_into(&mut errs);
				FunctionAst {
					generic_arguments,
					arguments,
					generic_return_type,
					return_type,
					body: Some(Box::new(body)),
				}
			}
			None => FunctionAst {
				generic_arguments,
				arguments,
				generic_return_type,
				return_type,
				body: None,
			},
		};
		WithErrors::new(func, errs)
	}
}

pub fn resolve_type(
	exp: &mut CompExpression,
	_env: &ExpEnvironment,
	located: Range<usize>,
) -> WithErrors<CompType> {
	use CompExpression::*;
	let mut errs: Vec<CompError> = Vec::new();
	let ty = match exp {
		Conversion(_exp, ty) => ty.clone(),
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
				result_type
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
				result_type
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
				result_type
			} else {
				errs.push(CompError::PropertyDoesNotExistOnType(
					key.clone(),
					val.result_type.clone(),
					located,
				));
				CompType::Unknown
			}
		}
		Struct(fields) => CompType::Struct(
			fields
				.iter()
				.map(|(k, v)| (k.clone(), v.1.result_type.clone()))
				.collect(),
		),
		Array(elements) => {
			let el_types = map_vec!(elements, |el| el.result_type.widen());
			CompType::Array(
				el_types
					.first()
					.cloned()
					.unwrap_or(CompType::Unknown)
					.boxed(),
				CompType::Constant(ConstantData::Int(elements.len() as i32)).boxed(),
			)
		}

		Typeof(_) => CompType::Type,
		List(exps) => exps
			.last()
			.map(|x| x.result_type.clone())
			.unwrap_or(CompType::Null),
		WhileLoop { cond, body } => {
			if !cond.result_type.is_bool() {
				errs.push(CompError::BoolInWhile(
					cond.result_type.clone(),
					cond.located.clone(),
				));
			}
			body.result_type.clone()
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
			CompType::Union(vec![then_ty, other_ty])
		}
		Value(data) => data.get_type(),
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
			result_type
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
					CompType::get_top_type(&elements).boxed(),
					CompType::Constant(ConstantData::Int(elements.len() as i32)).boxed(),
				)
			} else {
				exp_ty
			};
			if !var_ty.super_type_of(&exp_ty) {
				errs.push(CompError::InvalidAssignment(
					exp_ty.clone(),
					var_ty.clone(),
					located,
				));
			}
			var.accessing = accesses;
			exp_ty
		}
		BinOp(op, a, b) => op
			.resulting_type(&a.result_type, &b.result_type)
			.unwrap_or_else(|err| {
				errs.push(err);
				CompType::Unknown
			}),
		OneOp(_, val) => val.result_type.clone(),
		Read(var) => var.get_type(),
		Call(var, generics, args) => {
			let result_type = if let CompType::Callible(arg_types, return_type) = var.get_type() {
				if arg_types.len() != args.len() {
					errs.push(CompError::WrongArgumentsCount(
						var.get_name(),
						args.len(),
						arg_types.len(),
						located.clone(),
					))
				}
				for (x, y) in arg_types.iter().zip(args) {
					let x = x
						.substitute_generics(generics, located.clone())
						.collect_errors_into(&mut errs);
					if !x.super_type_of(&y.result_type) {
						errs.push(CompError::InvalidAssignment(
							y.result_type.clone(),
							x.clone(),
							y.located.clone(),
						))
					}
				}
				// We've checked the arguments, the resulting type must be the return type of the function
				return_type
					.substitute_generics(generics, located.clone())
					.collect_errors_into(&mut errs)
			} else {
				errs.push(CompError::NonfunctionCall(
					var.get_name(),
					var.get_type(),
					located,
				));
				CompType::Unknown
			};
			result_type
		}
	};
	WithErrors::new(ty, errs)
}

pub fn get_env(
	mut exp: CompExpression,
	env: &ExpEnvironment,
	located: Range<usize>,
	mut errs: Vec<CompError>,
) -> WithErrors<ExpEnvironment> {
	let ty = resolve_type(&mut exp, env, located.clone()).collect_errors_into(&mut errs);
	WithErrors::new(
		ExpEnvironment {
			expression: Box::new(exp.clone()),
			result_type: ty,
			located,
		},
		errs,
	)
}

fn get_env_from_scope(_scope: &Scope) -> ExpEnvironment {
	ExpEnvironment {
		result_type: CompType::Null,
		located: 0..0,
		expression: Box::new(CompExpression::List(Vec::new())),
	}
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

impl RawData {
	pub fn transform(&self, scope: &mut Scope, file: &str) -> WithErrors<CompData> {
		let mut errors: Vec<CompError> = Vec::new();
		let data = match self {
			RawData::Int(val) => CompData::Int(*val),
			RawData::Float(val) => CompData::Float(*val),
			RawData::Str(val) => CompData::Str(val.clone()),
			RawData::Bool(val) => CompData::Bool(*val),
			RawData::Null => CompData::Null,
			RawData::Func(func) => CompData::Func(
				func.transform_function(scope, file)
					.collect_errors_into(&mut errors),
			),
		};
		WithErrors { data, errors }
	}
}

impl CompExpression {
	pub fn replace_generics(&self, generics: &[CompType]) -> Self {
		use CompExpression::*;
		let generics_err_msg =
			"Generics error when calling generic function with generics from current function";
		match &self {
			Array(expressions) => Array(
				expressions
					.iter()
					.map(|x| x.replace_generics(generics))
					.collect(),
			),
			DotAccess(exp, property) => DotAccess(exp.replace_generics(generics), property.clone()),
			// TODO: Work out how to keep track of generics through nested functions
			// Currently it will take on multiple sets of generics but not distinguish between which set when accessing them by position
			Value(x) => Value(x.clone()),
			Typeof(exp) => Typeof(exp.replace_generics(generics)),
			Struct(fields) => Struct(
				fields
					.iter()
					.map(|(prop, (loc, exp))| {
						(prop.clone(), (loc.clone(), exp.replace_generics(generics)))
					})
					.collect(),
			),
			BinOp(op, left, right) => BinOp(
				op.clone(),
				left.replace_generics(generics),
				right.replace_generics(generics),
			),
			// TODO: make generics work properly here
			Read(var) => Read(var.clone()),
			OneOp(op, exp) => OneOp(op.clone(), exp.replace_generics(generics)),
			Call(var, passed_generics, args) => Call(
				var.clone(),
				map_vec!(passed_generics, |x| {
					x.substitute_generics(generics, 0..0)
						.expect(generics_err_msg)
				}),
				args.iter().map(|x| x.replace_generics(generics)).collect(),
			),
			Assign(mem, exp) => Assign(mem.clone(), exp.replace_generics(generics)),
			IfElse(crate::ast2::IfElse {
				cond,
				then,
				otherwise,
			}) => IfElse(crate::ast2::IfElse {
				cond: cond.replace_generics(generics),
				then: then.replace_generics(generics),
				otherwise: otherwise.replace_generics(generics),
			}),
			WhileLoop { cond, body } => WhileLoop {
				cond: cond.replace_generics(generics),
				body: body.replace_generics(generics),
			},
			Index(arr, i) => Index(arr.replace_generics(generics), i.replace_generics(generics)),
			List(expressions) => List(
				expressions
					.iter()
					.map(|x| x.replace_generics(generics))
					.collect(),
			),
			Conversion(exp, ty) => Conversion(
				exp.replace_generics(generics),
				ty.substitute_generics(generics, 0..0)
					.expect("Error in conversion"),
			),
		}
	}
}
impl ExpEnvironment {
	pub fn replace_generics(&self, generics: &[CompType]) -> Self {
		let exp = self.expression.replace_generics(generics);
		let ty = self
			.result_type
			.substitute_generics(generics, self.located.clone())
			.expect("Errors found when substituting generics in expressions");
		Self {
			expression: Box::new(exp),
			result_type: ty,
			located: self.located.clone(),
		}
	}
}
