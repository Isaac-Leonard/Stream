use crate::ast1::*;
use crate::ast3::*;
use crate::errors::CompError;
use crate::map_vec;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::hash::{self, Hasher};
use std::rc::Rc;
use std::{collections::HashMap, ops::Range};

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum CompType {
	Unknown,
	Not(Box<Self>),
	Callible(Vec<Self>, Box<Self>),
	Union(Vec<Self>),
	Array(Box<CompType>, Box<CompType>),
	Struct(Vec<(String, CompType)>),
	Null,
	Bool,
	Int,
	Float,
	Str(Box<CompType>),
	Ptr,
	/// Stores the position of the generic and the type it extends
	Generic(usize, Box<CompType>),
	Type,
	Constant(ConstantData),
	Touple(Vec<Self>),
	Char,
}

impl CompType {
	pub fn boxed(self) -> Box<Self> {
		Box::new(self)
	}

	pub fn contains_generic(&self) -> bool {
		use CompType::*;
		match self {
			Generic(_, _) => true,
			Type | Char | Unknown | Bool | Ptr | Int | Float | Null | Constant(_) | Not(_) => false,
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
			Self::Array(el_ty, len) => Self::Array(el_ty.widen().boxed(), len.widen().boxed()),
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

	pub fn super_of(&self, ty: &CompType) -> bool {
		if self == ty || (self == &CompType::Ptr && ty.is_str()) {
			true
		} else if let CompType::Union(types) = self {
			// TODO: Do deeper checking here
			types.contains(ty)
		} else if let CompType::Constant(data) = ty {
			match (self, data) {
				(CompType::Str(len), ConstantData::Str(str)) => match len.as_ref() {
					CompType::Constant(ConstantData::Int(len)) => *len == str.len() as i32,
					_ => false,
				},
				(CompType::Int, ConstantData::Int(_)) => true,
				(CompType::Float, ConstantData::Float(_)) => true,
				(CompType::Bool, ConstantData::Bool(_)) => true,
				(CompType::Null, ConstantData::Null) => true,
				_ => false,
			}
		} else if let (Self::Struct(data), Self::Struct(sub)) = (self, ty) {
			for (a, b) in data.iter().zip(sub) {
				if !a.1.super_of(&b.1) {
					return false;
				}
			}
			true
		} else if self == &CompType::Unknown || ty == &CompType::Unknown {
			// Return true if either is unknown
			// Fix to make work properly with generics
			true
		} else {
			false
		}
	}

	/// Recursively examines itself returning a new copy of itself with generic types substituted where they are found
	/// TODO: Work out a way to determine if too many generics were provided
	pub fn substitute_generics(&self, generics: &Vec<CompType>) -> (CompType, Vec<CompError>) {
		use CompType::*;
		let mut errors = Vec::new();
		macro_rules! substitute_generics {
			($ty:expr) => {{
				let mut res = $ty.substitute_generics(generics);
				errors.append(&mut res.1);
				res.0
			}};
		}

		let ty = match self {
			Ptr | Type | Not(_) | Unknown | Char | Bool | Constant(_) | Null | Int | Float => {
				self.clone()
			}
			Str(len) => Str(substitute_generics!(len).boxed()),
			Array(el_ty, len) => Array(
				substitute_generics!(el_ty).boxed(),
				substitute_generics!(len).boxed(),
			),
			Struct(data) => Struct(map_vec!(data, |x| (x.0.clone(), substitute_generics!(x.1)))),
			Union(types) => Union(map_vec!(types, |ty| substitute_generics!(ty))),
			Touple(elements) => Touple(map_vec!(elements, |ty| substitute_generics!(ty))),
			Callible(args, ret) => Callible(
				map_vec!(args, |x| substitute_generics!(x)),
				substitute_generics!(ret).boxed(),
			),
			Generic(pos, extends_ty) => {
				let substitute_ty = generics.get(*pos);
				if let Some(ty) = substitute_ty {
					if extends_ty.super_of(ty) {
						ty.clone()
					} else if let CompType::Generic(_, sub_ty) = ty {
						if sub_ty.super_of(extends_ty) {
							ty.clone()
						} else {
							errors.push(CompError::MismatchedGenericConstraint(
								ty.clone(),
								extends_ty.as_ref().clone(),
								0..0,
							));
							CompType::Unknown
						}
					} else {
						errors.push(CompError::MismatchedGenericConstraint(
							ty.clone(),
							extends_ty.as_ref().clone(),
							0..0,
						));
						CompType::Unknown
					}
				} else {
					errors.push(CompError::NotEnoughGenerics(0..0));
					CompType::Unknown
				}
			} // TODO: Hack till I can fill in the rest (I'm tired and its late)
		};
		(ty.flatten(), errors)
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
			(Struct(a), Struct(b)) => a.iter().zip(b).flat_map(|x| x.0.1.match_generics(&x.1.1)).collect(),
			// TODO: Handling here is wrong and needs to be fixed
			(Union(a), Union(b)) => a.iter().zip(b).flat_map(|x| x.0.match_generics(x.1)).collect(),
			(Callible(a_args, a_ret), Callible(b_args, b_ret)) => {
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
			Touple(elements) => Touple(map_vec!(elements, |el| el.flatten())),
			Constant(data) => Constant(data.clone()),
			Struct(keys) => {
				let mut keys = keys
					.iter()
					.map(|(k, v)| (k.clone(), v.flatten()))
					.collect::<Vec<_>>();
				keys.sort_by(|a, b| a.0.cmp(&b.0));
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
	Struct(Vec<((String, Range<usize>), ExpEnvironment)>),
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
	/// List of compilation errors
	pub errors: Vec<CompError>,
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
			CompExpression::Struct(key_vals) => key_vals.iter().find_map(|x| x.1.find_map(matcher)),
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
			CompExpression::Struct(key_vals) => {
				current.append(&mut key_vals.iter().flat_map(|x| x.1.map_each(mapper)).collect());
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
			CompExpression::Struct(key_vals) => {
				key_vals.iter_mut().for_each(|x| x.1.map_inplace(mapper))
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
										errors: Vec::new(),
									}),
									element.result_type.clone(),
								)],
							},
							element.clone(),
						)),
						result_type: element.result_type.clone(),
						located: 0..0,
						errors: Vec::new(),
					})
					.collect();

				list.push(ExpEnvironment {
					expression: Box::new(CompExpression::Read(var)),
					result_type: x.result_type.clone(),
					located: x.located.clone(),
					errors: Vec::new(),
				});
				Some(ExpEnvironment {
					expression: Box::new(CompExpression::List(list)),
					result_type: x.result_type.clone(),
					located: x.located.clone(),
					errors: x.errors.clone(),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
	pub scope: Scope,
	pub body: ExpEnvironment,
}

impl Program {
	pub fn get_exported(&self) -> Vec<CompVariable> {
		self.scope
			.variables
			.iter()
			.filter(|x| x.1.is_extern())
			.map(|x| x.1.clone())
			.collect()
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
	pub types: HashMap<String, CompType>,
	pub variables: HashMap<String, CompVariable>,
	pub preset_variables: HashMap<String, CompVariable>,
	pub parent: Option<Box<Self>>,
}

impl Scope {
	pub fn variable_exists(&self, name: &String) -> bool {
		if self.variables.contains_key(name) || self.preset_variables.contains_key(name) {
			true
		} else {
			match &self.parent {
				Some(parent) => (*parent).variable_exists(name),
				None => false,
			}
		}
	}

	pub fn add_variable(&mut self, var: CompVariable) -> &mut Scope {
		self.variables.insert(var.get_name(), var);
		self
	}

	pub fn add_type(&mut self, name: String, ty: CompType) -> &mut Scope {
		self.types.insert(name, ty);
		self
	}

	pub fn set_variable_initialised(&mut self, name: &String) {
		if let Some(var) = self.variables.get_mut(name) {
			var.set_initialised();
		}
	}

	pub fn set_variable_type<'a>(&'a mut self, name: &String, ty: &CompType) -> &'a mut Scope {
		if let Some(v) = self.variables.get_mut(name) {
			v.set_type(ty.clone());
		}
		self
	}

	pub fn get_variable(&self, name: &String) -> Result<CompVariable, String> {
		if let Some(var) = self.variables.get(name) {
			Ok(var.clone())
		} else if let Some(var) = self.preset_variables.get(name) {
			Ok(var.clone())
		} else {
			match &self.parent {
				Some(parent) => (*parent).get_variable(name),
				None => Err(format!("Cannot find variable '{}'", name)),
			}
		}
	}

	pub fn get_type(&self, name: &String) -> Result<CompType, CompError> {
		if let Some(ty) = self.types.get(name) {
			Ok(ty.clone())
		} else {
			match &self.parent {
				Some(parent) => (*parent).get_type(name),
				None => Err(CompError::CannotFindType(name.clone(), 0..0)),
			}
		}
	}

	pub fn constant_exists(&self, name: &String) -> bool {
		if let Some(var) = self.variables.get(name) {
			var.is_const()
		} else {
			match &self.parent {
				Some(parent) => (*parent).constant_exists(name),
				None => false,
			}
		}
	}

	pub fn variable_initialised(&self, name: &String) -> bool {
		if let Some(var) = self.variables.get(name) {
			var.is_initialised()
		} else {
			match &self.parent {
				Some(parent) => (*parent).constant_exists(name),
				None => false,
			}
		}
	}

	pub fn variable_has_type(&self, name: &String) -> bool {
		if let Some(var) = self.variables.get(name) {
			var.get_type() != CompType::Unknown
		} else {
			match &self.parent {
				Some(parent) => (*parent).constant_exists(name),
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
			Func(ast) => ast.as_type(),
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

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionAst {
	pub arguments: Vec<CompVariable>,
	pub return_type: CompType,
	pub body: Option<Box<Program>>,
}

impl FunctionAst {
	pub fn as_type(&self) -> CompType {
		CompType::Callible(
			self.arguments.iter().map(|x| x.get_type()).collect(),
			Box::new(self.return_type.clone()),
		)
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
				(Int, Constant(ConstantData::Int(_))) | (Constant(ConstantData::Int(_)), Int) => {
					Ok(Int)
				}
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
			_ => Err(self.invalid_comparison_msg(a, b)),
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
