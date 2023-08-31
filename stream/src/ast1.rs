use crate::errors::CompError;
use crate::map_vec;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::hash::{self, Hasher};
use std::rc::Rc;
use std::{collections::HashMap, ops::Range};

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
	Data(RawData),
	Identifier(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RawData {
	Str(String),
	Bool(bool),
	Int(i32),
	Float(f32),
	Func(Function),
	Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Import {
	All(Option<String>),
	Specific(Vec<(String, Option<String>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportFrom {
	pub imports: Import,
	pub file: String,
}

pub type SpannedExpression = (Range<usize>, Expression);

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
	DotAccess(Box<SpannedExpression>, (String, Range<usize>)),
	Struct(Vec<((String, Range<usize>), SpannedExpression)>),
	// The name of the new type, a list of named generics with optional type constraints and the shape of the new type
	TypeDeclaration(String, Vec<(String, Option<CustomType>)>, CustomType),
	Typeof(Box<SpannedExpression>),
	Array(Vec<SpannedExpression>),
	BinOp(Op, Box<SpannedExpression>, Box<SpannedExpression>),
	Terminal(Symbol),
	FuncCall(String, Vec<CustomType>, Vec<SpannedExpression>),
	Block(Vec<SpannedExpression>),
	IfElse(
		Box<SpannedExpression>,
		Box<SpannedExpression>,
		Box<SpannedExpression>,
	),
	Loop(Box<SpannedExpression>, Box<SpannedExpression>),
	Invalid,
	Assign(Box<SpannedExpression>, Box<SpannedExpression>),
	InitAssign(
		bool,
		bool,
		(String, Range<usize>),
		Option<CustomType>,
		Box<SpannedExpression>,
	),
	Index(Box<SpannedExpression>, Box<SpannedExpression>),
	Conversion(Box<SpannedExpression>, CustomType),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
	pub generics: Vec<(String, Option<CustomType>)>,
	pub args: Vec<((String, Range<usize>), Option<CustomType>)>,
	pub body: Option<Box<SpannedExpression>>,
	pub return_type: CustomType,
}

/// A type name and a list of generics to fill it out
/// Such as `Array<String>`
#[derive(Debug, Clone, PartialEq)]
pub struct UseType {
	pub generics: Vec<CustomType>,
	pub name: String,
}

impl UseType {
	pub fn simple(name: String) -> UseType {
		UseType {
			generics: Vec::new(),
			name,
		}
	}
	pub fn complex(name: String, generics: Vec<CustomType>) -> UseType {
		UseType { generics, name }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomType {
	Callible(Vec<(String, Option<Self>)>, Vec<Self>, Box<Self>),
	Union(Vec<Self>),
	Lone(UseType),
	Array(Box<Self>, Box<Self>),
	Struct(Vec<(String, Self)>),
	Constant(ConstantData),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustomTypeStruct {
	pub generics: Vec<String>,
	pub ty: CustomType,
}

impl CustomTypeStruct {
	pub fn simple(ty: CustomType) -> CustomTypeStruct {
		CustomTypeStruct {
			generics: Vec::new(),
			ty,
		}
	}
	pub fn complex(generics: Vec<String>, ty: CustomType) -> CustomTypeStruct {
		CustomTypeStruct { generics, ty }
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstantData {
	Str(String),
	Int(i32),
	Float(f32),
	Bool(bool),
	Null,
}

impl hash::Hash for ConstantData {
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		use ConstantData::*;
		match self {
			Null => hasher.write_u8(0),
			Int(i) => {
				hasher.write_u8(1);
				hasher.write_i32(*i);
			}
			Str(str) => {
				hasher.write_u8(2);
				hasher.write(str.as_bytes());
			}
			Float(f) => {
				hasher.write_u8(3);
				hasher.write(&f.to_le_bytes())
			}
			Bool(b) => {
				hasher.write_u8(3);
				hasher.write_u8(*b as u8);
			}
		}
	}
}
#[derive(Debug, PartialEq, Clone)]
pub enum Op {
	Add,
	Sub,
	Div,
	Mult,
	Eq,
	Neq,
	Le,
	Ge,
}

impl Op {
	pub fn get_str(&self) -> String {
		format!("{}", self)
	}
}
