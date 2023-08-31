use crate::ast1::*;
use crate::ast2::*;
use crate::errors::CompError;
use crate::map_vec;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::hash::{self, Hasher};
use std::rc::Rc;
use std::{collections::HashMap, ops::Range};

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
