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
    TypeDeclaration(String, CustomType),
    Typeof(Box<SpannedExpression>),
    Array(Vec<SpannedExpression>),
    BinOp(Op, Box<SpannedExpression>, Box<SpannedExpression>),
    Terminal(Symbol),
    FuncCall(String, Vec<SpannedExpression>),
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
    pub generics: Vec<String>,
    pub args: Vec<((String, Range<usize>), Option<CustomType>)>,
    pub body: Option<Box<SpannedExpression>>,
    pub return_type: CustomType,
}

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
    Callible(Vec<Self>, Box<Self>),
    Union(Vec<Self>),
    Lone(UseType),
    Array(Box<Self>, i32),
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
pub struct Variable {
    pub name: String,
    pub typing: CompType,
    pub initialised: bool,
    pub constant: bool,
    pub external: bool,
    pub declared_at: Option<(String, Range<usize>)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompVariable(Rc<RefCell<Variable>>);
impl CompVariable {
    pub fn new(init: Variable) -> Self {
        CompVariable(Rc::new(RefCell::new(init)))
    }

    pub fn get_name(&self) -> String {
        self.0.borrow().name.clone()
    }
    pub fn get_type(&self) -> CompType {
        // TODO: implement a version that returns a reference if possible
        self.0.borrow().typing.clone()
    }

    pub fn set_type(&self, ty: CompType) {
        self.0.borrow_mut().typing = ty;
    }

    pub fn is_const(&self) -> bool {
        self.0.borrow().constant
    }

    pub fn is_extern(&self) -> bool {
        self.0.borrow().external
    }

    pub fn get_declaration_location(&self) -> Option<(String, Range<usize>)> {
        self.0.borrow().declared_at.clone()
    }

    pub fn is_initialised(&self) -> bool {
        self.0.borrow_mut().initialised
    }

    pub fn set_initialised(&self) {
        self.0.borrow_mut().initialised = true;
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
                (Str(a), Str(b)) => Str(*a + *b),
                (Str(a), Constant(ConstantData::Str(b))) => Str(*a + b.len() as u32),
                (Constant(ConstantData::Str(a)), Str(b)) => Str(a.len() as u32 + *b),
                (Constant(ConstantData::Str(a)), Constant(ConstantData::Str(b))) => {
                    Constant(ConstantData::Str(a.clone() + b))
                }
                (Bool, Bool) | (Null, Null) | (Bool, Null) | (Null, Bool) => {
                    return Err(self.invalid_comparison_msg(a, b))
                }
                (Int, Int) => Int,
                (Float, Float) => Float,
                (Int, Constant(ConstantData::Int(_))) => Int,
                (Constant(ConstantData::Int(_)), Int) => Int,
                (Float, Constant(ConstantData::Float(_))) => Float,
                (Constant(ConstantData::Float(_)), Float) => Float,
                (Constant(ConstantData::Float(a)), Constant(ConstantData::Float(b))) => {
                    Constant(ConstantData::Float(a + b))
                }
                (Constant(ConstantData::Int(a)), Constant(ConstantData::Int(b))) => {
                    Constant(ConstantData::Int(a + b))
                }
                _ => return Err(self.invalid_comparison_msg(a, b)),
            }),
            Sub | Div | Mult => match (a, b) {
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
            Str(content) => CompType::Str(content.len() as u32),
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
pub enum IndexOption {
    Index(ExpEnvironment),
    Dot(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemoryLocation {
    pub variable: CompVariable,
    pub accessing: Vec<(IndexOption, CompType)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompExpression {
    DotAccess(ExpEnvironment, (String, Range<usize>)),
    Value(CompData),
    Array(Vec<ExpEnvironment>),
    Typeof(ExpEnvironment),
    Struct(Vec<((String, Range<usize>), ExpEnvironment)>),
    BinOp(Op, ExpEnvironment, ExpEnvironment),
    Read(CompVariable),
    OneOp(Prefix, ExpEnvironment),
    Call(CompVariable, Vec<ExpEnvironment>),
    Assign(MemoryLocation, ExpEnvironment),
    IfOnly {
        cond: ExpEnvironment,
        then: ExpEnvironment,
    },
    IfElse(IfElse),
    WhileLoop {
        cond: ExpEnvironment,
        body: ExpEnvironment,
    },
    Index(ExpEnvironment, ExpEnvironment),
    List(Vec<ExpEnvironment>),
    Prog(Program),
    Conversion(ExpEnvironment, CompType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpEnvironment {
    // Probably should be boxing where recursion actually occurs but this reduces the amount of code
    pub expression: Box<CompExpression>,
    pub var_types: HashMap<String, CompType>,
    pub result_type: CompType,
    pub located: Range<usize>,
    pub errors: Vec<CompError>,
}
impl ExpEnvironment {
    pub fn is_if_else(&self) -> bool {
        matches!(self.expression.as_ref(), CompExpression::IfElse(_))
    }
    fn find(&self, matcher: fn(&Self) -> bool) -> Option<&Self> {
        if matcher(self) {
            return Some(self);
        }
        match self.expression.as_ref() {
            CompExpression::List(exps)
            | CompExpression::Array(exps)
            | CompExpression::Call(_, exps) => exps.iter().find_map(|x| x.find(matcher)),
            CompExpression::Struct(key_vals) => key_vals.iter().find_map(|x| x.1.find(matcher)),
            CompExpression::IfElse(ifelse) => {
                let cond = ifelse.cond.find(matcher);
                if cond.is_some() {
                    return cond;
                }
                let then = ifelse.then.find(matcher);
                if then.is_some() {
                    return then;
                }
                let otherwise = ifelse.otherwise.find(matcher);
                if otherwise.is_some() {
                    return otherwise;
                }
                return None;
            }
            CompExpression::WhileLoop { cond: a, body: b }
            | CompExpression::BinOp(_, a, b)
            | CompExpression::IfOnly { cond: a, then: b }
            | CompExpression::Index(a, b) => {
                let a = a.find(matcher);
                if a.is_some() {
                    return a;
                }
                let b = b.find(matcher);
                if b.is_some() {
                    return b;
                }
                None
            }
            CompExpression::OneOp(_, exp)
            | CompExpression::Typeof(exp)
            | CompExpression::Conversion(exp, _)
            | CompExpression::DotAccess(exp, _) => exp.find(matcher),
            CompExpression::Assign(lvalue, rhs) => {
                let rhs = rhs.find(matcher);
                if rhs.is_some() {
                    return rhs;
                }
                for access in &lvalue.accessing {
                    if let IndexOption::Index(index) = &access.0 {
                        let access = index.find(matcher);
                        if access.is_some() {
                            return access;
                        }
                    }
                }
                None
            }
            CompExpression::Prog(prog) => prog.body.find(matcher),
            CompExpression::Read(_) | CompExpression::Value(_) => None,
        }
    }
    fn has(&self, matcher: fn(&Self) -> bool) -> bool {
        self.find(matcher).is_some()
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
pub enum ConstantData {
    Str(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Null,
}
impl ConstantData {
    pub fn widen(&self) -> CompType {
        use ConstantData::*;
        match self {
            Str(data) => CompType::Str(data.len() as u32),
            Int(_) => CompType::Int,
            Float(_) => CompType::Float,
            Bool(_) => CompType::Bool,
            Null => CompType::Null,
        }
    }
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

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum CompType {
    Unknown,
    Not(Box<Self>),
    Callible(Vec<Self>, Box<Self>),
    Union(Vec<Self>),
    Array(Box<CompType>, usize),
    Struct(Vec<(String, CompType)>),
    Null,
    Bool,
    Int,
    Float,
    Str(u32),
    Ptr,
    Generic(Box<CompType>),
    Type,
    Constant(ConstantData),
    Touple(Vec<Self>),
    Char,
}
impl CompType {
    pub fn is_primitive(&self) -> bool {
        use CompType::*;
        !matches!(self, Str(_) | Array(_, _) | Struct(_))
    }

    pub fn widen(&self) -> Self {
        match self {
            Self::Constant(data) => data.widen(),
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
                (CompType::Str(len), ConstantData::Str(str)) => *len == str.len() as u32,
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
            return true;
        } else {
            false
        }
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
            Array(ty, len) => Array(ty.clone(), *len),
            Int => Int,
            Float => Float,
            Str(len) => Str(*len),
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
            Generic(name) => Generic(name.clone()),
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
            Generic(name) => write!(f, "{}", name),
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
pub struct Accesses {
    pub variable: CompVariable,
    pub read: u32,
    pub write: u32,
    pub capture: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRVariable {}
#[derive(Debug, PartialEq, Clone)]
pub enum Readable {
    Variable(IRVariable),
    Value(ConstantData),
    Property(IRVariable, u32),
    Index(IRVariable, u32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Assignable {
    Variable(IRVariable),
    Property(IRVariable, u32),
    Index(IRVariable, u32),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRExpression {
    Typeof(Readable),
    BinOp(Op, Readable, Readable),
    OneOp(Prefix, Readable),
    Call(Readable, Vec<Readable>),
    Assign(Assignable, Readable),
    IfElse(IRVariable, Vec<Self>, Vec<Self>),
    WhileLoop(IRVariable, Vec<Self>),
    Conversion(Readable, CompType),
}
