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
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub generics: Vec<String>,
    pub args: Vec<((String, Range<usize>), CustomType)>,
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
pub struct NewVariable {
    pub name: String,
    pub typing: Option<CompType>,
    pub constant: bool,
    pub initialised: bool,
    pub external: bool,
    pub declared_at: (String, Range<usize>),
}
impl NewVariable {
    fn get_final(&self) -> Result<CompVariable, String> {
        if self.typing == None || !self.initialised {
            Err(format!("Cannot use uninitialised variable '{}'", self.name))
        } else {
            Ok(CompVariable {
                name: self.name.clone(),
                typing: self.typing.clone().unwrap(),
                constant: self.constant,
                external: self.external,
                declared_at: Some(self.declared_at.clone()),
            })
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompVariable {
    pub name: String,
    pub typing: CompType,
    pub constant: bool,
    pub external: bool,
    pub declared_at: Option<(String, Range<usize>)>,
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
            self.arguments.iter().map(|x| x.typing.clone()).collect(),
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
            Str(content) => CompType::Constant(ConstantData::Str(content.clone())),
            Func(ast) => ast.as_type(),
        }
    }
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
    Assign(ExpEnvironment, ExpEnvironment),
    IfOnly {
        cond: ExpEnvironment,
        then: ExpEnvironment,
    },
    IfElse {
        cond: ExpEnvironment,
        then: ExpEnvironment,
        otherwise: ExpEnvironment,
    },
    WhileLoop {
        cond: ExpEnvironment,
        body: ExpEnvironment,
    },
    Index(ExpEnvironment, ExpEnvironment),
    List(Vec<ExpEnvironment>),
    Prog(Program),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub scope: TempScope,
    pub body: ExpEnvironment,
}
impl Program {
    pub fn get_exported(&self) -> Vec<NewVariable> {
        self.scope
            .variables
            .iter()
            .filter(|x| x.1.external)
            .map(|x| x.1.clone())
            .collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TempScope {
    pub types: HashMap<String, CompType>,
    pub variables: HashMap<String, NewVariable>,
    pub preset_variables: HashMap<String, CompVariable>,
    pub parent: Option<Box<Self>>,
}
impl TempScope {
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

    pub fn add_variable(&mut self, var: NewVariable) -> &mut TempScope {
        self.variables.insert(var.name.clone(), var);
        self
    }

    pub fn add_type(&mut self, name: String, ty: CompType) -> &mut TempScope {
        self.types.insert(name, ty);
        self
    }

    pub fn set_variable_initialised(&mut self, name: &String) {
        if let Some(var) = self.variables.get_mut(name) {
            var.initialised = true;
        }
    }

    pub fn set_variable_type<'a>(&'a mut self, name: &String, ty: &CompType) -> &'a mut TempScope {
        if let Some(v) = self.variables.get_mut(name) {
            v.typing = Some(ty.clone());
        }
        self
    }

    pub fn get_variable(&self, name: &String) -> Result<CompVariable, String> {
        if let Some(var) = self.variables.get(name) {
            var.get_final()
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
            var.constant
        } else {
            match &self.parent {
                Some(parent) => (*parent).constant_exists(name),
                None => false,
            }
        }
    }

    pub fn variable_initialised(&self, name: &String) -> bool {
        if let Some(var) = self.variables.get(name) {
            var.initialised
        } else {
            match &self.parent {
                Some(parent) => (*parent).constant_exists(name),
                None => false,
            }
        }
    }
    pub fn variable_has_type(&self, name: &String) -> bool {
        if let Some(var) = self.variables.get(name) {
            var.typing != None
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

#[derive(Clone, Debug, PartialEq)]
pub struct PlainScope {
    parents: Vec<usize>,
    child_count: usize,
    children: Vec<PlainScopeRef>,
}
impl PlainScope {
    pub fn new() -> Self {
        Self {
            parents: Vec::new(),
            child_count: 0,
            children: Vec::new(),
        }
    }

    pub fn create_child(&mut self) -> PlainScopeRef {
        let mut parents = self.parents.clone();
        parents.push(self.child_count);
        self.child_count += 1;
        let child = PlainScopeRef::from(Self {
            parents,
            child_count: 0,
            children: Vec::new(),
        });
        self.children.push(child.clone());
        child
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariableOptions {
    Read,
    Write,
    Declare,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlainScopeRef(Rc<RefCell<PlainScope>>);
impl PlainScopeRef {
    pub fn parents(&self) -> Vec<usize> {
        self.0.as_ref().borrow().parents.clone()
    }
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(PlainScope::new())))
    }

    fn from(scope: PlainScope) -> Self {
        Self(Rc::new(RefCell::new(scope)))
    }

    pub fn create_child(&self) -> Self {
        self.0.as_ref().borrow_mut().create_child()
    }
}

#[derive(Clone, Debug, PartialEq)]
struct VariableTracker {
    occurs: usize,
    scope: PlainScopeRef,
    access_type: VariableOptions,
}

#[derive(Clone, Debug, PartialEq)]
struct VariableMap {
    occurs: usize,
    writes: HashMap<(String, Vec<usize>), Vec<VariableTracker>>,
    reads: HashMap<(String, Vec<usize>), Vec<VariableTracker>>,
}
impl VariableMap {
    pub fn new() -> Self {
        Self {
            occurs: 0,
            writes: HashMap::new(),
            reads: HashMap::new(),
        }
    }
    pub fn read(&mut self, name: &str, scope: &PlainScopeRef) {
        self.occurs += 1;
        let tracker = VariableTracker {
            scope: scope.clone(),
            occurs: self.occurs,
            access_type: VariableOptions::Read,
        };
        let mut parent = scope.parents();
        loop {
            let key = (name.to_string(), parent.clone());
            if let Some(trackers) = self.writes.get_mut(&key) {
                return trackers.push(tracker);
            }
            if parent.pop().is_none() {
                panic!("Attempted to read variable without declaration in counting code");
            };
        }
    }

    pub fn write(&mut self, name: &str, scope: &PlainScopeRef) {
        self.occurs += 1;
        let tracker = VariableTracker {
            scope: scope.clone(),
            occurs: self.occurs,
            access_type: VariableOptions::Write,
        };
        let mut parent = scope.parents();
        loop {
            let key = (name.to_string(), parent.clone());
            if let Some(trackers) = self.writes.get_mut(&key) {
                return trackers.push(tracker);
            }
            if parent.pop().is_none() {
                panic!("Attempted to write variable without declaration in counting code");
            };
        }
    }
    pub fn declare(&mut self, name: &str, scope: &PlainScopeRef) {
        self.occurs += 1;
        let tracker = VariableTracker {
            scope: scope.clone(),
            occurs: self.occurs,
            access_type: VariableOptions::Declare,
        };
        let key = (name.to_string(), scope.parents());
        if let std::collections::hash_map::Entry::Vacant(e) = self.writes.entry(key) {
            e.insert(vec![tracker]);
        } else {
            panic!("Attempted to redeclare variable in counting scopes");
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable(Rc<RefCell<VariableMap>>);
impl SymbolTable {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(VariableMap {
            occurs: 0,
            writes: HashMap::new(),
            reads: HashMap::new(),
        })))
    }

    pub fn read(&self, name: &str, scope: &PlainScopeRef) {
        self.0.as_ref().borrow_mut().read(name, scope)
    }

    pub fn write(&mut self, name: &str, scope: &PlainScopeRef) {
        self.0.as_ref().borrow_mut().read(name, scope)
    }
    pub fn declare(&self, name: &str, scope: &PlainScopeRef) {
        self.0.as_ref().borrow_mut().declare(name, scope)
    }
}
