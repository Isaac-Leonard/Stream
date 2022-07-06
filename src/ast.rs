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
    IfElse(IfElse),
    WhileLoop {
        cond: ExpEnvironment,
        body: ExpEnvironment,
    },
    Index(ExpEnvironment, ExpEnvironment),
    List(Vec<ExpEnvironment>),
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
        matches!(self.expression.as_ref(), CompExpression::Call(_, _))
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
            | CompExpression::Call(_, exps) => exps.iter().find_map(|x| x.find_map(matcher)),
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
            | CompExpression::Call(_, exps) => {
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

    pub fn map_inplace(&mut self, mapper: &mut dyn FnMut(&Self) -> Option<Self>) {
        if let Some(current) = mapper(self) {
            *self = current;
        };
        match self.expression.as_mut() {
            CompExpression::List(exps)
            | CompExpression::Array(exps)
            | CompExpression::Call(_, exps) => exps.iter_mut().for_each(|x| x.map_inplace(mapper)),
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
                let var = CompVariable::new(Variable {
                    name: ".array".to_string() + &count.to_string(),
                    initialised: false,
                    declared_at: None,
                    typing: x.result_type.clone(),
                    constant: false,
                    external: false,
                });
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
                                        var_types: x.var_types.clone(),
                                        located: 0..0,
                                        errors: Vec::new(),
                                    }),
                                    element.result_type.clone(),
                                )],
                            },
                            element.clone(),
                        )),
                        result_type: element.result_type.clone(),
                        var_types: x.var_types.clone(),
                        located: 0..0,
                        errors: Vec::new(),
                    })
                    .collect();

                list.push(ExpEnvironment {
                    expression: Box::new(CompExpression::Read(var)),
                    result_type: x.result_type.clone(),
                    var_types: x.var_types.clone(),
                    located: x.located.clone(),
                    errors: Vec::new(),
                });
                Some(ExpEnvironment {
                    expression: Box::new(CompExpression::List(list)),
                    result_type: x.result_type.clone(),
                    var_types: x.var_types.clone(),
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
            | CompExpression::Call(_, _)
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
