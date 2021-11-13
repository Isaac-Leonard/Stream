use std::{cell::RefCell, rc::Rc};
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
    Func(Function),
    ActiveFunc(ActiveFunction),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    Terminal(Symbol),
    FuncCall(String, Vec<Expression>),
}

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: Vec<(String, Vec<String>)>,
    pub body: Vec<Instr>,
    pub return_type: Vec<String>,
}
impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function({:?})", self.args)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    Invalid(String),
    Assign(String, Expression),
    LoneExpression(Expression),
    Loop(Expression, Vec<Self>),
    IfElse(Expression, Vec<Instr>, Vec<Instr>),
}
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDescriptor {
    pub name: String,
    pub shape: LangType,
}
#[derive(Clone, Debug, PartialEq)]
pub enum LangType {
    Bool,
    Int,
    Str,
    Func(Vec<LangType>, Box<LangType>),
    Union(Vec<LangType>),
    Null,
}
impl LangType {
    pub fn flatten(&self) -> LangType {
        match self {
            LangType::Union(types) => {
                if types.len() == 1 {
                    return types[0].clone();
                } else {
                    return self.clone();
                }
            }
            LangType::Func(args, ret) => LangType::Func(
                args.iter().map(|x| x.flatten()).collect(),
                Box::new(ret.flatten()),
            ),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub variables: Vec<Variable>,
    pub types: Vec<TypeDescriptor>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeRef(pub Rc<RefCell<Scope>>);
impl ScopeRef {
    pub fn new(variables: Vec<Variable>, parent: ScopeRef) -> ScopeRef {
        ScopeRef(Rc::new(RefCell::new(Scope {
            parent: Some(parent.0),
            variables,
            types: Vec::new(),
        })))
    }

    pub fn get_copy(&self) -> ScopeRef {
        ScopeRef(Rc::clone(&self.0))
    }

    pub fn set_variable(
        &self,
        name: &String,
        value: RawData,
        types: &Vec<TypeDescriptor>,
    ) -> RawData {
        let data_type = &get_type(&value, types).unwrap();
        let location = self.get_variable_location(name);
        match location {
            Some((scope, index)) => {
                let variables = &mut scope.0.as_ref().borrow_mut().variables;
                variables[index].value = value.clone();
            }
            None => {
                self.0.as_ref().borrow_mut().variables.push(Variable {
                    name: name.clone(),
                    value: value.clone(),
                    data_type: data_type.clone(),
                });
            }
        }

        return value.clone();
    }

    pub fn get_index_local(&self, name: &String) -> Option<usize> {
        Rc::clone(&self.0)
            .as_ref()
            .borrow()
            .variables
            .iter()
            .position(|x| x.name == *name)
    }

    pub fn get_variable_location(&self, name: &String) -> Option<(ScopeRef, usize)> {
        let index = self.get_index_local(name);
        match index {
            Some(i) => Some((self.get_copy(), i)),
            None => self
                .0
                .borrow_mut()
                .parent
                .as_ref()
                .map(|x| ScopeRef(Rc::clone(&x)).get_variable_location(name))
                .flatten(),
        }
    }

    pub fn get_variable(&self, name: &String) -> Option<Variable> {
        let location = self.get_variable_location(name);
        match location {
            Some((scope, index)) => Some(scope.0.as_ref().borrow().variables[index].clone()),

            None => None,
        }
    }
}

#[derive(Clone)]
pub struct ActiveFunction {
    pub args: Vec<(String, Vec<String>)>,
    pub call: fn(
        args: Vec<(String, Vec<String>)>,
        params: Vec<RawData>,
        body: &Vec<Instr>,
        stack: ScopeRef,
        types: &Vec<TypeDescriptor>,
        run: fn(
            ast: &[Instr],
            variables: ScopeRef,
            types: &Vec<TypeDescriptor>,
            global: bool,
        ) -> RawData,
    ) -> RawData,
    pub body: Vec<Instr>,
    pub stack: ScopeRef,
    pub return_type: Vec<String>,
}
impl PartialEq for ActiveFunction {
    fn eq(&self, other: &Self) -> bool {
        // Performs a rudimentary check to ensure return types and args have the same names
        // TOdO: Must improve this to full checking later.
        if self.return_type != other.return_type {
            return false;
        }
        if self.args != other.args {
            return false;
        }
        return true;
    }
}

impl ActiveFunction {
    pub fn from_raw(func: Function, stack: ScopeRef) -> ActiveFunction {
        ActiveFunction {
            args: func.args,
            call: |args, params, body, stack, itypes, run| {
                if args.len() != params.len() {
                    panic!("Called custom function with the wrong number of arguments")
                }
                let local_variables = args
                    .iter()
                    .zip(params)
                    .map(|(arg, param)| Variable {
                        name: arg.0.clone(),
                        value: param,
                        data_type: consolidate_type(&arg.1, itypes).unwrap(),
                    })
                    .collect::<Vec<_>>();
                let local_stack = ScopeRef::new(local_variables, stack);
                let res = run(&body, local_stack, itypes, false);
                return res;
            },
            body: func.body,
            stack,
            return_type: func.return_type,
        }
    }
    pub fn execute(
        &self,
        params: Vec<RawData>,
        types: &Vec<TypeDescriptor>,
        run: fn(
            ast: &[Instr],
            variables: ScopeRef,
            types: &Vec<TypeDescriptor>,
            global: bool,
        ) -> RawData,
    ) -> RawData {
        (self.call)(
            self.args.clone(),
            params,
            &self.body,
            self.stack.get_copy(),
            types,
            run,
        )
    }
}
impl std::fmt::Debug for ActiveFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function({:?})", self.args)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub name: String,
    pub data_type: LangType,
    pub value: RawData,
}
pub fn consolidate_type(
    names: &Vec<String>,
    defined_types: &Vec<TypeDescriptor>,
) -> Result<LangType, Vec<String>> {
    let res = names
        .iter()
        .map(|name| {
            defined_types
                .iter()
                .find(|x| x.name == *name)
                .ok_or(name.clone())
                .map(|x| x.shape.clone())
        })
        .collect::<Vec<Result<LangType, String>>>();
    let errors = res
        .iter()
        .filter_map::<String, _>(|x| x.clone().err())
        .collect::<Vec<_>>();
    if errors.len() > 0 {
        Result::Err(errors)
    } else {
        // Flatten the union to simplify the type if its only got one member
        Result::Ok(LangType::Union(res.iter().map(|x| x.clone().unwrap()).collect()).flatten())
    }
}

pub fn get_type(val: &RawData, types: &Vec<TypeDescriptor>) -> Result<LangType, Vec<String>> {
    use LangType::*;
    match val {
        RawData::Int(_) => Ok(Int),
        RawData::Bool(_) => Ok(Bool),
        RawData::Null => Ok(Null),
        RawData::Str(_) => Ok(Str),
        RawData::ActiveFunc(func) => {
            let arg_types = func
                .args
                .iter()
                .map(|x| consolidate_type(&x.1, &types))
                .collect::<Vec<_>>();
            let errors = arg_types
                .iter()
                .filter_map(|x| x.clone().err())
                .flatten()
                .collect::<Vec<_>>();
            if errors.len() > 0 {
                Err(errors)
            } else {
                let arg_types_flattened = arg_types
                    .iter()
                    .map(|x| x.clone().unwrap())
                    .collect::<Vec<_>>();
                Ok(Func(arg_types_flattened, Box::new(LangType::Null)))
            }
        }
        RawData::Func(func) => {
            let arg_types = func
                .args
                .iter()
                .map(|x| consolidate_type(&x.1, &types))
                .collect::<Vec<_>>();
            let errors = arg_types
                .iter()
                .filter_map(|x| x.clone().err())
                .flatten()
                .collect::<Vec<_>>();
            if errors.len() > 0 {
                Err(errors)
            } else {
                let arg_types_flattened = arg_types
                    .iter()
                    .map(|x| x.clone().unwrap())
                    .collect::<Vec<_>>();
                Ok(Func(arg_types_flattened, Box::new(LangType::Null)))
            }
        }
    }
}
