use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};
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
    InitAssign(bool, String, Option<Vec<String>>, Expression),
    LoneExpression(Expression),
    Loop(Expression, Vec<Self>),
    IfElse(Expression, Vec<Instr>, Vec<Instr>),
    TypeDeclaration(String, CustomType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomType {
    Callible(Vec<Self>, Box<Self>),
    // Only have union, can map singular types / aliases to a union then flaten them later
    Union(Vec<String>),
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
                .deref()
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

#[derive(Debug, PartialEq, Clone)]
pub struct CompVariable {
    name: String,
    typing: CompType,
    constant: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionAst {
    arguments: Vec<CompType>,
    return_type: CompType,
    body: Option<Box<Program>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mult,
    Eq,
    Le,
}
impl Op {
    fn get_str(&self) -> &str {
        use Op::*;
        match self {
            Add => "+",
            Sub => "-",
            Mult => "*",
            Div => "/",
            Eq => "==",
            Le => "<",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Neg,
}
impl Prefix {
    pub fn get_str(&self) -> &str {
        match self {
            Prefix::Neg => "-",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompExpression {
    Value(CompData),
    BinOp(Op, Box<CompExpression>, Box<CompExpression>),
    Read(CompVariable),
    OneOp(Prefix, Box<CompExpression>),
    Call(CompVariable, Vec<CompExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Action {
    Block(Vec<Action>),
    IfElse {
        cond: Box<Action>,
        then: Box<Program>,
        otherwise: Option<Box<Program>>,
    },
    WhileLoop {
        cond: Box<Action>,
        body: Box<Program>,
    },
    Assignment {
        name: String,
        value: Box<Action>,
    },
    Expression(CompExpression),
}

pub fn transform_type(
    ty: &CustomType,
    scope: &CompScope,
    types: &HashMap<String, CompType>,
) -> CompType {
    match ty {
        CustomType::Union(sub_types) => CompType::Union(
            sub_types
                .iter()
                .map(|x| types.get(x).cloned().unwrap_or_else(|| scope.get_type(x)))
                .collect(),
        ),
        CustomType::Callible(args, ret) => {
            let args = args
                .iter()
                .map(|x| transform_type(x, scope, types))
                .collect::<Vec<_>>();
            let ret = transform_type(ret, scope, types);
            CompType::Callible(args, Box::new(ret))
        }
    }
}

fn bin_exp(op: Op, left: &Expression, right: &Expression, scope: &CompScope) -> CompExpression {
    let left = transform_exp(&left, scope);
    let right = transform_exp(&right, scope);
    CompExpression::BinOp(op, Box::new(left), Box::new(right))
}

fn transform_exp(exp: &Expression, scope: &CompScope) -> CompExpression {
    match exp {
        Expression::LessThan(l, r) => bin_exp(Op::Le, l, r, scope),
        Expression::Addition(l, r) => bin_exp(Op::Add, l, r, scope),
        Expression::Multiplication(l, r) => bin_exp(Op::Mult, l, r, scope),
        Expression::Subtraction(l, r) => bin_exp(Op::Sub, l, r, scope),
        Expression::Division(l, r) => bin_exp(Op::Div, l, r, scope),
        Expression::Equal(l, r) => bin_exp(Op::Eq, l, r, scope),
        Expression::FuncCall(name, args) => CompExpression::Call(
            scope.get_variable(name),
            args.iter().map(|x| transform_exp(x, scope)).collect(),
        ),
        Expression::Terminal(sym) => match sym {
            Symbol::Identifier(name) => CompExpression::Read(scope.get_variable(name)),
            Symbol::Data(data) => CompExpression::Value(match data.clone() {
                RawData::Int(val) => CompData::Int(val),
                RawData::Str(val) => CompData::Str(val.clone()),
                RawData::Bool(val) => CompData::Bool(val),
                RawData::Null => CompData::Null,
                RawData::ActiveFunc(_) => panic!("ActiveFunc should not be used"),
                RawData::Func(func) => {
                    let temp_variables = func.args.iter().map(|x| CompVariable {
                        constant: true,
                        name: x.0.clone(),
                        typing: transform_type(
                            &CustomType::Union(x.1.clone()),
                            scope,
                            &HashMap::new(),
                        ),
                    });
                    let arguments = temp_variables.clone().map(|x| x.typing).collect::<Vec<_>>();
                    let mut local_variables = HashMap::new();
                    for var in temp_variables {
                        local_variables.insert(var.name.clone(), var);
                    }
                    let return_type = transform_type(
                        &CustomType::Union(func.return_type).clone(),
                        scope,
                        &HashMap::new(),
                    );
                    let local_scope =
                        resolve_scope(&func.body, scope, &mut local_variables, &mut HashMap::new());
                    let body = transform_ast(func.body, &local_scope);
                    CompData::Func(FunctionAst {
                        arguments,
                        return_type,
                        body: Some(Box::new(body)),
                    })
                }
            }),
        },
    }
}

fn resolve_scope(
    ast: &Vec<Instr>,
    scope: &CompScope,
    variables: &mut HashMap<String, CompVariable>,
    types: &mut HashMap<String, CompType>,
) -> CompScope {
    for stat in ast {
        match stat.clone() {
            Instr::TypeDeclaration(name, declared_type) => {
                if !types.contains_key(&name) {
                    types.insert(name, transform_type(&declared_type, scope, types));
                } else {
                    panic!("Type '{}' is already defined", name)
                }
            }
            Instr::InitAssign(constant, name, declared_type, exp) => {
                if variables.values().any(|x| x.name == name) {
                    panic!("Cannot re-declare variable in the same scope '{}'", name)
                }
                variables.insert(
                    name.clone(),
                    CompVariable {
                        constant,
                        name,
                        typing: transform_type(
                            &CustomType::Union(declared_type.unwrap_or(Vec::new())),
                            scope,
                            &HashMap::new(),
                        ),
                    },
                );
            }
            Instr::Assign(name, _) => {
                if scope.is_global() {
                    panic!("Cannot reassign after declaration in the global scope")
                }

                if !scope.variable_exists(&name) || !variables.iter().any(|x| x.0 == &name) {
                    panic!("Attempted to assign to undeclared variable '{}'", name)
                }
                if scope.constant_exists(&name) || variables.values().any(|x| x.constant) {
                    panic!("Attempted to reassign to constant variable '{}'", name)
                }
            }
            x => {}
        };
    }
    CompScope {
        variables: variables.clone(),
        types: types.clone(),
        parent: Some(Box::new(scope.clone())),
    }
}

pub fn transform_ast(ast: Vec<Instr>, scope: &CompScope) -> Program {
    let mut block = Vec::new();
    for stat in ast {
        block.push(match stat {
            Instr::TypeDeclaration(_, _) => Action::Block(Vec::new()),
            Instr::InitAssign(_, name, _, exp) | Instr::Assign(name, exp) => {
                let exp = Action::Expression(transform_exp(&exp, scope));
                Action::Assignment {
                    name,
                    value: Box::new(exp),
                }
            }
            Instr::Loop(exp, body) => {
                let cond = transform_exp(&exp, scope);
                let body = create_program(&body, scope);
                Action::WhileLoop {
                    cond: Box::new(Action::Expression(cond)),
                    body: Box::new(body),
                }
            }
            Instr::IfElse(cond, left, right) => {
                let cond = transform_exp(&cond, scope);
                let then = create_program(&left, scope);
                let alt = create_program(&right, scope);
                Action::IfElse {
                    cond: Box::new(Action::Expression(cond)),
                    then: Box::new(then),
                    otherwise: Some(Box::new(alt)),
                }
            }
            Instr::LoneExpression(exp) => Action::Expression(transform_exp(&exp, scope)),
            Instr::Invalid(x) => panic!("invalid '{}'", x),
            x => panic!("not implimented for '''"),
        });
    }
    Program {
        scope: scope.clone(),
        body: Action::Block(block),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompData {
    Int(i32),
    Str(String),
    Bool(bool),
    Null,
    Func(FunctionAst),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompSymbol {
    Data(CompData),
    Identifier(String),
}

pub fn create_program(ast: &Vec<Instr>, scope: &CompScope) -> Program {
    let local_scope = resolve_scope(ast, scope, &mut HashMap::new(), &mut HashMap::new());
    transform_ast(ast.clone(), &scope)
}

pub fn flatten_action(prog: Action) -> Action {
    prog
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompType {
    Callible(Vec<Self>, Box<Self>),
    Union(Vec<Self>),
    Int,
    Bool,
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    scope: CompScope,
    body: Action,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompScope {
    types: HashMap<String, CompType>,
    variables: HashMap<String, CompVariable>,
    parent: Option<Box<CompScope>>,
}
impl CompScope {
    fn get_variable(&self, name: &String) -> CompVariable {
        if let Some(var) = self.variables.get(name) {
            var.clone()
        } else {
            match &self.parent {
                Some(parent) => (*parent).get_variable(name),
                None => panic!("Cannot find variable '{}'", name),
            }
        }
    }

    fn get_type(&self, name: &String) -> CompType {
        if let Some(ty) = self.types.get(name) {
            ty.clone()
        } else {
            match &self.parent {
                Some(parent) => (*parent).get_type(name),
                None => panic!("Cannot find variable '{}'", name),
            }
        }
    }

    fn variable_exists(&self, name: &String) -> bool {
        if self.variables.contains_key(name) {
            true
        } else {
            match &self.parent {
                Some(parent) => (*parent).variable_exists(name),
                None => false,
            }
        }
    }

    fn constant_exists(&self, name: &String) -> bool {
        if let Some(var) = self.variables.get(name) {
            var.constant
        } else {
            match &self.parent {
                Some(parent) => (*parent).constant_exists(name),
                None => false,
            }
        }
    }

    fn make_child(&self) -> CompScope {
        CompScope {
            variables: HashMap::new(),
            types: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }
    pub fn is_global(&self) -> bool {
        self.parent.is_none()
    }
}
