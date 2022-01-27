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
    pub name: String,
    pub typing: CompType,
    pub constant: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionAst {
    arguments: Vec<CompType>,
    return_type: CompType,
    body: Option<Box<Program>>,
}
impl FunctionAst {
    pub fn as_type(&self) -> CompType {
        CompType::Callible(self.arguments.clone(), Box::new(self.return_type.clone()))
    }
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
    fn resulting_type(&self, a: &CompType, b: &CompType) -> Result<CompType, String> {
        use CompType::*;
        use Op::*;
        match self {
            Eq => Ok(Bool),
            Le => match (a, b) {
                (Float, Float) | (Int, Int) => Ok(Bool),
                _ => Err(self.invalid_comparison_msg(a, b)),
            },
            Add => {
                if a == b {
                    match a {
                        Bool | Null => Err(self.invalid_comparison_msg(a, b)),
                        x => Ok(x.clone()),
                    }
                } else {
                    Err(self.invalid_comparison_msg(a, b))
                }
            }
            Sub | Div | Mult => {
                if a == b {
                    match a {
                        Str | Bool | Null => Err(self.invalid_comparison_msg(a, b)),
                        x => Ok(x.clone()),
                    }
                } else {
                    Err(self.invalid_comparison_msg(a, b))
                }
            }
        }
    }
    fn invalid_comparison_msg(&self, a: &CompType, b: &CompType) -> String {
        format!(
            "Invalid operation '{}' for types '{}' and '{}' ",
            self.get_str(),
            a.get_str(),
            b.get_str()
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
    Assign(CompVariable, Box<CompExpression>),
    IfOnly {
        cond: Box<CompExpression>,
        then: Box<CompExpression>,
    },
    IfElse {
        cond: Box<CompExpression>,
        then: Box<CompExpression>,
        otherwise: Box<CompExpression>,
    },
    WhileLoop {
        cond: Box<CompExpression>,
        body: Box<CompExpression>,
    },
    List(Vec<CompExpression>),
    Prog(Box<Program>),
}
impl CompExpression {}

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
                    let body = transform_ast(&func.body, &local_scope);
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
                println!("{}", name);
                if variables.values().any(|x| x.name == name) {
                    panic!("Cannot re-declare variable in the same scope '{}'", name)
                }
                println!("{}", name);
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

                if !scope.variable_exists(&name) && !variables.contains_key(&name) {
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

pub fn transform_ast(ast: &Vec<Instr>, scope: &CompScope) -> Program {
    let mut expressions = Vec::new();
    for stat in ast {
        expressions.push(match stat {
            Instr::TypeDeclaration(_, _) => CompExpression::List(Vec::new()),
            Instr::InitAssign(_, name, _, exp) | Instr::Assign(name, exp) => {
                let exp = transform_exp(&exp, scope);
                CompExpression::Assign(scope.get_variable(&name), Box::new(exp))
            }
            Instr::Loop(exp, body) => {
                let cond = transform_exp(&exp, scope);
                let body = create_program(&body, scope);
                CompExpression::WhileLoop {
                    cond: Box::new(cond),
                    body: Box::new(CompExpression::Prog(Box::new(body))),
                }
            }
            Instr::IfElse(cond, left, right) => {
                let cond = transform_exp(&cond, scope);
                let then = create_program(&left, scope);
                let alt = create_program(&right, scope);
                CompExpression::IfElse {
                    cond: Box::new(cond),
                    then: Box::new(CompExpression::Prog(Box::new(then))),
                    otherwise: Box::new(CompExpression::Prog(Box::new(alt))),
                }
            }
            Instr::LoneExpression(exp) => transform_exp(&exp, scope),
            Instr::Invalid(x) => panic!("invalid '{}'", x),
            x => panic!("not implimented for '''"),
        });
    }
    Program {
        scope: scope.clone(),
        body: CompExpression::List(expressions),
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
            Bool(_) => CompType::Bool,
            Int(_) => CompType::Int,
            Float(_) => CompType::Float,
            Str(_) => CompType::Str,
            Func(ast) => ast.as_type(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum CompSymbol {
    Data(CompData),
    Identifier(String),
}

pub fn create_program(ast: &Vec<Instr>, scope: &CompScope) -> Program {
    let local_scope = resolve_scope(ast, scope, &mut HashMap::new(), &mut HashMap::new());
    transform_ast(ast, &local_scope)
}

pub fn flatten_action(action: CompExpression) -> Option<CompExpression> {
    match action {
        CompExpression::Prog(prog) => {
            if prog.scope.variables.is_empty() && prog.scope.types.is_empty() {
                flatten_action(prog.body)
            } else {
                Some(CompExpression::Prog(prog))
            }
        }
        CompExpression::List(expressions) => {
            if expressions.is_empty() {
                None
            } else {
                Some(CompExpression::List(expressions))
            }
        }
        CompExpression::IfElse {
            cond,
            then,
            otherwise,
        } => Some(match flatten_action(*otherwise) {
            Some(otherwise) => CompExpression::IfElse {
                cond,
                then,
                otherwise: Box::new(otherwise),
            },
            None => CompExpression::IfOnly { cond, then },
        }),
        x => Some(x),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompType {
    Callible(Vec<Self>, Box<Self>),
    Union(Vec<Self>),
    Null,
    Bool,
    Int,
    Float,
    Str,
}
impl CompType {
    fn super_of(&self, ty: &CompType) -> bool {
        if self == ty {
            true
        } else {
            false
        }
    }
    fn is_bool(&self) -> bool {
        self == &CompType::Bool
    }
    fn is_null(&self) -> bool {
        self == &CompType::Null
    }
    fn is_str(&self) -> bool {
        self == &CompType::Str
    }
    pub fn is_callable(&self) -> bool {
        match self {
            CompType::Callible(_, _) => true,
            _ => false,
        }
    }
    fn get_str(&self) -> String {
        use CompType::*;
        match self {
            Int => "Int".to_string(),
            Null => "Null".to_string(),
            Str => "Str".to_string(),
            Bool => "Bool".to_string(),
            Float => "Float".to_string(),
            Union(types) => types
                .iter()
                .map(CompType::get_str)
                .fold(String::new(), |a, b| a + &" | " + b.as_str())
                .clone(),
            Callible(args, ret) => {
                args.iter()
                    .map(CompType::get_str)
                    .fold(String::new(), |a, b| a + &", " + b.as_str())
                    + &": ".to_string()
                    + &ret.get_str()
            }
        }
    }
    fn get_discriminant(&self) -> i8 {
        use CompType::*;
        match self {
            Null => 0,
            Bool => 1,
            Int => 2,
            Float => 3,
            Str => 4,
            Callible(_, _) => 5,
            Union(_) => 7,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    scope: CompScope,
    body: CompExpression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompScope {
    pub types: HashMap<String, CompType>,
    pub variables: HashMap<String, CompVariable>,
    pub parent: Option<Box<CompScope>>,
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
                None => panic!("Cannot find type '{}'", name),
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

fn get_type_from_exp(exp: &CompExpression) -> Result<CompType, String> {
    use CompExpression::*;
    match exp {
        Prog(prog) => get_type_from_exp(&prog.body),
        List(exps) => {
            let mut types = exps.iter().map(|x| get_type_from_exp(x));
            let err = types.find(|x| x.is_err());
            if let Some(err) = err {
                err
            } else {
                types.last().unwrap_or(Ok(CompType::Null))
            }
        }
        WhileLoop { cond, body } => {
            let cond = get_type_from_exp(cond);
            if let Ok(cond) = cond {
                if cond.is_bool() {
                    get_type_from_exp(body)
                } else {
                    Err("The comparison expression in a while loop must return a Bool".to_string())
                }
            } else {
                cond
            }
        }
        IfElse {
            cond,
            then,
            otherwise,
        } => {
            let cond = get_type_from_exp(cond);
            if let Ok(cond) = cond {
                if cond.is_bool() {
                    let then_ty = get_type_from_exp(then);
                    let other_ty = get_type_from_exp(otherwise);
                    if then_ty.is_err() {
                        then_ty
                    } else if other_ty.is_err() {
                        other_ty
                    } else {
                        Ok(CompType::Union(vec![then_ty.unwrap(), other_ty.unwrap()]))
                    }
                } else {
                    Err(
                        "The comparison expression in an if expression must return a Bool"
                            .to_string(),
                    )
                }
            } else {
                cond
            }
        }
        IfOnly { cond, then } => {
            let cond = get_type_from_exp(cond);
            if let Ok(cond) = cond {
                if cond.is_bool() {
                    let then_ty = get_type_from_exp(then);
                    if then_ty.is_err() {
                        then_ty
                    } else {
                        Ok(CompType::Null)
                    }
                } else {
                    Err(
                        "The comparison expression in an if expression must return a Bool"
                            .to_string(),
                    )
                }
            } else {
                cond
            }
        }
        Value(data) => Ok(data.get_type()),
        Assign(var, exp) => {
            let exp = get_type_from_exp(exp);
            if let Ok(ty) = exp {
                if var.typing.super_of(&ty) {
                    Ok(var.typing.clone())
                } else {
                    Err(format!(
                        "Type '{}' is not assignable to type '{}'",
                        ty.get_str(),
                        var.typing.get_str()
                    ))
                }
            } else {
                exp
            }
        }
        BinOp(op, a, b) => {
            let a = get_type_from_exp(a);
            let b = get_type_from_exp(b);
            if let Ok(a) = a {
                if let Ok(b) = b {
                    op.resulting_type(&a, &b)
                } else {
                    b
                }
            } else {
                a
            }
        }
        OneOp(_, val) => get_type_from_exp(val),
        Read(var) => Ok(var.typing.clone()),
        Call(var, args) => {
            let var = var.clone();
            let mut arg_types = args.iter().map(|x| get_type_from_exp(x));
            if let Some(msg) = arg_types.find(|x| x.is_err()) {
                return msg;
            }
            let arg_types = arg_types.map(|x| x.unwrap());
            if let CompType::Callible(args, ret) = var.typing {
                let mismatched_args = args
                    .iter()
                    .zip(arg_types)
                    .map(|(x, y)| {
                        if !x.super_of(&y) {
                            Some(format!(
                                "Type '{}' is not assignable to type '{}'",
                                y.get_str(),
                                x.get_str()
                            ))
                        } else {
                            None
                        }
                    })
                    .find(|x| x.is_some())
                    .flatten();
                if let Some(err) = mismatched_args {
                    Err(err)
                } else {
                    Ok(*ret.clone())
                }
            } else {
                Err(format!(
                    "Cannot call variable of type '{}'",
                    var.typing.get_str()
                ))
            }
        }
    }
}
