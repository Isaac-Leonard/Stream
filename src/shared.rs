use std::collections::HashMap;

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};
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
    pub body: Option<Vec<Instr>>,
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
    InitAssign(bool, bool, String, Option<Vec<String>>, Expression),
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

#[derive(Debug, PartialEq, Clone)]
pub struct NewVariable {
    pub name: String,
    pub typing: Option<CompType>,
    pub constant: bool,
    pub initialised: bool,
    pub external: bool,
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
                if let (Str(a), Str(b)) = (a, b) {
                    Ok(Str(*a + *b))
                } else if a == b {
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
                        Str(_) | Bool | Null => Err(self.invalid_comparison_msg(a, b)),
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
    .flatten()
}

fn bin_exp(
    op: Op,
    left: &Expression,
    right: &Expression,
    scope: &TempScope,
) -> Result<CompExpression, Vec<String>> {
    let left = match transform_exp(&left, scope) {
        Ok(exp) => exp,
        Err(msg) => return Err(msg),
    };
    let right = match transform_exp(&right, scope) {
        Ok(exp) => exp,
        Err(msg) => return Err(msg),
    };
    Ok(CompExpression::BinOp(op, Box::new(left), Box::new(right)))
}

fn transform_exp(exp: &Expression, scope: &TempScope) -> Result<CompExpression, Vec<String>> {
    match exp {
        Expression::LessThan(l, r) => bin_exp(Op::Le, l, r, scope),
        Expression::Addition(l, r) => bin_exp(Op::Add, l, r, scope),
        Expression::Multiplication(l, r) => bin_exp(Op::Mult, l, r, scope),
        Expression::Subtraction(l, r) => bin_exp(Op::Sub, l, r, scope),
        Expression::Division(l, r) => bin_exp(Op::Div, l, r, scope),
        Expression::Equal(l, r) => bin_exp(Op::Eq, l, r, scope),
        Expression::FuncCall(name, args) => {
            let args = args
                .iter()
                .map(|x| transform_exp(x, scope))
                .collect::<Result<Vec<CompExpression>, Vec<String>>>();
            let func = match scope.get_variable(name) {
                Ok(var) => var,
                Err(msg) => return Err(vec![msg]),
            };
            match args {
                Ok(args) => Ok(CompExpression::Call(func, args)),
                Err(message) => Err(message),
            }
        }
        Expression::Terminal(sym) => match sym {
            Symbol::Identifier(name) => match scope.get_variable(name) {
                Ok(var) => Ok(CompExpression::Read(var)),
                Err(msg) => Err(vec![msg]),
            },
            Symbol::Data(data) => Ok(CompExpression::Value(match data.clone() {
                RawData::Int(val) => CompData::Int(val),
                RawData::Float(val) => CompData::Float(val),
                RawData::Str(val) => CompData::Str(val.clone()),
                RawData::Bool(val) => CompData::Bool(val),
                RawData::Null => CompData::Null,
                RawData::Func(func) => {
                    let temp_variables = func.args.iter().map(|x| CompVariable {
                        constant: true,
                        name: x.0.clone(),
                        typing: transform_type(
                            &CustomType::Union(x.1.clone()),
                            &scope.to_comp_scope_so_far(),
                            &HashMap::new(),
                        ),
                        external: false,
                    });
                    let arguments = temp_variables.clone().collect::<Vec<_>>();
                    let mut local_variables = HashMap::new();
                    for var in temp_variables {
                        local_variables.insert(var.name.clone(), var);
                    }
                    let return_type = transform_type(
                        &CustomType::Union(func.return_type).clone(),
                        &scope.to_comp_scope_so_far(),
                        &HashMap::new(),
                    );
                    match func.body {
                        Some(body) => {
                            let mut local_scope = resolve_scope(
                                &body,
                                &scope.to_comp_scope_so_far(),
                                &mut local_variables,
                                &mut HashMap::new(),
                            );
                            let body = transform_ast(&body, &mut local_scope);
                            let body = match body {
                                Err(messages) => return Err(messages),
                                Ok(x) => x,
                            };
                            CompData::Func(FunctionAst {
                                arguments,
                                return_type,
                                body: Some(Box::new(body)),
                            })
                        }
                        None => CompData::Func(FunctionAst {
                            arguments,
                            return_type,
                            body: None,
                        }),
                    }
                }
            })),
        },
    }
}

fn resolve_scope(
    ast: &Vec<Instr>,
    scope: &CompScope,
    preset_variables: &mut HashMap<String, CompVariable>,
    types: &mut HashMap<String, CompType>,
) -> TempScope {
    let mut variables: HashMap<String, NewVariable> = HashMap::new();
    for stat in ast {
        match stat.clone() {
            Instr::TypeDeclaration(name, declared_type) => {
                if !types.contains_key(&name) {
                    types.insert(name, transform_type(&declared_type, scope, types));
                } else {
                    panic!("Type '{}' is already defined", name)
                }
            }
            Instr::InitAssign(external, constant, name, declared_type, exp) => {
                if variables.values().any(|x| x.name == name) {
                    panic!("Cannot re-declare variable in the same scope '{}'", name)
                }
                if external && declared_type == None {
                    panic!("External variable '{}' must be declared with a type", name)
                }
                variables.insert(
                    name.clone(),
                    NewVariable {
                        constant,
                        name,
                        typing: declared_type
                            .map(|x| transform_type(&CustomType::Union(x), scope, types)),
                        initialised: false,
                        external,
                    },
                );
            }
            Instr::Assign(name, _) => {
                if scope.is_global() {
                    panic!(
                        "Cannot reassign after declaration in the global scope '{}'",
                        name
                    )
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
    TempScope {
        preset_variables: preset_variables.clone(),
        variables: variables.clone(),
        types: types.clone(),
        parent: Some(Box::new(scope.clone())),
    }
}

pub fn transform_ast(ast: &Vec<Instr>, mut scope: &mut TempScope) -> Result<Program, Vec<String>> {
    let mut expressions: Vec<CompExpression> = Vec::new();
    let mut errors: Vec<String> = Vec::new();
    for stat in ast {
        match stat {
            Instr::TypeDeclaration(_, _) => {}
            Instr::InitAssign(_, _, name, _, exp) => {
                if scope.variable_initialised(name) {
                    errors.push(format!(
                        "Cannot re-initialise already declared variable '{}'",
                        name
                    ));
                } else {
                    let exp = transform_exp(&exp, scope);
                    let exp = match exp {
                        Ok(exp) => exp,
                        Err(msg) => {
                            errors.append(&mut msg.clone());
                            continue;
                        }
                    };
                    if !scope.variable_has_type(name) {
                        let ty = get_type_from_exp(&exp);
                        if let Err(msg) = ty {
                            errors.push(msg);
                            continue;
                        } else if let Ok(ty) = ty {
                            scope = scope.set_variable_type(name, &ty);
                        }
                    }
                    scope.set_variable_initialised(name);
                    let var = scope.get_variable(&name);
                    if let Ok(var) = var {
                        let assign = CompExpression::Assign(var, Box::new(exp));
                        match get_type_from_exp(&assign) {
                            Ok(_) => expressions.push(assign),
                            Err(msg) => errors.push(msg),
                        };
                    } else if let Err(msg) = var {
                        errors.push(msg);
                    }
                }
            }
            Instr::Assign(name, exp) => {
                let exp = transform_exp(&exp, scope);
                let exp = match exp {
                    Ok(exp) => exp,
                    Err(msg) => {
                        errors.append(&mut msg.clone());
                        continue;
                    }
                };
                let var = scope.get_variable(name);
                let assign = match var {
                    Err(msg) => {
                        errors.push(msg);
                        continue;
                    }
                    Ok(var) => CompExpression::Assign(var, Box::new(exp)),
                };
                match get_type_from_exp(&assign) {
                    Ok(exp) => expressions.push(assign),
                    Err(msg) => errors.push(msg),
                };
            }
            Instr::Loop(exp, body) => {
                let cond = transform_exp(&exp, scope);
                let cond = match cond {
                    Ok(cond) => cond,
                    Err(msg) => {
                        errors.append(&mut msg.clone());
                        continue;
                    }
                };
                let body = transform_ast(&body, scope);
                match body {
                    Err(messages) => errors.append(&mut messages.clone()),
                    Ok(body) => expressions.push(CompExpression::WhileLoop {
                        cond: Box::new(cond),
                        body: Box::new(CompExpression::Prog(Box::new(body))),
                    }),
                };
            }
            Instr::IfElse(cond, left, right) => {
                let cond = transform_exp(&cond, scope);
                let cond = match cond {
                    Ok(cond) => cond,
                    Err(msg) => {
                        errors.append(&mut msg.clone());
                        continue;
                    }
                };
                let then = transform_ast(&left, scope);
                let then = match then {
                    Err(messages) => {
                        errors.append(&mut messages.clone());
                        continue;
                    }
                    Ok(x) => x,
                };
                let alt = transform_ast(&right, scope);
                let alt = match alt {
                    Err(messages) => {
                        errors.append(&mut messages.clone());
                        continue;
                    }
                    Ok(x) => x,
                };
                expressions.push(CompExpression::IfElse {
                    cond: Box::new(cond),
                    then: Box::new(CompExpression::Prog(Box::new(then))),
                    otherwise: Box::new(CompExpression::Prog(Box::new(alt))),
                });
            }
            Instr::LoneExpression(exp) => {
                let exp = transform_exp(&exp, scope);
                let exp = match exp {
                    Ok(exp) => expressions.push(exp),
                    Err(msg) => errors.append(&mut msg.clone()),
                };
            }
            Instr::Invalid(x) => {
                errors.push(format!("invalid '{}'", x));
            }
            x => panic!("not implimented for '''"),
        };
    }
    let expressions = expressions;
    let scope = scope.to_comp_scope();
    match scope {
        Ok(scope) => {
            if errors.is_empty() {
                Ok(Program {
                    scope: scope.clone(),
                    body: CompExpression::List(expressions),
                })
            } else {
                Err(errors)
            }
        }
        Err(messages) => {
            errors.append(&mut messages.clone());
            Err(errors)
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
            Bool(_) => CompType::Bool,
            Int(_) => CompType::Int,
            Float(_) => CompType::Float,
            Str(content) => CompType::Str(content.len() as u32),
            Func(ast) => ast.as_type(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum CompSymbol {
    Data(CompData),
    Identifier(String),
}

pub fn create_program(ast: &Vec<Instr>, scope: &CompScope) -> Result<Program, Vec<String>> {
    let mut local_scope = resolve_scope(ast, scope, &mut HashMap::new(), &mut HashMap::new());
    let prog = transform_ast(ast, &mut local_scope);
    if prog.is_err() {
        return prog;
    }
    let prog = prog.unwrap();
    match get_type_from_exp(&prog.body) {
        Err(messages) => Err(vec![messages]),
        _ => Ok(prog),
    }
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
    Str(u32),
    Ptr,
}
impl CompType {
    pub fn get_compiler_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use CompType::*;
        println!("{}", self.get_str());
        match self.clone() {
            Int => context.i32_type().as_basic_type_enum(),
            Float => context.f32_type().as_basic_type_enum(),
            Null => context.custom_width_int_type(1).as_basic_type_enum(),
            Str(len) => context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            Ptr => context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic)
                .as_basic_type_enum(),
            _ => panic!(
                "get_compiler_type not implemented for type '{}'",
                self.get_str()
            ),
        }
    }
    fn super_of(&self, ty: &CompType) -> bool {
        if self == ty {
            true
        } else if self == &CompType::Ptr && ty.is_str() {
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
        match self {
            CompType::Str(_) => true,
            _ => false,
        }
    }
    pub fn is_callable(&self) -> bool {
        match self {
            CompType::Callible(_, _) => true,
            _ => false,
        }
    }
    pub fn get_str(&self) -> String {
        use CompType::*;
        match self {
            Ptr => "Ptr".to_string(),
            Int => "Int".to_string(),
            Null => "Null".to_string(),
            Str(len) => format!("Str<{}>", len),
            Bool => "Bool".to_string(),
            Float => "Float".to_string(),
            Union(types) => types
                .iter()
                .map(CompType::get_str)
                .reduce(|a, b| format!("{} | {}", a, b))
                .unwrap(),
            Callible(args, ret) => format!(
                "({}): {}",
                args.iter()
                    .map(CompType::get_str)
                    .reduce(|a, b| format!("{}, {}", a, b))
                    .unwrap_or_else(String::new),
                ret.get_str()
            ),
        }
    }
    pub fn flatten(&self) -> CompType {
        use CompType::*;
        match self {
            Ptr => Ptr,
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
                    _ => Union(types),
                }
            }
        }
    }
    fn get_discriminant(&self) -> i8 {
        use CompType::*;
        match self {
            Ptr => 8,
            Null => 0,
            Bool => 1,
            Int => 2,
            Float => 3,
            Str(_) => 4,
            Callible(_, _) => 5,
            Union(_) => 7,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub scope: CompScope,
    pub body: CompExpression,
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

pub struct TempScope {
    pub types: HashMap<String, CompType>,
    pub variables: HashMap<String, NewVariable>,
    preset_variables: HashMap<String, CompVariable>,
    pub parent: Option<Box<CompScope>>,
}
impl TempScope {
    fn set_variable_initialised(&mut self, name: &String) {
        self.variables.get_mut(name).map(|v| v.initialised = true);
    }

    fn set_variable_type<'a>(&'a mut self, name: &String, ty: &CompType) -> &'a mut TempScope {
        self.variables
            .get_mut(name)
            .map(|v| v.typing = Some(ty.clone()));
        self
    }

    fn to_comp_scope(&self) -> Result<CompScope, Vec<String>> {
        let mut variables = self
            .variables
            .iter()
            .map(|v| v.1.get_final())
            .chain(self.preset_variables.values().map(|x| Ok(x.clone())));
        if variables.clone().any(|x| x.is_err()) {
            Err(variables.filter_map(|x| x.err()).collect())
        } else {
            Ok(CompScope {
                variables: variables
                    .map(|x| x.map(|x| (x.name.clone(), x)))
                    .collect::<Result<HashMap<_, _>, _>>()
                    .expect("Something went wrong"),
                types: self.types.clone(),
                parent: self.parent.clone(),
            })
        }
    }

    fn to_comp_scope_so_far(&self) -> CompScope {
        let variables = self
            .variables
            .iter()
            .filter(|v| v.1.initialised)
            .map(|v| (v.0.clone(), v.1.get_final().unwrap()))
            .chain(self.preset_variables.clone().into_iter())
            .collect::<HashMap<String, CompVariable>>();

        CompScope {
            variables,
            types: self.types.clone(),
            parent: self.parent.clone(),
        }
    }

    fn get_variable(&self, name: &String) -> Result<CompVariable, String> {
        if let Some(var) = self.variables.get(name) {
            var.get_final()
        } else if let Some(var) = self.preset_variables.get(name) {
            Ok(var.clone())
        } else {
            match &self.parent {
                Some(parent) => Ok((*parent).get_variable(name)),
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

    fn variable_initialised(&self, name: &String) -> bool {
        if let Some(var) = self.variables.get(name) {
            var.initialised
        } else {
            match &self.parent {
                Some(parent) => (*parent).constant_exists(name),
                None => false,
            }
        }
    }
    fn variable_has_type(&self, name: &String) -> bool {
        if let Some(var) = self.variables.get(name) {
            var.typing != None
        } else {
            match &self.parent {
                Some(parent) => (*parent).constant_exists(name),
                None => false,
            }
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
                let ty = ty.flatten();
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
            println!("{}", var.typing.get_str());
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
