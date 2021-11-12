use chumsky::{error::Cheap, prelude::*, recursive::Recursive, text::ident};
use std::{
    cell::RefCell,
    env, fs,
    io::{self, BufRead},
    rc::Rc,
};

#[derive(Clone, Debug)]
enum Symbol {
    Data(RawData),
    Identifier(String),
}

#[derive(Clone, Debug)]
enum Expression {
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    Terminal(Symbol),
    FuncCall(String, Vec<Expression>),
}

#[derive(Clone, Debug)]
enum Instr {
    Invalid(String),
    Assign(String, Expression),
    LoneExpression(Expression),
    Loop(Expression, Vec<Self>),
}

fn parse_to_i32(x: String) -> i32 {
    return x.parse::<i32>().unwrap();
}

fn integer() -> impl Parser<char, i32, Error = Cheap<char>> {
    filter::<_, _, Cheap<char>>(char::is_ascii_digit)
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(parse_to_i32)
}

fn string() -> impl Parser<char, String, Error = Cheap<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
}

fn symbol_parser() -> impl Parser<char, Symbol, Error = Cheap<char>> {
    ident()
        .map(String::from)
        .map(Symbol::Identifier)
        .or(string().map(RawData::Str).map(Symbol::Data))
        .or(integer().map(RawData::Int).map(Symbol::Data))
}
#[derive(Clone, Debug)]
enum LangType {
    Bool,
    Int,
    Str,
    Func(Vec<LangType>, Box<LangType>),
    Union(Vec<LangType>),
    Null,
}
impl LangType {
    fn flatten(&self) -> LangType {
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

#[derive(Clone, Debug)]
struct TypeDescriptor {
    name: String,
    shape: LangType,
}

fn consolidate_type(
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

fn type_specifyer() -> impl Parser<char, Vec<String>, Error = Cheap<char>> {
    just(':')
        .ignore_then(
            ident().padded().map(String::from).chain::<String, _, _>(
                just('|')
                    .ignore_then(ident().padded().map(String::from))
                    .repeated(),
            ),
        )
        .map(|x| x)
}

#[derive(Debug)]
struct Scope {
    variables: Vec<Variable>,
    parent: Option<Rc<RefCell<Scope>>>,
}
#[derive(Clone, Debug)]
struct ScopeRef(Rc<RefCell<Scope>>);
impl ScopeRef {
    fn new(variables: Vec<Variable>, parent: ScopeRef) -> ScopeRef {
        ScopeRef(Rc::new(RefCell::new(Scope {
            parent: Some(parent.0),
            variables,
        })))
    }

    fn get_copy(&self) -> ScopeRef {
        ScopeRef(Rc::clone(&self.0))
    }

    fn get_index_local(&self, name: &String) -> Option<usize> {
        Rc::clone(&self.0)
            .as_ref()
            .borrow()
            .variables
            .iter()
            .position(|x| x.name == *name)
    }

    fn get_variable_location(&self, name: &String) -> Option<(ScopeRef, usize)> {
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

    fn get_variable(&self, name: &String) -> Option<Variable> {
        let location = self.get_variable_location(name);
        match location {
            Some((scope, index)) => Some(scope.0.as_ref().borrow().variables[index].clone()),

            None => None,
        }
    }
}

fn set_variable(
    stack: ScopeRef,
    name: &String,
    exp: &Expression,
    types: &Vec<TypeDescriptor>,
) -> RawData {
    let value = &eval_exp(exp, ScopeRef(Rc::clone(&stack.0)), &types);
    let data_type = &get_type(value, types).unwrap();
    let location = stack.get_copy().get_variable_location(name);
    match location {
        Some((scope, index)) => {
            let variables = &mut scope.0.as_ref().borrow_mut().variables;
            if types_match(&variables[index].data_type, data_type) {
                variables[index.clone()].value = value.clone();
            } else {
                panic!(
                    "Attempted to assign value of type {:?} to variable of type {:?}",
                    data_type,
                    variables[index.clone()].data_type
                )
            }
        }
        None => {
            stack.0.as_ref().borrow_mut().variables.push(Variable {
                name: name.clone(),
                value: value.clone(),
                data_type: data_type.clone(),
            });
        }
    }

    return value.clone();
}

fn exp_parser<'a>(
    main_parser: Recursive<'a, char, Vec<Instr>, Cheap<char>>,
) -> impl Parser<char, Expression, Error = Cheap<char>> + 'a {
    use Expression::*;
    recursive(|exp| {
        let func_declaration = ident()
            .padded()
            .map(String::from)
            .then(type_specifyer())
            .chain(
                just(',')
                    .ignore_then(ident().padded().map(String::from).then(type_specifyer()))
                    .repeated(),
            )
            .or_not()
            .flatten()
            .delimited_by('(', ')')
            .then_ignore(seq(['=', '>']))
            .then(
                main_parser
                    .clone()
                    .delimited_by('{', '}')
                    .or(exp.clone().map(Instr::LoneExpression).map(|x| vec![x])),
            )
            .map(|(args, body)| {
                Symbol::Data(RawData::Func(Function {
                    args,
                    body,
                    call: |args, params, body, stack, itypes| {
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
                        let res = execute(&body, local_stack, itypes, false);
                        return res;
                    },
                }))
            });

        let func_call = ident()
            .padded()
            .map(String::from)
            .then(
                exp.clone()
                    .chain(just(',').ignore_then(exp.clone()).repeated())
                    .or_not()
                    .flatten()
                    .delimited_by('(', ')'),
            )
            .map(|(name, args)| FuncCall(name, args));

        let primary_exp = func_call
            .or(symbol_parser().map(Expression::Terminal))
            .or(func_declaration.map(Expression::Terminal))
            .or(exp.delimited_by('(', ')'))
            .boxed();

        let multiply_parser = primary_exp
            .clone()
            .then(one_of(['*', '/']).then(primary_exp.clone()).repeated())
            .map(|(l, t)| {
                t.iter().fold(l, |left, (op, right)| match op {
                    '*' => Expression::Multiplication(Box::new(left), Box::new(right.clone())),
                    '/' => Expression::Division(Box::new(left.clone()), Box::new(right.clone())),
                    _ => panic!("Unexpected operator {}", op),
                })
            })
            .boxed();
        let comparison_parser = multiply_parser
            .clone()
            .then_ignore(just('<'))
            .then(multiply_parser.clone())
            .map(|x| Expression::LessThan(Box::new(x.0), Box::new(x.1)));
        let equal_parser = comparison_parser
            .clone()
            .or(multiply_parser.clone())
            .then_ignore(seq(['=', '=']))
            .then(comparison_parser.clone().or(multiply_parser.clone()))
            .map(|x| Expression::Equal(Box::new(x.0), Box::new(x.1)));
        comparison_parser
            .or(equal_parser)
            .or(multiply_parser
                .clone()
                .then(one_of(['+', '-']).then(multiply_parser).repeated())
                .map(|x| {
                    x.1.iter().fold(x.0, |left, right| match right.0 {
                        '+' => Expression::Addition(Box::new(left), Box::new(right.1.clone())),
                        '-' => Expression::Subtraction(Box::new(left), Box::new(right.1.clone())),
                        _ => panic!("Error: Unexpected operator {}", right.0),
                    })
                }))
            .padded()
    })
}

fn parser() -> impl Parser<char, Vec<Instr>, Error = Cheap<char>> {
    use Instr::*;
    recursive(|bf: Recursive<char, Vec<Instr>, _>| {
        let exp = exp_parser(bf.clone()).boxed();
        ident()
            .map(String::from)
            .then(type_specifyer().or_not())
            .then_ignore(just('='))
            .then(exp.clone())
            .map(|x| Assign(x.0 .0, x.1))
            .or(seq("while".chars())
                .ignore_then(exp.clone())
                .then(bf.clone().delimited_by('{', '}'))
                .map(|x| Loop(x.0, x.1)))
            .or(exp.map(LoneExpression))
            .recover_with(nested_delimiters('{', '}', [], |_| {
                Invalid("Syntax error".to_string())
            }))
            .recover_with(skip_then_retry_until(['}']))
            .padded()
            .repeated()
    })
    .then_ignore(end())
}

#[derive(Clone, Debug)]
enum RawData {
    Str(String),
    Bool(bool),
    Int(i32),
    Func(Function),
    ActiveFunc(ActiveFunction),
    Null,
}

#[derive(Clone)]
struct Function {
    args: Vec<(String, Vec<String>)>,
    call: fn(
        args: Vec<(String, Vec<String>)>,
        params: Vec<RawData>,
        body: &Vec<Instr>,
        stack: ScopeRef,
        types: &Vec<TypeDescriptor>,
    ) -> RawData,
    body: Vec<Instr>,
}
impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function({:?})", self.args)
    }
}

#[derive(Clone)]
struct ActiveFunction {
    args: Vec<(String, Vec<String>)>,
    call: fn(
        args: Vec<(String, Vec<String>)>,
        params: Vec<RawData>,
        body: &Vec<Instr>,
        stack: ScopeRef,
        types: &Vec<TypeDescriptor>,
    ) -> RawData,
    body: Vec<Instr>,
    stack: ScopeRef,
}

impl ActiveFunction {
    fn from_raw(func: Function, stack: ScopeRef) -> ActiveFunction {
        ActiveFunction {
            args: func.args,
            call: func.call,
            body: func.body,
            stack,
        }
    }
    fn execute(&self, params: Vec<RawData>, types: &Vec<TypeDescriptor>) -> RawData {
        (self.call)(
            self.args.clone(),
            params,
            &self.body,
            self.stack.get_copy(),
            types,
        )
    }
}
impl std::fmt::Debug for ActiveFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Function({:?})", self.args)
    }
}

#[derive(Clone, Debug)]
struct Variable {
    name: String,
    data_type: LangType,
    value: RawData,
}

fn eval_symbal(sym: &Symbol, variables: ScopeRef) -> RawData {
    use Symbol::*;
    match sym {
        Data(x) => match x {
            RawData::Func(func) => {
                RawData::ActiveFunc(ActiveFunction::from_raw(func.clone(), variables.get_copy()))
            }
            _ => x.clone(),
        },
        Identifier(name) => variables
            .get_variable(name)
            .unwrap_or_else(|| panic!("Attempted to access undeclared variable '{}'", name))
            .value
            .clone(),
    }
}

fn int_or_panic(val: RawData) -> i32 {
    if let RawData::Int(x) = val {
        x
    } else {
        panic!("Expected Int but found {:?}", val);
    }
}

fn bool_or_panic(val: RawData) -> bool {
    if let RawData::Bool(x) = val {
        x
    } else {
        panic!("Expected Bool but found {:?}", val);
    }
}

fn eval_exp(exp: &Expression, variables: ScopeRef, types: &Vec<TypeDescriptor>) -> RawData {
    use Expression::*;
    match exp {
        Terminal(sym) => eval_symbal(sym, variables),
        Equal(left, right) => RawData::Bool(
            match (
                eval_exp(left, variables.get_copy(), types),
                eval_exp(right, variables, types),
            ) {
                (RawData::Bool(x), RawData::Bool(y)) => x == y,
                (RawData::Int(x), RawData::Int(y)) => x == y,
                _ => panic!(
                    "Checking equality of {:?} and {:?} is not implemented",
                    left, right
                ),
            },
        ),
        LessThan(left, right) => RawData::Bool(
            int_or_panic(eval_exp(left, variables.get_copy(), types))
                < int_or_panic(eval_exp(right, variables.get_copy(), types)),
        ),
        Addition(left, right) => match (
            eval_exp(left, variables.get_copy(), types),
            eval_exp(right, variables.get_copy(), types),
        ) {
            (RawData::Int(x), RawData::Int(y)) => RawData::Int(x + y),
            (RawData::Str(x), RawData::Str(y)) => RawData::Str(format!("{}{}", x, y)),
            (RawData::Int(x), RawData::Str(y)) => RawData::Str(format!("{}{}", x, y)),
            (RawData::Str(x), RawData::Int(y)) => RawData::Str(format!("{}{}", x, y)),
            (x, y) => panic!("Addition operator '+' not supported for {:?}, {:?}", x, y),
        },
        Subtraction(left, right) => RawData::Int(
            int_or_panic(eval_exp(left, variables.get_copy(), types))
                - int_or_panic(eval_exp(right, variables, types)),
        ),
        Multiplication(left, right) => RawData::Int(
            int_or_panic(eval_exp(left, variables.get_copy(), types))
                * int_or_panic(eval_exp(right, variables, types)),
        ),
        Division(left, right) => RawData::Int(
            int_or_panic(eval_exp(left, variables.get_copy(), types))
                / int_or_panic(eval_exp(right, variables, types)),
        ),
        FuncCall(name, args) => {
            if let RawData::ActiveFunc(y) = variables.get_variable(name).unwrap().value.clone() {
                y.execute(
                    args.iter()
                        .map(|exp| eval_exp(exp, variables.get_copy(), types))
                        .collect::<Vec<_>>(),
                    types,
                )
            } else {
                panic!(
                    "Attempted to call function on variable  with value '{:?}'",
                    variables.get_variable(name).unwrap().value.clone()
                );
            }
        }
    }
}

fn create_type_error(
    op_name: &str,
    touple: (Result<LangType, Vec<String>>, Result<LangType, Vec<String>>),
) -> Result<LangType, Vec<String>> {
    // Check if both branches failed, if so merge the errors and return them.
    // Otherwise check each branch for errors and return it
    // Otherwise the successful types don't match at this point so create a new error and return it
    match touple {
        // TODO: Maybe change these to linkedlists for less copying and mutibility
        (Err(mut l), Err(mut r)) => {
            l.append(&mut r);
            Err(l)
        }
        // TODO: I feel like this should be optemisible, directly return each branch instead of unwrapping them perhaps?
        (Err(l), _) => Err(l),
        (_, Err(r)) => Err(r),
        (l, r) => Err(vec![format!(
            "The '{}' operator cannot be used on differing types: '{:?} and '{:?}'",
            op_name, l, r
        )]),
    }
}

fn get_exp_type(
    exp: &Expression,
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Result<LangType, Vec<String>> {
    use Expression::*;
    use LangType::*;
    match exp {
        Terminal(sym) => match sym {
            Symbol::Data(RawData::Func(Function { args, body, call }))
            | Symbol::Data(RawData::ActiveFunc(ActiveFunction {
                args,
                body,
                stack: _,
                call,
            })) => {
                let arg_types: Result<Vec<TypeDescriptor>, Vec<String>> = args
                    .iter()
                    .map(|x| {
                        let shape = consolidate_type(&x.1, types).map_err(|x| {
                            vec![format!("Attempted to use undeclared types {:?}", x)]
                        });
                        if let Err(e) = shape {
                            return Err(e);
                        }
                        let shape = shape.unwrap();
                        Ok(TypeDescriptor {
                            name: x.0.clone(),
                            shape,
                        })
                    })
                    .collect();
                if let Err(e) = arg_types {
                    return Err(e);
                };
                let mut arg_types = arg_types.unwrap();
                println!("arg_types: {:?}", arg_types);
                let mut variables_with_args = variables.clone();
                variables_with_args.append(&mut arg_types.clone());
                let final_errors = type_check(body, &mut variables_with_args, types, false);
                if final_errors.len() > 0 {
                    Err(final_errors)
                } else {
                    // Replace this when we implement defined return types on functions
                    Ok(LangType::Func(
                        arg_types.iter().map(|x| x.shape.clone()).collect(),
                        Box::new(LangType::Null),
                    ))
                }
            }
            Symbol::Data(data) => get_type(data, types),
            Symbol::Identifier(name) => variables
                .iter()
                .find(|x| x.name == *name)
                .map(|x| x.shape.clone())
                .ok_or(vec![format!(
                    "Cannot access undeclared variable '{}'",
                    name
                )]),
        },
        Addition(x, y) => match (
            get_exp_type(x, variables, types, global),
            get_exp_type(y, variables, types, global),
        ) {
            (Ok(x), Ok(y)) => match (x, y) {
                (Int, Int) => Ok(Int),
                (Str, Str) | (Str, Int) | (Int, Str) => Ok(Str),
                // Let the rest go to the error manager
                (x, y) => create_type_error("==", (Ok(x), Ok(y))),
            },
            invalid => create_type_error("==", invalid),
        },
        Subtraction(x, y) | Multiplication(x, y) | Division(x, y) => {
            match (
                get_exp_type(x, variables, types, global),
                get_exp_type(y, variables, types, global),
            ) {
                (Ok(LangType::Int), Ok(LangType::Int)) => Ok(LangType::Int),
                // Kinda cheating here with the op_name param
                invalid => create_type_error("+' or '*' or '/", invalid),
            }
        }
        LessThan(x, y) => match (
            get_exp_type(x, variables, types, global),
            get_exp_type(y, variables, types, global),
        ) {
            (Ok(Int), Ok(Int)) => Ok(Bool),
            // More cheeting here
            invalid => create_type_error("<' or '=>' or '>=' or '>", invalid),
        },
        Equal(l, r) => match (
            get_exp_type(l, variables, types, global),
            get_exp_type(r, variables, types, global),
        ) {
            (Ok(x), Ok(y)) => match (x, y) {
                (Int, Int) | (Str, Str) | (Bool, Bool) => Ok(Bool),
                (l, r) => create_type_error("==", (Ok(l), Ok(r))),
            },
            invalid => create_type_error("==", invalid),
        },
        FuncCall(name, args) => {
            let func_type = variables
                .iter()
                .find(|x| x.name == *name)
                .map(|x| x.shape.clone());
            if let None = func_type {
                return Err(vec![format!(
                    "Attempted to access undeclared variable '{}'",
                    name
                )]);
            }
            let func = func_type.unwrap();
            if let Func(expected_args, ret) = func {
                if args.len() != expected_args.len() {
                    return Err(vec![format!(
                        "Function '{}' called with {} args but it expects {}",
                        name,
                        args.len(),
                        expected_args.len(),
                    )]);
                }
                let mismatched_args=                args.iter().zip(expected_args).filter_map(|x| {
                    let passed_type = get_exp_type(x.0, variables, types, global);
		    if let Err(error)=passed_type{return Some(error.clone());}
                    if !types_match(&passed_type.clone().unwrap(), &x.1) {Some(vec![format!("Attempted to pass type {:?} as argument to {} but {} expected {:?} at that possition", passed_type, name,name, x.1)])}else{None}
                }).flatten().map(|x|x.clone()).collect::<Vec<_>>();
                if mismatched_args.len() != 0 {
                    Err(mismatched_args)
                } else {
                    Ok(*ret.clone())
                }
            } else {
                Err(vec![format!(
                    "Attempted to call '{}' as a function but '{}' has type '{:?}'",
                    name, name, func
                )])
            }
        }
    }
}

fn get_type(val: &RawData, types: &Vec<TypeDescriptor>) -> Result<LangType, Vec<String>> {
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

fn types_match(a: &LangType, b: &LangType) -> bool {
    true
}

fn execute(
    ast: &[Instr],
    variables: ScopeRef,
    types: &Vec<TypeDescriptor>,
    global: bool,
) -> RawData {
    use Instr::*;
    let mut last_value = RawData::Null;
    for symbol in ast {
        match symbol {
            Invalid(_) => unreachable!(),
            LoneExpression(exp) => last_value = eval_exp(exp, variables.get_copy(), types),
            Assign(name, exp) => last_value = set_variable(variables.get_copy(), name, exp, types),
            Loop(exp, ast) => {
                while bool_or_panic(eval_exp(exp, variables.get_copy(), types)) {
                    last_value = execute(ast, variables.get_copy(), types, false)
                }
            }
        }
    }
    if global {
        let main_index = variables.get_variable(&"main".to_string()).unwrap();
        if let RawData::ActiveFunc(func) = main_index.clone().value.clone() {
            func.execute(Vec::new(), types);
        } else {
            panic!(
                "Main must be a function, not a {:?}",
                main_index.clone().clone().data_type.clone()
            )
        }
    }
    last_value
}

fn type_check_statement(
    stat: &Instr,
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Vec<String> {
    use Instr::*;
    return match stat {
        Assign(name, exp) => {
            let assigned_type = get_exp_type(exp, variables, types, global);
            println!("Adding '{}'", name.clone());
            if let Ok(shape) = assigned_type {
                variables.push(TypeDescriptor {
                    name: name.clone(),
                    shape: shape.clone(),
                });
            } else if let Err(e) = assigned_type {
                return e;
            };
            return Vec::new();
        }
        LoneExpression(exp) => match get_exp_type(exp, variables, types, global) {
            Err(e) => return e,
            _ => return Vec::new(),
        },
        Loop(check, body) => {
            if global {
                vec!["Cannot use loops outside of a function".to_string()]
            } else {
                let check_type = get_exp_type(check, variables, types, global);
                if let Err(e) = check_type {
                    return e;
                } else if let Ok(LangType::Bool) = check_type {
                    // Dont check for errors just return Err here because if the returned Vec is empty then there'll be no effect but if its not they'll be automatically added
                    return type_check(body, variables, types, global);
                } else {
                    return vec![
			format!(                        "The check expression in a while loop must result in a boolean value, not {:?}"
							 ,check_type.unwrap())
                    ];
                }
            }
        }
        Invalid(msg) => vec![msg.clone()],
    };
}

fn type_check(
    ast: &[Instr],
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Vec<String> {
    println!("");
    variables.iter().for_each(|x| println!("{:?}", x));
    println!("");
    let mut errors = Vec::new();
    for sym in ast {
        // typecheck each statement and unwrap the error result automatically so we can push it to the main list, otherwise do nothing
        errors.append(&mut type_check_statement(&sym, variables, types, global))
    }
    return errors;
}

fn main() {
    let lang_print = ActiveFunction {
        args: vec![("str".into(), vec!["string".into()])],
        body: Vec::new(),
        call: |args, params, _, _, _| {
            if args.len() != params.len() {
                panic!("Function called with invalid parameters {:?}", params)
            }
            println!("{:?}", params[0]);
            return RawData::Null;
        },
        stack: ScopeRef(Rc::new(RefCell::new(Scope {
            variables: Vec::new(),
            parent: None,
        }))),
    };
    let lang_input = ActiveFunction {
        args: Vec::new(),
        body: Vec::new(),
        call: |args, params, _, _, _| {
            if args.len() != params.len() {
                panic!("Function called with invalid parameters {:?}", params)
            }
            return RawData::Str(io::stdin().lock().lines().next().unwrap().unwrap());
        },
        stack: ScopeRef(Rc::new(RefCell::new(Scope {
            variables: Vec::new(),
            parent: None,
        }))),
    };

    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    // let src = "[!]+";
    let variables = ScopeRef(Rc::new(RefCell::new(Scope {
        parent: None,
        variables: vec![
            Variable {
                name: "print".to_string(),
                value: RawData::ActiveFunc(lang_print),
                data_type: LangType::Func(vec![LangType::Str], Box::new(LangType::Null)),
            },
            Variable {
                name: "getInput".to_string(),
                value: RawData::ActiveFunc(lang_input),
                data_type: LangType::Func(Vec::new(), Box::new(LangType::Str)),
            },
        ],
    })));
    let mut types = vec![
        TypeDescriptor {
            name: "int".to_string(),
            shape: LangType::Int,
        },
        TypeDescriptor {
            name: "string".to_string(),
            shape: LangType::Str,
        },
        TypeDescriptor {
            name: "null".to_string(),
            shape: LangType::Null,
        },
    ];

    let parsed = parser().parse(src.trim());
    println!("{:?}", parsed);
    match parsed {
        Ok(ast) => {
            println!("Parsing succeeded");
            let errors = type_check(
                &ast,
                &mut variables
                    .0
                    .as_ref()
                    .borrow()
                    .variables
                    .iter()
                    .map(|x| TypeDescriptor {
                        name: x.name.clone(),
                        shape: x.data_type.clone(),
                    })
                    .collect(),
                &mut types,
                true,
            );
            if errors.len() > 0 {
                errors.iter().for_each(|x| println!("{}", x))
            } else {
                execute(&ast, variables, &types, true);
            }
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}
