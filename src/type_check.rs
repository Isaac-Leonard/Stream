#[path = "shared.rs"]
pub mod shared;

use crate::shared::*;

pub fn create_type_error(
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

pub fn get_exp_type(
    exp: &Expression,
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Result<LangType, Vec<String>> {
    use Expression::*;
    use LangType::*;
    match exp {
        Terminal(sym) => match sym {
            Symbol::Data(RawData::Func(Function {
                args,
                body,
                return_type,
            }))
            | Symbol::Data(RawData::ActiveFunc(ActiveFunction {
                args,
                body,
                stack: _,
                call: _,
                return_type,
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
                let arg_types = arg_types.unwrap();
                let mut variables_with_args = variables.clone();
                variables_with_args.append(&mut arg_types.clone());
                let ret = consolidate_type(return_type, types);
                if ret.is_err() {
                    ret
                } else {
                    let final_errors = type_check(body, &mut variables_with_args, types, false);
                    if final_errors.len() > 0 {
                        Err(final_errors)
                    } else {
                        // Replace this when we implement defined return types on functions
                        Ok(LangType::Func(
                            arg_types.iter().map(|x| x.shape.clone()).collect(),
                            Box::new(ret.unwrap()),
                        ))
                    }
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
                (Str, Str) | (Str, Int) | (Int, Str) | (Null, Null) => Ok(Str),
                (Union(x), Int) | (Union(x), Str) | (Int, Union(x)) | (Str, Union(x)) => {
                    Ok(Union(x))
                }
                // Let the rest go to the error manager
                (x, y) => create_type_error("+", (Ok(x), Ok(y))),
            },
            invalid => create_type_error("+", invalid),
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
                    if !types_match(&passed_type.clone().unwrap(), &x.1) {Some(vec![format!("Attempted to pass type {:?} as argument to {} but {} expected {:?} at that position", passed_type, name,name, x.1)])}else{None}
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

pub fn match_type_against_union(sup: Vec<LangType>, sub: LangType) -> bool {
    sup.iter().find(|x| types_match(x, &sub)).is_some()
}

pub fn types_match(a: &LangType, b: &LangType) -> bool {
    use LangType::*;
    match (a, b) {
        (Int, Int) => true,
        (Bool, Bool) => true,
        (Str, Str) => true,
        (Null, Null) => true,
        (Union(sup), sub) | (sub, Union(sup)) => match_type_against_union(sup.clone(), sub.clone()),
        _ => false,
    }
}

fn create_type(
    custom: &CustomType,
    types: &mut Vec<TypeDescriptor>,
) -> Result<LangType, Vec<String>> {
    use CustomType::*;
    match custom {
        Union(sub_types) => consolidate_type(&sub_types, &types),
        Callible(args, ret) => {
            let arg_types = args
                .iter()
                .map(|x| create_type(x, types))
                .collect::<Result<Vec<LangType>, _>>();
            if let Err(e) = arg_types {
                return Err(e);
            }
            let return_type = create_type(ret, types);
            if let Err(e) = return_type {
                return Err(e);
            }
            return Ok(LangType::Func(
                arg_types.unwrap(),
                Box::new(return_type.unwrap()),
            ));
        }
    }
}

fn type_check_statement(
    stat: &Instr,
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Vec<String> {
    use Instr::*;
    return match stat {
        TypeDeclaration(name, custom) => {
            let new_type = create_type(custom, types);
            if let Err(e) = new_type {
                e
            } else {
                types.push(TypeDescriptor {
                    name: name.clone(),
                    shape: new_type.unwrap(),
                });
                Vec::new()
            }
        }
        Assign(name, exp) => {
            let exp_type = get_exp_type(exp, variables, types, global);
            if let Err(e) = exp_type {
                return e;
            }

            let variable = variables.iter().find(|x| x.name == *name);
            if let Some(var) = variable {
                if !types_match(&exp_type.clone().unwrap(), &var.shape) {
                    return vec![format!(
                        "Cannot assign type '{:?}' to variable of type '{:?}'",
                        exp_type.unwrap(),
                        var.shape
                    )];
                };
                return Vec::new();
            } else {
                return vec![format!("Cannot assign to undeclared variable '{}'", name)];
            }
        }
        InitAssign(mutible, name, var_type, exp) => {
            if variables.iter().find(|x| x.name == *name).is_some() {
                return vec![format!("Attempted to redeclare variable '{}'", name)];
            }
            let exp_type = get_exp_type(exp, variables, types, global);
            if let Err(e) = exp_type {
                return e;
            }
            let exp_type = exp_type.unwrap();
            let final_type: LangType;
            let mut var_type = var_type.as_ref().map(|x| consolidate_type(&x, types));
            if let Some(vtype) = var_type.clone() {
                if let Err(e) = vtype {
                    return e;
                }
                if !types_match(&exp_type, &vtype.clone().unwrap()) {
                    return vec![format!(
                        "Cannot assign type '{:?}' to variable '{}' with type '{:?}'",
                        exp_type,
                        name,
                        var_type.clone().unwrap()
                    )];
                }
                final_type = vtype.clone().unwrap();
            } else {
                final_type = exp_type;
            }
            variables.push(TypeDescriptor {
                name: name.clone(),
                shape: final_type,
            });

            return Vec::new();
        }
        LoneExpression(exp) => match get_exp_type(exp, variables, types, global) {
            Err(e) => return e,
            _ => return Vec::new(),
        },
        IfElse(exp, yes, no) => {
            if global {
                vec!["Cannot use if expressions outside of a function".to_string()]
            } else {
                let check_type = get_exp_type(exp, variables, types, global);
                if let Err(e) = check_type {
                    e
                } else if check_type.unwrap() != LangType::Bool {
                    vec!["The check expression in an if block must be a bool".to_string()]
                } else {
                    let mut errors = type_check(yes, variables, types, global);
                    errors.append(&mut type_check(no, variables, types, false));
                    errors
                }
            }
        }
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

pub fn type_check(
    ast: &Vec<Instr>,
    variables: &mut Vec<TypeDescriptor>,
    types: &mut Vec<TypeDescriptor>,
    global: bool,
) -> Vec<String> {
    let mut errors = Vec::new();
    for sym in ast {
        // typecheck each statement and unwrap the error result automatically so we can push it to the main list, otherwise do nothing
        errors.append(&mut type_check_statement(&sym, variables, types, global))
    }
    return errors;
}
