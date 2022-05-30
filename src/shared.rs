use crate::ast::*;
use crate::errors::*;
use crate::map_vec;
use crate::settings::Settings;
use std::collections::HashMap;
use std::ops::Range;

fn collect_ok_or_err<T, E>(
    iter: impl IntoIterator<Item = Result<T, E>>,
) -> Option<Result<Vec<T>, Vec<E>>> {
    let mut errors = Vec::new();
    let mut successes = Vec::new();
    for v in iter {
        match v {
            Ok(success) => successes.push(success),
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() && successes.is_empty() {
        None
    } else if errors.is_empty() {
        Some(Ok(successes))
    } else {
        Some(Err(errors))
    }
}

pub fn transform_type(ty: &CustomType, scope: &TempScope) -> Result<CompType, Vec<CompError>> {
    match ty {
        CustomType::Struct(data) => collect_ok_or_err(
            data.iter()
                .map(|x| Ok((x.0.clone(), transform_type(&x.1, scope)?))),
        )
        .unwrap_or_else(|| Ok(Vec::new()))
        .map(CompType::Struct)
        .map_err(|x: Vec<Vec<_>>| x.iter().flatten().cloned().collect()),
        CustomType::Array(el_ty, len) => Ok(CompType::Array(
            Box::new(transform_type(el_ty, scope)?),
            *len as usize,
        )),
        CustomType::Union(sub_types) => {
            collect_ok_or_err(sub_types.iter().map(|x| transform_type(x, scope)))
                .map(|x| x.map_err(|x| x.iter().flatten().cloned().collect()))
                .unwrap_or_else(|| Err(vec![CompError::EmptyType(0..0)]))
                .map(|x| CompType::Union(x).flatten())
        }

        CustomType::Callible(args, ret) => {
            let args = collect_ok_or_err(args.iter().cloned().map(|x| transform_type(&x, scope)))
                .unwrap_or(Ok(Vec::new()));
            let ret = transform_type(&*ret.clone(), scope);
            match (args, ret) {
                (Err(args), Err(ret)) => {
                    Err(args.iter().flatten().chain(ret.iter()).cloned().collect())
                }
                (Err(args), _) => Err(args.iter().flatten().cloned().collect()),
                (_, Err(ret)) => Err(ret),
                (Ok(args), Ok(ret)) => Ok(CompType::Callible(args, Box::new(ret))),
            }
        }
        CustomType::Lone(ty) => {
            let x = scope.get_type(&ty.name);
            if let Ok(x) = x {
                Ok(x)
            } else {
                if ty.name == "Str" && ty.generics.len() == 1 {
                    let len = ty.generics[0].clone();
                    if let CustomType::Constant(len) = len {
                        if let ConstantData::Int(len) = len {
                            Ok(CompType::Str(len as u32))
                        } else {
                            Err(vec![CompError::CannotFindType(ty.clone().name, 0..0)])
                        }
                    } else {
                        Err(vec![CompError::CannotFindType(ty.clone().name, 0..0)])
                    }
                } else {
                    Err(vec![CompError::CannotFindType(ty.clone().name, 0..0)])
                }
            }
        }
        _ => panic!("General constant types are not yet supported sorry"),
    }
}

fn bin_exp(
    op: &Op,
    left: &SpannedExpression,
    right: &SpannedExpression,
    env: &ExpEnvironment,
    scope: &mut TempScope,
    file: &str,
) -> (CompExpression, Vec<CompError>) {
    let left = transform_exp(left, env, scope, file);
    let right = transform_exp(right, env, scope, file);
    (
        CompExpression::BinOp(op.clone(), left.0, right.0),
        left.1.into_iter().chain(right.1).collect(),
    )
}

fn transform_exp(
    (loc, exp): &SpannedExpression,
    env: &ExpEnvironment,
    mut scope: &mut TempScope,
    file: &str,
) -> (ExpEnvironment, Vec<CompError>) {
    let mut errs = Vec::new();
    macro_rules! get_exp {
        ($exp:expr, $env:expr, $scope:expr) => {{
            let mut res = transform_exp($exp, $env, $scope, file);
            errs.append(&mut res.1);
            res.0
        }};
    }
    let expression = match exp {
        Expression::Struct(data) => {
            let res = data
                .iter()
                .map(|(k, v)| (k.clone(), get_exp!(v, env, scope)));
            CompExpression::Struct(res.collect())
        }
        Expression::Array(elements) => {
            let mut oks = Vec::new();
            let mut env = env.clone();
            for exp in elements {
                env = get_exp!(exp, &env, scope);
                oks.push(env.clone());
            }
            CompExpression::Array(oks)
        }
        Expression::DotAccess(val, key) => {
            CompExpression::DotAccess(get_exp!(val, env, scope), key.clone())
        }
        Expression::Typeof(exp) => CompExpression::Typeof(get_exp!(exp, env, scope)),
        Expression::Index(arr, index) => {
            let arr_exp = get_exp!(arr, env, scope);
            let index_exp = get_exp!(index, env, scope);
            CompExpression::Index(arr_exp, index_exp)
        }
        Expression::TypeDeclaration(_, _) => CompExpression::List(Vec::new()),
        Expression::InitAssign(_, _, name, _, exp) => {
            if scope.variable_initialised(&name.0) {
                errs.push(CompError::RedeclareInSameScope(name.0.clone(), loc.clone()));
            }

            let exp = get_exp!(exp, env, scope);
            let exp_ty = exp.result_type.clone();
            let has_type = scope.variable_has_type(&name.0);
            if !has_type {
                scope = scope.set_variable_type(&name.0, &exp_ty);
            }
            scope.set_variable_initialised(&name.0);
            let var = scope.get_variable(&name.0);
            let var = if let Ok(var) = var {
                var
            } else {
                errs.push(CompError::CannotFindVariable(name.0.clone(), loc.clone()));
                CompVariable {
                    name: name.0.clone(),
                    typing: exp_ty.clone(),
                    constant: false,
                    external: false,
                    declared_at: None,
                }
            };
            if has_type && !var.typing.super_of(&exp_ty) {
                errs.push(CompError::InvalidAssignment(
                    exp_ty,
                    var.typing.clone(),
                    loc.clone(),
                ));
            }
            CompExpression::Assign(
                ExpEnvironment {
                    result_type: var.typing.clone(),
                    expression: Box::new(CompExpression::Read(var)),
                    var_types: HashMap::new(),
                    located: loc.start..exp.located.start,
                    errors: Vec::new(),
                },
                exp,
            )
        }
        Expression::Assign(name, exp) => {
            let lhs = get_exp!(name, env, scope);
            let exp = get_exp!(exp, env, scope);
            CompExpression::Assign(lhs, exp)
        }
        Expression::IfElse(cond, left, right) => {
            let cond = get_exp!(cond, env, scope);
            let then = get_exp!(left, env, scope);
            let otherwise = get_exp!(right, env, scope);
            CompExpression::IfElse {
                cond,
                then,
                otherwise,
            }
        }
        Expression::Loop(exp, body) => {
            let cond = get_exp!(exp, env, scope);
            let body = get_exp!(body, env, scope);
            CompExpression::WhileLoop { cond, body }
        }
        Expression::Block(expressions) => {
            let mut env = env.clone();
            let mut oks = Vec::new();
            for exp in expressions {
                env = get_exp!(exp, &env, scope);
                oks.push(env.clone());
            }
            CompExpression::List(oks)
        }
        Expression::BinOp(op, l, r) => {
            let mut exp = bin_exp(op, &*l, r, env, scope, file);
            errs.append(&mut exp.1);
            exp.0
        }
        Expression::FuncCall(name, arguments) => {
            let mut args = Vec::new();
            let mut env = env.clone();
            for exp in arguments {
                env = get_exp!(exp, &env, scope);
                args.push(env.clone());
            }
            let func = if let Ok(var) = scope.get_variable(name) {
                var
            } else {
                errs.push(CompError::CannotFindVariable(name.clone(), loc.clone()));
                CompVariable {
                    name: name.clone(),
                    constant: false,
                    external: false,
                    typing: CompType::Unknown,
                    declared_at: None,
                }
            };
            CompExpression::Call(func, args)
        }
        Expression::Terminal(sym) => match sym {
            Symbol::Identifier(name) => {
                let var = if let Ok(var) = scope.get_variable(name) {
                    var
                } else {
                    errs.push(CompError::CannotFindVariable(name.clone(), loc.clone()));
                    CompVariable {
                        name: name.clone(),
                        constant: false,
                        external: false,
                        typing: CompType::Unknown,
                        declared_at: None,
                    }
                };
                CompExpression::Read(var)
            }
            Symbol::Data(data) => CompExpression::Value(match data.clone() {
                RawData::Int(val) => CompData::Int(val),
                RawData::Float(val) => CompData::Float(val),
                RawData::Str(val) => CompData::Str(val),
                RawData::Bool(val) => CompData::Bool(val),
                RawData::Null => CompData::Null,
                RawData::Func(func) => {
                    let mut temp_variables = Vec::new();
                    for arg in func.args {
                        let arg_ty = match transform_type(&arg.1, scope) {
                            Ok(ty) => ty,
                            Err(mut err) => {
                                errs.append(&mut err);
                                CompType::Unknown
                            }
                        };
                        let var = CompVariable {
                            name: arg.0 .0.clone(),
                            constant: true,
                            typing: arg_ty,
                            external: false,
                            declared_at: Some((file.to_string(), arg.0 .1)),
                        };
                        temp_variables.push(var);
                    }
                    let return_type = match transform_type(&func.return_type, scope) {
                        Ok(ty) => ty,
                        Err(mut err) => {
                            errs.append(&mut err);
                            CompType::Unknown
                        }
                    };
                    let arguments = temp_variables.clone();
                    let mut local_variables = HashMap::new();
                    for var in temp_variables {
                        local_variables.insert(var.name.clone(), var);
                    }
                    match func.body {
                        Some(body) => {
                            let mut local_scope = TempScope {
                                parent: Some(Box::new(scope.clone())),
                                preset_variables: local_variables,
                                variables: HashMap::new(),
                                types: HashMap::new(),
                            };
                            let local_scope = resolve_scope(&*body, &mut local_scope, file);
                            let (body, mut errors) = transform_ast(&*body, local_scope, file);
                            errs.append(&mut errors);
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
            }),
        },
        Expression::Invalid => panic!("invalid {:?}", loc),
    };
    get_env(&expression, env, loc.clone(), errs)
}

fn resolve_scope<'a>(
    (_, ast): &SpannedExpression,
    scope: &'a mut TempScope,
    file: &str,
) -> &'a mut TempScope {
    match ast {
        Expression::TypeDeclaration(name, declared_type) => {
            if !scope.types.contains_key(name) {
                if let Ok(ty) = transform_type(declared_type, scope) {
                    scope.add_type(name.clone(), ty);
                    scope
                } else {
                    scope
                }
            } else {
                scope
            }
        }
        Expression::InitAssign(external, constant, name, declared_type, _exp) => {
            if scope.variables.contains_key(&name.0) {
                scope
            } else {
                let typing = match declared_type {
                    None => None,
                    Some(x) => transform_type(x, scope).ok(),
                };
                scope.add_variable(NewVariable {
                    name: name.0.clone(),
                    constant: *constant,
                    typing,
                    initialised: false,
                    external: *external,
                    declared_at: (file.to_string(), name.1.clone()),
                })
            }
        }
        Expression::Assign(_, _) => scope,
        Expression::Block(expressions) => {
            for exp in expressions {
                resolve_scope(exp, scope, file);
            }
            scope
        }
        _x => scope,
    }
}

fn get_env_from_scope(scope: &TempScope) -> ExpEnvironment {
    ExpEnvironment {
        var_types: scope
            .variables
            .iter()
            .filter_map(|(name, var)| Some((name.clone(), var.typing.clone()?)))
            .clone()
            .collect(),
        result_type: CompType::Null,
        located: 0..0,
        expression: Box::new(CompExpression::List(Vec::new())),
        errors: Vec::new(),
    }
}

fn transform_ast(
    ast: &SpannedExpression,
    scope: &mut TempScope,
    file: &str,
) -> (Program, Vec<CompError>) {
    let env = get_env_from_scope(scope);
    let expression = transform_exp(ast, &env, scope, file);
    let prog = Program {
        scope: scope.clone(),
        body: expression.0,
    };
    (prog, expression.1)
}

pub fn create_program(
    ast: &SpannedExpression,
    scope: &mut TempScope,
    settings: &Settings,
) -> (Program, Vec<CompError>) {
    resolve_scope(ast, scope, &settings.input_name);
    transform_ast(ast, scope, &settings.input_name)
}

pub fn function_from_generics(
    func: Function,
    generics: Vec<CompType>,
    scope: &mut TempScope,
    file: &str,
) -> (FunctionAst, Vec<CompError>) {
    let mut errs = Vec::new();
    let mut scope = TempScope {
        parent: Some(Box::new(scope.clone())),
        preset_variables: HashMap::new(),
        variables: HashMap::new(),
        types: HashMap::new(),
    };
    let generic_names = func.generics;
    for gen in generic_names.into_iter().zip(generics) {
        scope.add_type(gen.0, gen.1);
    }
    let mut temp_variables = Vec::new();
    for x in func.args {
        let typing = match transform_type(&x.1.clone(), &scope) {
            Ok(ty) => ty,
            Err(mut err) => {
                errs.append(&mut err);
                CompType::Unknown
            }
        };
        temp_variables.push(CompVariable {
            name: x.0 .0.clone(),
            constant: true,
            typing,
            external: false,
            declared_at: Some((file.to_string(), x.0 .1.clone())),
        });
    }
    let return_type = match transform_type(&func.return_type, &scope) {
        Ok(ty) => ty,
        Err(mut err) => {
            errs.append(&mut err);
            CompType::Unknown
        }
    };
    let arguments = temp_variables.clone();
    let mut local_variables = HashMap::new();
    for var in temp_variables {
        local_variables.insert(var.name.clone(), var);
    }
    let func = match func.body {
        Some(body) => {
            let mut local_scope = TempScope {
                parent: Some(Box::new(scope.clone())),
                preset_variables: local_variables,
                variables: HashMap::new(),
                types: HashMap::new(),
            };
            let local_scope = resolve_scope(&*body, &mut local_scope, file);

            let env = get_env_from_scope(&scope);
            let mut expression = transform_exp(&*body, &env, &mut scope, file);
            let body = Program {
                scope: local_scope.clone(),
                body: expression.0,
            };
            errs.append(&mut expression.1);

            FunctionAst {
                arguments,
                return_type,
                body: Some(Box::new(body)),
            }
        }
        None => FunctionAst {
            arguments,
            return_type,
            body: None,
        },
    };
    (func, errs)
}

pub fn get_env(
    exp: &CompExpression,
    env: &ExpEnvironment,
    located: Range<usize>,
    mut errs: Vec<CompError>,
) -> (ExpEnvironment, Vec<CompError>) {
    use CompExpression::*;
    match exp {
        DotAccess(val, (key, _)) => {
            if let CompType::Union(types) = &val.result_type {
                let union = types
                    .clone()
                    .iter()
                    .filter_map(|x| match x {
                        CompType::Struct(keys) => keys
                            .clone()
                            .iter()
                            .find(|x| &x.0 == key)
                            .map(|x| x.1.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                let result_type = if union.len() != types.len() {
                    errs.push(CompError::MissingPropertyInUnion(
                        key.clone(),
                        val.result_type.clone(),
                        located.clone(),
                    ));
                    CompType::Unknown
                } else {
                    CompType::Union(union)
                };
                (
                    ExpEnvironment {
                        expression: Box::new(exp.clone()),
                        result_type,
                        located,
                        ..env.clone()
                    },
                    errs,
                )
            } else if let CompType::Struct(keys) = &val.result_type {
                let result_type = if let Some(ty) = &keys.clone().iter().find(|x| &x.0 == key) {
                    ty.1.clone()
                } else {
                    errs.push(CompError::PropertyDoesNotExistOnType(
                        key.clone(),
                        val.result_type.clone(),
                        located.clone(),
                    ));
                    CompType::Unknown
                };
                (
                    ExpEnvironment {
                        result_type,
                        expression: Box::new(exp.clone()),
                        located,
                        ..env.clone()
                    },
                    errs,
                )
            } else {
                errs.push(CompError::PropertyDoesNotExistOnType(
                    key.clone(),
                    val.result_type.clone(),
                    located.clone(),
                ));
                (
                    ExpEnvironment {
                        result_type: CompType::Unknown,
                        expression: Box::new(exp.clone()),
                        located,
                        ..env.clone()
                    },
                    errs,
                )
            }
        }
        Struct(keys) => {
            let result_type = CompType::Struct(map_vec!(keys, |(k, v)| (
                k.0.clone(),
                v.result_type.clone()
            )))
            .flatten();
            (
                ExpEnvironment {
                    located,
                    expression: Box::new(exp.clone()),
                    result_type,
                    ..env.clone()
                },
                errs,
            )
        }
        Array(elements) => {
            if elements.is_empty() {
                return (
                    ExpEnvironment {
                        expression: Box::new(exp.clone()),
                        result_type: CompType::Array(Box::new(CompType::Unknown), 0),
                        located,
                        ..env.clone()
                    },
                    errs,
                );
            }
            let el_ty = elements[0].result_type.clone();
            // TODO: Rework this to work with union types
            // Is it worth allowing inference for unions, or maybe make arrays work like touples?
            let non_allowed = elements
                .iter()
                .filter(|el| el.result_type != el_ty)
                .collect::<Vec<_>>();
            let result_type = if !non_allowed.is_empty() {
                errs.push(CompError::MismatchedTypeInArray(
                    el_ty,
                    map_vec!(non_allowed, |el| el.result_type.clone()),
                    non_allowed[0].located.start..non_allowed.last().unwrap().located.end,
                ));
                CompType::Array(Box::new(CompType::Unknown), elements.len())
            } else {
                CompType::Array(Box::new(el_ty), elements.len())
            };
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }

        Typeof(_) => (
            ExpEnvironment {
                expression: Box::new(exp.clone()),
                result_type: CompType::Type,
                located,
                ..env.clone()
            },
            errs,
        ),
        Prog(prog) => (
            ExpEnvironment {
                expression: Box::new(exp.clone()),
                result_type: prog.body.result_type.clone(),
                located,
                ..env.clone()
            },
            errs,
        ),
        List(exps) => (
            exps.last()
                .map(|x| ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: x.result_type.clone(),
                    located: located.clone(),
                    ..env.clone()
                })
                .unwrap_or(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Null,
                    located,
                    ..env.clone()
                }),
            errs,
        ),
        WhileLoop { cond, body } => {
            if !cond.result_type.is_bool() {
                errs.push(CompError::BoolInWhile(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ));
            }
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: body.result_type.clone(),
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        IfElse {
            cond,
            then,
            otherwise,
        } => {
            if !cond.result_type.is_bool() {
                errs.push(CompError::BoolInIf(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ));
            }
            let then_ty = then.result_type.clone();
            let other_ty = otherwise.result_type.clone();
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Union(vec![then_ty, other_ty]).flatten(),
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        IfOnly { cond, then: _ } => {
            if !cond.result_type.is_bool() {
                errs.push(CompError::BoolInIf(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ));
            }
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Null,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        Value(data) => (
            ExpEnvironment {
                expression: Box::new(exp.clone()),
                result_type: data.get_type(),
                located,
                ..env.clone()
            },
            errs,
        ),
        Index(arr, i) => {
            let arr_ty = arr.result_type.clone();
            let i_ty = i.result_type.clone();
            // TODO: Move to use an is_indexable method
            let result_type = if !arr_ty.is_str() && !arr_ty.is_array() {
                errs.push(CompError::CannotIndexType(arr_ty, i.located.clone()));
                CompType::Unknown
            } else if !i_ty.is_int() {
                errs.push(CompError::InvalidIndexType(i_ty, i.located.clone()));
                CompType::Unknown
            } else if let CompType::Array(elements, _) = arr_ty {
                *elements
            } else if arr_ty.is_str() {
                CompType::Int
            } else {
                CompType::Unknown
            };
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        Assign(var, lhs) => {
            let exp_ty = lhs.result_type.clone();
            let var_ty = var.result_type.clone();
            if !var_ty.super_of(&exp_ty) {
                errs.push(CompError::InvalidAssignment(
                    var_ty,
                    exp_ty.clone(),
                    located.clone(),
                ));
            }
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: exp_ty,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        BinOp(op, a, b) => {
            let result_type = match op.resulting_type(&a.result_type, &b.result_type) {
                Ok(ty) => ty,
                Err(err) => {
                    errs.push(err);
                    CompType::Unknown
                }
            };
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
        OneOp(_, val) => (
            ExpEnvironment {
                expression: Box::new(exp.clone()),
                result_type: val.result_type.clone(),
                located,
                ..env.clone()
            },
            errs,
        ),
        Read(var) => (
            ExpEnvironment {
                expression: Box::new(exp.clone()),
                result_type: var.typing.clone(),
                located,
                ..env.clone()
            },
            errs,
        ),
        Call(var, args) => {
            let var = var.clone();
            let result_type = if let CompType::Callible(arg_types, ret) = var.typing {
                let mismatched_args = arg_types
                    .iter()
                    .zip(args)
                    .map(|(x, y)| {
                        if !x.super_of(&y.result_type) {
                            Some(CompError::InvalidAssignment(
                                y.result_type.clone(),
                                x.clone(),
                                y.located.clone(),
                            ))
                        } else {
                            None
                        }
                    })
                    .find(|x| x.is_some())
                    .flatten();
                if let Some(err) = mismatched_args {
                    errs.push(err);
                }
                *ret
            } else {
                errs.push(CompError::NonfunctionCall(
                    var.name,
                    var.typing,
                    located.clone(),
                ));
                CompType::Unknown
            };
            (
                ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type,
                    located,
                    ..env.clone()
                },
                errs,
            )
        }
    }
}

fn calc_type_from_not(from: &CompType, not: &CompType) -> CompType {
    use CompType::*;
    if let Union(types) = from {
        if types.contains(not) {
            Union(types.iter().filter(|ty| ty != &not).cloned().collect()).flatten()
        } else {
            from.clone()
        }
    } else if not.is_primitive() && from.is_primitive() {
        if not == from {
            Union(Vec::new())
        } else {
            from.clone()
        }
    } else {
        // ToDO: Actually match all cases, this should not be reached if correct code is written but still technically reachable and so needs to be handled properly
        unreachable!();
        Union(Vec::new())
    }
}
