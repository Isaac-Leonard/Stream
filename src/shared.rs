use crate::ast::*;
use crate::errors::*;
use crate::map_vec;
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
        CustomType::Lone(ty) => scope
            .get_type(&ty.name)
            .map_err(|_| vec![CompError::CannotFindType(ty.clone().name, 0..0)]),
    }
}

fn bin_exp(
    op: &Op,
    left: &SpannedExpression,
    right: &SpannedExpression,
    env: &ExpEnvironment,
    scope: &mut TempScope,
) -> Result<CompExpression, Vec<CompError>> {
    let left = transform_exp(left, env, scope)?;
    let right = transform_exp(right, env, scope)?;
    Ok(CompExpression::BinOp(op.clone(), left, right))
}

fn transform_exp(
    (loc, exp): &SpannedExpression,
    env: &ExpEnvironment,
    mut scope: &mut TempScope,
) -> Result<ExpEnvironment, Vec<CompError>> {
    let expression = match exp {
        Expression::Struct(data) => collect_ok_or_err(
            data.iter()
                .map(|(k, v)| Ok((k.clone(), transform_exp(v, env, scope)?))),
        )
        .unwrap_or_else(|| Ok(Vec::new()))
        .map(|x: Vec<_>| CompExpression::Struct(x.iter().cloned().collect()))
        .map_err(|x: Vec<Vec<CompError>>| x.iter().flatten().cloned().collect::<Vec<_>>())?,
        Expression::Array(elements) => {
            let mut errors = Vec::new();
            let mut oks = Vec::new();
            let mut env = env.clone();
            for exp in elements {
                env = match transform_exp(exp, &env, scope) {
                    Ok(env) => {
                        oks.push(env.clone());
                        env
                    }
                    Err(err) => {
                        errors.push(err);
                        env
                    }
                };
            }
            if errors.is_empty() {
                CompExpression::Array(oks)
            } else {
                return Err(errors.iter().flatten().cloned().collect());
            }
        }
        Expression::DotAccess(val, key) => {
            CompExpression::DotAccess(transform_exp(val, env, scope)?, key.clone())
        }
        Expression::Typeof(exp) => transform_exp(exp, env, scope).map(CompExpression::Typeof)?,
        Expression::Index(arr, index) => {
            let arr_exp = transform_exp(arr, env, scope)?;
            let index_exp = transform_exp(index, env, scope)?;
            CompExpression::Index(arr_exp, index_exp)
        }
        Expression::TypeDeclaration(_, _) => CompExpression::List(Vec::new()),
        Expression::InitAssign(_, _, name, _, exp) => {
            if scope.variable_initialised(name) {
                return Err(vec![CompError::RedeclareInSameScope(
                    name.clone(),
                    loc.clone(),
                )]);
            }

            let exp = transform_exp(exp, env, scope)?;
            let exp_ty = exp.result_type.clone();
            let has_type = scope.variable_has_type(name);
            if !has_type {
                scope = scope.set_variable_type(name, &exp_ty);
            }
            scope.set_variable_initialised(name);
            let var = scope
                .get_variable(name)
                .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?;
            if has_type && !var.typing.super_of(&exp_ty) {
                return Err(vec![CompError::InvalidAssignment(
                    exp_ty,
                    var.typing,
                    loc.clone(),
                )]);
            }
            CompExpression::Assign(
                ExpEnvironment {
                    result_type: var.typing.clone(),
                    expression: Box::new(CompExpression::Read(var)),
                    var_types: HashMap::new(),
                    located: loc.start..exp.located.start,
                },
                exp,
            )
        }
        Expression::Assign(name, exp) => {
            let lhs = transform_exp(name, env, scope)?;
            let exp = transform_exp(exp, env, scope)?;
            CompExpression::Assign(lhs, exp)
        }
        Expression::IfElse(cond, left, right) => {
            let cond = transform_exp(cond, env, scope)?;
            let then = transform_exp(left, env, scope)?;
            let otherwise = transform_exp(right, env, scope)?;
            CompExpression::IfElse {
                cond,
                then,
                otherwise,
            }
        }
        Expression::Loop(exp, body) => {
            let cond = transform_exp(exp, env, scope)?;
            let body = transform_exp(body, env, scope)?;
            CompExpression::WhileLoop { cond, body }
        }
        Expression::Block(expressions) => {
            let mut env = env.clone();
            let mut errors = Vec::new();
            let mut oks = Vec::new();
            for exp in expressions {
                env = match transform_exp(exp, &env, scope) {
                    Ok(env) => {
                        oks.push(env.clone());
                        env
                    }
                    Err(err) => {
                        errors.push(err);
                        env
                    }
                };
            }
            if errors.is_empty() {
                CompExpression::List(oks)
            } else {
                return Err(errors.iter().flatten().cloned().collect());
            }
        }
        Expression::BinOp(op, l, r) => bin_exp(op, &*l, r, env, scope)?,
        Expression::FuncCall(name, arguments) => {
            let mut errors = Vec::new();
            let mut args = Vec::new();
            let mut env = env.clone();
            for exp in arguments {
                let res = transform_exp(exp, &env, scope);
                env = match res {
                    Ok(env) => {
                        args.push(env.clone());
                        env
                    }
                    Err(err) => {
                        errors.push(err);
                        env
                    }
                };
            }
            if !errors.is_empty() {
                return Err(errors.iter().flatten().cloned().collect());
            }
            let func = scope
                .get_variable(name)
                .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?;
            CompExpression::Call(func, args)
        }
        Expression::Terminal(sym) => match sym {
            Symbol::Identifier(name) => scope
                .get_variable(name)
                .map(CompExpression::Read)
                .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?,
            Symbol::Data(data) => CompExpression::Value(match data.clone() {
                RawData::Int(val) => CompData::Int(val),
                RawData::Float(val) => CompData::Float(val),
                RawData::Str(val) => CompData::Str(val),
                RawData::Bool(val) => CompData::Bool(val),
                RawData::Null => CompData::Null,
                RawData::Func(func) => {
                    let generics = func.generics;
                    let temp_variables = collect_ok_or_err(func.args.iter().map(|x| {
                        transform_type(&x.1.clone(), scope).map(|typing| CompVariable {
                            constant: true,
                            name: x.0.clone(),
                            typing,
                            external: false,
                        })
                    }))
                    .unwrap_or_else(|| Ok(Vec::new()));
                    let return_type = transform_type(&func.return_type, scope);
                    let (temp_variables, return_type) = match (temp_variables, return_type) {
                        (Ok(vars), Ok(ret)) => (vars, ret),
                        (Err(vars), Err(ret)) => {
                            return Err(vars.iter().flatten().chain(ret.iter()).cloned().collect())
                        }
                        (Err(vars), _) => return Err(vars.iter().flatten().cloned().collect()),
                        (_, Err(ret)) => return Err(ret),
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
                            let local_scope = resolve_scope(&*body, &mut local_scope);
                            let body = transform_ast(&*body, local_scope)?;

                            CompData::Func(FunctionAst {
                                generics,
                                arguments,
                                return_type,
                                body: Some(Box::new(body)),
                            })
                        }
                        None => CompData::Func(FunctionAst {
                            generics,
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
    get_env(&expression, env, loc.clone()).map_err(|x| vec![x])
}

fn resolve_scope<'a>((_, ast): &SpannedExpression, scope: &'a mut TempScope) -> &'a mut TempScope {
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
            if scope.variables.contains_key(name) {
                scope
            } else {
                let typing = match declared_type {
                    None => None,
                    Some(x) => transform_type(x, scope).map_or(None, Some),
                };
                scope.add_variable(NewVariable {
                    constant: *constant,
                    name: name.clone(),
                    typing,
                    initialised: false,
                    external: *external,
                })
            }
        }
        Expression::Assign(name, exp) => scope,
        Expression::Block(expressions) => {
            for exp in expressions {
                resolve_scope(exp, scope);
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
    }
}

fn transform_ast(
    ast: &SpannedExpression,
    scope: &mut TempScope,
) -> Result<Program, Vec<CompError>> {
    let env = get_env_from_scope(scope);
    let expression = transform_exp(ast, &env, scope)?;
    Ok(Program {
        scope: scope.clone(),
        body: expression,
    })
}

pub fn create_program(
    ast: &SpannedExpression,
    scope: &mut TempScope,
) -> Result<Program, Vec<CompError>> {
    resolve_scope(ast, scope);
    let prog = transform_ast(ast, scope)?;
    Ok(prog)
}

pub fn substitute_generics(func: &FunctionAst) -> FunctionAst {
    func.clone()
}

pub fn get_env(
    exp: &CompExpression,
    env: &ExpEnvironment,
    located: Range<usize>,
) -> Result<ExpEnvironment, CompError> {
    use CompExpression::*;
    match exp {
        DotAccess(val, key) => {
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
                if union.len() == types.len() {
                    Ok(ExpEnvironment {
                        expression: Box::new(exp.clone()),
                        result_type: CompType::Union(union),
                        located,
                        ..env.clone()
                    })
                } else {
                    Err(CompError::MissingPropertyInUnion(
                        key.clone(),
                        val.result_type.clone(),
                        located,
                    ))
                }
            } else if let CompType::Struct(keys) = &val.result_type {
                if let Some(ty) = &keys.clone().iter().find(|x| &x.0 == key) {
                    Ok(ExpEnvironment {
                        result_type: ty.1.clone(),
                        expression: Box::new(exp.clone()),
                        located,
                        ..env.clone()
                    })
                } else {
                    Err(CompError::PropertyDoesNotExistOnType(
                        key.clone(),
                        val.result_type.clone(),
                        located,
                    ))
                }
            } else {
                Err(CompError::PropertyDoesNotExistOnType(
                    key.clone(),
                    val.result_type.clone(),
                    located,
                ))
            }
        }
        Struct(keys) => Ok(ExpEnvironment {
            located,
            expression: Box::new(exp.clone()),
            result_type: CompType::Struct(map_vec!(keys, |(k, v)| (
                k.clone(),
                v.result_type.clone()
            )))
            .flatten(),
            ..env.clone()
        }),
        Array(elements) => {
            if elements.is_empty() {
                return Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Array(Box::new(CompType::Null), 0),
                    located,
                    ..env.clone()
                });
            }
            let el_ty = elements[0].result_type.clone();
            let non_allowed = elements
                .iter()
                .filter(|el| el.result_type != el_ty)
                .collect::<Vec<_>>();
            if !non_allowed.is_empty() {
                Err(CompError::MismatchedTypeInArray(
                    el_ty,
                    map_vec!(non_allowed, |el| el.result_type.clone()),
                    non_allowed[0].located.start..non_allowed.last().unwrap().located.end,
                ))
            } else {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Array(Box::new(el_ty), elements.len()),
                    located,
                    ..env.clone()
                })
            }
        }

        Typeof(_) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: CompType::Type,
            located,
            ..env.clone()
        }),
        Prog(prog) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: prog.body.result_type.clone(),
            located,
            ..env.clone()
        }),
        List(exps) => Ok(exps
            .last()
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
            })),
        WhileLoop { cond, body } => {
            if cond.result_type.is_bool() {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: body.result_type.clone(),
                    located,
                    ..env.clone()
                })
            } else {
                Err(CompError::BoolInWhile(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ))
            }
        }
        IfElse {
            cond,
            then,
            otherwise,
        } => {
            if cond.result_type.is_bool() {
                let then_ty = then.result_type.clone();
                let other_ty = otherwise.result_type.clone();
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Union(vec![then_ty, other_ty]).flatten(),
                    located,
                    ..env.clone()
                })
            } else {
                Err(CompError::BoolInIf(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ))
            }
        }
        IfOnly { cond, then: _ } => {
            if cond.result_type.is_bool() {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Null,
                    located,
                    ..env.clone()
                })
            } else {
                Err(CompError::BoolInIf(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ))
            }
        }
        Value(data) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: data.get_type(),
            located,
            ..env.clone()
        }),
        Index(arr, i) => {
            let arr_ty = arr.result_type.clone();
            let i_ty = i.result_type.clone();
            // TODO: Move to use an is_indexable method
            if !arr_ty.is_str() && !arr_ty.is_array() {
                Err(CompError::CannotIndexType(arr_ty, i.located.clone()))
            } else if !i_ty.is_int() {
                Err(CompError::InvalidIndexType(i_ty, i.located.clone()))
            } else if let CompType::Array(elements, _) = arr_ty {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: *elements,
                    located,
                    ..env.clone()
                })
            } else if arr_ty.is_str() {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: CompType::Int,
                    located,
                    ..env.clone()
                })
            } else {
                panic!("Here");
                unreachable!()
            }
        }
        Assign(var, lhs) => {
            let exp_ty = lhs.result_type.clone();
            let var_ty = var.result_type.clone();
            if var_ty.super_of(&exp_ty) {
                Ok(ExpEnvironment {
                    expression: Box::new(exp.clone()),
                    result_type: exp_ty,
                    located,
                    ..env.clone()
                })
            } else {
                Err(CompError::InvalidAssignment(var_ty, exp_ty, located))
            }
        }
        BinOp(op, a, b) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: op.resulting_type(&a.result_type, &b.result_type)?,
            located,
            ..env.clone()
        }),
        OneOp(_, val) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: val.result_type.clone(),
            located,
            ..env.clone()
        }),
        Read(var) => Ok(ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: var.typing.clone(),
            located,
            ..env.clone()
        }),
        Call(var, args) => {
            let var = var.clone();
            if let CompType::Callible(arg_types, ret) = var.typing {
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
                    Err(err)
                } else {
                    Ok(ExpEnvironment {
                        expression: Box::new(exp.clone()),
                        result_type: *ret,
                        located,
                        ..env.clone()
                    })
                }
            } else {
                Err(CompError::NonfunctionCall(var.name, var.typing, located))
            }
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
