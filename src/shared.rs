use crate::ast::*;
use crate::errors::*;
use std::collections::HashMap;

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
    op: Op,
    left: &Expression,
    right: &Expression,
    scope: &mut TempScope,
) -> Result<CompExpression, Vec<CompError>> {
    let left = match transform_exp(left, scope) {
        Ok(exp) => exp,
        Err(msg) => return Err(msg),
    };
    let right = match transform_exp(right, scope) {
        Ok(exp) => exp,
        Err(msg) => return Err(msg),
    };
    Ok(CompExpression::BinOp(op, left, right))
}

fn transform_exp(
    exp: &Expression,
    mut scope: &mut TempScope,
) -> Result<ExpEnvironment, Vec<CompError>> {
    let expression = match exp {
        Expression::Array(elements, _) => CompExpression::Array(
            collect_ok_or_err(elements.iter().map(|x| transform_exp(x, scope)))
                .unwrap_or_else(|| Ok(Vec::new()))
                .map_err(|x| x.iter().flatten().cloned().collect::<Vec<_>>())?,
        ),
        Expression::Typeof(name, loc) => CompExpression::Typeof(
            scope
                .get_variable(name)
                .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?,
        ),
        Expression::Index(arr, index, _loc) => {
            let arr_exp = transform_exp(arr, scope)?;
            let index_exp = transform_exp(index, scope)?;
            CompExpression::Index(arr_exp, index_exp)
        }
        Expression::TypeDeclaration(_, _, _) => CompExpression::List(Vec::new()),
        Expression::InitAssign(_, _, name, _, exp, loc) => {
            if scope.variable_initialised(name) {
                return Err(vec![CompError::RedeclareInSameScope(
                    name.clone(),
                    loc.clone(),
                )]);
            }

            let exp = transform_exp(exp, scope)?;
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
        Expression::Assign(name, exp, loc) => {
            let rhs = transform_exp(name, scope)?;
            let exp = transform_exp(exp, scope)?;
            CompExpression::Assign(rhs, exp)
        }
        Expression::IfElse(cond, left, right, _) => {
            let cond = transform_exp(cond, scope)?;
            let then = transform_exp(left, scope)?;
            let otherwise = transform_exp(right, scope)?;
            CompExpression::IfElse {
                cond,
                then,
                otherwise,
            }
        }
        Expression::Loop(exp, body, _) => {
            let cond = transform_exp(exp, scope)?;
            let body = transform_exp(body, scope)?;
            CompExpression::WhileLoop { cond, body }
        }
        Expression::Block(expressions, _) => {
            collect_ok_or_err(expressions.iter().map(|exp| transform_exp(exp, scope)))
                .unwrap_or_else(|| Ok(Vec::new()))
                .map(CompExpression::List)
                .map_err(|x| x.iter().flatten().cloned().collect::<Vec<_>>())?
        }
        Expression::LessThan(l, r, _) => bin_exp(Op::Le, l, r, scope)?,
        Expression::Addition(l, r, _) => bin_exp(Op::Add, l, r, scope)?,
        Expression::Multiplication(l, r, _) => bin_exp(Op::Mult, l, r, scope)?,
        Expression::Subtraction(l, r, _) => bin_exp(Op::Sub, l, r, scope)?,
        Expression::Division(l, r, _) => bin_exp(Op::Div, l, r, scope)?,
        Expression::Equal(l, r, _) => bin_exp(Op::Eq, l, r, scope)?,
        Expression::FuncCall(name, args, loc) => {
            let args = args
                .iter()
                .map(|x| transform_exp(x, scope))
                .collect::<Result<Vec<_>, Vec<CompError>>>();
            let func = match scope.get_variable(name) {
                Ok(var) => var,
                Err(_) => {
                    return Err(vec![CompError::CannotFindVariable(
                        name.clone(),
                        loc.clone(),
                    )])
                }
            };
            match args {
                Ok(args) => CompExpression::Call(func, args),
                Err(message) => return Err(message),
            }
        }
        Expression::Terminal(sym, loc) => match sym {
            Symbol::Identifier(name) => match scope.get_variable(name) {
                Ok(var) => CompExpression::Read(var),
                Err(_) => {
                    return Err(vec![CompError::CannotFindVariable(
                        name.clone(),
                        loc.clone(),
                    )])
                }
            },
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
                                parent: Some(Box::new(scope.to_comp_scope_so_far())),
                                preset_variables: local_variables,
                                variables: HashMap::new(),
                                types: HashMap::new(),
                            };
                            let local_scope = resolve_scope(&body, &mut local_scope)?;
                            let body = transform_ast(&body, local_scope)?;

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
        Expression::Invalid(x) => panic!("invalid {:?}", x),
    };
    Ok(ExpEnvironment {
        result_type: get_type_from_exp(&expression, HashMap::new()).map_err(|x| vec![x])?,
        expression: Box::new(expression),
        var_types: HashMap::new(),
        located: exp.get_range(),
    })
}

fn resolve_scope<'a>(
    ast: &Expression,
    scope: &'a mut TempScope,
) -> Result<&'a mut TempScope, Vec<CompError>> {
    match ast {
        Expression::TypeDeclaration(name, declared_type, loc) => {
            if !scope.types.contains_key(name) {
                Ok(scope.add_type(name.clone(), transform_type(declared_type, scope)?))
            } else {
                Err(vec![CompError::TypeAlreadyDefined(
                    name.clone(),
                    loc.clone(),
                )])
            }
        }
        Expression::InitAssign(external, constant, name, declared_type, _exp, loc) => {
            if scope.variables.contains_key(name) {
                Err(vec![CompError::RedeclareInSameScope(
                    name.clone(),
                    loc.clone(),
                )])
            } else {
                let typing = match declared_type {
                    None => None,
                    Some(x) => Some(transform_type(x, scope)?),
                };
                Ok(scope.add_variable(NewVariable {
                    constant: *constant,
                    name: name.clone(),
                    typing,
                    initialised: false,
                    external: *external,
                }))
            }
        }
        Expression::Assign(name, _, loc) => {
            if let Expression::Terminal(Symbol::Identifier(name), _) = name.as_ref() {
                if scope.parent.is_none() {
                    Err(vec![CompError::GlobalReassign(name.clone(), loc.clone())])
                } else if !scope.variable_exists(name) {
                    Err(vec![CompError::CannotFindVariable(
                        name.clone(),
                        loc.clone(),
                    )])
                } else if scope.constant_exists(name) {
                    Err(vec![CompError::ConstReassign(name.clone(), loc.clone())])
                } else {
                    Ok(scope)
                }
            } else {
                Err(vec![CompError::InvalidLeftHandForAssignment(
                    *name.clone(),
                    loc.clone(),
                )])
            }
        }
        Expression::Block(expressions, _) => {
            for exp in expressions {
                resolve_scope(exp, scope);
            }
            Ok(scope)
        }
        _x => Ok(scope),
    }
}

fn transform_ast(ast: &Expression, scope: &mut TempScope) -> Result<Program, Vec<CompError>> {
    let expression = transform_exp(ast, scope)?;
    Ok(Program {
        scope: scope.to_comp_scope_so_far(),
        body: expression,
    })
}

pub fn create_program(ast: &Expression, scope: &CompScope) -> Result<Program, Vec<CompError>> {
    let mut local_scope = TempScope {
        parent: Some(Box::new(scope.clone())),
        variables: HashMap::new(),
        preset_variables: HashMap::new(),
        types: HashMap::new(),
    };
    let local_scope = resolve_scope(ast, &mut local_scope)?;
    let prog = transform_ast(ast, local_scope)?;
    Ok(prog)
}

pub fn substitute_generics(func: &FunctionAst) -> FunctionAst {
    func.clone()
}

pub fn get_type_from_exp(
    exp: &CompExpression,
    _var_types: HashMap<String, CompType>,
) -> Result<CompType, CompError> {
    use CompExpression::*;
    match exp {
        Array(elements) => {
            if elements.is_empty() {
                return Ok(CompType::Array(Box::new(CompType::Null), 0));
            }
            let el_ty = elements[0].result_type.clone();
            let non_allowed = elements
                .iter()
                .filter(|el| el.result_type != el_ty)
                .map(|el| el.result_type.clone())
                .collect::<Vec<_>>();
            if !non_allowed.is_empty() {
                Err(CompError::MismatchedTypeInArray(el_ty, non_allowed, 0..0))
            } else {
                Ok(CompType::Array(Box::new(el_ty), elements.len()))
            }
        }

        Typeof(_) => Ok(CompType::Type),
        Prog(prog) => Ok(prog.body.result_type.clone()),
        List(exps) => Ok(exps
            .last()
            .map(|x| x.result_type.clone())
            .unwrap_or(CompType::Null)),
        WhileLoop { cond, body } => {
            if cond.result_type.is_bool() {
                Ok(body.result_type.clone())
            } else {
                Err(CompError::BoolInWhile(cond.result_type.clone(), 0..0))
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
                Ok(CompType::Union(vec![then_ty, other_ty]).flatten())
            } else {
                Err(CompError::BoolInIf(cond.result_type.clone(), 0..0))
            }
        }
        IfOnly { cond, then: _ } => {
            if cond.result_type.is_bool() {
                Ok(CompType::Null)
            } else {
                Err(CompError::BoolInIf(cond.result_type.clone(), 0..0))
            }
        }
        Value(data) => Ok(data.get_type()),
        Index(arr, i) => {
            let arr_ty = arr.result_type.clone();
            let i_ty = i.result_type.clone();
            // TODO: Move to use an is_indexable method
            if !arr_ty.is_str() && !arr_ty.is_array() {
                Err(CompError::CannotIndexType(arr_ty, 0..0))
            } else if !i_ty.is_int() {
                Err(CompError::InvalidIndexType(i_ty, 0..0))
            } else {
                Ok(CompType::Int)
            }
        }
        Assign(var, exp) => {
            let exp = exp.result_type.clone();
            let var_ty = var.result_type.clone();
            if var_ty.super_of(&exp) {
                Ok(var_ty)
            } else {
                Err(CompError::InvalidAssignment(var_ty, exp, 0..0))
            }
        }
        BinOp(op, a, b) => op.resulting_type(&a.result_type, &b.result_type),
        OneOp(_, val) => Ok(val.result_type.clone()),
        Read(var) => Ok(var.typing.clone()),
        Call(var, args) => {
            let var = var.clone();
            let arg_types = args.iter().map(|x| x.result_type.clone());
            if let CompType::Callible(args, ret) = var.typing {
                let mismatched_args = args
                    .iter()
                    .zip(arg_types)
                    .map(|(x, y)| {
                        if !x.super_of(&y) {
                            Some(CompError::InvalidAssignment(y, x.clone(), 0..0))
                        } else {
                            None
                        }
                    })
                    .find(|x| x.is_some())
                    .flatten();
                if let Some(err) = mismatched_args {
                    Err(err)
                } else {
                    Ok(*ret)
                }
            } else {
                Err(CompError::NonfunctionCall(var.name, var.typing, 0..0))
            }
        }
    }
}
