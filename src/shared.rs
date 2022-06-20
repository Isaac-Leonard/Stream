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
        CustomType::Constant(data) => Ok(CompType::Constant(data.clone())),
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

fn resolve_memory(
    mut exp: &SpannedExpression,
    env: &ExpEnvironment,
    scope: &mut TempScope,
    file: &str,
) -> (Option<MemoryLocation>, Vec<CompError>) {
    let mut accesses = Vec::new();
    let mut errs = Vec::new();
    loop {
        match &exp.1 {
            Expression::Index(left, index) => {
                accesses.push(IndexOption::Index(
                    transform_exp(&index, &env, scope, file).0,
                ));
                exp = left.as_ref();
            }
            Expression::DotAccess(left, prop) => {
                accesses.push(IndexOption::Dot(prop.0.clone()));
                exp = left.as_ref();
            }
            Expression::Terminal(Symbol::Identifier(ref name)) => {
                let var = scope.get_variable(name);
                return if let Ok(var) = var {
                    accesses.reverse();
                    (
                        Some(MemoryLocation {
                            variable: var,
                            accessing: map_vec!(accesses, |x| (x.clone(), CompType::Unknown)),
                        }),
                        errs,
                    )
                } else {
                    errs.push(CompError::CannotFindVariable(name.clone(), exp.0.clone()));
                    (None, errs)
                };
            }
            _ => return (None, errs),
        }
    }
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

    macro_rules! resolve_memory_location {
        ($exp:expr, $env:expr, $scope:expr) => {{
            let mut res = resolve_memory($exp, $env, $scope, file);
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
        Expression::Conversion(exp, ty) => {
            let ty = transform_type(ty, scope);
            let ty = match ty {
                Ok(ty) => ty,
                Err(mut errors) => {
                    errs.append(&mut errors);
                    CompType::Unknown
                }
            };
            CompExpression::Conversion(get_exp!(exp, env, scope), ty)
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

            let mem = MemoryLocation {
                variable: var,
                accessing: Vec::new(),
            };
            CompExpression::Assign(mem, exp)
        }
        Expression::Assign(name, exp) => {
            let lhs = resolve_memory_location!(name, env, scope);
            let exp = get_exp!(exp, env, scope);
            if let Some(lhs) = lhs {
                CompExpression::Assign(lhs, exp)
            } else {
                errs.push(CompError::InvalidLeftHandForAssignment(
                    name.1.clone(),
                    name.0.clone(),
                ));
                CompExpression::List(Vec::new())
            }
        }
        Expression::IfElse(cond, left, right) => {
            let cond = get_exp!(cond, env, scope);
            let then = get_exp!(left, env, scope);
            let otherwise = get_exp!(right, env, scope);
            CompExpression::IfElse(IfElse {
                cond,
                then,
                otherwise,
            })
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
                        let arg_ty = if let Some(ty) = arg.1 {
                            match transform_type(&ty, scope) {
                                Ok(ty) => ty,
                                Err(mut err) => {
                                    errs.append(&mut err);
                                    CompType::Unknown
                                }
                            }
                        } else {
                            CompType::Unknown
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
    get_env(expression, env, loc.clone(), errs)
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
                }
            }
            scope
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
        let typing = if let Some(ty) = x.1.clone() {
            match transform_type(&ty, &scope) {
                Ok(ty) => ty,
                Err(mut err) => {
                    errs.append(&mut err);
                    CompType::Unknown
                }
            }
        } else {
            CompType::Unknown
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

fn get_top_type(types: &[CompType]) -> CompType {
    use CompType::*;
    let mut highest_types = Vec::new();
    for ty in types {
        highest_types.push(match ty {
            Int | Constant(ConstantData::Int(_)) => Int,
            Float | Constant(ConstantData::Float(_)) => Float,
            Null | Constant(ConstantData::Null) => Null,
            Bool | Constant(ConstantData::Bool(_)) => Bool,
            Constant(ConstantData::Str(str)) => Str(str.len() as u32),
            x => x.clone(),
        });
    }
    return Union(highest_types).flatten();
}

pub fn get_env(
    mut exp: CompExpression,
    env: &ExpEnvironment,
    located: Range<usize>,
    errs: Vec<CompError>,
) -> (ExpEnvironment, Vec<CompError>) {
    let (ty, errs) = get_type(&mut exp, env, located.clone(), errs);
    (
        ExpEnvironment {
            expression: Box::new(exp.clone()),
            result_type: ty,
            located,
            ..env.clone()
        },
        errs,
    )
}

pub fn get_type(
    exp: &mut CompExpression,
    _env: &ExpEnvironment,
    located: Range<usize>,
    mut errs: Vec<CompError>,
) -> (CompType, Vec<CompError>) {
    use CompExpression::*;
    match exp {
        Conversion(exp, ty) => {
            eprintln!("convert {:?} to {:?}", exp.result_type, ty);
            (ty.clone(), errs)
        }
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
                (result_type, errs)
            } else if let CompType::Struct(keys) = &val.result_type {
                let result_type = if let Some(ty) = &keys.clone().iter().find(|x| &x.0 == key) {
                    ty.1.clone()
                } else {
                    errs.push(CompError::PropertyDoesNotExistOnType(
                        key.clone(),
                        val.result_type.clone(),
                        located,
                    ));
                    CompType::Unknown
                };
                (result_type, errs)
            } else {
                errs.push(CompError::PropertyDoesNotExistOnType(
                    key.clone(),
                    val.result_type.clone(),
                    located,
                ));
                (CompType::Unknown, errs)
            }
        }
        Struct(keys) => {
            let result_type = CompType::Struct(map_vec!(keys, |(k, v)| (
                k.0.clone(),
                v.result_type.clone()
            )))
            .flatten();
            (result_type, errs)
        }
        Array(elements) => {
            let el_types = map_vec!(elements, |el| el.result_type.widen());
            // TODO: Fix this as its hacky and will cause a crash
            (
                CompType::Array(Box::new(el_types[0].clone()), elements.len()),
                errs,
            )
        }

        Typeof(_) => (CompType::Type, errs),
        Prog(prog) => (prog.body.result_type.clone(), errs),
        List(exps) => (
            exps.last()
                .map(|x| x.result_type.clone())
                .unwrap_or(CompType::Null),
            errs,
        ),
        WhileLoop { cond, body } => {
            if !cond.result_type.is_bool() {
                errs.push(CompError::BoolInWhile(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ));
            }
            (body.result_type.clone(), errs)
        }
        IfElse(if_exp) => {
            if !if_exp.cond.result_type.is_bool() {
                errs.push(CompError::BoolInIf(
                    if_exp.cond.result_type.clone(),
                    if_exp.cond.located.clone(),
                ));
            }
            let then_ty = if_exp.then.result_type.clone();
            let other_ty = if_exp.otherwise.result_type.clone();
            (CompType::Union(vec![then_ty, other_ty]).flatten(), errs)
        }
        IfOnly { cond, then: _ } => {
            if !cond.result_type.is_bool() {
                errs.push(CompError::BoolInIf(
                    cond.result_type.clone(),
                    cond.located.clone(),
                ));
            }
            (CompType::Null, errs)
        }
        Value(data) => (data.get_type(), errs),
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
                CompType::Char
            } else {
                CompType::Unknown
            };
            (result_type, errs)
        }
        Assign(var, lhs) => {
            let exp_ty = lhs.result_type.clone();
            let mut var_ty = var.variable.typing.clone();
            let mut accesses = Vec::new();
            for access in &var.accessing {
                use IndexOption::*;
                match &access.0 {
                    Dot(prop) => {
                        if let CompType::Struct(data) = &var_ty {
                            let ty = data.iter().find(|x| &x.0 == prop).unwrap().1.clone();
                            accesses.push((Dot(prop.clone()), var_ty.clone()));
                            var_ty = ty;
                        } else {
                            errs.push(CompError::CannotIndexType(var_ty.clone(), 0..0));
                            accesses.push(access.clone());
                        }
                    }
                    Index(index) => {
                        if !index.result_type.is_int() {
                            var_ty = CompType::Unknown;
                            errs.push(CompError::InvalidIndexType(
                                index.result_type.clone(),
                                index.located.clone(),
                            ));
                            accesses.push((Index(index.clone()), CompType::Unknown))
                        } else if let CompType::Array(el_ty, len) = var_ty {
                            var_ty = el_ty.as_ref().clone();
                            accesses.push((Index(index.clone()), var_ty.clone()));
                        } else {
                            errs.push(CompError::CannotIndexType(
                                var_ty.clone(),
                                index.located.clone(),
                            ));
                            var_ty = CompType::Unknown;
                        };
                    }
                }
            }
            let exp_ty = if let CompType::Touple(elements) = exp_ty {
                CompType::Array(Box::new(get_top_type(&elements)), elements.len())
            } else {
                exp_ty
            };
            if !var_ty.super_of(&exp_ty) {
                errs.push(CompError::InvalidAssignment(
                    var_ty,
                    exp_ty.clone(),
                    located,
                ));
            }
            var.accessing = accesses;
            (exp_ty, errs)
        }
        BinOp(op, a, b) => {
            let result_type = match op.resulting_type(&a.result_type, &b.result_type) {
                Ok(ty) => ty,
                Err(err) => {
                    errs.push(err);
                    CompType::Unknown
                }
            };
            (result_type, errs)
        }
        OneOp(_, val) => (val.result_type.clone(), errs),
        Read(var) => (var.typing.clone(), errs),
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
                errs.push(CompError::NonfunctionCall(var.name, var.typing, located));
                CompType::Unknown
            };
            (result_type, errs)
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
