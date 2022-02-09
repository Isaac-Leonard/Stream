pub mod shared {
    use crate::ast::ast::*;
    use crate::errors::errors::*;
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

    pub fn transform_type(
        ty: &CustomType,
        scope: &CompScope,
        types: &HashMap<String, CompType>,
    ) -> Result<CompType, Vec<String>> {
        match ty {
            CustomType::Union(sub_types) => {
                collect_ok_or_err(sub_types.iter().map(|x| match types.get(x) {
                    None => scope.get_type(x),
                    Some(ty) => Ok(ty.flatten()),
                }))
                .unwrap_or_else(|| Err(vec!["Cannot have empty types".to_string()]))
                .map(|x| CompType::Union(x).flatten())
            }

            CustomType::Callible(args, ret) => {
                let args = collect_ok_or_err(args.iter().map(|x| transform_type(x, scope, types)))
                    .unwrap_or(Ok(Vec::new()));
                let ret = transform_type(ret, scope, types);
                match (args, ret) {
                    (Err(args), Err(ret)) => {
                        Err(args.iter().flatten().chain(ret.iter()).cloned().collect())
                    }
                    (Err(args), _) => Err(args.iter().flatten().cloned().collect()),
                    (_, Err(ret)) => Err(ret),
                    (Ok(args), Ok(ret)) => Ok(CompType::Callible(args, Box::new(ret))),
                }
            }
        }
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
                        let temp_variables = collect_ok_or_err(func.args.iter().map(|x| {
                            match transform_type(
                                &CustomType::Union(x.1.clone()),
                                &scope.to_comp_scope_so_far(),
                                &HashMap::new(),
                            ) {
                                Err(messages) => Err(messages),
                                Ok(typing) => Ok(CompVariable {
                                    constant: true,
                                    name: x.0.clone(),
                                    typing,
                                    external: false,
                                }),
                            }
                        }))
                        .unwrap_or_else(|| Ok(Vec::new()));
                        let return_type = transform_type(
                            &CustomType::Union(func.return_type).clone(),
                            &scope.to_comp_scope_so_far(),
                            &HashMap::new(),
                        );
                        let (temp_variables, return_type) = match (temp_variables, return_type) {
                            (Ok(vars), Ok(ret)) => (vars, ret),
                            (Err(vars), Err(ret)) => {
                                return Err(vars
                                    .iter()
                                    .flatten()
                                    .chain(ret.iter())
                                    .cloned()
                                    .collect())
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
                                let mut local_scope = match resolve_scope(
                                    &body,
                                    &scope.to_comp_scope_so_far(),
                                    &mut local_variables,
                                    &mut HashMap::new(),
                                ) {
                                    Err(messages) => return Err(messages),
                                    Ok(scope) => scope,
                                };
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
    ) -> Result<TempScope, Vec<String>> {
        let mut variables: HashMap<String, NewVariable> = HashMap::new();
        let mut errors = Vec::new();
        for stat in ast {
            match stat.clone() {
                Instr::TypeDeclaration(name, declared_type, _) => {
                    if !types.contains_key(&name) {
                        match transform_type(&declared_type, scope, types) {
                            Ok(ty) => {
                                types.insert(name, ty);
                            }
                            Err(messages) => errors.append(&mut messages.clone()),
                        }
                    } else {
                        errors.push(format!("Type '{}' is already defined", name));
                    }
                }
                Instr::InitAssign(external, constant, name, declared_type, _exp) => {
                    if variables.values().any(|x| x.name == name) {
                        errors.push(format!(
                            "Cannot re-declare variable in the same scope '{}'",
                            name
                        ));
                    } else if external && declared_type == None {
                        errors.push(format!(
                            "External variable '{}' must be declared with a type",
                            name
                        ));
                    } else {
                        let typing = declared_type
                            .map(
                                |x| match transform_type(&CustomType::Union(x), scope, types) {
                                    Ok(x) => Some(x),
                                    Err(messages) => {
                                        errors.append(&mut messages.clone());
                                        None
                                    }
                                },
                            )
                            .flatten();
                        variables.insert(
                            name.clone(),
                            NewVariable {
                                constant,
                                name,
                                typing,
                                initialised: false,
                                external,
                            },
                        );
                    }
                }
                Instr::Assign(name, _, loc) => {
                    if scope.is_global() {
                        errors.push(format!(
                            "Cannot reassign after declaration in the global scope '{}' at {}",
                            name, loc.start
                        ));
                    } else if !scope.variable_exists(&name) && !variables.contains_key(&name) {
                        errors.push(format!(
                            "Attempted to assign to undeclared variable '{}'",
                            name
                        ));
                    } else if scope.constant_exists(&name) || variables.values().any(|x| x.constant)
                    {
                        errors.push(format!(
                            "Attempted to reassign to constant variable '{}'",
                            name
                        ));
                    }
                }
                _x => {}
            };
        }
        if errors.is_empty() {
            Ok(TempScope {
                preset_variables: preset_variables.clone(),
                variables: variables.clone(),
                types: types.clone(),
                parent: Some(Box::new(scope.clone())),
            })
        } else {
            Err(errors)
        }
    }

    fn transform_ast(ast: &Vec<Instr>, mut scope: &mut TempScope) -> Result<Program, Vec<String>> {
        let mut expressions: Vec<CompExpression> = Vec::new();
        let mut errors: Vec<String> = Vec::new();
        for stat in ast {
            println!("{:?}", stat);
            match stat {
                Instr::TypeDeclaration(_, _, _) => {}
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
                Instr::Assign(name, exp, loc) => {
                    let exp = transform_exp(&exp, scope);
                    let exp = match exp {
                        Ok(exp) => exp,
                        Err(msg) => {
                            errors.append(&mut msg.clone());
                            continue;
                        }
                    };
                    if scope.constant_exists(name) {
                        errors.push(format!(
                            "Cannot reassign to constant variable '{}' at {}",
                            name, loc.start
                        ));
                        continue;
                    }
                    let var = scope.get_variable(name);
                    let assign = match var {
                        Err(msg) => {
                            errors.push(format!("{} at {}", msg, loc.start));
                            continue;
                        }
                        Ok(var) => CompExpression::Assign(var, Box::new(exp)),
                    };
                    match get_type_from_exp(&assign) {
                        Ok(_exp) => expressions.push(assign),
                        Err(msg) => errors.push(msg),
                    };
                }
                Instr::Loop(exp, body, _) => {
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
                Instr::IfElse(cond, left, right, loc) => {
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
                    let exp = CompExpression::IfElse {
                        cond: Box::new(cond),
                        then: Box::new(CompExpression::Prog(Box::new(then))),
                        otherwise: Box::new(CompExpression::Prog(Box::new(alt))),
                    };
                    match get_type_from_exp(&exp) {
                        Ok(_) => expressions.push(exp),
                        Err(msg) => {
                            errors.push(format!("{} from {} to {}", msg, loc.start, loc.end))
                        }
                    };
                }
                Instr::LoneExpression(exp, _) => {
                    let exp = transform_exp(&exp, scope);
                    let _exp = match exp {
                        Ok(exp) => expressions.push(exp),
                        Err(msg) => errors.append(&mut msg.clone()),
                    };
                }
                Instr::Invalid(x) => {
                    errors.push(format!("invalid '{}'", x));
                }
            };
        }
        let expressions = expressions;
        let scope = scope.to_comp_scope_so_far();
        if errors.is_empty() {
            Ok(Program {
                scope: scope.clone(),
                body: CompExpression::List(expressions),
            })
        } else {
            Err(errors)
        }
    }

    pub fn create_program(ast: &Vec<Instr>, scope: &CompScope) -> Result<Program, Vec<String>> {
        let mut local_scope =
            match resolve_scope(ast, scope, &mut HashMap::new(), &mut HashMap::new()) {
                Err(messages) => return Err(messages),
                Ok(scope) => scope,
            };
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
                        Err(
                            "The comparison expression in a while loop must return a Bool"
                                .to_string(),
                        )
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
                            Ok(
                                CompType::Union(vec![then_ty.unwrap(), other_ty.unwrap()])
                                    .flatten(),
                            )
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
}
