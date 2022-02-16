pub mod shared {
    use crate::ast::ast::*;
    use crate::errors::errors::*;
    use std::borrow::BorrowMut;
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
                collect_ok_or_err(sub_types.iter().map(|x| scope.get_type(x)))
                    .unwrap_or_else(|| Err(vec![CompError::EmptyType(0..0)]))
                    .map(|x| CompType::Union(x).flatten())
            }

            CustomType::Callible(args, ret) => {
                let args = collect_ok_or_err(args.iter().map(|x| transform_type(x, scope)))
                    .unwrap_or(Ok(Vec::new()));
                let ret = transform_type(ret, scope);
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
        scope: &mut TempScope,
    ) -> Result<CompExpression, Vec<CompError>> {
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

    fn transform_exp(
        exp: &Expression,
        mut scope: &mut TempScope,
    ) -> Result<CompExpression, Vec<CompError>> {
        match exp {
            Expression::TypeDeclaration(_, _, _) => Ok(CompExpression::List(Vec::new())),
            Expression::InitAssign(_, _, name, _, exp, loc) => {
                if scope.variable_initialised(name) {
                    return Err(vec![CompError::RedeclareInSameScope(
                        name.clone(),
                        loc.clone(),
                    )]);
                }

                let exp = transform_exp(&exp, scope)?;
                let exp_ty = get_type_from_exp(&exp).map_err(|x| vec![x])?;
                let has_type = scope.variable_has_type(name);
                if !has_type {
                    scope = scope.set_variable_type(name, &exp_ty);
                }
                scope.set_variable_initialised(name);
                let var = scope
                    .get_variable(&name)
                    .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?;
                if has_type && !var.typing.super_of(&exp_ty) {
                    return Err(vec![CompError::InvalidAssignment(
                        var.typing,
                        exp_ty,
                        loc.clone(),
                    )]);
                }
                Ok(CompExpression::Assign(var, Box::new(exp)))
            }
            Expression::Assign(name, exp, loc) => {
                let exp = transform_exp(&exp, scope)?;
                if scope.constant_exists(name) {
                    return Err(vec![CompError::ConstReassign(name.clone(), loc.clone())]);
                }
                let var = scope
                    .get_variable(name)
                    .map_err(|_| vec![CompError::CannotFindVariable(name.clone(), loc.clone())])?;

                let exp_ty = get_type_from_exp(&exp).map_err(|err| vec![err])?;
                if var.typing.super_of(&exp_ty) {
                    Ok(CompExpression::Assign(var, Box::new(exp)))
                } else {
                    Err(vec![CompError::InvalidAssignment(
                        exp_ty,
                        var.typing,
                        loc.clone(),
                    )])
                }
            }
            Expression::IfElse(cond, left, right, _) => {
                let cond = transform_exp(&cond, scope)?;
                let then = transform_ast(left, scope)?;
                let alt = transform_ast(right, scope)?;
                let exp = CompExpression::IfElse {
                    cond: Box::new(cond),
                    then: Box::new(CompExpression::Prog(Box::new(then))),
                    otherwise: Box::new(CompExpression::Prog(Box::new(alt))),
                };
                get_type_from_exp(&exp).map_err(|x| vec![x])?;
                Ok(exp)
            }
            Expression::Loop(exp, body, _) => {
                let cond = transform_exp(&exp, scope)?;
                let body = Box::new(transform_exp(&body, scope)?);
                Ok(CompExpression::WhileLoop {
                    cond: Box::new(cond),
                    body,
                })
            }
            Expression::Block(expressions, _) => {
                collect_ok_or_err(expressions.iter().map(|exp| transform_exp(exp, scope)))
                    .unwrap_or_else(|| Ok(Vec::new()))
                    .map(CompExpression::List)
                    .map_err(|x| x.iter().flatten().cloned().collect())
            }
            Expression::LessThan(l, r, _) => bin_exp(Op::Le, l, r, scope),
            Expression::Addition(l, r, _) => bin_exp(Op::Add, l, r, scope),
            Expression::Multiplication(l, r, _) => bin_exp(Op::Mult, l, r, scope),
            Expression::Subtraction(l, r, _) => bin_exp(Op::Sub, l, r, scope),
            Expression::Division(l, r, _) => bin_exp(Op::Div, l, r, scope),
            Expression::Equal(l, r, _) => bin_exp(Op::Eq, l, r, scope),
            Expression::FuncCall(name, args, loc) => {
                let args = args
                    .iter()
                    .map(|x| transform_exp(x, scope))
                    .collect::<Result<Vec<CompExpression>, Vec<CompError>>>();
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
                    Ok(args) => Ok(CompExpression::Call(func, args)),
                    Err(message) => Err(message),
                }
            }
            Expression::Terminal(sym, loc) => match sym {
                Symbol::Identifier(name) => match scope.get_variable(name) {
                    Ok(var) => Ok(CompExpression::Read(var)),
                    Err(_) => Err(vec![CompError::CannotFindVariable(
                        name.clone(),
                        loc.clone(),
                    )]),
                },
                Symbol::Data(data) => Ok(CompExpression::Value(match data.clone() {
                    RawData::Int(val) => CompData::Int(val),
                    RawData::Float(val) => CompData::Float(val),
                    RawData::Str(val) => CompData::Str(val.clone()),
                    RawData::Bool(val) => CompData::Bool(val),
                    RawData::Null => CompData::Null,
                    RawData::Func(func) => {
                        let temp_variables = collect_ok_or_err(func.args.iter().map(|x| {
                            match transform_type(&CustomType::Union(x.1.clone()), scope) {
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
                        let return_type =
                            transform_type(&CustomType::Union(func.return_type).clone(), scope);
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
                                let mut local_scope = TempScope {
                                    parent: Some(Box::new(scope.to_comp_scope_so_far())),
                                    preset_variables: local_variables,
                                    variables: HashMap::new(),
                                    types: HashMap::new(),
                                };
                                let mut local_scope = match resolve_scope(&body, &mut local_scope) {
                                    Err(messages) => return Err(messages),
                                    Ok(scope) => scope,
                                };
                                let body = transform_ast(&body, &mut local_scope)?;

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
            Expression::Invalid(x) => panic!("invalid {:?}", x),
        }
    }

    fn resolve_scope<'a>(
        ast: &Expression,
        mut scope: &'a mut TempScope,
    ) -> Result<&'a mut TempScope, Vec<CompError>> {
        match ast {
            Expression::TypeDeclaration(name, declared_type, loc) => {
                if !scope.types.contains_key(name) {
                    Ok(scope.add_type(name.clone(), transform_type(&declared_type, scope)?))
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
                } else if *external && declared_type.is_none() {
                    Err(vec![CompError::UntypedExternal(name.clone(), loc.clone())])
                } else {
                    let typing = match declared_type {
                        None => None,
                        Some(x) => Some(transform_type(&CustomType::Union(x.to_vec()), scope)?),
                    };
                    Ok(scope.add_variable(NewVariable {
                        constant: constant.clone(),
                        name: name.clone(),
                        typing,
                        initialised: false,
                        external: external.clone(),
                    }))
                }
            }
            Expression::Assign(name, _, loc) => {
                if scope.parent.is_none() {
                    Err(vec![CompError::GlobalReassign(name.clone(), loc.clone())])
                } else if !scope.variable_exists(name) {
                    Err(vec![CompError::CannotFindVariable(
                        name.clone(),
                        loc.clone(),
                    )])
                } else if scope.constant_exists(&name) {
                    Err(vec![CompError::ConstReassign(name.clone(), loc.clone())])
                } else {
                    Ok(scope)
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
        let mut local_scope = resolve_scope(ast, &mut local_scope)?;
        let prog = transform_ast(ast, &mut local_scope)?;
        get_type_from_exp(&prog.body).map_err(|x| vec![x])?;
        Ok(prog)
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

    fn get_type_from_exp(exp: &CompExpression) -> Result<CompType, CompError> {
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
                        Err(CompError::BoolInWhile(cond, 0..0))
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
                        Err(CompError::BoolInIf(cond, 0..0))
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
                        Err(CompError::BoolInIf(cond, 0..0))
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
                        Err(CompError::InvalidAssignment(var.typing.clone(), ty, 0..0))
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
                                Some(CompError::InvalidAssignment(x.clone(), y.clone(), 0..0))
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
                    Err(CompError::NonfunctionCall(var.name, var.typing, 0..0))
                }
            }
        }
    }
}
