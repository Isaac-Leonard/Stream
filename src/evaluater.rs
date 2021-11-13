#[path = "shared.rs"]
mod shared;

pub mod evaluater {
    use crate::shared::*;
    use std::rc::Rc;
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
                if let RawData::ActiveFunc(y) = variables.get_variable(name).unwrap().value.clone()
                {
                    y.execute(
                        args.iter()
                            .map(|exp| eval_exp(exp, variables.get_copy(), types))
                            .collect::<Vec<_>>(),
                        types,
                        execute,
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
    fn eval_symbal(sym: &Symbol, variables: ScopeRef) -> RawData {
        use Symbol::*;
        match sym {
            Data(x) => match x {
                RawData::Func(func) => RawData::ActiveFunc(ActiveFunction::from_raw(
                    func.clone(),
                    variables.get_copy(),
                )),
                _ => x.clone(),
            },
            Identifier(name) => variables
                .get_variable(name)
                .unwrap_or_else(|| panic!("Attempted to access undeclared variable '{}'", name))
                .value
                .clone(),
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
                variables[index.clone()].value = value.clone();
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
    pub fn execute(
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
                Assign(name, exp) => {
                    last_value = set_variable(variables.get_copy(), name, exp, types)
                }
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
                func.execute(Vec::new(), types, execute);
            } else {
                panic!(
                    "Main must be a function, not a {:?}",
                    main_index.clone().clone().data_type.clone()
                )
            }
        }
        last_value
    }
}
