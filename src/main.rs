mod compile;
mod evaluater;
mod parser;
mod shared;
mod type_check;
use chumsky::Parser;
use evaluater::evaluater::execute;
use parser::parser::*;
use shared::*;
use std::{
    cell::RefCell,
    env, fs,
    io::{self, BufRead},
    rc::Rc,
};
use type_check::type_check;
fn main() {
    let lang_print = ActiveFunction {
        args: vec![("str".into(), vec!["string".into()])],
        body: Vec::new(),
        call: |args, params, _, _, _, _| {
            if args.len() != params.len() {
                panic!("Function called with invalid parameters {:?}", params)
            }
            println!("{:?}", params[0]);
            return RawData::Null;
        },
        stack: ScopeRef(Rc::new(RefCell::new(Scope {
            variables: Vec::new(),
            parent: None,
            types: Vec::new(),
        }))),
        return_type: vec!["null".to_string()],
    };
    let lang_input = ActiveFunction {
        args: Vec::new(),
        body: Vec::new(),
        call: |args, params, _, _, _, _| {
            if args.len() != params.len() {
                panic!("Function called with invalid parameters {:?}", params)
            }
            return RawData::Str(io::stdin().lock().lines().next().unwrap().unwrap());
        },
        stack: ScopeRef(Rc::new(RefCell::new(Scope {
            variables: Vec::new(),
            parent: None,
            types: Vec::new(),
        }))),
        return_type: vec!["string".to_string()],
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
                data_type: LangType::Func(
                    vec![LangType::Union(vec![
                        LangType::Str,
                        LangType::Int,
                        LangType::Bool,
                        LangType::Null,
                    ])],
                    Box::new(LangType::Null),
                ),
            },
            Variable {
                name: "getInput".to_string(),
                value: RawData::ActiveFunc(lang_input),
                data_type: LangType::Func(Vec::new(), Box::new(LangType::Str)),
            },
        ],
        types: Vec::new(),
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
            {
                compile::compile::compile(&ast);
            }
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}
