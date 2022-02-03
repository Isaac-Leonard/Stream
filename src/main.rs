mod compile;
mod parser;
mod shared;
use chumsky::Parser;
use parser::parser::*;
use shared::shared::*;
use std::{collections::HashMap, env, fs};
fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    // let src = "[!]+";
    let mut types = HashMap::new();
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Float".to_string(), CompType::Float);
    types.insert("Str".to_string(), CompType::Ptr);
    types.insert("Bool".to_string(), CompType::Bool);
    types.insert("Null".to_string(), CompType::Null);
    let variables = HashMap::new();
    let global_scope = CompScope {
        variables,
        types,
        parent: None,
    };
    let parsed = parser().parse(src.trim());
    match parsed {
        Ok(ast) => {
            let prog = create_program(&ast, &global_scope);
            match prog {
                Ok(prog) => compile::compile::compile(&prog),
                Err(messages) => messages.into_iter().for_each(|e| println!("{}", e)),
            };
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}
