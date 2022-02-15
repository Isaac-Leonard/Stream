mod ast;
mod compile;
mod errors;
mod parser;
mod settings;
mod shared;
use ast::ast::*;
use chumsky::Parser;
use parser::parser::*;
use settings::settings::Settings;
use shared::shared::*;
use std::{collections::HashMap, env, fs};
fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let newlines_positions = src.split("\n").map(|x| x.len()).collect::<Vec<_>>();
    let mut positions = vec![0];
    let mut last = 0 as i32;
    for pos in newlines_positions {
        last += pos as i32 + 1;
        positions.push(last);
    }
    println!("{:?}", positions.clone());
    let settings = Settings {
        print_llvm: env::args().nth(2).is_some(),
    };
    // let src = "[!]+";
    let mut types = HashMap::new();
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Float".to_string(), CompType::Float);
    types.insert("Ptr".to_string(), CompType::Ptr);
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
            let prog = create_program(&Expression::Block(ast, 0..0), &global_scope);
            match prog {
                Ok(prog) => compile::compile::compile(&prog, settings),
                Err(messages) => messages
                    .into_iter()
                    .for_each(|e| println!("{}", e.get_msg(&positions))),
            };
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}
