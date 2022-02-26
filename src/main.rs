mod ast;
mod compile;
mod errors;
mod linker;
mod parser;
mod settings;
mod shared;
use ast::*;
use chumsky::Parser;
use parser::*;
use settings::Settings;
use shared::*;
use std::{collections::HashMap, env, fs};

fn calc_lines(file: &str) -> Vec<i32> {
    let newlines_positions = file.split('\n').map(|x| x.len()).collect::<Vec<_>>();
    let mut positions = vec![0];
    let mut last = 0;
    for pos in newlines_positions {
        last += pos as i32 + 1;
        positions.push(last);
    }
    positions
}

fn get_global_scope() -> CompScope {
    let mut types = HashMap::new();
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Float".to_string(), CompType::Float);
    types.insert("Ptr".to_string(), CompType::Ptr);
    types.insert("Bool".to_string(), CompType::Bool);
    types.insert("Null".to_string(), CompType::Null);
    let variables = HashMap::new();
    CompScope {
        variables,
        types,
        parent: None,
    }
}

fn compile_file(name: &str, settings: Settings) {
    let src = fs::read_to_string(name).expect("Failed to read file");

    let lines = calc_lines(&src);
    let global_scope = get_global_scope();
    let parsed = parser().parse(src.trim());
    match parsed {
        Ok(ast) => {
            let prog = create_program(&Expression::Block(ast.1, 0..0), &global_scope);
            match prog {
                Ok(prog) => compile::compile(&prog, settings),
                Err(messages) => messages
                    .into_iter()
                    .for_each(|e| println!("{}", e.get_msg(&lines))),
            };
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}

fn main() {
    let name = env::args().nth(1).expect("Expected file argument");
    let args = env::args().collect::<Vec<_>>();
    let settings = Settings {
        print_llvm: args.contains(&"-p".to_string()),
        skip_optimizations: !args.contains(&"-s".to_string()),
    };
    compile_file(&name, settings);
}
