mod compile;
mod parser;
mod shared;
use chumsky::Parser;
use parser::parser::*;
use shared::*;
use std::{
    cell::RefCell,
    collections::{hash_map, HashMap},
    env, fs,
    io::{self, BufRead},
    rc::Rc,
};
fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    // let src = "[!]+";
    let mut types = HashMap::new();
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Null".to_string(), CompType::Null);
    let mut variables = HashMap::new();
    let global_scope = CompScope {
        variables,
        types,
        parent: None,
    };
    let parsed = parser().parse(src.trim());
    match parsed {
        Ok(ast) => {
            let prog = create_program(&ast, &global_scope);
            compile::compile::compile(&prog);
        }
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}
