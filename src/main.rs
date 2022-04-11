mod ast;
mod compile;
mod config;
mod errors;
mod lexer;
mod linker;
mod macros;
mod parser;
mod runner;
mod settings;
mod shared;
use runner::*;
use settings::Settings;
use std::collections::HashMap;
use std::env;
fn main() {
    let name = env::args().nth(1).expect("Expected file argument");
    let args = env::args().collect::<Vec<_>>();
    let settings = Settings {
        print_llvm: args.contains(&"-p".to_string()),
        skip_optimizations: !args.contains(&"-s".to_string()),
        call_linker: true,
        input_name: name.clone(),
        object_name: name.replace(".bs", ".o"),
    };
    let mut files = parse_files(settings.clone(), HashMap::new());
    transform_files(&name, &mut files);
    if settings.call_linker {
        let mut linker = linker::Linker::new();
        for file in files {
            linker.input(&file.1.settings.object_name);
        }

        linker.output("testing");
        linker.link().map_err(|x| panic!("{}", x));
    };
}
