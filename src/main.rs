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
    let entry_name = env::args().nth(1).expect("Expected file argument");
    let name = resolve_path(&entry_name);
    let args = env::args().collect::<Vec<_>>();
    let name = if let Some(name) = name {
        name
    } else {
        eprintln!("The file '{}' does not exist", entry_name);
        return;
    };
    let settings = Settings {
        print_llvm: args.contains(&"-p".to_string()),
        skip_optimizations: !args.contains(&"-s".to_string()),
        call_linker: true,
        input_name: name.clone(),
        object_name: name.replace(".riv", ".o"),
    };
    let mut files = parse_files(settings.clone(), HashMap::new());
    transform_files(&name, &mut files);
    for prog in &files {
        if prog.1.errors.is_empty() {
            crate::compile::compile(prog.1.program.as_ref().unwrap(), prog.1.settings.clone());
        } else {
            prog.1
                .errors
                .iter()
                .for_each(|e| println!("{}", e.get_msg(&prog.1.line_numbers)));
        }
    }
    if settings.call_linker {
        let mut linker = linker::Linker::new();
        for file in files {
            linker.input(&file.1.settings.object_name);
        }

        linker.output("testing");
        linker.link().map_err(|x| panic!("{}", x));
    };
}
