use crate::ast::*;
use crate::errors::CompError;
use crate::lexer;
use crate::map_vec;
use crate::parser::*;
use crate::settings::Settings;
use crate::shared::*;
use chumsky::{Parser, Stream};
use std::path::Path;
use std::{collections::HashMap, fs};
pub fn calc_lines(file: &str) -> Vec<i32> {
    let newlines_positions = file.split('\n').map(|x| x.len()).collect::<Vec<_>>();
    let mut positions = vec![0];
    let mut last = 0;
    for pos in newlines_positions {
        last += pos as i32 + 1;
        positions.push(last);
    }
    positions
}

pub fn get_global_scope() -> TempScope {
    let mut types = HashMap::new();
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Float".to_string(), CompType::Float);
    types.insert("Ptr".to_string(), CompType::Ptr);
    types.insert("Bool".to_string(), CompType::Bool);
    types.insert("Null".to_string(), CompType::Null);
    let variables = HashMap::new();
    TempScope {
        preset_variables: HashMap::new(),
        variables,
        types,
        parent: None,
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct ImportMap {
    //TODO: Change string to path
    pub file: String,
    pub depends_on: Vec<Result<ImportFrom, String>>,

    pub program: Option<Program>,
    pub ast: Option<SpannedExpression>,
    pub compiled: bool,
    pub settings: Settings,
    pub line_numbers: Vec<i32>,
    pub errors: Vec<CompError>,
}
impl ImportMap {}

pub fn resolve_path(sub_path: &str) -> Option<String> {
    Some(
        Path::new(sub_path)
            .canonicalize()
            .ok()?
            .to_str()?
            .to_string(),
    )
}

fn normalise_deps(imports: Vec<ImportFrom>) -> Vec<Result<ImportFrom, String>> {
    map_vec!(imports, |x| Ok(ImportFrom {
        imports: x.imports.clone(),
        file: resolve_path(&x.file).ok_or_else(|| x.file.clone())?
    }))
}

pub fn parse_files(
    settings: Settings,
    mut files: HashMap<String, ImportMap>,
) -> HashMap<String, ImportMap> {
    let src = fs::read_to_string(&settings.input_name).expect("Failed to read file");

    let lines = calc_lines(&src);
    let tokens = lexer::lexer()
        .parse(src.trim())
        .map_err(|x| panic!("{:?}", x))
        .unwrap();
    let len = tokens.len();
    let parsed = parser().parse(Stream::from_iter(len..len + 1, tokens.into_iter()));
    match parsed {
        Ok(ast) => {
            let imports = normalise_deps(ast.0);
            for import in imports.iter().flatten() {
                let dep_name = import.file.clone();
                if !files.contains_key(&dep_name) {
                    let sub_settings = Settings {
                        call_linker: false,
                        input_name: dep_name.clone(),
                        object_name: dep_name.replace(".bs", ".o"),
                        ..settings
                    };
                    files = parse_files(sub_settings, files);
                }
            }

            files.insert(
                settings.input_name.clone(),
                ImportMap {
                    file: settings.input_name.clone(),
                    settings,
                    line_numbers: lines,
                    depends_on: imports,
                    ast: Some(ast.1),
                    program: None,
                    compiled: false,
                    errors: Vec::new(),
                },
            );
            files
        }
        Err(errs) => {
            errs.into_iter().for_each(|e| println!("{:?}", e));
            files
        }
    }
}

pub fn transform_files(name: &str, programs: &mut HashMap<String, ImportMap>) {
    let mut global_scope = get_global_scope();
    println!("{}", name);
    println!("{:?}", programs.keys());
    for import in programs
        .get(name)
        .unwrap()
        .depends_on
        .clone()
        .iter()
        .flatten()
    {
        transform_files(&import.file, programs);
        if let Some(prog) = &programs.get(&import.file).unwrap().program {
            println!("here");
            prog.get_exported().iter().for_each(|x| {
                global_scope.variables.insert(x.name.clone(), x.clone());
            });
        }
    }
    let mut program = programs.get_mut(name).unwrap();
    let prog = create_program(
        program.ast.as_ref().unwrap(),
        &mut global_scope,
        &program.settings,
    );
    program.program = Some(prog.0);
    program.errors = prog.1;
}
