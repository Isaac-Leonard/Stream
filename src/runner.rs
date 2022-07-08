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

pub fn get_global_scope() -> Scope {
    let mut types = HashMap::new();
    types.insert("Char".to_string(), CompType::Char);
    types.insert("Int".to_string(), CompType::Int);
    types.insert("Float".to_string(), CompType::Float);
    types.insert("Ptr".to_string(), CompType::Ptr);
    types.insert("Bool".to_string(), CompType::Bool);
    types.insert("Null".to_string(), CompType::Null);
    types.insert(
        "Str".to_string(),
        CompType::Str(CompType::Generic(0, CompType::Int.boxed()).boxed()),
    );
    let variables = HashMap::new();
    Scope {
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
    pub text: Option<String>,
    pub program: Option<Program>,
    pub ast: Option<SpannedExpression>,
    pub compiled: bool,
    pub settings: Settings,
    pub line_numbers: Vec<i32>,
    pub errors: Vec<CompError>,
}
impl ImportMap {
    pub fn new(settings: Settings) -> ImportMap {
        ImportMap {
            file: settings.input_name.clone(),
            settings,
            text: None,
            ast: None,
            program: None,
            compiled: false,
            depends_on: Vec::new(),
            line_numbers: Vec::new(),
            errors: Vec::new(),
        }
    }
    pub fn text(self, text: String) -> Self {
        ImportMap {
            line_numbers: calc_lines(&text),
            text: Some(text),
            ..self
        }
    }
}

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
    let src = fs::read_to_string(&settings.input_name).ok();
    let src = if let Some(src) = src {
        src
    } else {
        return files;
    };
    let tokens = lexer::lexer()
        .parse(src.clone())
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
                        object_name: dep_name.replace(".srm", ".o"),
                        ..settings
                    };
                    files = parse_files(sub_settings, files);
                }
            }
            let lines = calc_lines(&src);
            files.insert(
                settings.input_name.clone(),
                ImportMap {
                    file: settings.input_name.clone(),
                    settings,
                    line_numbers: lines,
                    text: Some(src),
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
    let mut import_errors = Vec::new();
    if let Some(program) = programs.get(name) {
        for import in program.depends_on.clone() {
            if let Ok(import) = import {
                transform_files(&import.file, programs);
                if let Some(prog) = programs.get(&import.file)  && let Some(ref prog)=&prog.program {
                    if let Import::All(None) = import.imports {
                        prog.get_exported().iter().for_each(|x| {
                            global_scope.variables.insert(x.get_name(), x.clone());
                        });
                    } else if let Import::Specific(imports) = import.imports {
                        prog.get_exported().iter().for_each(|x| {
                            if imports.iter().any(|y| y.0 == x.get_name()) {
                                global_scope.variables.insert(x.get_name(), x.clone());
                            }
                        });
                    } else {
                        panic!("Renaming imports is currently not supported, sorry")
                    }
                } else {
                    import_errors.push(CompError::ModuleNotFound(import.file.clone(), 0..0))
                }
            } else if let Err(name) = import {
                import_errors.push(CompError::ModuleNotFound(name.clone(), 0..0))
            }
        }
    }

    // Have to drop the borrow and get a mutable one
    if let Some(program) = programs.get_mut(name) {
        let mut prog = create_program(
            program.ast.as_ref().unwrap(),
            &mut global_scope,
            &program.settings,
        );
        program.program = Some(prog.0);
        program.errors = import_errors;
        program.errors.append(&mut prog.1);
    }
}
