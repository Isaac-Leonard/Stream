use std::process::Command;

use crate::config::{os, MACOS};

pub struct Linker {
    input_files: Vec<String>,
    output_file: String,
    dynamic: bool,
    demangle: bool,
}
impl Linker {
    pub fn link(&self) -> Result<(), String> {
        let mut command = Command::new("clang");
        command.arg("-g");
        command.arg("-o").arg(&self.output_file);
        for file in &self.input_files {
            command.arg(file);
        }
        command
            .output()
            .map_err(|x| format!("{:?}", x))
            .map(|x| String::from_utf8(x.stderr).expect("utf8 error"))
            .and_then(|x| if x.is_empty() { Ok(()) } else { Err(x) })
    }

    pub fn new() -> Self {
        Linker {
            input_files: Vec::new(),
            output_file: "testing".to_string(),
            dynamic: true,
            demangle: true,
        }
    }

    pub fn input<'a>(&'a mut self, name: &str) -> &'a Self {
        self.input_files.push(name.to_string());
        self
    }

    pub fn output<'a>(&'a mut self, name: &str) -> &'a Self {
        self.output_file = name.to_string();
        self
    }
}
