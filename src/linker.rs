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
        if os == MACOS {
            let mut command = Command::new("ld64.lld");
            if self.demangle {
                command.arg("-demangle");
            }
            command
                .arg("-lto_library")
                .arg("/usr/local/Cellar/llvm/13.0.0_2/lib/libLTO.dylib");
            if self.dynamic {
                command.arg("-dynamic");
            }
            command
                .arg("-arch")
                .arg("x86_64")
                .arg("-platform_version")
                .arg("macos")
                .arg("11.0.0")
                .arg("11.0.0")
                .arg("-syslibroot")
                .arg("/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk")
                .arg("-o")
                .arg(&self.output_file);
            for file in &self.input_files {
                command.arg(file);
            }
            command
                .arg("-lSystem")
                .arg(
                    "/usr/local/Cellar/llvm/13.0.1_1/lib/clang/13.0.1/lib/darwin/libclang_rt.osx.a",
                )
                .output()
                .map_err(|x| format!("{:?}", x))
                .map(|x| String::from_utf8(x.stderr).expect("utf8 error"))
                .and_then(|x| if x.is_empty() { Ok(()) } else { Err(x) })
        } else {
            Err(format!("Automatic linking not supported on this operating system '{}', please feel free to help contribute a patch",os ))
        }
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
