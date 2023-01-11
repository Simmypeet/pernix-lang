use clap::Parser;
use pernixc_common::{
    printing::{self, Severity},
    source_file::{SourceFile, SourceFileError},
};

/// Is the struct that represents the arguments that are passed to the compiler. It's used to start
/// the compilation process.
#[derive(Parser, Debug)]
#[clap(
    author = "Syn",
    about = "Is a compiler for the Syn programming language",
    version = "0.1.0"
)]
pub struct CompilatinonArgument {
    /// Is path to the file that is passed to the compiler.
    pub file_name: Vec<String>,

    /// Is the name of the assembly of this compilation.
    #[clap(short, long)]
    pub assembly_name: Option<String>,
}

/// Is the entry point of the compiler. Starts the compilation process.
pub fn main(compilation_argument: CompilatinonArgument) {
    // list of all source files to compile
    let mut source_files = Vec::new();

    for file_name in &compilation_argument.file_name {
        // get the absolute path of the file that is passed to the compiler.
        let absolute_path = match std::fs::canonicalize(file_name) {
            Ok(absolute_path) => absolute_path,
            Err(error) => {
                // file not found
                printing::print_message(
                    format!("file not found: {}", error).as_str(),
                    Severity::Error,
                    None,
                );
                std::process::exit(1);
            }
        };

        // create a new source file
        source_files.push({
            // create a new source file
            match SourceFile::new(absolute_path) {
                Ok(source_file) => source_file,
                Err(error) => {
                    match error {
                        // invalid module level
                        SourceFileError::InvalidModuleLevel => {
                            printing::print_message(
                                "invalid file/assembly name",
                                Severity::Error,
                                None,
                            );
                        }
                        // I/O error
                        SourceFileError::IoError(error) => {
                            printing::print_message(
                                format!("I/O error: {}", error).as_str(),
                                Severity::Error,
                                None,
                            );
                        }
                        // empty path
                        SourceFileError::EmptyPathBuf => {
                            printing::print_message("empty path", Severity::Error, None);
                        }
                    }
                    std::process::exit(1);
                }
            }
        });
    }
}
