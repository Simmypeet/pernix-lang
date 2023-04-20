//! Contains the [`FileParsing`] type and its associated types and functions.

use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

use derive_more::From;
use getset::Getters;
use pernixc_common::{
    printing::LogSeverity,
    source_file::{LoadError, SourceFile, Span},
};
use pernixc_lexical::{errors::LexicalError, token_stream::TokenStream};
use thiserror::Error;

use crate::{
    errors::SyntacticError,
    syntax_tree::{item::Item, File},
};

/// The error caused by encountering a duplicated module declaration in the same file.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct DuplicateModuleDeclaration {
    /// The span of the duplicated module declaration.
    #[get = "pub"]
    span: Span,
}

impl DuplicateModuleDeclaration {
    /// Prints the error message to the console.
    pub fn print(&self) {
        pernixc_common::printing::log(
            LogSeverity::Error,
            "the module declaration is duplicated in the same file",
        );

        pernixc_common::printing::print_source_code(&self.span, None);
        println!();
    }
}

/// The error encountered while parsing a source file.
#[derive(Debug, Error, From)]
#[allow(missing_docs)]
pub enum ParsingError {
    #[error("{0}")]
    LexicalError(LexicalError),

    #[error("{0}")]
    SyntacticError(SyntacticError),

    #[error("Module declaration is duplicated in the same file.")]
    DuplicateModuleDeclaration(DuplicateModuleDeclaration),

    #[error("{0}")]
    LoadError(LoadError),
}

impl ParsingError {
    /// Prints the error message to the console.
    pub fn print_error(&self) {
        match self {
            Self::LexicalError(error) => error.print(),
            Self::SyntacticError(error) => error.print(),
            Self::DuplicateModuleDeclaration(error) => error.print(),
            Self::LoadError(error) => match error {
                LoadError::IoLoadError(io_error) => match io_error.error().kind() {
                    std::io::ErrorKind::NotFound => {
                        // get the file name
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` does not exist",
                                io_error.path().display()
                            )
                            .as_str(),
                        );
                    }
                    std::io::ErrorKind::PermissionDenied => {
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` cannot be accessed",
                                io_error.path().display()
                            )
                            .as_str(),
                        );
                    }
                    _ => {
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "an error occurred while loading the source file `{}`",
                                io_error.path().display()
                            )
                            .as_str(),
                        );
                    }
                },
                LoadError::CreateError(_) => panic!("ICE: invalid argument input"),
            },
        }
    }
}

/// Is a structure containing the result of parsing the whole source file.
#[derive(Debug, Getters)]
pub struct FileParsing {
    /// Gets the source file that was parsed.
    #[get = "pub"]
    source_file: Arc<SourceFile>,

    /// Gets the syntax tree resulting from the parsing of the source file.
    #[get = "pub"]
    syntax_tree: File,

    /// Gets the list of errors encountered while parsing the source file.
    #[get = "pub"]
    file_parsing_errors: Vec<ParsingError>,
}

impl FileParsing {
    /// Destructs this [`FileParsing`] into its components.
    #[must_use]
    pub fn destruct(self) -> (Arc<SourceFile>, File, Vec<ParsingError>) {
        (self.source_file, self.syntax_tree, self.file_parsing_errors)
    }
}

/// The error caused by passing a non-root source file to the [`parse_files`] function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error)]
#[error("The given source file argument is not a root source file.")]
pub struct TargetParseError;

fn compile_syntax_tree(source_file: Arc<SourceFile>, results: &Arc<Mutex<Vec<FileParsing>>>) {
    let mut errors = Vec::new();

    // lexical analysis
    let (token_stream, lexical_errors) = TokenStream::tokenize(source_file.iter());
    errors.extend(lexical_errors.into_iter().map(ParsingError::LexicalError));

    // syntactic analysis
    let (file_syntax_tree, syntactic_errors) = File::parse(&token_stream);
    errors.extend(
        syntactic_errors
            .into_iter()
            .map(ParsingError::SyntacticError),
    );

    let mut submodules = HashSet::new();

    for item in &file_syntax_tree.items {
        // look for all module declaration symbol in the source file.
        let Item::Module(module_declaration) = item else {
            continue;
        };

        // check if the module declaration symbol is duplicated.
        if !submodules.insert(module_declaration.identifier().span().str()) {
            errors.push(
                DuplicateModuleDeclaration {
                    span: module_declaration.identifier().span().clone(),
                }
                .into(),
            );
        }
    }

    // start compiling the sub modules
    let mut join_handles = Vec::new();
    for module in submodules {
        let mut path = source_file.parent_directory().clone();
        if !source_file.is_root() {
            path.push(source_file.file_name());
        }
        path.push(format!("{module}.pnx"));

        // create a new module heirarchy
        let mut module_heirarchy = source_file.module_heirarchy().clone();
        module_heirarchy.push(module.to_string());

        match SourceFile::load(&path, module_heirarchy) {
            Ok(source_file) => {
                let modules = Arc::clone(results);

                join_handles.push(std::thread::spawn(move || {
                    compile_syntax_tree(source_file, &modules);
                }));
            }
            Err(error) => {
                errors.push(ParsingError::LoadError(error));
            }
        }
    }

    for join_handle in join_handles {
        join_handle.join().unwrap();
    }

    results.lock().unwrap().push(FileParsing {
        source_file,
        syntax_tree: file_syntax_tree,
        file_parsing_errors: errors,
    });
}

/// Is a structure containing the result of parsing the whole target.
///
/// The target is the collection of source files that were stemmed from the root source file through
/// the module declaration. These source files are then parsed into a list of [`FileParsing`], which
/// are then stored in this structure.
#[derive(Debug, Getters)]
pub struct TargetParsing {
    /// Gets the list of [`FileParsing`] that were parsed from the target.
    #[get = "pub"]
    file_parsings: Vec<FileParsing>,
}

impl TargetParsing {
    /// Destructs this [`TargetParsing`] into its components.
    #[must_use]
    pub fn destruct(self) -> Vec<FileParsing> { self.file_parsings }
}

/// Parses source files of the target from the root source file into a list of [`FileParsing`].
///
/// # Errors
/// - [`TargetParseError`] if the given source file is not a root source file.
pub fn parse_files(
    root_source_file: Arc<SourceFile>,
) -> Result<Vec<FileParsing>, TargetParseError> {
    if !root_source_file.is_root() {
        return Err(TargetParseError);
    }

    let results = Arc::new(Mutex::new(Vec::new()));

    compile_syntax_tree(root_source_file, &results);

    let syntax_trees = Arc::try_unwrap(results)
        .expect("should be the only reference")
        .into_inner()
        .expect("should be able to lock the mutex");

    Ok(syntax_trees)
}

#[cfg(test)]
mod tests;
