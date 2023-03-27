//! Contains the [`FileParsing`] type and its associated types and functions.

use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

use derive_more::From;
use getset::Getters;
use pernixc_common::source_file::{LoadError, SourceFile, Span};
use pernixc_lexical::{errors::LexicalError, token_stream::TokenStream};
use thiserror::Error;

use crate::{
    errors::SyntacticError,
    syntax_tree::{item::Item, File},
};

/// The error caused by encountering a duplicated module declaration in the same file.
#[derive(Debug, Clone, Copy, Eq, PartialOrd, Ord, Hash, PartialEq)]
pub struct DuplicateModuleDeclaration {
    /// The span of the duplicated module declaration.
    pub span: Span,
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

/// Is a structure containing the result of parsing a source file.
///
/// This struct is the final output of the syntax analysis phase and is meant to be used by the
/// next stage of the compilation process.
///
/// This struct is meant to represent only a valid syntax tree. Therefore, it is not possible to
/// create an invalid syntax tree using the public API.
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

fn compile_syntax_tree(source_file: SourceFile, results: &Arc<Mutex<Vec<FileParsing>>>) {
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
        if !submodules.insert(&source_file[module_declaration.identifier.span]) {
            errors.push(
                DuplicateModuleDeclaration {
                    span: module_declaration.identifier.span,
                }
                .into(),
            );
        }
    }

    // start compiling the sub modules
    let mut join_handles = Vec::new();
    for module in submodules {
        let path = if source_file.is_root() {
            // check within the same directory of this source file

            // the directory of this source file
            let mut directory = source_file
                .path_input()
                .parent()
                .map_or_else(std::path::PathBuf::new, |directory| {
                    std::path::PathBuf::from(directory)
                });

            // push the module name with the .pnx extension to the directory pathbuf
            directory.push(format!("{module}.pnx"));

            directory
        } else {
            // the directory of the root source file
            let mut directory = source_file.path_input().parent().unwrap().join(
                source_file
                    .path_input()
                    .file_stem()
                    .expect("should have a file stem"),
            );

            // add the module name to the directory
            directory.push(format!("{module}.pnx"));

            directory
        };

        // create a new module heirarchy
        let mut module_heirarchy = source_file.module_heirarchy().clone();
        module_heirarchy.push(module.to_string());

        match SourceFile::load(path, module_heirarchy) {
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
        source_file: Arc::new(source_file),
        syntax_tree: file_syntax_tree,
        file_parsing_errors: errors,
    });
}

/// Parses source files of the target from the root source file into a list of [`FileParsing`].
///
/// # Errors
/// - [`TargetParseError`] if the given source file is not a root source file.
pub fn parse_files(root_source_file: SourceFile) -> Result<Vec<FileParsing>, TargetParseError> {
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
