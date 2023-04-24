//! Contains the [`FileParsing`] type and its associated types and functions.

use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use derive_getters::Dissolve;
use derive_more::{Deref, From};
use getset::Getters;
use pernixc_common::{
    printing::LogSeverity,
    source_file::{LoadError, SourceFile, Span},
};
use pernixc_lexical::{errors::LexicalError, token_stream::TokenStream};
use thiserror::Error;

use crate::{
    errors::SyntacticError,
    syntax_tree::{
        item::{AccessModifier, Item},
        File,
    },
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
                LoadError::IoLoadError(io_error) => match io_error.error.kind() {
                    std::io::ErrorKind::NotFound => {
                        // get the file name
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` does not exist",
                                io_error.path.display()
                            )
                            .as_str(),
                        );
                    }
                    std::io::ErrorKind::PermissionDenied => {
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` cannot be accessed",
                                io_error.path.display()
                            )
                            .as_str(),
                        );
                    }
                    _ => {
                        pernixc_common::printing::log(
                            LogSeverity::Error,
                            format!(
                                "an error occurred while loading the source file `{}`",
                                io_error.path.display()
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
#[derive(Debug, Getters, Dissolve)]
pub struct FileParsing {
    /// Gets the source file that was parsed.
    #[get = "pub"]
    source_file: Arc<SourceFile>,

    /// Gets the syntax tree resulting from the parsing of the source file.
    #[get = "pub"]
    syntax_tree: File,

    /// Gets the access modifier of this module. If `None`, then the module is a root module and
    /// has an implicit `public` access modifier.
    #[get = "pub"]
    access_modifier: Option<AccessModifier>,
}

/// The error caused by passing a non-root source file to the [`parse_files`] function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error)]
#[error("The given source file argument is not a root source file.")]
pub struct TargetParseError;

fn compile_syntax_tree(
    source_file: Arc<SourceFile>,
    results: &Arc<RwLock<Vec<FileParsing>>>,
    errors: &Arc<RwLock<Vec<ParsingError>>>,
    access_modifier: Option<AccessModifier>,
) {
    // lexical analysis
    let (token_stream, lexical_errors) = TokenStream::tokenize(source_file.iter());
    errors
        .write()
        .unwrap()
        .extend(lexical_errors.into_iter().map(ParsingError::LexicalError));

    // syntactic analysis
    let (file_syntax_tree, syntactic_errors) = File::parse(&token_stream);
    errors.write().unwrap().extend(
        syntactic_errors
            .into_iter()
            .map(ParsingError::SyntacticError),
    );

    let mut submodules = HashMap::new();

    for item in file_syntax_tree.items() {
        // look for all module declaration symbol in the source file.
        let Item::Module(module_declaration) = item else {
            continue;
        };

        // check if the module declaration symbol is duplicated.
        if submodules
            .insert(
                module_declaration.identifier().span.str(),
                module_declaration.access_modifier().clone(),
            )
            .is_some()
        {
            errors.write().unwrap().push(
                DuplicateModuleDeclaration {
                    span: module_declaration.identifier().span.clone(),
                }
                .into(),
            );
        }
    }

    // start compiling the sub modules
    let mut join_handles = Vec::new();
    for (module, access_modifier) in submodules {
        let mut path = source_file.parent_directory().clone();
        if !source_file.is_root() {
            path.push(source_file.file_name());
        }
        path.push(format!("{module}.pnx"));

        // create a new module hierarchy
        let mut module_hierarchy = source_file.module_hierarchy().clone();
        module_hierarchy.push(module.to_string());

        match SourceFile::load(&path, module_hierarchy) {
            Ok(source_file) => {
                let modules = Arc::clone(results);
                let errors = Arc::clone(errors);

                join_handles.push(std::thread::spawn(move || {
                    compile_syntax_tree(source_file, &modules, &errors, Some(access_modifier));
                }));
            }
            Err(error) => {
                errors.write().unwrap().push(ParsingError::LoadError(error));
            }
        }
    }

    for join_handle in join_handles {
        join_handle.join().unwrap();
    }

    results.write().unwrap().push(FileParsing {
        source_file,
        syntax_tree: file_syntax_tree,
        access_modifier,
    });
}

/// Is a structure containing the result of parsing the whole target.
///
/// The target is the collection of source files that were stemmed from the root source file through
/// the module declaration. These source files are then parsed into a list of [`FileParsing`], which
/// are then stored in this structure.
///
/// The order of the [`FileParsing`] in this structure is sorted by the depth of the module
/// hierarchy of the source file. The source file with less depth is placed before the source file
/// with more depth in the list. However, the sorting between source files with the same depth is
/// undefined.
#[derive(Debug, Getters, Deref, Dissolve)]
pub struct TargetParsing {
    /// Gets the list of [`FileParsing`] that were parsed from the target.
    #[get = "pub"]
    #[deref]
    file_parsings: Vec<FileParsing>,
}

/// Parses source files of the target from the root source file into a list of [`FileParsing`].
///
/// # Errors
/// - [`TargetParseError`] if the given source file is not a root source file.
pub fn parse_target(
    root_source_file: Arc<SourceFile>,
) -> Result<(TargetParsing, Vec<ParsingError>), TargetParseError> {
    if !root_source_file.is_root() {
        return Err(TargetParseError);
    }

    let results = Arc::new(RwLock::new(Vec::new()));
    let errors = Arc::new(RwLock::new(Vec::new()));

    compile_syntax_tree(root_source_file, &results, &errors, None);

    let mut file_parsings = Arc::try_unwrap(results)
        .expect("should be the only reference")
        .into_inner()
        .expect("should be able to lock the mutex");
    let errors = Arc::try_unwrap(errors)
        .expect("should be the only reference")
        .into_inner()
        .expect("should be able to lock the mutex");

    // sort the file parsing by the depth of the module hierarchy (less depth first)
    file_parsings.sort_by_key(|file_parsing| file_parsing.source_file.module_hierarchy().len());

    Ok((TargetParsing { file_parsings }, errors))
}

#[cfg(test)]
mod tests;
