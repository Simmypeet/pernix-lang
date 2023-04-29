//! Contains the [`FileParsing`] type and its associated types and functions.

use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use derive_more::{Deref, From};
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{error::LexicalError, token_stream::TokenStream};
use pernixc_print::LogSeverity;
use pernixc_source::{LoadError, SourceFile, Span};
use pernixc_system::error_handler::ErrorHandler;
use thiserror::Error;

use crate::{
    error::SyntacticError,
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
        pernixc_print::print(
            LogSeverity::Error,
            "the module declaration is duplicated in the same file",
        );

        pernixc_print::print_source_code(&self.span, None);
        println!();
    }
}

/// Contains the errors related to branching the module heirarchy.
#[derive(Debug, Error, From)]
#[allow(missing_docs)]
pub enum ModuleError {
    #[error("Module declaration is duplicated in the same file.")]
    DuplicateModuleDeclaration(DuplicateModuleDeclaration),

    #[error("{0}")]
    LoadError(LoadError),
}

impl ModuleError {
    /// Prints the error message to the console.
    pub fn print_error(&self) {
        match self {
            Self::DuplicateModuleDeclaration(error) => error.print(),
            Self::LoadError(error) => match error {
                LoadError::IoLoadError(io_error) => match io_error.error.kind() {
                    std::io::ErrorKind::NotFound => {
                        // get the file name
                        pernixc_print::print(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` does not exist",
                                io_error.path.display()
                            )
                            .as_str(),
                        );
                    }
                    std::io::ErrorKind::PermissionDenied => {
                        pernixc_print::print(
                            LogSeverity::Error,
                            format!(
                                "the source file `{}` cannot be accessed",
                                io_error.path.display()
                            )
                            .as_str(),
                        );
                    }
                    _ => {
                        pernixc_print::print(
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
#[derive(Debug, Getters)]
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

impl FileParsing {
    /// Dissolves this structure into its components.
    #[must_use]
    pub fn dissolve(self) -> (Arc<SourceFile>, File, Option<AccessModifier>) {
        (self.source_file, self.syntax_tree, self.access_modifier)
    }
}

/// The error caused by passing a non-root source file to the [`parse_files`] function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error)]
#[error("The given source file argument is not a root source file.")]
pub struct TargetParseError;

fn compile_syntax_tree<
    'err,
    'scope,
    T: ErrorHandler<SyntacticError> + ErrorHandler<LexicalError> + ErrorHandler<ModuleError>,
>(
    source_file: Arc<SourceFile>,
    results: &Arc<RwLock<Vec<FileParsing>>>,
    access_modifier: Option<AccessModifier>,
    scope: &'scope std::thread::Scope<'err, '_>,
    errors: &'err T,
) where
    'scope: 'err,
{
    // lexical analysis
    let token_stream = TokenStream::tokenize(source_file.iter(), errors);

    // syntactic analysis
    let file_syntax_tree = File::parse(&token_stream, errors);

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
            errors.recieve(ModuleError::DuplicateModuleDeclaration(
                DuplicateModuleDeclaration {
                    span: module_declaration.identifier().span.clone(),
                },
            ));
        }
    }

    // start compiling the sub modules
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

                scope.spawn(move || {
                    compile_syntax_tree(
                        source_file,
                        &modules,
                        Some(access_modifier),
                        scope,
                        errors,
                    );
                });
            }
            Err(error) => {
                errors.recieve(ModuleError::LoadError(error));
            }
        }
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
#[derive(Debug, Getters, Deref)]
pub struct TargetParsing {
    /// Gets the list of [`FileParsing`] that were parsed from the target.
    #[get = "pub"]
    #[deref]
    file_parsings: Vec<FileParsing>,
}

impl TargetParsing {
    /// Dissolves this structure into the list of [`FileParsing`].
    #[must_use]
    pub fn dissolve(self) -> Vec<FileParsing> { self.file_parsings }
}

/// Is an enumeration of all errors that can be reported in the [`parse_target()`].
#[derive(Debug, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum AllParsingError {
    LexicalError(LexicalError),
    ModuleError(ModuleError),
    SyntacticError(SyntacticError),
}

/// Parses source files of the target from the root source file into a list of [`FileParsing`].
///
/// # Errors
/// - If the root source file is not a module.
pub fn parse_target<
    T: ErrorHandler<LexicalError> + ErrorHandler<SyntacticError> + ErrorHandler<ModuleError>,
>(
    root_source_file: Arc<SourceFile>,
    handler: &T,
) -> Result<TargetParsing, TargetParseError> {
    if !root_source_file.is_root() {
        return Err(TargetParseError);
    }

    let results = Arc::new(RwLock::new(Vec::new()));

    std::thread::scope(|scope| {
        compile_syntax_tree(root_source_file, &results, None, scope, handler);
    });

    let mut file_parsings = Arc::try_unwrap(results)
        .expect("should be the only reference")
        .into_inner()
        .expect("should be able to lock the mutex");

    // sort the file parsing by the depth of the module hierarchy (less depth first)
    file_parsings.sort_by_key(|file_parsing| file_parsing.source_file.module_hierarchy().len());

    Ok(TargetParsing { file_parsings })
}

#[cfg(test)]
mod tests;
