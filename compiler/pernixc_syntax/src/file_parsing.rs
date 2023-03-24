use core::str;
use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

use getset::Getters;
use pernixc_common::source_file::{SourceFile, SourceFileLoadError, Span, SpanEnding};
use pernixc_lexical::{errors::LexicalError, token_stream::TokenStream};
use thiserror::Error;

use crate::{
    errors::SyntacticError,
    syntax_tree::{item::ItemSyntaxTree, FileSyntaxTree},
};

#[derive(Debug, Error)]
pub enum FileParsingError {
    #[error("{0}")]
    Lexical(#[from] LexicalError),

    #[error("{0}")]
    Syntax(#[from] SyntacticError),

    #[error("Module declaration is duplicated in the same file.")]
    ModuleDeclarationDuplicate(Span),

    #[error("{0}")]
    SourceFileLoadError(#[from] SourceFileLoadError),
}

#[derive(Debug, Getters)]
pub struct FileParsing {
    #[get = "pub"]
    source_file:         Arc<SourceFile>,
    #[get = "pub"]
    syntax_tree:         FileSyntaxTree,
    #[get = "pub"]
    file_parsing_errors: Vec<FileParsingError>,
}

impl FileParsing {
    pub fn destruct(self) -> (Arc<SourceFile>, FileSyntaxTree, Vec<FileParsingError>) {
        (self.source_file, self.syntax_tree, self.file_parsing_errors)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error)]
#[error("The given source file argument is not a root source file.")]
pub struct TargetParseError;

fn span_substr(source_file: &SourceFile, span: Span) -> &str {
    match span.end {
        SpanEnding::Location(end_location) => {
            &source_file.content()[span.start.byte..end_location.byte]
        }
        SpanEnding::EndOfFile => &source_file.content()[span.start.byte..],
    }
}

fn compile_syntax_tree(source_file: SourceFile, results: Arc<Mutex<Vec<FileParsing>>>) {
    let mut errors = Vec::new();

    // lexical analysis
    let (token_stream, lexical_errors) = TokenStream::tokenize(source_file.iter());
    errors.extend(lexical_errors.into_iter().map(FileParsingError::Lexical));

    // syntactic analysis
    let (file_syntax_tree, syntactic_errors) = FileSyntaxTree::parse(&token_stream);
    errors.extend(syntactic_errors.into_iter().map(FileParsingError::Syntax));

    let mut submodules = HashSet::new();

    for item in &file_syntax_tree.items {
        // look for all module declaration symbol in the source file.
        let module_declaration = if let ItemSyntaxTree::Module(module) = item {
            module
        } else {
            continue;
        };

        // check if the module declaration symbol is duplicated.
        if !submodules.insert(span_substr(
            &source_file,
            module_declaration.identifier.span,
        )) {
            errors.push(FileParsingError::ModuleDeclarationDuplicate(
                module_declaration.identifier.span,
            ));
        }
    }

    // start compiling the sub modules
    let mut join_handles = Vec::new();
    for module in submodules {
        let path = if source_file.is_root() {
            // check within the same directory of this source file

            // the directory of this source file
            let mut directory = match source_file.path_input().parent() {
                Some(directory) => std::path::PathBuf::from(directory),
                None => {
                    // this source file is in the root directory
                    std::path::PathBuf::new()
                }
            };

            // push the module name with the .pnx extension to the directory pathbuf
            directory.push(format!("{}.pnx", module));

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
            directory.push(format!("{}.pnx", module));

            directory
        };

        // create a new module heirarchy
        let mut module_heirarchy = source_file.module_heirarchy().clone();
        module_heirarchy.push(module.to_string());

        match SourceFile::load(path, module_heirarchy) {
            Ok(source_file) => {
                let modules = Arc::clone(&results);

                join_handles.push(std::thread::spawn(move || {
                    compile_syntax_tree(source_file, modules)
                }));
            }
            Err(error) => {
                errors.push(FileParsingError::SourceFileLoadError(error));
            }
        }
    }

    for join_handle in join_handles {
        join_handle.join().unwrap();
    }

    results.lock().unwrap().push(FileParsing {
        source_file:         Arc::new(source_file),
        syntax_tree:         file_syntax_tree,
        file_parsing_errors: errors,
    });
}

pub fn parse_files(root_source_file: SourceFile) -> Result<Vec<FileParsing>, TargetParseError> {
    if !root_source_file.is_root() {
        return Err(TargetParseError);
    }

    let results = Arc::new(Mutex::new(Vec::new()));

    compile_syntax_tree(root_source_file, results.clone());

    let syntax_trees = Arc::try_unwrap(results)
        .expect("should be the only reference")
        .into_inner()
        .expect("should be able to lock the mutex");

    Ok(syntax_trees)
}

#[cfg(test)]
mod tests;
