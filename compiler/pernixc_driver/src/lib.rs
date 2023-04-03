use std::path::PathBuf;

use pernixc_common::source_file::SourceFile;
use pernixc_semantic::symbol::Table;
use pernixc_syntax::file_parsing::FileParsing;

#[derive(clap::Parser, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CompilationArgument {
    pub input: String,
    pub target_name: Option<String>,
}

fn late_frontend_compilation(syntax_trees: Vec<FileParsing>) {
    let result = Table::analyze(syntax_trees.into_iter());
}

pub fn compile(args: CompilationArgument) {
    let source_file =
        match SourceFile::load(PathBuf::from(args.input), vec![match args.target_name {
            Some(target_name) => target_name,
            None => "untitled".to_string(),
        }]) {
            Ok(source_file) => source_file,
            Err(..) => std::process::exit(1), // TODO: Better error handling
        };

    let syntax_trees =
        pernixc_syntax::file_parsing::parse_files(source_file).expect("should be no errors");
}

#[cfg(test)]
mod tests;
