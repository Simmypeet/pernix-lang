use clap::Parser;
use pernixc_common::{printing::Severity, source_file::SourceFile};
use pernixc_lexical_analysis::token_stream::TokenStream;
use pernixc_syntactic_analysis::abstract_syntax_tree::FileAST;

#[derive(Parser)]
#[clap(
    author = "Pernix",
    about = "Is a compiler for the Pernix programming language",
    version = "0.1.0"
)]
struct CompileArg {
    /// List of files to compile
    file: Vec<String>,
}

fn main() {
    let args = CompileArg::parse();

    if args.file.is_empty() {
        pernixc_common::printing::print_message("no input files!", Severity::Error, None);
        std::process::exit(1);
    }

    // get the source codes from the arguments
    let source_files = {
        let mut source_files = Vec::new();
        let mut found_error = false;

        for file in args.file {
            // get the file name from the path
            let file_name = match file.split('/').last() {
                Some(file_name) => file_name,
                None => {
                    pernixc_common::printing::print_message(
                        format!("`{}` is not a valid path", &file).as_str(),
                        Severity::Error,
                        None,
                    );
                    found_error = true;
                    continue;
                }
            };

            // check if the file exists
            let source_code = match std::fs::read_to_string(&file) {
                Ok(source_code) => source_code,
                Err(_) => {
                    pernixc_common::printing::print_message(
                        format!("`{}` does not exist", &file).as_str(),
                        Severity::Error,
                        None,
                    );
                    continue;
                }
            };

            source_files.push(SourceFile::new(source_code, file_name.to_string()));
        }

        if found_error {
            std::process::exit(1);
        } else {
            if source_files.is_empty() {
                pernixc_common::printing::print_message("no input files!", Severity::Error, None);
                std::process::exit(1);
            }

            source_files
        }
    };

    // lexical analysis phase
    let token_streams = std::thread::scope(|s| {
        let mut handles = Vec::new();

        // create a thread for each source file
        for source_file in &source_files {
            handles.push(s.spawn(|| TokenStream::tokenize(source_file)));
        }

        // token streams of the source files
        let mut token_streams = Vec::new();

        // join the threads
        for handle in handles {
            let result = handle.join().unwrap();

            // print the errors
            for error in result.1 {
                error.print_error(result.0.source_file());
            }

            // add the token stream to the vector
            token_streams.push(result.0);
        }

        token_streams
    });

    // syntactic analysis phase
    let abstract_syntax_trees = std::thread::scope(|s| {
        let mut handles = Vec::new();

        // create a thread for each token stream
        for token_stream in &token_streams {
            handles.push(s.spawn(|| FileAST::parse(token_stream)));
        }

        // abstract syntax trees of the source files
        let mut abstract_syntax_trees = Vec::new();

        // join the threads
        for handle in handles {
            let result = handle.join().unwrap();

            // print the errors
            for error in result.1 {
                error.print_error(result.0.source_file());
            }

            // add the abstract syntax tree to the vector
            abstract_syntax_trees.push(result.0);
        }

        abstract_syntax_trees
    });
}
