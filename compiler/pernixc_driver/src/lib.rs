//! This crate combines all the functionality of the compiler into a single binary.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    missing_docs,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::path::PathBuf;

pub use clap::Parser;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::SourceFile;
use pernixc_syntax::syntax_tree::target::Target;
use pernixc_system::diagnostic::Storage;

/// Represents the arguments passed into the compiler binary.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, clap::Parser)]
pub struct Argument {
    /// The path to the target's root source file to compile.
    path: PathBuf,

    /// The path to the output file.
    ///
    /// If the output path is not specified, the output will be written into the same directory
    /// and name as the target's root source file.
    #[clap(short = 'o', long = "output")]
    output_path: Option<PathBuf>,

    /// The name of the target to compile.
    ///
    /// If the target name is not specified, the name of the target will be inferred from the name
    /// of the target's root source file.
    #[clap(short = 't', long = "target")]
    target_name: Option<String>,
}

impl Argument {
    /// Gets the target name specified in the argument.
    #[must_use]
    pub fn target_name(&self) -> &str {
        self.target_name.as_ref().map_or_else(
            || {
                self.path
                    .file_stem()
                    .expect("failed to get file stem")
                    .to_str()
                    .expect("failed to convert file stem to str")
            },
            |target_name| target_name,
        )
    }
}

#[derive(Debug, EnumAsInner, From)]
enum TargetParseError {
    Lexical(pernixc_lexical::error::Error),
    Syntactic(pernixc_syntax::error::Error),
    Target(pernixc_syntax::syntax_tree::target::Error),
}

/// Starts the full compilation process for the given compilation [`Argument`].
pub fn run(argument: &Argument) {
    let target_root_source_file = match SourceFile::load(&argument.path) {
        Ok(x) => x,
        Err(err) => {
            eprintln!("error: failed to load target root source file `{err}`");
            std::process::exit(1);
        }
    };

    let target_parse_errors: Storage<TargetParseError> = Storage::new();
    let target = Target::parse(
        target_root_source_file,
        argument.target_name().to_string(),
        &target_parse_errors,
    );

    {
        let error_vec = target_parse_errors.into_vec();
        if !error_vec.is_empty() {
            for error in &error_vec {
                match error {
                    TargetParseError::Lexical(error) => error.print(),
                    TargetParseError::Syntactic(error) => error.print(),
                    TargetParseError::Target(error) => error.print(),
                }
            }
            std::process::exit(1);
        }
    }
}
