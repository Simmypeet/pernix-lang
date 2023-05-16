//! This crate contains the main entry point of the compiler.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::sync::Mutex;

use pernixc_lexical::error::Error as LexicalError;
use pernixc_print::LogSeverity;
use pernixc_semantic::symbol::{error::Error as SymbolError, table::Table};
use pernixc_source::SourceFile;
use pernixc_syntax::{
    error::Error as SyntacticError,
    target_parsing::{ModuleError, TargetParsing},
};
use pernixc_system::error_handler::{ErrorHandler, ErrorVec};

pub mod argument;

struct Printer {
    mutex: Mutex<()>,
}

impl ErrorHandler<LexicalError> for Printer {
    fn recieve(&self, error: LexicalError) {
        let _lock_guard = self.mutex.lock();

        error.print();
    }
}

impl ErrorHandler<SyntacticError> for Printer {
    fn recieve(&self, error: SyntacticError) {
        let _lock_guard = self.mutex.lock();

        error.print();
    }
}

impl ErrorHandler<ModuleError> for Printer {
    fn recieve(&self, error: ModuleError) {
        let _lock_guard = self.mutex.lock();

        error.print();
    }
}

/// Starts a compilation process with the given compilation argument.
pub fn compile(argument: &argument::Compilation) {
    let Ok(root_source_file) =
        SourceFile::load(argument.input(), vec![argument.get_target_name()]) else {
            pernixc_print::print(LogSeverity::Error, "failed to load source file");
            std::process::exit(2);
        };

    let printer = Printer {
        mutex: Mutex::new(()),
    };

    // parses the whole target module heirarchy
    let target_parsing = TargetParsing::parse(root_source_file, &printer).unwrap();

    let error_vec: ErrorVec<SymbolError> = ErrorVec::new();
    let table = Table::analyze(target_parsing, &error_vec);
}
