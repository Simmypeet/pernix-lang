//! Contains the main `run()` function for the compiler.

use std::{fs::File, process::ExitCode};

use argument::Arguments;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::{self, ColorChoice, StandardStream},
};
use pernixc_arena::ID;
use pernixc_diagnostic::Report;
use pernixc_handler::Storage;
use pernixc_lexical::{error::Error, token_stream::TokenStream};
use pernixc_source_file::{AbsoluteSpan, ByteIndex, SourceFile, SourceMap};
use pernixc_target::{Global, TargetID};
use term::get_coonfig;

pub mod argument;
pub mod term;

struct ReportTerm {
    config: codespan_reporting::term::Config,
    stderr: termcolor::StandardStream,
}

impl ReportTerm {
    fn report(
        &mut self,
        source_map: &mut SourceMap,
        diagnostic: &Diagnostic<Global<ID<SourceFile>>>,
    ) {
        let mut writer = self.stderr.lock();
        let _ = codespan_reporting::term::emit(
            &mut writer,
            &self.config,
            source_map,
            diagnostic,
        );
    }
}

fn pernix_diagnostic_to_codespan_diagnostic(
    diagostic: pernixc_diagnostic::Diagnostic<
        AbsoluteSpan<Global<ID<SourceFile>>>,
    >,
) -> codespan_reporting::diagnostic::Diagnostic<Global<ID<SourceFile>>> {
    let result = match diagostic.severity {
        pernixc_diagnostic::Severity::Error => {
            codespan_reporting::diagnostic::Diagnostic::error()
        }
        pernixc_diagnostic::Severity::Warning => {
            codespan_reporting::diagnostic::Diagnostic::warning()
        }
        pernixc_diagnostic::Severity::Info => {
            codespan_reporting::diagnostic::Diagnostic::note()
        }
    }
    .with_message(diagostic.message)
    .with_labels(
        std::iter::once({
            let mut primary = Label::primary(
                diagostic.span.source_id,
                diagostic.span.range(),
            );

            if let Some(label_message) = diagostic.label {
                primary = primary.with_message(label_message);
            }

            primary
        })
        .chain(diagostic.related.into_iter().map(|x| {
            Label::secondary(x.span.source_id, x.span.range())
                .with_message(x.message)
        }))
        .collect(),
    );

    if let Some(msg) = diagostic.help_message {
        result.with_notes(vec![msg])
    } else {
        result
    }
}

fn create_root_source_file(
    argument: &Arguments,
    source_map: &mut SourceMap,
    report_term: &mut ReportTerm,
) -> Option<Global<ID<SourceFile>>> {
    let file = match File::open(&argument.command.input().file) {
        Ok(file) => file,
        Err(error) => {
            let msg = Diagnostic::error().with_message(format!(
                "{}: {error}",
                argument.command.input().file.display()
            ));
            report_term.report(source_map, &msg);

            return None;
        }
    };

    let source_file =
        match SourceFile::load(file, argument.command.input().file.clone()) {
            Ok(file) => file,
            Err(pernixc_source_file::Error::Io(error)) => {
                let msg = Diagnostic::error().with_message(format!(
                    "{}: {error}",
                    argument.command.input().file.display()
                ));
                report_term.report(source_map, &msg);

                return None;
            }
            Err(pernixc_source_file::Error::Utf8(error)) => {
                let msg = Diagnostic::error().with_message(format!(
                    "{}: {error}",
                    argument.command.input().file.display()
                ));
                report_term.report(source_map, &msg);

                return None;
            }
        };

    let id = source_map.register(TargetID::Local, source_file);

    Some(TargetID::Local.make_global(id))
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn run(argument: &Arguments) -> ExitCode {
    let mut report_term = ReportTerm {
        config: get_coonfig(),
        stderr: StandardStream::stderr(ColorChoice::Always),
    };
    let mut source_map = SourceMap::new();

    let Some(source_id) =
        create_root_source_file(argument, &mut source_map, &mut report_term)
    else {
        return ExitCode::FAILURE;
    };

    let storage = Storage::<Error<ByteIndex, Global<ID<SourceFile>>>>::new();
    let _stream = TokenStream::tokenize(
        source_map[source_id].content(),
        source_id,
        &storage,
    );

    for diag in storage.into_vec() {
        let msg = diag.report(());
        let msg = pernix_diagnostic_to_codespan_diagnostic(msg);

        report_term.report(&mut source_map, &msg);
    }

    ExitCode::SUCCESS
}
