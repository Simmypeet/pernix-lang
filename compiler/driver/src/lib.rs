//! Contains the main `run()` function for the compiler.

use std::{process::ExitCode, sync::Arc};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::termcolor::WriteColor,
};
use pernixc_diagnostic::Highlight;
use pernixc_query::{runtime::persistence::Persistence, Engine};
use pernixc_source_file::{ByteIndex, GlobalSourceID};
use pernixc_symbol::source_map::{create_source_map, SourceMap};
use pernixc_target::{Arguments, TargetID};
use tracing::instrument;

pub mod term;

struct ReportTerm<'a> {
    config: codespan_reporting::term::Config,
    err_writer: &'a mut dyn WriteColor,
}

impl ReportTerm<'_> {
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn report(
        &mut self,
        source_map: &SourceMap,
        diagnostic: &Diagnostic<GlobalSourceID>,
    ) {
        let _ = codespan_reporting::term::emit(
            self.err_writer,
            &self.config,
            source_map,
            diagnostic,
        );
    }
}

fn pernix_diagnostic_to_codespan_diagnostic(
    diagnostic: &pernixc_diagnostic::Diagnostic<ByteIndex>,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagnostic.severity {
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
    .with_message(diagnostic.message.to_string());

    if let Some(Highlight { span, message }) = &diagnostic.primary_highlight {
        result = result.with_labels(
            std::iter::once({
                let mut primary = Label::primary(span.source_id, span.range());

                if let Some(label_message) = message {
                    primary = primary.with_message(label_message);
                }

                primary
            })
            .chain(diagnostic.related.iter().map(|x| {
                let mut label =
                    Label::secondary(x.span.source_id, x.span.range());

                if let Some(message) = x.message.as_ref() {
                    label = label.with_message(message.clone());
                }

                label
            }))
            .collect(),
        );
    }

    if let Some(msg) = &diagnostic.help_message {
        result.with_notes(vec![msg.to_string()])
    } else {
        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SortableDiagnostic(Diagnostic<GlobalSourceID>);

#[allow(clippy::trivially_copy_pass_by_ref)]
const fn cmp_label_style(a: &LabelStyle, b: &LabelStyle) -> std::cmp::Ordering {
    match (a, b) {
        (LabelStyle::Primary, LabelStyle::Primary) => std::cmp::Ordering::Equal,
        (LabelStyle::Primary, LabelStyle::Secondary) => {
            std::cmp::Ordering::Less
        }
        (LabelStyle::Secondary, LabelStyle::Primary) => {
            std::cmp::Ordering::Greater
        }
        (LabelStyle::Secondary, LabelStyle::Secondary) => {
            std::cmp::Ordering::Equal
        }
    }
}

fn cmp_label(
    a: &Label<GlobalSourceID>,
    b: &Label<GlobalSourceID>,
) -> std::cmp::Ordering {
    cmp_label_style(&a.style, &b.style)
        .then_with(|| a.file_id.cmp(&b.file_id))
        .then_with(|| a.range.start.cmp(&b.range.start))
        .then_with(|| a.range.end.cmp(&b.range.end))
        .then_with(|| a.message.cmp(&b.message))
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ComparableLabel<'a>(pub &'a Label<GlobalSourceID>);

impl PartialOrd for ComparableLabel<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ComparableLabel<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        cmp_label(self.0, other.0)
    }
}

impl PartialOrd for SortableDiagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SortableDiagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .severity
            .cmp(&other.0.severity)
            .then_with(|| {
                self.0
                    .labels
                    .iter()
                    .map(ComparableLabel)
                    .cmp(other.0.labels.iter().map(ComparableLabel))
            })
            .then_with(|| self.0.message.cmp(&other.0.message))
    }
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines, clippy::needless_pass_by_value)]
#[instrument(skip(err_writer, _out_writer))]
pub async fn run(
    argument: Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut serde_registry = pernixc_register::SerdeRegistry::default();

    let report_config = term::get_coonfig();
    let simple_file = codespan_reporting::files::SimpleFile::new("", "");

    // all the serialization/deserialization runtime information must be
    // registered before creating a persistence layer
    pernixc_register::Registration::register_serde_registry(
        &mut serde_registry,
    );
    pernixc_source_file::register_serde(&mut serde_registry);
    pernixc_lexical::register_serde(&mut serde_registry);
    pernixc_syntax::register_serde(&mut serde_registry);
    pernixc_type_system::register_serde(&mut serde_registry);
    pernixc_resolution::register_serde(&mut serde_registry);
    pernixc_semantic_element_impl::register_serde(&mut serde_registry);

    let mut engine = Engine::default();

    // setup the persistence layer if the incremental setting is set
    if let Some(incremental_path) = &argument.command.input().incremental_path {
        let persistence = match Persistence::new(
            incremental_path.clone(),
            Arc::new(serde_registry),
        ) {
            Ok(persistence) => persistence,
            Err(error) => {
                let diag = Diagnostic::error().with_message(format!(
                    "Failed to setup persistence: {error}"
                ));

                codespan_reporting::term::emit(
                    err_writer,
                    &report_config,
                    &simple_file,
                    &diag,
                )
                .unwrap();

                return ExitCode::FAILURE;
            }
        };

        // load the database from the persistence layer
        engine.database = match persistence.load_database() {
            Ok(database) => database,
            Err(error) => {
                let diag = Diagnostic::error().with_message(format!(
                    "Failed to load incremental database: {error}"
                ));

                codespan_reporting::term::emit(
                    err_writer,
                    &report_config,
                    &simple_file,
                    &diag,
                )
                .unwrap();

                return ExitCode::FAILURE;
            }
        };

        engine.runtime.persistence = Some(persistence);
    }

    // if persistence is set, setup the value that will be skipped from saving
    // into the persistence layer.
    if let Some(persistence) = engine.runtime.persistence.as_mut() {
        pernixc_register::Registration::register_skip_persistence(persistence);
        pernixc_source_file::skip_persistence(persistence);
        pernixc_lexical::skip_persistence(persistence);
        pernixc_syntax::skip_persistence(persistence);
        pernixc_type_system::skip_persistence(persistence);
        pernixc_resolution::skip_persistence(persistence);
        pernixc_semantic_element_impl::skip_persistence(persistence);
    }

    // final step, setup the query executors for the engine
    pernixc_register::Registration::register_executor(
        &mut engine.runtime.executor,
    );
    pernixc_source_file::register_executors(&mut engine.runtime.executor);
    pernixc_lexical::register_executors(&mut engine.runtime.executor);
    pernixc_syntax::register_executors(&mut engine.runtime.executor);
    pernixc_type_system::register_executors(&mut engine.runtime.executor);
    pernixc_resolution::register_executors(&mut engine.runtime.executor);
    pernixc_semantic_element_impl::register_executors(
        &mut engine.runtime.executor,
    );

    // set the initial input, the invocation arguments
    let local_target_id =
        TargetID::from_target_name(&argument.command.input().target_name());

    engine
        .input_session(async |x| {
            x.always_reverify();

            x.set_input(
                pernixc_target::LinkKey(local_target_id),
                Arc::new(std::iter::empty().collect()),
            )
            .await;

            x.set_input(
                pernixc_target::AllTargetIDsKey,
                Arc::new(std::iter::once(local_target_id).collect()),
            )
            .await;

            x.set_input(
                pernixc_target::MapKey,
                Arc::new(
                    std::iter::once((
                        argument.command.input().target_name(),
                        local_target_id,
                    ))
                    .collect(),
                ),
            )
            .await;

            if let Some(explicit_seed) = argument.command.input().target_seed {
                x.set_input(
                    pernixc_target::SeedKey(local_target_id),
                    explicit_seed,
                )
                .await;
            }

            x.set_input(
                pernixc_target::Key(local_target_id),
                Arc::new(argument),
            )
            .await;
        })
        .await;

    tracing::info!(
        "Starting compilation with database version: {}",
        engine.version()
    );

    // now the query can start ...

    let engine = Arc::new(engine);

    let diagnostic_count = {
        let tracked_engine = engine.tracked();
        let mut diagnostics = Vec::new();

        let symbol_errors = tracked_engine
            .query(&pernixc_symbol::diagnostic::RenderedKey(local_target_id))
            .await
            .unwrap();

        let term_errors = tracked_engine
            .query(&pernixc_semantic_element_impl::diagnostic::AllRenderedKey(
                local_target_id,
            ))
            .await
            .unwrap();

        for diag in symbol_errors.as_ref() {
            diagnostics.push(SortableDiagnostic(
                pernix_diagnostic_to_codespan_diagnostic(diag),
            ));
        }

        for diag in term_errors.iter().flat_map(|x| x.iter()) {
            diagnostics.push(SortableDiagnostic(
                pernix_diagnostic_to_codespan_diagnostic(diag),
            ));
        }

        diagnostics.sort();

        let source_map =
            tracked_engine.create_source_map(local_target_id).await;

        for diagnostic in &diagnostics {
            let mut report_term =
                ReportTerm { config: report_config.clone(), err_writer };

            report_term.report(&source_map, &diagnostic.0);
        }

        diagnostics.len()
    };

    if diagnostic_count != 0 {
        let diag = codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!(
                "Compilation aborted due to {diagnostic_count} error(s)"
            ));

        codespan_reporting::term::emit(
            err_writer,
            &report_config,
            &simple_file,
            &diag,
        )
        .unwrap();
    }

    let mut engine =
        Arc::try_unwrap(engine).expect("Engine should be unique at this point");

    if let Err(error) = engine.save_database() {
        let diag = codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!("Failed to save database: {error}"));

        codespan_reporting::term::emit(
            err_writer,
            &report_config,
            &simple_file,
            &diag,
        )
        .unwrap();
        return ExitCode::FAILURE;
    }

    if diagnostic_count == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
