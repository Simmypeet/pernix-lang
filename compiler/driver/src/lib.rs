//! Contains the main `run()` function for the compiler.

use std::{process::ExitCode, sync::Arc};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::termcolor::WriteColor,
};
use pernixc_file_tree::{
    errors::get_file_tree_rendered_errors, source_map::SourceMap,
};
use pernixc_hash::HashMap;
use pernixc_query::{
    runtime::persistence::{
        serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
        Persistence,
    },
    Engine, Key,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::{ByteIndex, GlobalSourceID};
use pernixc_stable_type_id::StableTypeID;
use pernixc_target::{get_invocation_arguments, Arguments, TargetID};
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

    if let Some((span, label)) = &diagnostic.span {
        result = result.with_labels(
            std::iter::once({
                let mut primary = Label::primary(span.source_id, span.range());

                if let Some(label_message) = label {
                    primary = primary.with_message(label_message);
                }

                primary
            })
            .chain(diagnostic.related.iter().map(|x| {
                Label::secondary(x.span.source_id, x.span.range())
                    .with_message(x.message.to_string())
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

/// An object for serialization support
struct SerdeExtension<S: Serializer<Self>, D: Deserializer<Self>> {
    pub registry:
        pernixc_query::runtime::persistence::serde::Registry<S, D, Self>,
}

impl<S: Serializer<Self>, D: Deserializer<Self>> Default
    for SerdeExtension<S, D>
{
    fn default() -> Self {
        Self {
            registry:
                pernixc_query::runtime::persistence::serde::Registry::default(),
        }
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicSerialize<S>
    for SerdeExtension<S, D>
{
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<
        StableTypeID,
        pernixc_query::runtime::persistence::serde::SerializationHelper<
            S,
            Self,
        >,
    > {
        self.registry.serialization_helpers_by_type_id()
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicDeserialize<D>
    for SerdeExtension<S, D>
{
    fn deserialization_helper_by_type_id(
        &self,
    ) -> &HashMap<
        StableTypeID,
        pernixc_query::runtime::persistence::serde::DeserializationHelper<
            D,
            Self,
        >,
    > {
        self.registry.deserialization_helpers_by_type_id()
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicRegistry<S, D>
    for SerdeExtension<S, D>
{
    fn register<K: Key + Serialize<S, Self> + Deserialize<D, Self>>(&mut self)
    where
        K::Value: Serialize<S, Self> + Deserialize<D, Self>,
        S::Error: Send + Sync,
    {
        self.registry.register::<K>();
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
pub fn run(
    argument: Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut serde_registry = SerdeExtension::default();
    let report_config = term::get_coonfig();
    let simple_file = codespan_reporting::files::SimpleFile::new("", "");

    // all the serialization/deserialization runtime information must be
    // registered before creating a persistence layer
    pernixc_target::register_serde(&mut serde_registry);
    pernixc_file_tree::register_serde(&mut serde_registry);
    // pernixc_symbol::register_serde(&mut serde_registry);

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
        pernixc_target::skip_persistence(persistence);
        pernixc_file_tree::skip_persistence(persistence);
        // pernixc_symbol::skip_persistence(persistence);
    }

    // final step, setup the query executors for the engine
    pernixc_file_tree::register_executors(&mut engine.runtime.executor);
    // pernixc_symbol::register_executors(&mut engine.runtime.executor);

    // set the initial input, the invocation arguments
    engine.input_session(|x| {
        x.always_reverify();

        x.set_input(
            pernixc_target::LinkKey(TargetID::Local),
            Arc::new(std::iter::empty().collect()),
        );
        x.set_input(
            pernixc_target::MapKey,
            Arc::new(
                std::iter::once((
                    argument.command.input().target_name(),
                    TargetID::Local,
                ))
                .collect(),
            ),
        );
        x.set_input(pernixc_target::Key(TargetID::Local), Arc::new(argument));
    });

    tracing::info!(
        "Starting compilation with database version: {}",
        engine.version()
    );

    // now the query can start ...

    let tracked_engine = engine.tracked();
    let argument = tracked_engine.get_invocation_arguments(TargetID::Local);
    let errors = tracked_engine.get_file_tree_rendered_errors(TargetID::Local);

    let module_tree_errors = match errors {
        Ok(parse) => parse,
        Err(error) => {
            let diag = codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(format!(
                    "Failed to load source file at `{}`: {error}",
                    argument.command.input().file.display()
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

    let mut diagnostics = Vec::new();

    let source_map = SourceMap(&tracked_engine);

    for diag in module_tree_errors.0.as_ref() {
        diagnostics.push(SortableDiagnostic(
            pernix_diagnostic_to_codespan_diagnostic(diag),
        ));
    }

    diagnostics.sort();

    for diagnostic in &diagnostics {
        let mut report_term =
            ReportTerm { config: report_config.clone(), err_writer };

        report_term.report(&source_map, &diagnostic.0);
    }

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

    if !diagnostics.is_empty() {
        let diag = codespan_reporting::diagnostic::Diagnostic::error()
            .with_message(format!(
                "Compilation aborted due to {} error(s)",
                diagnostics.len()
            ));

        codespan_reporting::term::emit(
            err_writer,
            &report_config,
            &simple_file,
            &diag,
        )
        .unwrap();
    }

    ExitCode::SUCCESS
}
