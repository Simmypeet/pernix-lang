//! Contains the main `run()` function for the compiler.

use std::{process::ExitCode, sync::Arc};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::termcolor::WriteColor,
};
use pernixc_bootstrap::arguments::{get_invocation_arguments, Arguments};
use pernixc_hash::{DashMap, HashMap};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::{
    runtime::persistence::serde::{
        DynamicDeserialize, DynamicRegistry, DynamicSerialize,
    },
    Key,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::{ByteIndex, GlobalSourceID, Location, SourceMap};
use pernixc_stable_type_id::StableTypeID;
use pernixc_target::TargetID;

pub mod term;

struct ReportTerm<'a> {
    config: codespan_reporting::term::Config,
    err_writer: &'a mut dyn WriteColor,
}

impl ReportTerm<'_> {
    fn report(
        &mut self,
        source_map: &mut SourceMap,
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
    diagostic: pernixc_diagnostic::Diagnostic<ByteIndex>,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagostic.severity {
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
    .with_message(diagostic.message);

    if let Some((span, label)) = diagostic.span {
        result = result.with_labels(
            std::iter::once({
                let mut primary = Label::primary(span.source_id, span.range());

                if let Some(label_message) = label {
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
    }

    if let Some(msg) = diagostic.help_message {
        result.with_notes(vec![msg])
    } else {
        result
    }
}

fn rel_pernix_diagnostic_to_codespan_diagnostic(
    diagostic: pernixc_diagnostic::Diagnostic<RelativeLocation>,
    source_map: &SourceMap,
    token_trees_by_source_id: &DashMap<
        GlobalSourceID,
        pernixc_lexical::tree::Tree,
    >,
) -> codespan_reporting::diagnostic::Diagnostic<GlobalSourceID> {
    let mut result = match diagostic.severity {
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
    .with_message(diagostic.message);

    if let Some((span, label)) = diagostic.span {
        result = result.with_labels(
            std::iter::once({
                let token_tree = token_trees_by_source_id
                    .get(&span.source_id)
                    .expect("Source ID not found in token trees");

                let source_file = source_map.get(span.source_id).unwrap();
                let begin =
                    span.start.to_absolute_index(&source_file, &token_tree);
                let end = span.end.to_absolute_index(&source_file, &token_tree);

                let mut primary = Label::primary(span.source_id, begin..end);

                if let Some(label_message) = label {
                    primary = primary.with_message(label_message);
                }

                primary
            })
            .chain(diagostic.related.into_iter().map(|x| {
                let token_tree = token_trees_by_source_id
                    .get(&x.span.source_id)
                    .expect("Source ID not found in token trees");

                let source_file = source_map.get(x.span.source_id).unwrap();

                let begin =
                    x.span.start.to_absolute_index(&source_file, &token_tree);
                let end =
                    x.span.end.to_absolute_index(&source_file, &token_tree);

                Label::secondary(x.span.source_id, begin..end)
                    .with_message(x.message)
            }))
            .collect(),
        );
    }

    if let Some(msg) = diagostic.help_message {
        result.with_notes(vec![msg])
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
pub fn run(
    argument: Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut serde_registry = SerdeExtension::default();
    let report_config = term::get_coonfig();
    let simple_file = codespan_reporting::files::SimpleFile::new("", "");

    pernixc_bootstrap::register_serde(&mut serde_registry);

    let mut engine = match pernixc_bootstrap::bootstrap(
        argument,
        Arc::new(serde_registry),
    ) {
        Ok(engine) => engine,
        Err(error) => {
            let diag = match error {
                pernixc_bootstrap::Error::SetupPersistence(io_error) => {
                    codespan_reporting::diagnostic::Diagnostic::error()
                        .with_message(format!(
                            "Failed to setup persistence: {io_error}"
                        ))
                }
                pernixc_bootstrap::Error::LoadIncrementalDatabase(io_error) => {
                    codespan_reporting::diagnostic::Diagnostic::error()
                        .with_message(format!(
                            "Failed to load incremental database: {io_error}"
                        ))
                }
            };

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

    let tracked_engine = engine.tracked();
    let argument = tracked_engine.get_invocation_arguments(TargetID::Local);
    let parse = engine
        .tracked()
        .query(&pernixc_bootstrap::module_tree::Key(TargetID::Local))
        .unwrap();

    let _parse = match parse {
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

    ExitCode::SUCCESS
}
