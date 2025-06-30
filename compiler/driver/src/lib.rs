//! Contains the main `run()` function for the compiler.

use std::{fs::File, path::PathBuf, process::ExitCode, sync::Arc};

use argument::Arguments;
use clap::Args;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle},
    term::termcolor::WriteColor,
};
use flexstr::SharedStr;
use pernixc_bootstrap::Bootstrap;
use pernixc_diagnostic::Report;
use pernixc_hash::{DashMap, HashMap};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::{
    runtime::persistence::{
        serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
        Persistence, ReadAny, WriteAny,
    },
    Key,
};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    de::Deserializer,
    ser::Serializer,
    Deserialize, Serialize,
};
use pernixc_source_file::{
    ByteIndex, GlobalSourceID, Location, SourceFile, SourceMap,
};
use pernixc_stable_type_id::StableTypeID;
use pernixc_target::TargetID;
use term::get_coonfig;

pub mod argument;
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

fn create_root_source_file(
    argument: &Arguments,
    source_map: &mut SourceMap,
    report_term: &mut ReportTerm,
) -> Option<GlobalSourceID> {
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

/// The input to the compiler.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Args)]
pub struct Input {
    /// The input file to compile.
    ///
    /// This file is the root source file of the compilation; the module will
    /// stem from this file.
    pub file: PathBuf,

    /// The name of the target; if not specified, the target name will be
    /// inferred from the file name.
    #[clap(short = 't', long = "target")]
    pub target_name: Option<String>,

    /// The paths to the `plib` library to link to the target.
    #[clap(short = 'l', long = "link")]
    pub library_paths: Vec<PathBuf>,

    /// The path to the incremental compilation data.
    #[clap(long = "inc")]
    pub incremental_path: Option<PathBuf>,

    /// Whether to show the progress of the compilation.
    #[clap(long)]
    pub show_progress: bool,
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
    let mut report_term = ReportTerm { config: get_coonfig(), err_writer };
    let mut source_map = SourceMap::new();

    let Some(root_source_id) =
        create_root_source_file(&argument, &mut source_map, &mut report_term)
    else {
        return ExitCode::FAILURE;
    };

    let target_name: SharedStr =
        argument.command.input().target_name.clone().map_or_else(
            || {
                source_map
                    .get(root_source_id)
                    .unwrap()
                    .full_path()
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .to_string()
                    .into()
            },
            Into::into,
        );

    // check if the incremental path is a directory and create it if it does not
    // exist
    if let Some(incremental_path) = &argument.command.input().incremental_path {
        let exists = incremental_path.exists();

        if exists && incremental_path.is_file() {
            let msg = Diagnostic::error().with_message(format!(
                "Incremental path `{}` is a file, not a directory.",
                incremental_path.display()
            ));
            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }

        if !exists {
            if let Err(error) = std::fs::create_dir_all(incremental_path) {
                let msg = Diagnostic::error().with_message(format!(
                    "Failed to create incremental directory `{}`: {error}",
                    incremental_path.display()
                ));
                report_term.report(&mut source_map, &msg);
                return ExitCode::FAILURE;
            }
        }
    }

    let persistence = match argument
        .command
        .input()
        .incremental_path
        .as_ref()
        .map(|x| {
            let mut serde_extension = SerdeExtension::<
                BinarySerializer<Box<dyn WriteAny>>,
                BinaryDeserializer<Box<dyn ReadAny>>,
            >::default();

            // register the serde extension with the persistence
            pernixc_bootstrap::register_serde(&mut serde_extension);

            let mut persistence =
                Persistence::new(x.clone(), Arc::new(serde_extension))?;

            pernixc_bootstrap::skip_persistence(&mut persistence);

            Ok::<_, redb::DatabaseError>(persistence)
        })
        .transpose()
    {
        Ok(persistence) => persistence,
        Err(err) => {
            let msg = Diagnostic::error().with_message(format!(
                "Failed to create persistence layer: {err}"
            ));
            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }
    };

    let token_trees_by_source_id = DashMap::default();

    let Bootstrap { engine, syntax_errors, semantic_diagnostics } =
        match pernixc_bootstrap::bootstrap(
            &mut source_map,
            root_source_id,
            target_name,
            argument.command.input().library_paths.iter().map(PathBuf::as_path),
            &token_trees_by_source_id,
            persistence,
        ) {
            Ok(database) => database,
            Err(error) => {
                let msg = match error {
                    pernixc_bootstrap::Error::OpenIncrementalFileIO(error) => {
                        Diagnostic::error().with_message(format!(
                            "Failed to open incremental file: {error}"
                        ))
                    }
                    pernixc_bootstrap::Error::IncrementalFileDeserialize(
                        error_kind,
                    ) => Diagnostic::error().with_message(format!(
                        "Failed to load (deserialize) incremental file: \
                         {error_kind}"
                    )),
                    pernixc_bootstrap::Error::ReadIncrementalFileIO(error) => {
                        Diagnostic::error().with_message(format!(
                            "Failed to read incremental file: {error}"
                        ))
                    }
                };

                report_term.report(&mut source_map, &msg);
                return ExitCode::FAILURE;
            }
        };

    let mut sortable_semantic_diagnostics = Vec::new();
    for diagnostic in syntax_errors {
        let codespan_reporting = match diagnostic {
            pernixc_bootstrap::tree::Error::Lexical(error) => {
                pernix_diagnostic_to_codespan_diagnostic(
                    error.report(&source_map),
                )
            }

            pernixc_bootstrap::tree::Error::Syntax(error) => {
                pernix_diagnostic_to_codespan_diagnostic(error.report(
                    &token_trees_by_source_id.get(&error.source_id).unwrap(),
                ))
            }

            pernixc_bootstrap::tree::Error::RootSubmoduleConflict(
                root_submodule_conflict,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                root_submodule_conflict.report(()),
                &source_map,
                &token_trees_by_source_id,
            ),

            pernixc_bootstrap::tree::Error::SourceFileLoadFail(
                source_file_load_fail,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                source_file_load_fail.report(()),
                &source_map,
                &token_trees_by_source_id,
            ),
            pernixc_bootstrap::tree::Error::ModuleRedefinition(
                module_redefinition,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                module_redefinition.report(()),
                &source_map,
                &token_trees_by_source_id,
            ),
        };

        sortable_semantic_diagnostics
            .push(SortableDiagnostic(codespan_reporting));
    }

    for diagnostic in semantic_diagnostics {
        let rel_pernixc_diagnostic = diagnostic.report(&engine);
        let codespan_reporting = rel_pernix_diagnostic_to_codespan_diagnostic(
            rel_pernixc_diagnostic,
            &source_map,
            &token_trees_by_source_id,
        );

        sortable_semantic_diagnostics
            .push(SortableDiagnostic(codespan_reporting));
    }

    sortable_semantic_diagnostics.sort_unstable();

    for diagnostic in sortable_semantic_diagnostics {
        report_term.report(&mut source_map, &diagnostic.0);
    }

    // save the database to the incremental path if it exists
    if let Err(err) = engine.try_save_database() {
        let msg = Diagnostic::error().with_message(format!(
            "Failed to save incremental database: {err}"
        ));
        report_term.report(&mut source_map, &msg);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
