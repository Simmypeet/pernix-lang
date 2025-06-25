//! Contains the main `run()` function for the compiler.

use std::{fs::File, io::BufWriter, path::PathBuf, process::ExitCode};

use argument::Arguments;
use clap::Args;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::WriteColor,
};
use flexstr::SharedStr;
use pernixc_diagnostic::Report;
use pernixc_hash::{DashMap, HashMap};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::{
    runtime::{
        persistence::{ReadAny, WriteAny},
        serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
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
    pub registry: pernixc_query::runtime::serde::Registry<S, D, Self>,
}

impl<S: Serializer<Self>, D: Deserializer<Self>> Default
    for SerdeExtension<S, D>
{
    fn default() -> Self {
        Self { registry: pernixc_query::runtime::serde::Registry::default() }
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicSerialize<S>
    for SerdeExtension<S, D>
{
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<
        StableTypeID,
        pernixc_query::runtime::serde::SerializationHelper<S, Self>,
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
        pernixc_query::runtime::serde::DeserializationHelper<D, Self>,
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

    let mut serde_extension = SerdeExtension::<
        BinarySerializer<Box<dyn WriteAny>>,
        BinaryDeserializer<Box<dyn ReadAny>>,
    >::default();

    pernixc_bootstrap::register_serde(&mut serde_extension);

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

    let token_trees_by_source_id = DashMap::default();
    let (engine, syntax_errors, init_semantic_errors) =
        match pernixc_bootstrap::bootstrap(
            &mut source_map,
            root_source_id,
            target_name,
            argument.command.input().library_paths.iter().map(PathBuf::as_path),
            &token_trees_by_source_id,
            argument
                .command
                .input()
                .incremental_path
                .as_ref()
                .map(|x| (x.as_path(), &mut serde_extension)),
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

    for diagnostic in &syntax_errors {
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

        report_term.report(&mut source_map, &codespan_reporting);
    }

    for diagnostic in &init_semantic_errors {
        let rel_pernixc_diagnostic = diagnostic.report(&engine);
        let codespan_reporting = rel_pernix_diagnostic_to_codespan_diagnostic(
            rel_pernixc_diagnostic,
            &source_map,
            &token_trees_by_source_id,
        );

        report_term.report(&mut source_map, &codespan_reporting);
    }

    if let Some(incremental_path) = &argument.command.input().incremental_path {
        let incremental_file = match File::create(incremental_path) {
            Ok(file) => file,
            Err(error) => {
                let msg = Diagnostic::error().with_message(format!(
                    "Failed to create incremental file: {error}"
                ));
                report_term.report(&mut source_map, &msg);
                return ExitCode::FAILURE;
            }
        };

        let mut binary_serializer = BinarySerializer::<Box<dyn WriteAny>>::new(
            Box::new(BufWriter::new(incremental_file)),
        );

        // Serialize using postcard and write to file
        if let Err(error) = Serialize::serialize(
            &engine.database,
            &mut binary_serializer,
            &serde_extension,
        ) {
            let msg = Diagnostic::error().with_message(format!(
                "Failed to serialize incremental file: {error}"
            ));
            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }
    }

    ExitCode::SUCCESS
}
