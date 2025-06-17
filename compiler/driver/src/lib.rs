//! Contains the main `run()` function for the compiler.

use std::{
    fs::File,
    io::{BufReader, BufWriter},
    path::PathBuf,
    process::ExitCode,
};

use argument::Arguments;
use clap::Args;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::termcolor::WriteColor,
};
use dashmap::DashMap;
use fnv::FnvBuildHasher;
use pernixc_diagnostic::Report;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::{
    serde::{DynamicDeserialize, DynamicRegistry, DynamicSerialize},
    Key,
};
use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    de::Deserializer,
    extension::{
        shared_pointer::{SharedPointerStore, SharedPointerTracker},
        SharedPointerDeserialize, SharedPointerSerialize,
    },
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
        FnvBuildHasher,
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
    pub registry: pernixc_query::serde::Registry<S, D, Self>,
    pub shared_pointer_store: SharedPointerStore,
    pub shared_pointer_tracker: SharedPointerTracker,
}

impl<S: Serializer<Self>, D: Deserializer<Self>> Default
    for SerdeExtension<S, D>
{
    fn default() -> Self {
        Self {
            registry: pernixc_query::serde::Registry::default(),
            shared_pointer_store: SharedPointerStore::default(),
            shared_pointer_tracker: SharedPointerTracker::default(),
        }
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> SharedPointerSerialize
    for SerdeExtension<S, D>
{
    fn register_arc<T>(&mut self, arc: &std::sync::Arc<T>) -> bool {
        self.shared_pointer_tracker.register_arc(arc)
    }

    fn register_rc<T>(&mut self, rc: &std::rc::Rc<T>) -> bool {
        self.shared_pointer_tracker.register_rc(rc)
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> SharedPointerDeserialize
    for SerdeExtension<S, D>
{
    fn store_arc<T: 'static + Send + Sync>(
        &mut self,
        pointer: usize,
        arc: std::sync::Arc<T>,
    ) {
        self.shared_pointer_store.store_arc(pointer, arc);
    }

    fn get_arc<T: 'static + Send + Sync>(
        &self,
        pointer: usize,
    ) -> Option<std::sync::Arc<T>> {
        self.shared_pointer_store.get_arc(pointer)
    }

    fn store_rc<T: 'static>(&mut self, pointer: usize, rc: std::rc::Rc<T>) {
        self.shared_pointer_store.store_rc(pointer, rc);
    }

    fn get_rc<T: 'static>(&self, pointer: usize) -> Option<std::rc::Rc<T>> {
        self.shared_pointer_store.get_rc(pointer)
    }
}

impl<S: Serializer<Self>, D: Deserializer<Self>> DynamicSerialize<S>
    for SerdeExtension<S, D>
{
    fn serialization_helper_by_type_id(
        &self,
    ) -> &HashMap<
        StableTypeID,
        pernixc_query::serde::SerializationHelper<S, Self>,
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
        pernixc_query::serde::DeserializationHelper<D, Self>,
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
    {
        self.registry.register::<K>();
    }
}

/// Runs the program with the given arguments.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn run(
    argument: &Arguments,
    err_writer: &mut dyn WriteColor,
    _out_writer: &mut dyn WriteColor,
) -> ExitCode {
    let mut report_term = ReportTerm { config: get_coonfig(), err_writer };
    let mut source_map = SourceMap::new();

    let Some(root_source_id) =
        create_root_source_file(argument, &mut source_map, &mut report_term)
    else {
        return ExitCode::FAILURE;
    };

    let mut query_runtime = pernixc_query::runtime::Runtime::default();
    let mut serde_extension = SerdeExtension::<
        BinarySerializer<BufWriter<File>>,
        BinaryDeserializer<BufReader<File>>,
    >::default();

    pernixc_init::register_runtime(&mut query_runtime, &mut serde_extension);

    let _target_name =
        argument.command.input().target_name.clone().unwrap_or_else(|| {
            source_map
                .get(root_source_id)
                .unwrap()
                .full_path()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string()
        });

    let database = match pernixc_init::start_query_database(
        &mut source_map,
        root_source_id,
        argument.command.input().library_paths.iter().map(PathBuf::as_path),
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
                pernixc_init::Error::OpenIncrementalFileIO(error) => {
                    Diagnostic::error().with_message(format!(
                        "Failed to open incremental file: {error}"
                    ))
                }
                pernixc_init::Error::IncrementalFileDeserialize(error_kind) => {
                    Diagnostic::error().with_message(format!(
                        "Failed to load (deserialize) incremental file: \
                     {error_kind}"
                    ))
                }
                pernixc_init::Error::ReadIncrementalFileIO(error) => {
                    Diagnostic::error().with_message(format!(
                        "Failed to read incremental file: {error}"
                    ))
                }
            };

            report_term.report(&mut source_map, &msg);
            return ExitCode::FAILURE;
        }
    };

    for diagnostic in database.module_parsing_errors {
        let codespan_reporting = match diagnostic {
            pernixc_init::module::Error::Lexical(error) => {
                pernix_diagnostic_to_codespan_diagnostic(
                    error.report(&source_map),
                )
            }

            pernixc_init::module::Error::Syntax(error) => {
                pernix_diagnostic_to_codespan_diagnostic(
                    error.report(
                        &database
                            .token_trees_by_source_id
                            .get(&error.source_id)
                            .unwrap(),
                    ),
                )
            }

            pernixc_init::module::Error::RootSubmoduleConflict(
                root_submodule_conflict,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                root_submodule_conflict.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),

            pernixc_init::module::Error::SourceFileLoadFail(
                source_file_load_fail,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                source_file_load_fail.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),
            pernixc_init::module::Error::ModuleRedefinition(
                module_redefinition,
            ) => rel_pernix_diagnostic_to_codespan_diagnostic(
                module_redefinition.report(()),
                &source_map,
                &database.token_trees_by_source_id,
            ),
        };

        report_term.report(&mut source_map, &codespan_reporting);
    }

    if let Some(incremental_path) =
        argument.command.input().incremental_path.as_ref()
    {
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

        let writer = BufWriter::new(incremental_file);
        let mut bianry_serializer = BinarySerializer::new(writer);

        // Serialize using postcard and write to file
        if let Err(error) = Serialize::serialize(
            &database.database,
            &mut bianry_serializer,
            &mut serde_extension,
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
