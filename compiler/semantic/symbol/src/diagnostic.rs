//! Contains all the diagnostics related creating a new compilation target.

use std::{fmt::Debug, sync::Arc};

use flexstr::SharedStr;
use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_module_tree::source_map::to_absolute_span;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};
use rayon::iter::{
    IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};

use crate::{
    import,
    name::{get_name, get_qualified_name},
    span::get_span,
    ID,
};

/// Query for retrieving all the diagnostics encountered during the symbol table
/// construction.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Value,
)]
#[id(TargetID)]
pub struct Diagnostics {
    /// A list of redefinition errors encountered during the symbol table
    /// construction.
    pub redefinition: Arc<[ItemRedifinition]>,

    /// A list of import diagnostics encountered during the symbol table
    /// construction.
    pub import: Arc<[Arc<[import::diagnostic::Diagnostic]>]>,
}

/// An executor for the [`Key`] query that retrieves all the diagnostics
/// encountered during the symbol table construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        &Key(target_id): &Key,
    ) -> Result<Diagnostics, pernixc_query::runtime::executor::CyclicError>
    {
        let table = engine.query(&crate::Key(target_id))?;

        let import = table
            .entries_by_id
            .iter()
            .filter_map(|x| x.1.imports.is_some().then_some(x.0))
            .collect::<Vec<_>>()
            .into_par_iter()
            .map(|x| {
                let imports = engine.query(&import::WithDiagnosticKey(
                    target_id.make_global(*x),
                ))?;

                Ok(imports.diagnostics)
            })
            .collect::<Result<Arc<[_]>, CyclicError>>()?;

        Ok(Diagnostics {
            redefinition: table.redefinition_errors.clone(),
            import,
        })
    }
}

/// The item symbol with the same name already exists in the given scope.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: Global<ID>,

    /// The ID of the new symbol.
    pub new_id: Global<ID>,

    /// The scope in which the duplication occurred.
    pub in_id: Global<ID>,
}

impl Report<&TrackedEngine<'_>> for ItemRedifinition {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<ByteIndex> {
        let existing_symbol_span = engine.get_span(self.existing_id);
        let new_symbol_span = engine.get_span(self.new_id);
        let existing_symbol_name = engine.get_name(self.existing_id);
        let in_name = engine.get_qualified_name(self.in_id);

        pernixc_diagnostic::Diagnostic {
            span: new_symbol_span.as_ref().map(|x| {
                (
                    engine.to_absolute_span(x),
                    Some("redefinition here".to_string()),
                )
            }),

            message: format!(
                "symbol `{existing_symbol_name}` is already defined in the \
                 scope `{in_name}`"
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: existing_symbol_span
                .as_ref()
                .map(|span| pernixc_diagnostic::Related {
                    span: engine.to_absolute_span(span),
                    message: format!(
                        "symbol `{existing_symbol_name}` is already defined \
                         here"
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<Global<ID>>,

    /// The span where the symbol was searched from.
    pub resolution_span: RelativeSpan,

    /// The name that failed to resolved.
    pub name: SharedStr,
}

/// A list of errors that has been rendered in a form of [`Diagnostic`] from the
/// [`Errors`]
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(TargetID)]
#[key(RenderedKey)]
#[value(Rendered)]
#[extend(method(get_symbol_table_rendered_errors), no_cyclic)]
pub struct Rendered(pub Arc<[Diagnostic<ByteIndex>]>);

/// An executor for rendering errors from the symbol table construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct RenderedExecutor;

impl pernixc_query::runtime::executor::Executor<RenderedKey>
    for RenderedExecutor
{
    fn execute(
        &self,
        engine: &TrackedEngine,
        key: &RenderedKey,
    ) -> Result<Rendered, pernixc_query::runtime::executor::CyclicError> {
        let errors = engine.query(&Key(key.0))?;

        Ok(Rendered(
            errors
                .redefinition
                .par_iter()
                .map(|error| error.report(engine))
                .chain(
                    errors
                        .import
                        .par_iter()
                        .flat_map(|x| x.par_iter())
                        .map(|error| error.report(engine)),
                )
                .collect::<Vec<_>>()
                .into(),
        ))
    }
}

/*
/// The symbol is more accessible than the parent symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsMoreAccessibleThanParent {
    /// The ID of the symbol that is more accessible than the parent symbol.
    pub symbol_id: Global<symbol::ID>,

    /// The ID of the parent symbol.
    pub parent_id: Global<symbol::ID>,
}

fn accessibility_description(
    engine: &TrackedEngine,
    target_id: TargetID,
    accessibility: Accessibility<symbol::ID>,
) -> String {
    match accessibility {
        Accessibility::Public => "publicly accessible".to_owned(),
        Accessibility::Scoped(module_id) => {
            let module_qualified_name =
                engine.get_qualified_name(Global::new(target_id, module_id));

            format!("accessible in `{module_qualified_name}`")
        }
    }
}

impl Report<&Engine> for SymbolIsMoreAccessibleThanParent {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let symbol_name = engine.get_name(self.symbol_id);
        let parent_qualified_name = engine.get_qualified_name(self.parent_id);

        let symbol_accessibility = engine.get_accessibility(self.symbol_id);
        let parent_accessibility = engine.get_accessibility(self.parent_id);

        let symbol_span = engine.get_span(self.symbol_id);
        let parent_span = engine.get_span(self.parent_id);

        let symbol_accessibility_description = accessibility_description(
            &engine,
            self.symbol_id.target_id,
            symbol_accessibility,
        );

        let parent_accessibility_description = accessibility_description(
            &engine,
            self.parent_id.target_id,
            parent_accessibility,
        );

        pernixc_diagnostic::Diagnostic {
            span: symbol_span.map(|span| {
                (
                    span,
                    Some(format!(
                        "the symbol `{}` is \
                         {symbol_accessibility_description}, which is more \
                         accessible than its parent symbol \
                         `{parent_qualified_name}`",
                        symbol_name.as_str(),
                    )),
                )
            }),
            message: "the symbol is more accessible than its parent symbol"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: parent_span
                .map(|span| {
                    vec![Related {
                        span,
                        message: format!(
                            "the parent symbol `{parent_qualified_name}` is \
                             {parent_accessibility_description}",
                        ),
                    }]
                })
                .unwrap_or_default(),
        }
    }
}

*/
