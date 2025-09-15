//! Contains the diagnostics related to import statements.

use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    name::{
        self,
        diagnostic::{SymbolIsNotAccessible, SymbolNotFound},
        get_qualified_name,
    },
    source_map::to_absolute_span,
    ID,
};

/// Enumeration of all diagnostics related to import statements.
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
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    Naming(name::diagnostic::Diagnostic),
    TargetRootInImportIsNotAllowedwithFrom(
        TargetRootInImportIsNotAllowedwithFrom,
    ),
    ConflictingUsing(ConflictingUsing),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::Naming(err) => err.report(engine).await,
            Self::TargetRootInImportIsNotAllowedwithFrom(err) => {
                err.report(engine).await
            }
            Self::ConflictingUsing(err) => err.report(engine).await,
        }
    }
}

/// The import items that have a `from` clause cannot have the `target` as the
/// root path.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct TargetRootInImportIsNotAllowedwithFrom {
    /// The span where the target root is found.
    pub target_root_span: RelativeSpan,
}

impl Report for TargetRootInImportIsNotAllowedwithFrom {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.target_root_span).await,
                Some(
                    "the `target` root path is not allowed with `from` clause"
                        .to_string(),
                ),
            )),
            message: "import items that have a `from` clause cannot have the \
                      `target` as the root path"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The name is already exists in the given module.
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
)]
pub struct ConflictingUsing {
    /// The span of the using statement.
    pub using_span: RelativeSpan,

    /// The name that conflicts with the existing name in the module.
    pub name: SharedStr,

    /// The module where the name is already defined.
    pub module_id: Global<ID>,

    /// The span of the conflicting name.
    ///
    /// This can either be the span to the declared symbol or the previous
    /// using that uses the given name.
    pub conflicting_span: Option<RelativeSpan>,
}

impl Report for ConflictingUsing {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let module_qualified_name =
            engine.get_qualified_name(self.module_id).await;

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.using_span).await,
                Some(format!(
                    "the using `{name}` conflicts with the existing name in \
                     the module `{module_qualified_name}`",
                    name = self.name
                )),
            )),
            message: "the using statement conflicts with an existing name in \
                      the module"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: match self.conflicting_span.as_ref() {
                Some(span) => {
                    vec![Highlight::new(
                        engine.to_absolute_span(span).await,
                        Some(format!(
                            "this symbol already defined the name `{}`",
                            self.name
                        )),
                    )]
                }
                None => vec![],
            },
        })
    }
}

impl From<SymbolNotFound> for Diagnostic {
    fn from(value: SymbolNotFound) -> Self {
        Self::Naming(name::diagnostic::Diagnostic::SymbolNotFound(value))
    }
}

impl From<SymbolIsNotAccessible> for Diagnostic {
    fn from(value: SymbolIsNotAccessible) -> Self {
        Self::Naming(name::diagnostic::Diagnostic::SymbolIsNotAccessible(value))
    }
}
