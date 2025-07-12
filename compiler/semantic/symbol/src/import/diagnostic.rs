//! Contains the diagnostics related to import statements.

use flexstr::SharedStr;
use pernixc_diagnostic::{Related, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_module_tree::source_map::to_absolute_span;
use pernixc_query::TrackedEngine;
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

impl Report<&TrackedEngine<'_>> for Diagnostic {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::Naming(err) => err.report(engine),
            Self::TargetRootInImportIsNotAllowedwithFrom(err) => {
                err.report(engine)
            }
            Self::ConflictingUsing(err) => err.report(engine),
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

impl Report<&TrackedEngine<'_>> for TargetRootInImportIsNotAllowedwithFrom {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.target_root_span),
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
        }
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

impl Report<&TrackedEngine<'_>> for ConflictingUsing {
    type Location = ByteIndex;

    fn report(
        &self,
        engine: &TrackedEngine<'_>,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let module_qualified_name = engine.get_qualified_name(self.module_id);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                engine.to_absolute_span(&self.using_span),
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
            related: self
                .conflicting_span
                .as_ref()
                .map(|span| {
                    vec![Related {
                        span: engine.to_absolute_span(span),
                        message: format!(
                            "this symbol already defined the name `{}`",
                            self.name
                        ),
                    }]
                })
                .unwrap_or_default(),
        }
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
