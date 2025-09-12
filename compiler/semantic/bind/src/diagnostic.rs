//! Diagnostics emitted during the binding phase of the compiler.

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;

use crate::{binder, pattern};

diagnostic_enum! {
    /// An enumeration of all diagnostics that can occur during binding the
    /// IR phase.
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
        derive_more::From,
    )]
    #[allow(clippy::large_enum_variant, missing_docs)]
    pub enum Diagnostic {
        Resolution(pernixc_resolution::diagnostic::Diagnostic),
        TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
        TypeCheck(binder::type_check::diagnostic::Diagnostic),
        ExpectedLValue(ExpectedLValue),
        PatternBinding(pattern::bind::diagnostic::Diagnostic),
        PatternInsertNameBinding(
            pattern::insert_name_binding::diagnostic::Diagnostic
        ),
        QualifiedIdentifier(
            expression::qualified_identifier::diagnostic::Diagnostic
        ),
        FunctionCall(expression::function_call::diagnostic::Diagnostic),
        Parenthesized(expression::parenthesized::diagnostic::Diagnostic),
        Numeric(expression::numeric::diagnostic::Diagnostic),
        Struct(expression::r#struct::diagnostic::Diagnostic),
        Postfix(expression::postfix::diagnostic::Diagnostic),
    }
}

/// A convenience macro for creating diagnostic enums and implementing the
/// `Report` trait for them.
#[macro_export]
macro_rules! diagnostic_enum {
    {
        $( #[$enum_meta:meta] )*
        $enum_vis:vis enum $ident:ident {
            $(
                $variant:ident($ty:ty)
            ),* $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        $enum_vis enum $ident {
            $(
                $variant($ty),
            )*
        }

        impl pernixc_diagnostic::Report<&pernixc_query::TrackedEngine> for Diagnostic {
            type Location = pernixc_source_file::ByteIndex;

            async fn report(
                &self,
                engine: &pernixc_query::TrackedEngine,
            ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
                match self {
                    $(
                        Diagnostic::$variant(inner) => inner.report(engine).await,
                    )*
                }
            }
        }
    };
}

pub use diagnostic_enum;

use crate::bind::expression;

/// Expected an l-value but found an r-value.
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
pub struct ExpectedLValue {
    /// The span of the r-value.
    pub expression_span: RelativeSpan,
}

impl Report<&TrackedEngine> for ExpectedLValue {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.expression_span).await)
                    .build(),
            )
            .severity(Severity::Error)
            .message("expected an l-value")
            .help_message(
                "an l-value refers to an expression that designates a storage \
                 location,
                for example, a variable or a dereferenced pointer",
            )
            .build()
    }
}
