use pernixc_diagnostic::{Highlight, Report};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::implements_arguments::get_implements_argument;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{source_map::to_absolute_span, span::get_span};
use pernixc_target::Global;
use pernixc_term::{display::Display, generic_arguments::GenericArguments};
use pernixc_type_system::diagnostic::UnsatisfiedPredicate;

/// Enumeration of all diagnostics during the well-formedness checking.
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
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
    MismatchedImplementationArguments(MismatchedImplementationArguments),
    AdtImplementationIsNotGeneralEnough(AdtImplementationIsNotGeneralEnough),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::TypeSystem(diag) => diag.report(parameter).await,
            Self::MismatchedImplementationArguments(diag) => {
                diag.report(parameter).await
            }
            Self::AdtImplementationIsNotGeneralEnough(diag) => {
                diag.report(parameter).await
            }
        }
    }
}

/// The generic arguments are not compatible with the generic arguments defined
/// in the implementation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MismatchedImplementationArguments {
    /// The ID of the ADT implementation where the generic arguments are
    /// mismatched.
    pub adt_implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments found in the implementation.
    pub found_generic_arguments: GenericArguments,

    /// The span of the instantiation that causes the mismatch.
    pub instantiation_span: RelativeSpan,
}

impl Report for MismatchedImplementationArguments {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let impl_span = if let Some(span) =
            engine.get_span(self.adt_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        let impl_arguments = engine
            .get_implements_argument(self.adt_implementation_id)
            .await
            .unwrap()
            .unwrap();

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(
                "the generic arguments are not compatible with the generic \
                 arguments defined in the implementation",
            )
            .primary_highlight(
                Highlight::builder()
                    .span(
                        engine.to_absolute_span(&self.instantiation_span).await,
                    )
                    .message({
                        let mut string = String::new();

                        string.push_str("the generic arguments supplied was `");
                        self.found_generic_arguments
                            .write_async(engine, &mut string)
                            .await
                            .unwrap();
                        string.push_str("` aren't compatible with `");
                        impl_arguments
                            .write_async(engine, &mut string)
                            .await
                            .unwrap();
                        string.push('`');

                        string
                    })
                    .build(),
            )
            .related(
                impl_span
                    .as_ref()
                    .map(|span| {
                        Highlight::new(
                            *span,
                            Some(
                                "the implementation is defined here"
                                    .to_string(),
                            ),
                        )
                    })
                    .into_iter()
                    .collect(),
            )
            .build())
    }
}

/// The ADT implementation is not general enough to satisfy the required forall
/// lifetimes in the generic arguments
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct AdtImplementationIsNotGeneralEnough {
    /// The ADT implementation ID where the generic arguments are not general
    /// enough.
    pub adt_implementation_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the ADT.
    pub generic_arguments: GenericArguments,

    /// The span location of where the ADT is instantiated.
    pub instantiation_span: RelativeSpan,
}

impl Report for AdtImplementationIsNotGeneralEnough {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.instantiation_span).await;
        let impl_span = if let Some(span) =
            engine.get_span(self.adt_implementation_id).await
        {
            Some(engine.to_absolute_span(&span).await)
        } else {
            None
        };

        Ok(pernixc_diagnostic::Diagnostic::builder()
            .message(
                "the struct/enum implementation is not general enough to \
                 satisfy the required forall lifetimes in the generic \
                 arguments",
            )
            .primary_highlight(Highlight::new(
                span,
                Some({
                    let mut string = String::new();

                    string.push_str("the generic arguments supplied was `");
                    self.generic_arguments
                        .write_async(engine, &mut string)
                        .await
                        .unwrap();
                    string.push('`');

                    string
                }),
            ))
            .related(
                impl_span
                    .map(|span| {
                        Highlight::new(
                            span,
                            Some(
                                "the implementation is defined here"
                                    .to_string(),
                            ),
                        )
                    })
                    .into_iter()
                    .collect(),
            )
            .build())
    }
}

impl From<UnsatisfiedPredicate> for Diagnostic {
    fn from(value: UnsatisfiedPredicate) -> Self {
        Self::TypeSystem(value.into())
    }
}

impl From<pernixc_type_system::diagnostic::ImplementationIsNotGeneralEnough>
    for Diagnostic
{
    fn from(
        value: pernixc_type_system::diagnostic::ImplementationIsNotGeneralEnough,
    ) -> Self {
        Self::TypeSystem(value.into())
    }
}
