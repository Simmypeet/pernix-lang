//! Contains the definition of [`LifetimeConstraint`].

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    diagnostic::{Diagnostic, UnsatisfiedPredicate},
    environment::Environment,
    normalizer::Normalizer,
};

/// Contains constraints related to lifetimes.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
)]
#[allow(missing_docs)]
pub enum LifetimeConstraint {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
}

impl LifetimeConstraint {
    /// Converts this lifetime constraint into a predicate.
    #[must_use]
    pub fn into_predicate(self) -> Predicate {
        match self {
            Self::LifetimeOutlives(outlives) => {
                Predicate::LifetimeOutlives(outlives)
            }
            Self::TypeOutlives(outlives) => Predicate::TypeOutlives(outlives),
        }
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if the given lifetime constraints can be proven satisfiable. e
    pub async fn check_lifetime_constraints<'a>(
        &self,
        lifetime_constraints: impl IntoIterator<Item = &'a LifetimeConstraint>,
        span: &RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) {
        for constraint in lifetime_constraints {
            match constraint {
                LifetimeConstraint::LifetimeOutlives(outlives) => {
                    match self.query(outlives).await {
                        Ok(true) => {}

                        Ok(false) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate::builder()
                                    .predicate(outlives.clone().into())
                                    .instantiation_span(*span)
                                    .build(),
                            ));
                        }

                        Err(e) => {
                            e.report_as_undecidable_predicate(
                                outlives.clone().into(),
                                None,
                                *span,
                                handler,
                            );
                        }
                    }
                }

                LifetimeConstraint::TypeOutlives(outlives) => {
                    match self.query(outlives).await {
                        Ok(true) => {}

                        Ok(false) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate::builder()
                                    .predicate(outlives.clone().into())
                                    .instantiation_span(*span)
                                    .build(),
                            ));
                        }

                        Err(e) => {
                            e.report_as_undecidable_predicate(
                                outlives.clone().into(),
                                None,
                                *span,
                                handler,
                            );
                        }
                    }
                }
            }
        }
    }
}
