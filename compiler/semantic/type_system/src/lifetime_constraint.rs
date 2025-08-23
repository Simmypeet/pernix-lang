//! Contains the definition of [`LifetimeConstraint`].

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{lifetime::Lifetime, predicate::Outlives};

use crate::{
    diagnostic::{Diagnostic, UnsatisfiedPredicate},
    environment::Environment,
    normalizer::Normalizer,
    Error,
};

/// Contains constraints related to lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum LifetimeConstraint {
    LifetimeOutlives(Outlives<Lifetime>),
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if the given lifetime constraints can be proven satisfiable. e
    pub async fn check_lifetime_constraints<'a>(
        &self,
        lifetime_constraints: impl IntoIterator<Item = &'a LifetimeConstraint>,
        span: &RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), Error> {
        let mut overflowed = false;
        for constraint in lifetime_constraints {
            match constraint {
                LifetimeConstraint::LifetimeOutlives(outlives) => {
                    match self.query(outlives).await {
                        Ok(Some(_)) => {}

                        Ok(None) => {
                            handler.receive(Diagnostic::UnsatisfiedPredicate(
                                UnsatisfiedPredicate::builder()
                                    .predicate(outlives.clone().into())
                                    .instantiation_span(*span)
                                    .build(),
                            ));
                        }

                        Err(Error::CyclicDependency(e)) => {
                            return Err(Error::CyclicDependency(e));
                        }
                        Err(Error::Overflow(e)) => {
                            let _ = e.report_as_undecidable_predicate(
                                outlives.clone().into(),
                                None,
                                *span,
                                handler,
                            );
                            overflowed = true;
                        }
                    }
                }
            }
        }

        if overflowed {
            return Err(Error::Overflow(crate::OverflowError));
        }
        Ok(())
    }
}
