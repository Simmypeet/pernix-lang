//! Implements the [`Query`] for the [`PositiveTrait`] and [`NegativeTrait`].

use std::{any::Any, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    predicate::{NegativeTrait, PositiveTrait, Predicate},
};

use crate::{
    environment::{Call, Environment, Query},
    normalizer::Normalizer,
    resolution::{self, Implementation},
    Error, Satisfied, Succeeded,
};

/// An enumeration of ways a positive trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PositiveSatisfied {
    /// The trait predicate was proven to be satisfied by the fact that the
    /// query was made inside the trait that is being queried.
    Environment,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    Implementation(Implementation),

    /// The trait predicate was proven to be satisfied by the premise.
    Premise,

    /// The trait predicate satsifiability is co-inductive. This means that the
    /// trait predicate is satisfied by the cyclic nature of the trait
    /// predicates.
    Cyclic,
}

/// An enumeration of ways a negative trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NegativeSatisfied {
    /// The trait predicate was proven to be satisfied by the premise.
    Premise,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    Implementation(Implementation),

    /// By proving that the positive trait predicate is not satisfied and the
    /// generic arguments are definite.
    UnsatisfiedPositive,
}

impl Query for PositiveTrait {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied>;
    type Error = Error;

    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // if this query was made in some trait implementation or trait, then
        // check if the trait implementation is the same as the query one.
        if let Some(result) =
            is_in_trait(self.trait_id, &self.generic_arguments, environment)
                .await?
        {
            return Ok(Some(Arc::new(Succeeded::with_constraints(
                PositiveSatisfied::Environment,
                result.constraints,
            ))));
        }

        // manually search for the trait implementation
        if let Some(result) = environment
            .query(&resolution::Resolve::new(
                self.trait_id,
                self.generic_arguments.clone(),
            ))
            .await?
        {
            if environment.tracked_engine().get_kind(result.result.id).await
                == Kind::PositiveTraitImplementation
            {
                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    PositiveSatisfied::Implementation(Implementation {
                        instantiation: result.result.instantiation.clone(),
                        id: result.result.id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    }),
                    result.constraints.clone(),
                ))));
            }
        }

        // look for the premise that matches
        for trait_premise in environment
            .premise()
            .predicates
            .iter()
            .filter_map(Predicate::as_positive_trait)
        {
            // skip if the trait id is different
            if trait_premise.trait_id != self.trait_id {
                continue;
            }

            let Some(compatiblity) = environment
                .subtypes_generic_arguments(
                    &self.generic_arguments,
                    &trait_premise.generic_arguments,
                )
                .await?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                PositiveSatisfied::Premise,
                compatiblity.constraints,
            ))));
        }

        Ok(None)
    }

    fn on_cyclic(
        &self,
        (): Self::Parameter,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[Call<Arc<dyn Any + Send + Sync>, Arc<dyn Any + Send + Sync>>],
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        Ok(Some(Arc::new(Succeeded::new(
            PositiveSatisfied::Cyclic, /* doesn't matter */
        ))))
    }
}

impl Query for NegativeTrait {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied>;
    type Error = Error;

    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // manually search for the trait implementation
        if let Some(result) = environment
            .query(&resolution::Resolve::new(
                self.trait_id,
                self.generic_arguments.clone(),
            ))
            .await?
        {
            if environment.tracked_engine().get_kind(result.result.id).await
                == Kind::NegativeTraitImplementation
            {
                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::Implementation(Implementation {
                        instantiation: result.result.instantiation.clone(),
                        id: result.result.id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    }),
                    result.constraints.clone(),
                ))));
            }
        }

        // look for the premise that matches
        for trait_premise in environment
            .premise()
            .predicates
            .iter()
            .filter_map(Predicate::as_negative_trait)
        {
            // skip if the trait id is different
            if trait_premise.trait_id != self.trait_id {
                continue;
            }

            let Some(compatiblity) = environment
                .subtypes_generic_arguments(
                    &self.generic_arguments,
                    &trait_premise.generic_arguments,
                )
                .await?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Arc::new(Succeeded::with_constraints(
                NegativeSatisfied::Premise,
                compatiblity.constraints,
            ))));
        }

        // must be definite and failed to prove the positive trait
        let Some(definition) = environment
            .generic_arguments_definite(&self.generic_arguments)
            .await?
        else {
            return Ok(None);
        };

        Ok(environment
            .query(&PositiveTrait::new(
                self.trait_id,
                false,
                self.generic_arguments.clone(),
            ))
            .await?
            .is_none()
            .then(|| {
                Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::UnsatisfiedPositive,
                    definition.constraints,
                ))
            }))
    }
}

async fn is_in_trait(
    trait_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, Error> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    let mut scope_walker =
        environment.tracked_engine().scope_walker(query_site);

    while let Some(current_id) = scope_walker.next().await {
        let current_id = Global::new(query_site.target_id, current_id);

        let Kind::Trait =
            environment.tracked_engine().get_kind(current_id).await
        else {
            continue;
        };

        // must be the same id
        if current_id != trait_id {
            continue;
        }

        let trait_generic_arguments = environment
            .tracked_engine()
            .get_generic_parameters(current_id)
            .await?
            .create_identity_generic_arguments(current_id);

        let Some(compatibility) = environment
            .subtypes_generic_arguments(
                generic_arguments,
                &trait_generic_arguments,
            )
            .await?
        else {
            continue;
        };

        if compatibility.result.forall_lifetime_errors.is_empty() {
            return Ok(Some(Succeeded::satisfied_with(
                compatibility.constraints,
            )));
        }
    }

    Ok(None)
}

#[cfg(test)]
mod test;
