use core::fmt;
use std::{any::Any, sync::Arc};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_table::{DisplayObject, GlobalID, Table};
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{NegativeTrait as Negative, PositiveTrait as Positive},
    Model,
};
use serde::{Deserialize, Serialize};

use crate::{
    environment::{Call, Environment, Query},
    normalizer::Normalizer,
    resolution::Implementation,
    AbruptError, Satisfied, Succeeded,
};

/// An enumeration of ways a positive trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PositiveSatisfied<M: Model> {
    /// The trait predicate was proven to be satisfied by the fact that the
    /// query was made inside the trait that is being queried.
    Environment,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    Implementation(Implementation<M>),

    /// The trait predicate was proven to be satisfied by the premise.
    Premise,

    /// The trait predicate satsifiability is co-inductive. This means that the
    /// trait predicate is satisfied by the cyclic nature of the trait
    /// predicates.
    Cyclic,
}

/// An enumeration of ways a negative trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NegativeSatisfied<M: Model> {
    /// The trait predicate was proven to be satisfied by the premise.
    Premise,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    Implementation(Implementation<M>),

    /// By proving that the positive trait predicate is not satisfied and the
    /// generic arguments are definite.
    UnsatisfiedPositive,
}

/// The kind of the trait predicate; either positive or negative.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Kind {
    Positive {
        /// Whether the implementation is const.
        is_const: bool,
    },

    Negative,
}

impl<M: Model> Query for Positive<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied<M>, M>;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        todo!()
        /*
        // if this query was made in some trait implementation or trait, then
        // check if the trait implementation is the same as the query one.
        if let Some(result) =
            is_in_trait(self.id, &self.generic_arguments, environment, context)?
        {
            return Ok(Some(Succeeded::with_constraints(
                PositiveSatisfied::ByEnvironment,
                result.constraints,
            )));
        }

        // manually search for the trait implementation
        match resolve_implementation_with_context(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        ) {
            Ok(Succeeded {
                result:
                    Implementation {
                        instantiation,
                        id: TraitImplementationID::Positive(id),
                        is_not_general_enough,
                    },
                constraints,
            }) => {
                return Ok(Some(Succeeded::with_constraints(
                    PositiveSatisfied::ByImplementation(Implementation {
                        instantiation,
                        id,
                        is_not_general_enough,
                    }),
                    constraints,
                )));
            }

            Err(ResolutionError::Overflow(exceed_limit_error)) => {
                return Err(exceed_limit_error);
            }

            Err(_) | Ok(_) => {}
        }

        // look for the premise that matches
        for trait_premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_positive_trait)
        {
            // skip if the trait id is different
            if trait_premise.id != self.id {
                continue;
            }

            let Some(compatiblity) =
                self.generic_arguments.compatible_with_context(
                    &trait_premise.generic_arguments,
                    Variance::Invariant,
                    environment,
                    context,
                )?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Succeeded::with_constraints(
                PositiveSatisfied::ByPremise,
                compatiblity.constraints,
            )));
        }

        Ok(None)
        */
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

impl<M: Model> Query for Negative<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied<M>, M>;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        todo!()
        /*
        // manually search for the trait implementation
        match resolve_implementation_with_context(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        ) {
            Ok(Succeeded {
                result:
                    Implementation {
                        instantiation,
                        id: TraitImplementationID::Negative(id),
                        is_not_general_enough,
                    },
                constraints,
            }) => {
                return Ok(Some(Succeeded::with_constraints(
                    NegativeSatisfied::ByImplementation(Implementation {
                        instantiation,
                        id,
                        is_not_general_enough,
                    }),
                    constraints,
                )));
            }

            Err(ResolutionError::Overflow(exceed_limit_error)) => {
                return Err(exceed_limit_error);
            }

            Err(_) | Ok(_) => {}
        }

        // look for the premise that matches
        for trait_premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_negative_trait)
        {
            // skip if the trait id is different
            if trait_premise.id != self.id {
                continue;
            }

            let Some(compatiblity) =
                self.generic_arguments.compatible_with_context(
                    &trait_premise.generic_arguments,
                    Variance::Invariant,
                    environment,
                    context,
                )?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Succeeded::with_constraints(
                NegativeSatisfied::ByPremise,
                compatiblity.constraints,
            )));
        }

        // must be definite and failed to prove the positive trait
        let Some(definition) = self
            .generic_arguments
            .definite_with_context(environment, context)?
        else {
            return Ok(None);
        };

        Ok(Positive::new(self.id, false, self.generic_arguments.clone())
            .query_with_context(environment, context)?
            .is_none()
            .then(|| {
                Succeeded::with_constraints(
                    NegativeSatisfied::ByUnsatisfiedPositive,
                    definition.constraints,
                )
            }))
        */
    }
}

fn is_in_trait<M: Model>(
    trait_id: GlobalID,
    generic_arguments: &GenericArguments<M>,
    environment: &mut Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
    todo!()
    /*
    let Some(query_site) = environment.premise.query_site else {
        return Ok(None);
    };

    for item_id in
        if let Some(iter) = environment.table.scope_walker(query_site) {
            iter
        } else {
            return Ok(None);
        }
    {
        let ItemID::Trait(env_trait_id) = item_id else {
            continue;
        };

        // must be the same id
        if env_trait_id != trait_id {
            return Ok(None);
        }

        let trait_symbol = environment.table.get(trait_id).unwrap();

        let trait_generic_arguments = trait_symbol
            .generic_declaration()
            .parameters
            .create_identity_generic_arguments(env_trait_id.into());

        let Some(compatiblity) = generic_arguments.compatible_with_context(
            &trait_generic_arguments,
            Variance::Invariant,
            environment,
            context,
        )?
        else {
            return Ok(None);
        };

        if compatiblity.result.forall_lifetime_errors.is_empty() {
            return Ok(Some(Succeeded::satisfied_with(
                compatiblity.constraints,
            )));
        }
    }

    Ok(None)
    */
}

// TODO: bring test back
// #[cfg(test)]
// mod tests;
