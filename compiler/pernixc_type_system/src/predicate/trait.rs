use std::{any::Any, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_table::{component::SymbolKind, GlobalID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    predicate::{
        NegativeTrait as Negative, PositiveTrait as Positive, Predicate,
    },
    variance::Variance,
    Model,
};

use crate::{
    environment::{Call, Environment, Query},
    normalizer::Normalizer,
    resolution::{self, Implementation},
    Error, Satisfied, Succeeded,
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

impl<M: Model> Query for Positive<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied<M>, M>;
    type Error = Error;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // if this query was made in some trait implementation or trait, then
        // check if the trait implementation is the same as the query one.
        if let Some(result) =
            is_in_trait(self.trait_id, &self.generic_arguments, environment)?
        {
            return Ok(Some(Arc::new(Succeeded::with_constraints(
                PositiveSatisfied::Environment,
                result.constraints,
            ))));
        }

        // manually search for the trait implementation
        match environment
            .resolve_implementation(self.trait_id, &self.generic_arguments)
        {
            Ok(Succeeded {
                result:
                    Implementation { instantiation, id, is_not_general_enough },
                constraints,
            }) => {
                if *environment.table().get::<SymbolKind>(id)
                    == SymbolKind::PositiveTraitImplementation
                {
                    return Ok(Some(Arc::new(Succeeded::with_constraints(
                        PositiveSatisfied::Implementation(Implementation {
                            instantiation,
                            id,
                            is_not_general_enough,
                        }),
                        constraints,
                    ))));
                }
            }

            Err(resolution::Error::Abort(error)) => {
                return Err(Error::Abort(error));
            }

            Err(resolution::Error::Overflow(error)) => {
                return Err(Error::Overflow(error));
            }

            Err(_) => {}
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

            let Some(compatiblity) = environment.generic_arguments_compatible(
                &self.generic_arguments,
                &trait_premise.generic_arguments,
                Variance::Invariant,
            )?
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

impl<M: Model> Query for Negative<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied<M>, M>;
    type Error = Error;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // manually search for the trait implementation
        match environment
            .resolve_implementation(self.trait_id, &self.generic_arguments)
        {
            Ok(Succeeded {
                result:
                    Implementation { instantiation, id, is_not_general_enough },
                constraints,
            }) => {
                if *environment.table().get::<SymbolKind>(id)
                    == SymbolKind::NegativeTraitImplementation
                {
                    return Ok(Some(Arc::new(Succeeded::with_constraints(
                        NegativeSatisfied::Implementation(Implementation {
                            instantiation,
                            id,
                            is_not_general_enough,
                        }),
                        constraints,
                    ))));
                }
            }

            Err(resolution::Error::Overflow(error)) => {
                return Err(Error::Overflow(error));
            }

            Err(resolution::Error::Abort(error)) => {
                return Err(Error::Abort(error));
            }

            Err(_) => {}
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

            let Some(compatiblity) = environment.generic_arguments_compatible(
                &self.generic_arguments,
                &trait_premise.generic_arguments,
                Variance::Invariant,
            )?
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
        let Some(definition) =
            environment.generic_arguments_definite(&self.generic_arguments)?
        else {
            return Ok(None);
        };

        Ok(environment
            .query(&Positive::new(
                self.trait_id,
                false,
                self.generic_arguments.clone(),
            ))?
            .is_none()
            .then(|| {
                Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::UnsatisfiedPositive,
                    definition.constraints,
                ))
            }))
    }
}

fn is_in_trait<M: Model>(
    trait_id: GlobalID,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Satisfied, M>>, Error> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    for current_id in environment.table().scope_walker(query_site) {
        let current_id = GlobalID::new(query_site.target_id, current_id);

        let SymbolKind::Trait =
            *environment.table().get::<SymbolKind>(current_id)
        else {
            continue;
        };

        // must be the same id
        if current_id != trait_id {
            continue;
        }

        let trait_generic_arguments = environment
            .table()
            .query::<GenericParameters>(current_id)?
            .create_identity_generic_arguments(current_id);

        let Some(compatibility) = environment.generic_arguments_compatible(
            generic_arguments,
            &trait_generic_arguments,
            Variance::Invariant,
        )?
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
