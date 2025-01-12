use core::fmt;
use std::{any::Any, sync::Arc};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_table::{DisplayObject, GlobalID, Table};
use serde::{Deserialize, Serialize};

use super::{contains_error, Implementation};
use crate::type_system::{
    self,
    instantiation::Instantiation,
    model::{Default, Model},
    normalizer::Normalizer,
    query::{Call, Query},
    term::{lifetime::Lifetime, GenericArguments},
    Environment, Satisfied, Succeeded,
};

/// An enumeration of ways a positive trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PositiveSatisfied<M: Model> {
    /// The trait predicate was proven to be satisfied by the fact that the
    /// query was made inside the trait that is being queried.
    ByEnvironment,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    ByImplementation(Implementation<M>),

    /// The trait predicate was proven to be satisfied by the premise.
    ByPremise,

    /// The trait predicate satsifiability is co-inductive. This means that the
    /// trait predicate is satisfied by the cyclic nature of the trait
    /// predicates.
    ByCyclic,
}

/// An enumeration of ways a negative trait predicate can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NegativeSatisfied<M: Model> {
    /// The trait predicate was proven to be satisfied by the premise.
    ByPremise,

    /// The trait predicate was proven to be satisfied by searching for the
    /// matching trait implementation.
    ByImplementation(Implementation<M>),

    /// By proving that the positive trait predicate is not satisfied and the
    /// generic arguments are definite.
    ByUnsatisfiedPositive,
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

/// Represents a predicate stating that there exists an implementation for the
/// given trait and generic arguments
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
    new,
)]
pub struct Positive<M: Model> {
    /// The trait to be implemented.
    pub trait_id: GlobalID,

    /// Whether the implementation is const.
    pub is_const: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> Positive<M> {
    /// Converts the [`Positive`] predicate from the [`Default`] model to the
    /// model `M`.
    #[must_use]
    pub fn from_default_model(predicate: Positive<Default>) -> Self {
        Self {
            trait_id: predicate.trait_id,
            is_const: predicate.is_const,
            generic_arguments: GenericArguments::from_default_model(
                predicate.generic_arguments,
            ),
        }
    }

    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }

    /// Converts a positive trait with the model `U` into the model `M`.
    #[must_use]
    pub fn from_other_model<U: Model>(term: Positive<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            trait_id: term.trait_id,
            is_const: term.is_const,
            generic_arguments: GenericArguments::from_other_model(
                term.generic_arguments,
            ),
        }
    }

    /// Tries to convert a positive trait with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        term: Positive<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            trait_id: term.trait_id,
            is_const: term.is_const,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> pernixc_table::Display for Positive<M>
where
    GenericArguments<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "trait ")?;

        if self.is_const {
            write!(f, "const ")?;
        }

        write!(
            f,
            "{}{}",
            table.get_qualified_name(self.trait_id).ok_or(fmt::Error)?,
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

impl<M: Model> Query for Positive<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied<M>, M>;
    type Error = type_system::AbruptError;

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
            PositiveSatisfied::ByCyclic, /* doesn't matter */
        ))))
    }
}

/// Represents a predicate stating that there exists no implementation for the
/// given trait and generic arguments
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
    new,
)]
pub struct Negative<M: Model> {
    /// The trait in question.
    pub trait_id: GlobalID,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> Negative<M> {
    /// Converts the [`Negative`] predicate from the [`Default`] model to the
    /// model `M`.
    #[must_use]
    pub fn from_default_model(predicate: Negative<Default>) -> Self {
        Self {
            trait_id: predicate.trait_id,
            generic_arguments: GenericArguments::from_default_model(
                predicate.generic_arguments,
            ),
        }
    }

    /// Checks if the trait contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.generic_arguments.lifetimes.iter().any(Lifetime::is_error)
            || self.generic_arguments.types.iter().any(contains_error)
            || self.generic_arguments.constants.iter().any(contains_error)
    }

    /// Applies an instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.generic_arguments.instantiate(instantiation);
    }

    /// Converts a negative trait with the model `U` into the model `M`.
    #[must_use]
    pub fn from_other_model<U: Model>(term: Negative<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            trait_id: term.trait_id,
            generic_arguments: GenericArguments::from_other_model(
                term.generic_arguments,
            ),
        }
    }

    /// Tries to convert a negative trait with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        term: Negative<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            trait_id: term.trait_id,
            generic_arguments: GenericArguments::try_from_other_model(
                term.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> pernixc_table::Display for Negative<M>
where
    GenericArguments<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "!trait ")?;

        write!(
            f,
            "{}{}",
            table.get_qualified_name(self.trait_id).ok_or(fmt::Error)?,
            DisplayObject { display: &self.generic_arguments, table }
        )
    }
}

impl<M: Model> Query for Negative<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied<M>, M>;
    type Error = type_system::AbruptError;

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
) -> Result<Option<Succeeded<Satisfied, M>>, type_system::AbruptError> {
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
