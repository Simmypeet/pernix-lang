//! Contains the definition of the [`Observer`] trait and its default
//! implementation.

use super::{
    environment::Environment,
    model::Model,
    normalizer::Normalizer,
    query::{Context, Record},
    term::GenericArguments,
    OverflowError,
};
use crate::{
    arena::ID,
    symbol::{self, table::State, AdtID, TraitImplementationType},
};

/// The observer trait that observes the record before the query is executed.
///
/// This is primarily used for dependency injection to allow the compiler build
/// the dependencies before the query is executed.
pub trait Observer<M: Model, T: State>: Sized {
    /// Invoked before the query is executed.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn on_query(
        record: &Record<M>,
        environment: &Environment<M, T, impl Normalizer<M, T>, Self>,
        context: &mut Context<M>,
    ) -> Result<(), OverflowError>;

    /// Invoked when the variance is being retrieved for the ADT.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn on_retrieving_variance(
        adt_id: AdtID,
        environment: &Environment<M, T, impl Normalizer<M, T>, Self>,
    ) -> Result<(), OverflowError>;

    /// Invoked when the trait implementation is being resolved.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn on_resolving_trait_implementation(
        trait_id: ID<symbol::Trait>,
        generic_arguments: &GenericArguments<M>,
        environment: &Environment<M, T, impl Normalizer<M, T>, Self>,
    ) -> Result<(), OverflowError>;

    /// Invoked when trait implementation type is resolved during normalization
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn on_trait_implementation_type_resolved(
        trait_implementation_type: ID<TraitImplementationType>,
        environment: &Environment<M, T, impl Normalizer<M, T>, Self>,
        context: &mut Context<M>,
    ) -> Result<(), OverflowError>;
}

/// The default observer that does nothing when observing the record.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

/// The instance of [`NoOp`] observer.
pub const NO_OP: &NoOp = &NoOp;

impl<M: Model, T: State> Observer<M, T> for NoOp {
    fn on_query(
        _: &Record<M>,
        _: &Environment<M, T, impl Normalizer<M, T>, Self>,
        _: &mut Context<M>,
    ) -> Result<(), OverflowError> {
        Ok(())
    }

    fn on_retrieving_variance(
        _: AdtID,
        _: &Environment<M, T, impl Normalizer<M, T>, Self>,
    ) -> Result<(), OverflowError> {
        Ok(())
    }

    fn on_resolving_trait_implementation(
        _: ID<symbol::Trait>,
        _: &GenericArguments<M>,
        _: &Environment<M, T, impl Normalizer<M, T>, Self>,
    ) -> Result<(), OverflowError> {
        Ok(())
    }

    fn on_trait_implementation_type_resolved(
        _: ID<TraitImplementationType>,
        _: &Environment<M, T, impl Normalizer<M, T>, Self>,
        _: &mut Context<M>,
    ) -> Result<(), OverflowError> {
        Ok(())
    }
}
