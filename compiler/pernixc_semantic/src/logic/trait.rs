//! Contains logic related to trait resolution.

use super::{Mapping, Substitution};
use crate::{
    arena::ID,
    entity::Model,
    symbol::{self, ImplementationID, Symbolic, Trait},
    table::Table,
};

/// Represents the implements resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implementation {
    /// Deduced trait implements's generic parameters substitution.
    pub deduced_unification: Substitution<Symbolic>,

    /// The resolved implements id.
    pub implements_id: ImplementationID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementationKey {
    Positive(ID<symbol::Implementation>),
    Negative(ID<symbol::ImplementationSignature>),
}

impl Table {
    /// Resolves for the implements of the given trait.
    ///
    /// # Parameters
    ///
    /// * `trait_id`: The trait to resolve for.
    /// * `generic_arguments`: The generic arguments of the trait.
    /// * `mapping`: The maping premises defined in the current context.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the input parameters are invalid such as the trait ID is not in the
    /// table, or the generic arguments are not valid for the trait. Otherwise, returns a vector of
    /// [`Implementation`] that are resolved.
    #[must_use]
    pub fn resolve_implements<S: Model>(
        &self,
        trait_id: ID<Trait>,
        mapping: &Mapping<S>,
    ) -> Option<Vec<Implementation>> {
        let traits = self.traits().get(trait_id)?;

        for (key, signature) in traits
            .implementations
            .iter()
            .enumerate()
            .map(|(k, v)| (ImplementationKey::Positive(ID::new(k)), &v.signature))
            .chain(
                traits
                    .negative_implements
                    .iter()
                    .enumerate()
                    .map(|(k, v)| (ImplementationKey::Negative(ID::new(k)), &v.signature)),
            )
        {}

        todo!()
    }
}
