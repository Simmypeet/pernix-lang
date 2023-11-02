//! Contains logic related to trait resolution.

use super::{Mapping, Substitution};
use crate::{
    arena::ID,
    entity::{constant::Constant, r#type::Type, GenericArguments, Model},
    symbol::{self, ImplementsID, Symbolic, Trait},
    table::Table,
};

/// Represents the implements resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Implements<'a> {
    /// Deduced trait implements's generic parameters substitution.
    pub deduced_unification: Substitution<'a, Symbolic>,

    /// The resolved implements id.
    pub implements_id: ImplementsID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementsKey {
    Positive(ID<symbol::Implements>),
    Negative(ID<symbol::ImplementsSignature>),
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
    /// [`Implements`] that are resolved.
    #[must_use]
    pub fn resolve_implements<'a, S: Model>(
        &self,
        trait_id: ID<Trait>,
        mapping: &Mapping<'a, S>,
    ) -> Option<Vec<Implements<'a>>> {
        let traits = self.traits().get(trait_id)?;

        for (key, signature) in traits
            .implements
            .iter()
            .enumerate()
            .map(|(k, v)| (ImplementsKey::Positive(ID::new(k)), &v.signature))
            .chain(
                traits
                    .negative_implements
                    .iter()
                    .enumerate()
                    .map(|(k, v)| (ImplementsKey::Negative(ID::new(k)), &v.signature)),
            )
        {}

        todo!()
    }
}

impl<S: Model> Type<S> {
    /// Checks if the type is definite (with considering the mapping premises).
    #[must_use]
    pub fn is_definite(&self, mapping_premises: &Mapping<S>, table: &Table) -> bool {
        match self {
            Self::Primitive(_) => true,
            Self::Inference(_) => false,
            Self::Algebraic(adt) => adt.generic_arguments.is_definite(mapping_premises, table),
            Self::Pointer(pointer) => pointer.pointee.is_definite(mapping_premises, table),
            Self::Reference(reference) => reference.pointee.is_definite(mapping_premises, table),
            Self::Array(array) => {
                array.element.is_definite(mapping_premises, table)
                    && array.length.is_definite(mapping_premises, table)
            }
            Self::TraitMember(_) => todo!(),
            Self::Parameter(_) => todo!(),
            Self::Tuple(_) => todo!(),
        }
    }
}

impl<S: Model> Constant<S> {
    /// Checks if the type is definite (with considering the mapping premises).
    #[must_use]
    pub fn is_definite(&self, mapping_premises: &Mapping<S>, table: &Table) -> bool {
        match self {
            Self::Primitive(_) => true,
            Self::Inference(_) => false,
            Self::Struct(_) => todo!(),
            Self::Enum(_) => todo!(),
            Self::Array(_) => todo!(),
            Self::TraitMember(_) => todo!(),
            Self::Parameter(_) => todo!(),
            Self::Tuple(_) => todo!(),
        }
    }
}

impl<S: Model> GenericArguments<S> {
    /// Checks if the type is definite (with considering the mapping premises).
    #[must_use]
    pub fn is_definite(&self, mapping_premises: &Mapping<S>, table: &Table) -> bool {
        self.types
            .iter()
            .all(|ty| ty.is_definite(mapping_premises, table))
            && self
                .constants
                .iter()
                .all(|constant| constant.is_definite(mapping_premises, table))
    }
}
