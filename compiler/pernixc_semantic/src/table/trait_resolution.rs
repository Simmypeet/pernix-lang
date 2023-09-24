use super::{resolution, Table};
use crate::symbol::{LocalSubstitution, TraitRef};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitSubstitution {
    pub trait_ref: TraitRef,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, thiserror::Error)]
#[error("trait substitution is not concrete and cannot be resolved")]
pub struct NonConcreteSubstitutionError;

impl Table {
    /// Resolves a trait substitution to a concrete implements.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the trait substitution is not concrete.
    pub fn resolve_trait_implements(
        &self,
        _trait_substitution: &TraitSubstitution,
    ) -> Result<Option<resolution::Implements>, NonConcreteSubstitutionError> {
        todo!()
    }
}
