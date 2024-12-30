//! The containing all the componenets used by the symbols.

use std::collections::HashMap;

use super::{table::representation, Global};
use crate::arena::ID;

pub mod accessibility;
pub mod member;
pub mod parent;
pub mod syntax_tree;

/// A tag struct used for signifying that the input component is required for
/// the symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Required;

/// A tag struct used for signifying that the input component is optional for
/// the symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Optional;

/// A trait used for retrieving the input components forr a particular symbol.
pub(super) trait Input<T> {
    /// If true the component must be present in the map for the symbol.
    type Requirement;

    fn get_map(
        representation: &representation::Input,
    ) -> &HashMap<Global<ID<Self>>, T>;
}

impl representation::Input {
    /// Gets the input component of a particular symbol.
    #[allow(private_bounds)]
    pub fn get_input<C, T: Input<C, Requirement = Required> + 'static>(
        &self,
        global_id: Global<ID<T>>,
    ) -> &C {
        T::get_map(self)
            .get(&global_id)
            .unwrap_or_else(|| panic!("{global_id:?} not found"))
    }

    /// Tries to get the optional input component of a particular symbol.
    #[allow(private_bounds)]
    pub fn try_get_input<C, T: Input<C, Requirement = Optional> + 'static>(
        &self,
        global_id: Global<ID<T>>,
    ) -> Option<&C> {
        T::get_map(self).get(&global_id)
    }
}
