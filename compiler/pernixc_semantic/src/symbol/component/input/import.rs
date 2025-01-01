//! Contains the definition of [`Import`] and its implementation for
//! components.

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use super::{Input, Required};
use crate::{
    arena::ID,
    symbol::{Global, Module},
};

/// Represents the import of a module.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Import(HashSet<Global<ID<Module>>>);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct Map {
    pub(super) imports: HashMap<Global<ID<Module>>, Import>,
}

impl Input<Import> for Module {
    type Requirement = Required;
    type ID = ID<Self>;

    fn get_map(
        representation: &super::Map,
    ) -> &HashMap<Global<ID<Self>>, Import> {
        &representation.import.imports
    }

    fn get_map_mut(
        representation: &mut super::Map,
    ) -> &mut HashMap<Global<ID<Self>>, Import> {
        &mut representation.import.imports
    }
}
