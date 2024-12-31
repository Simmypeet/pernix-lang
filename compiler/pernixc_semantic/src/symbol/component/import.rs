//! Contains the definition of [`Import`] and its implementation for
//! components.

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use super::{Input, Required};
use crate::{
    arena::ID,
    symbol::{table, Global, Module},
};

/// Represents the import of a module.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Import(HashSet<Global<ID<Module>>>);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(in crate::symbol) struct Map {
    imports: HashMap<Global<ID<Module>>, Import>,
}

impl Input<Import> for Module {
    type Requirement = Required;

    fn get_map(
        representation: &table::Input,
    ) -> &HashMap<Global<ID<Self>>, Import> {
        &representation.import_map.imports
    }
}
