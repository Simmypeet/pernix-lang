//! Contains the definition of [`Import`] and its implementation for
//! components.

use std::collections::HashMap;

use pernixc_base::source_file::Span;
use serde::{Deserialize, Serialize};

use super::{Input, Required};
use crate::{
    arena::ID,
    symbol::{Global, Module, ModuleMemberID},
};

/// Represents the using of a module member.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Using {
    /// The ID of the module member that is being used.
    pub module_member_id: Global<ModuleMemberID>,

    /// The span of the using statement.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents the import of a module.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Import(HashMap<String, Using>);

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
