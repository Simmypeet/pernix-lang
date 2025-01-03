//! Contains the definition of [`Representation`] and [`Table`]: the semantic
//! representation of the program.

use std::collections::{HashMap, HashSet};

use derive_new::new;
use pernixc_component::Storage;
use pernixc_syntax::syntax_tree::AccessModifier;
use serde::{Deserialize, Serialize};
pub use target::AddTargetError;

use crate::component::{Accessibility, Parent, SymbolKind};

mod target;

/// Represents an identifier for a target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct TargetID(usize);

impl TargetID {
    /// The core target.
    pub const CORE: Self = Self(0);
}

/// Represents an identifier for a symbol within a target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct ID(usize);

impl ID {
    /// The modue root symbol.
    pub const ROOT_MODULE: Self = Self(0);
}

/// Represents an identifier for a symbol across the targets.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    new,
)]
pub struct GlobalID {
    /// The target in which the symbol is defined.
    pub target_id: TargetID,

    /// The identifier of the symbol within the target.
    pub id: ID,
}

/// Represents a compilation target symbola.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Target {
    generated_ids: usize,
    linked_targets: HashSet<TargetID>,
}

impl Target {
    /// Generates an ID for a new symbol that will be defined within the target.
    pub fn generate_id(&mut self) -> ID {
        let id = self.generated_ids;
        self.generated_ids += 1;
        ID(id)
    }
}

/// Represents the semantic representation of the program.
#[derive(Debug)]
pub struct Representation {
    storage: Storage<GlobalID>,

    targets_by_id: HashMap<TargetID, Target>,
    targets_by_name: HashMap<String, TargetID>,
}

impl Representation {
    /// Returns the [`ID`]` that is the module and closest to the given
    /// [`GlobalID`] (including itself).
    ///
    /// # Panics
    ///
    /// If the given [`GlobalID`] is not a module.
    pub fn get_closet_module_id(&self, mut id: GlobalID) -> ID {
        loop {
            if *self.storage.get::<SymbolKind>(id).unwrap()
                == SymbolKind::Module
            {
                return id.id;
            }

            id = GlobalID::new(
                id.target_id,
                **self.storage.get::<Parent>(id).unwrap(),
            );
        }
    }

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Panics
    ///
    /// If the parent item ID is not found.
    pub fn create_accessibility(
        &self,
        parent_id: GlobalID,
        access_modifier: &AccessModifier,
    ) -> Accessibility {
        match access_modifier {
            AccessModifier::Public(_) => Accessibility::Public,
            AccessModifier::Private(_) => {
                let parent_module_id = self.get_closet_module_id(parent_id);
                Accessibility::Scoped(parent_module_id)
            }
            AccessModifier::Internal(_) => {
                Accessibility::Scoped(ID::ROOT_MODULE)
            }
        }
    }
}
