//! Contains the definition of [`Representation`] and [`Table`].
//!
//! The [`Table`] is the final representation of the semantic analysis phase.

use std::{
    any::Any,
    collections::{HashMap, HashSet},
    ops::Deref,
};

use derive_more::{Deref, DerefMut};
use derive_new::new;
use pernixc_component::Storage;
use pernixc_syntax::syntax_tree::AccessModifier;
use serde::{ser::SerializeMap, Deserialize, Serialize, Serializer};
pub use target::AddTargetError;

use crate::component::{
    Accessibility, HierarchyRelationship, Implemented, Implements, Import,
    Member, Name, Parent, SymbolKind,
};

pub mod deserialization;
pub mod diagnostic;
pub mod reflector;
pub mod resolution;

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
pub struct TargetID(u64);

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
#[derive(Debug, Default)]
pub struct Representation {
    storage: Storage<GlobalID>,
    targets_by_id: HashMap<TargetID, Target>,
    targets_by_name: HashMap<String, TargetID>,
}

impl Serialize for Representation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // for the of simplicity, we serialize as map
        let mut map = serializer.serialize_map(None)?;
        let reflector = Table::reflector();

        map.serialize_entry(
            "storage",
            &self.storage.as_serializable(&reflector),
        )?;
        map.serialize_entry("targets_by_id", &self.targets_by_id)?;
        map.serialize_entry("targets_by_name", &self.targets_by_name)?;

        map.end()
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone, Copy)]
pub struct ScopeWalker<'a> {
    representation: &'a Representation,
    current_id: Option<ID>,
    target_id: TargetID,
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next = self
                    .representation
                    .storage
                    .get::<Parent>(GlobalID::new(self.target_id, current_id))
                    .map(|x| **x);

                self.current_id = next;
                Some(current_id)
            }
            None => None,
        }
    }
}

/// The error type returned by [`Representation::get_member_of()`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetMemberError {
    #[error("the given item ID is not valid")]
    InvalidID,
    #[error("the member with the given name is not found")]
    MemberNotFound,
}

impl Representation {
    /// Gets the component of the given symbol.
    pub fn get<C: Any>(
        &self,
        id: GlobalID,
    ) -> Option<impl Deref<Target = C> + '_> {
        self.storage.get::<C>(id)
    }

    /// Creates a [`Library`] representation for serialization.
    pub fn as_library<'a>(
        &'a self,
        compilation_meta_data: &'a CompilationMetaData,
    ) -> Library {
        Library { representation: self, compilation_meta_data }
    }

    /// Gets the qualified name of the symbol such as `module::function`.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    pub fn get_qualified_name(&self, mut id: GlobalID) -> Option<String> {
        let mut qualified_name = String::new();

        loop {
            let current_name = self.storage.get::<Name>(id)?;

            if qualified_name.is_empty() {
                qualified_name.push_str(&current_name);
            } else {
                qualified_name.insert_str(0, "::");
                qualified_name.insert_str(0, &current_name);
            }

            if let Some(parent_id) = self.storage.get::<Parent>(id) {
                id = GlobalID::new(id.target_id, **parent_id);
            } else {
                break;
            }
        }

        Some(qualified_name)
    }

    /// Returns the [`ID`]` that is the module and closest to the given
    /// [`GlobalID`] (including itself).
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    pub fn get_closet_module_id(&self, mut id: GlobalID) -> Option<ID> {
        loop {
            if *self.storage.get::<SymbolKind>(id)? == SymbolKind::Module {
                return Some(id.id);
            }

            id = GlobalID::new(id.target_id, **self.storage.get::<Parent>(id)?);
        }
    }

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    pub fn create_accessibility(
        &self,
        parent_id: GlobalID,
        access_modifier: &AccessModifier,
    ) -> Option<Accessibility> {
        match access_modifier {
            AccessModifier::Public(_) => Some(Accessibility::Public),
            AccessModifier::Private(_) => {
                let parent_module_id = self.get_closet_module_id(parent_id)?;
                Some(Accessibility::Scoped(parent_module_id))
            }
            AccessModifier::Internal(_) => {
                Some(Accessibility::Scoped(ID::ROOT_MODULE))
            }
        }
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
    /// given [`ItemID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub fn scope_walker(&self, id: GlobalID) -> ScopeWalker {
        ScopeWalker {
            representation: self,
            current_id: Some(id.id),
            target_id: id.target_id,
        }
    }

    /// Computes the [`HierarchyRelationship`] between the two given item IDs.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first` symbol.
    #[must_use]
    pub fn symbol_hierarchy_relationship(
        &self,
        target_id: TargetID,
        first: ID,
        second: ID,
    ) -> HierarchyRelationship {
        // the two symbols are the same.
        if first == second {
            return HierarchyRelationship::Equivalent;
        }

        for first_parent in self.scope_walker(GlobalID::new(target_id, first)) {
            if first_parent == second {
                return HierarchyRelationship::Child;
            }
        }

        for second_parent in self.scope_walker(GlobalID::new(target_id, second))
        {
            if second_parent == first {
                return HierarchyRelationship::Parent;
            }
        }

        HierarchyRelationship::Unrelated
    }

    /// Computes the [`HierarchyRelationship`] between the two given
    /// accessibilities.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first`
    /// accessibility.
    ///
    /// # Returns
    ///
    /// Returns `None` if either `first` or `second` contains an invalid
    /// module ID.
    #[must_use]
    pub fn accessibility_hierarchy_relationship(
        &self,
        target_id: TargetID,
        first: Accessibility,
        second: Accessibility,
    ) -> Option<HierarchyRelationship> {
        match (first, second) {
            (Accessibility::Public, Accessibility::Public) => {
                Some(HierarchyRelationship::Equivalent)
            }
            (Accessibility::Public, Accessibility::Scoped(_)) => {
                Some(HierarchyRelationship::Parent)
            }
            (Accessibility::Scoped(_), Accessibility::Public) => {
                Some(HierarchyRelationship::Child)
            }
            (Accessibility::Scoped(first), Accessibility::Scoped(second)) => {
                Some(self.symbol_hierarchy_relationship(
                    target_id,
                    first.into(),
                    second.into(),
                ))
            }
        }
    }

    /// Gets the [`Accessibility`] of the given symbol.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    pub fn get_accessibility(&self, id: GlobalID) -> Option<Accessibility> {
        match *self.storage.get::<SymbolKind>(id)? {
            SymbolKind::Module
            | SymbolKind::Struct
            | SymbolKind::Trait
            | SymbolKind::Enum
            | SymbolKind::Type
            | SymbolKind::Constant
            | SymbolKind::TraitType
            | SymbolKind::TraitFunction
            | SymbolKind::TraitConstant
            | SymbolKind::Marker
            | SymbolKind::AdtImplementationFunction
            | SymbolKind::Function => {
                self.storage.get::<Accessibility>(id).as_deref().copied()
            }

            // based on the parent's accessibility
            SymbolKind::TraitImplementationFunction
            | SymbolKind::TraitImplementationType
            | SymbolKind::TraitImplementationConstant
            | SymbolKind::Variant => self.get_accessibility(GlobalID::new(
                id.target_id,
                **self.storage.get::<Parent>(id).unwrap(),
            )),

            SymbolKind::PositiveTraitImplementation
            | SymbolKind::NegativeTraitImplementation
            | SymbolKind::PositiveMarkerImplementation
            | SymbolKind::NegativeMarkerImplementation
            | SymbolKind::AdtImplementation => self.get_accessibility(
                **self.storage.get::<Implements>(id).unwrap(),
            ),
        }
    }

    /// Gets the [`GlobalID`] of the symbol with the given sequence of qualified
    /// name.
    pub fn get_by_qualified_name<'a>(
        &self,
        qualified_names: impl IntoIterator<Item = &'a str>,
    ) -> Option<GlobalID> {
        let mut current_id: Option<GlobalID> = None;

        for name in qualified_names {
            match current_id {
                Some(searched_in_item_id) => {
                    current_id = Some(
                        self.storage
                            .get::<Member>(searched_in_item_id)
                            .and_then(|x| {
                                x.get(name).copied().map(|x| {
                                    GlobalID::new(
                                        searched_in_item_id.target_id,
                                        x,
                                    )
                                })
                            })?,
                    );
                }
                None => {
                    current_id = self
                        .targets_by_name
                        .get(name)
                        .map(|&x| GlobalID::new(x, ID::ROOT_MODULE));
                }
            }
        }

        current_id
    }

    /// Searches for a member with the given name in the given item ID.
    #[must_use]
    pub fn get_member_of(
        &self,
        id: GlobalID,
        member_name: &str,
    ) -> Result<GlobalID, GetMemberError> {
        if let Some(via_member) =
            self.get::<Member>(id).and_then(|x| x.get(member_name).copied())
        {
            return Ok(GlobalID::new(id.target_id, via_member));
        }

        let symbol_kind =
            self.get::<SymbolKind>(id).ok_or(GetMemberError::InvalidID)?;

        match (*symbol_kind == SymbolKind::Module, symbol_kind.is_adt()) {
            (true, false) => {
                let imports =
                    self.get::<Import>(id).ok_or(GetMemberError::InvalidID)?;

                imports
                    .get(member_name)
                    .map(|x| x.id)
                    .ok_or(GetMemberError::MemberNotFound)
            }

            // serach for the member of implementations
            (false, true) => {
                let implements = self
                    .get::<Implemented>(id)
                    .ok_or(GetMemberError::InvalidID)?;

                for implementation_id in implements.iter().copied() {
                    let Some(member) = self.get::<Member>(implementation_id)
                    else {
                        continue;
                    };

                    if let Some(id) = member.get(member_name) {
                        return Ok(GlobalID::new(
                            implementation_id.target_id,
                            *id,
                        ));
                    }
                }

                Err(GetMemberError::MemberNotFound)
            }

            _ => Err(GetMemberError::MemberNotFound),
        }
    }

    /// Determines whether the given `referred` is accessible from the
    /// `referring_site` as if the `referred` has the given
    /// `referred_accessibility`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    #[must_use]
    pub fn is_accessible_from(
        &self,
        referring_site: ID,
        referred_target_id: TargetID,
        referred_accessibility: Accessibility,
    ) -> Option<bool> {
        match referred_accessibility {
            Accessibility::Public => Some(true),

            Accessibility::Scoped(module_id) => {
                let referring_site_module_id = self.get_closet_module_id(
                    GlobalID::new(referred_target_id, referring_site),
                )?;

                Some(matches!(
                    self.symbol_hierarchy_relationship(
                        referred_target_id,
                        module_id.into(),
                        referring_site_module_id.into(),
                    ),
                    HierarchyRelationship::Parent
                        | HierarchyRelationship::Equivalent
                ))
            }
        }
    }

    /// Checks if the `referred` is accessible from the `referring_site`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    #[must_use]
    pub fn symbol_accessible(
        &self,
        referring_site: GlobalID,
        referred: GlobalID,
    ) -> Option<bool> {
        let referred_accessibility = self.get_accessibility(referred)?;

        self.is_accessible_from(
            referring_site.id,
            referred.target_id,
            referred_accessibility,
        )
    }
}

/// Represents the semantic representation of the program.
#[derive(Debug, Default, Deref, DerefMut)]
pub struct Table {
    #[deref]
    #[deref_mut]
    representation: Representation,
}

impl Serialize for Table {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.representation.serialize(serializer)
    }
}

/// Contains the information of what and how program is being compiled.
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
pub struct CompilationMetaData {
    /// The target that is being compiled.
    pub target_id: TargetID,
}

/// The serialization representation of the [`Table`] as a library.
#[derive(Debug, Clone, Copy)]
pub struct Library<'a> {
    representation: &'a Representation,
    compilation_meta_data: &'a CompilationMetaData,
}

impl Serialize for Library<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // for the of simplicity, we serialize as map
        let mut map = serializer.serialize_map(None)?;

        map.serialize_entry("representation", &self.representation)?;
        map.serialize_entry(
            "compilation_meta_data",
            &self.compilation_meta_data,
        )?;

        map.end()
    }
}
