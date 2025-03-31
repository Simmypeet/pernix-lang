//! Contains the definition of [`Table`], the final data structure that will be
//! used to codegen.

use std::{
    any::Any,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use component::Derived;
use derive_more::{Deref, DerefMut};
use derive_new::new;
use diagnostic::Diagnostic;
use getset::Getters;
use parking_lot::Mutex;
use pernixc_handler::Handler;
use pernixc_storage::{serde::Reflector, ArcTrait, GetMutError, Storage};
use pernixc_syntax::syntax_tree::AccessModifier;
use query::{AnySendSync, Builder, Context};
use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};
pub use target::AddTargetError;

use crate::component::{
    Accessibility, HierarchyRelationship, Implemented, Implements, Import,
    Input, InputMut, Member, Name, Parent, SymbolKind,
};

pub mod component;
pub mod deserialization;
pub mod diagnostic;
pub mod query;
pub mod reflector;
pub mod resolution;

mod arbitrary;
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
pub struct TargetID(pub u64);

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
pub struct ID(pub usize);

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

    /// Returns an iterator of all symbols in this target.
    pub fn all_symbols(&self) -> impl Iterator<Item = ID> + '_ {
        (0..self.generated_ids).map(ID)
    }
}

/// Represents the semantic representation of the program.
#[derive(Debug)]
pub struct Representation {
    storage: Storage<GlobalID, ArcTrait>,
    targets_by_id: HashMap<TargetID, Target>,
    targets_by_name: HashMap<String, TargetID>,
}

impl Default for Representation {
    fn default() -> Self {
        let mut repr = Self {
            storage: Storage::default(),
            targets_by_id: HashMap::default(),
            targets_by_name: HashMap::default(),
        };

        assert!(repr
            .targets_by_id
            .insert(TargetID::CORE, Target {
                generated_ids: 0,
                linked_targets: HashSet::new()
            })
            .is_none());
        assert!(repr
            .targets_by_name
            .insert("core".to_string(), TargetID::CORE)
            .is_none());

        repr
    }
}

struct SerializableRepresentation<'a, T, E> {
    representation: &'a Representation,
    reflector: &'a Reflector<GlobalID, ArcTrait, T, E>,
}

impl<T: Serialize, E: std::fmt::Display + 'static> Serialize
    for SerializableRepresentation<'_, T, E>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // for the of simplicity, we serialize as map
        let mut state = serializer.serialize_struct("Representation", 3)?;

        state.serialize_field(
            "storage",
            &self.representation.storage.as_serializable(self.reflector),
        )?;
        state.serialize_field(
            "targets_by_id",
            &self.representation.targets_by_id,
        )?;
        state.serialize_field(
            "targets_by_name",
            &self.representation.targets_by_name,
        )?;

        state.end()
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    representation: &'a Representation,
    current_id: Option<ID>,
    target_id: TargetID,
}

impl Iterator for ScopeWalker<'_> {
    type Item = ID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next = self
                    .representation
                    .get::<Parent>(GlobalID::new(self.target_id, current_id))
                    .parent;

                self.current_id = next;
                Some(current_id)
            }
            None => None,
        }
    }
}

impl Representation {
    /// Gets the **input** component of the given type from the symbol with the
    /// given ID.
    #[must_use]
    pub fn get<T: Input + Any + Send + Sync>(&self, id: GlobalID) -> Arc<T> {
        self.storage.get_cloned::<T>(id).unwrap_or_else(|| {
            panic!(
                "the symbol with the ID {id:?} is not found or it doesn't \
                 have a component of type name {}",
                std::any::type_name::<T>()
            )
        })
    }

    /// Gets the mutable **input** component of the given type from the symbol
    /// with the given ID.
    ///
    /// # Errors
    ///
    /// - [`GetMutError::NotFound`]: if the component or symbol is not found.
    /// - [`GetMutError::ArcNotUnique`]: if the component is shared and cannot
    ///   be uniquely borrowed.
    pub fn get_mut<T: InputMut + Any>(
        &self,
        id: GlobalID,
    ) -> Result<impl std::ops::DerefMut<Target = T> + '_, GetMutError> {
        self.storage.get_mut::<T>(id)
    }

    /// Checks whether the symbol with the given ID has the component of the
    /// given type.
    #[must_use]
    pub fn has<T: Any>(&self, id: GlobalID) -> bool {
        self.storage.get::<T>(id).is_some()
    }

    /// Creates a [`Library`] representation for serialization.
    #[must_use]
    pub const fn as_library<'a, T, E>(
        &'a self,
        compilation_meta_data: &'a CompilationMetaData,
        reflector: &'a Reflector<GlobalID, ArcTrait, T, E>,
    ) -> Library<'a, T, E> {
        Library { representation: self, compilation_meta_data, reflector }
    }

    /// Adds a component to the storage.
    #[must_use]
    pub fn add_component<T: Any + Send + Sync>(
        &self,
        id: GlobalID,
        component: T,
    ) -> bool {
        self.storage.add_component(id, component)
    }

    /// Gets the qualified name of the symbol such as `module::function`.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    #[must_use]
    pub fn get_qualified_name(&self, mut id: GlobalID) -> String {
        let mut qualified_name = String::new();

        loop {
            let current_name = self.get::<Name>(id);

            if qualified_name.is_empty() {
                qualified_name.push_str(&current_name);
            } else {
                qualified_name.insert_str(0, "::");
                qualified_name.insert_str(0, &current_name);
            }

            if let Some(parent_id) = self.get::<Parent>(id).parent {
                id = GlobalID::new(id.target_id, parent_id);
            } else {
                break;
            }
        }

        qualified_name
    }

    /// Returns the [`ID`] that is the module and closest to the given
    /// [`GlobalID`] (including itself).
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    #[must_use]
    pub fn get_closet_module_id(&self, mut id: GlobalID) -> ID {
        loop {
            if *self.get::<SymbolKind>(id) == SymbolKind::Module {
                return id.id;
            }

            id = GlobalID::new(
                id.target_id,
                self.get::<Parent>(id)
                    .parent
                    .expect("should always have a parent "),
            );
        }
    }

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Returns
    ///
    /// Returns `None` if the `id` is not found.
    #[must_use]
    pub fn create_accessibility(
        &self,
        parent_id: GlobalID,
        access_modifier: &AccessModifier,
    ) -> Option<Accessibility> {
        match access_modifier {
            AccessModifier::Public(_) => Some(Accessibility::Public),
            AccessModifier::Private(_) => {
                let parent_module_id = self.get_closet_module_id(parent_id);
                Some(Accessibility::Scoped(parent_module_id))
            }
            AccessModifier::Internal(_) => {
                Some(Accessibility::Scoped(ID::ROOT_MODULE))
            }
        }
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
    /// given [`GlobalID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub const fn scope_walker(&self, id: GlobalID) -> ScopeWalker {
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
                Some(
                    self.symbol_hierarchy_relationship(
                        target_id, first, second,
                    ),
                )
            }
        }
    }

    /// Gets the [`Accessibility`] of the given symbol.
    #[must_use]
    pub fn get_accessibility(&self, id: GlobalID) -> Accessibility {
        match *self.get::<SymbolKind>(id) {
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
            | SymbolKind::ExternFunction
            | SymbolKind::Function => *self.get::<Accessibility>(id),

            // based on the parent's accessibility
            SymbolKind::TraitImplementationFunction
            | SymbolKind::TraitImplementationType
            | SymbolKind::TraitImplementationConstant
            | SymbolKind::Variant => self.get_accessibility(GlobalID::new(
                id.target_id,
                self.get::<Parent>(id).parent.unwrap(),
            )),

            SymbolKind::PositiveTraitImplementation
            | SymbolKind::NegativeTraitImplementation
            | SymbolKind::PositiveMarkerImplementation
            | SymbolKind::NegativeMarkerImplementation
            | SymbolKind::AdtImplementation => {
                self.get_accessibility(**self.get::<Implements>(id))
            }
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
    ) -> Option<GlobalID> {
        let symbol_kind = *self.get::<SymbolKind>(id);
        if let Some(Some(test)) = symbol_kind
            .has_member()
            .then(|| self.get::<Member>(id).0.get(member_name).copied())
        {
            return Some(GlobalID::new(id.target_id, test));
        }

        match (symbol_kind == SymbolKind::Module, symbol_kind.is_adt()) {
            (true, false) => {
                self.get::<Import>(id).0.get(member_name).map(|x| x.id)
            }

            // serach for the member of implementations
            (false, true) => {
                let implements = self.get::<Implemented>(id);

                for implementation_id in implements.iter().copied() {
                    if let Some(id) =
                        self.get::<Member>(implementation_id).get(member_name)
                    {
                        return Some(GlobalID::new(
                            implementation_id.target_id,
                            *id,
                        ));
                    }
                }

                None
            }

            _ => None,
        }
    }

    /// Similar to [`Representation::is_accessible_from`] but with the global ID
    /// and global accessibility.
    #[must_use]
    pub fn is_accessible_from_globally(
        &self,
        referring_site: GlobalID,
        referred_accessibility: GlobalAccessibility,
    ) -> bool {
        match referred_accessibility {
            GlobalAccessibility::Public => true,

            GlobalAccessibility::Scoped(module_id) => {
                if module_id.target_id != referring_site.target_id {
                    return false;
                }

                let referring_site_module_id =
                    self.get_closet_module_id(referring_site);

                matches!(
                    self.symbol_hierarchy_relationship(
                        referring_site.target_id,
                        module_id.id,
                        referring_site_module_id,
                    ),
                    HierarchyRelationship::Parent
                        | HierarchyRelationship::Equivalent
                )
            }
        }
    }

    /// Determines whether the given `referred` is accessible from the
    /// `referring_site` as if the `referred` has the given
    /// `referred_accessibility`.
    #[must_use]
    pub fn is_accessible_from(
        &self,
        referring_site: ID,
        referred_target_id: TargetID,
        referred_accessibility: Accessibility,
    ) -> bool {
        match referred_accessibility {
            Accessibility::Public => true,

            Accessibility::Scoped(module_id) => {
                let referring_site_module_id = self.get_closet_module_id(
                    GlobalID::new(referred_target_id, referring_site),
                );

                matches!(
                    self.symbol_hierarchy_relationship(
                        referred_target_id,
                        module_id,
                        referring_site_module_id,
                    ),
                    HierarchyRelationship::Parent
                        | HierarchyRelationship::Equivalent
                )
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
    ) -> bool {
        let referred_accessibility = self.get_accessibility(referred);

        self.is_accessible_from(
            referring_site.id,
            referred.target_id,
            referred_accessibility,
        )
    }
}

/// Represents the semantic representation of the program.
#[derive(Deref, DerefMut, Getters)]
pub struct Table {
    #[deref]
    #[deref_mut]
    representation: Representation,

    query_context: Mutex<Context>,

    /// The handler that will be used to report diagnostics during the
    /// computation of components happened in the table.
    #[get = "pub"]
    handler: Arc<dyn Handler<Box<dyn Diagnostic>>>,
}

impl Table {
    /// Creates a new empty [`Table`] with the given handler.
    #[must_use]
    pub fn new(handler: Arc<dyn Handler<Box<dyn Diagnostic>>>) -> Self {
        Self {
            representation: Representation::default(),
            query_context: Mutex::new(Context::default()),
            handler,
        }
    }

    /// Sets the builder for the given derived component type.
    ///
    /// # Returns
    ///
    /// Returns `true` if there's no existing builder for the given type.
    /// Otherwise, returns `false` and the builder is not set.
    #[must_use]
    pub fn set_builder<T: Derived, B: Builder<T>>(
        &mut self,
        builder: B,
    ) -> bool {
        self.query_context.get_mut().set_builder::<T, B>(builder)
    }

    /// Sets the builder for the given derived component type.
    ///
    /// This will overwrite the existing builder for the given type and returns
    /// the previous builder if there's any.
    pub fn set_builder_overwrite<T: Derived, B: Builder<T>>(
        &mut self,
        builder: B,
    ) -> Option<Arc<dyn AnySendSync>> {
        self.query_context.get_mut().set_builder_overwrite::<T, B>(builder)
    }

    /// Sets the handler and returns the previous handler.
    pub fn set_handler(
        &mut self,
        handler: Arc<dyn Handler<Box<dyn Diagnostic>>>,
    ) -> Arc<dyn Handler<Box<dyn Diagnostic>>> {
        std::mem::replace(&mut self.handler, handler)
    }
}

impl std::fmt::Debug for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Table")
            .field("representation", &self.representation)
            .field("query_context", &self.query_context)
            .finish_non_exhaustive()
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
#[derive(Copy)]
pub struct Library<'a, T, E> {
    representation: &'a Representation,
    compilation_meta_data: &'a CompilationMetaData,
    reflector: &'a Reflector<GlobalID, ArcTrait, T, E>,
}

impl<T: std::fmt::Debug, E: std::fmt::Debug> std::fmt::Debug
    for Library<'_, T, E>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Library")
            .field("representation", &self.representation)
            .field("compilation_meta_data", &self.compilation_meta_data)
            .finish_non_exhaustive()
    }
}

impl<T, E> Clone for Library<'_, T, E> {
    fn clone(&self) -> Self {
        Self {
            representation: self.representation,
            compilation_meta_data: self.compilation_meta_data,
            reflector: self.reflector,
        }
    }
}

impl<T: Serialize, E: std::fmt::Display + 'static> Serialize
    for Library<'_, T, E>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Library", 2)?;

        state.serialize_field(
            "representation",
            &SerializableRepresentation {
                representation: self.representation,
                reflector: self.reflector,
            },
        )?;
        state.serialize_field(
            "compilation_meta_data",
            &self.compilation_meta_data,
        )?;

        state.end()
    }
}

/// Contaains the display object that requires the table.
///
/// Primarily used for implementing [`std::fmt::Display`] trait.
#[derive(Debug, Clone, Copy)]
pub struct DisplayObject<'a, D: ?Sized> {
    /// The table in which the display object will refer to.
    pub table: &'a Table,

    /// The display object that requires the table.
    pub display: &'a D,
}

impl<Error: Display + ?Sized> std::fmt::Display for DisplayObject<'_, Error> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display.fmt(self.table, f)
    }
}

/// Similar to [`std::fmt::Display`] but with the table in which the error
/// occurred.
pub trait Display {
    #[allow(missing_docs, clippy::missing_errors_doc)]
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result;
}

/// An ID used to refer to a particular symbol defined in a particular
/// **parent** symbol.
///
/// This can refer to anything that is defined in a parent symbol depending on
/// the context.
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
pub struct MemberID<ChildID> {
    /// Symbol ID of the parent symbol.
    pub parent: GlobalID,

    /// Symbol ID of the child symbol.
    pub id: ChildID,
}

/// An error returned by the [`Representation::merge_accessibility_down`]
/// function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
#[allow(missing_docs)]
pub enum MergeAccessibilityError {
    /// The accessibility objects contain an invalid global id.
    InvalidModuleID,

    /// Two accessibility objects are scoped to different modules.
    Unrelated,
}

/// Similar to regular [`Accessibility`] but with the global ID on
/// [`GlobalAccessibility::Scoped`] variant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub enum GlobalAccessibility {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(GlobalID),
}

impl Representation {
    /// Merges two accessibilities down the hierarchy.
    ///
    /// The resulting accessibility is the least accessible of the two given
    /// accessibilities.
    ///
    /// # Errors
    ///
    /// See [`MergeAccessibilityError`] for more information.
    pub fn merge_accessibility_down(
        &self,
        first: GlobalAccessibility,
        second: GlobalAccessibility,
    ) -> Result<GlobalAccessibility, MergeAccessibilityError> {
        Ok(match (first, second) {
            (GlobalAccessibility::Public, GlobalAccessibility::Public) => {
                GlobalAccessibility::Public
            }
            (
                GlobalAccessibility::Public,
                GlobalAccessibility::Scoped(scope),
            )
            | (
                GlobalAccessibility::Scoped(scope),
                GlobalAccessibility::Public,
            ) => GlobalAccessibility::Scoped(scope),

            (
                GlobalAccessibility::Scoped(first),
                GlobalAccessibility::Scoped(second),
            ) => {
                if first.target_id != second.target_id {
                    return Err(MergeAccessibilityError::Unrelated);
                }

                match self.symbol_hierarchy_relationship(
                    first.target_id,
                    first.id,
                    second.id,
                ) {
                    HierarchyRelationship::Parent => {
                        GlobalAccessibility::Scoped(second)
                    }
                    HierarchyRelationship::Child
                    | HierarchyRelationship::Equivalent => {
                        GlobalAccessibility::Scoped(first)
                    }
                    HierarchyRelationship::Unrelated => {
                        return Err(MergeAccessibilityError::Unrelated)
                    }
                }
            }
        })
    }
}

impl Representation {
    /// Retrieves the [`Target`] with the given ID.
    #[must_use]
    pub fn get_target(&self, target_id: TargetID) -> Option<&Target> {
        self.targets_by_id.get(&target_id)
    }
}

#[cfg(test)]
mod test;
