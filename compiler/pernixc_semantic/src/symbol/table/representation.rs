//! Contains the definition of [`Representation`] and methods.

use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap},
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

use building::drafting::Drafter;
use getset::Getters;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use paste::paste;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{
    item::UsingKind, target::Target, AccessModifier, ConnectedList,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::{Building, State, Suboptimal, Success, Table};
use crate::{
    arena::{Arena, ID},
    error::{self, ConflictingUsing, ExpectModule, SymbolNotFound},
    symbol::{
        self, Accessibility, Adt, AdtID, AdtImplementation,
        AdtImplementationFunction, Callable, CallableID, Constant, Enum,
        Function, Generic, GenericDeclaration, GenericID, GenericTemplate,
        Global, GlobalID, HierarchyRelationship, Implementation,
        ImplementationID, ImplementationTemplate, Marker,
        MarkerImplementationID, Module, ModuleMemberID,
        NegativeMarkerImplementation, NegativeTraitImplementation,
        PositiveMarkerImplementation, PositiveTraitImplementation,
        ResolvableImplementation, Struct, Trait, TraitConstant, TraitFunction,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationID, TraitImplementationType, TraitType, Type,
        Variant,
    },
    type_system::{
        model::{Default, Model},
        predicate::Predicate,
        term::{
            constant,
            lifetime::Lifetime,
            r#type::{self, MemberSymbolID},
            GenericArguments,
        },
        Premise,
    },
};

pub(crate) mod building;

/// A trait used to access the symbols defined in the table.
pub trait Element: Sized + Debug + Send + Sync + 'static {
    /// Gets the arena reference containing *this* kind of symbol.
    fn get_arena<T: Container>(
        table: &Representation<T>,
    ) -> &Arena<T::Wrap<Self>, ID<Self>>;

    /// Gets the mutable arena reference containing *this* kind of symbol.
    fn get_arena_mut<T: Container>(
        table: &mut Representation<T>,
    ) -> &mut Arena<T::Wrap<Self>, ID<Self>>;
}

/// A trait used to access the symbols defined in the table.
pub trait Index<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output<'a>: Sized
    where
        Self: 'a;

    /// Returns the output of the indexing operation if the index is valid.
    fn get(&self, index: Idx) -> Option<Self::Output<'_>>;
}

/// A trait used to mutably access the symbols defined in the table.
pub trait IndexMut<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output<'a>: Sized
    where
        Self: 'a;

    /// Returns the output of the indexing operation if the index is valid.
    fn get_mut(&mut self, index: Idx) -> Option<Self::Output<'_>>;
}

impl<T: Element, S: Container> Index<ID<T>> for Representation<S> {
    type Output<'a>
        = S::Read<'a, T>
    where
        Self: 'a;

    fn get(&self, index: ID<T>) -> Option<Self::Output<'_>> {
        T::get_arena(self).get(index).map(|x| S::read(x))
    }
}

impl<T: Element, S: Container> IndexMut<ID<T>> for Representation<S> {
    type Output<'a>
        = S::Write<'a, T>
    where
        Self: 'a;

    fn get_mut(&mut self, index: ID<T>) -> Option<Self::Output<'_>> {
        T::get_arena_mut(self).get_mut(index).map(|x| S::write(x))
    }
}

/// A trait used to wrap a symbol in a container.
///
/// This is primarily used to either wrap a symbol in a [`RwLock`]
/// or not at all.
///
/// See [`RwLockContainer`] for an example.
pub trait Container:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type of the wrapped value.
    type Wrap<T: Debug + 'static + Send + Sync>: Debug + 'static + Send + Sync;

    /// The type of the read guard of the wrapped value.
    #[clippy::has_significant_drop]
    type Read<'a, T: ?Sized + 'a>: Deref<Target = T> + 'a;

    /// The type of the write guard of the wrapped value.
    #[clippy::has_significant_drop]
    type Write<'a, T: ?Sized + 'a>: DerefMut<Target = T> + 'a;

    /// The type of the mapped read guard of the wrapped value.
    #[clippy::has_significant_drop]
    type MappedRead<'a, T: ?Sized + 'a>: Deref<Target = T> + 'a;

    /// Wraps the given value.
    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T>;

    /// Reads the given value.
    fn read<T: Debug + 'static + Send + Sync>(
        value: &Self::Wrap<T>,
    ) -> Self::Read<'_, T>;

    /// Writes the given value.
    fn write<T: Debug + 'static + Send + Sync>(
        value: &mut Self::Wrap<T>,
    ) -> Self::Write<'_, T>;

    /// Maps the given value into another sub-field value.
    fn map_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::Read<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U>;

    /// Maps the given value into another sub-field value.
    fn map_mapped_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::MappedRead<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U>;
}

/// A struct which implements [`Container`] by wrapping the value in a
/// [`RwLock`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct RwLockContainer;

impl Container for RwLockContainer {
    type MappedRead<'a, T: ?Sized + 'a> = MappedRwLockReadGuard<'a, T>;
    type Read<'a, T: ?Sized + 'a> = RwLockReadGuard<'a, T>;
    type Write<'a, T: ?Sized + 'a> = &'a mut T;
    type Wrap<T: Debug + 'static + Send + Sync> = RwLock<T>;

    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T> {
        RwLock::new(value)
    }

    fn read<T: Debug + 'static + Send + Sync>(
        value: &Self::Wrap<T>,
    ) -> Self::Read<'_, T> {
        value.read_recursive()
    }

    fn map_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::Read<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U> {
        RwLockReadGuard::map(value, f)
    }

    fn map_mapped_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::MappedRead<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U> {
        MappedRwLockReadGuard::map(value, f)
    }

    fn write<T: Debug + 'static + Send + Sync>(
        value: &mut Self::Wrap<T>,
    ) -> Self::Write<'_, T> {
        value.get_mut()
    }
}

/// A struct which implements [`Container`] by not wrapping the value at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoContainer;

impl Container for NoContainer {
    type MappedRead<'a, T: ?Sized + 'a> = &'a T;
    type Read<'a, T: ?Sized + 'a> = &'a T;
    type Wrap<T: Debug + 'static + Send + Sync> = T;
    type Write<'a, T: ?Sized + 'a> = &'a mut T;

    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T> {
        value
    }

    fn read<T: Debug + 'static + Send + Sync>(
        value: &Self::Wrap<T>,
    ) -> Self::Read<'_, T> {
        value
    }

    fn map_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::Read<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U> {
        f(value)
    }

    fn map_mapped_read<'a, T: ?Sized + 'a, U: ?Sized + 'a>(
        value: Self::MappedRead<'a, T>,
        f: impl FnOnce(&T) -> &U,
    ) -> Self::MappedRead<'a, U> {
        f(value)
    }

    fn write<T: Debug + 'static + Send + Sync>(
        value: &mut Self::Wrap<T>,
    ) -> Self::Write<'_, T> {
        value
    }
}

/// The representation of the table without any state information.
#[derive(Debug, Getters)]
pub struct Representation<T: Container> {
    modules: Arena<T::Wrap<Module>, ID<Module>>,
    structs: Arena<T::Wrap<Struct>, ID<Struct>>,
    enums: Arena<T::Wrap<Enum>, ID<Enum>>,
    variants: Arena<T::Wrap<Variant>, ID<Variant>>,
    types: Arena<T::Wrap<Type>, ID<Type>>,
    functions: Arena<T::Wrap<Function>, ID<Function>>,
    constants: Arena<T::Wrap<Constant>, ID<Constant>>,

    traits: Arena<T::Wrap<Trait>, ID<Trait>>,
    trait_types: Arena<T::Wrap<TraitType>, ID<TraitType>>,
    trait_functions: Arena<T::Wrap<TraitFunction>, ID<TraitFunction>>,
    trait_constants: Arena<T::Wrap<TraitConstant>, ID<TraitConstant>>,

    positive_trait_implementations: Arena<
        T::Wrap<PositiveTraitImplementation>,
        ID<PositiveTraitImplementation>,
    >,
    negative_trait_implementations: Arena<
        T::Wrap<NegativeTraitImplementation>,
        ID<NegativeTraitImplementation>,
    >,

    trait_implementation_types:
        Arena<T::Wrap<TraitImplementationType>, ID<TraitImplementationType>>,
    trait_implementation_functions: Arena<
        T::Wrap<TraitImplementationFunction>,
        ID<TraitImplementationFunction>,
    >,
    trait_implementation_constants: Arena<
        T::Wrap<TraitImplementationConstant>,
        ID<TraitImplementationConstant>,
    >,

    adt_implementations:
        Arena<T::Wrap<AdtImplementation>, ID<AdtImplementation>>,

    adt_implementation_functions: Arena<
        T::Wrap<AdtImplementationFunction>,
        ID<AdtImplementationFunction>,
    >,

    markers: Arena<T::Wrap<Marker>, ID<Marker>>,

    positive_marker_implementations: Arena<
        T::Wrap<PositiveMarkerImplementation>,
        ID<PositiveMarkerImplementation>,
    >,

    negative_marker_implementations: Arena<
        T::Wrap<NegativeMarkerImplementation>,
        ID<NegativeMarkerImplementation>,
    >,

    /// Maps the root module (target) names to their IDs.
    #[get = "pub"]
    root_module_ids_by_name: HashMap<String, ID<Module>>,
}

impl<T: Container> std::default::Default for Representation<T> {
    fn default() -> Self {
        Self {
            modules: Arena::default(),
            structs: Arena::default(),
            enums: Arena::default(),
            variants: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            constants: Arena::default(),
            traits: Arena::default(),
            trait_types: Arena::default(),
            trait_functions: Arena::default(),
            trait_constants: Arena::default(),
            positive_trait_implementations: Arena::default(),
            negative_trait_implementations: Arena::default(),
            trait_implementation_types: Arena::default(),
            trait_implementation_functions: Arena::default(),
            trait_implementation_constants: Arena::default(),
            adt_implementations: Arena::default(),
            adt_implementation_functions: Arena::default(),
            markers: Arena::default(),
            positive_marker_implementations: Arena::default(),
            negative_marker_implementations: Arena::default(),
            root_module_ids_by_name: HashMap::default(),
        }
    }
}

macro_rules! implements_element {
    ($symbol:ident) => {
        impl Element for $symbol {
            /// Gets the arena reference containing *this* kind of symbol.
            fn get_arena<T: Container>(
                table: &Representation<T>,
            ) -> &Arena<T::Wrap<Self>, ID<Self>> {
                paste! {
                    &table.[<$symbol:snake s>]
                }
            }

            /// Gets the mutable arena reference containing *this* kind of
            /// symbol.
            fn get_arena_mut<T: Container>(
                table: &mut Representation<T>,
            ) -> &mut Arena<T::Wrap<Self>, ID<Self>> {
                paste! {
                    &mut table.[<$symbol:snake s>]
                }
            }
        }
    };
}

implements_element!(Module);
implements_element!(Struct);
implements_element!(Enum);
implements_element!(Type);
implements_element!(Function);
implements_element!(Constant);
implements_element!(Trait);
implements_element!(TraitType);
implements_element!(TraitFunction);
implements_element!(TraitConstant);
implements_element!(Variant);
implements_element!(PositiveTraitImplementation);
implements_element!(NegativeTraitImplementation);
implements_element!(TraitImplementationType);
implements_element!(TraitImplementationConstant);
implements_element!(TraitImplementationFunction);
implements_element!(AdtImplementation);
implements_element!(AdtImplementationFunction);
implements_element!(Marker);
implements_element!(PositiveMarkerImplementation);
implements_element!(NegativeMarkerImplementation);

macro_rules! get {
    ($self:ident, $id:ident, $kind:ident, $($field:ident),*) => {
        match $id {
            $(
                $kind::$field($id) => $self.get($id).map(|x| T::map_read(x, |x| x as _)),
            )*
        }
    };
}

/// The error type returned by [`Table::get_by_qualified_name()`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetByQualifiedNameError<'a> {
    #[error(
        "The given name `{name}` does not exist in the table (searched in \
         `{searched_in_global_id:?}`)"
    )]
    SymbolNotFound {
        /// The global ID that was searched in. If `None`, the search was
        /// started from the root.
        searched_in_global_id: Option<GlobalID>,

        /// The name that was searched.
        name: &'a str,
    },
    #[error("the given iterator is empty")]
    EmptyIterator,
}

/// The error type returned by [`Table::get_member_of()`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetMemberError {
    #[error("the given global ID is not valid")]
    InvalidID,
    #[error("the member with the given name is not found")]
    MemberNotFound,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum MergeAccessibilityError {
    #[error("the accessibility objects contain an invalid module ID")]
    InvalidModuleID,

    #[error("two accessibility objects are scoped to different modules")]
    Unrelated,
}

impl From<MergeAccessibilityError> for GetTermAccessibilityError {
    fn from(err: MergeAccessibilityError) -> Self {
        match err {
            MergeAccessibilityError::InvalidModuleID => Self::InvalidID,
            MergeAccessibilityError::Unrelated => Self::Unrealated,
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetTermAccessibilityError {
    #[error("the term contains an invalid ID")]
    InvalidID,

    #[error(
        "the term contains two or more accessibilities that are scoped to \
         different modules"
    )]
    Unrealated,
}

impl<T: Container> Representation<T> {
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
        first: Accessibility,
        second: Accessibility,
    ) -> Option<HierarchyRelationship> {
        // check if the module IDs are valid.
        if let Accessibility::Scoped(first) = first {
            let _ = self.get(first)?;
        }
        if let Accessibility::Scoped(second) = second {
            let _ = self.get(second)?;
        }

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
                self.symbol_hierarchy_relationship(first.into(), second.into())
            }
        }
    }

    /// Computes the [`HierarchyRelationship`] between the two given global IDs.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first` symbol.
    ///
    /// # Returns
    ///
    /// Returns `None` if either `first` or `second` is not a valid ID.
    #[must_use]
    pub fn symbol_hierarchy_relationship(
        &self,
        first: GlobalID,
        second: GlobalID,
    ) -> Option<HierarchyRelationship> {
        if self.get_global(first).is_none() || self.get_global(second).is_none()
        {
            return None;
        }

        // the two symbols are the same.
        if first == second {
            return Some(HierarchyRelationship::Equivalent);
        }

        for first_parent in self.scope_walker(first)? {
            if first_parent == second {
                return Some(HierarchyRelationship::Child);
            }
        }

        for second_parent in self.scope_walker(second)? {
            if second_parent == first {
                return Some(HierarchyRelationship::Parent);
            }
        }

        Some(HierarchyRelationship::Unrelated)
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
        referring_site: GlobalID,
        referred_accessibility: Accessibility,
    ) -> Option<bool> {
        match referred_accessibility {
            Accessibility::Public => {
                // PEDANTIC: check if the referring site is a valid ID.
                drop(self.get_global(referring_site)?);

                Some(true)
            }

            Accessibility::Scoped(module_id) => {
                let referring_site_module_id =
                    self.get_closet_module_id(referring_site)?;

                Some(matches!(
                    self.symbol_hierarchy_relationship(
                        module_id.into(),
                        referring_site_module_id.into(),
                    )?,
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

        self.is_accessible_from(referring_site, referred_accessibility)
    }

    /// Returns the root [`Module`] ID that contains the given [`GlobalID`]
    /// (including itself).
    #[must_use]
    pub fn get_root_module_id(
        &self,
        mut global_id: GlobalID,
    ) -> Option<ID<Module>> {
        while let Some(parent_id) =
            self.get_global(global_id)?.parent_global_id()
        {
            global_id = parent_id;
        }

        Some(
            global_id
                .into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Returns the [`Module`] ID that is the closest to the given [`GlobalID`]
    /// (including itself).
    #[must_use]
    pub fn get_closet_module_id(
        &self,
        mut global_id: GlobalID,
    ) -> Option<ID<Module>> {
        // including itself
        loop {
            if let GlobalID::Module(module_id) = global_id {
                drop(self.get(module_id)?);
                return Some(module_id);
            }

            global_id = self
                .get_global(global_id)?
                .parent_global_id()
                .expect("should've found at least one module");
        }
    }

    /// Searches for a member with the given name in the given global ID.
    ///
    /// # Errors
    ///
    /// See [`GetMemberError`] for more information.
    pub fn get_member_of(
        &self,
        global_id: GlobalID,
        member_name: &str,
    ) -> Result<GlobalID, GetMemberError> {
        match global_id {
            GlobalID::Module(id) => self
                .get(id)
                .ok_or(GetMemberError::InvalidID)?
                .member_ids_by_name
                .get(member_name)
                .map(|x| (*x).into()),
            GlobalID::Struct(id) => self
                .get(id)
                .ok_or(GetMemberError::InvalidID)?
                .implementations
                .iter()
                .find_map(|x| {
                    self.get(*x)
                        .unwrap()
                        .member_ids_by_name
                        .get(member_name)
                        .map(|x| (*x).into())
                }),
            GlobalID::Trait(id) => self
                .get(id)
                .ok_or(GetMemberError::InvalidID)?
                .member_ids_by_name
                .get(member_name)
                .map(|x| (*x).into()),
            GlobalID::Enum(id) => {
                let enum_sym = self.get(id).ok_or(GetMemberError::InvalidID)?;

                enum_sym
                    .variant_ids_by_name
                    .get(member_name)
                    .map(|x| (*x).into())
                    .or_else(|| {
                        enum_sym.implementations.iter().find_map(|x| {
                            self.get(*x)
                                .unwrap()
                                .member_ids_by_name()
                                .get(member_name)
                                .map(|x| (*x).into())
                        })
                    })
            }
            GlobalID::PositiveTraitImplementation(id) => self
                .get(id)
                .ok_or(GetMemberError::InvalidID)?
                .member_ids_by_name
                .get(member_name)
                .map(|x| (*x).into()),
            GlobalID::AdtImplementation(x) => self
                .get(x)
                .ok_or(GetMemberError::InvalidID)?
                .member_ids_by_name
                .get(member_name)
                .map(|x| (*x).into()),

            GlobalID::NegativeTraitImplementation(_)
            | GlobalID::TraitImplementationFunction(_)
            | GlobalID::TraitImplementationType(_)
            | GlobalID::TraitImplementationConstant(_)
            | GlobalID::Type(_)
            | GlobalID::Constant(_)
            | GlobalID::Function(_)
            | GlobalID::Variant(_)
            | GlobalID::TraitType(_)
            | GlobalID::TraitFunction(_)
            | GlobalID::TraitConstant(_)
            | GlobalID::AdtImplementationFunction(_)
            | GlobalID::Marker(_)
            | GlobalID::PositiveMarkerImplementation(_)
            | GlobalID::NegativeMarkerImplementation(_) => None,
        }
        .ok_or(GetMemberError::MemberNotFound)
    }

    /// Gets a [`GlobalID`] from the given qualified identifier.
    ///
    /// # Errors
    ///
    /// - [`GetByQualifiedNameError::SymbolNotFound`]: if the symbol is not
    ///   found.
    /// - [`GetByQualifiedNameError::EmptyIterator`]: if the given iterator is
    ///   empty.
    pub fn get_by_qualified_name<'a>(
        &self,
        qualified_names: impl IntoIterator<Item = &'a str>,
    ) -> Result<GlobalID, GetByQualifiedNameError<'a>> {
        let mut current_id: Option<GlobalID> = None;

        for name in qualified_names {
            match current_id {
                Some(searched_in_global_id) => {
                    current_id = Some(
                        self.get_member_of(searched_in_global_id, name)
                            .map_err(|err| match err {
                                GetMemberError::InvalidID => {
                                    unreachable!("invalid ID in the table")
                                }
                                GetMemberError::MemberNotFound => {
                                    GetByQualifiedNameError::SymbolNotFound {
                                        searched_in_global_id: Some(
                                            searched_in_global_id,
                                        ),
                                        name,
                                    }
                                }
                            })?,
                    );
                }
                None => {
                    current_id =
                        Some(self.root_module_ids_by_name.get(name).map_or(
                            Err(GetByQualifiedNameError::SymbolNotFound {
                                searched_in_global_id: None,
                                name,
                            }),
                            |some| Ok(GlobalID::Module(*some)),
                        )?);
                }
            }
        }

        current_id.ok_or(GetByQualifiedNameError::EmptyIterator)
    }

    /// Gets the fully qualified name of the given [`GlobalID`].
    ///
    /// # Returns
    ///
    /// Returns `None` if the given [`GlobalID`] is not valid.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_qualified_name(&self, global_id: GlobalID) -> Option<String> {
        match global_id {
            GlobalID::PositiveTraitImplementation(id) => {
                self.get_qualified_name(self.get(id)?.implemented_id.into())
            }
            GlobalID::NegativeTraitImplementation(id) => {
                self.get_qualified_name(self.get(id)?.implemented_id.into())
            }

            GlobalID::AdtImplementation(id) => {
                self.get_qualified_name(self.get(id)?.implemented_id.into())
            }

            GlobalID::AdtImplementationFunction(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_id)
                        .unwrap()
                        .implemented_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }

            GlobalID::TraitImplementationType(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_id)
                        .unwrap()
                        .implemented_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }
            GlobalID::TraitImplementationFunction(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_id)
                        .unwrap()
                        .implemented_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }
            GlobalID::TraitImplementationConstant(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_id)
                        .unwrap()
                        .implemented_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }

            mut normal => {
                let mut qualified_name = String::new();

                loop {
                    let global = self.get_global(normal)?;

                    if qualified_name.is_empty() {
                        qualified_name.push_str(global.name());
                    } else {
                        qualified_name.insert_str(0, "::");
                        qualified_name.insert_str(0, global.name());
                    }

                    if let Some(parent_id) = global.parent_global_id() {
                        normal = parent_id;
                    } else {
                        break;
                    }
                }

                Some(qualified_name)
            }
        }
    }

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
        first: Accessibility,
        second: Accessibility,
    ) -> Result<Accessibility, MergeAccessibilityError> {
        Ok(match (first, second) {
            (Accessibility::Public, Accessibility::Public) => {
                Accessibility::Public
            }
            (Accessibility::Public, Accessibility::Scoped(scope))
            | (Accessibility::Scoped(scope), Accessibility::Public) => {
                Accessibility::Scoped(scope)
            }
            (Accessibility::Scoped(first), Accessibility::Scoped(second)) => {
                match self
                    .symbol_hierarchy_relationship(first.into(), second.into())
                    .ok_or(MergeAccessibilityError::InvalidModuleID)?
                {
                    HierarchyRelationship::Parent => {
                        Accessibility::Scoped(second)
                    }
                    HierarchyRelationship::Child
                    | HierarchyRelationship::Equivalent => {
                        Accessibility::Scoped(first)
                    }
                    HierarchyRelationship::Unrelated => {
                        return Err(MergeAccessibilityError::Unrelated)
                    }
                }
            }
        })
    }

    /// Gets overall accessibility of the given [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    #[allow(clippy::too_many_lines, clippy::uninhabited_references)]
    pub fn get_type_accessibility(
        &self,
        ty: &r#type::Type<Default>,
    ) -> Result<Accessibility, GetTermAccessibilityError> {
        match ty {
            r#type::Type::Inference(never) => match *never {},

            r#type::Type::MemberSymbol(member_symbol) => {
                let MemberSymbolID::Function(member_function_id) =
                    member_symbol.id;

                let symbol_accessibility = self
                    .get_accessibility(member_function_id.into())
                    .ok_or(GetTermAccessibilityError::InvalidID)?;

                let member_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?;

                let parent_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?;

                let generic_arguments_accessibility = self
                    .merge_accessibility_down(
                        member_generic_accessibility,
                        parent_generic_accessibility,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            r#type::Type::Symbol(adt) => {
                let symbol_accessibility = self
                    .get_accessibility(adt.id.into())
                    .ok_or(GetTermAccessibilityError::InvalidID)?;
                let generic_arguments_accessibility = self
                    .get_generic_arguments_accessibility(
                        &adt.generic_arguments,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            r#type::Type::Pointer(pointer) => {
                self.get_type_accessibility(&pointer.pointee)
            }

            r#type::Type::Reference(reference) => {
                let lt_accessibility =
                    self.get_lifetime_accessibility(&reference.lifetime)?;
                let ty_accessibility =
                    self.get_type_accessibility(&reference.pointee)?;

                Ok(self.merge_accessibility_down(
                    lt_accessibility,
                    ty_accessibility,
                )?)
            }

            r#type::Type::Array(array) => {
                let ty_accessibility =
                    self.get_type_accessibility(&array.r#type)?;
                let length_accessibility =
                    self.get_constant_accessibility(&array.length)?;

                Ok(self.merge_accessibility_down(
                    ty_accessibility,
                    length_accessibility,
                )?)
            }

            r#type::Type::Parameter(parameter) => self
                .get_accessibility(parameter.parent.into())
                .ok_or(GetTermAccessibilityError::InvalidID),

            r#type::Type::Error(_) | r#type::Type::Primitive(_) => {
                Ok(Accessibility::Public)
            }

            r#type::Type::Tuple(tuple) => {
                let mut current_min = Accessibility::Public;

                for element in &tuple.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_type_accessibility(&element.term)?,
                    )?;
                }

                Ok(current_min)
            }

            r#type::Type::TraitMember(member_symbol) => {
                let symbol_accessibility = self
                    .get_accessibility(member_symbol.id.into())
                    .ok_or(GetTermAccessibilityError::InvalidID)?;

                let member_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?;

                let parent_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?;

                let generic_arguments_accessibility = self
                    .merge_accessibility_down(
                        member_generic_accessibility,
                        parent_generic_accessibility,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            r#type::Type::Phantom(phantom) => {
                self.get_type_accessibility(&phantom.0)
            }
        }
    }

    /// Gets overall accessibility of the given [`constant::Constant`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    #[allow(clippy::uninhabited_references)]
    pub fn get_constant_accessibility(
        &self,
        constant: &constant::Constant<Default>,
    ) -> Result<Accessibility, GetTermAccessibilityError> {
        match constant {
            constant::Constant::Inference(never) => match *never {},

            constant::Constant::Struct(constant) => {
                let mut current_min = self
                    .get_accessibility(constant.id.into())
                    .ok_or(GetTermAccessibilityError::InvalidID)?;

                for field in &constant.fields {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(field)?,
                    )?;
                }

                Ok(current_min)
            }

            constant::Constant::Array(constant) => {
                let mut current_min = Accessibility::Public;

                for element in &constant.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(element)?,
                    )?;
                }

                Ok(current_min)
            }

            constant::Constant::Parameter(id) => Ok(self
                .get_accessibility(id.parent.into())
                .ok_or(GetTermAccessibilityError::InvalidID)?),

            constant::Constant::Error(_)
            | constant::Constant::Phantom
            | constant::Constant::Primitive(_) => Ok(Accessibility::Public),

            constant::Constant::Enum(constant) => {
                let mut current_min = self
                    .get_accessibility(constant.variant_id.into())
                    .ok_or(GetTermAccessibilityError::InvalidID)?;

                if let Some(associated_value) = &constant.associated_value {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(associated_value)?,
                    )?;
                }

                Ok(current_min)
            }

            constant::Constant::Tuple(tuple) => {
                let mut current_min = Accessibility::Public;

                for element in &tuple.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(&element.term)?,
                    )?;
                }

                Ok(current_min)
            }
        }
    }

    /// Gets overall accessibility of the given [`Lifetime`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    #[allow(clippy::uninhabited_references)]
    pub fn get_lifetime_accessibility(
        &self,
        lifetime: &Lifetime<Default>,
    ) -> Result<Accessibility, GetTermAccessibilityError> {
        match lifetime {
            Lifetime::Parameter(lifetime_parameter_id) => self
                .get_accessibility(lifetime_parameter_id.parent.into())
                .ok_or(GetTermAccessibilityError::InvalidID),

            Lifetime::Inference(never) => match *never {},

            Lifetime::Error(_) | Lifetime::Forall(_) | Lifetime::Static => {
                Ok(Accessibility::Public)
            }
        }
    }

    /// Gets overall accessibility of the given [`GenericArguments`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    pub fn get_generic_arguments_accessibility(
        &self,
        generic_arguments: &GenericArguments<Default>,
    ) -> Result<Accessibility, GetTermAccessibilityError> {
        let mut current_min = Accessibility::Public;

        for lifetime in &generic_arguments.lifetimes {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_lifetime_accessibility(lifetime)?,
            )?;
        }

        for ty in &generic_arguments.types {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_type_accessibility(ty)?,
            )?;
        }

        for constant in &generic_arguments.constants {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_constant_accessibility(constant)?,
            )?;
        }

        Ok(current_min)
    }

    /// Gets the active [`Premise`] starting at the given [`GlobalID`] scope
    /// mapped to their [`Span`]s of their declaration.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `global_id` is not a valid ID.
    #[must_use]
    pub fn get_active_premise_predicates_with_span<M: Model>(
        &self,
        global_id: GlobalID,
    ) -> Option<HashMap<Predicate<M>, Vec<Span>>> {
        let mut spans_by_predicate: HashMap<Predicate<M>, Vec<Span>> =
            HashMap::default();

        for global_id in self.scope_walker(global_id)? {
            let Ok(generic_id) = GenericID::try_from(global_id) else {
                continue;
            };

            let generic = self.get_generic(generic_id)?;

            for predicate in &generic.generic_declaration().predicates {
                let Some(span) = predicate.span.clone() else {
                    continue;
                };

                let predicate =
                    Predicate::from_default_model(predicate.predicate.clone());

                spans_by_predicate.entry(predicate).or_default().push(span);
            }
        }

        Some(spans_by_predicate)
    }

    /// Gets the active [`Premise`] starting at the given [`GlobalID`] scope.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `global_id` is not a valid ID.
    #[must_use]
    pub fn get_active_premise<M: Model>(
        &self,
        global_id: GlobalID,
    ) -> Option<Premise<M>> {
        let mut premise = Premise {
            predicates: BTreeSet::new(),
            query_site: Some(global_id),
        };

        for global_id in self.scope_walker(global_id)? {
            let Ok(generic_id) = GenericID::try_from(global_id) else {
                continue;
            };

            let generic = self.get_generic(generic_id)?;

            premise.predicates.extend(
                generic.generic_declaration().predicates.iter().map(|x| {
                    Predicate::from_default_model(x.predicate.clone())
                }),
            );
        }

        Some(premise)
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
    /// given [`GlobalID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub fn scope_walker(&self, global_id: GlobalID) -> Option<ScopeWalker<T>> {
        drop(self.get_global(global_id)?);

        Some(ScopeWalker { table: self, current_id: Some(global_id) })
    }

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `parent_id` is not a valid ID.
    #[must_use]
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
                let root_module_id = self.get_root_module_id(parent_id)?;

                Some(Accessibility::Scoped(root_module_id))
            }
        }
    }

    /// Gets the accessibility of the given [`GlobalID`].
    ///
    /// # Errors
    ///
    /// Returns `None` if the given [`GlobalID`] is not a valid ID.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_accessibility(
        &self,
        global_id: GlobalID,
    ) -> Option<Accessibility> {
        macro_rules! arm_expression {
            ($table:ident, $id:ident, $kind:ident) => {
                paste! {
                    $table
                        .[<$kind:snake s>]
                        .get($id)
                        .map(|$id| T::read($id).accessibility)
                }
            };

            ($table:ident, $id:ident, $kind:ident, $expr:expr) => {
                paste! {
                    $table
                        .[<$kind:snake s>]
                        .get($id)
                        .and_then(|$id| $expr)
                }
            };
        }
        macro_rules! get_accessibility {
            ($self:ident, $table:ident, $(($kind:ident $(, $expr:expr)?)),*) => {

                match $self {
                    $(
                        GlobalID::$kind($self) =>
                            arm_expression!($table, $self, $kind $(, $expr)?),
                    )*
                }
            };
        }

        get_accessibility!(
            global_id,
            self,
            (Module),
            (Struct),
            (Enum),
            (Trait),
            (Type),
            (Constant),
            (Function),
            (AdtImplementationFunction),
            (
                Variant,
                self.get_accessibility(
                    T::read(global_id).parent_enum_id.into()
                )
            ),
            (TraitType),
            (TraitConstant),
            (TraitFunction),
            (
                PositiveTraitImplementation,
                self.get_accessibility(
                    T::read(global_id).implemented_id.into()
                )
            ),
            (Marker),
            (
                PositiveMarkerImplementation,
                self.get_accessibility(
                    T::read(global_id).implemented_id.into()
                )
            ),
            (
                NegativeMarkerImplementation,
                self.get_accessibility(
                    T::read(global_id).implemented_id.into()
                )
            ),
            (TraitImplementationType),
            (TraitImplementationFunction),
            (TraitImplementationConstant),
            (
                NegativeTraitImplementation,
                self.get_accessibility(
                    T::read(global_id).implemented_id.into()
                )
            ),
            (AdtImplementation, {
                self.get_accessibility(T::read(global_id).implemented_id.into())
            })
        )
    }

    /// Gets the dyn [`Implementation`] symbol from the given ID.
    ///
    /// # Returns
    ///
    /// Returns `None` if the given ID is not a valid ID.
    #[must_use]
    pub fn get_implementation(
        &self,
        implementation_id: ImplementationID,
    ) -> Option<T::MappedRead<'_, dyn Implementation>> {
        get!(
            self,
            implementation_id,
            ImplementationID,
            PositiveTrait,
            NegativeTrait,
            NegativeMarker,
            PositiveMarker,
            Adt
        )
    }

    /// Returns the [`Generic`] symbol from the given [`GenericID`]
    #[must_use]
    pub fn get_generic(
        &self,
        generic_id: GenericID,
    ) -> Option<T::MappedRead<'_, dyn Generic>> {
        get!(
            self,
            generic_id,
            GenericID,
            Struct,
            Trait,
            Enum,
            Function,
            Constant,
            Type,
            TraitFunction,
            TraitConstant,
            TraitType,
            PositiveTraitImplementation,
            TraitImplementationType,
            TraitImplementationConstant,
            TraitImplementationFunction,
            NegativeTraitImplementation,
            AdtImplementation,
            AdtImplementationFunction,
            Marker,
            PositiveMarkerImplementation,
            NegativeMarkerImplementation
        )
    }

    /// Returns the [`Adt`] symbol from the given [`AdtID`].
    #[must_use]
    pub fn get_adt(&self, adt_id: AdtID) -> Option<T::MappedRead<'_, dyn Adt>> {
        match adt_id {
            AdtID::Struct(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            AdtID::Enum(id) => self.get(id).map(|x| T::map_read(x, |x| x as _)),
        }
    }

    /// Returns the [`Callable`] symbol from the given [`CallableID`].
    #[must_use]
    pub fn get_callable(
        &self,
        callable_id: CallableID,
    ) -> Option<T::MappedRead<'_, dyn Callable>> {
        match callable_id {
            CallableID::Function(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            CallableID::TraitFunction(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            CallableID::TraitImplementationFunction(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            CallableID::AdtImplementationFunction(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
        }
    }

    /// Returns the [`Global`] symbol from the given [`GlobalID`].
    #[must_use]
    pub fn get_global(
        &self,
        global_id: GlobalID,
    ) -> Option<T::MappedRead<'_, dyn Global>> {
        get!(
            self,
            global_id,
            GlobalID,
            Module,
            Struct,
            TraitType,
            TraitConstant,
            TraitFunction,
            Trait,
            Enum,
            Type,
            Constant,
            Function,
            Variant,
            TraitImplementationFunction,
            PositiveTraitImplementation,
            TraitImplementationType,
            TraitImplementationConstant,
            NegativeTraitImplementation,
            AdtImplementation,
            AdtImplementationFunction,
            Marker,
            PositiveMarkerImplementation,
            NegativeMarkerImplementation
        )
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a, T: Container> {
    table: &'a Representation<T>,
    current_id: Option<GlobalID>,
}

impl<'a, T: Container> Iterator for ScopeWalker<'a, T> {
    type Item = GlobalID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self
                    .table
                    .get_global(current_id)
                    .unwrap()
                    .parent_global_id();
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}

/// The error type returned by [`build()`].
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum BuildTableError {
    #[error("the target `{0}` was already defined")]
    DuplicateTargetName(String),

    #[error("the table was built with some errors")]
    Suboptimal(Table<Suboptimal>),
}

fn convert_rw_locked_arena<T: 'static>(
    arena: Arena<RwLock<T>, ID<T>>,
) -> Arena<T, ID<T>> {
    let mut new_arena = Arena::default();

    for (idx, value) in arena {
        assert!(new_arena.insert_with_id(idx, value.into_inner()).is_ok());
    }

    new_arena
}

struct HandlerAdaptor<'a> {
    handler: &'a dyn Handler<Box<dyn error::Error>>,
    received: RwLock<bool>,
}

impl<'a> Handler<Box<dyn error::Error>> for HandlerAdaptor<'a> {
    fn receive(&self, error: Box<dyn error::Error>) {
        self.handler.receive(error);
        *self.received.write() = true;
    }
}

#[allow(clippy::too_many_lines)]
fn transition_to_building(
    mut drafting_table: Table<
        Building<RwLockContainer, building::drafting::Drafter>,
    >,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Table<Building<RwLockContainer, building::finalizing::Finalizer>> {
    let usings_by_module_id =
        std::mem::take(&mut drafting_table.state.usings_by_module_id);
    let implementations_by_module_id =
        std::mem::take(&mut drafting_table.state.implementations_by_module_id);

    // add usings to the modules
    usings_by_module_id
        .into_par_iter()
        .flat_map(|(id, usings)| {
            usings.into_par_iter().map(move |using| (id, using))
        })
        .for_each(|(current_module_id, using)| match using.kind() {
            UsingKind::One(a) => {
                let Ok(id) = drafting_table.representation.resolve_simple_path(
                    a.simple_path(),
                    current_module_id.into(),
                    true,
                    handler,
                ) else {
                    return;
                };

                let GlobalID::Module(id) = id else {
                    handler.receive(Box::new(ExpectModule {
                        module_path: a.simple_path().span(),
                        found_id: id,
                    }));

                    return;
                };

                let name = a.alias().as_ref().map_or_else(
                    || {
                        drafting_table
                            .representation
                            .get(id)
                            .unwrap()
                            .name()
                            .to_owned()
                    },
                    |x| x.identifier().span.str().to_owned(),
                );

                let mut module = drafting_table
                    .representation
                    .modules
                    .get(current_module_id)
                    .unwrap()
                    .write();

                if let Some(existing) = module
                    .member_ids_by_name
                    .get(&name)
                    .map(|x| {
                        drafting_table
                            .representation
                            .get_global((*x).into())
                            .unwrap()
                            .span()
                            .cloned()
                            .unwrap()
                    })
                    .or_else(|| {
                        module.imports.get(&name).map(|x| x.1.clone().unwrap())
                    })
                {
                    handler.receive(Box::new(ConflictingUsing {
                        using_span: a.alias().as_ref().map_or_else(
                            || a.simple_path().span(),
                            SourceElement::span,
                        ),
                        name: name.clone(),
                        module_id: current_module_id,
                        conflicting_span: existing,
                    }));
                } else {
                    module.imports.insert(
                        name,
                        (
                            id.into(),
                            Some(a.alias().as_ref().map_or_else(
                                || a.simple_path().span(),
                                |x| x.identifier().span(),
                            )),
                        ),
                    );
                }
            }

            UsingKind::From(a) => {
                let Ok(from_id) =
                    drafting_table.representation.resolve_simple_path(
                        a.from().simple_path(),
                        current_module_id.into(),
                        true,
                        handler,
                    )
                else {
                    return;
                };

                let GlobalID::Module(from_id) = from_id else {
                    handler.receive(Box::new(ExpectModule {
                        module_path: a.from().simple_path().span(),
                        found_id: from_id,
                    }));

                    return;
                };

                for import in a
                    .imports()
                    .connected_list()
                    .as_ref()
                    .into_iter()
                    .flat_map(ConnectedList::elements)
                {
                    let Some(id) = drafting_table
                        .representation
                        .get(from_id)
                        .unwrap()
                        .member_ids_by_name
                        .get(import.identifier().span.str())
                        .copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_global_id: Some(from_id.into()),
                            resolution_span: import.identifier().span.clone(),
                        }));
                        continue;
                    };

                    let name = import.alias().as_ref().map_or_else(
                        || {
                            drafting_table
                                .representation
                                .get_global(id.into())
                                .unwrap()
                                .name()
                                .to_owned()
                        },
                        |x| x.identifier().span.str().to_owned(),
                    );

                    let mut module = drafting_table
                        .representation
                        .modules
                        .get(current_module_id)
                        .unwrap()
                        .write();

                    if let Some(existing) = module
                        .member_ids_by_name
                        .get(&name)
                        .map(|x| {
                            drafting_table
                                .representation
                                .get_global((*x).into())
                                .unwrap()
                                .span()
                                .cloned()
                                .unwrap()
                        })
                        .or_else(|| {
                            module
                                .imports
                                .get(&name)
                                .map(|x| x.1.clone().unwrap())
                        })
                    {
                        handler.receive(Box::new(ConflictingUsing {
                            using_span: import.alias().as_ref().map_or_else(
                                || import.identifier().span(),
                                SourceElement::span,
                            ),
                            name: name.clone(),
                            module_id: current_module_id,
                            conflicting_span: existing,
                        }));
                    } else {
                        module.imports.insert(
                            name,
                            (
                                id,
                                Some(import.alias().as_ref().map_or_else(
                                    || import.identifier().span.clone(),
                                    |x| x.identifier().span.clone(),
                                )),
                            ),
                        );
                    }
                }
            }
        });

    // add implementation to the appropriate symbols
    for (current_module_id, implementation) in implementations_by_module_id
        .into_iter()
        .flat_map(|(current_module_id, implementations)| {
            implementations
                .into_iter()
                .map(move |implementation| (current_module_id, implementation))
        })
    {
        drafting_table.draft_implementation(
            implementation,
            current_module_id,
            handler,
        );
    }

    let finalizer = drafting_table.state.inner_state.finalizer;

    Table {
        representation: drafting_table.representation,
        state: Building::new(finalizer),
    }
}

#[allow(clippy::result_large_err)]
fn draft_table(
    targets: impl Iterator<Item = Target>,
    handler: &HandlerAdaptor,
) -> Result<Table<Building<RwLockContainer, Drafter>>, BuildTableError> {
    let mut drafting_table = Table {
        representation: Representation::default(),
        state: Building::new(Drafter::default()),
    };
    drafting_table.initialize_core();

    // draft all targets
    for target in targets {
        let (syntax_tree, name) = target.dissolve();
        let module_id = drafting_table.draft_module(
            syntax_tree,
            name.clone(),
            None,
            handler,
        );

        #[allow(clippy::significant_drop_in_scrutinee)]
        match drafting_table.representation.root_module_ids_by_name.entry(name)
        {
            Entry::Occupied(entry) => {
                return Err(BuildTableError::DuplicateTargetName(
                    entry.key().clone(),
                ));
            }
            Entry::Vacant(entry) => {
                entry.insert(module_id);
            }
        }
    }

    Ok(drafting_table)
}

/// Builds a symbol table from the given targets.
///
/// # Errors
///
/// See [`BuildTableError`] for more information.
#[allow(clippy::result_large_err)]
pub fn build(
    targets: impl Iterator<Item = Target>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<Table<Success>, BuildTableError> {
    let handler = HandlerAdaptor { handler, received: RwLock::new(false) };

    let building_table =
        transition_to_building(draft_table(targets, &handler)?, &handler);

    building_table.build_all(&handler);

    // unwrap mutexes and convert to the final table
    let representation = Representation::<NoContainer> {
        modules: convert_rw_locked_arena(building_table.representation.modules),
        structs: convert_rw_locked_arena(building_table.representation.structs),
        enums: convert_rw_locked_arena(building_table.representation.enums),
        variants: convert_rw_locked_arena(
            building_table.representation.variants,
        ),
        types: convert_rw_locked_arena(building_table.representation.types),
        functions: convert_rw_locked_arena(
            building_table.representation.functions,
        ),
        constants: convert_rw_locked_arena(
            building_table.representation.constants,
        ),
        traits: convert_rw_locked_arena(building_table.representation.traits),
        trait_types: convert_rw_locked_arena(
            building_table.representation.trait_types,
        ),
        trait_constants: convert_rw_locked_arena(
            building_table.representation.trait_constants,
        ),
        trait_functions: convert_rw_locked_arena(
            building_table.representation.trait_functions,
        ),
        positive_trait_implementations: convert_rw_locked_arena(
            building_table.representation.positive_trait_implementations,
        ),
        negative_trait_implementations: convert_rw_locked_arena(
            building_table.representation.negative_trait_implementations,
        ),
        trait_implementation_types: convert_rw_locked_arena(
            building_table.representation.trait_implementation_types,
        ),
        trait_implementation_functions: convert_rw_locked_arena(
            building_table.representation.trait_implementation_functions,
        ),
        trait_implementation_constants: convert_rw_locked_arena(
            building_table.representation.trait_implementation_constants,
        ),
        adt_implementations: convert_rw_locked_arena(
            building_table.representation.adt_implementations,
        ),
        adt_implementation_functions: convert_rw_locked_arena(
            building_table.representation.adt_implementation_functions,
        ),
        markers: convert_rw_locked_arena(building_table.representation.markers),
        positive_marker_implementations: convert_rw_locked_arena(
            building_table.representation.positive_marker_implementations,
        ),
        negative_marker_implementations: convert_rw_locked_arena(
            building_table.representation.negative_marker_implementations,
        ),
        root_module_ids_by_name: building_table
            .representation
            .root_module_ids_by_name,
    };

    if *handler.received.read_recursive() {
        Err(BuildTableError::Suboptimal(Table {
            representation,
            state: Suboptimal(()),
        }))
    } else {
        Ok(Table { representation, state: Success(()) })
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum InsertSymbolWithIDError<ParentID, ID> {
    #[error("the parent ID does not exist in the table")]
    InvalidParentID(ParentID),

    #[error("the symbol with the given name already exists")]
    ExistingSymbolName(ID),

    #[error("the symbol with the given existing ID already exists")]
    ExistingSymbolID,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum InsertImplementationError {
    #[error("the parent ID does not exist in the table")]
    InvalidParentModuleID,

    #[error("the implemented ID does not exist in the table")]
    InvalidImplementedID,
}

/// The result of the various insert functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Insertion<T, E> {
    /// The [`ID`] of the inserted symbol.
    pub id: ID<T>,

    /// If there exists a symbol with the same name in the scope, this field
    /// contains the ID of the existing symbol.
    pub duplication: Option<E>,
}

impl<T, E> Insertion<T, E> {
    /// Converts the [`Insertion`] to a tuple.
    #[must_use]
    pub fn unwrap_no_duplication(self) -> ID<T> {
        assert!(self.duplication.is_none(), "duplication found");
        self.id
    }
}

impl<T: Container> Representation<T> {
    /// Creates a new [`Module`] as a root module (target) in the table.
    pub fn create_root_module(
        &mut self,
        name: String,
    ) -> Insertion<Module, ID<Module>> {
        let id = self.modules.insert(T::wrap(Module {
            name: name.clone(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            member_ids_by_name: HashMap::new(),
            span: None,
            imports: HashMap::new(),
        }));

        let duplication = match self.root_module_ids_by_name.entry(name) {
            Entry::Occupied(entry) => Some(*entry.get()),
            Entry::Vacant(entry) => {
                entry.insert(id);
                None
            }
        };

        Insertion { id, duplication }
    }

    /// Inserts a new implementation to the given symbol.
    ///
    /// # Errors
    ///
    /// See [`InsertImplementationError`] for more information.
    #[allow(private_bounds, clippy::type_complexity)]
    pub fn insert_implementation<
        Definition,
        ImplementationID: From<
                ID<
                    GenericTemplate<
                        ID<Module>,
                        ImplementationTemplate<ImplementedID, Definition>,
                    >,
                >,
            > + Eq
            + Hash,
        Implemented: symbol::ImplementedMut<ImplementationID> + Element + Global,
        ImplementedID: Copy + From<ID<Implemented>>,
    >(
        &mut self,
        implemented_id: ID<Implemented>,
        parent_id: ID<Module>,
        generic_declaration: GenericDeclaration,
        span: Option<Span>,
        arguments: GenericArguments<Default>,
        definition: Definition,
    ) -> Result<
        ID<
            GenericTemplate<
                ID<Module>,
                ImplementationTemplate<ImplementedID, Definition>,
            >,
        >,
        InsertImplementationError,
    >
    where
        GenericTemplate<
            ID<Module>,
            ImplementationTemplate<ImplementedID, Definition>,
        >: Element,

        ID<Implemented>: Into<GlobalID>,
    {
        if self.modules.get(parent_id).is_none() {
            return Err(InsertImplementationError::InvalidParentModuleID);
        }

        let (name, accessibility) = {
            let implemented_sym = self
                .get(implemented_id)
                .ok_or(InsertImplementationError::InvalidImplementedID)?;

            (
                implemented_sym.name().to_owned(),
                self.get_accessibility(implemented_id.into()).unwrap(),
            )
        };

        // create trait implementation
        let implementation_id = GenericTemplate::get_arena_mut(self).insert(
            T::wrap(GenericTemplate {
                name,
                accessibility,
                parent_id,
                span,
                generic_declaration,
                definition: ImplementationTemplate {
                    arguments,
                    implemented_id: ImplementedID::from(implemented_id),
                    definition,
                },
            }),
        );

        // add to the implementation list
        let mut implemented_sym = self.get_mut(implemented_id).unwrap();
        assert!(implemented_sym
            .implementations_mut()
            .insert(implementation_id.into()));

        Ok(implementation_id)
    }

    /// Inserts a new [`Variant`] to the given parent [`Enum`].
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `parent_enum_id` is not a valid ID and the
    /// symbol will not be inserted.
    pub fn insert_variant(
        &mut self,
        name: String,
        parent_enum_id: ID<Enum>,
        associated_type: Option<r#type::Type<Default>>,
        span: Option<Span>,
    ) -> Option<Insertion<Variant, ID<Variant>>> {
        // the given `parent_id` does not exist, fatal error
        let _ = self.get(parent_enum_id)?;

        // create trait member
        let member_id = self.variants.insert(T::wrap(Variant {
            name: name.clone(),
            parent_enum_id,
            associated_type,
            span,
        }));

        // add the trait member to the parent trait
        let mut parent_symbol = self.get_mut(parent_enum_id).unwrap();

        let duplication = match parent_symbol.variant_ids_by_name.entry(name) {
            Entry::Occupied(entry) => Some(*entry.get()),
            Entry::Vacant(entry) => {
                entry.insert(member_id);
                None
            }
        };

        if duplication.is_none() {
            parent_symbol.variant_declaration_order.push(member_id);
        }

        Some(Insertion { id: member_id, duplication })
    }

    /// Inserts a new member to the given parent symbol.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `parent_id` is not a valid ID and the
    /// symbol will not be inserted.
    #[allow(private_bounds)]
    pub fn insert_member<
        Definition,
        Parent: symbol::ParentMut + Element,
        ParentID: Copy + From<ID<Parent>>,
    >(
        &mut self,
        name: String,
        accessibility: Accessibility,
        parent_id: ID<Parent>,
        span: Option<Span>,
        generic_declaration: GenericDeclaration,
        definition: Definition,
    ) -> Option<
        Insertion<GenericTemplate<ParentID, Definition>, Parent::MemberID>,
    >
    where
        ID<GenericTemplate<ParentID, Definition>>: Into<Parent::MemberID>,
        GenericTemplate<ParentID, Definition>: Element,
    {
        // the given `parent_id` does not exist, fatal error
        let _ = self.get(parent_id)?;

        // create trait member
        let member_id = GenericTemplate::get_arena_mut(self).insert(T::wrap(
            GenericTemplate {
                name: name.clone(),
                accessibility,
                parent_id: parent_id.into(),
                span,
                generic_declaration,
                definition,
            },
        ));

        // add the trait member to the parent trait
        let mut parent_symbol = self.get_mut(parent_id).unwrap();

        let duplication =
            match parent_symbol.member_ids_by_name_mut().entry(name) {
                Entry::Occupied(entry) => Some(*entry.get()),
                Entry::Vacant(entry) => {
                    entry.insert(member_id.into());
                    None
                }
            };

        Some(Insertion { id: member_id, duplication })
    }

    /// Inserts a new child module to the given parent module.
    ///
    /// # Returns
    ///
    /// Returns [`None`] if the given `parent_id` is not a valid ID and the
    /// symbol will not be inserted.
    pub fn insert_module(
        &mut self,
        name: String,
        accessibility: Accessibility,
        parent_module_id: ID<Module>,
        span: Option<Span>,
    ) -> Option<Insertion<Module, ModuleMemberID>> {
        // the given `parent_module_id` does not exist, fatal error
        let _ = self.get(parent_module_id)?;

        // create module member
        let member_id = self.modules.insert(T::wrap(Module {
            name: name.clone(),
            accessibility,
            parent_module_id: Some(parent_module_id),
            member_ids_by_name: HashMap::new(),
            imports: HashMap::new(),
            span,
        }));

        // add the module member to the parent module
        let mut parent_module = self.get_mut(parent_module_id).unwrap();

        let duplication = match parent_module.member_ids_by_name.entry(name) {
            Entry::Occupied(entry) => Some(*entry.get()),
            Entry::Vacant(entry) => {
                entry.insert(member_id.into());
                None
            }
        };

        Some(Insertion { id: member_id, duplication })
    }
}

impl<T: Container> Index<TraitImplementationID> for Representation<T> {
    type Output<'a> =
        T::MappedRead<'a, dyn ResolvableImplementation<ID<Trait>>>;

    fn get(&self, index: TraitImplementationID) -> Option<Self::Output<'_>> {
        match index {
            TraitImplementationID::Positive(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            TraitImplementationID::Negative(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
        }
    }
}

impl<T: Container> Index<MarkerImplementationID> for Representation<T> {
    type Output<'a> =
        T::MappedRead<'a, dyn ResolvableImplementation<ID<Marker>>>;

    fn get(&self, index: MarkerImplementationID) -> Option<Self::Output<'_>> {
        match index {
            MarkerImplementationID::Positive(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
            MarkerImplementationID::Negative(id) => {
                self.get(id).map(|x| T::map_read(x, |x| x as _))
            }
        }
    }
}

impl<T: State + std::default::Default> std::default::Default for Table<T> {
    fn default() -> Self {
        let mut representation = Representation::default();
        representation.initialize_core();

        Self { representation, state: T::default() }
    }
}

#[cfg(test)]
pub(crate) mod test;
