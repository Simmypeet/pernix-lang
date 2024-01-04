//! Contains the definition of [`Table`]

use std::{collections::HashMap, fmt::Debug, hash::Hash, ops::Deref};

use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use paste::paste;

use crate::{
    arena::{Arena, ID},
    symbol::{
        AdtImplementation, AdtImplementationConstant, AdtImplementationFunction,
        AdtImplementationType, Constant, Enum, Function, GlobalID, Module,
        NegativeTraitImplementation, Struct, Trait, TraitConstant, TraitFunction,
        TraitImplementation, TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type, Variant,
    },
};

/// A trait used to access the symbols defined in the table.
pub trait Index<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output<'a>: Sized
    where
        Self: 'a;

    /// Returns the output of the indexing operation if the index is valid.
    fn get(&self, index: Idx) -> Option<Self::Output<'_>>;
}

impl<T: Element, S: State> Index<ID<T>> for Representation<S>
where
    ID<T>: Into<GlobalID>,
{
    type Output<'a> = <S::Container as Container>::Read<'a, T> where Self: 'a;

    fn get(&self, index: ID<T>) -> Option<Self::Output<'_>> {
        T::get_arena(self).get(index).map(|x| S::Container::read(x))
    }
}

/// A trait used to wrap a symbol in a container.
///
/// This is primarily used to either wrap a symbol in a [`RwLock`] or not at all.
///
/// See [`RwLockContainer`] for an example.
pub trait Container:
    Debug + Clone + Copy + PartialEq + Eq + PartialOrd + Ord + Hash + Default + 'static + Send + Sync
{
    /// The type of the wrapped value.
    type Wrap<T: Debug + 'static + Send + Sync>: Debug + 'static + Send + Sync;

    /// The type of the read guard of the wrapped value.
    #[clippy::has_significant_drop]
    type Read<'a, T: ?Sized + 'a>: Deref<Target = T> + 'a;

    /// The type of the mapped read guard of the wrapped value.
    #[clippy::has_significant_drop]
    type MappedRead<'a, T: ?Sized + 'a>: Deref<Target = T> + 'a;

    /// Wraps the given value.
    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T>;

    /// Reads the given value.
    fn read<T: Debug + 'static + Send + Sync>(value: &Self::Wrap<T>) -> Self::Read<'_, T>;

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

/// A struct which implements [`Container`] by wrapping the value in a [`RwLock`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct RwLockContainer;

impl Container for RwLockContainer {
    type MappedRead<'a, T: ?Sized + 'a> = MappedRwLockReadGuard<'a, T>;
    type Read<'a, T: ?Sized + 'a> = RwLockReadGuard<'a, T>;
    type Wrap<T: Debug + 'static + Send + Sync> = RwLock<T>;

    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T> { RwLock::new(value) }

    fn read<T: Debug + 'static + Send + Sync>(value: &Self::Wrap<T>) -> Self::Read<'_, T> {
        value.read()
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
}

/// A struct which implements [`Container`] by not wrapping the value at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoContainer;

impl Container for NoContainer {
    type MappedRead<'a, T: ?Sized + 'a> = &'a T;
    type Read<'a, T: ?Sized + 'a> = &'a T;
    type Wrap<T: Debug + 'static + Send + Sync> = T;

    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T> { value }

    fn read<T: Debug + 'static + Send + Sync>(value: &Self::Wrap<T>) -> Self::Read<'_, T> { value }

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
}

/// A struct which implements [`State`] used to signify that the table is built with some errors
/// and is not suitable for the next phase (i.e. code generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl private::Sealed for Suboptimal {}

impl State for Suboptimal {
    type Container = NoContainer;

    fn on_global_id_resolved(_: &Table<Self>, _: GlobalID, _: GlobalID) {}
}

/// A struct which implements [`State`] used to signify that the table is built successfully and
/// is ready to be used for the next phase (i.e. code generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success;

impl private::Sealed for Success {}

impl State for Success {
    type Container = NoContainer;

    fn on_global_id_resolved(_: &Table<Self>, _: GlobalID, _: GlobalID) {}
}

mod private {
    pub trait Sealed {}
}

/// Represents a state object for the [`Table`].
///
/// This is used to distiguish between the states of the symbols in the table.
#[doc(hidden)]
pub trait State: Default + Debug + private::Sealed + 'static + Send + Sync {
    /// The container type used to wrap the symbols in the table.
    type Container: Container;

    /// Gets notified when the table performs a symbol resolution and resolves the given
    /// [`GlobalID`]
    ///
    /// This is notified before the generic arguments are resolved for the given [`GlobalID`].
    fn on_global_id_resolved(
        table: &Table<Self>,
        referring_site: GlobalID,
        resolved_global_id: GlobalID,
    );
}

/// The representation of the table without any state information.
#[derive(Debug)]
pub struct Representation<T: State> {
    modules: Arena<<T::Container as Container>::Wrap<Module>, ID<Module>>,
    structs: Arena<<T::Container as Container>::Wrap<Struct>, ID<Struct>>,
    enums: Arena<<T::Container as Container>::Wrap<Enum>, ID<Enum>>,
    variants: Arena<<T::Container as Container>::Wrap<Variant>, ID<Variant>>,
    pub(crate) types: Arena<<T::Container as Container>::Wrap<Type>, ID<Type>>,
    functions: Arena<<T::Container as Container>::Wrap<Function>, ID<Function>>,
    constants: Arena<<T::Container as Container>::Wrap<Constant>, ID<Constant>>,

    traits: Arena<<T::Container as Container>::Wrap<Trait>, ID<Trait>>,
    trait_types: Arena<<T::Container as Container>::Wrap<TraitType>, ID<TraitType>>,
    trait_functions: Arena<<T::Container as Container>::Wrap<TraitFunction>, ID<TraitFunction>>,
    trait_constants: Arena<<T::Container as Container>::Wrap<TraitConstant>, ID<TraitConstant>>,

    trait_implementations:
        Arena<<T::Container as Container>::Wrap<TraitImplementation>, ID<TraitImplementation>>,
    negative_trait_implementations: Arena<
        <T::Container as Container>::Wrap<NegativeTraitImplementation>,
        ID<NegativeTraitImplementation>,
    >,

    trait_implementation_types: Arena<
        <T::Container as Container>::Wrap<TraitImplementationType>,
        ID<TraitImplementationType>,
    >,
    trait_implementation_functions: Arena<
        <T::Container as Container>::Wrap<TraitImplementationFunction>,
        ID<TraitImplementationFunction>,
    >,
    trait_implementation_constants: Arena<
        <T::Container as Container>::Wrap<TraitImplementationConstant>,
        ID<TraitImplementationConstant>,
    >,

    adt_implementations:
        Arena<<T::Container as Container>::Wrap<AdtImplementation>, ID<AdtImplementation>>,

    adt_implementation_types:
        Arena<<T::Container as Container>::Wrap<AdtImplementationType>, ID<AdtImplementationType>>,
    adt_implementation_functions: Arena<
        <T::Container as Container>::Wrap<AdtImplementationFunction>,
        ID<AdtImplementationFunction>,
    >,
    adt_implementation_constants: Arena<
        <T::Container as Container>::Wrap<AdtImplementationConstant>,
        ID<AdtImplementationConstant>,
    >,

    root_module_ids_by_name: HashMap<String, ID<Module>>,
}

/// Contains all the symbols and information defined in the target.
#[derive(Debug, derive_more::Deref)]
pub struct Table<T: State> {
    #[deref]
    pub(crate) representation: Representation<T>,
    state: T,
}

impl<T: State> Table<T> {
    pub(crate) fn default() -> Self {
        Self {
            representation: Representation {
                modules: Arena::default(),
                structs: Arena::default(),
                enums: Arena::default(),
                types: Arena::default(),
                functions: Arena::default(),
                constants: Arena::default(),
                traits: Arena::default(),
                variants: Arena::default(),
                trait_implementations: Arena::default(),
                trait_implementation_constants: Arena::default(),
                trait_implementation_functions: Arena::default(),
                trait_implementation_types: Arena::default(),
                trait_constants: Arena::default(),
                trait_functions: Arena::default(),
                trait_types: Arena::default(),
                negative_trait_implementations: Arena::default(),
                adt_implementations: Arena::default(),
                adt_implementation_constants: Arena::default(),
                adt_implementation_functions: Arena::default(),
                adt_implementation_types: Arena::default(),

                root_module_ids_by_name: HashMap::new(),
            },
            state: T::default(),
        }
    }
}

/// A trait used to access the symbols defined in the table.
trait Element: Sized + Debug + Send + Sync + 'static {
    /// Gets the arena reference containing *this* kind of symbol.
    fn get_arena<T: State>(
        table: &Representation<T>,
    ) -> &Arena<<T::Container as Container>::Wrap<Self>, ID<Self>>;

    /// Gets the mutable arena reference containing *this* kind of symbol.
    fn get_arena_mut<T: State>(
        table: &mut Representation<T>,
    ) -> &mut Arena<<T::Container as Container>::Wrap<Self>, ID<Self>>;
}

macro_rules! implements_symbol {
    ($symbol:ident) => {
        impl Element for $symbol {
            /// Gets the arena reference containing *this* kind of symbol.
            fn get_arena<T: State>(
                table: &Representation<T>,
            ) -> &Arena<<T::Container as Container>::Wrap<Self>, ID<Self>> {
                paste! {
                    &table.[<$symbol:snake s>]
                }
            }

            /// Gets the mutable arena reference containing *this* kind of symbol.
            fn get_arena_mut<T: State>(
                table: &mut Representation<T>,
            ) -> &mut Arena<<T::Container as Container>::Wrap<Self>, ID<Self>> {
                paste! {
                    &mut table.[<$symbol:snake s>]
                }
            }
        }
    };
}

implements_symbol!(Module);
implements_symbol!(Struct);
implements_symbol!(Enum);
implements_symbol!(Type);
implements_symbol!(Function);
implements_symbol!(Constant);
implements_symbol!(Trait);
implements_symbol!(TraitType);
implements_symbol!(TraitFunction);
implements_symbol!(TraitConstant);
implements_symbol!(Variant);
implements_symbol!(TraitImplementation);
implements_symbol!(NegativeTraitImplementation);
implements_symbol!(TraitImplementationType);
implements_symbol!(TraitImplementationConstant);
implements_symbol!(TraitImplementationFunction);
implements_symbol!(AdtImplementation);
implements_symbol!(AdtImplementationType);
implements_symbol!(AdtImplementationConstant);
implements_symbol!(AdtImplementationFunction);
