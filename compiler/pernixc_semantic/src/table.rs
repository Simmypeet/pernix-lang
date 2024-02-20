//! Contains the definition of [`Table`]

use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    ops::Deref,
};

use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use paste::paste;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::target::Target;
use rayon::iter::ParallelIterator;

use self::state::drafting::Drafting;
use crate::{
    arena::{Arena, ID},
    error,
    semantic::term::{constant, lifetime::Lifetime, r#type, GenericArguments},
    symbol::{
        Accessibility, AdtImplementation, AdtImplementationConstant,
        AdtImplementationFunction, AdtImplementationType, Constant, Enum,
        Function, Generic, GenericID, Global, GlobalID,
        ImplementationSignature, Module, NegativeTraitImplementation, Struct,
        Trait, TraitConstant, TraitFunction, TraitImplementation,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationKindID, TraitImplementationType, TraitType, Type,
        Variant,
    },
};

mod state;

/// A trait used to access the symbols defined in the table.
pub trait Index<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output<'a>: Sized
    where
        Self: 'a;

    /// Returns the output of the indexing operation if the index is valid.
    fn get(&self, index: Idx) -> Option<Self::Output<'_>>;
}

impl<T: Element, S: Container> Index<ID<T>> for Representation<S>
where
    ID<T>: Into<GlobalID>,
{
    type Output<'a> = S::Read<'a, T> where Self: 'a;

    fn get(&self, index: ID<T>) -> Option<Self::Output<'_>> {
        T::get_arena(self).get(index).map(|x| S::read(x))
    }
}

/// A trait used to wrap a symbol in a container.
///
/// This is primarily used to either wrap a symbol in a [`RwLock`] or not at
/// all.
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
    + Default
    + 'static
    + Send
    + Sync
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
    fn read<T: Debug + 'static + Send + Sync>(
        value: &Self::Wrap<T>,
    ) -> Self::Read<'_, T>;

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
    type Wrap<T: Debug + 'static + Send + Sync> = RwLock<T>;

    fn wrap<T: Debug + 'static + Send + Sync>(value: T) -> Self::Wrap<T> {
        RwLock::new(value)
    }

    fn read<T: Debug + 'static + Send + Sync>(
        value: &Self::Wrap<T>,
    ) -> Self::Read<'_, T> {
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
}

/// A struct which implements [`State`] used to signify that the table is built
/// with some errors and is not suitable for the next phase (i.e. code
/// generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal(());

impl State for Suboptimal {
    type Container = NoContainer;
}

/// A struct which implements [`State`] used to signify that the table is built
/// successfully and is ready to be used for the next phase (i.e. code
/// generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Success(() /* Preventing arbitrary instantiation */);

impl State for Success {
    type Container = NoContainer;
}

/// Represents a state object for the [`Table`].
///
/// This is used to distinguish between the states of the symbols in the table.
#[doc(hidden)]
pub trait State: Debug + 'static + Send + Sync {
    /// The container type used to wrap the symbols in the table.
    type Container: Container;
}

/// The representation of the table without any state information.
#[derive(Debug)]
pub struct Representation<T: Container> {
    modules: Arena<T::Wrap<Module>, ID<Module>>,
    structs: Arena<T::Wrap<Struct>, ID<Struct>>,
    enums: Arena<T::Wrap<Enum>, ID<Enum>>,
    variants: Arena<T::Wrap<Variant>, ID<Variant>>,
    pub(crate) types: Arena<T::Wrap<Type>, ID<Type>>,
    functions: Arena<T::Wrap<Function>, ID<Function>>,
    constants: Arena<T::Wrap<Constant>, ID<Constant>>,

    traits: Arena<T::Wrap<Trait>, ID<Trait>>,
    trait_types: Arena<T::Wrap<TraitType>, ID<TraitType>>,
    trait_functions: Arena<T::Wrap<TraitFunction>, ID<TraitFunction>>,
    trait_constants: Arena<T::Wrap<TraitConstant>, ID<TraitConstant>>,

    trait_implementations:
        Arena<T::Wrap<TraitImplementation>, ID<TraitImplementation>>,
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

    adt_implementation_types:
        Arena<T::Wrap<AdtImplementationType>, ID<AdtImplementationType>>,
    adt_implementation_functions: Arena<
        T::Wrap<AdtImplementationFunction>,
        ID<AdtImplementationFunction>,
    >,
    adt_implementation_constants: Arena<
        T::Wrap<AdtImplementationConstant>,
        ID<AdtImplementationConstant>,
    >,

    #[allow(unused)]
    root_module_ids_by_name: HashMap<String, ID<Module>>,
}

impl<T: Container> Default for Representation<T> {
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
            trait_implementations: Arena::default(),
            negative_trait_implementations: Arena::default(),
            trait_implementation_types: Arena::default(),
            trait_implementation_functions: Arena::default(),
            trait_implementation_constants: Arena::default(),
            adt_implementations: Arena::default(),
            adt_implementation_types: Arena::default(),
            adt_implementation_functions: Arena::default(),
            adt_implementation_constants: Arena::default(),
            root_module_ids_by_name: HashMap::default(),
        }
    }
}

/// Contains all the symbols and information defined in the target.
#[derive(Debug, derive_more::Deref)]
pub struct Table<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Container>,

    #[allow(unused)]
    state: T,
}

impl<T: State + Default> Default for Table<T> {
    fn default() -> Self {
        Self { representation: Representation::default(), state: T::default() }
    }
}

/// A trait used to access the symbols defined in the table.
trait Element: Sized + Debug + Send + Sync + 'static {
    /// Gets the arena reference containing *this* kind of symbol.
    fn get_arena<T: Container>(
        table: &Representation<T>,
    ) -> &Arena<T::Wrap<Self>, ID<Self>>;

    /// Gets the mutable arena reference containing *this* kind of symbol.
    fn get_arena_mut<T: Container>(
        table: &mut Representation<T>,
    ) -> &mut Arena<T::Wrap<Self>, ID<Self>>;
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
implements_element!(TraitImplementation);
implements_element!(NegativeTraitImplementation);
implements_element!(TraitImplementationType);
implements_element!(TraitImplementationConstant);
implements_element!(TraitImplementationFunction);
implements_element!(AdtImplementation);
implements_element!(AdtImplementationType);
implements_element!(AdtImplementationConstant);
implements_element!(AdtImplementationFunction);

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

impl<T: Container> Representation<T> {
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
        match self.get_accessibility(referred)? {
            Accessibility::Public => {
                // PEDANTIC: check if the referring site is a valid ID.
                drop(self.get_global(referring_site)?);

                Some(true)
            }
            Accessibility::Private => {
                let mut referring_module_id =
                    self.get_closet_module_id(referring_site)?;
                let referred_module_id = self.get_closet_module_id(
                    self.get_global(referred)?
                        .parent_global_id()
                        .unwrap_or(referred),
                )?;

                loop {
                    if referring_module_id == referred_module_id {
                        return Some(true);
                    }

                    let Some(next) =
                        self.get(referring_module_id)?.parent_module_id
                    else {
                        return Some(false);
                    };

                    referring_module_id = next;
                }
            }
            Accessibility::Internal => Some(
                self.get_root_module_id(referred)?
                    == self.get_root_module_id(referring_site)?,
            ),
        }
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

    /// Returns the [`ImplementationSignature`] of the given
    /// [`TraitImplementationKindID`].
    #[must_use]
    pub fn get_trait_implementation_signature(
        &self,
        trait_implementation_kind: TraitImplementationKindID,
    ) -> Option<T::MappedRead<'_, ImplementationSignature<ID<Trait>>>> {
        match trait_implementation_kind {
            TraitImplementationKindID::Positive(id) => {
                self.get(id).map(|x| T::map_read(x, |x| &x.signature))
            }
            TraitImplementationKindID::Negative(id) => {
                self.get(id).map(|x| T::map_read(x, |x| &x.signature))
            }
        }
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
        qualified_names: impl Iterator<Item = &'a str>,
    ) -> Result<GlobalID, GetByQualifiedNameError<'a>> {
        let mut current_id: Option<GlobalID> = None;

        for name in qualified_names {
            match current_id {
                Some(searched_in_global_id) => {
                    current_id = Some(
                        self.get_global(searched_in_global_id)
                            .unwrap()
                            .get_member(name)
                            .ok_or(GetByQualifiedNameError::SymbolNotFound {
                                searched_in_global_id: Some(
                                    searched_in_global_id,
                                ),
                                name,
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
    pub fn get_qualified_name(&self, global_id: GlobalID) -> Option<String> {
        match global_id {
            GlobalID::TraitImplementation(id) => self.get_qualified_name(
                self.get(id)?.signature.implemented_id.into(),
            ),
            GlobalID::NegativeTraitImplementation(id) => self
                .get_qualified_name(
                    self.get(id)?.signature.implemented_id.into(),
                ),

            GlobalID::TraitImplementationType(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_id)
                        .unwrap()
                        .signature
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
                        .signature
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
                        .signature
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

    /// Gets overall accessibility of the given [`r#type::Type`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the
    /// least accessible type component of the given [`r#type::Type`].
    ///
    /// Example:
    ///
    /// ``` txt
    /// private struct Foo{}
    /// internal struct Bar{}
    ///
    /// // The overall accessibility of `Spam<Foo, Bar>` is `private`.
    /// ```
    ///
    /// # Returns
    ///
    /// `None` if the type contains an invalid id as its component.
    #[must_use]
    pub fn get_type_overall_accessibility(
        &self,
        ty: &r#type::Type,
    ) -> Option<Accessibility> {
        match ty {
            r#type::Type::Local(local) => {
                self.get_type_overall_accessibility(&local.0)
            }
            r#type::Type::Inference(never) => match *never {},
            r#type::Type::Symbol(adt) => {
                Some(self.get_accessibility(adt.id.into())?.min(
                    self.get_generic_arguments_overall_accessibility(
                        &adt.generic_arguments,
                    )?,
                ))
            }
            r#type::Type::Pointer(pointer) => {
                self.get_type_overall_accessibility(&pointer.pointee)
            }
            r#type::Type::Reference(reference) => Some(
                self.get_lifetime_overall_accessibility(&reference.lifetime)?
                    .min(
                        self.get_type_overall_accessibility(
                            &reference.pointee,
                        )?,
                    ),
            ),
            r#type::Type::Array(array) => Some(
                self.get_constant_overall_accessibility(&array.length)?
                    .min(self.get_type_overall_accessibility(&array.r#type)?),
            ),
            r#type::Type::Parameter(parameter) => {
                self.get_accessibility(parameter.parent.into())
            }
            r#type::Type::Primitive(_) => Some(Accessibility::Public),
            r#type::Type::Tuple(tuple) => {
                let mut current_min = Accessibility::Public;

                for element in &tuple.elements {
                    current_min = current_min.min(
                        self.get_type_overall_accessibility(element.as_term())?,
                    );
                }

                Some(current_min)
            }
            r#type::Type::MemberSymbol(member_symbol) => Some(
                self.get_accessibility(member_symbol.id.into())?
                    .min(self.get_generic_arguments_overall_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?)
                    .min(self.get_generic_arguments_overall_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?),
            ),
            r#type::Type::TraitMember(member_symbol) => Some(
                self.get_accessibility(member_symbol.id.into())?.min(
                    self.get_generic_arguments_overall_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?
                    .min(
                        self.get_generic_arguments_overall_accessibility(
                            &member_symbol.member_generic_arguments,
                        )?,
                    ),
                ),
            ),
        }
    }

    /// Gets overall accessibility of the given [`constant::Constant`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the
    /// least accessible component of the given [`constant::Constant`].
    ///
    /// # Returns
    ///
    /// `None` if the constant contains an invalid id as its component.
    #[must_use]
    pub fn get_constant_overall_accessibility(
        &self,
        constant: &constant::Constant,
    ) -> Option<Accessibility> {
        match constant {
            constant::Constant::Local(local) => {
                self.get_constant_overall_accessibility(&local.0)
            }
            constant::Constant::TraitMember(trait_member) => Some(
                self.get_accessibility(trait_member.id.into())?.min(
                    self.get_generic_arguments_overall_accessibility(
                        &trait_member.parent_generic_arguments,
                    )?
                    .min(
                        self.get_generic_arguments_overall_accessibility(
                            &trait_member.member_generic_arguments,
                        )?,
                    ),
                ),
            ),
            constant::Constant::Inference(never) => match *never {},

            constant::Constant::Struct(constant) => {
                let mut current_min =
                    self.get_accessibility(constant.id.into())?;

                for field in &constant.fields {
                    current_min = current_min
                        .min(self.get_constant_overall_accessibility(field)?);
                }

                Some(current_min)
            }

            constant::Constant::Array(constant) => constant
                .elements
                .iter()
                .map(|x| self.get_constant_overall_accessibility(x))
                .try_fold(Accessibility::Public, |acc, x| Some(acc.min(x?))),

            constant::Constant::Parameter(_)
            | constant::Constant::Primitive(_) => Some(Accessibility::Public),

            constant::Constant::MemberSymbol(member_symbol) => Some(
                self.get_accessibility(member_symbol.id.into())?
                    .min(self.get_generic_arguments_overall_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?)
                    .min(self.get_generic_arguments_overall_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?),
            ),

            constant::Constant::Enum(constant) => {
                let mut current_min =
                    self.get_accessibility(constant.variant_id.into())?;

                if let Some(associated_value) = &constant.associated_value {
                    current_min = current_min.min(
                        self.get_constant_overall_accessibility(
                            associated_value,
                        )?,
                    );
                }

                Some(current_min)
            }

            constant::Constant::Symbol(symbol) => {
                Some(self.get_accessibility(symbol.id.into())?.min(
                    self.get_generic_arguments_overall_accessibility(
                        &symbol.generic_arguments,
                    )?,
                ))
            }

            constant::Constant::Tuple(tuple) => {
                let mut current_min = Accessibility::Public;

                for element in &tuple.elements {
                    current_min = current_min.min(
                        self.get_constant_overall_accessibility(
                            element.as_term(),
                        )?,
                    );
                }

                Some(current_min)
            }
        }
    }

    /// Gets overall accessibility of the given [`Lifetime`].
    ///
    /// # Returns
    ///
    /// `None` if the lifetime contains an invalid id as its component.
    #[must_use]
    pub fn get_lifetime_overall_accessibility(
        &self,
        lifetime: &Lifetime,
    ) -> Option<Accessibility> {
        match lifetime {
            Lifetime::Parameter(lifetime_parameter_id) => {
                self.get_accessibility(lifetime_parameter_id.parent.into())
            }

            Lifetime::Inference(never) => match never.0 {},
            Lifetime::Local(local) => match local.0 {},

            Lifetime::Forall(_) | Lifetime::Static => {
                Some(Accessibility::Public)
            }
        }
    }

    /// Gets overall accessibility of the given [`GenericArguments`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the
    /// least accessible component of the given [`GenericArguments`].
    ///
    /// # Returns
    ///
    /// `None` if the generic arguments contains an invalid id as its component.
    /// If the generic arguments is empty, returns
    /// `Some(Accessibility::Public)`.
    #[must_use]
    pub fn get_generic_arguments_overall_accessibility(
        &self,
        generic_arguments: &GenericArguments,
    ) -> Option<Accessibility> {
        let mut current_min = Accessibility::Public;

        for lifetime in &generic_arguments.lifetimes {
            current_min = current_min
                .min(self.get_lifetime_overall_accessibility(lifetime)?);
        }

        for ty in &generic_arguments.types {
            current_min =
                current_min.min(self.get_type_overall_accessibility(ty)?);
        }

        for constant in &generic_arguments.constants {
            current_min = current_min
                .min(self.get_constant_overall_accessibility(constant)?);
        }

        Some(current_min)
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

    /// Gets the accessibility of the given [`GlobalID`].
    ///
    /// # Returns
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
                    $table.[<$kind:snake s>].get($id).map(|$id| T::read($id).accessibility)
                }
            };

            ($table:ident, $id:ident, $kind:ident, $expr:expr) => {
                paste! {
                    $table.[<$kind:snake s>].get($id).and_then(|$id| $expr)
                }
            };
        }
        macro_rules! get_accessibility {
            ($self:ident, $table:ident, $(($kind:ident $(, $expr:expr)?)),*) => {

                match $self {
                    $(
                        GlobalID::$kind($self) => arm_expression!($table, $self, $kind $(, $expr)?),
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
            (AdtImplementationType),
            (AdtImplementationFunction),
            (AdtImplementationConstant),
            (
                Variant,
                self.get_accessibility(
                    T::read(global_id).parent_enum_id.into()
                )
            ),
            (
                TraitType,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                TraitConstant,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                TraitFunction,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                TraitImplementation,
                self.get_accessibility(
                    T::read(global_id).signature.implemented_id.into()
                )
            ),
            (
                TraitImplementationType,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                TraitImplementationFunction,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                TraitImplementationConstant,
                self.get_accessibility(T::read(global_id).parent_id.into())
            ),
            (
                NegativeTraitImplementation,
                self.get_accessibility(
                    T::read(global_id).signature.implemented_id.into()
                )
            ),
            (AdtImplementation, {
                let implemented_id =
                    T::read(global_id).signature.implemented_id;
                match implemented_id {
                    crate::symbol::AdtKindID::Struct(id) => {
                        self.get_accessibility(id.into())
                    }
                    crate::symbol::AdtKindID::Enum(id) => {
                        self.get_accessibility(id.into())
                    }
                }
            })
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
            TraitImplementation,
            TraitImplementationType,
            TraitImplementationConstant,
            TraitImplementationFunction,
            NegativeTraitImplementation,
            AdtImplementation,
            AdtImplementationType,
            AdtImplementationConstant,
            AdtImplementationFunction
        )
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
            TraitImplementation,
            TraitImplementationType,
            TraitImplementationConstant,
            NegativeTraitImplementation,
            AdtImplementation,
            AdtImplementationType,
            AdtImplementationFunction,
            AdtImplementationConstant
        )
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order.
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
        new_arena.insert_with_id(idx, value.into_inner()).unwrap();
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

fn transition_to_building(
    drafting_table: Table<state::drafting::Drafting>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Table<state::building::Building> {
    let representation = drafting_table.representation;
    let drafting = drafting_table.state;

    // transition to building table
    let building = drafting.building;
    let building_table = Table { representation, state: building };

    // add usings to the modules
    for (module_id, usings) in drafting.usings_by_module_id {
        for using in usings {
            let Ok(using_module_id) = building_table.resolve_module_path(
                using.module_path(),
                module_id.into(),
                handler,
            ) else {
                continue;
            };

            building_table
                .modules
                .get(module_id)
                .unwrap()
                .write()
                .usings
                .insert(using_module_id);
        }
    }

    building_table
}

#[allow(clippy::result_large_err)]
fn draft_table(
    targets: impl ParallelIterator<Item = Target>,
    handler: &HandlerAdaptor,
) -> Result<Table<Drafting>, BuildTableError> {
    let drafting_table = RwLock::new(Table {
        representation: Representation::default(),
        state: state::drafting::Drafting::default(),
    });
    // draft all targets
    targets.try_for_each(|target| {
        let (syntax_tree, name) = target.dissolve();
        let module_id = Table::draft_module(
            &drafting_table,
            syntax_tree,
            name.clone(),
            None,
            handler,
        );

        #[allow(clippy::significant_drop_in_scrutinee)]
        match drafting_table
            .write()
            .representation
            .root_module_ids_by_name
            .entry(name)
        {
            Entry::Occupied(entry) => {
                Err(BuildTableError::DuplicateTargetName(entry.key().clone()))
            }
            Entry::Vacant(entry) => {
                entry.insert(module_id);
                Ok(())
            }
        }
    })?;

    Ok(drafting_table.into_inner())
}

/// Builds a symbol table from the given targets.
///
/// # Errors
///
/// See [`BuildTableError`] for more information.
#[allow(clippy::result_large_err)]
pub fn build(
    targets: impl ParallelIterator<Item = Target>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<Table<Success>, BuildTableError> {
    let handler = HandlerAdaptor { handler, received: RwLock::new(false) };

    let building_table =
        transition_to_building(draft_table(targets, &handler)?, &handler);

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
        trait_implementations: convert_rw_locked_arena(
            building_table.representation.trait_implementations,
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
        adt_implementation_types: convert_rw_locked_arena(
            building_table.representation.adt_implementation_types,
        ),
        adt_implementation_functions: convert_rw_locked_arena(
            building_table.representation.adt_implementation_functions,
        ),
        adt_implementation_constants: convert_rw_locked_arena(
            building_table.representation.adt_implementation_constants,
        ),
        root_module_ids_by_name: building_table
            .representation
            .root_module_ids_by_name,
    };

    if *handler.received.read() {
        Err(BuildTableError::Suboptimal(Table {
            representation,
            state: Suboptimal(()),
        }))
    } else {
        Ok(Table { representation, state: Success(()) })
    }
}
