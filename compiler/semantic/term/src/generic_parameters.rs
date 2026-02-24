//! Contains the definition of [`GenericParameters`] component.
use std::collections::hash_map::Entry;

use derive_new::new;
use getset::Getters;
use paste::paste;
use pernixc_arena::Arena;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::MemberID;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    instance::{Instance, TraitRef},
    lifetime::Lifetime,
    r#type::Type,
};

/// Key for querying generic parameters for a given global symbol ID.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Interned<GenericParameters>)]
#[extend(name = get_generic_parameters, by_val)]
pub struct Key {
    /// The global symbol ID to get the generic parameters for.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

/// Represents the generic parameters (e.g. `['a, T, const C: TYPE]`).
/// It contains the list of lifetime parameters, type parameters, and constant
/// parameters, as well as the order of their declaration and the maps that
/// maps the name of the generic parameters to their IDs.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Getters,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
pub struct GenericParameters {
    lifetimes: Arena<LifetimeParameter>,
    types: Arena<TypeParameter>,
    constants: Arena<ConstantParameter>,
    instances: Arena<InstanceParameter>,

    lifetime_order: Vec<pernixc_arena::ID<LifetimeParameter>>,
    type_order: Vec<pernixc_arena::ID<TypeParameter>>,
    constant_order: Vec<pernixc_arena::ID<ConstantParameter>>,
    instance_order: Vec<pernixc_arena::ID<InstanceParameter>>,

    lifetime_parameter_ids_by_name:
        HashMap<Interned<str>, pernixc_arena::ID<LifetimeParameter>>,
    type_parameter_ids_by_name:
        HashMap<Interned<str>, pernixc_arena::ID<TypeParameter>>,
    constant_parameter_ids_by_name:
        HashMap<Interned<str>, pernixc_arena::ID<ConstantParameter>>,
    instance_parameter_ids_by_name:
        HashMap<Interned<str>, pernixc_arena::ID<InstanceParameter>>,

    default_type_parameters: Vec<Type>,
    default_constant_parameters: Vec<Constant>,
}

/// Implemented by all generic parameters [`LifetimeParameter`],
/// [`TypeParameter`], [`ConstantParameter`], and [`InstanceParameter`].
pub trait GenericParameter: Sized + Send + Sync + 'static {
    /// Gets the name of the generic parameter.
    ///
    /// If the generic parameter is anonymous, (i.e. elided lifetime parameter),
    /// then this method returns `None`.
    fn name(&self) -> &Interned<str>;

    /// Gets the span where the generic parameter is declared.
    fn span(&self) -> Option<&RelativeSpan>;

    /// Gets the kind of the generic parameter.
    fn kind() -> GenericKind;

    /// Adds a new generic parameter to the list of generic parameters.
    ///
    /// # Errors
    ///
    /// If the generic parameter has a name and it is a duplicate, then it
    /// returns `Err(ID)` where `ID` is the ID of the generic parameter.
    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>>;
}

/// Represents a lifetime parameter, denoted by `'a` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    new,
)]
pub struct LifetimeParameter {
    name: Interned<str>,
    span: Option<RelativeSpan>,
}

impl GenericParameter for LifetimeParameter {
    fn name(&self) -> &Interned<str> { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Lifetime }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_lifetime_parameter(parameter)
    }
}

/// Represents a type parameter, denoted by `T` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    new,
)]
pub struct TypeParameter {
    name: Interned<str>,
    span: Option<RelativeSpan>,
}

impl GenericParameter for TypeParameter {
    fn name(&self) -> &Interned<str> { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Type }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_type_parameter(parameter)
    }
}

/// Represents a constant parameter, denoted by `const C: TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    new,
)]
pub struct ConstantParameter {
    name: Interned<str>,
    r#type: Type,
    span: Option<RelativeSpan>,
}

impl GenericParameter for ConstantParameter {
    fn name(&self) -> &Interned<str> { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Constant }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_constant_parameter(parameter)
    }
}

/// Represents the instance parameter, denoted by `instance T: Trait` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    new,
)]
pub struct InstanceParameter {
    name: Interned<str>,
    trait_ref: TraitRef,
    span: Option<RelativeSpan>,
}

impl InstanceParameter {
    /// Gets the name of the instance parameter.
    #[must_use]
    pub const fn name(&self) -> &Interned<str> { &self.name }

    /// Gets the trait reference that this instance parameter implements.
    #[must_use]
    pub const fn trait_ref(&self) -> &TraitRef { &self.trait_ref }

    /// Gets the span where the instance parameter is declared.
    #[must_use]
    pub const fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }
}

impl GenericParameter for InstanceParameter {
    fn name(&self) -> &Interned<str> { &self.name }

    fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }

    fn kind() -> GenericKind { GenericKind::Instance }

    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>> {
        generic_parameters.add_instance_parameter(parameter)
    }
}

/// Enumeration of all kinds of generic parameters.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
#[allow(missing_docs)]
pub enum GenericKind {
    Type,
    Lifetime,
    Constant,
    Instance,
}

macro_rules! implements_get_parameter {
    ($self:ident, $kind:ident) => {
        paste! {
            /// Gets the generic parameter with the given ID.
            ///
            /// # Panics
            ///
            /// Panics if the generic parameter with the given ID does not exist.
            #[must_use]
            pub fn [<get_ $kind:snake _parameter>](
                &self,
                id: pernixc_arena::ID<[< $kind Parameter >]>,
            ) -> & [< $kind Parameter >] {
                self.[< $kind:snake s>].get(id).unwrap()
            }
        }
    };
}

macro_rules! implements_add_parameter {
    ($self:ident, $kind:ident) => {
        paste! {
            /// Adds a new generic parameter to the list of generic parameters.
            ///
            /// The generic parameter will be added to the list of generic
            /// parameters and the order of the declaration.
            ///
            /// If the generic parameter has a name, then it will be added to the
            /// map that maps the name of the generic parameter to its ID.
            ///
            /// # Errors
            ///
            /// If the generic parameter has a name and it is not a duplicate, then
            /// it returns `Ok(ID)` where `ID` is the ID of the generic parameter.
            pub fn [<add_ $kind:snake _parameter>](
                &mut $self,
                parameter: [< $kind Parameter >]
            ) -> Result<pernixc_arena::ID<[< $kind Parameter >]>, pernixc_arena::ID<[< $kind Parameter >]>> {
                let entry =
                    match $self
                        .[< $kind:snake _parameter_ids_by_name >]
                        .entry(parameter.name().to_owned()) {
                        Entry::Vacant(entry) => Some(entry),
                        Entry::Occupied(entry) => {
                            return Err(*entry.get());
                        }
                    };

                let id = $self.[< $kind:snake s>].insert(parameter);
                $self.[< $kind:snake _order >].push(id);

                if let Some(entry) = entry {
                    entry.insert(id);
                }

                Ok(id)
            }
        }
    };
}

impl GenericParameters {
    /// Returns an iterator of all lifetime parameter IDs that iterates in order
    /// as they are declared.
    #[must_use]
    pub fn lifetime_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<LifetimeParameter>>
    {
        self.lifetime_order.iter().copied()
    }

    /// Returns an iterator of all type parameter IDs that iterates in order
    /// as they are declared.
    #[must_use]
    pub fn type_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<TypeParameter>> {
        self.type_order.iter().copied()
    }

    /// Returns an iterator of all constant parameter IDs that iterates in order
    /// as they are declared.
    #[must_use]
    pub fn constant_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<ConstantParameter>>
    {
        self.constant_order.iter().copied()
    }

    /// Returns an iterator of all instance parameter IDs that iterates in order
    /// as they are declared.
    #[must_use]
    pub fn instance_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<InstanceParameter>>
    {
        self.instance_order.iter().copied()
    }

    /// Returns an iterator of all type parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn type_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<TypeParameter>, &TypeParameter),
    > {
        self.type_order.iter().copied().map(|x| (x, self.types.get(x).unwrap()))
    }

    /// Returns an iterator of all lifetime parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn lifetime_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<LifetimeParameter>, &LifetimeParameter),
    > {
        self.lifetime_order
            .iter()
            .copied()
            .map(|x| (x, self.lifetimes.get(x).unwrap()))
    }

    /// Returns an iterator of all constant parameters that iterates in order as
    /// they are declared.
    #[must_use]
    pub fn constant_parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (pernixc_arena::ID<ConstantParameter>, &ConstantParameter),
    > {
        self.constant_order
            .iter()
            .copied()
            .map(|x| (x, self.constants.get(x).unwrap()))
    }

    implements_add_parameter!(self, Lifetime);

    implements_add_parameter!(self, Type);

    implements_add_parameter!(self, Constant);

    implements_add_parameter!(self, Instance);

    implements_get_parameter!(self, Lifetime);

    implements_get_parameter!(self, Type);

    implements_get_parameter!(self, Constant);

    implements_get_parameter!(self, Instance);

    /// Creates a [`GenericArguments`] that all of its parameters are the
    /// generic parameters of this [`GenericParameters`].
    #[must_use]
    pub fn create_identity_generic_arguments(
        &self,
        global_id: Global<pernixc_symbol::ID>,
    ) -> GenericArguments {
        GenericArguments::new(
            self.lifetime_order
                .iter()
                .copied()
                .map(|id| Lifetime::Parameter(MemberID::new(global_id, id)))
                .collect(),
            self.type_order
                .iter()
                .copied()
                .map(|id| Type::Parameter(MemberID::new(global_id, id)))
                .collect(),
            self.constant_order
                .iter()
                .copied()
                .map(|id| Constant::Parameter(MemberID::new(global_id, id)))
                .collect(),
            self.instance_order
                .iter()
                .copied()
                .map(|id| Instance::Parameter(MemberID::new(global_id, id)))
                .collect(),
        )
    }

    /// Checks whether there are no generic parameters defined.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
    }

    /// Gets the lifetime parameter ID at the given index in the order of
    /// declaration.
    #[must_use]
    pub fn get_lifetime_parameter_at_index(
        &self,
        index: usize,
    ) -> pernixc_arena::ID<LifetimeParameter> {
        *self.lifetime_order.get(index).unwrap()
    }

    /// Gets the type parameter ID at the given index in the order of
    /// declaration.
    #[must_use]
    pub fn get_type_parameter_at_index(
        &self,
        index: usize,
    ) -> pernixc_arena::ID<TypeParameter> {
        *self.type_order.get(index).unwrap()
    }

    /// Gets the constant parameter ID at the given index in the order of
    /// declaration.
    #[must_use]
    pub fn get_constant_parameter_at_index(
        &self,
        index: usize,
    ) -> pernixc_arena::ID<ConstantParameter> {
        *self.constant_order.get(index).unwrap()
    }

    /// Gets the instance parameter ID at the given index in the order of
    /// declaration.
    #[must_use]
    pub fn get_instance_parameter_at_index(
        &self,
        index: usize,
    ) -> pernixc_arena::ID<InstanceParameter> {
        *self.instance_order.get(index).unwrap()
    }
}

/// An ID to a type parameter.
pub type TypeParameterID = MemberID<pernixc_arena::ID<TypeParameter>>;

/// An ID to a constant parameter.
pub type ConstantParameterID = MemberID<pernixc_arena::ID<ConstantParameter>>;

/// An ID to a lifetime parameter.
pub type LifetimeParameterID = MemberID<pernixc_arena::ID<LifetimeParameter>>;

/// An ID to a instance parameter.
pub type InstanceParameterID = MemberID<pernixc_arena::ID<InstanceParameter>>;
