//! Data definitions for generic parameters.

use std::{collections::hash_map::Entry, ops::Index};

use derive_new::new;
use getset::Getters;
use paste::paste;
use pernixc_arena::Arena;
#[allow(unused_imports)]
use pernixc_extend::extend;
use pernixc_hash::FxHashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::Interner;
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

/// Query key for retrieving generic parameters of a symbol.
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
    symbol_id: Global<pernixc_symbol::SymbolID>,
}

impl Key {
    /// Creates a new query key.
    #[must_use]
    pub const fn new(symbol_id: Global<pernixc_symbol::SymbolID>) -> Self {
        Self { symbol_id }
    }

    /// Returns the symbol id.
    #[must_use]
    pub const fn symbol_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.symbol_id
    }
}

/// Represents the generic parameters declared by a symbol.
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
        FxHashMap<Interned<str>, pernixc_arena::ID<LifetimeParameter>>,
    type_parameter_ids_by_name:
        FxHashMap<Interned<str>, pernixc_arena::ID<TypeParameter>>,
    constant_parameter_ids_by_name:
        FxHashMap<Interned<str>, pernixc_arena::ID<ConstantParameter>>,
    instance_parameter_ids_by_name:
        FxHashMap<Interned<str>, pernixc_arena::ID<InstanceParameter>>,

    default_type_parameters: Vec<Interned<Type>>,
    default_constant_parameters: Vec<Interned<Constant>>,
}

mod sealed {
    use pernixc_arena::Arena;
    use pernixc_hash::FxHashMap;
    use qbice::storage::intern::Interned;

    pub trait Sealed {
        fn get_generic_parameters_arena(
            generic_parameters: &super::GenericParameters,
        ) -> &Arena<Self>
        where
            Self: Sized;

        fn get_parameter_ids_by_name(
            generic_parameters: &super::GenericParameters,
        ) -> &FxHashMap<Interned<str>, pernixc_arena::ID<Self>>
        where
            Self: Sized;

        fn get_parameter_order(
            generic_parameters: &super::GenericParameters,
        ) -> &Vec<pernixc_arena::ID<Self>>
        where
            Self: Sized;
    }
}

/// Implemented by all generic parameter kinds.
pub trait GenericParameter:
    sealed::Sealed + Sized + Send + Sync + 'static
{
    /// Gets the parameter name.
    fn name(&self) -> &Interned<str>;

    /// Gets the declaration span.
    fn span(&self) -> Option<&RelativeSpan>;

    /// Gets the parameter kind.
    fn kind() -> GenericKind;

    /// Adds a parameter to a [`GenericParameters`] list.
    fn add_generic_parameter(
        generic_parameters: &mut GenericParameters,
        parameter: Self,
    ) -> Result<pernixc_arena::ID<Self>, pernixc_arena::ID<Self>>;
}

/// Represents a lifetime parameter.
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

impl sealed::Sealed for LifetimeParameter {
    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.lifetimes
    }

    fn get_parameter_ids_by_name(
        generic_parameters: &GenericParameters,
    ) -> &FxHashMap<Interned<str>, pernixc_arena::ID<Self>> {
        &generic_parameters.lifetime_parameter_ids_by_name
    }

    fn get_parameter_order(
        generic_parameters: &GenericParameters,
    ) -> &Vec<pernixc_arena::ID<Self>> {
        &generic_parameters.lifetime_order
    }
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

/// Represents a type parameter.
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

impl sealed::Sealed for TypeParameter {
    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.types
    }

    fn get_parameter_ids_by_name(
        generic_parameters: &GenericParameters,
    ) -> &FxHashMap<Interned<str>, pernixc_arena::ID<Self>> {
        &generic_parameters.type_parameter_ids_by_name
    }

    fn get_parameter_order(
        generic_parameters: &GenericParameters,
    ) -> &Vec<pernixc_arena::ID<Self>> {
        &generic_parameters.type_order
    }
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

/// Represents a constant parameter.
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
    Getters,
)]
pub struct ConstantParameter {
    name: Interned<str>,
    #[getset(get = "pub")]
    r#type: Interned<Type>,
    span: Option<RelativeSpan>,
}

impl sealed::Sealed for ConstantParameter {
    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.constants
    }

    fn get_parameter_ids_by_name(
        generic_parameters: &GenericParameters,
    ) -> &FxHashMap<Interned<str>, pernixc_arena::ID<Self>> {
        &generic_parameters.constant_parameter_ids_by_name
    }

    fn get_parameter_order(
        generic_parameters: &GenericParameters,
    ) -> &Vec<pernixc_arena::ID<Self>> {
        &generic_parameters.constant_order
    }
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

/// Represents an instance parameter.
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
    trait_ref: Option<Interned<TraitRef>>,
    span: Option<RelativeSpan>,
}

impl InstanceParameter {
    /// Returns the parameter name.
    #[must_use]
    pub const fn name(&self) -> &Interned<str> { &self.name }

    /// Returns the trait reference constraint.
    #[must_use]
    pub const fn trait_ref(&self) -> Option<&Interned<TraitRef>> {
        self.trait_ref.as_ref()
    }

    /// Returns the declaration span.
    #[must_use]
    pub const fn span(&self) -> Option<&RelativeSpan> { self.span.as_ref() }
}

impl sealed::Sealed for InstanceParameter {
    fn get_generic_parameters_arena(
        generic_parameters: &GenericParameters,
    ) -> &Arena<Self> {
        &generic_parameters.instances
    }

    fn get_parameter_ids_by_name(
        generic_parameters: &GenericParameters,
    ) -> &FxHashMap<Interned<str>, pernixc_arena::ID<Self>> {
        &generic_parameters.instance_parameter_ids_by_name
    }

    fn get_parameter_order(
        generic_parameters: &GenericParameters,
    ) -> &Vec<pernixc_arena::ID<Self>> {
        &generic_parameters.instance_order
    }
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

/// Enumeration of generic parameter kinds.
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
            #[must_use]
            pub fn [<get_ $kind:snake _parameter>](
                &$self,
                id: pernixc_arena::ID<[< $kind Parameter >]>,
            ) -> &[< $kind Parameter >] {
                $self.[< $kind:snake s>].get(id).unwrap()
            }
        }
    };
}

macro_rules! implements_add_parameter {
    ($self:ident, $kind:ident) => {
        paste! {
            pub fn [<add_ $kind:snake _parameter>](
                &mut $self,
                parameter: [< $kind Parameter >],
            ) -> Result<
                pernixc_arena::ID<[< $kind Parameter >]>,
                pernixc_arena::ID<[< $kind Parameter >]>,
            > {
                let entry = match $self
                    .[< $kind:snake _parameter_ids_by_name >]
                    .entry(parameter.name().clone())
                {
                    Entry::Vacant(entry) => Some(entry),
                    Entry::Occupied(entry) => return Err(*entry.get()),
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
    /// Returns the declared lifetime parameter order.
    #[must_use]
    pub fn lifetime_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<LifetimeParameter>>
    {
        self.lifetime_order.iter().copied()
    }

    /// Returns the declared type parameter order.
    #[must_use]
    pub fn type_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<TypeParameter>> {
        self.type_order.iter().copied()
    }

    /// Returns the declared constant parameter order.
    #[must_use]
    pub fn constant_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<ConstantParameter>>
    {
        self.constant_order.iter().copied()
    }

    /// Returns the declared instance parameter order.
    #[must_use]
    pub fn instance_parameter_order(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<InstanceParameter>>
    {
        self.instance_order.iter().copied()
    }

    /// Returns the number of lifetime parameters.
    #[must_use]
    pub const fn lifetime_parameters_len(&self) -> usize {
        self.lifetime_order.len()
    }

    /// Returns the number of type parameters.
    #[must_use]
    pub const fn type_parameters_len(&self) -> usize { self.type_order.len() }

    /// Returns the number of constant parameters.
    #[must_use]
    pub const fn constant_parameters_len(&self) -> usize {
        self.constant_order.len()
    }

    /// Returns the number of instance parameters.
    #[must_use]
    pub const fn instance_parameters_len(&self) -> usize {
        self.instance_order.len()
    }

    /// Looks up a parameter id by name.
    #[must_use]
    pub fn get_parameter_id_by_name<T: GenericParameter>(
        &self,
        name: &str,
    ) -> Option<pernixc_arena::ID<T>> {
        T::get_parameter_ids_by_name(self).get(name).copied()
    }

    /// Returns the parameter ids of a particular kind.
    #[must_use]
    pub fn parameter_ids<T: GenericParameter>(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<T>> {
        T::get_generic_parameters_arena(self).ids()
    }

    /// Returns the declared parameter order of a particular kind.
    #[must_use]
    pub fn parameter_order<T: GenericParameter>(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<T>> {
        T::get_parameter_order(self).iter().copied()
    }

    /// Returns the default type parameters.
    #[must_use]
    pub fn default_type_parameters(&self) -> &[Interned<Type>] {
        &self.default_type_parameters
    }

    /// Returns the default constant parameters.
    #[must_use]
    pub fn default_constant_parameters(&self) -> &[Interned<Constant>] {
        &self.default_constant_parameters
    }

    /// Returns whether there are no generic parameters defined.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
            && self.instances.is_empty()
    }

    implements_add_parameter!(self, Lifetime);
    implements_add_parameter!(self, Type);
    implements_add_parameter!(self, Constant);
    implements_add_parameter!(self, Instance);

    implements_get_parameter!(self, Lifetime);
    implements_get_parameter!(self, Type);
    implements_get_parameter!(self, Constant);
    implements_get_parameter!(self, Instance);

    /// Creates the identity generic arguments for a symbol using an interner.
    #[must_use]
    pub fn create_identity_generic_arguments(
        &self,
        global_id: Global<pernixc_symbol::SymbolID>,
        interner: &impl Interner,
    ) -> Interned<GenericArguments> {
        interner.intern(GenericArguments::new(
            self.lifetime_order
                .iter()
                .copied()
                .map(|id| {
                    interner.intern(Lifetime::Parameter(MemberID::new(
                        global_id, id,
                    )))
                })
                .collect(),
            self.type_order
                .iter()
                .copied()
                .map(|id| {
                    interner
                        .intern(Type::Parameter(MemberID::new(global_id, id)))
                })
                .collect(),
            self.constant_order
                .iter()
                .copied()
                .map(|id| {
                    interner.intern(Constant::Parameter(MemberID::new(
                        global_id, id,
                    )))
                })
                .collect(),
            self.instance_order
                .iter()
                .copied()
                .map(|id| {
                    interner.intern(Instance::Parameter(MemberID::new(
                        global_id, id,
                    )))
                })
                .collect(),
        ))
    }
}

/// An id to a type parameter.
pub type TypeParameterID = MemberID<pernixc_arena::ID<TypeParameter>>;

/// An id to a constant parameter.
pub type ConstantParameterID = MemberID<pernixc_arena::ID<ConstantParameter>>;

/// An id to a lifetime parameter.
pub type LifetimeParameterID = MemberID<pernixc_arena::ID<LifetimeParameter>>;

/// An id to an instance parameter.
pub type InstanceParameterID = MemberID<pernixc_arena::ID<InstanceParameter>>;

impl<T: GenericParameter> Index<pernixc_arena::ID<T>> for GenericParameters {
    type Output = T;

    fn index(&self, index: pernixc_arena::ID<T>) -> &Self::Output {
        T::get_generic_parameters_arena(self).get(index).unwrap()
    }
}
