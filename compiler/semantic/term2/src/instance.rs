//! Data definitions for instance terms.

use enum_as_inner::EnumAsInner;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{InstanceParameter, InstanceParameterID},
    inference,
    lifetime::Lifetime,
    r#type::Type,
};

/// Refers to a trait with generic arguments applied.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct TraitRef(Symbol);

impl TraitRef {
    /// Creates a new trait reference.
    #[must_use]
    pub const fn new(
        id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: Interned<GenericArguments>,
    ) -> Self {
        Self(Symbol::new(id, generic_arguments))
    }

    /// Creates a trait reference from a symbol application.
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Returns the trait id.
    #[must_use]
    pub const fn trait_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.0.id()
    }

    /// Returns the applied generic arguments.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<GenericArguments> {
        self.0.generic_arguments()
    }

    /// Returns the applied generic arguments by value.
    #[must_use]
    pub fn into_generic_arguments(self) -> Interned<GenericArguments> {
        self.0.into_generic_arguments()
    }
}

/// Refers to an instance-associated term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct InstanceAssociated {
    instance: Interned<Instance>,
    trait_associated_symbol_id: Global<pernixc_symbol::SymbolID>,
    trait_associated_symbol_generic_arguments: Interned<GenericArguments>,
}

impl InstanceAssociated {
    /// Creates a new instance-associated term.
    #[must_use]
    pub const fn new(
        instance: Interned<Instance>,
        trait_associated_symbol_id: Global<pernixc_symbol::SymbolID>,
        trait_associated_symbol_generic_arguments: Interned<GenericArguments>,
    ) -> Self {
        Self {
            instance,
            trait_associated_symbol_id,
            trait_associated_symbol_generic_arguments,
        }
    }

    /// Returns the parent instance.
    #[must_use]
    pub const fn instance(&self) -> &Interned<Instance> { &self.instance }

    /// Returns the associated symbol id.
    #[must_use]
    pub const fn trait_associated_symbol_id(
        &self,
    ) -> Global<pernixc_symbol::SymbolID> {
        self.trait_associated_symbol_id
    }

    /// Returns the associated symbol generic arguments.
    #[must_use]
    pub const fn associated_instance_generic_arguments(
        &self,
    ) -> &Interned<GenericArguments> {
        &self.trait_associated_symbol_generic_arguments
    }

    /// Returns a lifetime argument from the associated symbol application.
    #[must_use]
    pub fn get_lifetime(&self, index: usize) -> Option<&Interned<Lifetime>> {
        self.trait_associated_symbol_generic_arguments.lifetimes().get(index)
    }

    /// Returns a type argument from the associated symbol application.
    #[must_use]
    pub fn get_type(&self, index: usize) -> Option<&Interned<Type>> {
        self.trait_associated_symbol_generic_arguments.types().get(index)
    }

    /// Returns a constant argument from the associated symbol application.
    #[must_use]
    pub fn get_constant(&self, index: usize) -> Option<&Interned<Constant>> {
        self.trait_associated_symbol_generic_arguments.constants().get(index)
    }

    /// Returns an instance argument from the associated symbol application.
    #[must_use]
    pub fn get_instance(&self, index: usize) -> Option<&Interned<Instance>> {
        self.trait_associated_symbol_generic_arguments.instances().get(index)
    }
}

/// Represents `this` under a trait body.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct AnoymousTrait {
    trait_id: Global<pernixc_symbol::SymbolID>,
}

impl AnoymousTrait {
    /// Creates a new anonymous trait instance payload.
    #[must_use]
    pub const fn new(trait_id: Global<pernixc_symbol::SymbolID>) -> Self {
        Self { trait_id }
    }

    /// Returns the trait id.
    #[must_use]
    pub const fn trait_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.trait_id
    }
}

/// Represents an instance term payload.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    EnumAsInner,
    derive_more::From,
    Identifiable,
)]
pub enum Instance {
    Symbol(Symbol),
    Parameter(InstanceParameterID),
    InstanceAssociated(InstanceAssociated),
    Inference(inference::Variable<Self>),
    AnonymousTrait(AnoymousTrait),
    Error(Error),
}

impl Default for Instance {
    fn default() -> Self { Self::Error(Error) }
}

impl Instance {
    /// Creates an instance parameter reference.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        instance_id: pernixc_arena::ID<InstanceParameter>,
    ) -> Self {
        Self::Parameter(InstanceParameterID::new(parent_global_id, instance_id))
    }

    /// Creates an error instance payload.
    #[must_use]
    pub const fn new_error() -> Self { Self::Error(Error) }

    /// Creates an anonymous trait instance payload.
    #[must_use]
    pub const fn new_anonymous_trait(
        trait_id: Global<pernixc_symbol::SymbolID>,
    ) -> Self {
        Self::AnonymousTrait(AnoymousTrait::new(trait_id))
    }
}
