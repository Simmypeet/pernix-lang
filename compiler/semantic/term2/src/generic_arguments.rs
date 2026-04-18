//! Data definitions for generic arguments and symbol applications.

use derive_new::new;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    constant::Constant,
    generic_parameters::{
        ConstantParameterID, GenericParameters, InstanceParameterID,
        LifetimeParameterID, TypeParameterID,
    },
    instance::Instance,
    lifetime::Lifetime,
    r#type::Type,
};

/// Represents the generic arguments supplied to a symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Encode,
    Decode,
    StableHash,
    Identifiable,
    new,
)]
pub struct GenericArguments {
    lifetimes: Vec<Interned<Lifetime>>,
    types: Vec<Interned<Type>>,
    constants: Vec<Interned<Constant>>,
    instances: Vec<Interned<Instance>>,
}

impl GenericArguments {
    /// Creates generic arguments with a single type argument.
    #[must_use]
    pub fn new_single_type(r#type: Interned<Type>) -> Self {
        Self {
            lifetimes: Vec::new(),
            types: vec![r#type],
            constants: Vec::new(),
            instances: Vec::new(),
        }
    }

    /// Returns the lifetime arguments.
    #[must_use]
    pub fn lifetimes(&self) -> &[Interned<Lifetime>] { &self.lifetimes }

    /// Returns the type arguments.
    #[must_use]
    pub fn types(&self) -> &[Interned<Type>] { &self.types }

    /// Returns the constant arguments.
    #[must_use]
    pub fn constants(&self) -> &[Interned<Constant>] { &self.constants }

    /// Returns the instance arguments.
    #[must_use]
    pub fn instances(&self) -> &[Interned<Instance>] { &self.instances }

    /// Returns the lifetime arguments mutably.
    #[must_use]
    pub fn lifetimes_mut(&mut self) -> &mut [Interned<Lifetime>] {
        &mut self.lifetimes
    }

    /// Returns the type arguments mutably.
    #[must_use]
    pub fn types_mut(&mut self) -> &mut [Interned<Type>] { &mut self.types }

    /// Returns the constant arguments mutably.
    #[must_use]
    pub fn constants_mut(&mut self) -> &mut [Interned<Constant>] {
        &mut self.constants
    }

    /// Returns the instance arguments mutably.
    #[must_use]
    pub fn instances_mut(&mut self) -> &mut [Interned<Instance>] {
        &mut self.instances
    }

    /// Checks whether all argument lists are empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
            && self.instances.is_empty()
    }

    /// Checks whether the argument arity matches another argument list.
    #[must_use]
    pub const fn arity_matches(&self, other: &Self) -> bool {
        self.lifetimes.len() == other.lifetimes.len()
            && self.types.len() == other.types.len()
            && self.constants.len() == other.constants.len()
            && self.instances.len() == other.instances.len()
    }

    /// Checks whether the arity matches the given generic parameters.
    #[must_use]
    pub const fn param_arity_matches(
        &self,
        generic_parameters: &GenericParameters,
    ) -> bool {
        self.lifetimes.len() == generic_parameters.lifetime_parameters_len()
            && self.types.len() == generic_parameters.type_parameters_len()
            && self.constants.len()
                == generic_parameters.constant_parameters_len()
            && self.instances.len()
                == generic_parameters.instance_parameters_len()
    }

    /// Pushes a lifetime argument.
    pub fn push_lifetime(&mut self, lifetime: Interned<Lifetime>) {
        self.lifetimes.push(lifetime);
    }

    /// Pushes a type argument.
    pub fn push_type(&mut self, r#type: Interned<Type>) {
        self.types.push(r#type);
    }

    /// Pushes a constant argument.
    pub fn push_constant(&mut self, constant: Interned<Constant>) {
        self.constants.push(constant);
    }

    /// Pushes an instance argument.
    pub fn push_instance(&mut self, instance: Interned<Instance>) {
        self.instances.push(instance);
    }

    /// Pushes an argument of a particular kind.
    pub fn push<T: Element>(&mut self, element: Interned<T>) {
        T::get_mut(self).push(element);
    }

    /// Returns the number of arguments of a particular kind.
    #[must_use]
    pub fn len_of<T: Element>(&self) -> usize { T::get(self).len() }

    /// Resizes an argument list of a particular kind.
    pub fn resize<T: Element>(&mut self, new_len: usize, default: Interned<T>) {
        T::get_mut(self).resize(new_len, default);
    }

    /// Destructures the generic arguments.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn into_arguments(
        self,
    ) -> (
        Vec<Interned<Lifetime>>,
        Vec<Interned<Type>>,
        Vec<Interned<Constant>>,
        Vec<Interned<Instance>>,
    ) {
        (self.lifetimes, self.types, self.constants, self.instances)
    }

    /// Checks whether the generic arguments are an identity mapping for a
    /// symbol.
    #[must_use]
    pub fn is_identity(
        &self,
        generic_parameters: &GenericParameters,
        symbol_id: Global<pernixc_symbol::SymbolID>,
    ) -> bool {
        if !self.param_arity_matches(generic_parameters) {
            return false;
        }

        for (lifetime, lifetime_param) in self
            .lifetimes
            .iter()
            .zip(generic_parameters.lifetime_parameter_order())
        {
            if lifetime.as_ref()
                != &Lifetime::Parameter(LifetimeParameterID::new(
                    symbol_id,
                    lifetime_param,
                ))
            {
                return false;
            }
        }

        for (r#type, type_param) in
            self.types.iter().zip(generic_parameters.type_parameter_order())
        {
            if r#type.as_ref()
                != &Type::Parameter(TypeParameterID::new(symbol_id, type_param))
            {
                return false;
            }
        }

        for (constant, constant_param) in self
            .constants
            .iter()
            .zip(generic_parameters.constant_parameter_order())
        {
            if constant.as_ref()
                != &Constant::Parameter(ConstantParameterID::new(
                    symbol_id,
                    constant_param,
                ))
            {
                return false;
            }
        }

        for (instance, instance_param) in self
            .instances
            .iter()
            .zip(generic_parameters.instance_parameter_order())
        {
            if instance.as_ref()
                != &Instance::Parameter(InstanceParameterID::new(
                    symbol_id,
                    instance_param,
                ))
            {
                return false;
            }
        }

        true
    }
}

mod sealed {
    use qbice::storage::intern::Interned;

    use crate::{
        constant::Constant, generic_arguments::GenericArguments,
        instance::Instance, lifetime::Lifetime, r#type::Type,
    };

    pub trait Sealed {
        fn get(generic_arguments: &GenericArguments) -> &[Interned<Self>]
        where
            Self: Sized;

        fn get_mut(
            generic_arguments: &mut GenericArguments,
        ) -> &mut Vec<Interned<Self>>
        where
            Self: Sized;
    }

    impl Sealed for Lifetime {
        fn get(generic_arguments: &GenericArguments) -> &[Interned<Self>] {
            &generic_arguments.lifetimes
        }

        fn get_mut(
            generic_arguments: &mut GenericArguments,
        ) -> &mut Vec<Interned<Self>> {
            &mut generic_arguments.lifetimes
        }
    }

    impl Sealed for Type {
        fn get(generic_arguments: &GenericArguments) -> &[Interned<Self>] {
            &generic_arguments.types
        }

        fn get_mut(
            generic_arguments: &mut GenericArguments,
        ) -> &mut Vec<Interned<Self>> {
            &mut generic_arguments.types
        }
    }

    impl Sealed for Constant {
        fn get(generic_arguments: &GenericArguments) -> &[Interned<Self>] {
            &generic_arguments.constants
        }

        fn get_mut(
            generic_arguments: &mut GenericArguments,
        ) -> &mut Vec<Interned<Self>> {
            &mut generic_arguments.constants
        }
    }

    impl Sealed for Instance {
        fn get(generic_arguments: &GenericArguments) -> &[Interned<Self>] {
            &generic_arguments.instances
        }

        fn get_mut(
            generic_arguments: &mut GenericArguments,
        ) -> &mut Vec<Interned<Self>> {
            &mut generic_arguments.instances
        }
    }
}

/// A term kind that can be extracted from a [`GenericArguments`].
pub trait Element: sealed::Sealed {}

impl Element for Lifetime {}
impl Element for Type {}
impl Element for Constant {}
impl Element for Instance {}

/// Represents a symbol application.
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
pub struct Symbol {
    id: Global<pernixc_symbol::SymbolID>,
    generic_arguments: Interned<GenericArguments>,
}

impl Symbol {
    /// Creates a new symbol application.
    #[must_use]
    pub const fn new(
        id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: Interned<GenericArguments>,
    ) -> Self {
        Self { id, generic_arguments }
    }

    /// Returns the symbol id.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::SymbolID> { self.id }

    /// Returns the symbol generic arguments.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<GenericArguments> {
        &self.generic_arguments
    }

    /// Destructures the symbol application.
    #[must_use]
    pub fn destructure(
        self,
    ) -> (Global<pernixc_symbol::SymbolID>, Interned<GenericArguments>) {
        (self.id, self.generic_arguments)
    }

    /// Returns the generic arguments by value.
    #[must_use]
    pub fn into_generic_arguments(self) -> Interned<GenericArguments> {
        self.generic_arguments
    }
}

/// Represents an associated symbol application.
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
    new,
)]
pub struct AssociatedSymbol {
    id: Global<pernixc_symbol::SymbolID>,
    parent_generic_arguments: Interned<GenericArguments>,
    member_generic_arguments: Interned<GenericArguments>,
}

impl AssociatedSymbol {
    /// Returns the associated symbol id.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::SymbolID> { self.id }

    /// Returns the parent generic arguments.
    #[must_use]
    pub const fn parent_generic_arguments(
        &self,
    ) -> &Interned<GenericArguments> {
        &self.parent_generic_arguments
    }

    /// Returns the member generic arguments.
    #[must_use]
    pub const fn member_generic_arguments(
        &self,
    ) -> &Interned<GenericArguments> {
        &self.member_generic_arguments
    }

    /// Destructures the associated symbol.
    #[must_use]
    pub fn into_id_and_member_generic_arguments(
        self,
    ) -> (Global<pernixc_symbol::SymbolID>, Interned<GenericArguments>) {
        (self.id, self.member_generic_arguments)
    }
}
