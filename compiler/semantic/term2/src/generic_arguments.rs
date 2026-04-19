//! Data definitions for generic arguments and symbol applications.

use derive_new::new;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    TermRef,
    constant::Constant,
    generic_parameters::{
        ConstantParameterID, GenericParameters, InstanceParameterID,
        LifetimeParameterID, TypeParameterID,
    },
    instance::Instance,
    lifetime::Lifetime,
    matching::{Matching, Substructural},
    r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    /// Streams the structural matches between two generic argument lists.
    pub(crate) fn substructural_match<'a, L, T, C, I, Y>(
        &'a self,
        other: &'a Self,
        to_location: impl Fn(usize) -> Y + 'a,
    ) -> Option<impl Iterator<Item = Substructural<L, T, C, I>> + 'a>
    where
        Y: Into<L> + Into<T> + Into<C> + Into<I> + Copy + 'a,
    {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
            || self.instances.len() != other.instances.len()
        {
            return None;
        }

        Some(pernixc_coroutine_iter::coroutine_iter!({
            for (idx, (lhs, rhs)) in self
                .lifetimes
                .iter()
                .cloned()
                .zip(other.lifetimes.iter().cloned())
                .enumerate()
            {
                let location = to_location(idx);
                yield Substructural::Lifetime(Matching::new(
                    lhs,
                    rhs,
                    location.into(),
                    location.into(),
                ));
            }

            for (idx, (lhs, rhs)) in self
                .types
                .iter()
                .cloned()
                .zip(other.types.iter().cloned())
                .enumerate()
            {
                let location = to_location(idx);
                yield Substructural::Type(Matching::new(
                    lhs,
                    rhs,
                    location.into(),
                    location.into(),
                ));
            }

            for (idx, (lhs, rhs)) in self
                .constants
                .iter()
                .cloned()
                .zip(other.constants.iter().cloned())
                .enumerate()
            {
                let location = to_location(idx);
                yield Substructural::Constant(Matching::new(
                    lhs,
                    rhs,
                    location.into(),
                    location.into(),
                ));
            }

            for (idx, (lhs, rhs)) in self
                .instances
                .iter()
                .cloned()
                .zip(other.instances.iter().cloned())
                .enumerate()
            {
                let location = to_location(idx);
                yield Substructural::Instance(Matching::new(
                    lhs,
                    rhs,
                    location.into(),
                    location.into(),
                ));
            }
        }))
    }

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

    /// Returns a particular argument by kind and index.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubGenericArgumentsLocation,
    ) -> Option<&Interned<T>> {
        T::get(self).get(location.0)
    }

    /// Iterates over all immediate sub-terms in canonical generic argument
    /// order: lifetime, type, constant, then instance.
    pub fn iter_sub_term(&self) -> impl Iterator<Item = TermRef<'_>> + '_ {
        self.iter_sub_terms_with_location(
            |_: SubGenericArgumentsLocation| (),
            |_: SubGenericArgumentsLocation| (),
            |_: SubGenericArgumentsLocation| (),
            |_: SubGenericArgumentsLocation| (),
        )
        .map(|(term_ref, ())| term_ref)
    }

    /// Iterates over all immediate sub-terms in canonical generic argument
    /// order: lifetime, type, constant, then instance.
    pub(crate) fn iter_sub_terms_with_location<
        'this,
        TermLocation,
        MLifetime,
        MType,
        MConstant,
        MInstance,
    >(
        &'this self,
        map_lifetime_location: MLifetime,
        map_type_location: MType,
        map_constant_location: MConstant,
        map_instance_location: MInstance,
    ) -> impl Iterator<Item = (TermRef<'this>, TermLocation)> + 'this
    where
        MLifetime: Fn(SubGenericArgumentsLocation) -> TermLocation + 'this,
        MType: Fn(SubGenericArgumentsLocation) -> TermLocation + 'this,
        MConstant: Fn(SubGenericArgumentsLocation) -> TermLocation + 'this,
        MInstance: Fn(SubGenericArgumentsLocation) -> TermLocation + 'this,
    {
        let lifetimes =
            self.lifetimes.iter().enumerate().map(move |(index, lifetime)| {
                (
                    TermRef::Lifetime(lifetime),
                    map_lifetime_location(SubGenericArgumentsLocation::new(
                        index,
                    )),
                )
            });

        let types =
            self.types.iter().enumerate().map(move |(index, r#type)| {
                (
                    TermRef::Type(r#type),
                    map_type_location(SubGenericArgumentsLocation::new(index)),
                )
            });

        let constants =
            self.constants.iter().enumerate().map(move |(index, constant)| {
                (
                    TermRef::Constant(constant),
                    map_constant_location(SubGenericArgumentsLocation::new(
                        index,
                    )),
                )
            });

        let instances =
            self.instances.iter().enumerate().map(move |(index, instance)| {
                (
                    TermRef::Instance(instance),
                    map_instance_location(SubGenericArgumentsLocation::new(
                        index,
                    )),
                )
            });

        lifetimes.chain(types).chain(constants).chain(instances)
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

/// Represents the location of a term inside a generic argument list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubGenericArgumentsLocation(usize);

impl SubGenericArgumentsLocation {
    /// Returns the index inside the generic argument list.
    #[must_use]
    pub const fn index(&self) -> usize { self.0 }
}

/// Represents the location of a term inside a symbol application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubSymbolLocation(usize);

impl SubSymbolLocation {
    /// Returns the index inside the symbol generic arguments.
    #[must_use]
    pub const fn index(&self) -> usize { self.0 }
}

/// Represents the location of a term inside an associated symbol application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubAssociatedSymbolLocation {
    index: usize,
    from_parent: bool,
}

impl SubAssociatedSymbolLocation {
    /// Returns the index inside the selected generic argument list.
    #[must_use]
    pub const fn index(&self) -> usize { self.index }

    /// Returns whether the location points into the parent generic arguments.
    #[must_use]
    pub const fn is_from_parent(&self) -> bool { self.from_parent }
}

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

    /// Returns a term from the symbol generic arguments.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubSymbolLocation,
    ) -> Option<&Interned<T>> {
        self.generic_arguments
            .get_term(SubGenericArgumentsLocation::new(location.index()))
    }

    /// Iterates over all generic argument sub-terms.
    pub fn iter_sub_terms<
        'this,
        TermLocation,
        MLifetime,
        MType,
        MConstant,
        MInstance,
    >(
        &'this self,
        map_lifetime_location: MLifetime,
        map_type_location: MType,
        map_constant_location: MConstant,
        map_instance_location: MInstance,
    ) -> impl Iterator<Item = (TermRef<'this>, TermLocation)> + 'this
    where
        MLifetime: Fn(SubSymbolLocation) -> TermLocation + 'this,
        MType: Fn(SubSymbolLocation) -> TermLocation + 'this,
        MConstant: Fn(SubSymbolLocation) -> TermLocation + 'this,
        MInstance: Fn(SubSymbolLocation) -> TermLocation + 'this,
    {
        self.generic_arguments.iter_sub_terms_with_location(
            move |location| {
                map_lifetime_location(SubSymbolLocation::new(location.index()))
            },
            move |location| {
                map_type_location(SubSymbolLocation::new(location.index()))
            },
            move |location| {
                map_constant_location(SubSymbolLocation::new(location.index()))
            },
            move |location| {
                map_instance_location(SubSymbolLocation::new(location.index()))
            },
        )
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

    /// Returns a term from either the parent or member generic arguments.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubAssociatedSymbolLocation,
    ) -> Option<&Interned<T>> {
        let generic_arguments = if location.is_from_parent() {
            self.parent_generic_arguments()
        } else {
            self.member_generic_arguments()
        };

        generic_arguments
            .get_term(SubGenericArgumentsLocation::new(location.index()))
    }

    /// Iterates over all generic argument sub-terms from parent first,
    /// followed by member generic arguments.
    pub fn iter_sub_terms<
        'this,
        TermLocation,
        MLifetime,
        MType,
        MConstant,
        MInstance,
    >(
        &'this self,
        map_lifetime_location: MLifetime,
        map_type_location: MType,
        map_constant_location: MConstant,
        map_instance_location: MInstance,
    ) -> impl Iterator<Item = (TermRef<'this>, TermLocation)> + 'this
    where
        MLifetime:
            Fn(SubAssociatedSymbolLocation) -> TermLocation + Clone + 'this,
        MType: Fn(SubAssociatedSymbolLocation) -> TermLocation + Clone + 'this,
        MConstant:
            Fn(SubAssociatedSymbolLocation) -> TermLocation + Clone + 'this,
        MInstance:
            Fn(SubAssociatedSymbolLocation) -> TermLocation + Clone + 'this,
    {
        self.parent_generic_arguments()
            .as_ref()
            .iter_sub_terms_with_location(
                {
                    let map_lifetime_location = map_lifetime_location.clone();

                    move |location| {
                        map_lifetime_location(SubAssociatedSymbolLocation::new(
                            location.index(),
                            true,
                        ))
                    }
                },
                {
                    let map_type_location = map_type_location.clone();

                    move |location| {
                        map_type_location(SubAssociatedSymbolLocation::new(
                            location.index(),
                            true,
                        ))
                    }
                },
                {
                    let map_constant_location = map_constant_location.clone();

                    move |location| {
                        map_constant_location(SubAssociatedSymbolLocation::new(
                            location.index(),
                            true,
                        ))
                    }
                },
                {
                    let map_instance_location = map_instance_location.clone();

                    move |location| {
                        map_instance_location(SubAssociatedSymbolLocation::new(
                            location.index(),
                            true,
                        ))
                    }
                },
            )
            .chain(
                self.member_generic_arguments()
                    .as_ref()
                    .iter_sub_terms_with_location(
                        {
                            let map_lifetime_location = map_lifetime_location;

                            move |location| {
                                map_lifetime_location(
                                    SubAssociatedSymbolLocation::new(
                                        location.index(),
                                        false,
                                    ),
                                )
                            }
                        },
                        {
                            let map_type_location = map_type_location;

                            move |location| {
                                map_type_location(
                                    SubAssociatedSymbolLocation::new(
                                        location.index(),
                                        false,
                                    ),
                                )
                            }
                        },
                        {
                            let map_constant_location = map_constant_location;

                            move |location| {
                                map_constant_location(
                                    SubAssociatedSymbolLocation::new(
                                        location.index(),
                                        false,
                                    ),
                                )
                            }
                        },
                        {
                            let map_instance_location = map_instance_location;

                            move |location| {
                                map_instance_location(
                                    SubAssociatedSymbolLocation::new(
                                        location.index(),
                                        false,
                                    ),
                                )
                            }
                        },
                    ),
            )
    }
}

#[cfg(test)]
mod tests {
    use pernixc_symbol::SymbolID;
    use pernixc_target::TargetID;

    use super::*;
    use crate::{
        generic_parameters::{
            GenericParameters, LifetimeParameter, TypeParameter,
        },
        lifetime::Lifetime,
        test_support::create_test_engine,
        r#type::{Primitive, Type},
    };

    #[tokio::test]
    async fn identity_generic_arguments_are_interned() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let mut generic_parameters = GenericParameters::default();
        let lifetime_id = generic_parameters
            .add_lifetime_parameter(LifetimeParameter::new(
                tracked.intern_unsized("a".to_owned()),
                None,
            ))
            .unwrap();
        let type_id = generic_parameters
            .add_type_parameter(TypeParameter::new(
                tracked.intern_unsized("T".to_owned()),
                None,
            ))
            .unwrap();

        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(7));
        let generic_arguments = generic_parameters
            .create_identity_generic_arguments(symbol_id, &tracked);

        assert_eq!(
            generic_arguments.lifetimes()[0].as_ref(),
            &Lifetime::Parameter(LifetimeParameterID::new(
                symbol_id,
                lifetime_id
            )),
        );
        assert_eq!(
            generic_arguments.types()[0].as_ref(),
            &Type::Parameter(TypeParameterID::new(symbol_id, type_id)),
        );

        let symbol = Symbol::new(symbol_id, generic_arguments.clone());
        assert_eq!(symbol.generic_arguments(), &generic_arguments);
    }

    #[tokio::test]
    async fn single_type_generic_arguments_keep_interned_type() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
        let generic_arguments =
            GenericArguments::new_single_type(r#type.clone());

        assert_eq!(generic_arguments.types(), &[r#type]);
        assert!(generic_arguments.lifetimes().is_empty());
        assert!(generic_arguments.constants().is_empty());
        assert!(generic_arguments.instances().is_empty());
    }
}
