//! Defines the [`Instance`] term

use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::get_name;
use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::{
    constant::Constant,
    generic_arguments::{
        GenericArguments, SubGenericArgumentsLocation, SubSymbolLocation,
        Symbol,
    },
    generic_parameters::{InstanceParameterID, get_generic_parameters},
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, Location, SubTerm, TermLocation},
    r#type::Type,
    visitor::Element,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// The `TraitRef` refers to a trait that an instance implements.
///
/// This is analogous to "expression" has a specific "type". In this case, the
/// "instance" implements a specific "trait".
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
pub struct TraitRef(Symbol);

/// Refers to a terrm that is associated with an instance.
///
/// This could be any term that is associated with an instance, such as "type"
/// or even "instance" associated with an instance.
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
    instance: Box<Instance>,
    trait_associated_symbol_id: Global<pernixc_symbol::ID>,
    trait_associated_symbol_generic_arguments: GenericArguments,
}

impl InstanceAssociated {
    /// Creates a new `InstanceAssociated`.
    #[must_use]
    pub const fn new(
        instance: Box<Instance>,
        trait_associated_symbol_id: Global<pernixc_symbol::ID>,
        trait_associated_symbol_generic_arguments: GenericArguments,
    ) -> Self {
        Self {
            instance,
            trait_associated_symbol_id,
            trait_associated_symbol_generic_arguments,
        }
    }

    /// Returns a reference to the inner instance.
    #[must_use]
    pub fn instance(&self) -> &Instance { &self.instance }

    /// Returns a mutable reference to the inner instance.
    #[must_use]
    pub fn instance_mut(&mut self) -> &mut Instance { &mut self.instance }

    /// Returns the trait associated symbol ID.
    #[must_use]
    pub const fn trait_associated_symbol_id(
        &self,
    ) -> Global<pernixc_symbol::ID> {
        self.trait_associated_symbol_id
    }

    /// Returns a reference to the generic arguments of the trait associated
    /// symbol.
    #[must_use]
    pub const fn trait_associated_symbol_generic_arguments(
        &self,
    ) -> &GenericArguments {
        &self.trait_associated_symbol_generic_arguments
    }

    /// Returns a mutable reference to the generic arguments of the trait
    /// associated symbol.
    #[must_use]
    pub const fn trait_associated_symbol_generic_arguments_mut(
        &mut self,
    ) -> &mut GenericArguments {
        &mut self.trait_associated_symbol_generic_arguments
    }
}

/// An instance of a trait implementation.
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
pub enum Instance {
    /// Directly refers to an `instance` symbol being defined on module level.
    Symbol(Symbol),

    /// Refers to an instance parameter denoted by `instance I: Trait` syntax.
    Parameter(InstanceParameterID),

    /// Refers to an instance that is associated with another instance.
    ///
    /// # Example
    ///
    /// ```pnx
    /// public trait Fizz:
    ///     public instance Inner
    ///
    /// public struct Buzz[instance I: Fizz]:
    ///     public myField: Foo[I::Inner]
    /// ```
    ///
    /// In the above example, `I::Inner` is an instance associated with the
    /// instance parameter `I`.
    InstanceAssociated(InstanceAssociated),
}

impl From<InstanceParameterID> for Instance {
    fn from(param_id: InstanceParameterID) -> Self { Self::Parameter(param_id) }
}

impl crate::display::Display for InstanceParameterID {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let generic_parameters =
            engine.get_generic_parameters(self.parent_id).await;

        write!(
            formatter,
            "{}",
            &**generic_parameters.get_instance_parameter(self.id).name()
        )
    }
}

impl crate::display::Display for TraitRef {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        crate::display::Display::fmt(&self.0, engine, formatter).await
    }
}

impl crate::display::Display for InstanceAssociated {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        Box::pin(crate::display::Display::fmt(
            &*self.instance,
            engine,
            formatter,
        ))
        .await?;

        write!(formatter, "::")?;

        let name = engine.get_name(self.trait_associated_symbol_id).await;
        write!(formatter, "{}", &*name)?;

        Box::pin(crate::display::Display::fmt(
            &self.trait_associated_symbol_generic_arguments,
            engine,
            formatter,
        ))
        .await
    }
}

impl crate::display::Display for Instance {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Symbol(symbol) => {
                Box::pin(crate::display::Display::fmt(
                    symbol, engine, formatter,
                ))
                .await
            }
            Self::Parameter(param_id) => {
                crate::display::Display::fmt(param_id, engine, formatter).await
            }
            Self::InstanceAssociated(instance_associated) => {
                Box::pin(crate::display::Display::fmt(
                    instance_associated,
                    engine,
                    formatter,
                ))
                .await
            }
        }
    }
}

/// Represents a sub-term location where the sub-term is stored within an
/// [`InstanceAssociated`] term.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubInstanceAssociatedLocation {
    /// The inner instance of the [`InstanceAssociated`].
    Instance,

    /// The index of the sub-term in the generic arguments of the trait
    /// associated symbol.
    GenericArguments(SubGenericArgumentsLocation),
}

/// The location pointing to a sub-lifetime term in an instance.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in an [`Instance::Symbol`] variant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A lifetime argument in an [`Instance::InstanceAssociated`]'s generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubLifetimeLocation> for TermLocation {
    fn from(value: SubLifetimeLocation) -> Self {
        Self::Lifetime(sub_term::SubLifetimeLocation::FromInstance(value))
    }
}

impl Location<Instance, Lifetime> for SubLifetimeLocation {
    fn assign_sub_term(self, term: &mut Instance, sub_term: Lifetime) {
        let reference = match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location)
                .unwrap(),

            term => panic!(
                "invalid sub-lifetime location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Instance) -> Option<Lifetime> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location)
                .cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Instance) -> Option<&Lifetime> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Instance) -> Option<&mut Lifetime> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-type term in an instance.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubTypeLocation {
    /// The index of type argument in an [`Instance::Symbol`] variant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A type argument in an [`Instance::InstanceAssociated`]'s generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubTypeLocation> for TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromInstance(value))
    }
}

impl Location<Instance, Type> for SubTypeLocation {
    fn assign_sub_term(self, term: &mut Instance, sub_term: Type) {
        let reference = match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location)
                .unwrap(),

            term => {
                panic!("invalid sub-type location: {self:?} for term: {term:?}")
            }
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Instance) -> Option<Type> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location)
                .cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Instance) -> Option<&Type> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Instance) -> Option<&mut Type> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-constant term in an instance.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubConstantLocation {
    /// The index of constant argument in an [`Instance::Symbol`] variant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A constant argument in an [`Instance::InstanceAssociated`]'s generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubConstantLocation> for TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromInstance(value))
    }
}

impl Location<Instance, Constant> for SubConstantLocation {
    fn assign_sub_term(self, term: &mut Instance, sub_term: Constant) {
        let reference = match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location)
                .unwrap(),

            term => panic!(
                "invalid sub-constant location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Instance) -> Option<Constant> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location)
                .cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Instance) -> Option<&Constant> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Instance) -> Option<&mut Constant> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-instance term in an instance.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubInstanceLocation {
    /// The index of instance argument in an [`Instance::Symbol`] variant.
    #[from]
    Symbol(SubSymbolLocation),

    /// An instance in an [`Instance::InstanceAssociated`] variant.
    #[from]
    InstanceAssociated(SubInstanceAssociatedLocation),
}

impl From<SubGenericArgumentsLocation> for SubInstanceLocation {
    fn from(value: SubGenericArgumentsLocation) -> Self {
        Self::InstanceAssociated(
            SubInstanceAssociatedLocation::GenericArguments(value),
        )
    }
}

impl From<SubInstanceLocation> for TermLocation {
    fn from(value: SubInstanceLocation) -> Self {
        Self::Instance(sub_term::SubInstanceLocation::FromInstance(value))
    }
}

impl Location<Instance, Instance> for SubInstanceLocation {
    fn assign_sub_term(self, term: &mut Instance, sub_term: Instance) {
        let reference = match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ) => &mut *instance_associated.instance,

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(idx),
                ),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(idx)
                .unwrap(),

            term => panic!(
                "invalid sub-instance location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Instance) -> Option<Instance> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ) => Some((*instance_associated.instance).clone()),

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(idx),
                ),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(idx)
                .cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Instance) -> Option<&Instance> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ) => Some(&*instance_associated.instance),

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(idx),
                ),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term(idx),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Instance) -> Option<&mut Instance> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ) => Some(&mut *instance_associated.instance),

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(idx),
                ),
            ) => instance_associated
                .trait_associated_symbol_generic_arguments
                .get_term_mut(idx),

            _ => None,
        }
    }
}

impl SubTerm for Instance {
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type SubInstanceLocation = SubInstanceLocation;
    type ThisSubTermLocation = SubInstanceLocation;
}

impl Match for Instance {
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id() == rhs.id() => {
                lhs.generic_arguments().substructural_match(
                    rhs.generic_arguments(),
                    Substructural::default(),
                    SubSymbolLocation::new,
                )
            }

            (Self::InstanceAssociated(lhs), Self::InstanceAssociated(rhs))
                if lhs.trait_associated_symbol_id
                    == rhs.trait_associated_symbol_id =>
            {
                let mut substructural = lhs
                    .trait_associated_symbol_generic_arguments
                    .substructural_match(
                        &rhs.trait_associated_symbol_generic_arguments,
                        Substructural::default(),
                        SubGenericArgumentsLocation::new,
                    )?;

                substructural.instances_mut().push(Matching::new(
                    (*lhs.instance).clone(),
                    (*rhs.instance).clone(),
                    SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance,
                    ),
                    SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance,
                    ),
                ));

                Some(substructural)
            }

            // InstanceAssociated and Parameter don't have structural matching
            _ => None,
        }
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        substructural.instances()
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        substructural.instances_mut()
    }
}
