//! Data definitions for instance terms.

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    TermRef,
    constant::Constant,
    error::Error,
    generic_arguments::{
        GenericArguments, SubGenericArgumentsLocation, SubSymbolLocation,
        Symbol,
    },
    generic_parameters::{InstanceParameter, InstanceParameterID},
    inference,
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, IterSubTerms, SubTerm},
    r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    fn generic_argument_arity_matches(&self, other: &Self) -> bool {
        self.trait_associated_symbol_generic_arguments.lifetimes().len()
            == other.trait_associated_symbol_generic_arguments.lifetimes().len()
            && self.trait_associated_symbol_generic_arguments.types().len()
                == other.trait_associated_symbol_generic_arguments.types().len()
            && self.trait_associated_symbol_generic_arguments.constants().len()
                == other
                    .trait_associated_symbol_generic_arguments
                    .constants()
                    .len()
            && self.trait_associated_symbol_generic_arguments.instances().len()
                == other
                    .trait_associated_symbol_generic_arguments
                    .instances()
                    .len()
    }

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
    pub fn get_lifetime(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Interned<Lifetime>> {
        self.trait_associated_symbol_generic_arguments
            .lifetimes()
            .get(location.index())
    }

    /// Returns a type argument from the associated symbol application.
    #[must_use]
    pub fn get_type(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Interned<Type>> {
        self.trait_associated_symbol_generic_arguments
            .types()
            .get(location.index())
    }

    /// Returns a constant argument from the associated symbol application.
    #[must_use]
    pub fn get_constant(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Interned<Constant>> {
        self.trait_associated_symbol_generic_arguments
            .constants()
            .get(location.index())
    }

    /// Returns an instance argument from the associated symbol application.
    #[must_use]
    pub fn get_instance(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Interned<Instance>> {
        self.trait_associated_symbol_generic_arguments
            .instances()
            .get(location.index())
    }

    /// Streams the substructural matches between two instance-associated terms.
    pub(crate) fn substructural_match<'a, L, T, C, I>(
        &'a self,
        other: &'a Self,
        to_lifetime_loc: impl Fn(usize) -> L + 'a,
        to_type_loc: impl Fn(usize) -> T + 'a,
        to_constant_loc: impl Fn(usize) -> C + 'a,
        to_instance_generic_args_loc: impl Fn(usize) -> I + 'a,
        instance_loc: I,
    ) -> Option<impl Iterator<Item = Substructural<L, T, C, I>> + 'a>
    where
        L: Copy + 'a,
        T: Copy + 'a,
        C: Copy + 'a,
        I: Copy + 'a,
    {
        if !self.generic_argument_arity_matches(other) {
            return None;
        }

        Some(pernixc_coroutine_iter::coroutine_iter!({
            for (idx, (lhs, rhs)) in self
                .trait_associated_symbol_generic_arguments
                .lifetimes()
                .iter()
                .cloned()
                .zip(
                    other
                        .trait_associated_symbol_generic_arguments
                        .lifetimes()
                        .iter()
                        .cloned(),
                )
                .enumerate()
            {
                let location = to_lifetime_loc(idx);
                yield Substructural::Lifetime(Matching::new(
                    lhs, rhs, location, location,
                ));
            }

            for (idx, (lhs, rhs)) in self
                .trait_associated_symbol_generic_arguments
                .types()
                .iter()
                .cloned()
                .zip(
                    other
                        .trait_associated_symbol_generic_arguments
                        .types()
                        .iter()
                        .cloned(),
                )
                .enumerate()
            {
                let location = to_type_loc(idx);
                yield Substructural::Type(Matching::new(
                    lhs, rhs, location, location,
                ));
            }

            for (idx, (lhs, rhs)) in self
                .trait_associated_symbol_generic_arguments
                .constants()
                .iter()
                .cloned()
                .zip(
                    other
                        .trait_associated_symbol_generic_arguments
                        .constants()
                        .iter()
                        .cloned(),
                )
                .enumerate()
            {
                let location = to_constant_loc(idx);
                yield Substructural::Constant(Matching::new(
                    lhs, rhs, location, location,
                ));
            }

            for (idx, (lhs, rhs)) in self
                .trait_associated_symbol_generic_arguments
                .instances()
                .iter()
                .cloned()
                .zip(
                    other
                        .trait_associated_symbol_generic_arguments
                        .instances()
                        .iter()
                        .cloned(),
                )
                .enumerate()
            {
                let location = to_instance_generic_args_loc(idx);
                yield Substructural::Instance(Matching::new(
                    lhs, rhs, location, location,
                ));
            }

            yield Substructural::Instance(Matching::new(
                self.instance.clone(),
                other.instance.clone(),
                instance_loc,
                instance_loc,
            ));
        }))
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

/// Represents the location of a generic argument inside an instance-associated
/// symbol application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubInstanceAssociatedGenericArgsLocation(usize);

impl SubInstanceAssociatedGenericArgsLocation {
    /// Returns the index of the generic argument.
    #[must_use]
    pub const fn index(&self) -> usize { self.0 }
}

/// Represents the location of an instance sub-term inside an
/// [`InstanceAssociated`] payload.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubInstanceAssociatedLocation {
    /// The direct parent instance field.
    Instance,

    /// A nested instance inside the associated symbol generic arguments.
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

    /// A lifetime argument in an [`Instance::InstanceAssociated`] generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubLifetimeLocation> for sub_term::TermLocation {
    fn from(value: SubLifetimeLocation) -> Self {
        Self::Lifetime(sub_term::SubLifetimeLocation::FromInstance(value))
    }
}

impl sub_term::Location<Instance, Lifetime> for SubLifetimeLocation {
    fn try_get_sub_term(
        self,
        term: &Instance,
        _: &TrackedEngine,
    ) -> Option<Interned<Lifetime>> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .associated_instance_generic_arguments()
                .get_term(location)
                .cloned(),

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

    /// A type argument in an [`Instance::InstanceAssociated`] generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubTypeLocation> for sub_term::TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromInstance(value))
    }
}

impl sub_term::Location<Instance, Type> for SubTypeLocation {
    fn try_get_sub_term(
        self,
        term: &Instance,
        _: &TrackedEngine,
    ) -> Option<Interned<Type>> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .associated_instance_generic_arguments()
                .get_term(location)
                .cloned(),

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

    /// A constant argument in an [`Instance::InstanceAssociated`] generic
    /// arguments.
    #[from]
    InstanceAssociatedGenericArguments(SubGenericArgumentsLocation),
}

impl From<SubConstantLocation> for sub_term::TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromInstance(value))
    }
}

impl sub_term::Location<Instance, Constant> for SubConstantLocation {
    fn try_get_sub_term(
        self,
        term: &Instance,
        _: &TrackedEngine,
    ) -> Option<Interned<Constant>> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociatedGenericArguments(location),
            ) => instance_associated
                .associated_instance_generic_arguments()
                .get_term(location)
                .cloned(),

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

impl From<SubInstanceLocation> for sub_term::TermLocation {
    fn from(value: SubInstanceLocation) -> Self {
        Self::Instance(sub_term::SubInstanceLocation::FromInstance(value))
    }
}

impl sub_term::Location<Instance, Instance> for SubInstanceLocation {
    fn try_get_sub_term(
        self,
        term: &Instance,
        _: &TrackedEngine,
    ) -> Option<Interned<Instance>> {
        match (term, self) {
            (Instance::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ) => Some(instance_associated.instance().clone()),

            (
                Instance::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(location),
                ),
            ) => instance_associated
                .associated_instance_generic_arguments()
                .get_term(location)
                .cloned(),

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
    fn substructural_match<'a>(
        &'a self,
        other: &'a Self,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Self::SubLifetimeLocation,
                Self::SubTypeLocation,
                Self::SubConstantLocation,
                Self::SubInstanceLocation,
            >,
        > + 'a,
    > {
        enum MatchPlan<'a> {
            Symbol(&'a Symbol, &'a Symbol),
            InstanceAssociated(&'a InstanceAssociated, &'a InstanceAssociated),
        }

        let match_plan =
            if let (Self::Symbol(lhs), Self::Symbol(rhs)) = (self, other) {
                if lhs.id() != rhs.id() {
                    return None;
                }

                MatchPlan::Symbol(lhs, rhs)
            } else if let (
                Self::InstanceAssociated(lhs),
                Self::InstanceAssociated(rhs),
            ) = (self, other)
            {
                if lhs.trait_associated_symbol_id()
                    != rhs.trait_associated_symbol_id()
                {
                    return None;
                }

                MatchPlan::InstanceAssociated(lhs, rhs)
            } else {
                return None;
            };

        Some(pernixc_coroutine_iter::coroutine_iter!({
            match match_plan {
                MatchPlan::Symbol(lhs, rhs) => {
                    for substructural in lhs.generic_arguments().as_ref().substructural_match(
                        rhs.generic_arguments().as_ref(),
                        SubSymbolLocation::new,
                    ).expect("validated symbol ids before building iterator") {
                        yield substructural;
                    }
                }

                MatchPlan::InstanceAssociated(lhs, rhs) => {
                    for substructural in lhs.substructural_match(
                        rhs,
                        |idx| {
                            SubLifetimeLocation::InstanceAssociatedGenericArguments(
                                SubGenericArgumentsLocation::new(idx),
                            )
                        },
                        |idx| {
                            SubTypeLocation::InstanceAssociatedGenericArguments(
                                SubGenericArgumentsLocation::new(idx),
                            )
                        },
                        |idx| {
                            SubConstantLocation::InstanceAssociatedGenericArguments(
                                SubGenericArgumentsLocation::new(idx),
                            )
                        },
                        |idx| {
                            SubInstanceLocation::InstanceAssociated(
                                SubInstanceAssociatedLocation::GenericArguments(
                                    SubGenericArgumentsLocation::new(idx),
                                ),
                            )
                        },
                        SubInstanceLocation::InstanceAssociated(
                            SubInstanceAssociatedLocation::Instance,
                        ),
                    )
                    .expect(
                        "validated instance-associated symbol ids before building iterator",
                    ) {
                        yield substructural;
                    }
                }
            }
        }))
    }

    fn from_self_matching(
        matching: Matching<Interned<Self>, Self::ThisSubTermLocation>,
    ) -> Substructural<
        Self::SubLifetimeLocation,
        Self::SubTypeLocation,
        Self::SubConstantLocation,
        Self::SubInstanceLocation,
    > {
        Substructural::Instance(matching)
    }
}

/// Location of an immediate child yielded by [`IterSubTerms`] for [`Instance`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum IterSubTermLocation {
    /// The child is a lifetime.
    Lifetime(SubLifetimeLocation),

    /// The child is a type.
    Type(SubTypeLocation),

    /// The child is a constant.
    Constant(SubConstantLocation),

    /// The child is an instance.
    Instance(SubInstanceLocation),
}

fn iter_symbol_sub_terms(
    symbol: &Symbol,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    symbol.iter_sub_terms(
        |location| {
            IterSubTermLocation::Lifetime(SubLifetimeLocation::Symbol(location))
        },
        |location| IterSubTermLocation::Type(SubTypeLocation::Symbol(location)),
        |location| {
            IterSubTermLocation::Constant(SubConstantLocation::Symbol(location))
        },
        |location| {
            IterSubTermLocation::Instance(SubInstanceLocation::Symbol(location))
        },
    )
}

fn iter_instance_associated_sub_terms(
    instance_associated: &InstanceAssociated,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    pernixc_coroutine_iter::coroutine_iter!({
        for sub_term in instance_associated
            .associated_instance_generic_arguments()
            .iter_sub_terms_with_location(
                |location| {
                    IterSubTermLocation::Lifetime(
                        SubLifetimeLocation::InstanceAssociatedGenericArguments(
                            SubGenericArgumentsLocation::new(location.index()),
                        ),
                    )
                },
                |location| {
                    IterSubTermLocation::Type(
                        SubTypeLocation::InstanceAssociatedGenericArguments(
                            SubGenericArgumentsLocation::new(location.index()),
                        ),
                    )
                },
                |location| {
                    IterSubTermLocation::Constant(
                        SubConstantLocation::InstanceAssociatedGenericArguments(
                            SubGenericArgumentsLocation::new(location.index()),
                        ),
                    )
                },
                |location| {
                    IterSubTermLocation::Instance(
                        SubInstanceLocation::InstanceAssociated(
                            SubInstanceAssociatedLocation::GenericArguments(
                                SubGenericArgumentsLocation::new(
                                    location.index(),
                                ),
                            ),
                        ),
                    )
                },
            )
        {
            yield sub_term;
        }

        yield (
            TermRef::Instance(instance_associated.instance()),
            IterSubTermLocation::Instance(
                SubInstanceLocation::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ),
        );
    })
}

impl IterSubTerms for Instance {
    type TermLocation = IterSubTermLocation;

    fn iter_sub_terms(
        &self,
    ) -> impl Iterator<Item = (TermRef<'_>, Self::TermLocation)> + '_ {
        pernixc_coroutine_iter::coroutine_iter!({
            match self {
                Self::Symbol(symbol) => {
                    for sub_term in iter_symbol_sub_terms(symbol) {
                        yield sub_term;
                    }
                }

                Self::InstanceAssociated(instance_associated) => {
                    for sub_term in
                        iter_instance_associated_sub_terms(instance_associated)
                    {
                        yield sub_term;
                    }
                }

                Self::Parameter(_)
                | Self::Inference(_)
                | Self::AnonymousTrait(_)
                | Self::Error(_) => {}
            }
        })
    }
}
