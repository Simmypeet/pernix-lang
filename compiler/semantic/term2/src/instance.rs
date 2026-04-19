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
    folding::{Abort, Foldable, Folder, fold_interned},
    generic_arguments::{
        GenericArguments, SubGenericArgumentsLocation, SubSymbolLocation,
        Symbol, fold_symbol_payload,
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
    Identifiable,
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

pub(crate) fn fold_trait_ref_payload<F: Folder>(
    trait_ref: &mut TraitRef,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    let mut generic_arguments = trait_ref.generic_arguments().clone();
    GenericArguments::fold_with(&mut generic_arguments, folder, engine)?;

    *trait_ref = TraitRef::new(trait_ref.trait_id(), generic_arguments);
    Ok(())
}

pub(crate) fn fold_instance_associated_payload<F: Folder>(
    instance_associated: &mut InstanceAssociated,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    let mut instance = instance_associated.instance().clone();
    Instance::fold_with(&mut instance, folder, engine)?;

    let mut generic_arguments =
        instance_associated.associated_instance_generic_arguments().clone();
    GenericArguments::fold_with(&mut generic_arguments, folder, engine)?;

    *instance_associated = InstanceAssociated::new(
        instance,
        instance_associated.trait_associated_symbol_id(),
        generic_arguments,
    );
    Ok(())
}

fn fold_instance_payload<F: Folder>(
    instance: &mut Instance,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    *instance = match instance.clone() {
        Instance::Symbol(mut symbol) => {
            fold_symbol_payload(&mut symbol, folder, engine)?;
            Instance::Symbol(symbol)
        }

        Instance::Parameter(parameter) => Instance::Parameter(parameter),

        Instance::InstanceAssociated(mut instance_associated) => {
            fold_instance_associated_payload(
                &mut instance_associated,
                folder,
                engine,
            )?;

            Instance::InstanceAssociated(instance_associated)
        }

        Instance::Inference(variable) => Instance::Inference(variable),
        Instance::AnonymousTrait(trait_instance) => {
            Instance::AnonymousTrait(trait_instance)
        }
        Instance::Error(error) => Instance::Error(error),
    };

    Ok(())
}

impl Foldable for TraitRef {
    fn fold_with<F: Folder>(
        term: &mut Interned<Self>,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        fold_interned(
            term,
            folder,
            engine,
            fold_trait_ref_payload,
            |_, _, _| Ok(()),
        )
    }
}

impl Foldable for InstanceAssociated {
    fn fold_with<F: Folder>(
        term: &mut Interned<Self>,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        fold_interned(
            term,
            folder,
            engine,
            fold_instance_associated_payload,
            |_, _, _| Ok(()),
        )
    }
}

impl Foldable for Instance {
    fn fold_with<F: Folder>(
        term: &mut Interned<Self>,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        fold_interned(
            term,
            folder,
            engine,
            fold_instance_payload,
            Folder::fold_instance,
        )
    }
}

#[cfg(test)]
mod tests {
    use pernixc_symbol::SymbolID;
    use pernixc_target::TargetID;

    use super::*;
    use crate::{
        TermRef, constant,
        matching::{Match, Substructural},
        sub_term::{IterSubTerms, Location, RecursivelyIterSubTerms},
        test_support::create_test_engine,
        r#type::{Primitive, Type},
    };

    #[tokio::test]
    async fn sub_term_locations_return_interned_children() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(11));
        let nested_symbol_id =
            TargetID::TEST.make_global(SymbolID::from_u128(12));

        let lifetime = tracked.intern(Lifetime::Static);
        let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
        let constant = tracked.intern(constant::Constant::Primitive(
            constant::Primitive::Bool(true),
        ));
        let parent_instance = tracked
            .intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
        let nested_instance = tracked.intern(Instance::AnonymousTrait(
            AnoymousTrait::new(nested_symbol_id),
        ));
        let generic_arguments = tracked.intern(GenericArguments::new(
            vec![lifetime.clone()],
            vec![r#type.clone()],
            vec![constant.clone()],
            vec![nested_instance.clone()],
        ));
        let instance = tracked.intern(Instance::InstanceAssociated(
            InstanceAssociated::new(
                parent_instance.clone(),
                symbol_id,
                generic_arguments,
            ),
        ));

        assert_eq!(
            SubLifetimeLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            )
            .get_sub_term(instance.as_ref(), &tracked),
            lifetime,
        );
        assert_eq!(
            SubTypeLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            )
            .get_sub_term(instance.as_ref(), &tracked),
            r#type,
        );
        assert_eq!(
            SubConstantLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            )
            .get_sub_term(instance.as_ref(), &tracked),
            constant,
        );
        assert_eq!(
            SubInstanceLocation::InstanceAssociated(
                SubInstanceAssociatedLocation::Instance,
            )
            .get_sub_term(instance.as_ref(), &tracked),
            parent_instance,
        );
        assert_eq!(
            SubInstanceLocation::InstanceAssociated(
                SubInstanceAssociatedLocation::GenericArguments(
                    SubGenericArgumentsLocation::new(0),
                ),
            )
            .get_sub_term(instance.as_ref(), &tracked),
            nested_instance,
        );
    }

    #[tokio::test]
    async fn iter_sub_terms_instance_associated_order_is_stable() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(101));
        let nested_symbol_id =
            TargetID::TEST.make_global(SymbolID::from_u128(102));

        let lifetime = tracked.intern(Lifetime::Static);
        let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
        let constant = tracked.intern(constant::Constant::Primitive(
            constant::Primitive::Bool(true),
        ));
        let parent_instance = tracked
            .intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
        let nested_instance = tracked.intern(Instance::AnonymousTrait(
            AnoymousTrait::new(nested_symbol_id),
        ));
        let generic_arguments = tracked.intern(GenericArguments::new(
            vec![lifetime.clone()],
            vec![r#type.clone()],
            vec![constant.clone()],
            vec![nested_instance.clone()],
        ));
        let instance = Instance::InstanceAssociated(InstanceAssociated::new(
            parent_instance.clone(),
            symbol_id,
            generic_arguments,
        ));

        let sub_terms: Vec<_> = instance.iter_sub_terms().collect();
        assert_eq!(sub_terms.len(), 5);

        assert!(matches!(
            sub_terms[0].0,
            TermRef::Lifetime(term) if term == &lifetime
        ));
        assert_eq!(
            sub_terms[0].1,
            IterSubTermLocation::Lifetime(
                SubLifetimeLocation::InstanceAssociatedGenericArguments(
                    SubGenericArgumentsLocation::new(0),
                ),
            ),
        );

        assert!(matches!(
            sub_terms[1].0,
            TermRef::Type(term) if term == &r#type
        ));
        assert_eq!(
            sub_terms[1].1,
            IterSubTermLocation::Type(
                SubTypeLocation::InstanceAssociatedGenericArguments(
                    SubGenericArgumentsLocation::new(0),
                ),
            ),
        );

        assert!(matches!(
            sub_terms[2].0,
            TermRef::Constant(term) if term == &constant
        ));
        assert_eq!(
            sub_terms[2].1,
            IterSubTermLocation::Constant(
                SubConstantLocation::InstanceAssociatedGenericArguments(
                    SubGenericArgumentsLocation::new(0),
                ),
            ),
        );

        assert!(matches!(
            sub_terms[3].0,
            TermRef::Instance(term) if term == &nested_instance
        ));
        assert_eq!(
            sub_terms[3].1,
            IterSubTermLocation::Instance(
                SubInstanceLocation::InstanceAssociated(
                    SubInstanceAssociatedLocation::GenericArguments(
                        SubGenericArgumentsLocation::new(0),
                    ),
                ),
            ),
        );

        assert!(matches!(
            sub_terms[4].0,
            TermRef::Instance(term) if term == &parent_instance
        ));
        assert_eq!(
            sub_terms[4].1,
            IterSubTermLocation::Instance(
                SubInstanceLocation::InstanceAssociated(
                    SubInstanceAssociatedLocation::Instance,
                ),
            ),
        );
    }

    #[tokio::test]
    async fn substructural_match_streams_parent_instance_last() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(301));
        let left_nested_id =
            TargetID::TEST.make_global(SymbolID::from_u128(302));
        let right_nested_id =
            TargetID::TEST.make_global(SymbolID::from_u128(303));

        let lhs_lifetime = tracked.intern(Lifetime::Static);
        let rhs_lifetime = tracked.intern(Lifetime::Erased);
        let lhs_type = tracked.intern(Type::Primitive(Primitive::Bool));
        let rhs_type = tracked.intern(Type::Primitive(Primitive::Uint32));
        let lhs_constant = tracked.intern(constant::Constant::Primitive(
            constant::Primitive::Uint8(1),
        ));
        let rhs_constant = tracked.intern(constant::Constant::Primitive(
            constant::Primitive::Uint8(2),
        ));
        let lhs_nested = tracked.intern(Instance::AnonymousTrait(
            AnoymousTrait::new(left_nested_id),
        ));
        let rhs_nested = tracked.intern(Instance::AnonymousTrait(
            AnoymousTrait::new(right_nested_id),
        ));
        let lhs_parent = tracked
            .intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
        let rhs_parent = tracked
            .intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));

        let lhs = Instance::InstanceAssociated(InstanceAssociated::new(
            lhs_parent.clone(),
            symbol_id,
            tracked.intern(GenericArguments::new(
                vec![lhs_lifetime],
                vec![lhs_type],
                vec![lhs_constant],
                vec![lhs_nested],
            )),
        ));
        let rhs = Instance::InstanceAssociated(InstanceAssociated::new(
            rhs_parent.clone(),
            symbol_id,
            tracked.intern(GenericArguments::new(
                vec![rhs_lifetime],
                vec![rhs_type],
                vec![rhs_constant],
                vec![rhs_nested],
            )),
        ));

        let matches: Vec<_> = lhs.substructural_match(&rhs).unwrap().collect();
        assert_eq!(matches.len(), 5);

        assert!(matches!(&matches[0], Substructural::Lifetime(_)));
        assert!(matches!(&matches[1], Substructural::Type(_)));
        assert!(matches!(&matches[2], Substructural::Constant(_)));
        assert!(matches!(&matches[3], Substructural::Instance(_)));

        assert!(matches!(
            &matches[4],
            Substructural::Instance(matching)
                if matching.lhs() == &lhs_parent
                && matching.rhs() == &rhs_parent
                && matching.lhs_location()
                    == &SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance,
                    )
                && matching.rhs_location()
                    == &SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance,
                    )
        ));
    }

    #[tokio::test]
    async fn recursive_iteration_includes_root_in_depth_first_order() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(201));
        let nested_symbol_id =
            TargetID::TEST.make_global(SymbolID::from_u128(202));

        let lifetime = tracked.intern(Lifetime::Static);
        let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
        let constant = tracked.intern(constant::Constant::Primitive(
            constant::Primitive::Bool(true),
        ));
        let parent_instance = tracked
            .intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
        let nested_instance = tracked.intern(Instance::AnonymousTrait(
            AnoymousTrait::new(nested_symbol_id),
        ));
        let generic_arguments = tracked.intern(GenericArguments::new(
            vec![lifetime.clone()],
            vec![r#type.clone()],
            vec![constant.clone()],
            vec![nested_instance.clone()],
        ));
        let root = tracked.intern(Instance::InstanceAssociated(
            InstanceAssociated::new(
                parent_instance.clone(),
                symbol_id,
                generic_arguments,
            ),
        ));

        let terms: Vec<_> = root.iter_sub_terms_recursive().collect();
        assert_eq!(terms.len(), 6);

        assert!(matches!(terms[0], TermRef::Instance(term) if term == &root));
        assert!(
            matches!(terms[1], TermRef::Lifetime(term) if term == &lifetime)
        );
        assert!(matches!(terms[2], TermRef::Type(term) if term == &r#type));
        assert!(
            matches!(terms[3], TermRef::Constant(term) if term == &constant)
        );
        assert!(matches!(
            terms[4],
            TermRef::Instance(term) if term == &nested_instance
        ));
        assert!(matches!(
            terms[5],
            TermRef::Instance(term) if term == &parent_instance
        ));
    }
}
