//! Defines the [`Instance`] term

use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::get_name;
use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    constant::Constant,
    error::{Error, contains_error},
    generic_arguments::{
        GenericArguments, SubGenericArgumentsLocation, SubSymbolLocation,
        Symbol,
    },
    generic_parameters::{InstanceParameterID, get_generic_parameters},
    inference,
    instantiation::Instantiation,
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, Location, SubTerm, TermLocation},
    r#type::Type,
    visitor::{self, AsyncMutable, AsyncVisitor, Mutable, Visitor},
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
    Identifiable,
)]
pub struct TraitRef(Symbol);

impl TraitRef {
    /// Retrieves the trait ID of this `TraitRef`.
    #[must_use]
    pub const fn trait_id(&self) -> Global<pernixc_symbol::ID> { self.0.id() }

    /// Returns a reference to the generic arguments of the trait associated
    /// with this `TraitRef`.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        self.0.generic_arguments()
    }
}

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

    /// If the inner instance is a symbol, returns a reference to the symbol.
    /// Otherwise, returns `None`.
    #[must_use]
    pub fn instance_as_symbol(&self) -> Option<&Symbol> {
        match self.instance.as_ref() {
            Instance::Symbol(symbol) => Some(symbol),
            _ => None,
        }
    }

    /// If the inner instance is an instance associated with another instance,
    /// returns a reference to the inner `InstanceAssociated`. Otherwise,
    /// returns `None`.
    #[must_use]
    pub fn instance_as_associated_instance(&self) -> Option<&Self> {
        match self.instance.as_ref() {
            Instance::InstanceAssociated(instance_associated) => {
                Some(instance_associated)
            }

            _ => None,
        }
    }

    /// Returns the generic arguments supplied to the associated instance
    /// symbol.
    #[must_use]
    pub const fn associated_instance_generic_arguments(
        &self,
    ) -> &GenericArguments {
        &self.trait_associated_symbol_generic_arguments
    }

    /// Returns the trait associated symbol ID.
    #[must_use]
    pub const fn trait_associated_symbol_id(
        &self,
    ) -> Global<pernixc_symbol::ID> {
        self.trait_associated_symbol_id
    }

    /// Returns a reference to the lifetime at the given location.
    #[must_use]
    pub fn get_lifetime(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Lifetime> {
        self.trait_associated_symbol_generic_arguments
            .lifetimes()
            .get(location.0)
    }

    /// Returns a mutable reference to the lifetime at the given location.
    #[must_use]
    pub fn get_lifetime_mut(
        &mut self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&mut Lifetime> {
        self.trait_associated_symbol_generic_arguments
            .lifetimes_mut()
            .get_mut(location.0)
    }

    /// Returns a reference to the type at the given location.
    #[must_use]
    pub fn get_type(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Type> {
        self.trait_associated_symbol_generic_arguments.types().get(location.0)
    }

    /// Returns a mutable reference to the type at the given location.
    #[must_use]
    pub fn get_type_mut(
        &mut self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&mut Type> {
        self.trait_associated_symbol_generic_arguments
            .types_mut()
            .get_mut(location.0)
    }

    /// Returns a reference to the constant at the given location.
    #[must_use]
    pub fn get_constant(
        &self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Constant> {
        self.trait_associated_symbol_generic_arguments
            .constants()
            .get(location.0)
    }

    /// Returns a mutable reference to the constant at the given location.
    #[must_use]
    pub fn get_constant_mut(
        &mut self,
        location: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&mut Constant> {
        self.trait_associated_symbol_generic_arguments
            .constants_mut()
            .get_mut(location.0)
    }

    /// Returns a reference to the instance at the given index in the generic
    /// arguments.
    #[must_use]
    pub fn get_instance(
        &self,
        index: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&Instance> {
        self.trait_associated_symbol_generic_arguments.instances().get(index.0)
    }

    /// Returns a mutable reference to the instance at the given index in the
    /// generic arguments.
    #[must_use]
    pub fn get_instance_mut(
        &mut self,
        index: SubInstanceAssociatedGenericArgsLocation,
    ) -> Option<&mut Instance> {
        self.trait_associated_symbol_generic_arguments
            .instances_mut()
            .get_mut(index.0)
    }

    /// Performs substructural matching between two `InstanceAssociated` terms.
    ///
    /// This method creates `Matching` entries for all sub-terms (lifetimes,
    /// types, constants, instances) in the generic arguments, as well as
    /// the parent instance.
    ///
    /// # Type Parameters
    ///
    /// - `L`: The lifetime location type for the substructural result
    /// - `T`: The type location type for the substructural result
    /// - `C`: The constant location type for the substructural result
    /// - `I`: The instance location type for the substructural result
    ///
    /// # Arguments
    ///
    /// - `other`: The other `InstanceAssociated` to match against
    /// - `to_lifetime_loc`: Closure to create lifetime locations from index
    /// - `to_type_loc`: Closure to create type locations from index
    /// - `to_constant_loc`: Closure to create constant locations from index
    /// - `to_instance_generic_args_loc`: Closure to create instance locations
    ///   from index (for instances in generic arguments)
    /// - `instance_loc`: The location for the parent instance field
    ///
    /// # Returns
    ///
    /// `Some(Substructural)` if matching succeeded, `None` if the generic
    /// arguments have different lengths.
    #[allow(clippy::type_complexity)]
    pub(crate) fn substructural_match<L, T, C, I>(
        &self,
        other: &Self,
        to_lifetime_loc: impl Fn(usize) -> L,
        to_type_loc: impl Fn(usize) -> T,
        to_constant_loc: impl Fn(usize) -> C,
        to_instance_generic_args_loc: impl Fn(usize) -> I,
        instance_loc: I,
    ) -> Option<Substructural<L, T, C, I>>
    where
        L: Copy,
        T: Copy,
        C: Copy,
        I: Copy,
    {
        // Check that generic arguments have matching lengths
        if self.trait_associated_symbol_generic_arguments.lifetimes().len()
            != other.trait_associated_symbol_generic_arguments.lifetimes().len()
            || self.trait_associated_symbol_generic_arguments.types().len()
                != other.trait_associated_symbol_generic_arguments.types().len()
            || self.trait_associated_symbol_generic_arguments.constants().len()
                != other
                    .trait_associated_symbol_generic_arguments
                    .constants()
                    .len()
            || self.trait_associated_symbol_generic_arguments.instances().len()
                != other
                    .trait_associated_symbol_generic_arguments
                    .instances()
                    .len()
        {
            return None;
        }

        let mut substructural = Substructural::default();

        // Match lifetimes
        for (idx, (l, r)) in self
            .trait_associated_symbol_generic_arguments
            .lifetimes()
            .iter()
            .zip(
                other
                    .trait_associated_symbol_generic_arguments
                    .lifetimes()
                    .iter(),
            )
            .enumerate()
        {
            let loc = to_lifetime_loc(idx);
            substructural.lifetimes_mut().push(Matching::new(
                l.clone(),
                r.clone(),
                loc,
                loc,
            ));
        }

        // Match types
        for (idx, (l, r)) in self
            .trait_associated_symbol_generic_arguments
            .types()
            .iter()
            .zip(other.trait_associated_symbol_generic_arguments.types().iter())
            .enumerate()
        {
            let loc = to_type_loc(idx);
            substructural.types_mut().push(Matching::new(
                l.clone(),
                r.clone(),
                loc,
                loc,
            ));
        }

        // Match constants
        for (idx, (l, r)) in self
            .trait_associated_symbol_generic_arguments
            .constants()
            .iter()
            .zip(
                other
                    .trait_associated_symbol_generic_arguments
                    .constants()
                    .iter(),
            )
            .enumerate()
        {
            let loc = to_constant_loc(idx);
            substructural.constants_mut().push(Matching::new(
                l.clone(),
                r.clone(),
                loc,
                loc,
            ));
        }

        // Match instances in generic arguments
        for (idx, (l, r)) in self
            .trait_associated_symbol_generic_arguments
            .instances()
            .iter()
            .zip(
                other
                    .trait_associated_symbol_generic_arguments
                    .instances()
                    .iter(),
            )
            .enumerate()
        {
            let loc = to_instance_generic_args_loc(idx);
            substructural.instances_mut().push(Matching::new(
                l.clone(),
                r.clone(),
                loc,
                loc,
            ));
        }

        // Match the parent instance
        substructural.instances_mut().push(Matching::new(
            self.instance().clone(),
            other.instance().clone(),
            instance_loc,
            instance_loc,
        ));

        Some(substructural)
    }

    /// Applies the mapping from the given instantiation to this
    /// `InstanceAssociated`.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.trait_associated_symbol_generic_arguments
            .instantiate(instantiation);
        instantiation.instantiate(self.instance_mut());
    }

    /// Checks if any term in this `InstanceAssociated` contains an error.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.trait_associated_symbol_generic_arguments.contains_error()
            || contains_error(&*self.instance)
    }
}

macro_rules! implements_instance_associated {
    (
        $self:ident,
        $visitor:ident,
        $visit_instance:ident,
        $accept_one_level:ident,
        $instance_accessor:ident,
        $map_idx:expr,
        $direct_instance_loc:expr
        $(, $await:ident)?
    ) => {{
        // Delegate to GenericArguments.accept_one_level for generic args
        if !$self
            .trait_associated_symbol_generic_arguments
            .$accept_one_level::<T, _, _>($visitor, $map_idx)$(.$await)?
        {
            return false;
        }

        // Visit the direct instance
        let loc: T::SubInstanceLocation = $direct_instance_loc.into();
        $visitor.$visit_instance($self.$instance_accessor(), loc.into())$(.$await)?
    }};
}

impl InstanceAssociated {
    /// Visits one level of sub-terms in this `InstanceAssociated`.
    ///
    /// This visits all terms in the generic arguments as well as the
    /// direct instance field.
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        Idx,
        IDirectLoc,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
        direct_instance_loc: IDirectLoc,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        IDirectLoc: Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<sub_term::SubLifetimeLocation>,
        T::SubTypeLocation: Into<sub_term::SubTypeLocation>,
        T::SubConstantLocation: Into<sub_term::SubConstantLocation>,
        T::SubInstanceLocation: Into<sub_term::SubInstanceLocation>,
    {
        implements_instance_associated!(
            self,
            visitor,
            visit,
            accept_one_level,
            instance,
            map_idx,
            direct_instance_loc
        )
    }

    /// Async version of [`Self::accept_one_level`].
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        Idx,
        IDirectLoc,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
        direct_instance_loc: IDirectLoc,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        IDirectLoc: Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<sub_term::SubLifetimeLocation>,
        T::SubTypeLocation: Into<sub_term::SubTypeLocation>,
        T::SubConstantLocation: Into<sub_term::SubConstantLocation>,
        T::SubInstanceLocation: Into<sub_term::SubInstanceLocation>,
    {
        implements_instance_associated!(
            self,
            visitor,
            visit,
            accept_one_level_async,
            instance,
            map_idx,
            direct_instance_loc,
            await
        )
    }

    /// Mutable version of [`Self::accept_one_level`].
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        Idx,
        IDirectLoc,
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
        direct_instance_loc: IDirectLoc,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        IDirectLoc: Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<sub_term::SubLifetimeLocation>,
        T::SubTypeLocation: Into<sub_term::SubTypeLocation>,
        T::SubConstantLocation: Into<sub_term::SubConstantLocation>,
        T::SubInstanceLocation: Into<sub_term::SubInstanceLocation>,
    {
        implements_instance_associated!(
            self,
            visitor,
            visit,
            accept_one_level_mut,
            instance_mut,
            map_idx,
            direct_instance_loc
        )
    }

    /// Async mutable version of [`Self::accept_one_level`].
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        Idx,
        IDirectLoc,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
        direct_instance_loc: IDirectLoc,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>
            + Into<T::SubInstanceLocation>,
        IDirectLoc: Into<T::SubInstanceLocation>,
        T::SubLifetimeLocation: Into<sub_term::SubLifetimeLocation>,
        T::SubTypeLocation: Into<sub_term::SubTypeLocation>,
        T::SubConstantLocation: Into<sub_term::SubConstantLocation>,
        T::SubInstanceLocation: Into<sub_term::SubInstanceLocation>,
    {
        implements_instance_associated!(
            self,
            visitor,
            visit,
            accept_one_level_async_mut,
            instance_mut,
            map_idx,
            direct_instance_loc,
            await
        )
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
    EnumAsInner,
    derive_more::From,
    Identifiable,
)]
pub enum Instance {
    /// Directly refers to an `instance` symbol being defined on module level.
    #[from]
    Symbol(Symbol),

    /// Refers to an instance parameter denoted by `instance I: Trait` syntax.
    #[from]
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
    #[from]
    InstanceAssociated(InstanceAssociated),

    /// Used as an inference variable in the IR binding.
    #[from]
    Inference(inference::Variable<Self>),

    /// Represents an erroneous instance term, used for error recovery.
    #[from]
    Error(Error),
}

impl crate::display::Display for InstanceParameterID {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let generic_parameters =
            engine.get_generic_parameters(self.parent_id()).await;

        write!(
            formatter,
            "{}",
            &**generic_parameters.get_instance_parameter(self.id()).name()
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

            Self::Inference(var) => {
                let Some(rendering) = formatter
                    .configuration()
                    .instance_infernces()
                    .and_then(|x| x.get(var))
                else {
                    return write!(formatter, "_");
                };

                match rendering {
                    crate::display::InferenceRendering::Recurse(ty) => {
                        Box::pin(ty.fmt(engine, formatter)).await
                    }
                    crate::display::InferenceRendering::Rendered(flex_str) => {
                        write!(formatter, "{}", flex_str.as_ref())
                    }
                }
            }

            Self::Error(_) => write!(formatter, "{{error}}"),
        }
    }
}

/// Represents a sub-term location within an [`InstanceAssociated`] term's
/// generic arguments.
///
/// This is used for accessing lifetimes, types, and constants within the
/// `trait_associated_symbol_generic_arguments` field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SubInstanceAssociatedGenericArgsLocation(usize);

impl SubInstanceAssociatedGenericArgsLocation {
    /// Creates a new `SubInstanceAssociatedGenericArgsLocation` with the given
    /// index.
    #[must_use]
    pub const fn new(index: usize) -> Self { Self(index) }

    /// Returns the inner index.
    #[must_use]
    pub const fn index(&self) -> usize { self.0 }
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
                if lhs.trait_associated_symbol_id()
                    == rhs.trait_associated_symbol_id() =>
            {
                lhs.substructural_match(
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
