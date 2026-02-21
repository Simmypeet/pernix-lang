//! Contains the definition of [`GenericArguments`] and related types.

use std::{fmt::Write, ops::Not};

use derive_new::new;
use pernixc_symbol::{
    name::{get_name, get_qualified_name},
    parent::get_parent,
};
use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    constant::Constant,
    generic_parameters::{
        ConstantParameterID, GenericKind, GenericParameters,
        LifetimeParameterID, TypeParameterID,
    },
    instantiation::Instantiation,
    lifetime::Lifetime,
    matching::{Matching, Substructural},
    sub_term::{SubConstantLocation, SubLifetimeLocation, SubTypeLocation},
    r#type::Type,
    visitor::{self, AsyncMutable, AsyncVisitor, Mutable, Visitor},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a list of generic arguments supplied to a particular symbol that
/// has generic parameters (e.g., `symbol[ARGS]`).
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
    /// The lifetimes supplied to the term.
    lifetimes: Vec<Lifetime>,

    /// The types supplied to the term.
    types: Vec<Type>,

    /// The constants supplied to the term.
    constants: Vec<Constant>,
}

impl GenericArguments {
    /// Returns the lifetimes supplied to the term.
    #[must_use]
    pub fn lifetimes(&self) -> &[Lifetime] { &self.lifetimes }

    /// Returns the types supplied to the term.
    #[must_use]
    pub fn types(&self) -> &[Type] { &self.types }

    /// Returns the constants supplied to the term.
    #[must_use]
    pub fn constants(&self) -> &[Constant] { &self.constants }

    /// Constructs the generic arguments from the given generic parameters and
    /// instantiation.
    ///
    /// This works by extraing the mapping from the `instantiation` for each
    /// generic parameter in `generic_parameters` and constructing the generic
    /// arguments from the extracted mapping.
    #[must_use]
    pub fn from_generic_parameters_and_instantiation(
        instantiation: &Instantiation,
        generic_parameters: &GenericParameters,
        global_id: Global<pernixc_symbol::ID>,
    ) -> Self {
        let mut generic_arguments = Self::default();

        for lifetime_id in generic_parameters.lifetime_order() {
            let lifetime_parameter = Lifetime::Parameter(
                LifetimeParameterID::new(global_id, *lifetime_id),
            );

            let lifetime = instantiation
                .get_lifetime_mapping(&lifetime_parameter)
                .unwrap();

            generic_arguments.lifetimes.push(lifetime.clone());
        }

        for type_id in generic_parameters.type_order() {
            let type_parameter =
                Type::Parameter(TypeParameterID::new(global_id, *type_id));

            let r#type =
                instantiation.get_type_mapping(&type_parameter).unwrap();

            generic_arguments.types.push(r#type.clone());
        }

        for constant_id in generic_parameters.constant_order() {
            let constant_parameter = Constant::Parameter(
                ConstantParameterID::new(global_id, *constant_id),
            );

            let constant = instantiation
                .get_constant_mapping(&constant_parameter)
                .unwrap();

            generic_arguments.constants.push(constant.clone());
        }

        generic_arguments
    }

    /// Applies the given instantiation to `self`, replacing any generic
    /// arguments that are mapped by the `instantiation` with their
    /// corresponding mapped values.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        for lifetime in &mut self.lifetimes {
            instantiation.instantiate(lifetime);
        }

        for r#type in &mut self.types {
            instantiation.instantiate(r#type);
        }

        for constant in &mut self.constants {
            instantiation.instantiate(constant);
        }
    }
    /// Destructures the generic arguments into its components.
    #[must_use]
    pub fn into_arguments(self) -> (Vec<Lifetime>, Vec<Type>, Vec<Constant>) {
        (self.lifetimes, self.types, self.constants)
    }

    /// Checks if the generic arguments are empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lifetimes.is_empty()
            && self.types.is_empty()
            && self.constants.is_empty()
    }

    /// Checks if `self` and `other` have the same number of generic arguments.
    #[must_use]
    pub const fn has_same_arguments_count(&self, other: &Self) -> bool {
        self.lifetimes.len() == other.lifetimes.len()
            && self.types.len() == other.types.len()
            && self.constants.len() == other.constants.len()
    }
}

impl crate::display::Display for GenericArguments {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if !formatter.configuration().generic_arguments_will_be_displayed(self)
        {
            return Ok(());
        }

        let last_is =
            if self.constants.is_empty().not() {
                GenericKind::Constant
            } else if self.types.is_empty().not() {
                GenericKind::Type
            } else if self.lifetimes.iter().any(|x| {
                formatter.configuration().lifetime_will_be_displayed(x)
            }) {
                GenericKind::Lifetime
            } else {
                unreachable!()
            };

        write!(formatter, "[")?;

        let lts = self
            .lifetimes
            .iter()
            .filter(|lt| {
                formatter.configuration().lifetime_will_be_displayed(lt)
            })
            .collect::<Vec<_>>();
        let lts_len = lts.len();

        for (i, lt) in lts.into_iter().enumerate() {
            crate::display::Display::fmt(lt, engine, formatter).await?;

            if i + 1 != lts_len || last_is != GenericKind::Lifetime {
                write!(formatter, ", ")?;
            }
        }

        let tys_len = self.types.len();

        for (i, ty) in self.types.iter().enumerate() {
            crate::display::Display::fmt(ty, engine, formatter).await?;

            if i + 1 != tys_len || last_is != GenericKind::Type {
                write!(formatter, ", ")?;
            }
        }

        let consts_len = self.constants.len();

        for (i, constant) in self.constants.iter().enumerate() {
            crate::display::Display::fmt(constant, engine, formatter).await?;

            if i + 1 != consts_len || last_is != GenericKind::Constant {
                write!(formatter, ", ")?;
            }
        }

        write!(formatter, "]")
    }
}

mod sealed {
    use crate::generic_arguments::GenericArguments;

    pub trait Sealed {
        fn get(generic_arguments: &GenericArguments) -> &[Self]
        where
            Self: Sized;

        fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self>
        where
            Self: Sized;
    }
}

/// A trait for retrieving the arguments array from a generic arguments.
#[allow(missing_docs)]
pub trait Element: sealed::Sealed {}

impl sealed::Sealed for Lifetime {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.lifetimes
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.lifetimes
    }
}

impl Element for Lifetime {}

impl sealed::Sealed for Type {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.types
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }
}

impl Element for Type {}

impl sealed::Sealed for Constant {
    fn get(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }
}

impl Element for Constant {}

/// Represents a term where the a symbol is supplied with generic arguments
/// (e.g., `symbol[ARGS]`).
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
pub struct Symbol {
    /// The ID of the symbol that is supplied with generic arguments.
    id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the symbol.
    generic_arguments: GenericArguments,
}

impl Symbol {
    /// Returns the ID of the symbol.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::ID> { self.id }

    /// Returns the generic arguments supplied to the symbol.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        &self.generic_arguments
    }
}

/// Represents a term where the associated symbol is supplied with generic
/// arguments as well as their parent (e.g., `symbol[ARGS]::associated[ARGS]`).
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
    /// The ID of the associated symbol.
    ///
    /// By associated symbol, we mean a symbol that is defined in the context
    /// of another symbol, such as a method or an associated type.
    id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the parent of the associated symbol.
    parent_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the associated symbol.
    member_generic_arguments: GenericArguments,
}

impl AssociatedSymbol {
    /// Returns the ID of the associated symbol.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::ID> { self.id }

    /// Returns the generic arguments supplied to the parent of the associated
    /// symbol.
    #[must_use]
    pub const fn parent_generic_arguments(&self) -> &GenericArguments {
        &self.parent_generic_arguments
    }

    /// Returns the generic arguments supplied to the associated symbol.
    #[must_use]
    pub const fn member_generic_arguments(&self) -> &GenericArguments {
        &self.member_generic_arguments
    }
}

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
///
/// The `usize` represents the index of the sub-term in the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubSymbolLocation(usize);

/// Represents a sub-term location where the sub-term is stored as a generic
/// arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct SubMemberSymbolLocation {
    /// The index of the sub-term in the generic arguments.
    index: usize,

    /// True if the sub-term is in the parent's generic arguments part,
    /// otherwise false.
    from_parent: bool,
}

impl AssociatedSymbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubMemberSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = if location.from_parent {
            T::get_mut(&mut self.parent_generic_arguments)
        } else {
            T::get_mut(&mut self.member_generic_arguments)
        };

        generic_arguments.get_mut(location.index)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubMemberSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = if location.from_parent {
            T::get(&self.parent_generic_arguments)
        } else {
            T::get(&self.member_generic_arguments)
        };

        generic_arguments.get(location.index)
    }
}

impl Symbol {
    /// Returns a mutable reference to a particular sub-term of this generic
    /// arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term_mut<T: Element>(
        &mut self,
        location: SubSymbolLocation,
    ) -> Option<&mut T> {
        let generic_arguments = T::get_mut(&mut self.generic_arguments);

        generic_arguments.get_mut(location.0)
    }

    /// Returns a reference to a particular sub-term of this generic arguments.
    ///
    /// Returns `None` if the location is invalid.
    #[must_use]
    pub fn get_term<T: Element>(
        &self,
        location: SubSymbolLocation,
    ) -> Option<&T> {
        let generic_arguments = T::get(&self.generic_arguments);

        generic_arguments.get(location.0)
    }
}

impl GenericArguments {
    /// Substructurally matches `self` to `to`.
    pub fn substructural_match<L, T, C, Y>(
        &self,
        other: &Self,
        mut existing: Substructural<L, T, C>,
        to_location: impl Fn(usize) -> Y,
    ) -> Option<Substructural<L, T, C>>
    where
        Y: Into<L> + Into<T> + Into<C> + Copy,
    {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return None;
        }

        for (idx, (lhs, rhs)) in self
            .lifetimes
            .iter()
            .cloned()
            .zip(other.lifetimes.iter().cloned())
            .enumerate()
        {
            let location = to_location(idx);
            existing.lifetimes_mut().push(Matching::new(
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
            existing.types_mut().push(Matching::new(
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
            existing.constants_mut().push(Matching::new(
                lhs,
                rhs,
                location.into(),
                location.into(),
            ));
        }

        Some(existing)
    }
}

impl GenericArguments {
    /// Checks if there's any errornous term in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.lifetimes.iter().any(Lifetime::is_error)
            || self.types.iter().any(Type::is_error)
            || self.constants.iter().any(Constant::is_error)
    }
}

impl crate::display::Display for Symbol {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        if formatter.configuration().short_qualified_identifiers() {
            let name = engine.get_name(self.id).await;
            write!(formatter, "{}", &*name)?;
        } else {
            let qualified_name = engine.get_qualified_name(self.id).await;
            write!(formatter, "{qualified_name}")?;
        }

        self.generic_arguments.fmt(engine, formatter).await
    }
}

impl crate::display::Display for AssociatedSymbol {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let parent_id = self
            .id
            .target_id
            .make_global(engine.get_parent(self.id).await.unwrap());

        let parent_qualified_name = engine.get_qualified_name(parent_id).await;
        write!(formatter, "{parent_qualified_name}")?;
        self.parent_generic_arguments.fmt(engine, formatter).await?;

        let name = engine.get_name(self.id).await;
        write!(formatter, "::{}", &*name)?;

        self.member_generic_arguments.fmt(engine, formatter).await?;

        Ok(())
    }
}

macro_rules! implements_generic_arguments {
    (
        $self:ident,
        $visitor:ident,
        $visit_type:ident,
        $visit_lifetime:ident,
        $visit_constant:ident,
        $iter:ident,
        $map_idx:ident
        $(, $await:ident)?
    ) => {{
        for (id, lifetime) in $self.lifetimes.$iter().enumerate() {
            if !$visitor.$visit_lifetime(
                lifetime,
                Into::<T::SubLifetimeLocation>::into($map_idx(id)).into(),
            )$(.$await)? {
                return false;
            }
        }

        for (idx, ty) in $self.types.$iter().enumerate() {
            if !$visitor.$visit_type(
                ty,
                Into::<T::SubTypeLocation>::into($map_idx(idx)).into(),
            )$(.$await)? {
                return false;
            }
        }

        for (idx, constant) in $self.constants.$iter().enumerate() {
            if !$visitor.$visit_constant(
                constant,
                Into::<T::SubConstantLocation>::into($map_idx(idx)).into(),
            )$(.$await)? {
                return false;
            }
        }

        true
    }};
}

impl GenericArguments {
    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level<
        'a,
        T: visitor::Element,
        Idx,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, iter, map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    async fn accept_one_level_async<
        T: visitor::Element,
        Idx,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, iter, map_idx, await
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level_mut<
        T: visitor::Element,
        Idx,
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, iter_mut, map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    async fn accept_one_level_async_mut<
        T: visitor::Element,
        Idx,
        V: AsyncMutable<Lifetime> + AsyncMutable<Type> + AsyncMutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        map_idx: impl Fn(usize) -> Idx,
    ) -> bool
    where
        Idx: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_generic_arguments!(
            self, visitor, visit, visit, visit, iter_mut, map_idx, await
        )
    }
}

macro_rules! implements_symbol {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident,
        $visit_type:ident
        $(, $await:ident)?
    ) => {{
        if !$self
            .generic_arguments
            .$accept_one_level::<$visit_type, _, _>(
                $visitor,
                |id| SubSymbolLocation(id),
            )$(.$await)? {
            return false;
        }

        true
    }};
}

impl Symbol {
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_async, T, await)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_mut, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        V: AsyncMutable<Lifetime> + AsyncMutable<Type> + AsyncMutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_symbol!(self, visitor, accept_one_level_async_mut, T, await)
    }
}

macro_rules! implements_associated_symbol {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident,
        $visit_type:ident
        $(, $await:ident)?
    ) => {
        $self
            .parent_generic_arguments
            .$accept_one_level::<$visit_type, _, _>(
                $visitor,
                |id| SubMemberSymbolLocation {
                    index: id,
                    from_parent: true,
                }
            )$(.$await)?
            && $self
                .member_generic_arguments
                .$accept_one_level::<$visit_type, _, _>(
                    $visitor,
                    |id| SubMemberSymbolLocation {
                        index: id,
                        from_parent: false,
                    }
                )$(.$await)?
    };
}

impl AssociatedSymbol {
    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level<
        'a,
        T: visitor::Element,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool
    where
        SubMemberSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_associated_symbol!(self, visitor, accept_one_level, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async<
        T: visitor::Element,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool
    where
        SubMemberSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_associated_symbol!(
            self,
            visitor,
            accept_one_level_async,
            T,
            await
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) fn accept_one_level_mut<
        T: visitor::Element,
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubMemberSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_associated_symbol!(self, visitor, accept_one_level_mut, T)
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    pub(crate) async fn accept_one_level_async_mut<
        T: visitor::Element,
        V: AsyncMutable<Lifetime> + AsyncMutable<Type> + AsyncMutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool
    where
        SubMemberSymbolLocation: Into<T::SubConstantLocation>
            + Into<T::SubTypeLocation>
            + Into<T::SubLifetimeLocation>,
        T::SubLifetimeLocation: Into<SubLifetimeLocation>,
        T::SubTypeLocation: Into<SubTypeLocation>,
        T::SubConstantLocation: Into<SubConstantLocation>,
    {
        implements_associated_symbol!(
            self,
            visitor,
            accept_one_level_async_mut,
            T,
            await
        )
    }
}
