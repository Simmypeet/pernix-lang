//! Implements the visitor patsern for semantic terms.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;
use thiserror::Error;

use super::{
    sub_term::SubTerm,
    term::{
        constant::{self, Constant},
        lifetime::Lifetime,
        r#type::{self, Type},
        GenericArguments, Never, Tuple, TupleElement,
    },
};
use crate::semantic::sub_term::{
    SubMemberSymbolLocation, SubSymbolLocation, SubTraitMemberLocation,
    SubTupleLocation,
};

/// An enumeration of locations where a lifetime can be located.
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
pub enum SubLifetimeLocation {
    /// The location points to a lifetime that is a part of a type.
    FromType(r#type::SubLifetimeLocation),

    /// The location points to a lifetime that is a part of a constant.
    FromConstant(constant::SubLifetimeLocation),
}

/// An enumeration of locations where a type can be located.
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
pub enum SubTypeLocation {
    /// The location points to a type that is a part of a type.
    FromType(r#type::SubTypeLocation),

    /// The location points to a type that is a part of a constant.
    FromConstant(constant::SubTypeLocation),
}

/// An enumeration of locations where a constant can be located.
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
pub enum SubConstantLocation {
    /// The location points to a constant that is a part of a type.
    FromType(r#type::SubConstantLocation),

    /// The location points to a constant that is a part of a constant.
    FromConstant(constant::SubConstantLocation),
}

/// Enumeration of all sub-location of all kinds of terms.
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
pub enum SubTermLocation {
    /// The location points to a lifetime.
    Lifetime(SubLifetimeLocation),

    /// The location points to a type.
    Type(SubTypeLocation),

    /// The location points to a constant.
    Constant(SubConstantLocation),
}

/// Represents a visitor that visits a term recursively.
pub trait Recursive<T> {
    /// Visits a term
    ///
    /// # Parameters
    ///
    /// - `term`: The term to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit(
        &mut self,
        term: &T,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;
}

/// Represents a mutable visitor that visits a term recursively.
pub trait MutableRecursive<T> {
    /// Visits a term.
    ///
    /// # Parameters
    ///
    /// - `T`: The term to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit(
        &mut self,
        term: &mut T,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;
}

/// Represents a visitor that visits a term.
pub trait Visitor<T: Element> {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    #[must_use]
    fn visit(&mut self, term: &T, location: T::Location) -> bool;
}

/// Represents a mutable visitor that visits a term.
#[allow(clippy::module_name_repetitions)]
pub trait Mutable<T: Element> {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    #[must_use]
    fn visit(&mut self, term: &mut T, location: T::Location) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("cannot call `accept_one_level` on a non-application term")]
#[allow(missing_docs)]
pub struct VisitNonApplicationTermError;

#[derive(Debug)]
struct RecursiveVisitorAdapter<
    'a,
    V: Recursive<Lifetime> + Recursive<Type> + Recursive<Constant>,
> {
    visitor: &'a mut V,
    current_locations: Vec<SubTermLocation>,
}

macro_rules! implements_visit_recursive {
    ($self:ident, $visit_fn:ident, $term:ident, $location:ident) => {{
        let current_locations = $self.current_locations.clone();
        let next_locations = $self
            .current_locations
            .iter()
            .copied()
            .chain(std::iter::once($location.into()))
            .collect::<Vec<_>>();

        if !$self.visitor.visit($term, next_locations.iter().copied()) {
            return false;
        }

        $self.current_locations = next_locations;

        if !$term.$visit_fn($self).unwrap_or(true) {
            return false;
        }

        $self.current_locations = current_locations;

        true
    }};
}

impl<
        'a,
        T: Element,
        V: Recursive<T>
            + Recursive<Lifetime>
            + Recursive<Type>
            + Recursive<Constant>,
    > Visitor<T> for RecursiveVisitorAdapter<'a, V>
{
    fn visit(&mut self, term: &T, location: T::Location) -> bool {
        implements_visit_recursive!(self, accept_one_level, term, location)
    }
}

#[derive(Debug)]
struct RecursiveVisitorAdapterMut<
    'a,
    V: MutableRecursive<Lifetime>
        + MutableRecursive<Type>
        + MutableRecursive<Constant>,
> {
    visitor: &'a mut V,
    current_locations: Vec<SubTermLocation>,
}

impl<
        'a,
        T: Element,
        V: MutableRecursive<T>
            + MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>,
    > Mutable<T> for RecursiveVisitorAdapterMut<'a, V>
{
    fn visit(&mut self, term: &mut T, location: T::Location) -> bool {
        implements_visit_recursive!(self, accept_one_level_mut, term, location)
    }
}

/// A term for the visitor pattern.
pub trait Element: Sized + SubTerm {
    /// The location type of the term.
    type Location: Debug
        + Clone
        + Copy
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + Into<SubTermLocation>
        + From<Self::ThisSubTermLocation>;

    /// Invokes the visitor on the term itself once.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single<V: Visitor<Lifetime> + Visitor<Type> + Visitor<Constant>>(
        &self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Similar to [`Element::accept_single()`], but for mutable references.
    fn accept_single_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Invokes the [`Recursive`] visitor on the term itself.
    fn accept_single_recursive<
        V: Recursive<Lifetime> + Recursive<Type> + Recursive<Constant>,
    >(
        &self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Similar to [`Element::accept_single_recursive()`], but for mutable
    /// references.
    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Visits one sub-level of the term.
    ///
    /// # Example
    ///
    /// When a term `Type[int32, Vec[float32]]` got called, the visitor will be
    /// visiting only `int32` and `Vec[int32]` (i.e. the sub-terms of the
    /// term), which is unlike the `accept` method that will also visit the
    /// term itself and `float32`.
    ///
    /// # Returns
    ///
    /// If returns boolean false, the visitor will early stop visiting the
    /// sub-terms of the term.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level<
        V: Visitor<Lifetime> + Visitor<Type> + Visitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;

    /// Similar to [`Element::accept_one_level()`], but for mutable references.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;
}

/// Invokes the visitor on the term itself and all of its sub-terms recursively.
///
/// # Example
///
/// When a term `Type[int32, Vec[float32]]` got called, the visitor will be
/// visiting `Type[int32, Vec[float32]]`, `int32`, `Vec[float32]`, and
/// `float32`.
///
/// # Returns
///
/// Returns `true` if the visitor has visited all of the sub-terms of the term.
pub fn accept_recursive<
    V: Recursive<Lifetime> + Recursive<Type> + Recursive<Constant>,
>(
    element: &impl Element,
    visitor: &mut V,
) -> bool {
    if !element.accept_single_recursive(visitor, std::iter::empty()) {
        return false;
    }

    let mut adapter =
        RecursiveVisitorAdapter { visitor, current_locations: Vec::new() };

    match element.accept_one_level(&mut adapter) {
        Ok(result) => result,
        Err(VisitNonApplicationTermError) => true,
    }
}

/// Similar to [`accept_recursive()`], but for mutable references.
pub fn accept_recursive_mut<
    V: MutableRecursive<Lifetime>
        + MutableRecursive<Type>
        + MutableRecursive<Constant>,
>(
    element: &mut impl Element,
    visitor: &mut V,
) -> bool {
    if !element.accept_single_recursive_mut(visitor, std::iter::empty()) {
        return false;
    }

    let mut adapter =
        RecursiveVisitorAdapterMut { visitor, current_locations: Vec::new() };

    match element.accept_one_level_mut(&mut adapter) {
        Ok(result) => result,
        Err(VisitNonApplicationTermError) => true,
    }
}

macro_rules! implements_tuple {
    ($self:ident, $visitor:ident, $accept_single:ident, $iter:ident) => {{
        for (idx, element) in $self.elements.$iter().enumerate() {
            if !match element {
                TupleElement::Regular(term) | TupleElement::Unpacked(term) => {
                    term.$accept_single(
                        $visitor,
                        SubTupleLocation::Single(idx).into().into(),
                    )
                }
            } {
                return false;
            }
        }

        true
    }};
}

impl<Term: Element + Clone> Tuple<Term>
where
    Self: TryFrom<Term, Error = Term> + Into<Term>,
    SubTupleLocation: Into<Term::ThisSubTermLocation>,
{
    fn accept_one_level<
        V: Visitor<Lifetime> + Visitor<Type> + Visitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single, iter)
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single_mut, iter_mut)
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
    ) => {{
        for (id, lifetime) in $self.lifetimes.$iter().enumerate() {
            if !$visitor.$visit_lifetime(
                lifetime,
                Into::<T::SubLifetimeLocation>::into($map_idx(id)).into(),
            ) {
                return false;
            }
        }

        for (idx, ty) in $self.types.$iter().enumerate() {
            if !$visitor.$visit_type(
                ty,
                Into::<T::SubTypeLocation>::into($map_idx(idx)).into(),
            ) {
                return false;
            }
        }

        for (idx, constant) in $self.constants.$iter().enumerate() {
            if !$visitor.$visit_constant(
                constant,
                Into::<T::SubConstantLocation>::into($map_idx(idx)).into(),
            ) {
                return false;
            }
        }

        true
    }};
}

impl GenericArguments {
    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level<
        T: Element,
        Idx,
        V: Visitor<Lifetime> + Visitor<Type> + Visitor<Constant>,
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
            self, visitor, visit, visit, visit, iter, map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level_mut<
        T: Element,
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
}

macro_rules! implements_type {
    (
        $self:ident,
        $visitor:ident,
        $visit_type:ident,
        $visit_lifetime:ident,
        $visit_constant:ident,
        $accept_one_level:ident
        $(, $ref:ident)?
    ) => {
        match $self {
            Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                if !term
                    .generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubSymbolLocation(id),
                    ) {
                    return Ok(false);
                }

                Ok(true)
            }
            Self::Phantom(term) => Ok(
                $visitor.$visit_type(
                    &$($ref)?*term.0,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Phantom)
                )
            ),
            Self::Pointer(term) => Ok($visitor.$visit_type(
                &$($ref)?*term.pointee,
                SubTypeLocation::FromType(r#type::SubTypeLocation::Pointer)
            )),
            Self::Reference(term) => Ok($visitor
                .$visit_lifetime(
                    &$($ref)?term.lifetime,
                    SubLifetimeLocation::FromType(
                        r#type::SubLifetimeLocation::Reference
                    )
                )
                && $visitor.$visit_type(
                    &$($ref)?*term.pointee,
                    SubTypeLocation::FromType(
                        r#type::SubTypeLocation::Reference
                    )
                )),
            Self::Array(term) => Ok($visitor
                .$visit_type(
                    &$($ref)?*term.r#type,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Array)
                )
                && $visitor
                .$visit_constant(
                    &$($ref)? term.length,
                    SubConstantLocation::FromType(
                        r#type::SubConstantLocation::Array
                    )
                )),
            Self::Tuple(tuple) => Ok(tuple.$accept_one_level($visitor)),
            Self::Local(local) => Ok($visitor
                .$visit_type(
                    &$($ref)?*local.0,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Local)
                )
            ),
            Self::TraitMember(trait_member) => Ok(
                trait_member
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        })
                    )
                    && trait_member
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>(
                            $visitor,
                            |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        ),
            ),
            Self::MemberSymbol(implementation) => Ok(implementation
                .member_generic_arguments
                .$accept_one_level::<Self, _, _>(
                    $visitor,
                    |id| SubMemberSymbolLocation {
                        index: id,
                        from_parent: false,
                    }
                )
                && implementation
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }
                    )),
        }
    };
}

impl Element for Type {
    type Location = SubTypeLocation;

    fn accept_single<
        V: Visitor<Lifetime> + Visitor<Self> + Visitor<Constant>,
    >(
        &self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Lifetime> + Mutable<Self> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        V: Recursive<Lifetime> + Recursive<Self> + Recursive<Constant>,
    >(
        &self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Self>
            + MutableRecursive<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        V: Visitor<Lifetime> + Visitor<Self> + Visitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(self, visitor, visit, visit, visit, accept_one_level)
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime> + Mutable<Self> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(
            self,
            visitor,
            visit,
            visit,
            visit,
            accept_one_level_mut,
            mut
        )
    }
}

impl From<Never> for SubLifetimeLocation {
    fn from(val: Never) -> Self { match val {} }
}

impl Element for Lifetime {
    type Location = SubLifetimeLocation;

    fn accept_single<V: Visitor<Self> + Visitor<Type> + Visitor<Constant>>(
        &self,
        visitor: &mut V,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Self> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        V: Recursive<Self> + Recursive<Type> + Recursive<Constant>,
    >(
        &self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Self>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        V: Visitor<Self> + Visitor<Type> + Visitor<Constant>,
    >(
        &self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_one_level_mut<
        V: Mutable<Self> + Mutable<Type> + Mutable<Constant>,
    >(
        &mut self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }
}

macro_rules! implements_constant {
    (
        $self:ident,
        $visitor:ident,
        $visit_type:ident,
        $visit_lifetime:ident,
        $visit_constant:ident,
        $accept_one_level:ident,
        $as_ref:ident,
        $iter:ident
        $(, $ref:ident)?
    ) => {
        match $self {
            Self::Phantom(_)
            | Self::Primitive(_)
            | Self::Parameter(_)
            | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Struct(term) => {
                for (idx, field) in term.fields.$iter().enumerate() {
                    if !$visitor.$visit_constant(
                        field,
                        SubConstantLocation::FromConstant(
                            constant::SubConstantLocation::Struct(idx)
                        )
                    ) {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            Self::Enum(term) => Ok(term
                .associated_value
                .$as_ref()
                .map_or(true, |x| $visitor.$visit_constant(
                    & $($ref)? **x,
                    SubConstantLocation::FromConstant(
                        constant::SubConstantLocation::Enum
                    )
                ))),
            Self::Array(term) => {
                Ok(term.elements.$iter().enumerate().all(|(idx, x)|
                    $visitor.$visit_constant(
                        x,
                        SubConstantLocation::FromConstant(
                            constant::SubConstantLocation::Array(idx)
                        )
                    )
                ))
            }

            Self::Tuple(tuple) => Ok(tuple.$accept_one_level($visitor)),
            Self::Local(local) => Ok($visitor.$visit_constant(
                &$($ref)?*local.0,
                SubConstantLocation::FromConstant(
                    constant::SubConstantLocation::Local
                )
            )),

            Self::TraitMember(trait_member) => {
                Ok(trait_member
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>($visitor, |id|
                        SubTraitMemberLocation(SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }))
                    && trait_member
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>($visitor, |id|
                            SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        ),
                    )
            }

            Self::Symbol(symbol) => {
                Ok(symbol.generic_arguments.$accept_one_level::<Self, _, _>(
                    $visitor,
                    |id| SubSymbolLocation(id),
                ))
            }
            Self::MemberSymbol(term) => {
                Ok(term.member_generic_arguments.$accept_one_level::<Self, _, _>(
                    $visitor,
                    |id| SubMemberSymbolLocation {
                        index: id,
                        from_parent: false,
                    })
                    && term
                        .parent_generic_arguments
                        .$accept_one_level::<Self, _, _>($visitor,
                        |id| SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }
                    )
                )
            }
        }
    };
}

impl Element for Constant {
    type Location = SubConstantLocation;

    fn accept_single<V: Visitor<Lifetime> + Visitor<Type> + Visitor<Self>>(
        &self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        V: Recursive<Lifetime> + Recursive<Type> + Recursive<Self>,
    >(
        &self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Self>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        V: Visitor<Lifetime> + Visitor<Type> + Visitor<Self>,
    >(
        &self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_constant!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit,
            accept_one_level,
            as_ref,
            iter
        )
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_constant!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit,
            accept_one_level_mut,
            as_mut,
            iter_mut,
            mut
        )
    }
}
