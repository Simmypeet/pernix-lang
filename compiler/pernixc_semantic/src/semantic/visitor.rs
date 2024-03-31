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
pub trait Recursive {
    /// Visits a type.
    ///
    /// # Parameters
    ///
    /// - `ty`: The type to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_type(
        &mut self,
        ty: &Type,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Visits a lifetime.
    ///
    /// # Parameters
    ///
    /// - `lifetime`: The lifetime to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_lifetime(
        &mut self,
        lifetime: &Lifetime,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Visits a constant.
    ///
    /// # Parameters
    ///
    /// - `constant`: The constant to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_constant(
        &mut self,
        constant: &Constant,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;
}

/// Represents a mutable visitor that visits a term recursively.
pub trait MutableRecursive {
    /// Visits a type.
    ///
    /// # Parameters
    ///
    /// - `ty`: The type to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_type(
        &mut self,
        ty: &mut Type,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Visits a lifetime.
    ///
    /// # Parameters
    ///
    /// - `lifetime`: The lifetime to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_lifetime(
        &mut self,
        lifetime: &mut Lifetime,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Visits a constant.
    ///
    /// # Parameters
    ///
    /// - `constant`: The constant to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current term. The last element of the iterator is the location of the
    ///   current term. If the iterator is empty, then the current term is the
    ///   root term.
    fn visit_constant(
        &mut self,
        constant: &mut Constant,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;
}

/// Represents a visitor that visits a term.
pub trait Visitor {
    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the type.
    #[must_use]
    fn visit_type(&mut self, ty: &Type, location: SubTypeLocation) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the lifetime.
    #[must_use]
    fn visit_lifetime(
        &mut self,
        lifetime: &Lifetime,
        location: SubLifetimeLocation,
    ) -> bool;

    /// Visits a constant.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the constant.
    #[must_use]
    fn visit_constant(
        &mut self,
        constant: &Constant,
        location: SubConstantLocation,
    ) -> bool;
}

/// Represents a mutable visitor that visits a term.
#[allow(clippy::module_name_repetitions)]
pub trait Mutable {
    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the type.
    #[must_use]
    fn visit_type(&mut self, ty: &mut Type, location: SubTypeLocation) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the lifetime.
    #[must_use]
    fn visit_lifetime(
        &mut self,
        lifetime: &mut Lifetime,
        locatinon: SubLifetimeLocation,
    ) -> bool;

    /// Visits a constant.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the constant.
    #[must_use]
    fn visit_constant(
        &mut self,
        constant: &mut Constant,
        location: SubConstantLocation,
    ) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("cannot call `accept_one_level` on a non-application term")]
#[allow(missing_docs)]
pub struct VisitNonApplicationTermError;

#[derive(Debug)]
struct RecursiveVisitorAdapter<'a, V: Recursive> {
    visitor: &'a mut V,
    current_locations: Vec<SubTermLocation>,
}

macro_rules! implements_visit_recursive {
    ($self:ident, $visit_fn:ident, $term:ident, $location:ident) => {{
        let current_locations = $self.current_locations.clone();

        if !$self.visitor.$visit_fn(
            $term,
            current_locations
                .iter()
                .copied()
                .chain(std::iter::once($location.into())),
        ) {
            return false;
        }

        $self.current_locations = current_locations;

        true
    }};
}

impl<'a, V: Recursive> Visitor for RecursiveVisitorAdapter<'a, V> {
    fn visit_type(&mut self, ty: &Type, location: SubTypeLocation) -> bool {
        implements_visit_recursive!(self, visit_type, ty, location)
    }

    fn visit_lifetime(
        &mut self,
        lifetime: &Lifetime,
        location: SubLifetimeLocation,
    ) -> bool {
        implements_visit_recursive!(self, visit_lifetime, lifetime, location)
    }

    fn visit_constant(
        &mut self,
        constant: &Constant,
        location: SubConstantLocation,
    ) -> bool {
        implements_visit_recursive!(self, visit_constant, constant, location)
    }
}

#[derive(Debug)]
struct RecursiveVisitorAdapterMut<'a, V: MutableRecursive> {
    visitor: &'a mut V,
    current_locations: Vec<SubTermLocation>,
}

impl<'a, V: MutableRecursive> Mutable for RecursiveVisitorAdapterMut<'a, V> {
    fn visit_type(&mut self, ty: &mut Type, location: SubTypeLocation) -> bool {
        implements_visit_recursive!(self, visit_type, ty, location)
    }

    fn visit_lifetime(
        &mut self,
        lifetime: &mut Lifetime,
        location: SubLifetimeLocation,
    ) -> bool {
        implements_visit_recursive!(self, visit_lifetime, lifetime, location)
    }

    fn visit_constant(
        &mut self,
        constant: &mut Constant,
        location: SubConstantLocation,
    ) -> bool {
        implements_visit_recursive!(self, visit_constant, constant, location)
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
    fn accept_single(
        &self,
        visitor: &mut impl Visitor,
        location: Self::Location,
    ) -> bool;

    /// Similar to [`Element::accept_single()`], but for mutable references.
    fn accept_single_mut(
        &mut self,
        visitor: &mut impl Mutable,
        location: Self::Location,
    ) -> bool;

    /// Invokes the [`Recursive`] visitor on the term itself.
    fn accept_single_recursive(
        &self,
        visitor: &mut impl Recursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool;

    /// Similar to [`Element::accept_single_recursive()`], but for mutable
    /// references.
    fn accept_single_recursive_mut(
        &mut self,
        visitor: &mut impl MutableRecursive,
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
    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError>;

    /// Similar to [`Element::accept_one_level()`], but for mutable references.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level_mut(
        &mut self,
        visitor: &mut impl Mutable,
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
pub fn accept_recursive(
    element: &impl Element,
    visitor: &mut impl Recursive,
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
pub fn accept_recursive_mut(
    element: &mut impl Element,
    visitor: &mut impl MutableRecursive,
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
    fn accept_one_level(&self, visitor: &mut impl Visitor) -> bool {
        implements_tuple!(self, visitor, accept_single, iter)
    }

    fn accept_one_level_mut(&mut self, visitor: &mut impl Mutable) -> bool {
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
    fn accept_one_level<T: Element, Idx>(
        &self,
        visitor: &mut impl Visitor,
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
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            iter,
            map_idx
        )
    }

    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level_mut<T: Element, Idx>(
        &mut self,
        visitor: &mut impl Mutable,
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
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            iter_mut,
            map_idx
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
                    .$accept_one_level::<Self, _>(
                        $visitor,
                        |id| SubSymbolLocation(id),
                    ) {
                    return Ok(false);
                }

                Ok(true)
            }
            Self::Phantom(term) => Ok(
                $visitor.$visit_type(
                    &$($ref)?term.0,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Phantom)
                )
            ),
            Self::Pointer(term) => Ok($visitor.$visit_type(
                &$($ref)? term.pointee,
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
                    &$($ref)?term.pointee,
                    SubTypeLocation::FromType(
                        r#type::SubTypeLocation::Reference
                    )
                )),
            Self::Array(term) => Ok($visitor
                .$visit_type(
                    &$($ref)?term.r#type,
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
                    &$($ref)?local.0,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Local)
                )
            ),
            Self::TraitMember(trait_member) => Ok(
                trait_member
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _>(
                        $visitor,
                        |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        })
                    )
                    && trait_member
                        .member_generic_arguments
                        .$accept_one_level::<Self, _>(
                            $visitor,
                            |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        ),
            ),
            Self::MemberSymbol(implementation) => Ok(implementation
                .member_generic_arguments
                .$accept_one_level::<Self, _>(
                    $visitor,
                    |id| SubMemberSymbolLocation {
                        index: id,
                        from_parent: false,
                    }
                )
                && implementation
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _>(
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

    fn accept_single(
        &self,
        visitor: &mut impl Visitor,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit_type(self, location)
    }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            accept_one_level
        )
    }

    fn accept_single_mut(
        &mut self,
        visitor: &mut impl Mutable,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit_type(self, location)
    }

    fn accept_one_level_mut(
        &mut self,
        visitor: &mut impl Mutable,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            accept_one_level_mut,
            mut
        )
    }

    fn accept_single_recursive(
        &self,
        visitor: &mut impl Recursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_type(self, locations)
    }

    fn accept_single_recursive_mut(
        &mut self,
        visitor: &mut impl MutableRecursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_type(self, locations)
    }
}

impl From<Never> for SubLifetimeLocation {
    fn from(val: Never) -> Self { match val {} }
}

impl Element for Lifetime {
    type Location = SubLifetimeLocation;

    fn accept_single(
        &self,
        visitor: &mut impl Visitor,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit_lifetime(self, location)
    }

    fn accept_one_level(
        &self,
        _: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_single_mut(
        &mut self,
        visitor: &mut impl Mutable,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit_lifetime(self, location)
    }

    fn accept_one_level_mut(
        &mut self,
        _: &mut impl Mutable,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_single_recursive(
        &self,
        visitor: &mut impl Recursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_lifetime(self, locations)
    }

    fn accept_single_recursive_mut(
        &mut self,
        visitor: &mut impl MutableRecursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_lifetime(self, locations)
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
                    x,
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
                &$($ref)?local.0,
                SubConstantLocation::FromConstant(
                    constant::SubConstantLocation::Local
                )
            )),

            Self::TraitMember(trait_member) => {
                Ok(trait_member
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _>($visitor, |id|
                        SubTraitMemberLocation(SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }))
                    && trait_member
                        .member_generic_arguments
                        .$accept_one_level::<Self, _>($visitor, |id|
                            SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        ),
                    )
            }

            Self::Symbol(symbol) => {
                Ok(symbol.generic_arguments.$accept_one_level::<Self, _>(
                    $visitor,
                    |id| SubSymbolLocation(id),
                ))
            }
            Self::MemberSymbol(term) => {
                Ok(term.member_generic_arguments.$accept_one_level::<Self, _>(
                    $visitor,
                    |id| SubMemberSymbolLocation {
                        index: id,
                        from_parent: false,
                    })
                    && term
                        .parent_generic_arguments
                        .$accept_one_level::<Self, _>($visitor,
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

    fn accept_single(
        &self,
        visitor: &mut impl Visitor,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit_constant(self, location)
    }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_constant!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            accept_one_level,
            as_ref,
            iter
        )
    }

    fn accept_single_mut(
        &mut self,
        visitor: &mut impl Mutable,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit_constant(self, location)
    }

    fn accept_one_level_mut(
        &mut self,
        visitor: &mut impl Mutable,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_constant!(
            self,
            visitor,
            visit_type,
            visit_lifetime,
            visit_constant,
            accept_one_level_mut,
            as_mut,
            iter_mut,
            mut
        )
    }

    fn accept_single_recursive(
        &self,
        visitor: &mut impl Recursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_constant(self, locations)
    }

    fn accept_single_recursive_mut(
        &mut self,
        visitor: &mut impl MutableRecursive,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        visitor.visit_constant(self, locations)
    }
}
