//! Implements the visitor patsern for semantic terms.

use std::{fmt::Debug, future::Future, hash::Hash};

use super::sub_term::{
    SubConstantLocation, SubLifetimeLocation, SubTerm, SubTypeLocation,
    TermLocation,
};
use crate::{
    constant::{self, Constant},
    generic_arguments::{
        GenericArguments, SubMemberSymbolLocation, SubSymbolLocation,
        SubTraitMemberLocation,
    },
    lifetime::Lifetime,
    r#type::{self, SubFunctionSignatureLocation, Type},
    tuple::{SubTupleLocation, Tuple},
    Never, TermRef,
};

/// Represents a visitor that visits a term recursively.
pub trait Recursive<'a, T> {
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
        term: &'a T,
        locations: impl Iterator<Item = TermLocation>,
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
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool;
}

/// Represents a visitor that visits a term.
pub trait Visitor<'a, T: Element> {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    #[must_use]
    fn visit(&mut self, term: &'a T, location: T::Location) -> bool;
}

/// An asynchronous version of the [`Visitor`] trait.
pub trait AsyncVisitor<T: Element> {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    fn visit<'a>(
        &'a mut self,
        term: &'a T,
        location: T::Location,
    ) -> impl Future<Output = bool> + 'a;
}

/// Represents a mutable visitor that visits a term.
pub trait Mutable<T: Element> {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    #[must_use]
    fn visit(&mut self, term: &mut T, location: T::Location) -> bool;
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("cannot call `accept_one_level` on a non-application term")]
#[allow(missing_docs)]
pub struct VisitNonApplicationTermError;

#[derive(Debug)]
struct RecursiveVisitorAdapter<'a, V> {
    visitor: &'a mut V,
    current_locations: Vec<TermLocation>,
}

macro_rules! implements_visit_recursive {
    ($self:ident, $visit_fn:ident, $term:ident, $location:ident) => {{
        $self.current_locations.push($location.into());

        let locations =
            $self.current_locations.iter().copied().collect::<Vec<_>>();

        match $term.$visit_fn($self) {
            Ok(result) => {
                if !result {
                    return false;
                }
            }
            Err(VisitNonApplicationTermError) => {}
        }

        $self.current_locations.pop();

        $self.visitor.visit($term, locations.into_iter())
    }};
}

impl<
        'b,
        T: Element,
        V: Recursive<'b, T>
            + Recursive<'b, Lifetime>
            + Recursive<'b, Type>
            + Recursive<'b, Constant>,
    > Visitor<'b, T> for RecursiveVisitorAdapter<'_, V>
{
    fn visit(&mut self, term: &'b T, location: T::Location) -> bool {
        implements_visit_recursive!(self, accept_one_level, term, location)
    }
}

#[derive(Debug)]
struct RecursiveVisitorAdapterMut<'a, V> {
    visitor: &'a mut V,
    current_locations: Vec<TermLocation>,
}

impl<
        T: Element,
        V: MutableRecursive<T>
            + MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>,
    > Mutable<T> for RecursiveVisitorAdapterMut<'_, V>
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
        + Into<TermLocation>
        + From<Self::ThisSubTermLocation>;

    /// Invokes the visitor on the term itself once.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Similar to [`Element::accept_single()`], but for asynchronous visitors.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single_async<
        'a,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a;

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
        'a,
        V: Recursive<'a, Lifetime> + Recursive<'a, Type> + Recursive<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
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
        locations: impl Iterator<Item = TermLocation>,
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
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;

    /// Similar to [`Element::accept_one_level()`], but for asynchronous
    /// visitors.
    fn accept_one_level_async<
        'a,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &'a self,
        visitor: &'a mut V,
    ) -> impl Future<Output = Result<bool, VisitNonApplicationTermError>> + 'a;

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
pub fn accept_recursive<
    'a,
    V: Recursive<'a, Lifetime> + Recursive<'a, Type> + Recursive<'a, Constant>,
    E: Element,
>(
    element: &'a E,
    visitor: &mut V,
) -> bool {
    let mut adapter =
        RecursiveVisitorAdapter { visitor, current_locations: Vec::new() };

    if let Ok(result) = element.accept_one_level(&mut adapter) {
        if !result {
            return false;
        }
    }

    !element.accept_single_recursive(visitor, std::iter::empty())
}

/// Similar to [`accept_recursive()`], but for mutable references.
pub fn accept_recursive_mut<
    V: MutableRecursive<Lifetime>
        + MutableRecursive<Type>
        + MutableRecursive<Constant>,
    E: Element,
>(
    element: &mut E,
    visitor: &mut V,
) -> bool {
    if !element.accept_single_recursive_mut(visitor, std::iter::empty()) {
        return false;
    }

    let mut adapter =
        RecursiveVisitorAdapterMut { visitor, current_locations: Vec::new() };

    element.accept_one_level_mut(&mut adapter).unwrap_or(true)
}

macro_rules! implements_tuple {
    ($self:ident, $visitor:ident, $accept_single:ident, $iter:ident $(,$await:ident)?) => {{
        for (idx, element) in $self.elements.$iter().enumerate() {
            if !element.term.$accept_single(
                $visitor,
                T::Location::from(SubTupleLocation::Single(idx).into()),
            )$(.$await)? {
                return false;
            }
        }

        true
    }};
}

impl<T: Element + Clone> Tuple<T>
where
    Self: TryFrom<T, Error = T> + Into<T>,
    SubTupleLocation: Into<T::ThisSubTermLocation>,
{
    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single, iter)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single_async, iter, await)
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
        T: Element,
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
        T: Element,
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
        $accept_one_level:ident,
        [$($ref:tt)*]
        $(, $await:ident)?
    ) => {
        match $self {
            Self::Error(_) | Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                if !term
                    .generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubSymbolLocation(id),
                    )$(.$await)? {
                    return Ok(false);
                }

                Ok(true)
            }
            Self::Phantom(term) => Ok(
                $visitor.$visit_type(
                    $($ref)**term.0,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Phantom)
                )$(.$await)?
            ),
            Self::Pointer(term) => Ok($visitor.$visit_type(
                $($ref)**term.pointee,
                SubTypeLocation::FromType(r#type::SubTypeLocation::Pointer)
            )$(.$await)?),
            Self::Reference(term) => Ok($visitor
                .$visit_lifetime(
                    $($ref)*term.lifetime,
                    SubLifetimeLocation::FromType(
                        r#type::SubLifetimeLocation::Reference
                    )
                )$(.$await)?
                && $visitor.$visit_type(
                    $($ref)**term.pointee,
                    SubTypeLocation::FromType(
                        r#type::SubTypeLocation::Reference
                    )
                )$(.$await)?),
            Self::Array(term) => Ok($visitor
                .$visit_type(
                    $($ref)**term.r#type,
                    SubTypeLocation::FromType(r#type::SubTypeLocation::Array)
                )$(.$await)?
                && $visitor
                .$visit_constant(
                    $($ref)*term.length,
                    SubConstantLocation::FromType(
                        r#type::SubConstantLocation::Array
                    )
                )$(.$await)?),
            Self::Tuple(tuple) => Ok(tuple.$accept_one_level($visitor)$(.$await)?),
            Self::MemberSymbol(member_symbol) => Ok(
                member_symbol
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }
                    )$(.$await)?
                    && member_symbol
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>(
                            $visitor,
                            |id| SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            }
                        )$(.$await)?,
            ),
            Self::TraitMember(term) => Ok(
                term
                    .0
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        })
                    )$(.$await)?
                    && term
                        .0
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>(
                            $visitor,
                            |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        )$(.$await)?,
            ),

            Self::FunctionSignature(term) => {
                for (idx, parameter) in ( $($ref)* term.parameters).into_iter().enumerate() {
                    if !$visitor.$visit_type(
                        parameter,
                        SubTypeLocation::FromType(
                            r#type::SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::Parameter(idx)
                            )
                        )
                    )$(.$await)? {
                        return Ok(false);
                    }
                }

                Ok(
                    $visitor.$visit_type(
                        $($ref)**term.return_type,
                        SubTypeLocation::FromType(
                            r#type::SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::ReturnType
                            )
                        )
                    )$(.$await)?
                )
            },
        }
    };
}

impl Element for Type {
    type Location = SubTypeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Self> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    async fn accept_single_async<
        'a,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Self> + AsyncVisitor<Constant>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location).await
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
        'a,
        V: Recursive<'a, Lifetime> + Recursive<'a, Self> + Recursive<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
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
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Self> + Visitor<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(
            self,
            visitor,
            visit,
            visit,
            visit,
            accept_one_level,
            [&]
        )
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
            [&mut]
        )
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Self> + AsyncVisitor<Constant>,
    >(
        &self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(
            self,
            visitor,
            visit,
            visit,
            visit,
            accept_one_level_async,
            [&],
            await
        )
    }
}

impl From<Never> for SubLifetimeLocation {
    fn from(val: Never) -> Self { match val {} }
}

impl Element for Lifetime {
    type Location = SubLifetimeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Self> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
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
        'a,
        V: Recursive<'a, Self> + Recursive<'a, Type> + Recursive<'a, Constant>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
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
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Self> + Visitor<'a, Type> + Visitor<'a, Constant>,
    >(
        &'a self,
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

    fn accept_single_async<
        'a,
        V: AsyncVisitor<Self> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Self> + AsyncVisitor<Type> + AsyncVisitor<Constant>,
    >(
        &self,
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
        $iter:ident,
        [$($ref:tt)*]
        $(, $await:ident)?
    ) => {
        match $self {
            Self::Error(_)
            | Self::Phantom
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
                    )$(. $await)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            Self::Enum(term) => {
                match term.associated_value.$as_ref() {
                    Some(value) => Ok($visitor.$visit_constant(
                        $($ref)* **value,
                        SubConstantLocation::FromConstant(
                            constant::SubConstantLocation::Enum
                        ),
                    )$(. $await)?),
                    None => Ok(true),
                }
            }
            Self::Array(term) => {
                for (idx, element) in term.elements.$iter().enumerate() {
                    if !$visitor.$visit_constant(
                        element,
                        SubConstantLocation::FromConstant(
                            constant::SubConstantLocation::Array(idx)
                        )
                    )$(. $await)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            Self::Tuple(tuple) => Ok(tuple.$accept_one_level($visitor)$(. $await)?),
        }
    };
}

impl Element for Constant {
    type Location = SubConstantLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        'a,
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime> + Recursive<'a, Type> + Recursive<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
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
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime> + Visitor<'a, Type> + Visitor<'a, Self>,
    >(
        &'a self,
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
            iter,
            [&]
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
            [&mut]
        )
    }

    fn accept_single_async<
        'a,
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Self>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime> + AsyncVisitor<Type> + AsyncVisitor<Self>,
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
            accept_one_level_async,
            as_ref,
            iter,
            [&],
            await
        )
    }
}

type Storage<'a> = Vec<(TermRef<'a>, Vec<TermLocation>)>;

#[derive(Debug, Clone)]
struct Collector<'a> {
    terms: Storage<'a>,
}

impl<'a, T: 'a> Recursive<'a, T> for Collector<'a>
where
    &'a T: Into<TermRef<'a>>,
{
    fn visit(
        &mut self,
        term: &'a T,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        self.terms.push((term.into(), locations.collect()));
        true
    }
}

/// An iterator that iterates over all the sub-terms of a term recursively.
///
/// The terms are visited in a depth-first manner.
#[derive(Debug, Clone)]
pub struct RecursiveIterator<'a>(<Storage<'a> as IntoIterator>::IntoIter);

impl<'a> RecursiveIterator<'a> {
    /// Creates a new recursive iterator from a term.
    pub fn new<T: Element>(term: &'a T) -> Self {
        let mut collector = Collector { terms: Storage::new() };
        accept_recursive(term, &mut collector);

        Self(collector.terms.into_iter())
    }
}

impl<'a> Iterator for RecursiveIterator<'a> {
    type Item = (TermRef<'a>, Vec<TermLocation>);

    fn next(&mut self) -> Option<Self::Item> { self.0.next() }
}
