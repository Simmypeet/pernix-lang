//! Implements the visitor patsern for semantic terms.

use std::{fmt::Debug, hash::Hash};

use thiserror::Error;

use super::sub_term::{
    SubConstantLocation, SubLifetimeLocation, SubTerm, SubTypeLocation,
    TermLocation,
};
use crate::{
    constant::{self, Constant},
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    r#type::{self, SubFunctionSignatureLocation, Type},
    sub_term::{
        SubMemberSymbolLocation, SubSymbolLocation, SubTraitMemberLocation,
        SubTupleLocation,
    },
    Kind, Model, Never, Tuple,
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

/// Represents a mutable visitor that visits a term.
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
            + Recursive<'b, Lifetime<T::Model>>
            + Recursive<'b, Type<T::Model>>
            + Recursive<'b, Constant<T::Model>>,
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
            + MutableRecursive<Lifetime<T::Model>>
            + MutableRecursive<Type<T::Model>>
            + MutableRecursive<Constant<T::Model>>,
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
        V: Visitor<'a, Lifetime<Self::Model>>
            + Visitor<'a, Type<Self::Model>>
            + Visitor<'a, Constant<Self::Model>>,
    >(
        &'a self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Similar to [`Element::accept_single()`], but for mutable references.
    fn accept_single_mut<
        V: Mutable<Lifetime<Self::Model>>
            + Mutable<Type<Self::Model>>
            + Mutable<Constant<Self::Model>>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Invokes the [`Recursive`] visitor on the term itself.
    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime<Self::Model>>
            + Recursive<'a, Type<Self::Model>>
            + Recursive<'a, Constant<Self::Model>>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool;

    /// Similar to [`Element::accept_single_recursive()`], but for mutable
    /// references.
    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime<Self::Model>>
            + MutableRecursive<Type<Self::Model>>
            + MutableRecursive<Constant<Self::Model>>,
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
        V: Visitor<'a, Lifetime<Self::Model>>
            + Visitor<'a, Type<Self::Model>>
            + Visitor<'a, Constant<Self::Model>>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;

    /// Similar to [`Element::accept_one_level()`], but for mutable references.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level_mut<
        V: Mutable<Lifetime<Self::Model>>
            + Mutable<Type<Self::Model>>
            + Mutable<Constant<Self::Model>>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;
}

/// Invokes the visitor on the term itself and all of its sub-terms recursively.
pub fn accept_recursive<
    'a,
    M: Model,
    V: Recursive<'a, Lifetime<M>>
        + Recursive<'a, Type<M>>
        + Recursive<'a, Constant<M>>,
    E: Element<Model = M>,
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
    M: Model,
    V: MutableRecursive<Lifetime<M>>
        + MutableRecursive<Type<M>>
        + MutableRecursive<Constant<M>>,
    E: Element<Model = M>,
>(
    element: &mut E,
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
            if !element.term.$accept_single(
                $visitor,
                T::Location::from(SubTupleLocation::Single(idx).into()),
            ) {
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
        V: Visitor<'a, Lifetime<T::Model>>
            + Visitor<'a, Type<T::Model>>
            + Visitor<'a, Constant<T::Model>>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> bool {
        implements_tuple!(self, visitor, accept_single, iter)
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime<T::Model>>
            + Mutable<Type<T::Model>>
            + Mutable<Constant<T::Model>>,
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

impl<M: Model> GenericArguments<M> {
    #[allow(clippy::trait_duplication_in_bounds)]
    fn accept_one_level<
        'a,
        T: Element<Model = M>,
        Idx,
        V: Visitor<'a, Lifetime<M>>
            + Visitor<'a, Type<M>>
            + Visitor<'a, Constant<M>>,
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
    fn accept_one_level_mut<
        T: Element<Model = M>,
        Idx,
        V: Mutable<Lifetime<M>> + Mutable<Type<M>> + Mutable<Constant<M>>,
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
            Self::Error(_) | Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
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
            Self::MemberSymbol(member_symbol) => Ok(
                member_symbol
                    .parent_generic_arguments
                    .$accept_one_level::<Self, _, _>(
                        $visitor,
                        |id| SubMemberSymbolLocation {
                            index: id,
                            from_parent: true,
                        }
                    )
                    && member_symbol
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>(
                            $visitor,
                            |id| SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            }
                        ),
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
                    )
                    && term
                        .0
                        .member_generic_arguments
                        .$accept_one_level::<Self, _, _>(
                            $visitor,
                            |id| SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: id,
                                from_parent: false,
                            })
                        ),
            ),

            Self::FunctionSignature(term) => {
                for (idx, parameter) in (& $($ref)? term.parameters).into_iter().enumerate() {
                    if !$visitor.$visit_type(
                        parameter,
                        SubTypeLocation::FromType(
                            r#type::SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::Parameter(idx)
                            )
                        )
                    ) {
                        return Ok(false);
                    }
                }

                Ok(
                    $visitor.$visit_type(
                        &$($ref)?*term.return_type,
                        SubTypeLocation::FromType(
                            r#type::SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::ReturnType
                            )
                        )
                    )
                )
            },
        }
    };
}

impl<M: Model> Element for Type<M> {
    type Location = SubTypeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime<M>> + Visitor<'a, Self> + Visitor<'a, Constant<M>>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Lifetime<M>> + Mutable<Self> + Mutable<Constant<M>>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime<M>>
            + Recursive<'a, Self>
            + Recursive<'a, Constant<M>>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime<M>>
            + MutableRecursive<Self>
            + MutableRecursive<Constant<M>>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime<M>> + Visitor<'a, Self> + Visitor<'a, Constant<M>>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_type!(self, visitor, visit, visit, visit, accept_one_level)
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime<M>> + Mutable<Self> + Mutable<Constant<M>>,
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

impl<M: Model> Element for Lifetime<M> {
    type Location = SubLifetimeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Self> + Visitor<'a, Type<M>> + Visitor<'a, Constant<M>>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Self> + Mutable<Type<M>> + Mutable<Constant<M>>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Self>
            + Recursive<'a, Type<M>>
            + Recursive<'a, Constant<M>>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Self>
            + MutableRecursive<Type<M>>
            + MutableRecursive<Constant<M>>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Self> + Visitor<'a, Type<M>> + Visitor<'a, Constant<M>>,
    >(
        &'a self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_one_level_mut<
        V: Mutable<Self> + Mutable<Type<M>> + Mutable<Constant<M>>,
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
        }
    };
}

impl<M: Model> Element for Constant<M> {
    type Location = SubConstantLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime<M>> + Visitor<'a, Type<M>> + Visitor<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        'a,
        V: Mutable<Lifetime<M>> + Mutable<Type<M>> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime<M>>
            + Recursive<'a, Type<M>>
            + Recursive<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime<M>>
            + MutableRecursive<Type<M>>
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
        V: Visitor<'a, Lifetime<M>> + Visitor<'a, Type<M>> + Visitor<'a, Self>,
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
            iter
        )
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime<M>> + Mutable<Type<M>> + Mutable<Self>,
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

type Storage<'a, M> = Vec<(Kind<'a, M>, Vec<TermLocation>)>;

#[derive(Debug, Clone)]
struct Collector<'a, M: Model> {
    terms: Storage<'a, M>,
}

impl<'a, T: 'a, M: Model> Recursive<'a, T> for Collector<'a, M>
where
    &'a T: Into<Kind<'a, M>>,
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
pub struct RecursiveIterator<'a, M: Model>(
    <Storage<'a, M> as IntoIterator>::IntoIter,
);

impl<'a, M: Model> RecursiveIterator<'a, M> {
    /// Creates a new recursive iterator from a term.
    pub fn new<T: Element<Model = M>>(term: &'a T) -> Self {
        let mut collector = Collector { terms: Storage::new() };
        accept_recursive(term, &mut collector);

        Self(collector.terms.into_iter())
    }
}

impl<'a, M: Model> Iterator for RecursiveIterator<'a, M> {
    type Item = (Kind<'a, M>, Vec<TermLocation>);

    fn next(&mut self) -> Option<Self::Item> { self.0.next() }
}
