//! Implements the visitor patsern for semantic terms.

use std::{fmt::Debug, future::Future, hash::Hash};

use super::sub_term::{
    SubConstantLocation, SubLifetimeLocation, SubTerm, SubTypeLocation,
    TermLocation,
};
use crate::{
    Never, TermRef,
    constant::{self, Constant},
    generic_arguments::SubGenericArgumentsLocation,
    instance::{
        self, Instance, SubInstanceAssociatedGenericArgsLocation,
        SubInstanceAssociatedLocation,
    },
    lifetime::Lifetime,
    sub_term::SubInstanceLocation,
    r#type::{
        self, SubFunctionSignatureLocation,
        SubInstanceAssociatedInstanceLocation, Type,
    },
};

pub mod symbol;

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
pub trait AsyncVisitor<T: Element>: Send {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    fn visit(
        &mut self,
        term: &T,
        location: T::Location,
    ) -> impl Future<Output = bool> + Send;
}

/// An asynchronous version of the [`Recursive`] trait.
pub trait AsyncRecursive<T: Element>: Send {
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
        term: &T,
        locations: impl Iterator<Item = TermLocation> + Send,
    ) -> impl Future<Output = bool> + Send;
}

/// An asynchronous version of the [`MutableRecursive`] trait.
pub trait AsyncMutableRecursive<T: Element>: Send {
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
        locations: impl Iterator<Item = TermLocation> + Send,
    ) -> impl Future<Output = bool> + Send;
}

/// An asynchronous version of the [`Mutable`] trait.
pub trait AsyncMutable<T: Element>: Send {
    /// Visits a term.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of
    /// the term.
    fn visit(
        &mut self,
        term: &mut T,
        location: T::Location,
    ) -> impl Future<Output = bool> + Send;
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
    ($self:ident, $term:ident, $location:ident, $visit:expr $(, $await:ident)?) => {{
        $self.current_locations.push($location.into());

        let locations =
            $self.current_locations.iter().copied().collect::<Vec<_>>();

        match $visit {
            Ok(result) => {
                if !result {
                    return false;
                }
            }
            Err(VisitNonApplicationTermError) => {}
        }

        $self.current_locations.pop();

        $self.visitor.visit($term, locations.into_iter()) $(.$await)?
    }};
}

impl<
    'b,
    T: Element,
    V: Recursive<'b, T>
        + Recursive<'b, Lifetime>
        + Recursive<'b, Type>
        + Recursive<'b, Constant>
        + Recursive<'b, Instance>,
> Visitor<'b, T> for RecursiveVisitorAdapter<'_, V>
{
    fn visit(&mut self, term: &'b T, location: T::Location) -> bool {
        implements_visit_recursive!(
            self,
            term,
            location,
            term.accept_one_level(self)
        )
    }
}

#[derive(Debug)]
pub struct RecursiveVisitorAdapterAsync<'a, V> {
    visitor: &'a mut V,
    current_locations: Vec<TermLocation>,
}

impl<
    T: Element + Sync,
    V: AsyncRecursive<T>
        + AsyncRecursive<Lifetime>
        + AsyncRecursive<Type>
        + AsyncRecursive<Constant>
        + AsyncRecursive<Instance>,
> AsyncVisitor<T> for RecursiveVisitorAdapterAsync<'_, V>
{
    async fn visit(
        &mut self,
        term: &T,
        location: <T as Element>::Location,
    ) -> bool {
        implements_visit_recursive!(
            self,
            term,
            location,
            Box::pin(term.accept_one_level_async(self)).await,
            await
        )
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
        + MutableRecursive<Constant>
        + MutableRecursive<Instance>,
> Mutable<T> for RecursiveVisitorAdapterMut<'_, V>
{
    fn visit(&mut self, term: &mut T, location: T::Location) -> bool {
        implements_visit_recursive!(
            self,
            term,
            location,
            term.accept_one_level_mut(self)
        )
    }
}

#[derive(Debug)]
pub struct RecursiveVisitorAdapterAsyncMut<'a, V> {
    visitor: &'a mut V,
    current_locations: Vec<TermLocation>,
}

impl<
    T: Element,
    V: AsyncMutableRecursive<T>
        + AsyncMutableRecursive<Lifetime>
        + AsyncMutableRecursive<Type>
        + AsyncMutableRecursive<Constant>
        + AsyncMutableRecursive<Instance>,
> AsyncMutable<T> for RecursiveVisitorAdapterAsyncMut<'_, V>
{
    async fn visit(
        &mut self,
        term: &mut T,
        location: <T as Element>::Location,
    ) -> bool {
        implements_visit_recursive!(
            self,
            term,
            location,
            Box::pin(term.accept_one_level_async_mut(self)).await,
            await
        )
    }
}

/// A term for the visitor pattern.
pub trait Element: Send + Sized + SubTerm {
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
        + Send
        + From<Self::ThisSubTermLocation>;

    /// Invokes the visitor on the term itself once.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
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
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a;

    /// Similar to [`Element::accept_single()`], but for mutable references.
    fn accept_single_mut<
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool;

    /// Similar to [`Element::accept_single_mut()`], but for asynchronous
    /// visitors.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single_async_mut<
        'a,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a;

    /// Invokes the [`Recursive`] visitor on the term itself.
    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime>
            + Recursive<'a, Type>
            + Recursive<'a, Constant>
            + Recursive<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool;

    /// Similar to [`Element::accept_single_recursive()`], but for
    /// asynchronous visitors.
    fn accept_single_recursive_async<
        'a,
        V: AsyncRecursive<Lifetime>
            + AsyncRecursive<Type>
            + AsyncRecursive<Constant>
            + AsyncRecursive<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> impl Future<Output = bool> + Send + 'a;

    /// Similar to [`Element::accept_single_recursive()`], but for mutable
    /// references.
    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>
            + MutableRecursive<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool;

    /// Similar to [`Element::accept_single_recursive()`], but for asynchronous
    /// visitors.
    fn accept_single_recursive_mut_async<
        'a,
        V: AsyncMutableRecursive<Lifetime>
            + AsyncMutableRecursive<Type>
            + AsyncMutableRecursive<Constant>
            + AsyncMutableRecursive<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> impl Future<Output = bool> + Send + 'a;

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
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;

    /// Similar to [`Element::accept_one_level()`], but for asynchronous
    /// visitors.
    fn accept_one_level_async<
        'a,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
    ) -> impl Future<Output = Result<bool, VisitNonApplicationTermError>> + Send + 'a;

    /// Similar to [`Element::accept_one_level_mut()`], but for asynchronous
    /// visitors.
    fn accept_one_level_async_mut<
        'a,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
    ) -> impl Future<Output = Result<bool, VisitNonApplicationTermError>> + Send + 'a;

    /// Similar to [`Element::accept_one_level()`], but for mutable references.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level_mut<
        V: Mutable<Lifetime>
            + Mutable<Type>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError>;
}

/// Invokes the visitor on the term itself and all of its sub-terms recursively.
pub fn accept_recursive<
    'a,
    V: Recursive<'a, Lifetime>
        + Recursive<'a, Type>
        + Recursive<'a, Constant>
        + Recursive<'a, Instance>,
    E: Element,
>(
    element: &'a E,
    visitor: &mut V,
) -> bool {
    let mut adapter =
        RecursiveVisitorAdapter { visitor, current_locations: Vec::new() };

    if let Ok(result) = element.accept_one_level(&mut adapter)
        && !result
    {
        return false;
    }

    !element.accept_single_recursive(visitor, std::iter::empty())
}

/// Similar to [`accept_recursive()`], but for asynchronous visitors.
pub async fn accept_recursive_async<
    V: AsyncRecursive<Lifetime>
        + AsyncRecursive<Type>
        + AsyncRecursive<Constant>
        + AsyncRecursive<Instance>,
    E: Element,
>(
    element: &E,
    visitor: &mut V,
) -> bool {
    if !element.accept_single_recursive_async(visitor, std::iter::empty()).await
    {
        return false;
    }

    let mut adapter =
        RecursiveVisitorAdapterAsync { visitor, current_locations: Vec::new() };

    element.accept_one_level_async(&mut adapter).await.unwrap_or(true)
}

/// Similar to [`accept_recursive()`], but for mutable references.
pub fn accept_recursive_mut<
    V: MutableRecursive<Lifetime>
        + MutableRecursive<Type>
        + MutableRecursive<Constant>
        + MutableRecursive<Instance>,
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

/// Similar to [`accept_recursive()`], but for mutable references.
pub async fn accept_recursive_mut_async<
    V: AsyncMutableRecursive<Lifetime>
        + AsyncMutableRecursive<Type>
        + AsyncMutableRecursive<Constant>
        + AsyncMutableRecursive<Instance>,
    E: Element,
>(
    element: &mut E,
    visitor: &mut V,
) -> bool {
    if !element
        .accept_single_recursive_mut_async(visitor, std::iter::empty())
        .await
    {
        return false;
    }

    let mut adapter = RecursiveVisitorAdapterAsyncMut {
        visitor,
        current_locations: Vec::new(),
    };

    element.accept_one_level_async_mut(&mut adapter).await.unwrap_or(true)
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
                Ok(term.$accept_one_level::<Self, _>(
                    $visitor,
                )$(.$await)?)
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
            Self::AssociatedSymbol(member_symbol) => Ok(
                member_symbol.$accept_one_level::<Self, _>(
                    $visitor,
                )$(.$await)?
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
            Self::InstanceAssociated(term) => {
                Ok(term.$accept_one_level::<Self, _, _, _>(
                    $visitor,
                    |idx| SubInstanceAssociatedGenericArgsLocation::new(idx),
                    r#type::SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedInstanceLocation::Instance
                    ),
                )$(.$await)?)
            },
        }
    };
}

impl Element for Type {
    type Location = SubTypeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Self>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    async fn accept_single_async<
        'a,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Self>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location).await
    }

    fn accept_single_mut<
        V: Mutable<Lifetime>
            + Mutable<Self>
            + Mutable<Constant>
            + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubTypeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime>
            + Recursive<'a, Self>
            + Recursive<'a, Constant>
            + Recursive<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_async<
        'a,
        V: AsyncRecursive<Lifetime>
            + AsyncRecursive<Self>
            + AsyncRecursive<Constant>
            + AsyncRecursive<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Self>
            + MutableRecursive<Constant>
            + MutableRecursive<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_mut_async<
        'a,
        V: AsyncMutableRecursive<Lifetime>
            + AsyncMutableRecursive<Self>
            + AsyncMutableRecursive<Constant>
            + AsyncMutableRecursive<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Self>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
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
        V: Mutable<Lifetime>
            + Mutable<Self>
            + Mutable<Constant>
            + Mutable<Instance>,
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
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Self>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
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

    async fn accept_one_level_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Self>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
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
            accept_one_level_async_mut,
            [&mut],
            await
        )
    }

    async fn accept_single_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Self>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location).await
    }
}

impl From<Never> for SubLifetimeLocation {
    fn from(val: Never) -> Self { match val {} }
}

impl Element for Lifetime {
    type Location = SubLifetimeLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Self>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubLifetimeLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Self> + Mutable<Type> + Mutable<Constant> + Mutable<Instance>,
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
            + Recursive<'a, Type>
            + Recursive<'a, Constant>
            + Recursive<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_async<
        'a,
        V: AsyncRecursive<Self>
            + AsyncRecursive<Type>
            + AsyncRecursive<Constant>
            + AsyncRecursive<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Self>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>
            + MutableRecursive<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_mut_async<
        'a,
        V: AsyncMutableRecursive<Self>
            + AsyncMutableRecursive<Type>
            + AsyncMutableRecursive<Constant>
            + AsyncMutableRecursive<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Self>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_one_level_mut<
        V: Mutable<Self> + Mutable<Type> + Mutable<Constant> + Mutable<Instance>,
    >(
        &mut self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    fn accept_single_async<
        'a,
        V: AsyncVisitor<Self>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Self>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Instance>,
    >(
        &self,
        _: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }

    async fn accept_single_async_mut<
        V: AsyncMutable<Self>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location).await
    }

    async fn accept_one_level_async_mut<
        V: AsyncMutable<Self>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Instance>,
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
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Self>
            + Visitor<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        'a,
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Self> + Mutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: SubConstantLocation,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime>
            + Recursive<'a, Type>
            + Recursive<'a, Self>
            + Recursive<'a, Instance>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_async<
        'a,
        V: AsyncRecursive<Lifetime>
            + AsyncRecursive<Type>
            + AsyncRecursive<Self>
            + AsyncRecursive<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Self>
            + MutableRecursive<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_mut_async<
        'a,
        V: AsyncMutableRecursive<Lifetime>
            + AsyncMutableRecursive<Type>
            + AsyncMutableRecursive<Self>
            + AsyncMutableRecursive<Instance>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Self>
            + Visitor<'a, Instance>,
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
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Self> + Mutable<Instance>,
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
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Self>
            + AsyncVisitor<Instance>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Self>
            + AsyncVisitor<Instance>,
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

    async fn accept_single_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Self>
            + AsyncMutable<Instance>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location).await
    }

    async fn accept_one_level_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Self>
            + AsyncMutable<Instance>,
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
            accept_one_level_async_mut,
            as_mut,
            iter_mut,
            [&mut],
            await
        )
    }
}

macro_rules! implements_instance_ref {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident
        $(, $await:ident)?
    ) => {
        match $self {
            Self::AnonymousTrait(_)
            | Self::Inference(_)
            | Self::Parameter(_)
            | Self::Error(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                Ok(term.$accept_one_level::<Self, _>(
                    $visitor,
                )$(.$await)?)
            }

            Self::InstanceAssociated(term) => {
                Ok(term.$accept_one_level::<Self, _, _, _>(
                    $visitor,
                    |idx| SubGenericArgumentsLocation::new(idx),
                    instance::SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance
                    ),
                )$(.$await)?)
            }
        }
    };
}

macro_rules! implements_instance_mut {
    (
        $self:ident,
        $visitor:ident,
        $accept_one_level:ident
        $(, $await:ident)?
    ) => {
        match $self {
            Self::AnonymousTrait(_)
            | Self::Inference(_)
            | Self::Parameter(_)
            | Self::Error(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                Ok(term.$accept_one_level::<Self, _>(
                    $visitor,
                )$(.$await)?)
            }

            Self::InstanceAssociated(term) => {
                Ok(term.$accept_one_level::<Self, _, _, _>(
                    $visitor,
                    |idx| SubGenericArgumentsLocation::new(idx),
                    instance::SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedLocation::Instance
                    ),
                )$(.$await)?)
            }
        }
    };
}

impl Element for Instance {
    type Location = SubInstanceLocation;

    fn accept_single<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_async<
        'a,
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Self>,
    >(
        &'a self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    fn accept_single_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
        location: Self::Location,
    ) -> bool {
        visitor.visit(self, location)
    }

    fn accept_single_async_mut<
        'a,
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Self>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        location: Self::Location,
    ) -> impl Future<Output = bool> + 'a {
        visitor.visit(self, location)
    }

    fn accept_single_recursive<
        'a,
        V: Recursive<'a, Lifetime>
            + Recursive<'a, Type>
            + Recursive<'a, Constant>
            + Recursive<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_async<
        'a,
        V: AsyncRecursive<Lifetime>
            + AsyncRecursive<Type>
            + AsyncRecursive<Constant>
            + AsyncRecursive<Self>,
    >(
        &'a self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_single_recursive_mut<
        V: MutableRecursive<Lifetime>
            + MutableRecursive<Type>
            + MutableRecursive<Constant>
            + MutableRecursive<Self>,
    >(
        &mut self,
        visitor: &mut V,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        visitor.visit(self, locations)
    }

    async fn accept_single_recursive_mut_async<
        'a,
        V: AsyncMutableRecursive<Lifetime>
            + AsyncMutableRecursive<Type>
            + AsyncMutableRecursive<Constant>
            + AsyncMutableRecursive<Self>,
    >(
        &'a mut self,
        visitor: &'a mut V,
        locations: impl Iterator<Item = TermLocation> + Send + 'a,
    ) -> bool {
        visitor.visit(self, locations).await
    }

    fn accept_one_level<
        'a,
        V: Visitor<'a, Lifetime>
            + Visitor<'a, Type>
            + Visitor<'a, Constant>
            + Visitor<'a, Self>,
    >(
        &'a self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_instance_ref!(self, visitor, accept_one_level)
    }

    async fn accept_one_level_async<
        V: AsyncVisitor<Lifetime>
            + AsyncVisitor<Type>
            + AsyncVisitor<Constant>
            + AsyncVisitor<Self>,
    >(
        &self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_instance_ref!(self, visitor, accept_one_level_async, await)
    }

    async fn accept_one_level_async_mut<
        V: AsyncMutable<Lifetime>
            + AsyncMutable<Type>
            + AsyncMutable<Constant>
            + AsyncMutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_instance_mut!(
            self,
            visitor,
            accept_one_level_async_mut,
            await
        )
    }

    fn accept_one_level_mut<
        V: Mutable<Lifetime> + Mutable<Type> + Mutable<Constant> + Mutable<Self>,
    >(
        &mut self,
        visitor: &mut V,
    ) -> Result<bool, VisitNonApplicationTermError> {
        implements_instance_mut!(self, visitor, accept_one_level_mut)
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

pub use symbol::{
    AsyncRecursiveSymbolVisitor, SymbolElement, accept_symbol_recursive_async,
};
