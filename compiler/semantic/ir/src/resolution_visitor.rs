//! Defines the trait [`ResolutionVisitable`]; an object primarily used for
//! transforming the inference variables in the IR to concrete types after
//! type inference has been completed.

use std::{fmt::Write, ops::Deref};

use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    implements::get_implements, implements_arguments::get_implements_argument,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    name::get_name,
    parent::get_parent_global,
};
use pernixc_term::{
    self, TermMut, TermRef, display,
    generic_arguments::{
        AssociatedSymbol, DisplaySymbolWithGenericArguments, Symbol,
    },
    instance::InstanceAssociated,
    lifetime::Lifetime,
    r#type::Type,
};
use qbice::{Decode, Encode, StableHash};

/// A type representing visitation abort, used to short-circuit the entire
/// visitation process.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Abort;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum ResolutionOwned {
    Symbol(Symbol),
    Variant(pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(AssociatedSymbol),
    InstanceAssociated(InstanceAssociated),
    Type(Type),
    Lifetime(Lifetime),
}

#[derive(Debug)]
pub enum ResolutionMut<'x> {
    Symbol(&'x mut Symbol),
    Variant(&'x mut pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'x mut AssociatedSymbol),
    InstanceAssociated(&'x mut InstanceAssociated),
    Type(&'x mut Type),
    Lifetime(&'x mut Lifetime),
}

#[derive(Debug, Clone, Copy)]
pub enum Resolution<'x> {
    Symbol(&'x Symbol),
    Variant(&'x pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'x AssociatedSymbol),
    InstanceAssociated(&'x InstanceAssociated),
    Type(&'x Type),
    Lifetime(&'x Lifetime),
}

/// Alias for immutable resolution references.
pub type ResolutionRef<'x> = Resolution<'x>;

impl Resolution<'_> {
    #[must_use]
    pub fn to_owned(&self) -> ResolutionOwned {
        match self {
            Self::Symbol(symbol) => ResolutionOwned::Symbol((*symbol).clone()),
            Self::Variant(variant) => {
                ResolutionOwned::Variant((*variant).clone())
            }
            Self::AssociatedSymbol(associated_symbol) => {
                ResolutionOwned::AssociatedSymbol((*associated_symbol).clone())
            }
            Self::InstanceAssociated(instance_associated) => {
                ResolutionOwned::InstanceAssociated(
                    (*instance_associated).clone(),
                )
            }
            Self::Type(ty) => ResolutionOwned::Type((*ty).clone()),
            Self::Lifetime(lifetime) => {
                ResolutionOwned::Lifetime((*lifetime).clone())
            }
        }
    }

    /// Returns an iterator over immutable references to all sub-terms in the
    /// resolution.
    pub fn iter_all_term(&self) -> impl Iterator<Item = TermRef<'_>> {
        enum Six<A, B, C, D, E, F> {
            A(A),
            B(B),
            C(C),
            D(D),
            E(E),
            F(F),
        }

        impl<
            A: Iterator,
            B: Iterator<Item = A::Item>,
            C: Iterator<Item = A::Item>,
            D: Iterator<Item = A::Item>,
            E: Iterator<Item = A::Item>,
            F: Iterator<Item = A::Item>,
        > Iterator for Six<A, B, C, D, E, F>
        {
            type Item = A::Item;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::A(a) => a.next(),
                    Self::B(b) => b.next(),
                    Self::C(c) => c.next(),
                    Self::D(d) => d.next(),
                    Self::E(e) => e.next(),
                    Self::F(f) => f.next(),
                }
            }
        }

        match self {
            Self::Symbol(symbol) => Six::A(symbol.iter_all_term()),
            Self::Variant(variant) => Six::B(variant.iter_all_term()),
            Self::AssociatedSymbol(associated_symbol) => {
                Six::C(associated_symbol.iter_all_term())
            }
            Self::InstanceAssociated(instance_associated) => {
                Six::D(instance_associated.iter_all_term())
            }
            Self::Type(ty) => Six::E(std::iter::once(TermRef::Type(ty))),
            Self::Lifetime(lifetime) => {
                Six::F(std::iter::once(TermRef::Lifetime(lifetime)))
            }
        }
    }
}

impl ResolutionMut<'_> {
    #[must_use]
    pub const fn as_resolution_ref(&self) -> Resolution<'_> {
        match self {
            Self::Symbol(symbol) => Resolution::Symbol(symbol),
            Self::Variant(variant) => Resolution::Variant(variant),
            Self::AssociatedSymbol(associated_symbol) => {
                Resolution::AssociatedSymbol(associated_symbol)
            }
            Self::InstanceAssociated(instance_associated) => {
                Resolution::InstanceAssociated(instance_associated)
            }
            Self::Type(ty) => Resolution::Type(ty),
            Self::Lifetime(lifetime) => Resolution::Lifetime(lifetime),
        }
    }

    #[must_use]
    pub fn to_owned(&self) -> ResolutionOwned {
        match self {
            Self::Symbol(symbol) => ResolutionOwned::Symbol((*symbol).clone()),
            Self::Variant(variant) => {
                ResolutionOwned::Variant((*variant).clone())
            }
            Self::AssociatedSymbol(associated_symbol) => {
                ResolutionOwned::AssociatedSymbol((*associated_symbol).clone())
            }
            Self::InstanceAssociated(instance_associated) => {
                ResolutionOwned::InstanceAssociated(
                    (*instance_associated).clone(),
                )
            }
            Self::Type(ty) => ResolutionOwned::Type((*ty).clone()),
            Self::Lifetime(lifetime) => {
                ResolutionOwned::Lifetime((*lifetime).clone())
            }
        }
    }
}

impl display::Display for ResolutionOwned {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Symbol(symbol) => symbol.fmt(engine, formatter).await,
            Self::Variant(variant) => variant.fmt(engine, formatter).await,

            Self::AssociatedSymbol(associated_symbol) => {
                let parent = engine
                    .get_parent_global(associated_symbol.id())
                    .await
                    .unwrap();

                let parent_kind = engine.get_kind(parent).await;

                if parent_kind == Kind::PositiveImplementation
                    && let Some(implements_sym) =
                        engine.get_implements(parent).await
                    && let Some(mut generic_arguments) = engine
                        .get_implements_argument(parent)
                        .await
                        .map(|x| x.deref().clone())
                {
                    let inst = associated_symbol
                        .parent_generic_arguments()
                        .create_instantiation_for_generic_symbol(parent, engine)
                        .await;

                    generic_arguments.instantiate(&inst);

                    DisplaySymbolWithGenericArguments::new(
                        implements_sym,
                        &generic_arguments,
                    )
                    .fmt(engine, formatter)
                    .await?;
                } else {
                    DisplaySymbolWithGenericArguments::new(
                        parent,
                        associated_symbol.parent_generic_arguments(),
                    )
                    .fmt(engine, formatter)
                    .await?;
                }

                let name = engine.get_name(associated_symbol.id()).await;
                write!(formatter, "::{}", &*name)?;

                associated_symbol
                    .member_generic_arguments()
                    .fmt(engine, formatter)
                    .await
            }

            Self::InstanceAssociated(instance_associated) => {
                instance_associated.fmt(engine, formatter).await
            }
            Self::Type(ty) => ty.fmt(engine, formatter).await,
            Self::Lifetime(lifetime) => lifetime.fmt(engine, formatter).await,
        }
    }
}

impl ResolutionMut<'_> {
    /// Returns an iterator over mutable references to all sub-terms in the
    /// resolution.
    pub fn iter_all_term_mut(&mut self) -> impl Iterator<Item = TermMut<'_>> {
        enum Six<A, B, C, D, E, F> {
            A(A),
            B(B),
            C(C),
            D(D),
            E(E),
            F(F),
        }

        impl<
            A: Iterator,
            B: Iterator<Item = A::Item>,
            C: Iterator<Item = A::Item>,
            D: Iterator<Item = A::Item>,
            E: Iterator<Item = A::Item>,
            F: Iterator<Item = A::Item>,
        > Iterator for Six<A, B, C, D, E, F>
        {
            type Item = A::Item;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::A(a) => a.next(),
                    Self::B(b) => b.next(),
                    Self::C(c) => c.next(),
                    Self::D(d) => d.next(),
                    Self::E(e) => e.next(),
                    Self::F(f) => f.next(),
                }
            }
        }

        match self {
            Self::Symbol(symbol) => Six::A(symbol.iter_all_term_mut()),
            Self::Variant(variant) => Six::B(variant.iter_all_term_mut()),
            Self::AssociatedSymbol(associated_symbol) => {
                Six::C(associated_symbol.iter_all_term_mut())
            }
            Self::InstanceAssociated(instance_associated) => {
                Six::D(instance_associated.iter_all_term_mut())
            }
            Self::Type(ty) => Six::E(std::iter::once(TermMut::Type(ty))),
            Self::Lifetime(lifetime) => {
                Six::F(std::iter::once(TermMut::Lifetime(lifetime)))
            }
        }
    }
}

/// A trait for transforming the [`ResolutionMut`]s in an object
pub trait MutableResolutionVisitor {
    /// Visits the given term `term`, using the provided `source` for error
    /// reporting if necessary.
    ///
    /// Returns `Err(Abort)` to short-circuit the entire visitation process.
    #[allow(async_fn_in_trait)]
    async fn visit_mut(
        &mut self,
        resolution: ResolutionMut<'_>,
        span: RelativeSpan,
    ) -> Result<(), Abort>;
}

/// A trait for inspecting the [`Resolution`]s in an object.
pub trait ResolutionVisitor {
    /// Visits the given resolution `resolution`, using the provided `source`
    /// for error reporting if necessary.
    ///
    /// Returns `Err(Abort)` to short-circuit the entire visitation process.
    #[allow(async_fn_in_trait)]
    async fn visit(
        &mut self,
        resolution: Resolution<'_>,
        span: RelativeSpan,
    ) -> Result<(), Abort>;
}

/// A trait for an object that can have [`MutableResolutionVisitor`] elements
/// visited within it.
pub trait MutableResolutionVisitable {
    /// Visits the types, lifetimes, and constants in self using the given
    /// visitor.
    ///
    /// Returns `Err(Abort)` if the visitor aborts the visitation process.
    #[allow(async_fn_in_trait)]
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort>;
}

/// A trait for an object that can have [`ResolutionVisitor`] elements visited
/// within it.
pub trait ResolutionVisitable {
    /// Visits the types, lifetimes, and constants in self using the given
    /// visitor.
    ///
    /// Returns `Err(Abort)` if the visitor aborts the visitation process.
    #[allow(async_fn_in_trait)]
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort>;
}
