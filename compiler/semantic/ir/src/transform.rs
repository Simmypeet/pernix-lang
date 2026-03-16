//! Defines the trait [`Transformable`]; an object primarily used for
//! transforming the inference variables in the IR to concrete types after
//! type inference has been completed.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_term::{
    self, TermMut,
    generic_arguments::{AssociatedSymbol, Symbol},
    instance::InstanceAssociated,
    lifetime::Lifetime,
    r#type::Type,
};

#[derive(Debug)]
pub enum ResolutionMut<'x> {
    Symbol(&'x mut Symbol),
    Variant(&'x mut pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'x mut AssociatedSymbol),
    InstanceAssociated(&'x mut InstanceAssociated),
    Type(&'x mut Type),
    Lifetime(&'x mut Lifetime),
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
pub trait Transformer {
    /// Transforms the given term `term`, using the provided `source` for error
    /// reporting if necessary.
    #[allow(async_fn_in_trait)]
    async fn transform(
        &mut self,
        resolution: ResolutionMut<'_>,
        span: RelativeSpan,
    );
}

/// A trait for an object that can have [`Transformable`] elements transformed
/// within it.
pub trait Element {
    /// Transforms the types, lifetimes, and constants in self using the given
    /// transformer.
    #[allow(async_fn_in_trait)]
    async fn transform<T: Transformer>(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    );
}
