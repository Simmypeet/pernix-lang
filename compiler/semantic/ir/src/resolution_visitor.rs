//! Defines the trait [`ResolutionVisitable`]; an object primarily used for
//! transforming the inference variables in the IR to concrete types after
//! type inference has been completed.

use std::{fmt::Write, ops::Deref};

use derive_more::From;
use pernixc_extend::extend;
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

#[derive(Debug, From)]
pub enum ResolutionMut<'x> {
    Symbol(&'x mut Symbol),
    Variant(&'x mut pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'x mut AssociatedSymbol),
    InstanceAssociated(&'x mut InstanceAssociated),
    Type(&'x mut Type),
    Lifetime(&'x mut Lifetime),
}

#[derive(Debug, Clone, Copy, From)]
pub enum Resolution<'x> {
    Symbol(&'x Symbol),
    Variant(&'x pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'x AssociatedSymbol),
    InstanceAssociated(&'x InstanceAssociated),
    Type(&'x Type),
    Lifetime(&'x Lifetime),
}

#[derive(Debug, Clone, Copy, From)]
pub enum SymbolicResolution<'a> {
    Symbol(&'a Symbol),
    Variant(&'a pernixc_resolution::qualified_identifier::Variant),
    AssociatedSymbol(&'a AssociatedSymbol),
    InstanceAssociated(&'a InstanceAssociated),
}

/// Extension method for creating a well-formedness check obligation for a
/// [`Variant`].
///
/// The obligation is for the parent enum, not the variant itself.
#[extend]
pub async fn create_wf_check_obligation<'a>(
    self: &'a pernixc_resolution::qualified_identifier::Variant,
    engine: &pernixc_qbice::TrackedEngine,
) -> pernixc_type_system::wf_check::WfCheckObligation<'a> {
    let parent_enum_id = self.parent_enum_id(engine).await;
    pernixc_type_system::wf_check::WfCheckObligation::new(
        parent_enum_id,
        self.enum_generic_arguments().instances(),
    )
}

impl SymbolicResolution<'_> {
    /// Creates an instantiation for this symbolic resolution.
    ///
    /// Returns `None` if the instantiation creation fails (e.g., for
    /// `InstanceAssociated` when the trait reference lookup fails).
    pub async fn create_instantiation(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
    ) -> Option<pernixc_term::instantiation::Instantiation> {
        match self {
            Self::Symbol(sym) => Some(sym.create_instantiation(engine).await),
            Self::Variant(variant) => {
                Some(variant.create_instantiation(engine).await)
            }
            Self::AssociatedSymbol(assoc) => {
                Some(assoc.create_instantiation(engine).await)
            }
            Self::InstanceAssociated(inst) => {
                // Use the extension method which can fail
                use pernixc_semantic_element::trait_ref::create_instantiation;
                inst.create_instantiation(engine).await
            }
        }
    }

    /// Returns the well-formedness check obligations for this symbolic
    /// resolution.
    ///
    /// Each obligation contains a symbol ID and the instances that need to be
    /// checked.
    ///
    /// - For `Symbol`: returns the symbol's own ID with its instances.
    /// - For `Variant`: returns the parent enum ID with the variant's
    ///   instances.
    /// - For `AssociatedSymbol`: returns both the symbol ID with member
    ///   instances and parent ID with parent instances.
    /// - For `InstanceAssociated`: returns the trait associated symbol ID with
    ///   its instances.
    pub async fn get_wf_check_obligations(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
    ) -> impl Iterator<Item = pernixc_type_system::wf_check::WfCheckObligation<'_>>
    {
        use pernixc_type_system::wf_check::{
            CreateWfCheckObligation, create_wf_check_obligation_for_member,
            create_wf_check_obligation_for_parent,
        };

        match self {
            Self::Symbol(sym) => WfCheckObligationsIterator::Single(
                std::iter::once(sym.create_wf_check_obligation()),
            ),
            Self::Variant(variant) => {
                WfCheckObligationsIterator::Single(std::iter::once(
                    variant.create_wf_check_obligation(engine).await,
                ))
            }
            Self::AssociatedSymbol(assoc) => {
                WfCheckObligationsIterator::Double(
                    std::iter::once(
                        assoc.create_wf_check_obligation_for_member(),
                    )
                    .chain(std::iter::once(
                        assoc
                            .create_wf_check_obligation_for_parent(engine)
                            .await,
                    )),
                )
            }
            Self::InstanceAssociated(inst) => {
                WfCheckObligationsIterator::Single(std::iter::once(
                    inst.create_wf_check_obligation(),
                ))
            }
        }
    }
}

/// Iterator over well-formedness check obligations.
///
/// This enum is used to avoid heap allocations when iterating over obligations.
enum WfCheckObligationsIterator<'a> {
    Single(
        std::iter::Once<pernixc_type_system::wf_check::WfCheckObligation<'a>>,
    ),
    Double(
        std::iter::Chain<
            std::iter::Once<
                pernixc_type_system::wf_check::WfCheckObligation<'a>,
            >,
            std::iter::Once<
                pernixc_type_system::wf_check::WfCheckObligation<'a>,
            >,
        >,
    ),
}

impl<'a> Iterator for WfCheckObligationsIterator<'a> {
    type Item = pernixc_type_system::wf_check::WfCheckObligation<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Single(iter) => iter.next(),
            Self::Double(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Single(_) => (1, Some(1)),
            Self::Double(_) => (2, Some(2)),
        }
    }
}

impl ExactSizeIterator for WfCheckObligationsIterator<'_> {
    fn len(&self) -> usize {
        match self {
            Self::Single(_) => 1,
            Self::Double(_) => 2,
        }
    }
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

/// A trait for recursively inspecting symbolic resolutions within terms.
pub trait RecursiveSymbolicResolutionVisitor: Send {
    /// Visits the given symbolic resolution `resolution`, using the provided
    /// `span` for error reporting if necessary.
    ///
    /// Returns `Err(Abort)` to short-circuit the entire visitation process.
    fn visit<'s, 'e>(
        &'s mut self,
        resolution: SymbolicResolution<'e>,
        span: RelativeSpan,
    ) -> impl std::future::Future<Output = Result<(), Abort>>
    + Send
    + use<'s, 'e, Self>;
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

/// Accepts a recursive symbolic resolution visitor on a visitable element.
///
/// This function traverses all resolutions in the given element, and for each
/// resolution that contains symbolic elements (`Symbol`, `AssociatedSymbol`,
/// `InstanceAssociated`, or `Variant`), it recursively visits those symbols
/// by delegating to `pernixc_term::accept_symbol_recursive_async`.
pub async fn accept_recursive_symbolic_resolution_visitor<
    E: ResolutionVisitable,
    V: RecursiveSymbolicResolutionVisitor,
>(
    element: &E,
    visitor: &mut V,
) -> Result<(), Abort> {
    /// Internal adapter that implements `AsyncRecursiveSymbolVisitor` to bridge
    /// to `RecursiveSymbolicResolutionVisitor`.
    struct Adapter<'a, V> {
        visitor: &'a mut V,
        span: RelativeSpan,
    }

    impl<V: RecursiveSymbolicResolutionVisitor>
        pernixc_term::visitor::symbol::AsyncRecursiveSymbolVisitor
        for Adapter<'_, V>
    {
        async fn visit_symbol(
            &mut self,
            symbol_element: pernixc_term::visitor::symbol::SymbolElement<'_>,
        ) -> bool {
            // Convert SymbolElement to SymbolicResolution
            let symbolic_resolution = match symbol_element {
                pernixc_term::visitor::symbol::SymbolElement::Symbol(sym) => {
                    SymbolicResolution::Symbol(sym)
                }
                pernixc_term::visitor::symbol::SymbolElement::AssociatedSymbol(
                    assoc,
                ) => SymbolicResolution::AssociatedSymbol(assoc),
                pernixc_term::visitor::symbol::SymbolElement::InstanceAssociated(
                    inst,
                ) => SymbolicResolution::InstanceAssociated(inst),
            };

            // Convert Result<(), Abort> to bool
            // Ok(()) => true (continue visiting)
            // Err(Abort) => false (stop visiting)
            self.visitor.visit(symbolic_resolution, self.span).await.is_ok()
        }
    }

    /// Internal visitor that collects resolutions and delegates to the symbol
    /// visitor.
    struct ResolutionCollector<'a, V> {
        visitor: &'a mut V,
    }

    impl<V: RecursiveSymbolicResolutionVisitor> ResolutionVisitor
        for ResolutionCollector<'_, V>
    {
        async fn visit(
            &mut self,
            resolution: Resolution<'_>,
            span: RelativeSpan,
        ) -> Result<(), Abort> {
            // Visit all terms within the resolution and delegate symbol
            // visitation to our adapter
            let mut adapter = Adapter { visitor: self.visitor, span };

            // Iterate over all terms in the resolution
            for term in resolution.iter_all_term() {
                match term {
                    TermRef::Type(ty) => {
                        pernixc_term::visitor::symbol::accept_symbol_recursive_async(
                            ty,
                            &mut adapter,
                        )
                        .await;
                    }
                    TermRef::Instance(inst) => {
                        pernixc_term::visitor::symbol::accept_symbol_recursive_async(
                            inst,
                            &mut adapter,
                        )
                        .await;
                    }
                    TermRef::Lifetime(_) | TermRef::Constant(_) => {
                        // Lifetimes and Constants don't contain symbols
                    }
                }
            }

            // Finally, match the Resolution itself and convert to
            // SymbolicResolution if applicable
            match resolution {
                Resolution::Symbol(sym) => {
                    self.visitor
                        .visit(SymbolicResolution::Symbol(sym), span)
                        .await?;
                }
                Resolution::Variant(variant) => {
                    self.visitor
                        .visit(SymbolicResolution::Variant(variant), span)
                        .await?;
                }
                Resolution::AssociatedSymbol(assoc) => {
                    self.visitor
                        .visit(
                            SymbolicResolution::AssociatedSymbol(assoc),
                            span,
                        )
                        .await?;
                }
                Resolution::InstanceAssociated(inst) => {
                    self.visitor
                        .visit(
                            SymbolicResolution::InstanceAssociated(inst),
                            span,
                        )
                        .await?;
                }
                Resolution::Type(_) | Resolution::Lifetime(_) => {
                    // Type and Lifetime are not symbolic resolutions
                }
            }

            Ok(())
        }
    }

    let mut collector = ResolutionCollector { visitor };
    element.accept(&mut collector).await
}

/// An adaptor struct that implements [`ResolutionVisitor`] for any type that
/// can be converted into a [`Resolution`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntoResolutionWithSpan<'a, T> {
    into_resolution: &'a T,
    span: RelativeSpan,
}

impl<'a, T> IntoResolutionWithSpan<'a, T> {
    /// Creates a new `IntoResolutionWithSpan` adaptor.
    #[must_use]
    pub const fn new(into_resolution: &'a T, span: RelativeSpan) -> Self {
        Self { into_resolution, span }
    }
}

impl<'a, T> ResolutionVisitable for IntoResolutionWithSpan<'a, T>
where
    &'a T: Into<Resolution<'a>>,
{
    async fn accept<V: ResolutionVisitor>(
        &self,
        visitor: &mut V,
    ) -> Result<(), Abort> {
        let resolution = self.into_resolution.into();
        visitor.visit(resolution, self.span).await
    }
}
