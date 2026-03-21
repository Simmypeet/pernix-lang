use std::future::Future;

use crate::{
    generic_arguments::{AssociatedSymbol, Symbol},
    instance::InstanceAssociated,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{AsyncRecursive, Constant, Instance, Lifetime},
};

/// An enum representing different symbol element types with references.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolElement<'a> {
    Symbol(&'a Symbol),
    AssociatedSymbol(&'a AssociatedSymbol),
    InstanceAssociated(&'a InstanceAssociated),
}

/// A visitor trait that visits symbol elements when they appear in Type or
/// Instance terms.
///
/// This trait uses `AsyncRecursive` as the adaptor and invokes the visitor
/// when Type or Instance have one of the variants defined in `SymbolElement`.
pub trait AsyncRecursiveSymbolVisitor: Send {
    /// Visits a symbol element.
    ///
    /// # Parameters
    ///
    /// - `symbol`: The symbol element to visit.
    /// - `locations`: The iterator of all the locations taken to reach the
    ///   current symbol element. The last element of the iterator is the
    ///   location of the current term containing the symbol. If the iterator is
    ///   empty, then the symbol is in the root term.
    ///
    /// # Returns
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms.
    fn visit_symbol(
        &mut self,
        symbol: SymbolElement<'_>,
        locations: impl Iterator<Item = TermLocation> + Send,
    ) -> impl Future<Output = bool> + Send;
}

#[derive(Debug)]
struct AsyncRecursiveSymbolVisitorAdapter<'a, V> {
    visitor: &'a mut V,
}

impl<V: AsyncRecursiveSymbolVisitor> AsyncRecursive<Type>
    for AsyncRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        term: &Type,
        locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Type::Symbol(symbol) => {
                self.visitor
                    .visit_symbol(SymbolElement::Symbol(symbol), locations)
                    .await
            }
            Type::AssociatedSymbol(associated_symbol) => {
                self.visitor
                    .visit_symbol(
                        SymbolElement::AssociatedSymbol(associated_symbol),
                        locations,
                    )
                    .await
            }
            Type::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol(
                        SymbolElement::InstanceAssociated(instance_associated),
                        locations,
                    )
                    .await
            }
            _ => true,
        }
    }
}

impl<V: AsyncRecursiveSymbolVisitor> AsyncRecursive<Instance>
    for AsyncRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        term: &Instance,
        locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Instance::Symbol(symbol) => {
                self.visitor
                    .visit_symbol(SymbolElement::Symbol(symbol), locations)
                    .await
            }
            Instance::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol(
                        SymbolElement::InstanceAssociated(instance_associated),
                        locations,
                    )
                    .await
            }
            _ => true,
        }
    }
}

impl<V: AsyncRecursiveSymbolVisitor> AsyncRecursive<Lifetime>
    for AsyncRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        _term: &Lifetime,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        true
    }
}

impl<V: AsyncRecursiveSymbolVisitor> AsyncRecursive<Constant>
    for AsyncRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        _term: &Constant,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        true
    }
}

/// Accepts a term and visits all symbol elements within it using an
/// `AsyncRecursiveSymbolVisitor`.
///
/// This function uses the `AsyncRecursive` adaptor to traverse Type and
/// Instance terms and invoke the visitor when encountering [`Symbol`],
/// [`AssociatedSymbol`], or [`InstanceAssociated`] variants.
pub async fn accept_symbol_recursive_async<
    V: AsyncRecursiveSymbolVisitor,
    E: crate::visitor::Element,
>(
    element: &E,
    visitor: &mut V,
) -> bool {
    let mut adapter = AsyncRecursiveSymbolVisitorAdapter { visitor };
    crate::visitor::accept_recursive_async(element, &mut adapter).await
}
