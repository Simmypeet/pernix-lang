use std::future::Future;

use crate::{
    generic_arguments::{AssociatedSymbol, Symbol},
    instance::InstanceAssociated,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{
        AsyncMutableRecursive, AsyncRecursive, Constant, Instance, Lifetime,
    },
};

/// An enum representing different symbol element types with references.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolElement<'a> {
    Symbol(&'a Symbol),
    AssociatedSymbol(&'a AssociatedSymbol),
    InstanceAssociated(&'a InstanceAssociated),
}

/// An enum representing different symbol element types with mutable references.
#[derive(Debug, PartialEq, Eq)]
pub enum SymbolElementMut<'a> {
    Symbol(&'a mut Symbol),
    AssociatedSymbol(&'a mut AssociatedSymbol),
    InstanceAssociated(&'a mut InstanceAssociated),
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
    ///
    /// # Returns
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms.
    fn visit_symbol<'s, 'e>(
        &'s mut self,
        symbol: SymbolElement<'e>,
    ) -> impl Future<Output = bool> + Send + use<'s, 'e, Self>;
}

/// A visitor trait that visits symbol elements mutably when they appear in
/// Type or Instance terms.
///
/// This trait uses `AsyncRecursiveMut` as the adaptor and invokes the visitor
/// when Type or Instance have one of the variants defined in
/// `SymbolElementMut`.
pub trait AsyncMutableRecursiveSymbolVisitor: Send {
    /// Visits a symbol element mutably.
    ///
    /// # Parameters
    ///
    /// - `symbol`: The mutable symbol element to visit.
    ///
    /// # Returns
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms.
    fn visit_symbol_mut<'s, 'e>(
        &'s mut self,
        symbol: SymbolElementMut<'e>,
    ) -> impl Future<Output = bool> + Send + use<'s, 'e, Self>;
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
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Type::Symbol(symbol) => {
                self.visitor.visit_symbol(SymbolElement::Symbol(symbol)).await
            }
            Type::AssociatedSymbol(associated_symbol) => {
                self.visitor
                    .visit_symbol(SymbolElement::AssociatedSymbol(
                        associated_symbol,
                    ))
                    .await
            }
            Type::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol(SymbolElement::InstanceAssociated(
                        instance_associated,
                    ))
                    .await
            }
            Type::Inference(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::FunctionSignature(_)
            | Type::Error(_) => true,
        }
    }
}

impl<V: AsyncRecursiveSymbolVisitor> AsyncRecursive<Instance>
    for AsyncRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        term: &Instance,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Instance::Symbol(symbol) => {
                self.visitor.visit_symbol(SymbolElement::Symbol(symbol)).await
            }
            Instance::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol(SymbolElement::InstanceAssociated(
                        instance_associated,
                    ))
                    .await
            }
            Instance::Parameter(_)
            | Instance::Inference(_)
            | Instance::AnonymousTrait(_)
            | Instance::Error(_) => true,
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

#[derive(Debug)]
struct AsyncMutableRecursiveSymbolVisitorAdapter<'a, V> {
    visitor: &'a mut V,
}

impl<V: AsyncMutableRecursiveSymbolVisitor> AsyncMutableRecursive<Type>
    for AsyncMutableRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        term: &mut Type,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Type::Symbol(symbol) => {
                self.visitor
                    .visit_symbol_mut(SymbolElementMut::Symbol(symbol))
                    .await
            }
            Type::AssociatedSymbol(associated_symbol) => {
                self.visitor
                    .visit_symbol_mut(SymbolElementMut::AssociatedSymbol(
                        associated_symbol,
                    ))
                    .await
            }
            Type::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol_mut(SymbolElementMut::InstanceAssociated(
                        instance_associated,
                    ))
                    .await
            }
            Type::Inference(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::FunctionSignature(_)
            | Type::Error(_) => true,
        }
    }
}

impl<V: AsyncMutableRecursiveSymbolVisitor> AsyncMutableRecursive<Instance>
    for AsyncMutableRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        term: &mut Instance,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        match term {
            Instance::Symbol(symbol) => {
                self.visitor
                    .visit_symbol_mut(SymbolElementMut::Symbol(symbol))
                    .await
            }
            Instance::InstanceAssociated(instance_associated) => {
                self.visitor
                    .visit_symbol_mut(SymbolElementMut::InstanceAssociated(
                        instance_associated,
                    ))
                    .await
            }
            Instance::Parameter(_)
            | Instance::Inference(_)
            | Instance::AnonymousTrait(_)
            | Instance::Error(_) => true,
        }
    }
}

impl<V: AsyncMutableRecursiveSymbolVisitor> AsyncMutableRecursive<Lifetime>
    for AsyncMutableRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        _term: &mut Lifetime,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        true
    }
}

impl<V: AsyncMutableRecursiveSymbolVisitor> AsyncMutableRecursive<Constant>
    for AsyncMutableRecursiveSymbolVisitorAdapter<'_, V>
{
    async fn visit(
        &mut self,
        _term: &mut Constant,
        _locations: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        true
    }
}

/// Accepts a term mutably and visits all symbol elements within it using an
/// `AsyncMutableRecursiveSymbolVisitor`.
///
/// This function uses the `AsyncMutableRecursive` adaptor to traverse Type and
/// Instance terms and invoke the visitor when encountering [`Symbol`],
/// [`AssociatedSymbol`], or [`InstanceAssociated`] variants.
pub async fn accept_symbol_recursive_async_mut<
    V: AsyncMutableRecursiveSymbolVisitor,
    E: crate::visitor::Element,
>(
    element: &mut E,
    visitor: &mut V,
) -> bool {
    let mut adapter = AsyncMutableRecursiveSymbolVisitorAdapter { visitor };
    crate::visitor::accept_recursive_mut_async(element, &mut adapter).await
}
