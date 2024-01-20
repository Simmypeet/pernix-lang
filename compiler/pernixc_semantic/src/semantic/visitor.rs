//! Implements the visitor pattern for semantic terms.

use thiserror::Error;

use super::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Tuple, TupleElement,
};

/// Represents a visitor that visits a term.
pub trait Visitor {
    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the type.
    #[must_use]
    fn visit_type(&mut self, ty: &Type) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the lifetime.
    #[must_use]
    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool;

    /// Visits a constant.  
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the constant.
    #[must_use]
    fn visit_constant(&mut self, constant: &Constant) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("cannot call `accept_one_level` on a non-application term")]
#[allow(missing_docs)]
pub struct VisitNonApplicationTermError;

#[derive(Debug)]
struct RecursiveVisitorAdapter<'a, V: Visitor> {
    visitor: &'a mut V,
}

impl<'a, V: Visitor> Visitor for RecursiveVisitorAdapter<'a, V> {
    fn visit_type(&mut self, ty: &Type) -> bool {
        if !self.visitor.visit_type(ty) {
            return false;
        }

        ty.accept_one_level(self).unwrap_or(true)
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool {
        if !self.visitor.visit_lifetime(lifetime) {
            return false;
        }

        lifetime.accept_one_level(self).unwrap_or(true)
    }

    fn visit_constant(&mut self, constant: &Constant) -> bool {
        if !self.visitor.visit_constant(constant) {
            return false;
        }

        constant.accept_one_level(self).unwrap_or(true)
    }
}

/// A term for the visitor pattern.
pub trait Element {
    /// Invokes the visitor on the term itself once.
    ///
    /// # Returns
    ///
    /// Returns whatever the visitor `visit_*` method returns.
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool;

    /// Visits one sub-level of the term.
    ///
    /// # Example
    ///
    /// When a term `Type[int32, Vec[float32]]` got called, the visitor will be visiting only
    /// `int32` and `Vec[int32]` (i.e. the sub-terms of the term), which is unlike the `accept`
    /// method that will also visit the term itself and `float32`.
    ///
    /// # Returns
    ///
    /// If returns boolean false, the visitor will early stop visiting the sub-terms of the term.
    ///
    /// # Errors
    ///
    /// When visiting a non-application term (i.e. `int32, float32`)
    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError>;
}

/// Invokes the visitor on the term itself and all of its sub-terms recursively.
///
/// # Example
///
/// When a term `Type[int32, Vec[float32]]` got called, the visitor will be visiting
/// `Type[int32, Vec[float32]]`, `int32`, `Vec[float32]`, and `float32`.
///
/// # Returns
///
/// Returns `true` if the visitor has visited all of the sub-terms of the term.
pub fn accept_recursive(element: &impl Element, visitor: &mut impl Visitor) -> bool {
    if !element.accept_single(visitor) {
        return false;
    }

    let mut adapter = RecursiveVisitorAdapter { visitor };

    match element.accept_one_level(&mut adapter) {
        Ok(result) => result,
        Err(VisitNonApplicationTermError) => true,
    }
}

impl<Term: Element + Clone> Tuple<Term>
where
    Self: TryFrom<Term, Error = Term> + Into<Term>,
{
    fn accept_one_level(&self, visitor: &mut impl Visitor) -> bool {
        for element in &self.elements {
            if !match element {
                TupleElement::Regular(term) | TupleElement::Unpacked(term) => {
                    term.accept_single(visitor)
                }
            } {
                return false;
            }
        }

        true
    }
}

impl GenericArguments {
    fn accept_one_level(&self, visitor: &mut impl Visitor) -> bool {
        for lifetime in &self.lifetimes {
            if !visitor.visit_lifetime(lifetime) {
                return false;
            }
        }

        for ty in &self.types {
            if !visitor.visit_type(ty) {
                return false;
            }
        }

        for constant in &self.constants {
            if !visitor.visit_constant(constant) {
                return false;
            }
        }

        true
    }
}

impl Element for Type {
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool { visitor.visit_type(self) }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError> {
        match self {
            Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                if !term.generic_arguments.accept_one_level(visitor) {
                    return Ok(false);
                }

                Ok(true)
            }
            Self::Pointer(term) => Ok(visitor.visit_type(&term.pointee)),
            Self::Reference(term) => {
                Ok(visitor.visit_lifetime(&term.lifetime) && visitor.visit_type(&term.pointee))
            }
            Self::Array(term) => {
                Ok(visitor.visit_type(&term.r#type) && visitor.visit_constant(&term.length))
            }
            Self::Tuple(tuple) => Ok(tuple.accept_one_level(visitor)),
            Self::Local(local) => Ok(visitor.visit_type(&local.0)),
            Self::MemberSymbol(implementation) => Ok(implementation
                .member_generic_arguments
                .accept_one_level(visitor)
                && implementation
                    .parent_generic_arguments
                    .accept_one_level(visitor)),
        }
    }
}

impl Element for Lifetime {
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool { visitor.visit_lifetime(self) }

    fn accept_one_level(&self, _: &mut impl Visitor) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }
}

impl Element for Constant {
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool { visitor.visit_constant(self) }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
    ) -> Result<bool, VisitNonApplicationTermError> {
        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Struct(term) => {
                for field in &term.fields {
                    if !visitor.visit_constant(field) {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            Self::Enum(term) => Ok(term
                .associated_value
                .as_ref()
                .map_or(true, |x| visitor.visit_constant(x))),
            Self::Array(term) => Ok(term.elements.iter().all(|x| visitor.visit_constant(x))),

            Self::Local(local) => Ok(visitor.visit_constant(&local.0)),
            Self::Tuple(tuple) => Ok(tuple.accept_one_level(visitor)),
            Self::Symbol(symbol) => Ok(symbol.generic_arguments.accept_one_level(visitor)),
            Self::MemberSymbol(term) => Ok(term.member_generic_arguments.accept_one_level(visitor)
                && term.parent_generic_arguments.accept_one_level(visitor)),
        }
    }
}
