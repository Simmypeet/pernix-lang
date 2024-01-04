//! Implements the visitor pattern for semantic terms.

use thiserror::Error;

use super::{
    substitution::{Substitute, Substitution},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{SymbolKindID, Type},
        GenericArguments, Tuple, TupleElement,
    },
};
use crate::table::{Index, State, Table};

/// Represents a visitor that visits a term.
pub trait Visitor {
    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the type.
    #[must_use]
    fn visit_type(&mut self, ty: &Type, source: Source) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the lifetime.
    #[must_use]
    fn visit_lifetime(&mut self, lifetime: &Lifetime, source: Source) -> bool;

    /// Visits a constant.  
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the constant.
    #[must_use]
    fn visit_constant(&mut self, constant: &Constant, source: Source) -> bool;
}

/// Specifies where the term is coming from for the visitor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Source {
    /// The term is coming from the input.
    Term,

    /// The term is coming from the fields/variants of an ADT.
    AdtStructure,
}

/// Determines which terms should be included in the visit.
#[derive(Debug)]
pub enum VisitMode<'a, S: State> {
    /// Only visit the sub-terms of the given term (including the term itself).
    OnlySubTerms,

    /// Additionally visit the types appearing the ADT terms as well (i.e. the fields of a struct
    /// or variant of the enum).
    ///
    /// The table is required to look up the fields/variants of the ADT.
    IncludeAdtStructure(&'a Table<S>),
}

impl<S: State> Clone for VisitMode<'_, S> {
    fn clone(&self) -> Self {
        match self {
            Self::OnlySubTerms => Self::OnlySubTerms,
            Self::IncludeAdtStructure(table) => Self::IncludeAdtStructure(table),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("cannot call `accept_one_level` on a non-application term")]
#[allow(missing_docs)]
pub struct VisitNonApplicationTermError;

#[derive(Debug)]
struct RecursiveVisitorAdapter<'a, V: Visitor, S: State> {
    visitor: &'a mut V,
    mode: VisitMode<'a, S>,
}

impl<'a, V: Visitor, S: State> Visitor for RecursiveVisitorAdapter<'a, V, S> {
    fn visit_type(&mut self, ty: &Type, source: Source) -> bool {
        if !self.visitor.visit_type(ty, source) {
            return false;
        }

        ty.accept_one_level(self, self.mode.clone()).unwrap_or(true)
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime, source: Source) -> bool {
        if !self.visitor.visit_lifetime(lifetime, source) {
            return false;
        }

        lifetime
            .accept_one_level(self, self.mode.clone())
            .unwrap_or(true)
    }

    fn visit_constant(&mut self, constant: &Constant, source: Source) -> bool {
        if !self.visitor.visit_constant(constant, source) {
            return false;
        }

        constant
            .accept_one_level(self, self.mode.clone())
            .unwrap_or(true)
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
        mode: VisitMode<impl State>,
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
pub fn accept_recursive(
    element: &impl Element,
    visitor: &mut impl Visitor,
    mode: VisitMode<impl State>,
) -> bool {
    if !element.accept_single(visitor) {
        return false;
    }

    let mut adapter = RecursiveVisitorAdapter {
        visitor,
        mode: mode.clone(),
    };

    match element.accept_one_level(&mut adapter, mode) {
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
            if !visitor.visit_lifetime(lifetime, Source::Term) {
                return false;
            }
        }

        for ty in &self.types {
            if !visitor.visit_type(ty, Source::Term) {
                return false;
            }
        }

        for constant in &self.constants {
            if !visitor.visit_constant(constant, Source::Term) {
                return false;
            }
        }

        true
    }
}

impl Element for Type {
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool {
        visitor.visit_type(self, Source::Term)
    }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
        mode: VisitMode<impl State>,
    ) -> Result<bool, VisitNonApplicationTermError> {
        match self {
            Self::Primitive(_) | Self::Parameter(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Symbol(term) => {
                if !term.generic_arguments.accept_one_level(visitor) {
                    return Ok(false);
                }

                if let VisitMode::IncludeAdtStructure(table) = mode.clone() {
                    match term.id {
                        SymbolKindID::Struct(id) => {
                            let Some(symbol) = table.get(id) else {
                                return Ok(true);
                            };

                            for field_ty in symbol.fields.values().map(|x| {
                                let mut x = x.r#type.clone();
                                x.apply(&Substitution::from_generic_arguments(
                                    term.generic_arguments.clone(),
                                    id.into(),
                                ));
                                x
                            }) {
                                if !visitor.visit_type(&field_ty, Source::AdtStructure) {
                                    return Ok(false);
                                }
                            }

                            Ok(true)
                        }
                        SymbolKindID::Enum(id) => {
                            let Some(symbol) = table.get(id) else {
                                return Ok(true);
                            };

                            for variant_type in
                                symbol.variant_ids_by_name.values().filter_map(|x| {
                                    table.get(*x).and_then(|x| {
                                        x.associated_type.as_ref().map(|x| {
                                            let mut ty = x.clone();
                                            ty.apply(&Substitution::from_generic_arguments(
                                                term.generic_arguments.clone(),
                                                id.into(),
                                            ));
                                            ty
                                        })
                                    })
                                })
                            {
                                if !visitor.visit_type(&variant_type, Source::AdtStructure) {
                                    return Ok(false);
                                }
                            }

                            Ok(true)
                        }
                        SymbolKindID::Type(_) => Ok(true),
                    }
                } else {
                    Ok(true)
                }
            }
            Self::Pointer(term) => Ok(visitor.visit_type(&term.pointee, Source::Term)),
            Self::Reference(term) => Ok(visitor.visit_lifetime(&term.lifetime, Source::Term)
                && visitor.visit_type(&term.pointee, Source::Term)),
            Self::Array(term) => Ok(visitor.visit_type(&term.element, Source::Term)
                && visitor.visit_constant(&term.length, Source::Term)),
            Self::Tuple(tuple) => Ok(tuple.accept_one_level(visitor)),
            Self::Local(local) => Ok(visitor.visit_type(&local.0, Source::Term)),
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
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool {
        visitor.visit_lifetime(self, Source::Term)
    }

    fn accept_one_level(
        &self,
        _: &mut impl Visitor,
        _: VisitMode<impl State>,
    ) -> Result<bool, VisitNonApplicationTermError> {
        Err(VisitNonApplicationTermError)
    }
}

impl Element for Constant {
    fn accept_single(&self, visitor: &mut impl Visitor) -> bool {
        visitor.visit_constant(self, Source::Term)
    }

    fn accept_one_level(
        &self,
        visitor: &mut impl Visitor,
        _: VisitMode<impl State>,
    ) -> Result<bool, VisitNonApplicationTermError> {
        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {
                Err(VisitNonApplicationTermError)
            }

            Self::Struct(term) => {
                for field in &term.fields {
                    if !visitor.visit_constant(field, Source::Term) {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            Self::Enum(term) => Ok(term
                .associated_value
                .as_ref()
                .map_or(true, |x| visitor.visit_constant(x, Source::Term))),
            Self::Array(term) => Ok(term
                .elements
                .iter()
                .all(|x| visitor.visit_constant(x, Source::Term))),

            Self::Local(local) => Ok(visitor.visit_constant(&local.0, Source::Term)),
            Self::Tuple(tuple) => Ok(tuple.accept_one_level(visitor)),
            Self::Symbol(symbol) => Ok(symbol.generic_arguments.accept_one_level(visitor)),
            Self::MemberSymbol(term) => Ok(term.member_generic_arguments.accept_one_level(visitor)
                && term.parent_generic_arguments.accept_one_level(visitor)),
        }
    }
}
