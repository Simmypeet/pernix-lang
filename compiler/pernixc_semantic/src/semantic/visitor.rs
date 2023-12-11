//! Provides the visitor pattern for the semantic logic.

use super::{
    model::Model,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Tuple,
        TupleElement, Unpacked,
    },
};

/// Represents a visitor that visits a term.
pub trait Visitor {
    /// The model of the term which is being visited.
    type Model: Model;

    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the type.
    fn visit_type(&mut self, ty: &Type<Self::Model>) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the lifetime.
    fn visit_lifetime(&mut self, ty: &Lifetime<Self::Model>) -> bool;

    /// Visits a constant.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the constant.
    fn visit_constant(&mut self, ty: &Constant<Self::Model>) -> bool;
}

/// A term that can be visited by a visitor.
pub trait Element {
    /// The model of the term.
    type Model: Model;

    /// Accepts a visitor.
    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool);
}

fn visit_generic_arguments<V: Visitor>(
    generic_arguments: &GenericArguments<V::Model>,
    visitor: &mut V,
) {
    for ty in &generic_arguments.types {
        ty.accept(visitor, false);
    }

    for constant in &generic_arguments.constants {
        constant.accept(visitor, false);
    }

    for lifetime in &generic_arguments.lifetimes {
        lifetime.accept(visitor, false);
    }
}

fn visit_tuple<
    S: Model,
    Term,
    Parameter: Clone + TryFrom<Term, Error = Term>,
    TraitMember: Clone + TryFrom<Term, Error = Term>,
    V: Visitor<Model = S>,
>(
    tuple: &Tuple<Term, Parameter, TraitMember>,
    visitor: &mut V,
) where
    Term: Element<Model = S>
        + From<Parameter>
        + From<TraitMember>
        + From<Tuple<Term, Parameter, TraitMember>>,
{
    for element in &tuple.elements {
        match element {
            TupleElement::Regular(term) => term.accept(visitor, false),
            TupleElement::Unpacked(unpacked) => match unpacked {
                Unpacked::Parameter(parameter) => {
                    Term::from(parameter.clone()).accept(visitor, false);
                }
                Unpacked::TraitMember(trait_member) => {
                    Term::from(trait_member.clone()).accept(visitor, false);
                }
            },
        }
    }
}

impl<S: Model> Element for Type<S> {
    type Model = S;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool) {
        if !sub_structural_visit && !visitor.visit_type(self) {
            return;
        }

        match self {
            Self::Error | Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {}

            Self::Algebraic(adt) => visit_generic_arguments(&adt.generic_arguments, visitor),
            Self::Pointer(pointer) => pointer.pointee.accept(visitor, sub_structural_visit),

            Self::Reference(reference) => {
                reference.pointee.accept(visitor, sub_structural_visit);
                reference.lifetime.accept(visitor, sub_structural_visit);
            }
            Self::Array(array) => {
                array.element.accept(visitor, sub_structural_visit);
                array.length.accept(visitor, sub_structural_visit);
            }
            Self::TraitMember(trait_member) => {
                visit_generic_arguments(&trait_member.member_generic_arguments, visitor);
                visit_generic_arguments(&trait_member.trait_generic_arguments, visitor);
            }
            Self::Tuple(tuple) => visit_tuple(tuple, visitor),
            Self::Local(local) => local.0.accept(visitor, sub_structural_visit),
        }
    }
}

impl<S: Model> Element for Lifetime<S> {
    type Model = S;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool) {
        // no sub-structural visit for lifetimes
        if sub_structural_visit {
            return;
        }

        visitor.visit_lifetime(self);
    }
}

impl<S: Model> Element for Constant<S> {
    type Model = S;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool) {
        // no sub-structural visit for constants
        if !sub_structural_visit && !visitor.visit_constant(self) {
            return;
        }

        match self {
            Self::Error | Self::Primitive(_) | Self::Inference(_) | Self::Parameter(_) => {}
            Self::Struct(val) => {
                visit_generic_arguments(&val.generic_arguments, visitor);

                for field in &val.fields {
                    field.accept(visitor, sub_structural_visit);
                }
            }
            Self::Enum(val) => {
                visit_generic_arguments(&val.generic_arguments, visitor);

                if let Some(val) = &val.associated_value {
                    val.accept(visitor, sub_structural_visit);
                }
            }
            Self::Array(array) => {
                for element in &array.elements {
                    element.accept(visitor, sub_structural_visit);
                }
            }
            Self::TraitMember(trait_member) => {
                visit_generic_arguments(&trait_member.trait_arguments, visitor);
            }
            Self::Local(local) => {
                local.0.accept(visitor, sub_structural_visit);
            }
            Self::Tuple(tuple) => {
                visit_tuple(tuple, visitor);
            }
        }
    }
}
