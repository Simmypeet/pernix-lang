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
    fn visit_lifetime(&mut self, lifetime: &Lifetime<Self::Model>) -> bool;

    /// Visits a constant.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the constant.
    fn visit_constant(&mut self, constant: &Constant<Self::Model>) -> bool;
}

/// A term that can be visited by a visitor.
pub trait Element {
    /// The model of the term.
    type Model: Model;

    /// Accepts a visitor.
    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool);
}

impl<S: Model> Element for GenericArguments<S> {
    type Model = S;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, _: bool) {
        for ty in &self.types {
            ty.accept(visitor, false);
        }

        for constant in &self.constants {
            constant.accept(visitor, false);
        }

        for lifetime in &self.lifetimes {
            lifetime.accept(visitor, false);
        }
    }
}

impl<Term, Parameter, TraitMember> Element for Tuple<Term, Parameter, TraitMember>
where
    Term: Element + From<Parameter> + From<TraitMember> + From<Self>,
    Parameter: Clone + TryFrom<Term, Error = Term>,
    TraitMember: Clone + TryFrom<Term, Error = Term>,
{
    type Model = Term::Model;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, _: bool) {
        for element in &self.elements {
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
}

impl<S: Model> Element for Type<S> {
    type Model = S;

    fn accept(&self, visitor: &mut impl Visitor<Model = Self::Model>, sub_structural_visit: bool) {
        if !sub_structural_visit && !visitor.visit_type(self) {
            return;
        }

        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => {}

            Self::Algebraic(adt) => adt.generic_arguments.accept(visitor, false),
            Self::Pointer(pointer) => pointer.pointee.accept(visitor, false),

            Self::Reference(reference) => {
                reference.pointee.accept(visitor, false);
                reference.lifetime.accept(visitor, false);
            }
            Self::Array(array) => {
                array.element.accept(visitor, false);
                array.length.accept(visitor, false);
            }
            Self::TraitMember(trait_member) => {
                trait_member.member_generic_arguments.accept(visitor, false);
                trait_member.trait_generic_arguments.accept(visitor, false);
            }
            Self::Tuple(tuple) => tuple.accept(visitor, false),
            Self::Local(local) => local.0.accept(visitor, false),
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
            Self::Primitive(_) | Self::Inference(_) | Self::Parameter(_) => {}
            Self::Struct(val) => {
                val.generic_arguments.accept(visitor, false);

                for field in &val.fields {
                    field.accept(visitor, false);
                }
            }
            Self::Enum(val) => {
                val.generic_arguments.accept(visitor, false);

                if let Some(val) = &val.associated_value {
                    val.accept(visitor, false);
                }
            }
            Self::Array(array) => {
                for element in &array.elements {
                    element.accept(visitor, false);
                }
            }
            Self::TraitMember(trait_member) => {
                trait_member.trait_generic_arguments.accept(visitor, false);
                trait_member
                    .constant_generic_arguments
                    .accept(visitor, false);
            }
            Self::Local(local) => {
                local.0.accept(visitor, false);
            }
            Self::Tuple(tuple) => {
                tuple.accept(visitor, false);
            }
            Self::Symbol(symbol) => {
                symbol.generic_arguments.accept(visitor, false);
            }
            Self::Implementation(symbol) => {
                symbol
                    .implementation_generic_arguments
                    .accept(visitor, false);
                symbol.constant_generic_arguments.accept(visitor, false);
            }
        }
    }
}
