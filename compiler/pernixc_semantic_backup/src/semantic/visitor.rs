//! Provides the visitor pattern for the semantic logic.

use super::{
    model::{Entity, Model},
    substitution::{Substitute, Substitution},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{SymbolKindID, Type},
        GenericArguments, Tuple, TupleElement, Unpacked,
    },
};
use crate::table::{Index, State, Table};

/// Represents a visitor that visits a term.
pub trait Visitor {
    /// The model of the term which is being visited.
    type Model: Model;

    /// Visits a type.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the type.
    #[must_use]
    fn visit_type(&mut self, ty: &Type<Self::Model>, source: Source) -> bool;

    /// Visits a lifetime.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the lifetime.
    #[must_use]
    fn visit_lifetime(&mut self, lifetime: &Lifetime<Self::Model>, source: Source) -> bool;

    /// Visits a constant.
    ///
    /// Returns `true` if the visitor should continue visiting the sub-terms of the constant.
    #[must_use]
    fn visit_constant(&mut self, constant: &Constant<Self::Model>, source: Source) -> bool;
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

/// A term that can be visited by a visitor.
pub trait Element {
    /// The model of the term.
    type Model: Model;

    /// Accepts a visitor.
    #[must_use]
    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        mode: VisitMode<impl State>,
    ) -> bool;
}

impl<S: Model> Element for GenericArguments<S> {
    type Model = S;

    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        mode: VisitMode<impl State>,
    ) -> bool {
        for ty in &self.types {
            if !ty.accept(visitor, mode.clone()) {
                return false;
            }
        }

        for constant in &self.constants {
            if !constant.accept(visitor, mode.clone()) {
                return false;
            }
        }

        for lifetime in &self.lifetimes {
            if !lifetime.accept(visitor, mode.clone()) {
                return false;
            }
        }

        true
    }
}

impl<Term, Parameter, TraitMember> Element for Tuple<Term, Parameter, TraitMember>
where
    Term: Element + From<Parameter> + From<TraitMember> + From<Self>,
    Parameter: Clone + TryFrom<Term, Error = Term>,
    TraitMember: Clone + TryFrom<Term, Error = Term>,
{
    type Model = Term::Model;

    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        mode: VisitMode<impl State>,
    ) -> bool {
        for element in &self.elements {
            if !match element {
                TupleElement::Regular(term) => term.accept(visitor, mode.clone()),
                TupleElement::Unpacked(unpacked) => match unpacked {
                    Unpacked::Parameter(parameter) => {
                        Term::from(parameter.clone()).accept(visitor, mode.clone())
                    }
                    Unpacked::TraitMember(trait_member) => {
                        Term::from(trait_member.clone()).accept(visitor, mode.clone())
                    }
                },
            } {
                return false;
            }
        }

        true
    }
}

impl<S: Model> Type<S> {
    fn accept_internal(
        &self,
        visitor: &mut impl Visitor<Model = S>,
        mode: VisitMode<impl State>,
        source: Source,
    ) -> bool {
        if !visitor.visit_type(self, source) {
            return false;
        }

        match self {
            Self::Parameter(_) | Self::Primitive(_) | Self::Inference(_) => true,

            Self::Symbol(adt) => {
                if !adt.generic_arguments.accept(visitor, mode.clone()) {
                    return false;
                }

                if let VisitMode::IncludeAdtStructure(table) = mode.clone() {
                    match adt.id {
                        SymbolKindID::Struct(id) => {
                            let Some(symbol) = table.get(id) else {
                                return true;
                            };

                            for field_ty in symbol.fields.values().map(|x| {
                                let mut ty = x.r#type.clone().into_other_model();
                                ty.apply(&Substitution::from_generic_arguments(
                                    adt.generic_arguments.clone(),
                                    id.into(),
                                ));
                                ty
                            }) {
                                if !field_ty.accept_internal(
                                    visitor,
                                    mode.clone(),
                                    Source::AdtStructure,
                                ) {
                                    return false;
                                }
                            }

                            true
                        }
                        SymbolKindID::Enum(id) => {
                            let Some(symbol) = table.get(id) else {
                                return true;
                            };

                            for variant_type in
                                symbol.variant_ids_by_name.values().filter_map(|x| {
                                    table.get(*x).and_then(|x| {
                                        x.associated_type.as_ref().map(|x| {
                                            let mut ty = x.clone().into_other_model();
                                            ty.apply(&Substitution::from_generic_arguments(
                                                adt.generic_arguments.clone(),
                                                id.into(),
                                            ));
                                            ty
                                        })
                                    })
                                })
                            {
                                if !variant_type.accept_internal(
                                    visitor,
                                    mode.clone(),
                                    Source::AdtStructure,
                                ) {
                                    return false;
                                }
                            }

                            true
                        }
                        SymbolKindID::Type(_) => true,
                    }
                } else {
                    true
                }
            }
            Self::Pointer(pointer) => pointer.pointee.accept(visitor, mode),

            Self::Implementation(implementation) => {
                implementation
                    .parent_generic_arguments
                    .accept(visitor, mode.clone())
                    && implementation
                        .member_generic_arguments
                        .accept(visitor, mode.clone())
            }

            Self::Reference(reference) => {
                reference.pointee.accept(visitor, mode.clone())
                    && reference.lifetime.accept(visitor, mode)
            }
            Self::Array(array) => {
                array.element.accept(visitor, mode.clone()) && array.length.accept(visitor, mode)
            }
            Self::TraitMember(trait_member) => {
                trait_member
                    .member_generic_arguments
                    .accept(visitor, mode.clone())
                    && trait_member.parent_generic_arguments.accept(visitor, mode)
            }
            Self::Tuple(tuple) => tuple.accept(visitor, mode),
            Self::Local(local) => local.0.accept(visitor, mode),
        }
    }
}

impl<S: Model> Element for Type<S> {
    type Model = S;

    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        mode: VisitMode<impl State>,
    ) -> bool {
        self.accept_internal(visitor, mode, Source::Term)
    }
}

impl<S: Model> Element for Lifetime<S> {
    type Model = S;

    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        _: VisitMode<impl State>,
    ) -> bool {
        // no sub-structural visit for lifetimes
        visitor.visit_lifetime(self, Source::Term)
    }
}

impl<S: Model> Element for Constant<S> {
    type Model = S;

    fn accept(
        &self,
        visitor: &mut impl Visitor<Model = Self::Model>,
        mode: VisitMode<impl State>,
    ) -> bool {
        // no sub-structural visit for constants
        if !visitor.visit_constant(self, Source::Term) {
            return false;
        }

        match self {
            Self::Primitive(_) | Self::Inference(_) | Self::Parameter(_) => true,
            Self::Struct(val) => {
                if !val.generic_arguments.accept(visitor, mode.clone()) {
                    return false;
                }

                for field in &val.fields {
                    if !field.accept(visitor, mode.clone()) {
                        return false;
                    }
                }

                true
            }
            Self::Enum(val) => {
                if !val.generic_arguments.accept(visitor, mode.clone()) {
                    return false;
                }

                val.associated_value
                    .as_ref()
                    .map_or(true, |val| val.accept(visitor, mode.clone()))
            }
            Self::Array(array) => {
                for element in &array.elements {
                    if !element.accept(visitor, mode.clone()) {
                        return false;
                    }
                }
                true
            }
            Self::TraitMember(trait_member) => {
                trait_member
                    .parent_generic_arguments
                    .accept(visitor, mode.clone())
                    && trait_member
                        .member_generic_arguments
                        .accept(visitor, mode.clone())
            }
            Self::Local(local) => local.0.accept(visitor, mode),
            Self::Tuple(tuple) => tuple.accept(visitor, mode),
            Self::Symbol(symbol) => symbol.generic_arguments.accept(visitor, mode),
            Self::Implementation(symbol) => {
                symbol
                    .parent_generic_arguments
                    .accept(visitor, mode.clone())
                    && symbol.member_generic_arguments.accept(visitor, mode)
            }
        }
    }
}
