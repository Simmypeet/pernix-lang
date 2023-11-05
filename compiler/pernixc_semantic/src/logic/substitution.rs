//! Contains the code related to applying substitutions to entities.

use super::Substitution;
use crate::entity::{
    constant::{self, Constant},
    r#type::{self, Type},
    region::Region,
    GenericArguments, Model,
};

macro_rules! substitute_tuple_term {
    ($kind:ident, $self:ident, $tuple:ident, $substitution:ident) => {
        if $tuple
            .elements
            .iter()
            .filter(|x| x.as_unpacked().is_some())
            .count()
            == 0
        {
            for element in &mut $tuple.elements {
                element.as_regular_mut().unwrap().apply($substitution);
            }
        } else {
            let mut elements = Vec::with_capacity($tuple.elements.len());

            for element in $tuple.elements.clone() {
                match element {
                    $kind::TupleElement::Regular(mut regular) => {
                        regular.apply($substitution);
                        elements.push($kind::TupleElement::Regular(regular));
                    }
                    $kind::TupleElement::Unpacked(unpacked) => {
                        let mut unpacked = match unpacked {
                            $kind::Unpacked::Parameter(parameter) => Self::Parameter(parameter),
                            $kind::Unpacked::TraitMember(trait_member) => {
                                Self::TraitMember(trait_member)
                            }
                        };

                        unpacked.apply($substitution);

                        match unpacked {
                            Self::TraitMember(trait_member) => {
                                elements.push($kind::TupleElement::Unpacked(
                                    $kind::Unpacked::TraitMember(trait_member),
                                ));
                            }
                            Self::Parameter(parameter) => {
                                elements.push($kind::TupleElement::Unpacked(
                                    $kind::Unpacked::Parameter(parameter),
                                ));
                            }
                            Self::Tuple(tuples) => {
                                elements.reserve($tuple.elements.len());
                                for element in tuples.elements {
                                    elements.push(element);
                                }
                            }
                            regular => {
                                elements.push($kind::TupleElement::Regular(regular));
                            }
                        }
                    }
                }
            }

            *$self = Self::Tuple($kind::Tuple { elements });
        }
    };
}

impl<S: Model> Type<S> {
    /// Applies the substitution to the type.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.types.get(self).cloned() {
            *self = ok;
            return;
        };

        match self {
            Self::Algebraic(algebraic) => algebraic.generic_arguments.apply(substitution),
            Self::Pointer(pointer) => pointer.pointee.apply(substitution),
            Self::Reference(reference) => {
                reference.region.apply(substitution);
                reference.pointee.apply(substitution);
            }
            Self::Array(array) => {
                array.length.apply(substitution);
                array.element.apply(substitution);
            }
            Self::TraitMember(trait_member) => {
                trait_member.trait_generic_arguments.apply(substitution);
                trait_member.member_generic_arguments.apply(substitution);
            }
            Self::Tuple(tuple) => {
                substitute_tuple_term!(r#type, self, tuple, substitution);
            }
            _ => {}
        }
    }
}

impl<S: Model> Constant<S> {
    /// Applies the substitution to the constant.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.constants.get(self).cloned() {
            *self = ok;
            return;
        };

        match self {
            Self::Struct(structs) => {
                structs.generic_arguments.apply(substitution);

                for field in &mut structs.fields {
                    field.apply(substitution);
                }
            }
            Self::Enum(enums) => {
                enums.generic_arguments.apply(substitution);

                if let Some(associated_value) = &mut enums.associated_value {
                    associated_value.apply(substitution);
                }
            }
            Self::Array(array) => {
                array.element_ty.apply(substitution);

                for element in &mut array.elements {
                    element.apply(substitution);
                }
            }
            Self::TraitMember(trait_member) => {
                trait_member.trait_arguments.apply(substitution);
            }
            Self::Tuple(tuple) => {
                substitute_tuple_term!(constant, self, tuple, substitution);
            }
            _ => {}
        }
    }
}

impl<S: Model> Region<S> {
    /// Applies the substitution to the region.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.regions.get(self).cloned() {
            *self = ok;
        };
    }
}

impl<S: Model> GenericArguments<S> {
    /// Applies the substitution to the generic arguments.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        for region in &mut self.regions {
            region.apply(substitution);
        }

        for ty in &mut self.types {
            ty.apply(substitution);
        }

        for constant in &mut self.constants {
            constant.apply(substitution);
        }
    }
}
