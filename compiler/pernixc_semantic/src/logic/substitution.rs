use super::Substitution;
use crate::entity::{
    constant::Constant,
    r#type::{self, Tuple, Type},
    region::Region,
    GenericArguments, Model,
};

impl<S: Model> Type<S> {
    /// Applies the substitution to the type.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.types.get(self).cloned() {
            *self = ok.into_owned();
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
                if tuple
                    .elements
                    .iter()
                    .filter(|x| x.as_unpacked().is_some())
                    .count()
                    == 0
                {
                    for element in &mut tuple.elements {
                        element.as_regular_mut().unwrap().apply(substitution);
                    }
                } else {
                    let mut elements = Vec::with_capacity(tuple.elements.len());

                    for element in tuple.elements.clone() {
                        match element {
                            r#type::TupleElement::Regular(mut regular) => {
                                regular.apply(substitution);
                                elements.push(r#type::TupleElement::Regular(regular));
                            }
                            r#type::TupleElement::Unpacked(unpacked) => {
                                let mut unpacked = match unpacked {
                                    r#type::Unpacked::Parameter(parameter) => {
                                        Self::Parameter(parameter)
                                    }
                                    r#type::Unpacked::TraitMember(trait_member) => {
                                        Self::TraitMember(trait_member)
                                    }
                                };

                                unpacked.apply(substitution);

                                match unpacked {
                                    Self::TraitMember(trait_member) => {
                                        elements.push(r#type::TupleElement::Unpacked(
                                            r#type::Unpacked::TraitMember(trait_member),
                                        ));
                                    }
                                    Self::Parameter(parameter) => {
                                        elements.push(r#type::TupleElement::Unpacked(
                                            r#type::Unpacked::Parameter(parameter),
                                        ));
                                    }
                                    Self::Tuple(tuples) => {
                                        elements.reserve(tuple.elements.len());
                                        for element in tuples.elements {
                                            elements.push(element);
                                        }
                                    }
                                    regular => {
                                        elements.push(r#type::TupleElement::Regular(regular));
                                    }
                                }
                            }
                        }
                    }

                    *self = Self::Tuple(Tuple { elements });
                }
            }
            _ => {}
        }
    }
}

impl<S: Model> Constant<S> {
    /// Applies the substitution to the constant.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.constants.get(self).cloned() {
            *self = ok.into_owned();
        };
    }
}

impl<S: Model> Region<S> {
    /// Applies the substitution to the region.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        if let Some(ok) = substitution.regions.get(self).cloned() {
            *self = ok.into_owned();
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
