//! Contains logic related for checking the coherence between the traits and
//! their implementations.

use std::collections::HashSet;

use diagnostic::{ImplementedForeignAdt, OrphanRuleViolation};
use pernixc_component::implementation::Implementation;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{Implements, LocationSpan, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameterID, GenericParameters, LifetimeParameterID,
        TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::Type,
    sub_term::{
        SubConstantLocation, SubLifetimeLocation, SubTypeLocation, TermLocation,
    },
    visitor::RecursiveIterator,
    Default, Kind,
};
use pernixc_type_system::{environment::Environment, normalizer};

use crate::{occurrences::Checker, type_system::TableExt};

pub mod diagnostic;

/// Contains all the unused generic parameters in a generic arguments.
struct UnusedGenericParameters {
    lifetimes: HashSet<LifetimeParameterID>,
    types: HashSet<TypeParameterID>,
    constants: HashSet<ConstantParameterID>,
}

impl UnusedGenericParameters {
    fn check_in_generic_arguments(&mut self, args: &GenericArguments<Default>) {
        for lt in &args.lifetimes {
            self.check_in_lifetime(lt);
        }

        for ty in &args.types {
            self.check_in_type(ty);
        }

        for val in &args.constants {
            self.check_in_constant(val);
        }
    }

    fn check_in_type(&mut self, ty: &Type<Default>) {
        match ty {
            Type::Parameter(parameter) => {
                self.types.remove(parameter);
            }
            Type::MemberSymbol(symbol) => {
                self.check_in_generic_arguments(
                    &symbol.parent_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &symbol.member_generic_arguments,
                );
            }
            Type::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }
            Type::Pointer(pointer) => {
                self.check_in_type(&pointer.pointee);
            }
            Type::Reference(reference) => {
                self.check_in_lifetime(&reference.lifetime);
                self.check_in_type(&reference.pointee);
            }
            Type::Array(array) => {
                self.check_in_type(&array.r#type);
                self.check_in_constant(&array.length);
            }
            Type::Tuple(tuple) => {
                for ty in &tuple.elements {
                    self.check_in_type(&ty.term);
                }
            }
            Type::Phantom(phantom) => {
                self.check_in_type(&phantom.0);
            }

            Type::TraitMember(_)
            | Type::Inference(_)
            | Type::Error(_)
            | Type::Primitive(_) => {}
        }
    }

    fn check_in_lifetime(&mut self, lt: &Lifetime<Default>) {
        match lt {
            Lifetime::Parameter(parameter) => {
                self.lifetimes.remove(parameter);
            }

            Lifetime::Inference(_)
            | Lifetime::Static
            | Lifetime::Elided(_)
            | Lifetime::Error(_)
            | Lifetime::Forall(_) => {}
        }
    }

    fn check_in_constant(&mut self, val: &Constant<Default>) {
        match val {
            Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            Constant::Phantom
            | Constant::Error(_)
            | Constant::Primitive(_)
            | Constant::Inference(_) => {}

            Constant::Struct(val) => {
                for field in &val.fields {
                    self.check_in_constant(field);
                }
            }

            Constant::Enum(val) => {
                if let Some(value) = &val.associated_value {
                    self.check_in_constant(value);
                }
            }

            Constant::Array(array) => {
                for element in &array.elements {
                    self.check_in_constant(element);
                }
            }

            Constant::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.check_in_constant(&element.term);
                }
            }
        }
    }
}

/// Checks if all the generic parameters are used in the generic arguments
/// signature of the implementation.
pub(super) fn check_unused_generic_parameters(
    table: &Table,
    implementation_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let (Some(generic_parameters), Some(implementation)) = (
        table.query::<GenericParameters>(implementation_id),
        table.query::<Implementation>(implementation_id),
    ) else {
        return;
    };

    let mut unused_generic_arguments = UnusedGenericParameters {
        lifetimes: generic_parameters
            .lifetime_order()
            .iter()
            .map(|x| LifetimeParameterID { parent: implementation_id, id: *x })
            .collect(),
        types: generic_parameters
            .type_order()
            .iter()
            .map(|x| TypeParameterID { parent: implementation_id, id: *x })
            .collect(),
        constants: generic_parameters
            .constant_order()
            .iter()
            .map(|x| ConstantParameterID { parent: implementation_id, id: *x })
            .collect(),
    };

    unused_generic_arguments
        .check_in_generic_arguments(&implementation.generic_arguments);

    for lt in unused_generic_arguments.lifetimes.iter().copied() {
        handler.receive(Box::new(
            diagnostic::UnusedGenericParameterInImplementation {
                generic_parameter_id: lt,
            },
        ));
    }

    for ty in unused_generic_arguments.types.iter().copied() {
        handler.receive(Box::new(
            diagnostic::UnusedGenericParameterInImplementation {
                generic_parameter_id: ty,
            },
        ));
    }

    for val in unused_generic_arguments.constants.iter().copied() {
        handler.receive(Box::new(
            diagnostic::UnusedGenericParameterInImplementation {
                generic_parameter_id: val,
            },
        ));
    }
}

/// Check the where clause predicate requirements of the implemented symbol.
pub(super) fn check_implemented_instantiation(
    table: &Table,
    implementation_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let Some(implementation) = table.query::<Implementation>(implementation_id)
    else {
        return;
    };

    let implements = table.get::<Implements>(implementation_id);
    let location_span = table.get::<LocationSpan>(implementation_id);

    let premise = table.get_active_premise(implementation_id);
    let (environment, _) =
        Environment::new_with(premise, table, normalizer::NO_OP);

    let checker = Checker::new(&environment, handler);

    checker.check_instantiation_predicates_by_generic_arguments(
        implements.0,
        implementation.generic_arguments.clone(),
        &location_span.span.clone().unwrap(),
    );
}

/// Performs orphan rule check on the implementation.
pub(super) fn check_orphan_rule(
    table: &Table,
    implementation_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let implemented_id = table.get::<Implements>(implementation_id).0;

    // If the implementation is in the same module as the implemented symbol,
    // then no need to check the orphan rule.
    if implemented_id.target_id == implementation_id.target_id {
        return;
    }

    let symbol_kind = *table.get::<SymbolKind>(implementation_id);
    let Some(implementation) = table.query::<Implementation>(implementation_id)
    else {
        return;
    };

    if symbol_kind == SymbolKind::AdtImplementation {
        handler.receive(Box::new(ImplementedForeignAdt {
            adt_implementation_id: implementation_id,
        }));
    } else {
        // must contains at least one symbol that's from this compiling target
        let check = |(kind, locations): (Kind<Default>, Vec<TermLocation>)| {
            let into_trait_member = locations.iter().any(|x| match x {
                TermLocation::Lifetime(SubLifetimeLocation::FromType(
                    location,
                )) => location.is_trait_member(),
                TermLocation::Type(SubTypeLocation::FromType(location)) => {
                    location.is_trait_member()
                }
                TermLocation::Constant(SubConstantLocation::FromConstant(
                    _,
                )) => false,
                TermLocation::Constant(SubConstantLocation::FromType(
                    location,
                )) => location.is_trait_member(),
            });

            if into_trait_member {
                return false;
            }

            match kind {
                Kind::Type(Type::Symbol(symbol)) => {
                    let symbol_kind = *table.get::<SymbolKind>(symbol.id);

                    if !symbol_kind.is_adt() {
                        return false;
                    }

                    symbol.id.target_id == implementation_id.target_id
                }

                Kind::Constant(Constant::Struct(val)) => {
                    val.id.target_id == implementation_id.target_id
                }

                Kind::Constant(Constant::Enum(val)) => {
                    val.variant_id.target_id == implementation_id.target_id
                }

                _ => false,
            }
        };

        if !implementation
            .generic_arguments
            .types
            .iter()
            .flat_map(RecursiveIterator::new)
            .chain(
                implementation
                    .generic_arguments
                    .constants
                    .iter()
                    .flat_map(RecursiveIterator::new),
            )
            .any(check)
        {
            handler
                .receive(Box::new(OrphanRuleViolation { implementation_id }));
        }
    }
}

#[cfg(test)]
mod test;
