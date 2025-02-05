//! Contains logic related for checking the coherence between the traits and
//! their implementations.

use std::{borrow::Cow, collections::HashSet};

use diagnostic::{
    ExtraneousImplementationMemberPredicate,
    FinalImplementationCannotBeOverriden, ImplementedForeignAdt,
    MismatchedGenericParameterCountInImplementation,
    MismatchedImplementationConstantTypeParameter, OrphanRuleViolation,
};
use pernixc_component::implementation::Implementation;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{
        Implemented, Implements, LocationSpan, Member, Name, Parent,
        SymbolKind, TraitImplementation,
    },
    diagnostic::Diagnostic,
    query::CyclicDependencyError,
    GlobalID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameterID, GenericKind, GenericParameters,
        LifetimeParameterID, TypeParameterID,
    },
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::Type,
    sub_term::{
        SubConstantLocation, SubLifetimeLocation, SubTypeLocation, TermLocation,
    },
    variance::Variance,
    visitor::RecursiveIterator,
    where_clause::WhereClause,
    Default, Kind,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
    order::Order,
    AbruptError,
};

use crate::{
    occurrences::Checker,
    type_system::{
        diagnostic::{
            OverflowOperation, TypeSystemOverflow, UndecidablePredicate,
        },
        TableExt,
    },
};

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
            Type::FunctionSignature(signature) => {
                for ty in &signature.parameters {
                    self.check_in_type(ty);
                }

                self.check_in_type(&signature.return_type);
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
    let (Ok(generic_parameters), Ok(implementation)) = (
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
    let Ok(implementation) = table.query::<Implementation>(implementation_id)
    else {
        return;
    };

    let implements = table.get::<Implements>(implementation_id);
    let location_span = table.get::<LocationSpan>(implementation_id);

    let premise = table.get_active_premise(implementation_id);
    let environment =
        Environment::new(Cow::Borrowed(&premise), table, normalizer::NO_OP);

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
    let Ok(implementation) = table.query::<Implementation>(implementation_id)
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

/// Performs orphan rule check on the implementation.
pub(super) fn check_overlapping(
    table: &Table,
    implementation_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let implemented_id = table.get::<Implements>(implementation_id).0;
    let implementations = table.get::<Implemented>(implemented_id);

    let Ok(this_implementation) =
        table.query::<Implementation>(implementation_id)
    else {
        return;
    };

    let default_premise = Premise::<Default>::default();
    let environment = Environment::new(
        Cow::Borrowed(&default_premise),
        table,
        normalizer::NO_OP,
    );

    for id in implementations.iter().copied() {
        if id == implementation_id {
            continue;
        }

        let other_symbol_kind = *table.get::<SymbolKind>(id);
        let Ok(other_implementation) = table.query::<Implementation>(id) else {
            continue;
        };

        // determine the order of the two implementations
        let order = match environment.order(
            &this_implementation.generic_arguments,
            &other_implementation.generic_arguments,
        ) {
            Ok(order) => order,

            Err(AbruptError::CyclicDependency(CyclicDependencyError)) => {
                continue
            }
            Err(AbruptError::Overflow(overflow_error)) => {
                handler.receive(Box::new(TypeSystemOverflow {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: table
                        .get::<LocationSpan>(implementation_id)
                        .span
                        .clone()
                        .unwrap(),
                    overflow_error,
                }));
                continue;
            }
        };

        let other_is_final = match other_symbol_kind {
            SymbolKind::PositiveMarkerImplementation
            | SymbolKind::NegativeMarkerImplementation => true,

            SymbolKind::PositiveTraitImplementation
            | SymbolKind::NegativeTraitImplementation => {
                let other_trait_impl = table.get::<TraitImplementation>(id);
                other_trait_impl.is_final
            }

            _ => unreachable!(),
        };

        if other_is_final && order == Order::MoreSpecific {
            handler.receive(Box::new(FinalImplementationCannotBeOverriden {
                final_implementation_id: id,
                overriden_implementation_id: implementation_id,
            }));
        } else if order == Order::Ambiguous {
            if id.target_id == implementation_id.target_id {
                if implementation_id > id {
                    handler.receive(Box::new(
                        diagnostic::AmbiguousImplementation {
                            second_implementation_id: implementation_id,
                            first_implementation_id: id,
                        },
                    ));
                }
            } else {
                handler.receive(Box::new(
                    diagnostic::AmbiguousImplementation {
                        first_implementation_id: implementation_id,
                        second_implementation_id: id,
                    },
                ));
            }
        }
    }
}

pub(super) fn create_impl_member_to_trait_member_inst(
    impl_args: GenericArguments<Default>,
    trait_id: GlobalID,
    trait_generic_params: &GenericParameters,
    trait_member_generic_params: &GenericParameters,
    impl_member_generic_params: &GenericParameters,
    trait_member_id: GlobalID,
    implementation_member_id: GlobalID,
) -> Instantiation<Default> {
    let mut trait_to_impl_inst = Instantiation::from_generic_arguments(
        impl_args,
        trait_id,
        trait_generic_params,
    )
    .unwrap();

    assert!(trait_to_impl_inst
        .append_from_generic_parameters_mapping(
            implementation_member_id,
            impl_member_generic_params,
            trait_member_id,
            trait_member_generic_params,
        )
        .is_empty());

    trait_to_impl_inst
}

#[allow(clippy::too_many_lines)]
pub(super) fn check_implementation_member(
    table: &Table,
    implementation_member_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let this_symbol_kind = *table.get::<SymbolKind>(implementation_member_id);
    let this_name = table.get::<Name>(implementation_member_id);
    let parent_impl = GlobalID::new(
        implementation_member_id.target_id,
        table.get::<Parent>(implementation_member_id).parent.unwrap(),
    );

    let trait_id = table.get::<Implements>(parent_impl).0;
    let trait_member_id = GlobalID::new(
        trait_id.target_id,
        table.get::<Member>(trait_id)[&this_name.0],
    );

    let (
        Ok(impl_member_generic_params),
        Ok(trait_member_generic_params),
        Ok(impl_args),
        Ok(trait_params),
    ) = (
        table.query::<GenericParameters>(implementation_member_id),
        table.query::<GenericParameters>(trait_member_id),
        table.query::<Implementation>(parent_impl),
        table.query::<GenericParameters>(trait_id),
    )
    else {
        return;
    };

    if this_symbol_kind != SymbolKind::TraitImplementationFunction
        && impl_member_generic_params.lifetimes().len()
            != trait_member_generic_params.lifetimes().len()
    {
        handler.receive(Box::new(
            MismatchedGenericParameterCountInImplementation {
                implementation_member_id,
                trait_member_id,
                expected_count: trait_member_generic_params.lifetimes().len(),
                declared_count: impl_member_generic_params.lifetimes().len(),
                generic_kind: GenericKind::Lifetime,
            },
        ));
    }

    if impl_member_generic_params.types().len()
        != trait_member_generic_params.types().len()
    {
        handler.receive(Box::new(
            MismatchedGenericParameterCountInImplementation {
                implementation_member_id,
                trait_member_id,
                expected_count: trait_member_generic_params.types().len(),
                declared_count: impl_member_generic_params.types().len(),
                generic_kind: GenericKind::Type,
            },
        ));
    }

    if impl_member_generic_params.constants().len()
        != trait_member_generic_params.constants().len()
    {
        handler.receive(Box::new(
            MismatchedGenericParameterCountInImplementation {
                implementation_member_id,
                trait_member_id,
                expected_count: trait_member_generic_params.constants().len(),
                declared_count: impl_member_generic_params.constants().len(),
                generic_kind: GenericKind::Constant,
            },
        ));
    }

    if matches!(
        this_symbol_kind,
        SymbolKind::TraitImplementationType
            | SymbolKind::TraitImplementationConstant
    ) {
        let trait_to_impl_inst = create_impl_member_to_trait_member_inst(
            impl_args.generic_arguments.clone(),
            trait_id,
            &trait_params,
            &trait_member_generic_params,
            &impl_member_generic_params,
            trait_member_id,
            implementation_member_id,
        );

        let active_premise =
            table.get_active_premise::<Default>(implementation_member_id);

        let impl_member_env = Environment::new(
            Cow::Borrowed(&active_premise),
            table,
            normalizer::NO_OP,
        );

        let trait_member_premise = {
            let stub = table.get_active_premise(trait_member_id);

            Premise {
                query_site: impl_member_env.premise().query_site,
                predicates: stub
                    .predicates
                    .into_iter()
                    .map(|mut x| {
                        x.instantiate(&trait_to_impl_inst);
                        x
                    })
                    .collect(),
            }
        };

        let trait_member_env = Environment::new(
            Cow::Borrowed(&trait_member_premise),
            table,
            normalizer::NO_OP,
        );

        implemented_constant_type_check(
            implementation_member_id,
            trait_member_id,
            &trait_to_impl_inst,
            &impl_member_generic_params,
            &trait_member_generic_params,
            &impl_member_env,
            handler,
        );

        implemented_predicate_check(
            table,
            implementation_member_id,
            trait_member_id,
            &trait_to_impl_inst,
            &impl_member_env,
            handler,
        );

        extraneous_predicate_check(
            table,
            implementation_member_id,
            &trait_to_impl_inst,
            &trait_member_env,
            handler,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn implemented_constant_type_check(
    implementation_member_id: GlobalID,
    trait_member_id: GlobalID,
    trait_to_impl_inst: &Instantiation<Default>,
    impl_member_generic_params: &GenericParameters,
    trait_member_generic_params: &GenericParameters,
    environment: &Environment<Default, normalizer::NoOp>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for ((tr_const_id, tr_const_param), (im_const_id, im_const_param)) in
        trait_member_generic_params
            .constant_parameters_as_order()
            .zip(impl_member_generic_params.constant_parameters_as_order())
    {
        let mut tr_const_ty = tr_const_param.r#type.clone();
        instantiation::instantiate(&mut tr_const_ty, trait_to_impl_inst);

        match environment.compatible(
            &im_const_param.r#type,
            &tr_const_ty,
            Variance::Covariant,
        ) {
            Ok(Some(_)) => {
                // no need to check lifetimes related to the constant
            }

            Err(_) | Ok(None) => {
                handler.receive(Box::new(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: implementation_member_id,
                                id: im_const_id,
                            },
                        trait_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: trait_member_id,
                                id: tr_const_id,
                            },
                    },
                ));
            }
        }
    }
}

fn implemented_predicate_check(
    table: &Table,
    implementation_member_id: GlobalID,
    trait_member_id: GlobalID,
    trait_to_impl_inst: &Instantiation<Default>,
    environment: &Environment<Default, normalizer::NoOp>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let Ok(trait_member_where_clause) =
        table.query::<WhereClause>(trait_member_id)
    else {
        return;
    };
    let member_span = table
        .get::<LocationSpan>(implementation_member_id)
        .span
        .clone()
        .unwrap();

    for (predicate, span) in
        trait_member_where_clause.predicates.iter().map(|x| {
            let mut pred = x.predicate.clone();
            pred.instantiate(trait_to_impl_inst);

            (pred, x.span.clone())
        })
    {
        let checker = Checker::new(environment, handler);
        checker
            .predicate_satisfied(predicate, span)
            .into_iter()
            .for_each(|x| x.report(member_span.clone(), handler));
    }
}

fn extraneous_predicate_check(
    table: &Table,
    implementation_member_id: GlobalID,
    trait_to_impl_inst: &Instantiation<Default>,
    environment: &Environment<Default, normalizer::NoOp>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    // based on the environment from the trait member side, check if there's any
    // predicate on the implementation member side that's not satisfiable.

    let Ok(impl_member_where_clause) =
        table.query::<WhereClause>(implementation_member_id)
    else {
        return;
    };

    for (predicate, span) in
        impl_member_where_clause.predicates.iter().map(|x| {
            let mut pred = x.predicate.clone();
            pred.instantiate(trait_to_impl_inst);

            (pred, x.span.clone().unwrap()) // span should be present
        })
    {
        let checker = Checker::new(environment, handler);
        let errors =
            checker.predicate_satisfied(predicate.clone(), Some(span.clone()));

        let contains_error = errors.iter().any(|x| {
            x.is_unsatisfied() || x.is_implementation_is_not_general_enough()
        });

        if contains_error {
            handler.receive(Box::new(
                ExtraneousImplementationMemberPredicate {
                    trait_implementation_member_id: implementation_member_id,
                    predicate,
                    predicate_span: span.clone(),
                },
            ));
        }

        for (pred, decl_span, overflow) in
            errors.into_iter().filter_map(|x| x.into_undecidable().ok())
        {
            handler.receive(Box::new(UndecidablePredicate {
                predicate: pred,
                predicate_declaration_span: decl_span,
                instantiation_span: span.clone(),
                overflow_error: overflow,
            }));
        }
    }
}

#[cfg(test)]
mod test;
