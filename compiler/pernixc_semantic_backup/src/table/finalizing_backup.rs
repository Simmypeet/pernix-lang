//! Contains codes related to finalizing the table.
//!
//! Finalizing symbols occurs when the symbol is being used i.e.resolved, in the code. The symbol
//! must be finalized before it can be used in order to to make it semantically valid. We must
//! also check for cyclic dependency while finalizing the symbol.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::Arc,
};

use parking_lot::{Mutex, RwLockReadGuard};
use paste::paste;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{
    self,
    item::{LifetimeBoundOperand, TraitMemberBoundArgument},
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use super::{
    resolution::{Checking, Config, Resolution},
    Index, IndexMut, IndexRaw, Table,
};
use crate::{
    arena::ID,
    entity::{
        constant,
        lifetime::Lifetime,
        predicate::{
            self, ConstantType, Equals, Forall, LifetimeOutlives, Predicate, Premises, TypeOutlives,
        },
        r#type, Entity, GenericArguments, Model, Substitution,
    },
    error::{
        self, AmbiguousImplementation, CyclicDependency, GenericParameterDuplication,
        InvalidTypeInConstantTypePredicate, InvalidTypeInOutlivesPredicate,
        MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationConstantTypeParameter, MisorderedGenericParameter,
        PrivateEntityLeakedToPublicInterface, TraitMemberBoundArgumentMismatched,
        TraitMemberExpected, UnusedGenericParameterInImplementation,
        WhereClausePredicateNotSatisfied,
    },
    logic::{r#trait::LifetimeConstraint, unification, TraitSatisfiability},
    symbol::{
        self, Constant, ConstantParameter, ConstantParameterID, Enum, Function, Generic, GenericID,
        GenericKind, GlobalID, ImplementationKindID, ImplementationMemberID,
        ImplementationSignature, LifetimeParameter, LifetimeParameterID, LocalGenericParameterID,
        Struct, Symbolic, Trait, TraitMemberID, Type, TypeParameter, TypeParameterID,
    },
    table::state::{Constructing, ConstructingLock, State},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ImplementationUnifierConfig;

impl<S: Model> unification::Config<S> for ImplementationUnifierConfig {
    fn type_mappable(&self, unifier: &r#type::Type<S>, _: &r#type::Type<S>) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn constant_mappable(
        &self,
        unifier: &constant::Constant<S>,
        _: &constant::Constant<S>,
    ) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn lifetime_mappable(&self, unifier: &Lifetime<S>, _: &Lifetime<S>) -> bool {
        unifier.is_named()
    }
}

/// A struct which composed of the required checking and the span where the checking occurs.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct CheckingWithSpan {
    checking: Checking<Symbolic>,
    span: Span,
}

/// Is a struct implementing [`Config`] trait that stores the checking and the span where the
/// checking occurs.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Storage<'a> {
    checkings: &'a mut Vec<CheckingWithSpan>,
}

impl Config<Symbolic> for Storage<'_> {
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<Symbolic>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<Symbolic>> { None }

    fn constant_arguments_placeholder(&mut self) -> Option<constant::Constant<Symbolic>> { None }

    fn check(&mut self, checking: Checking<Symbolic>, span: Span) {
        self.checkings.push(CheckingWithSpan { checking, span });
    }
}

struct TraitResolverConfigAdapter<'a, 'b> {
    forall_lifetimes_by_name: HashMap<&'b str, Lifetime<Symbolic>>,
    inner: &'a mut dyn Config<Symbolic>,
}

impl Config<Symbolic> for TraitResolverConfigAdapter<'_, '_> {
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<Symbolic>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<Symbolic>> { None }

    fn constant_arguments_placeholder(&mut self) -> Option<constant::Constant<Symbolic>> { None }

    fn check(&mut self, checking: Checking<Symbolic>, span: Span) {
        self.inner.check(checking, span);
    }

    fn extra_lifetime_provider(&self, lifetime: &str) -> Option<Lifetime<Symbolic>> {
        self.forall_lifetimes_by_name.get(lifetime).copied()
    }
}

impl Table {
    // check all the required checking and report to the handler
    #[allow(clippy::too_many_lines)]
    fn check(
        &self,
        global_id: GlobalID,
        checkings: impl Iterator<Item = CheckingWithSpan>,
        handler: &dyn Handler<error::Error>,
    ) {
        let checkings = checkings.collect::<Vec<_>>();

        println!(
            "{} required {checkings:#?}",
            self.get_qualified_name(global_id).unwrap()
        );

        let premises = self.get_premise_predicates(global_id).unwrap();
        let premises = Premises::from_predicates(premises.into_iter().map(|x| x.predicate));

        for checking in checkings {
            match checking.checking {
                Checking::Predicate(predicate) => match predicate {
                    Predicate::LifetimeOutlives(x) => {
                        if !x.satisfies(&premises, self) {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::LifetimeOutlives(x),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                    Predicate::TypeOutlives(x) => {
                        if !x.satisfies(&premises, self) {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::TypeOutlives(x),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                    Predicate::TypeEquals(type_equals) => {
                        if !type_equals.satisfies(&premises, self) {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::TypeEquals(type_equals),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                    Predicate::ConstantEquals(constant_type) => {
                        if !constant_type.satisfies(&premises, self) {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::ConstantEquals(constant_type),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                    Predicate::Trait(trait_bound) => {
                        if let TraitSatisfiability::Satisfiable(x) =
                            trait_bound.satisfies(&premises, self)
                        {
                            println!("satisfiable with {x:#?}");
                            for constraint in x {
                                match constraint {
                                    LifetimeConstraint::LifetimeOutlives(x) => {
                                        if !x.satisfies(&premises, self) {
                                            handler.receive(
                                                error::Error::WhereClausePredicateNotSatisfied(
                                                    WhereClausePredicateNotSatisfied {
                                                        predicate: Predicate::LifetimeOutlives(x),
                                                        span: checking.span.clone(),
                                                    },
                                                ),
                                            );
                                        }
                                    }
                                    LifetimeConstraint::TypeOutlives(x) => {
                                        if !x.satisfies(&premises, self) {
                                            handler.receive(
                                                error::Error::WhereClausePredicateNotSatisfied(
                                                    WhereClausePredicateNotSatisfied {
                                                        predicate: Predicate::TypeOutlives(x),
                                                        span: checking.span.clone(),
                                                    },
                                                ),
                                            );
                                        }
                                    }
                                }
                            }
                        } else {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::Trait(trait_bound),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                    Predicate::ConstantType(constant_type) => {
                        if !constant_type.satisfies(&premises, self) {
                            handler.receive(error::Error::WhereClausePredicateNotSatisfied(
                                WhereClausePredicateNotSatisfied {
                                    predicate: Predicate::ConstantType(constant_type),
                                    span: checking.span,
                                },
                            ));
                        }
                    }
                },
                Checking::TypeCheck(_, _) => todo!(),
            }
        }

        // TlODO: implement this
    }

    // create a lifetime outlives predicate for the given generic symbol
    fn create_trait_member_predicate<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        trait_member_bound: syntax_tree::item::TraitMemberBound,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T> + IndexMut<T>,
        <Self as Index<T>>::Output: Generic,
    {
        let span = trait_member_bound.span();
        let (operand_syn, _, argument_syn) = trait_member_bound.dissolve();

        let Ok(resolution) = self.resolve(&operand_syn, generic_id.into(), config, handler) else {
            return;
        };

        match (resolution, argument_syn) {
            // trait type
            (Resolution::TraitType(trait_ty), TraitMemberBoundArgument::Type(ty)) => {
                let Ok(resolve_ty) = self.resolve_type(&ty, generic_id.into(), config, handler)
                else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();
                generic_symbol
                    .generic_declaration_mut()
                    .predicates
                    .push(symbol::Predicate {
                        predicate: predicate::Predicate::TypeEquals(Equals {
                            lhs: r#type::Type::TraitMember(r#type::TraitMember {
                                trait_type_id: trait_ty.member.id,
                                trait_generic_arguments: trait_ty.parent_generic_arguments,
                                member_generic_arguments: trait_ty.member.generic_arguments,
                            }),
                            rhs: resolve_ty,
                        }),
                        span: Some(span),
                        explicit: true,
                    });
            }

            // trait constant
            (
                Resolution::TraitConstant(trait_constant),
                TraitMemberBoundArgument::Constant(constant),
            ) => {
                let Ok(evaluated_constant) =
                    self.evaluate(constant.expression(), generic_id.into(), handler)
                else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();
                generic_symbol
                    .generic_declaration_mut()
                    .predicates
                    .push(symbol::Predicate {
                        predicate: predicate::Predicate::ConstantEquals(Equals {
                            lhs: constant::Constant::TraitMember(constant::TraitMember {
                                trait_constant_id: trait_constant.member,
                                trait_arguments: trait_constant.parent_generic_arguments,
                            }),
                            rhs: evaluated_constant,
                        }),
                        span: Some(span),
                        explicit: true,
                    });
            }

            // mismatched type
            (Resolution::TraitType(..) | Resolution::TraitConstant(..), mismatched) => handler
                .receive(error::Error::TraitMemberBoundArgumentMismatched(
                    TraitMemberBoundArgumentMismatched {
                        trait_member_bound_argument_span: mismatched.span(),
                    },
                )),

            (resolved, _) => {
                handler.receive(error::Error::TraitMemberExpected(TraitMemberExpected {
                    non_trait_member_span: operand_syn.span(),
                    resolved,
                }));
            }
        }
    }

    // create a lifetime outlives predicate for the given generic symbol
    fn create_reigon_outlives_predicate<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        lifetime_bound: syntax_tree::item::LifetimeBound,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T> + IndexMut<T>,
        <Self as Index<T>>::Output: Generic,
    {
        let span = lifetime_bound.span();
        let (operand_syn, _, argument_syns) = lifetime_bound.dissolve();
        let mut arguments = Vec::new();

        for argument_syn in argument_syns.into_elements() {
            let Ok(argument) =
                self.resolve_lifetime(&argument_syn, generic_id.into(), config, handler)
            else {
                continue;
            };

            arguments.push(argument);
        }

        match operand_syn {
            LifetimeBoundOperand::LifetimeParameter(lt_parameter) => {
                let Ok(lifetime_parameter_id) = self.resolve_lifetime_parameter(
                    lt_parameter.identifier(),
                    generic_id.into(),
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).expect("should've been valid");

                for argument in arguments {
                    generic_symbol
                        .generic_declaration_mut()
                        .predicates
                        .push(symbol::Predicate {
                            predicate: predicate::Predicate::LifetimeOutlives(LifetimeOutlives {
                                operand: Lifetime::Named(lifetime_parameter_id),
                                argument,
                            }),
                            span: Some(span.clone()),
                            explicit: true,
                        });
                }
            }
            LifetimeBoundOperand::QualifiedIdentifier(qualified_identifier) => {
                let Ok(ty) = self.resolve_qualified_identifier_type(
                    &qualified_identifier,
                    generic_id.into(),
                    config,
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();

                let ty @ (r#type::Type::Parameter(..) | r#type::Type::TraitMember(..)) = ty else {
                    handler.receive(error::Error::InvalidTypeInOutlivesPredicate(
                        InvalidTypeInOutlivesPredicate {
                            outlive_predicate_span: span,
                            invalid_type: ty,
                        },
                    ));
                    return;
                };

                for argument in arguments {
                    generic_symbol
                        .generic_declaration_mut()
                        .predicates
                        .push(symbol::Predicate {
                            predicate: predicate::Predicate::TypeOutlives(TypeOutlives {
                                operand: ty.clone(),
                                argument,
                            }),
                            span: Some(span.clone()),
                            explicit: true,
                        });
                }
            }
        }
    }

    // create a constant type predicate
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_constant_type_predicate<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        constant_type_bound: &syntax_tree::item::ConstantTypeBound,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T> + IndexMut<T>,
        <Self as Index<T>>::Output: Generic,
    {
        let Ok(resolved_ty) = self.resolve_qualified_identifier_type(
            constant_type_bound.qualified_identifier(),
            generic_id.into(),
            config,
            handler,
        ) else {
            return;
        };

        let ty @ (r#type::Type::Parameter(..) | r#type::Type::TraitMember(..)) = resolved_ty else {
            handler.receive(error::Error::InvalidTypeInConstantTypePredicate(
                InvalidTypeInConstantTypePredicate {
                    constant_type_predicate_span: constant_type_bound.span(),
                    invalid_type: resolved_ty,
                },
            ));
            return;
        };

        let mut generic_symbol = self.get_mut(generic_id).unwrap();

        generic_symbol
            .generic_declaration_mut()
            .predicates
            .push(symbol::Predicate {
                predicate: predicate::Predicate::ConstantType(ConstantType { r#type: ty }),
                span: Some(constant_type_bound.span()),
                explicit: true,
            });
    }

    // create a trait predicate
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_trait_predicate<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        trait_bound: &syntax_tree::item::TraitBound,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T> + IndexMut<T>,
        <Self as Index<T>>::Output: Generic,
    {
        if self
            .resolve_trait_path(
                trait_bound.qualified_identifier(),
                generic_id.into(),
                handler,
            )
            .is_err()
        {
            return;
        }

        // creates higher-ranked lifetime maps
        let higher_ranked_lifetimes = {
            let mut higher_ranked_lifetimes = HashMap::new();

            if let Some(higher_ranked_lifetimes_syns) =
                trait_bound.higher_ranked_lifetime_parameters().as_ref()
            {
                for higher_ranked_lifetime_syn in higher_ranked_lifetimes_syns
                    .lifetime_parameter_list()
                    .elements()
                {
                    match higher_ranked_lifetimes
                        .entry(higher_ranked_lifetime_syn.identifier().span.str())
                    {
                        Entry::Occupied(_) => {
                            handler.receive(error::Error::HigherRankedLifetimeRedeclaration(
                                error::HigherRankedLifetimeRedeclaration {
                                    redeclaration_span: higher_ranked_lifetime_syn.span(),
                                },
                            ));
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Lifetime::Forall(Forall::generate()));
                        }
                    }
                }
            };

            higher_ranked_lifetimes
        };

        let mut adapter = TraitResolverConfigAdapter {
            forall_lifetimes_by_name: higher_ranked_lifetimes,
            inner: config,
        };

        let Ok(resolution) = self
            .resolve(
                trait_bound.qualified_identifier(),
                generic_id.into(),
                &mut adapter,
                handler,
            )
            .map(|x| {
                x.into_trait()
                    .expect("should be a trait as we've resolved ealier")
            })
        else {
            return;
        };

        self.get_mut(generic_id)
            .unwrap()
            .generic_declaration_mut()
            .predicates
            .push(symbol::Predicate {
                predicate: Predicate::Trait(predicate::Trait {
                    trait_id: resolution.id,
                    const_trait: trait_bound.const_keyword().is_some(),
                    generic_arguments: resolution.generic_arguments.into_other_model(),
                }),
                span: Some(trait_bound.span()),
                explicit: true,
            });
    }

    // create a list of predicates for the where clause
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_where_clause_predicates<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        syntax_tree: Option<syntax_tree::item::WhereClause>,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T> + IndexMut<T>,
        <Self as Index<T>>::Output: Generic,
    {
        let Some(where_clause) = syntax_tree else {
            return;
        };

        for clause in where_clause.dissolve().2.into_elements() {
            match clause {
                syntax_tree::item::Predicate::TraitMember(trait_member_bound) => {
                    self.create_trait_member_predicate(
                        generic_id,
                        trait_member_bound,
                        config,
                        handler,
                    );
                }
                syntax_tree::item::Predicate::Trait(trait_bound) => {
                    self.create_trait_predicate(generic_id, &trait_bound, config, handler);
                }
                syntax_tree::item::Predicate::Lifetime(lifetime_bound) => self
                    .create_reigon_outlives_predicate(generic_id, lifetime_bound, config, handler),
                syntax_tree::item::Predicate::ConstantType(constant_type) => {
                    self.create_constant_type_predicate(
                        generic_id,
                        &constant_type,
                        config,
                        handler,
                    );
                }
            }
        }
    }

    // create a generic parameter for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_generic_parameters<T: Into<GenericID> + Into<GlobalID> + Copy>(
        &self,
        generic_id: T,
        syntax_tree: Option<syntax_tree::item::GenericParameters>,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: IndexRaw<T>,
        <Self as Index<T>>::Output: Generic,
    {
        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree
            .into_iter()
            .flat_map(|x| x.dissolve().1.into_elements())
        {
            match parameter {
                syntax_tree::item::GenericParameter::Constant(constant) => {
                    // extract out the default constant syntax tree
                    let (identifier, _, type_syn, default) = constant.dissolve();

                    constant_parameter_syns.push((identifier, type_syn));

                    if let Some(default) = default {
                        default_constant_syns.push(default);
                    } else if !default_type_syns.is_empty() || !default_constant_syns.is_empty() {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Type(type_syn)
                    if constant_parameter_syns.is_empty() =>
                {
                    // extract out the default type syntax tree
                    let (type_syn, default) = type_syn.dissolve();

                    type_parameter_syns.push(type_syn);

                    if let Some(default) = default {
                        default_type_syns.push(default);
                    } else if !default_type_syns.is_empty() || !default_constant_syns.is_empty() {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Lifetime(lifetiem)
                    if constant_parameter_syns.is_empty() && type_parameter_syns.is_empty() =>
                {
                    lifetime_parameter_syns.push(lifetiem);
                }
                arg => {
                    handler.receive(error::Error::MisorderedGenericParameter(
                        MisorderedGenericParameter {
                            generic_kind: match arg {
                                syntax_tree::item::GenericParameter::Lifetime(_) => {
                                    GenericKind::Lifetime
                                }
                                syntax_tree::item::GenericParameter::Type(_) => GenericKind::Type,
                                syntax_tree::item::GenericParameter::Constant(_) => {
                                    GenericKind::Constant
                                }
                            },
                            generic_parameter_span: arg.span(),
                        },
                    ));
                    return;
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            handler.receive(error::Error::DefaultGenericParameterMustBeTrailing(
                error::DefaultGenericParameterMustBeTrailing {
                    invalid_generic_default_parameter_spans: default_type_syns
                        .iter()
                        .map(SourceElement::span)
                        .chain(default_constant_syns.iter().map(SourceElement::span))
                        .collect(),
                },
            ));
        }

        let mut generic_symbol = self.get_raw(generic_id).unwrap().upgradable_read();

        macro_rules! insert_generic_parameter {
            ($syns:ident, $kind:ident, $parameter:ident, $ident_expr:expr, $span_expr:expr, $parameter_expr:expr) => {
                paste! {
                    for $parameter in $syns {
                        let parameter_expr = $parameter_expr;

                        generic_symbol.with_upgraded(|generic_symbol| {
                            if let Err((existing_id, ..)) = generic_symbol
                                .generic_declaration_mut()
                                .parameters
                                .[<$kind:lower s>]
                                .insert($ident_expr, parameter_expr)
                            {
                                handler.receive(error::Error::[<$kind ParameterDuplication>](
                                    GenericParameterDuplication {
                                        existing_generic_parameter_id: [<$kind ParameterID>] {
                                            parent: generic_id.into(),
                                            id: existing_id,
                                        },
                                        duplicating_generic_parameter_span: $span_expr,
                                    },
                                ));
                            }
                        });
                    }
                }
            };
        }

        let this_accessibility = self
            .get_accessibility(generic_id.into())
            .expect("should be a valid id");

        insert_generic_parameter!(
            lifetime_parameter_syns,
            Lifetime,
            parameter,
            parameter.identifier().span.str().to_owned(),
            parameter.span(),
            LifetimeParameter {
                name: parameter.identifier().span.str().to_owned(),
                bound_kind: crate::symbol::BoundKind::Early,
                parent_generic_id: generic_id.into(),
                span: Some(parameter.span()),
            }
        );

        insert_generic_parameter!(
            type_parameter_syns,
            Type,
            parameter,
            parameter.span.str().to_owned(),
            parameter.span(),
            TypeParameter {
                name: parameter.span.str().to_owned(),
                parent_generic_id: generic_id.into(),
                span: Some(parameter.span()),
            }
        );

        insert_generic_parameter!(
            constant_parameter_syns,
            Constant,
            parameter,
            parameter.0.span.str().to_owned(),
            parameter.0.span(),
            {
                let constant_type = self
                    .resolve_type(&parameter.1, generic_id.into(), config, handler)
                    .unwrap_or_default();

                let ty_accessibility = self
                    .get_type_overall_accessibility(&constant_type)
                    .expect("should be valid");

                // no private type in public interface
                if this_accessibility > ty_accessibility {
                    handler.receive(error::Error::PrivateTypeLeakedToPublicInterface(
                        PrivateEntityLeakedToPublicInterface {
                            entity: constant_type.clone(),
                            leaked_span: parameter.1.span(),
                            public_interface_id: generic_id.into(),
                        },
                    ));
                }

                // type must be a type constant
                config.check(
                    Checking::Predicate(Predicate::ConstantType(ConstantType {
                        r#type: constant_type.clone(),
                    })),
                    parameter.1.span(),
                );

                ConstantParameter {
                    name: parameter.0.span.str().to_owned(),
                    parent_generic_id: generic_id.into(),
                    r#type: constant_type,
                    span: Some(parameter.0.span()),
                }
            }
        );

        if errornous_default_parameters {
            return;
        }

        // create default parameters
        for default_ty in default_type_syns {
            let ty = self
                .resolve_type(default_ty.value(), generic_id.into(), config, handler)
                .unwrap_or_default();

            // no private type in public interface
            if this_accessibility
                > self
                    .get_type_overall_accessibility(&ty)
                    .expect("should be valid")
            {
                handler.receive(error::Error::PrivateTypeLeakedToPublicInterface(
                    PrivateEntityLeakedToPublicInterface {
                        entity: ty.clone(),
                        leaked_span: default_ty.span(),
                        public_interface_id: generic_id.into(),
                    },
                ));
            }

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_type_parameters
                    .push(ty);
            });
        }

        // create default constant parameters
        let starting_index = generic_symbol
            .generic_declaration()
            .parameters
            .constants
            .len()
            - default_constant_syns.len();

        for (idx, default_constant) in default_constant_syns.into_iter().enumerate() {
            let default_constant_value = self
                .evaluate_functional(default_constant.value(), generic_id.into(), handler)
                .unwrap_or_default();

            let expected_ty = generic_symbol
                .generic_declaration()
                .parameters
                .constants
                .get(ID::new(idx + starting_index))
                .expect("should be a valid index")
                .r#type
                .clone();

            // no private constant in public interface
            if this_accessibility
                > self
                    .get_constant_overall_accessibility(&default_constant_value)
                    .expect("should be valid")
            {
                handler.receive(error::Error::PrivateConstantLeakedToPublicInterface(
                    PrivateEntityLeakedToPublicInterface {
                        entity: default_constant_value.clone(),
                        leaked_span: default_constant.value().span(),
                        public_interface_id: generic_id.into(),
                    },
                ));
            }

            // add type checking
            config.check(
                Checking::TypeCheck(default_constant_value.clone(), expected_ty),
                default_constant.value().span(),
            );

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_constant_parameters
                    .push(default_constant_value);
            });
        }
    }

    fn finalize_struct(
        &self,
        struct_id: ID<Struct>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Struct,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_enum(
        &self,
        enum_id: ID<Enum>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::EnumSignature,
        handler: &dyn Handler<error::Error>,
    ) {
        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        let (.., geeneric_arguments, where_clause) = syntax_tree.dissolve();

        // create generic parameters and where clause predicates
        self.create_generic_parameters(enum_id, geeneric_arguments, &mut storage, handler);
        self.create_where_clause_predicates(enum_id, where_clause, &mut storage, handler);

        drop(constructing_lock);

        self.check(enum_id.into(), checking_with_spans.into_iter(), handler);
    }

    fn finalize_constant(
        &self,
        constant_id: ID<Constant>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Constant,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    #[allow(clippy::needless_pass_by_value)]
    fn finalize_type(
        &self,
        type_id: ID<Type>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Type,
        handler: &dyn Handler<error::Error>,
    ) {
        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        // dissolve the syntax tree into parts
        let (_, signature, definition, _) = syntax_tree.dissolve();
        let (_, _, generic_parameters) = signature.dissolve();
        let (_, ty_syn, where_clause) = definition.dissolve();

        // create generic parameters and where clause predicates
        self.create_generic_parameters(type_id, generic_parameters, &mut storage, handler);
        self.create_where_clause_predicates(type_id, where_clause, &mut storage, handler);

        if let Ok(ty) = self.resolve_type(&ty_syn, type_id.into(), &mut storage, handler) {
            // no private type in public interface
            if self
                .get_accessibility(type_id.into())
                .expect("should be valid")
                > self
                    .get_type_overall_accessibility(&ty)
                    .expect("should be valid")
            {
                handler.receive(error::Error::PrivateTypeLeakedToPublicInterface(
                    PrivateEntityLeakedToPublicInterface {
                        entity: ty.clone(),
                        leaked_span: ty_syn.span(),
                        public_interface_id: type_id.into(),
                    },
                ));
            }

            self.get_mut(type_id).unwrap().r#type = ty;
        }

        // done, symbol is ready to be used
        drop(constructing_lock);

        // do checking and report to the handler
        self.check(type_id.into(), checking_with_spans.into_iter(), handler);
    }

    fn finalize_function(
        &self,
        function_id: ID<Function>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Function,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    #[allow(clippy::too_many_lines)]
    fn finalize_trait(
        &self,
        trait_id: ID<Trait>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::TraitSignature,
        handler: &dyn Handler<error::Error>,
    ) {
        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        let (.., generic_parameters_syn, where_clause_syn) = syntax_tree.dissolve();

        // create generic parameters and where clause predicates
        self.create_generic_parameters(trait_id, generic_parameters_syn, &mut storage, handler);
        self.create_where_clause_predicates(trait_id, where_clause_syn, &mut storage, handler);

        // done, symbol is ready to be used
        drop(constructing_lock);

        // reconsider all the implementation
        {
            let mut trait_symbol = self.get_raw(trait_id).unwrap().upgradable_read();

            // constructs all the implementations
            trait_symbol
                .implementations
                .par_iter()
                .copied()
                .for_each(|implementation_id| {
                    println!("finalizing implementation: {implementation_id:?}");
                    let _ = self.finalize(implementation_id.into(), None, handler);
                });
            trait_symbol
                .negative_implementations
                .par_iter()
                .copied()
                .for_each(|negative_implementation_id| {
                    let _ = self.finalize(negative_implementation_id.into(), None, handler);
                });

            // gather all valid implementation key
            let candidate_implementations = trait_symbol
                .implementations
                .iter()
                .copied()
                .filter_map(|x| {
                    if self.check_implementation_generic_parameters_occurances(
                        x.into(),
                        &self.get(x).unwrap().signature,
                        trait_id,
                        handler,
                    ) {
                        Some(ImplementationKindID::Positive(x))
                    } else {
                        None
                    }
                })
                .chain(
                    trait_symbol
                        .negative_implementations
                        .iter()
                        .copied()
                        .filter_map(|x| {
                            if self.check_implementation_generic_parameters_occurances(
                                x.into(),
                                &self.get(x).unwrap().signature,
                                trait_id,
                                handler,
                            ) {
                                Some(ImplementationKindID::Negative(x))
                            } else {
                                None
                            }
                        }),
                );

            let mut valid_implementation_ids: Vec<ImplementationKindID> = Vec::new();

            'outer: for candidate in candidate_implementations {
                for &existing in &valid_implementation_ids {
                    let existing_generic_argument = match existing {
                        ImplementationKindID::Positive(id) => {
                            RwLockReadGuard::map(self.get(id).unwrap(), |id| {
                                &id.signature.arguments
                            })
                        }
                        ImplementationKindID::Negative(id) => {
                            RwLockReadGuard::map(self.get(id).unwrap(), |id| {
                                &id.signature.arguments
                            })
                        }
                    };

                    let candidate_generic_argument = match candidate {
                        ImplementationKindID::Positive(id) => {
                            RwLockReadGuard::map(self.get(id).unwrap(), |id| {
                                &id.signature.arguments
                            })
                        }
                        ImplementationKindID::Negative(id) => {
                            RwLockReadGuard::map(self.get(id).unwrap(), |id| {
                                &id.signature.arguments
                            })
                        }
                    };
                    let order = existing_generic_argument.order(&candidate_generic_argument);
                    drop((existing_generic_argument, candidate_generic_argument));

                    match order {
                        crate::logic::r#trait::Order::Incompatible
                        | crate::logic::r#trait::Order::MoreGeneral
                        | crate::logic::r#trait::Order::MoreSpecific => {}
                        crate::logic::r#trait::Order::Ambiguous => {
                            handler.receive(error::Error::AmbiguousImplementation(
                                AmbiguousImplementation {
                                    first_implementation_id: existing,
                                    second_implementation_id: candidate,
                                },
                            ));
                            continue 'outer;
                        }
                    }
                }

                valid_implementation_ids.push(candidate);
            }

            trait_symbol.with_upgraded(|x| x.implementations.clear());
            trait_symbol.with_upgraded(|x| x.negative_implementations.clear());

            for implementation_id in valid_implementation_ids {
                match implementation_id {
                    ImplementationKindID::Positive(id) => {
                        trait_symbol.with_upgraded(|x| x.implementations.push(id));
                    }
                    ImplementationKindID::Negative(id) => {
                        trait_symbol.with_upgraded(|x| x.negative_implementations.push(id));
                    }
                }
            }
        }

        // do checking and report to the handler
        self.check(trait_id.into(), checking_with_spans.into_iter(), handler);
    }

    #[allow(clippy::significant_drop_tightening)]
    fn finalize_implementation(
        &self,
        implementation_id: ID<symbol::Implementation>,
        constructing_lock: ConstructingLock,
        signature_syn: syntax_tree::item::ImplementationSignature,
        handler: &dyn Handler<error::Error>,
    ) {
        let parent_trait_id = self.get(implementation_id).unwrap().signature.trait_id;
        let (_, generic_arguments_syn, _, qualified_identifier_syn, where_clause_syn) =
            signature_syn.dissolve();

        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        // create generic parameters and where clause predicates
        self.create_generic_parameters(
            implementation_id,
            generic_arguments_syn,
            &mut storage,
            handler,
        );
        self.create_where_clause_predicates(
            implementation_id,
            where_clause_syn,
            &mut storage,
            handler,
        );

        let generic_identifier = qualified_identifier_syn
            .rest()
            .last()
            .map_or(qualified_identifier_syn.first(), |(_, x)| x);

        let generic_argumnent = match self.resolve_generic_arguments(
            generic_identifier,
            implementation_id.into(),
            parent_trait_id.into(),
            &mut storage,
            handler,
        ) {
            Ok(Some(generic_argumnet)) => generic_argumnet,
            Ok(None) => unreachable!("the trait symbol is generic, should have generic arguments"),
            Err(_) => {
                // error, this implementation is invalid, no need to go further
                return;
            }
        };
        self.get_mut(implementation_id).unwrap().signature.arguments = generic_argumnent.clone();

        drop(constructing_lock);

        let substitution =
            Substitution::from_generic_arguments(generic_argumnent, parent_trait_id.into());

        let predicates = self
            .get(parent_trait_id)
            .unwrap()
            .generic_declaration
            .predicates
            .iter()
            .map(|x| x.predicate.clone())
            .collect::<Vec<_>>();

        // add additional required predicate to the storage
        for predicate in predicates {
            let mut predicate = predicate.into_other_model();
            predicate.apply(&substitution);

            storage.check(Checking::Predicate(predicate), generic_identifier.span());
        }

        self.check(
            implementation_id.into(),
            checking_with_spans.into_iter(),
            handler,
        );
        self.check_implementation_exlusitivity(
            &self.get(implementation_id).unwrap().signature.arguments,
            implementation_id.into(),
            parent_trait_id,
            handler,
        );
    }

    /// Checks if all generic parameters defined in the implementation occurs in the trait
    /// arguments.
    ///
    /// # Returns
    ///
    /// Returns `false` if not all generic parameters defined in the implementation occurs in the
    /// trait arguments.
    fn check_implementation_generic_parameters_occurances(
        &self,
        implementation_kind_id: ImplementationKindID,
        implementation_signature: &ImplementationSignature,
        parent_trait_id: ID<Trait>,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        let trait_symbol = self.get(parent_trait_id).unwrap();

        let all_lifetime_supplied = implementation_signature.arguments.lifetimes.len()
            == trait_symbol.generic_declaration.parameters.lifetimes.len();
        let all_type_supplied = implementation_signature.arguments.types.len()
            == trait_symbol.generic_declaration.parameters.types.len();
        let all_constant_supplied = implementation_signature.arguments.constants.len()
            == trait_symbol.generic_declaration.parameters.constants.len();

        drop(trait_symbol);

        // make sure that this implementation is valid.
        if !all_lifetime_supplied || !all_type_supplied || !all_constant_supplied {
            println!("{all_lifetime_supplied} {all_type_supplied} {all_constant_supplied}");
            return false;
        }

        let mut occurs = Occurs::default();
        occurs.generic_arguments_occurs(&implementation_signature.arguments);

        // check if every generic parameters occurs
        let mut found_unused = false;

        macro_rules! term_occured {
            ($term:ident) => {
                paste! {
                    for parameter_id in (0..implementation_signature
                        .generic_declaration
                        .parameters
                        .[<$term:lower s>]
                        .len())
                        .map(|x| [<$term ParameterID>] {
                            parent: implementation_kind_id.into(),
                            id: ID::new(x)
                        })
                    {
                        if !occurs
                            .[<$term:lower _parameters>]
                            .contains(&parameter_id)
                        {
                            found_unused = true;
                            handler.receive(error::Error::UnusedGenericParameterInImplementation(
                                UnusedGenericParameterInImplementation {
                                    generic_parameter_id: LocalGenericParameterID::$term(parameter_id.id),
                                    implementation_kind_id
                                }
                            ));
                        }
                    }
                }
            };
        }

        term_occured!(Lifetime);
        term_occured!(Type);
        term_occured!(Constant);

        // report non occured generic parameters
        !found_unused
    }

    fn check_implementation_exlusitivity(
        &self,
        trait_generic_argumnents: &GenericArguments<Symbolic>,
        implementation_kind_id: ImplementationKindID,
        parent_trait_id: ID<Trait>,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_negative_implementation(
        &self,
        negative_implementation_id: ID<symbol::NegativeImplementation>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::ImplementationSignature,
        handler: &dyn Handler<error::Error>,
    ) {
        let parent_trait_id = self
            .get(negative_implementation_id)
            .unwrap()
            .signature
            .trait_id;

        let (_, generic_arguments_syn, _, qualified_identifier_syn, where_clause_syn) =
            syntax_tree.dissolve();

        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        // create generic parameters and where clause predicates
        self.create_generic_parameters(
            negative_implementation_id,
            generic_arguments_syn,
            &mut storage,
            handler,
        );
        self.create_where_clause_predicates(
            negative_implementation_id,
            where_clause_syn,
            &mut storage,
            handler,
        );

        let generic_identifier = qualified_identifier_syn
            .rest()
            .last()
            .map_or(qualified_identifier_syn.first(), |(_, x)| x);

        let generic_argumnent = match self.resolve_generic_arguments(
            generic_identifier,
            negative_implementation_id.into(),
            parent_trait_id.into(),
            &mut storage,
            handler,
        ) {
            Ok(Some(generic_argumnet)) => generic_argumnet,
            Ok(None) => unreachable!("the trait symbol is generic, should have generic arguments"),
            Err(_) => {
                // error, this implementation is invalid, no need to go further
                return;
            }
        };
        self.get_mut(negative_implementation_id)
            .unwrap()
            .signature
            .arguments = generic_argumnent.clone();

        drop(constructing_lock);

        let substitution =
            Substitution::from_generic_arguments(generic_argumnent, parent_trait_id.into());

        let predicates = self
            .get(parent_trait_id)
            .unwrap()
            .generic_declaration
            .predicates
            .iter()
            .map(|x| x.predicate.clone())
            .collect::<Vec<_>>();

        // add additional required predicate to the storage
        for predicate in predicates {
            let mut predicate = predicate.into_other_model();
            predicate.apply(&substitution);

            storage.check(Checking::Predicate(predicate), generic_identifier.span());
        }

        self.check(
            negative_implementation_id.into(),
            checking_with_spans.into_iter(),
            handler,
        );
        self.check_implementation_exlusitivity(
            &self
                .get(negative_implementation_id)
                .unwrap()
                .signature
                .arguments,
            negative_implementation_id.into(),
            parent_trait_id,
            handler,
        );
    }

    fn finalize_trait_function(
        &self,
        trait_function_id: ID<symbol::TraitFunction>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::TraitFunction,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_trait_constant(
        &self,
        trait_constant_id: ID<symbol::TraitConstant>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::TraitConstant,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn check_implementation_type_compatibility(
        &self,
        parent_implementation_id: ID<symbol::Implementation>,
        implementation_type_id: ID<symbol::ImplementationType>,
        trait_type_id: ID<symbol::TraitType>,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        // TODO: check if the where clause of the implementation is **strictly** identical to the
        // trait.
        self.check_implementation_member_generic_parameters(
            parent_implementation_id,
            implementation_type_id,
            trait_type_id,
            handler,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn check_implementation_member_generic_parameters<
        T: Copy + Into<GlobalID> + Into<GenericID> + Into<ImplementationMemberID>,
        U: Copy + Into<GlobalID> + Into<GenericID> + Into<TraitMemberID>,
    >(
        &self,
        parent_implementation_id: ID<symbol::Implementation>,
        implementation_member_id: T,
        trait_member_id: U,
        handler: &dyn Handler<error::Error>,
    ) -> bool
    where
        Self: Index<T> + Index<U>,
        <Self as Index<T>>::Output: Generic,
        <Self as Index<U>>::Output: Generic,
    {
        let trait_member = self.get(trait_member_id).unwrap();
        let implementation_member = self.get(implementation_member_id).unwrap();

        let parent_generic_parameters = &trait_member.generic_declaration().parameters;
        let implementation_generic_parameters =
            &implementation_member.generic_declaration().parameters;

        let mut is_error = false;

        macro_rules! count_check {
            ($kind:ident) => {
                paste! {
                if parent_generic_parameters.[<$kind:lower s>].len()
                    != implementation_generic_parameters.[<$kind:lower s>].len()
                {
                    is_error = true;
                    handler.receive(
                        error::Error::MismatchedGenericParameterCountInImplementation(
                            MismatchedGenericParameterCountInImplementation {
                                implementation_member_id: implementation_member_id.into(),
                                trait_member_id: trait_member_id.into(),
                                expected_count: parent_generic_parameters.[<$kind:lower s>].len(),
                                declared_count: implementation_generic_parameters.[<$kind:lower s>].len(),
                                generic_kind: GenericKind::$kind,
                            },
                        ),
                    );
                }
                }
            };
        }

        count_check!(Lifetime);
        count_check!(Type);
        count_check!(Constant);

        if is_error {
            return false;
        }

        let premises = Premises::from_predicates(
            self.get_premise_predicates(implementation_member_id.into())
                .unwrap()
                .into_iter()
                .map(|x| x.predicate),
        );

        let mut substitution = Substitution::from_generic_arguments(
            self.get(parent_implementation_id)
                .unwrap()
                .signature
                .arguments
                .clone(),
            self.get(parent_implementation_id)
                .unwrap()
                .signature
                .trait_id
                .into(),
        );

        macro_rules! trait_to_implementation_generic_parameter {
            ($kind:ident, $kind2:path, $kind3:ident, $map:ident) => {
                paste! {
                for idx in 0..implementation_member
                    .generic_declaration()
                    .parameters
                    .[<$kind:lower s>]
                    .len()
                {
                    substitution.$map.insert(
                        $kind2::$kind3([<$kind ParameterID>] {
                            parent: trait_member_id.into(),
                            id: ID::new(idx),
                        }),
                        $kind2::$kind3([<$kind ParameterID>] {
                            parent: implementation_member_id.into(),
                            id: ID::new(idx),
                        }),
                    );
                }
                }
            };
        }

        trait_to_implementation_generic_parameter!(Lifetime, Lifetime, Named, lifetimes);
        trait_to_implementation_generic_parameter!(Type, r#type::Type, Parameter, types);
        trait_to_implementation_generic_parameter!(
            Constant,
            constant::Constant,
            Parameter,
            constants
        );

        #[allow(clippy::significant_drop_in_scrutinee)]
        for (idx, (trait_constant_parameter, implementation_constant_parameter)) in trait_member
            .generic_declaration()
            .parameters
            .constants
            .values()
            .zip(
                implementation_member
                    .generic_declaration()
                    .parameters
                    .constants
                    .values(),
            )
            .enumerate()
        {
            let mut ty_copied = trait_constant_parameter.r#type.clone();
            ty_copied.apply(&substitution);

            if !ty_copied.equals(&implementation_constant_parameter.r#type, &premises, self) {
                handler.receive(error::Error::MismatchedImplementationConstantTypeParameter(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_id: implementation_member_id.into(),
                        trait_member_id: trait_member_id.into(),
                        constant_parameter_index: idx,
                    },
                ));
                is_error = true;
            }
        }

        drop(trait_member);
        drop(implementation_member);

        !is_error
    }

    fn finalize_trait_type(
        &self,
        trait_type_id: ID<symbol::TraitType>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::TraitType,
        handler: &dyn Handler<error::Error>,
    ) {
        // parent trait must be finalized before
        let parent_trait_id = self.get(trait_type_id).unwrap().parent_trait_id;
        let _ = self.finalize(parent_trait_id.into(), Some(trait_type_id.into()), handler);

        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        let (signature, where_clause_syn, _) = syntax_tree.dissolve();
        let (_, _, generic_parameters_syn) = signature.dissolve();

        // create generic parameters and where clause predicates
        self.create_generic_parameters(
            trait_type_id,
            generic_parameters_syn,
            &mut storage,
            handler,
        );
        self.create_where_clause_predicates(trait_type_id, where_clause_syn, &mut storage, handler);

        drop(constructing_lock);

        let parent_trait = self.get(parent_trait_id).unwrap();
        for &implementation in &parent_trait.implementations {
            let Some(implementation_eq_id) = self
                .get(implementation)
                .unwrap()
                .implementation_type_ids_by_trait_type_id
                .get(&trait_type_id)
                .copied()
            else {
                continue;
            };

            let _ = self.finalize(implementation_eq_id.into(), None, handler);

            // if not compatible, remove the implementation
            if !self.check_implementation_type_compatibility(
                implementation,
                implementation_eq_id,
                trait_type_id,
                handler,
            ) {
                self.get_mut(implementation)
                    .unwrap()
                    .implementation_type_ids_by_trait_type_id
                    .remove(&trait_type_id);
            }
        }

        drop(parent_trait);

        self.check(
            trait_type_id.into(),
            checking_with_spans.into_iter(),
            handler,
        );
    }

    fn finalize_implementation_function(
        &self,
        implementation_function_id: ID<symbol::ImplementationFunction>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::ImplementationFunction,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_implementation_constant(
        &self,
        implementation_constant_id: ID<symbol::ImplementationConstant>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::ImplementationConstant,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_implementation_type(
        &self,
        implementation_type_id: ID<symbol::ImplementationType>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::ImplementationType,
        handler: &dyn Handler<error::Error>,
    ) {
        let parent_trait_id = self
            .get(
                self.get(implementation_type_id)
                    .unwrap()
                    .parent_implementation_id,
            )
            .unwrap()
            .signature
            .trait_id;

        let _ = self.finalize(
            parent_trait_id.into(),
            Some(implementation_type_id.into()),
            handler,
        );

        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        let (signature, type_definition, _) = syntax_tree.dissolve();
        let (_, _, generic_parameters_syn) = signature.dissolve();
        let (_, ty_syn, where_clause_syn) = type_definition.dissolve();

        // create generic parameters and where clause predicates
        self.create_generic_parameters(
            implementation_type_id,
            generic_parameters_syn,
            &mut storage,
            handler,
        );
        self.create_where_clause_predicates(
            implementation_type_id,
            where_clause_syn,
            &mut storage,
            handler,
        );

        let ty = self
            .resolve_type(
                &ty_syn,
                implementation_type_id.into(),
                &mut storage,
                handler,
            )
            .unwrap_or_default();

        // no private type in public interface
        if self
            .get_accessibility(implementation_type_id.into())
            .expect("should be valid")
            > self
                .get_type_overall_accessibility(&ty)
                .expect("should be valid")
        {
            handler.receive(error::Error::PrivateTypeLeakedToPublicInterface(
                PrivateEntityLeakedToPublicInterface {
                    entity: ty.clone(),
                    leaked_span: ty_syn.span(),
                    public_interface_id: implementation_type_id.into(),
                },
            ));
        }

        self.get_mut(implementation_type_id).unwrap().r#type = ty;

        drop(constructing_lock);

        self.check(
            implementation_type_id.into(),
            checking_with_spans.into_iter(),
            handler,
        );
    }

    fn finalize_variant(
        &self,
        variant_id: ID<symbol::Variant>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Variant,
        handler: &dyn Handler<error::Error>,
    ) {
        let parent_enum_id = self.get(variant_id).unwrap().parent_enum_id;
        let _ = self.finalize(parent_enum_id.into(), Some(variant_id.into()), handler);
        let (.., association) = syntax_tree.dissolve();

        if let Some(association) = association {
            let mut checking_with_spans = Vec::new();
            let mut storage = Storage {
                checkings: &mut checking_with_spans,
            };

            let (_, ty, _) = association.dissolve();

            let association_type = self
                .resolve_type(&ty, variant_id.into(), &mut storage, handler)
                .unwrap_or_default();

            // no private type in public interface
            if self
                .get_accessibility(variant_id.into())
                .expect("should be valid")
                > self
                    .get_type_overall_accessibility(&association_type)
                    .expect("should be valid")
            {
                handler.receive(error::Error::PrivateTypeLeakedToPublicInterface(
                    PrivateEntityLeakedToPublicInterface {
                        entity: association_type.clone(),
                        leaked_span: ty.span(),
                        public_interface_id: variant_id.into(),
                    },
                ));
            }

            self.get_mut(variant_id).unwrap().associated_type = Some(association_type);
        }

        drop(constructing_lock);
    }

    /// Returns `false` if found a cyclic dependency while finalizing.
    #[must_use]
    #[allow(
        clippy::significant_drop_tightening,
        clippy::too_many_lines,
        clippy::cognitive_complexity
    )]
    pub(super) fn finalize(
        &self,
        global_id: GlobalID,
        referring_site: Option<GlobalID>,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        /*
        This is one of the most complicated part of the compiler.
        Hopefully, it's correct because I'm not sure about it.
         */
        macro_rules! remove_dependency {
            ($state_manager:ident) => {
                paste! {
                    if let Some(referring_site) = referring_site {
                        $state_manager
                            .with_upgraded(|x| x.dependencies.remove(&referring_site));
                    }
                }
            };
        }

        macro_rules! match_finalize {
            ($state_manager:ident, $($kind:ident),*) => {
                match global_id {
                    $(
                        GlobalID::$kind(global_id) => {paste! {
                            if $state_manager
                                .[<states_by_ $kind:snake _id>]
                                .get(&global_id)
                                .is_none() {
                                remove_dependency!($state_manager);
                                return true;
                            }

                            let result = $state_manager.with_upgraded(|state_manager| {
                                let Some(state) = state_manager
                                    .[<states_by_ $kind:snake _id>]
                                    .get_mut(&global_id)
                                else {
                                    panic!("should've exited above");
                                };

                                match state {
                                    drafted @ State::Drafted(..) => {
                                        let mut inner_drafted = None;
                                        let waiting_lock = Arc::new(Mutex::new(()));

                                        take_mut::take(drafted, |drafted| {
                                            inner_drafted = Some(drafted.into_drafted().unwrap());

                                            State::Constructing(Constructing {
                                                waiting_lock: waiting_lock.clone(),
                                            })
                                        });


                                        Ok((waiting_lock, inner_drafted.unwrap()))
                                    }
                                    State::Constructing(constructing) =>
                                        Err(constructing
                                            .waiting_lock
                                            .clone()),
                                }
                            });

                            // preven deadlock
                            drop($state_manager);

                            match result {
                                Ok((waiting_lock, inner_drafted)) => {
                                    let constructing_lock = ConstructingLock::new(
                                        global_id.into(),
                                        waiting_lock.lock(),
                                        self,
                                        referring_site
                                    );

                                    self.[<finalize_ $kind:snake>](
                                        global_id,
                                        constructing_lock,
                                        inner_drafted,
                                        handler
                                    );

                                    return true;
                                }
                                Err(constructing) => {
                                    constructing
                                }
                            }
                        }},
                    )*
                    _ => {
                        remove_dependency!($state_manager);
                        return true
                    },
                }
            };
        }

        let mut state_manager = self.state_manager.upgradable_read();

        if let Some(referring_site) = referring_site {
            if referring_site == global_id {
                handler.receive(error::Error::CyclicDependency(CyclicDependency {
                    participants: vec![global_id],
                }));

                return false;
            }

            // dependency cyclic check starts here
            let mut dependency_stack = vec![global_id];
            let mut current_node = global_id;

            while let Some(dependant) = state_manager.dependencies.get(&current_node).copied() {
                dependency_stack.push(dependant);

                // cyclic dependency found
                if dependant == referring_site {
                    println!("cyclic dependency found: {:?}", state_manager.dependencies);
                    let dependency_stack_set = dependency_stack.iter().copied().collect();

                    let reported = !state_manager
                        .reported_cyclic_dependencies
                        .contains(&dependency_stack_set);
                    if reported {
                        state_manager.with_upgraded(|x| {
                            x.reported_cyclic_dependencies.insert(dependency_stack_set)
                        });
                        handler.receive(error::Error::CyclicDependency(CyclicDependency {
                            participants: dependency_stack,
                        }));
                    }

                    return false;
                }

                current_node = dependant;
            }

            // no cyclic dependency, add it to the dependency graph
            state_manager.with_upgraded(|x| x.dependencies.insert(referring_site, global_id));
        }

        let waiting_lock = match_finalize!(
            state_manager,
            Enum,
            Variant,
            Struct,
            Constant,
            Type,
            Function,
            Trait,
            TraitFunction,
            TraitType,
            TraitConstant,
            Implementation,
            NegativeImplementation,
            ImplementationFunction,
            ImplementationConstant,
            ImplementationType
        );

        let s = waiting_lock.lock();
        drop(s);

        if let Some(referring_site) = referring_site {
            self.state_manager
                .write()
                .dependencies
                .remove(&referring_site);
        }

        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Occurs {
    type_parameters: HashSet<TypeParameterID>,
    constant_parameters: HashSet<ConstantParameterID>,
    lifetime_parameters: HashSet<LifetimeParameterID>,
}

impl Occurs {
    fn type_occurs(&mut self, ty: &r#type::Type<Symbolic>) {
        match ty {
            r#type::Type::Primitive(_)
            | r#type::Type::Inference(_)
            | r#type::Type::TraitMember(_) => {}

            r#type::Type::Algebraic(adt) => {
                self.generic_arguments_occurs(&adt.generic_arguments);
            }
            r#type::Type::Pointer(pointer) => {
                self.type_occurs(&pointer.pointee);
            }
            r#type::Type::Reference(reference) => {
                self.lifetime_occurs(&reference.lifetime);
                self.type_occurs(&reference.pointee);
            }
            r#type::Type::Array(array) => {
                self.type_occurs(&array.element);
                self.constant_occurs(&array.length);
            }
            r#type::Type::Parameter(ty) => {
                self.type_parameters.insert(*ty);
            }
            r#type::Type::Tuple(ty) => {
                for element in &ty.elements {
                    match element {
                        r#type::TupleElement::Regular(reg) => self.type_occurs(reg),
                        r#type::TupleElement::Unpacked(r#type::Unpacked::Parameter(param)) => {
                            self.type_parameters.insert(*param);
                        }
                        r#type::TupleElement::Unpacked(r#type::Unpacked::TraitMember(..)) => {}
                    }
                }
            }
        }
    }

    fn lifetime_occurs(&mut self, lifetime: &Lifetime<Symbolic>) {
        match lifetime {
            Lifetime::Forall(_) | Lifetime::Static | Lifetime::Local(_) => {}
            Lifetime::Named(lifetime) => {
                self.lifetime_parameters.insert(*lifetime);
            }
        }
    }

    fn constant_occurs(&mut self, constants: &constant::Constant<Symbolic>) {
        match constants {
            constant::Constant::Primitive(..)
            | constant::Constant::Inference(..)
            | constant::Constant::TraitMember(..) => {}

            constant::Constant::Struct(constant) => {
                for field in &constant.fields {
                    self.constant_occurs(field);
                }
            }

            constant::Constant::Enum(constant) => {
                if let Some(val) = &constant.associated_value {
                    self.constant_occurs(val);
                }
            }

            constant::Constant::Array(array) => {
                for val in &array.elements {
                    self.constant_occurs(val);
                }
            }
            constant::Constant::Parameter(parameter) => {
                self.constant_parameters.insert(*parameter);
            }
            constant::Constant::Tuple(tuple) => {
                for element in &tuple.elements {
                    match element {
                        constant::TupleElement::Regular(reg) => self.constant_occurs(reg),
                        constant::TupleElement::Unpacked(constant::Unpacked::Parameter(param)) => {
                            self.constant_parameters.insert(*param);
                        }
                        constant::TupleElement::Unpacked(constant::Unpacked::TraitMember(..)) => {}
                    }
                }
            }
        }
    }

    fn generic_arguments_occurs(&mut self, generic_arguments: &GenericArguments<Symbolic>) {
        for lifetime in &generic_arguments.lifetimes {
            self.lifetime_occurs(lifetime);
        }

        for ty in &generic_arguments.types {
            self.type_occurs(ty);
        }

        for constant in &generic_arguments.constants {
            self.constant_occurs(constant);
        }
    }
}
