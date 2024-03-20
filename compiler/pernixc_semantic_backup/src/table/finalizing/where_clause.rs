use std::collections::{hash_map::Entry, HashMap};

use paste::paste;
use pernixc_base::source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self,
    item::{LifetimeBoundOperand, TraitMemberBoundArgument},
};

use crate::{
    arena::ID,
    entity::{
        constant,
        lifetime::Lifetime,
        predicate::{
            self, ConstantType, Equals, Forall, LifetimeOutlives, Predicate,
            TypeOutlives,
        },
        r#type, Entity, GenericArguments,
    },
    error::{
        self, InvalidTypeInConstantTypePredicate,
        InvalidTypeInOutlivesPredicate, TraitMemberBoundArgumentMismatched,
        TraitMemberExpected,
    },
    symbol::{
        self, Constant, Enum, Function, Generic, GenericID, GlobalID,
        Implementation, ImplementationConstant, ImplementationFunction,
        ImplementationType, NegativeImplementation, Struct, Symbolic, Trait,
        TraitConstant, TraitFunction, TraitType, Type, Variant,
    },
    table::{
        resolution::{self, Checking, Resolution},
        state, Index, IndexMut, IndexRaw, Table,
    },
};

pub(in crate::table) trait Finalize<Syn>
where
    Self: Sized,
{
    fn finalize(
        table: &Table,
        syntax_tree: &Syn,
        config: &mut state::Config<Self>,
    );
}

struct TraitResolverConfigAdapter<'a, 'b> {
    forall_lifetimes_by_name: HashMap<&'b str, Lifetime<Symbolic>>,
    inner: &'a mut dyn resolution::Config<Symbolic>,
}

impl resolution::Config<Symbolic> for TraitResolverConfigAdapter<'_, '_> {
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<Symbolic>> {
        self.inner.lifetime_arguments_placeholder()
    }

    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<Symbolic>> {
        self.inner.type_arguments_placeholder()
    }

    fn constant_arguments_placeholder(
        &mut self,
    ) -> Option<constant::Constant<Symbolic>> {
        self.inner.constant_arguments_placeholder()
    }

    fn check(&mut self, checking: Checking<Symbolic>, span: Span) {
        self.inner.check(checking, span);
    }

    fn extra_lifetime_provider(
        &self,
        lifetime: &str,
    ) -> Option<Lifetime<Symbolic>> {
        self.forall_lifetimes_by_name.get(lifetime).copied()
    }

    fn on_global_id_resolved(&mut self, global_id: GlobalID) {
        self.inner.on_global_id_resolved(global_id);
    }

    fn on_generic_arguments_resolved(
        &mut self,
        generic_id: GenericID,
        generic_arguments: &GenericArguments<Symbolic>,
    ) {
        self.inner.on_generic_arguments_resolved(generic_id, generic_arguments);
    }

    fn on_resolved(&mut self, resolution: &Resolution<Symbolic>) {
        self.inner.on_resolved(resolution);
    }
}

impl Table {
    fn create_trait_member_predicate<T>(
        &self,
        trait_member_bound: &syntax_tree::item::TraitMemberBound,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T> + IndexMut<ID<T>>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let handler = config.handler();
        let span = trait_member_bound.span();

        let Ok(resolution) = self.resolve(
            trait_member_bound.qualified_identifier(),
            config.current_id().into(),
            config,
            handler,
        ) else {
            return;
        };

        match (resolution, trait_member_bound.argument()) {
            // trait type
            (
                Resolution::TraitType(trait_ty),
                TraitMemberBoundArgument::Type(ty),
            ) => {
                let Ok(resolve_ty) = self.resolve_type(
                    ty,
                    config.current_id().into(),
                    config,
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol =
                    self.get_mut(config.current_id()).unwrap();
                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::TypeEquals(Equals {
                            lhs: r#type::Type::TraitMember(
                                r#type::TraitMember {
                                    trait_type_id: trait_ty.member.id,
                                    trait_generic_arguments: trait_ty
                                        .parent_generic_arguments,
                                    member_generic_arguments: trait_ty
                                        .member
                                        .generic_arguments,
                                },
                            ),
                            rhs: resolve_ty,
                        }),
                        span: Some(span),
                        explicit: true,
                    },
                );
            }

            // trait constant
            (
                Resolution::TraitConstant(trait_constant),
                TraitMemberBoundArgument::Constant(constant),
            ) => {
                let Ok(evaluated_constant) = self.evaluate(
                    constant.expression(),
                    config.current_id().into(),
                    config.handler(),
                ) else {
                    return;
                };

                let mut generic_symbol =
                    self.get_mut(config.current_id()).unwrap();
                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::ConstantEquals(
                            Equals {
                                lhs: constant::Constant::TraitMember(
                                    constant::TraitMember {
                                        trait_constant_id: trait_constant
                                            .member,
                                        trait_arguments: trait_constant
                                            .parent_generic_arguments,
                                    },
                                ),
                                rhs: evaluated_constant,
                            },
                        ),
                        span: Some(span),
                        explicit: true,
                    },
                );
            }

            // mismatched type
            (
                Resolution::TraitType(..) | Resolution::TraitConstant(..),
                mismatched,
            ) => config.handler().receive(
                error::Error::TraitMemberBoundArgumentMismatched(
                    TraitMemberBoundArgumentMismatched {
                        trait_member_bound_argument_span: mismatched.span(),
                    },
                ),
            ),

            (resolved, _) => {
                config.handler().receive(error::Error::TraitMemberExpected(
                    TraitMemberExpected {
                        non_trait_member_span: trait_member_bound
                            .argument()
                            .span(),
                        resolved,
                    },
                ));
            }
        }
    }

    fn create_lifetime_outlives_predicate<T>(
        &self,
        lifetime_bound: &syntax_tree::item::LifetimeBound,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T> + IndexMut<ID<T>>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let handler = config.handler();
        let mut arguments = Vec::new();

        for argument_syn in lifetime_bound.arguments().elements() {
            let Ok(argument) = self.resolve_lifetime(
                argument_syn,
                config.current_id().into(),
                config,
                handler,
            ) else {
                continue;
            };

            arguments.push(argument);
        }

        match lifetime_bound.operand() {
            LifetimeBoundOperand::LifetimeParameter(lt_parameter) => {
                let Ok(lifetime_parameter_id) = self
                    .resolve_lifetime_parameter(
                        lt_parameter.identifier(),
                        config.current_id().into(),
                        handler,
                    )
                else {
                    return;
                };

                let mut generic_symbol = self
                    .get_mut(config.current_id())
                    .expect("should've been valid");

                for argument in arguments {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::LifetimeOutlives(
                                LifetimeOutlives {
                                    operand: Lifetime::Parameter(
                                        lifetime_parameter_id,
                                    ),
                                    argument,
                                },
                            ),
                            span: Some(lifetime_bound.span()),
                            explicit: true,
                        },
                    );
                }
            }
            LifetimeBoundOperand::QualifiedIdentifier(qualified_identifier) => {
                let Ok(ty) = self.resolve_qualified_identifier_type(
                    qualified_identifier,
                    config.current_id().into(),
                    config,
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol =
                    self.get_mut(config.current_id()).unwrap();

                let ty @ (r#type::Type::Parameter(..)
                | r#type::Type::TraitMember(..)) = ty
                else {
                    handler.receive(
                        error::Error::InvalidTypeInOutlivesPredicate(
                            InvalidTypeInOutlivesPredicate {
                                outlive_predicate_span: lifetime_bound.span(),
                                invalid_type: ty,
                            },
                        ),
                    );
                    return;
                };

                for argument in arguments {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TypeOutlives(
                                TypeOutlives { operand: ty.clone(), argument },
                            ),
                            span: Some(lifetime_bound.span()),
                            explicit: true,
                        },
                    );
                }
            }
        }
    }

    fn create_constant_type_predicate<T>(
        &self,
        constant_type_bound: &syntax_tree::item::ConstantTypeBound,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T> + IndexMut<ID<T>>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let handler = config.handler();
        let Ok(resolved_ty) = self.resolve_qualified_identifier_type(
            constant_type_bound.qualified_identifier(),
            config.current_id().into(),
            config,
            handler,
        ) else {
            return;
        };

        let ty @ (r#type::Type::Parameter(..) | r#type::Type::TraitMember(..)) =
            resolved_ty
        else {
            handler.receive(error::Error::InvalidTypeInConstantTypePredicate(
                InvalidTypeInConstantTypePredicate {
                    constant_type_predicate_span: constant_type_bound.span(),
                    invalid_type: resolved_ty,
                },
            ));
            return;
        };

        let mut generic_symbol = self.get_mut(config.current_id()).unwrap();

        generic_symbol.generic_declaration_mut().predicates.push(
            symbol::Predicate {
                predicate: predicate::Predicate::ConstantType(ConstantType {
                    r#type: ty,
                }),
                span: Some(constant_type_bound.span()),
                explicit: true,
            },
        );
    }

    fn create_trait_bound_predicate<T>(
        &self,
        trait_bound: &syntax_tree::item::TraitBound,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T> + IndexMut<ID<T>>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let handler = config.handler();
        if self
            .resolve_trait_path(
                trait_bound.qualified_identifier(),
                config.current_id().into(),
                config.handler(),
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
                    match higher_ranked_lifetimes.entry(
                        higher_ranked_lifetime_syn.identifier().span.str(),
                    ) {
                        Entry::Occupied(_) => {
                            handler.receive(
                                error::Error::HigherRankedLifetimeRedeclaration(
                                    error::HigherRankedLifetimeRedeclaration {
                                        redeclaration_span:
                                            higher_ranked_lifetime_syn.span(),
                                    },
                                ),
                            );
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Lifetime::Forall(Forall::generate()));
                        }
                    }
                }
            };

            higher_ranked_lifetimes
        };

        let current_id = config.current_id();
        let mut adapter = TraitResolverConfigAdapter {
            forall_lifetimes_by_name: higher_ranked_lifetimes,
            inner: config,
        };

        let Ok(resolution) = self
            .resolve(
                trait_bound.qualified_identifier(),
                current_id.into(),
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

        self.get_mut(config.current_id())
            .unwrap()
            .generic_declaration_mut()
            .predicates
            .push(symbol::Predicate {
                predicate: Predicate::Trait(predicate::Trait {
                    trait_id: resolution.id,
                    const_trait: trait_bound.const_keyword().is_some(),
                    generic_arguments: resolution
                        .generic_arguments
                        .into_other_model(),
                }),
                span: Some(trait_bound.span()),
                explicit: true,
            });
    }

    // create a list of predicates for the where clause
    #[allow(clippy::too_many_lines)]
    fn create_where_clause_predicates<T>(
        &self,
        syntax_tree: Option<&syntax_tree::item::WhereClause>,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T> + IndexMut<ID<T>>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let Some(where_clause) = syntax_tree else {
            return;
        };

        for clause in where_clause.constraint_list().elements() {
            match clause {
                syntax_tree::item::Predicate::TraitMember(
                    trait_member_bound,
                ) => {
                    self.create_trait_member_predicate(
                        trait_member_bound,
                        config,
                    );
                }
                syntax_tree::item::Predicate::Trait(trait_bound) => {
                    self.create_trait_bound_predicate(trait_bound, config);
                }
                syntax_tree::item::Predicate::Lifetime(lifetime_bound) => {
                    self.create_lifetime_outlives_predicate(
                        lifetime_bound,
                        config,
                    );
                }
                syntax_tree::item::Predicate::ConstantType(constant_type) => {
                    self.create_constant_type_predicate(constant_type, config);
                }
            }
        }
    }
}

macro_rules! implements_finalize {
    ($kind:ident, $syntax:path, $table:ident, $syntax_tree:ident, $config:ident, $where_clause:expr) => {
        paste! {
            impl Finalize<$syntax> for $kind {
                fn finalize(
                    $table: &Table,
                    $syntax_tree: &$syntax,
                    $config: &mut state::Config<Self>,
                ) {
                    $table.create_where_clause_predicates(
                        $where_clause,
                        $config,
                    );
                }
            }
        }
    };

    // for non-generic symbol
    ($kind:ident, $syntax_tree:path) => {
        paste! {
            impl Finalize<$syntax_tree> for $kind {
                fn finalize(
                    _: &Table,
                    _: &$syntax_tree,
                    _: &mut state::Config<Self>,
                ) {}
            }
        }
    };
}

implements_finalize!(
    Enum,
    syntax_tree::item::EnumSignature,
    table,
    syntax_tree,
    config,
    syntax_tree.where_clause().as_ref()
);
implements_finalize!(Variant, syntax_tree::item::Variant);
implements_finalize!(
    Struct,
    syntax_tree::item::Struct,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().where_clause().as_ref()
);
implements_finalize!(Constant, syntax_tree::item::Constant);
implements_finalize!(
    Type,
    syntax_tree::item::Type,
    table,
    syntax_tree,
    config,
    syntax_tree.definition().where_clause().as_ref()
);
implements_finalize!(
    Function,
    syntax_tree::item::Function,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().where_clause().as_ref()
);
implements_finalize!(
    Trait,
    syntax_tree::item::TraitSignature,
    table,
    syntax_tree,
    config,
    syntax_tree.where_clause().as_ref()
);
implements_finalize!(
    TraitFunction,
    syntax_tree::item::TraitFunction,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().where_clause().as_ref()
);
implements_finalize!(
    TraitType,
    syntax_tree::item::TraitType,
    table,
    syntax_tree,
    config,
    syntax_tree.where_clause().as_ref()
);
implements_finalize!(TraitConstant, syntax_tree::item::TraitConstant);
implements_finalize!(
    ImplementationFunction,
    syntax_tree::item::ImplementationFunction,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().where_clause().as_ref()
);
implements_finalize!(
    ImplementationType,
    syntax_tree::item::ImplementationType,
    table,
    syntax_tree,
    config,
    syntax_tree.definition().where_clause().as_ref()
);
implements_finalize!(
    ImplementationConstant,
    syntax_tree::item::ImplementationConstant
);

macro_rules! implements_finalize_for_implementation {
    ($kind:ident) => {
        impl Finalize<syntax_tree::item::ImplementationSignature> for $kind {
            fn finalize(
                table: &Table,
                syntax_tree: &syntax_tree::item::ImplementationSignature,
                config: &mut state::Config<Self>,
            ) {
                table.create_where_clause_predicates(
                    syntax_tree.where_clause().as_ref(),
                    config,
                );

                #[allow(clippy::significant_drop_in_scrutinee)]
                for (checking, _) in config.checkings().iter() {
                    // automatically add lifetime outlives predicates
                    let Checking::Predicate(
                        outlives @ (Predicate::TypeOutlives(_)
                        | Predicate::LifetimeOutlives(_)),
                    ) = checking
                    else {
                        continue;
                    };

                    table
                        .get_mut(config.current_id())
                        .unwrap()
                        .signature
                        .generic_declaration
                        .predicates
                        .push(symbol::Predicate {
                            predicate: outlives.clone(),
                            span: None,
                            explicit: false,
                        });
                }
            }
        }
    };
}

implements_finalize_for_implementation!(Implementation);
implements_finalize_for_implementation!(NegativeImplementation);
