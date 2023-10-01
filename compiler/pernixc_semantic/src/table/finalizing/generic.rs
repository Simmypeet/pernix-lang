use pernixc_base::{
    diagnostic::{Handler, Storage},
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self};

use crate::{
    error::{
        self, ConstantParameterDuplication, LifetimeParameterDeclaredAfterTypeOrConstantParameter,
        LifetimeParameterDuplication, TypeParameterDeclaredAfterConstantParameter,
        TypeParameterDuplication,
    },
    symbol::{
        AssociatedBounds, ConstantParameter, ConstantParameterRef, GenericItemRef,
        LifetimeParameter, LifetimeParameterRef, LocalConstantParameterRef,
        LocalLifetimeParameterRef, LocalTypeParameterRef, TypeParameter, TypeParameterRef,
    },
    table::{
        resolution::{CheckingBehavior, CheckingWithSpan, Config, ExplicitLifetimeRequired},
        Table,
    },
};

/*
PEDANTIC CHECKING:

Right now, the compiler will delete all the generic parameters and where clause declaration of the
symbol if there is any error in the declaration. This is not the optimal behavior, we should strive
to keep as much information as possible. Therefore, in the future, we should only delete the part
that is erroneous, and keep the rest of the declaration. However, we must be very sure that what
we keep is definitely **VALID**. Any incorrect information will lead to a cascade of errors, since
we assert that anything appear on the table is always correct, which might break the whole system
unexpectedly.
*/

impl Table {
    #[allow(clippy::too_many_lines)]
    fn create_trait_associated_bounds(
        &mut self,
        _generic_item_ref: GenericItemRef,
        _constraint_list: &syntax_tree::item::ConstraintList,
        _where_clause_span: &Span,
        _checking_handler: &impl Handler<CheckingWithSpan>,
        _handler: &impl Handler<error::Error>,
    ) -> Option<AssociatedBounds> {
        todo!()
        /*
        let parent_where_clause = self
            .get_global_item(generic_item_ref.into())
            .unwrap()
            .parent()
            .map_or_else(WhereClause::default, |parent| {
                self.get_active_where_clause(parent).unwrap()
            });

        let mut associated_bounds = AssociatedBounds::default();

        let config = Config {
            referring_site: generic_item_ref.into(),
            checking: CheckingBehavior::Defer(Some(checking_handler)),
            explicit_lifetime_required: ExplicitLifetimeRequired::True,
        };

        // construct the associated type bounds first
        for bound in constraint_list.elements() {
            let Constraint::TraitAssociation(trait_association) = bound else {
                continue;
            };

            // resolve the trait association
            let Ok(resolved) = self.resolve_with_finalization(
                trait_association.qualified_identifier(),
                &config,
                handler,
            ) else {
                continue;
            };

            match (resolved, trait_association.argument()) {
                // trait type bound
                (
                    Resolution::TraitType(associated_ty),
                    TraitAssociationBoundArgument::Type(bound),
                ) => {
                    // resolve the bound type
                    let Ok(resolved_ty) =
                        self.resolve_type_with_finalization(bound, &config, handler)
                    else {
                        continue;
                    };

                    // insert the associated bound
                    let Entry::Vacant(entry) =
                        associated_bounds
                            .associated_type_bounds
                            .entry(ty::TraitAssociated {
                                trait_index: associated_ty.trait_index,
                                associated_index: associated_ty.associated_index,
                                trait_substitution: associated_ty.trait_substitution,
                                associated_substitution: associated_ty.associated_substitution,
                            })
                    else {
                        handler.receive(error::Error::AssociatedBoundConflict(
                            AssociatedBoundConflict {
                                where_clause_span: where_clause_span.clone(),
                            },
                        ));
                        continue;
                    };

                    entry.insert(resolved_ty);
                }
                // trait constant bound
                (
                    Resolution::TraitConstant(associated_constant),
                    TraitAssociationBoundArgument::Constant(constant),
                ) => {
                    // resolve the bound type
                    let Ok(resolved_constant) = self.evaluate_constant(
                        constant.expression(),
                        generic_item_ref.into(),
                        CheckingBehavior::Defer(Some(checking_handler)),
                        handler,
                    ) else {
                        continue;
                    };

                    // insert the associated bound
                    let Entry::Vacant(entry) = associated_bounds.associated_constant_bounds.entry(
                        constant::TraitAssociated {
                            trait_index: associated_constant.trait_index,
                            associated_index: associated_constant.constant_index,
                            trait_substitution: associated_constant.trait_substitution,
                        },
                    ) else {
                        handler.receive(error::Error::AssociatedBoundConflict(
                            AssociatedBoundConflict {
                                where_clause_span: where_clause_span.clone(),
                            },
                        ));
                        continue;
                    };

                    entry.insert(resolved_constant);
                }

                // mismatched type and constant
                (Resolution::TraitType(..) | Resolution::TraitConstant(..), _) => handler.receive(
                    error::Error::MismatchedTraitAssociatedBound(MismatchedTraitAssociatedBound {
                        trait_associated_bound_span: trait_association.span(),
                    }),
                ),

                // expect trait association
                _ => {
                    handler.receive(error::Error::TraitAssociatedItemExpected(
                        TraitAssociatedItemExpected {
                            expected_span: trait_association.span(),
                        },
                    ));
                }
            }
        }

        associated_bounds =
            match self.flatten_associated_bounds(associated_bounds, &ToCheckingWithSpanHandler {
                span: where_clause_span.clone(),
                handler: checking_handler,
            }) {
                Ok(ok) => ok,
                Err(err) => {
                    Self::handle_associated_bound_error(&err, where_clause_span.clone(), handler);
                    return None;
                }
            };

        // if we can combine this new associated bounds with the parent associated bounds without
        // any errors, then it's pretty much valid.
        if let Err(associated_bounds_error) = self.combine_associated_bounds(
            parent_where_clause.associated_bounds,
            &associated_bounds,
            &ToCheckingWithSpanHandler {
                span: where_clause_span.clone(),
                handler: checking_handler,
            },
        ) {
            Self::handle_associated_bound_error(
                &associated_bounds_error,
                where_clause_span.clone(),
                handler,
            );
            return None;
        }

        Some(associated_bounds)

        */
    }

    fn create_where_clause(
        &mut self,
        generic_item_ref: GenericItemRef,
        where_clause_syntax: syntax_tree::item::WhereClause,
        checking_handler: &impl Handler<CheckingWithSpan>,
        handler: &impl Handler<error::Error>,
    ) {
        let where_clause_span = where_clause_syntax.span();
        let (_where_clause_keyword, _colonn, constraint_list) = where_clause_syntax.dissolve();

        let _associated_bounds = self.create_trait_associated_bounds(
            generic_item_ref,
            &constraint_list,
            &where_clause_span,
            checking_handler,
            handler,
        );
    }

    #[allow(clippy::too_many_lines)]
    fn create_generic_parameters(
        &mut self,
        generic_item_ref: GenericItemRef,
        generic_parameters_syntax: syntax_tree::item::GenericParameters,
        checking_handler: &impl Handler<CheckingWithSpan>,
        handler: &impl Handler<error::Error>,
    ) {
        let mut lifetime_parameter_syntaxes = Vec::new();
        let mut type_parameter_syntaxes = Vec::new();
        let mut constant_parameter_syntaxes = Vec::new();

        let (_, generic_parameter_syntaxes, _) = generic_parameters_syntax.dissolve();

        for generic_parameter in generic_parameter_syntaxes.into_elements() {
            match generic_parameter {
                syntax_tree::item::GenericParameter::Lifetime(lt_syn) => {
                    // check if the lifetime parameter is declared after type or constant parameter
                    if !(type_parameter_syntaxes.is_empty()
                        && constant_parameter_syntaxes.is_empty())
                    {
                        handler.receive(
                            error::Error::LifetimeParameterDeclaredAfterTypeOrConstantParameter(
                                LifetimeParameterDeclaredAfterTypeOrConstantParameter {
                                    lifetime_parameter_span: lt_syn.span(),
                                },
                            ),
                        );
                        continue;
                    }

                    lifetime_parameter_syntaxes.push(lt_syn);
                }
                syntax_tree::item::GenericParameter::Type(ty_syn) => {
                    if !constant_parameter_syntaxes.is_empty() {
                        handler.receive(error::Error::TypeParameterDeclaredAfterConstantParameter(
                            TypeParameterDeclaredAfterConstantParameter {
                                type_parameter_span: ty_syn.span(),
                            },
                        ));
                        continue;
                    }

                    type_parameter_syntaxes.push(ty_syn);
                }
                syntax_tree::item::GenericParameter::Constant(const_syn) => {
                    constant_parameter_syntaxes.push(const_syn);
                }
            }
        }

        let generic_item = self
            .get_generic_item_mut(generic_item_ref)
            .expect("should be a valid ref");

        // add lifetime parameters
        for lt_syn in lifetime_parameter_syntaxes {
            let index = generic_item.generic_parameters().lifetimes.len();
            if let Err(existing_index) = generic_item.generic_parameters_mut().lifetimes.insert(
                lt_syn.identifier().span.str().to_string(),
                LifetimeParameter {
                    name: lt_syn.identifier().span.str().to_string(),
                    local_lifetime_parameter_ref: LocalLifetimeParameterRef(index),
                    parent_generic_item_ref: generic_item_ref,
                    span: Some(lt_syn.span()),
                },
            ) {
                handler.receive(error::Error::LifetimeParameterDuplication(
                    LifetimeParameterDuplication {
                        duplicate_span: lt_syn.span(),
                        existing_lifetime_parameter_ref: LifetimeParameterRef {
                            generic_item_ref,
                            local_ref: LocalLifetimeParameterRef(existing_index),
                        },
                    },
                ));
            };
        }

        // add the type parameter
        for ty_parameter in type_parameter_syntaxes {
            let index = generic_item.generic_parameters().types.len();
            if let Err(existing_index) = generic_item.generic_parameters_mut().types.insert(
                ty_parameter.identifier().span.str().to_string(),
                TypeParameter {
                    name: ty_parameter.identifier().span.str().to_string(),
                    local_type_parameter_ref: LocalTypeParameterRef(index),
                    parent_generic_item_ref: generic_item_ref,
                    span: Some(ty_parameter.span()),
                },
            ) {
                handler.receive(error::Error::TypeParameterDuplication(
                    TypeParameterDuplication {
                        duplicate_span: ty_parameter.span(),
                        existing_type_parameter_ref: TypeParameterRef {
                            generic_item_ref,
                            local_ref: LocalTypeParameterRef(existing_index),
                        },
                    },
                ));
            }
        }

        // add the constant parameters
        for constant_parameter in constant_parameter_syntaxes {
            let index = self
                .get_generic_item(generic_item_ref)
                .unwrap()
                .generic_parameters()
                .constants
                .len();
            let Ok(ty) = self.resolve_type_with_finalization(
                constant_parameter.ty(),
                &Config {
                    referring_site: generic_item_ref.into(),
                    checking: CheckingBehavior::Defer(Some(checking_handler)),
                    explicit_lifetime_required: ExplicitLifetimeRequired::True,
                },
                handler,
            ) else {
                continue;
            };

            if let Err(existing_index) = self
                .get_generic_item_mut(generic_item_ref)
                .unwrap()
                .generic_parameters_mut()
                .constants
                .insert(
                    constant_parameter.identifier().span.str().to_string(),
                    ConstantParameter {
                        name: constant_parameter.identifier().span.str().to_string(),
                        local_constant_parameter_ref: LocalConstantParameterRef(index),
                        ty,
                        parent_generic_item_ref: generic_item_ref,
                        span: Some(constant_parameter.span()),
                    },
                )
            {
                handler.receive(error::Error::ConstantParameterDuplication(
                    ConstantParameterDuplication {
                        duplicate_span: constant_parameter.span(),
                        existing_constant_parameter_ref: ConstantParameterRef {
                            generic_item_ref,
                            local_ref: LocalConstantParameterRef(existing_index),
                        },
                    },
                ));
            }
        }
    }

    pub(super) fn finalize_generic(
        &mut self,
        generic_item_ref: GenericItemRef,
        generic_parameters_syntax: Option<syntax_tree::item::GenericParameters>,
        where_clause_syntax: Option<syntax_tree::item::WhereClause>,
        handler: &impl Handler<error::Error>,
    ) {
        let checking_storage = Storage::<CheckingWithSpan>::new();

        // create generic parameters
        if let Some(generic_parameters_syntax) = generic_parameters_syntax {
            self.create_generic_parameters(
                generic_item_ref,
                generic_parameters_syntax,
                &checking_storage,
                handler,
            );
        }

        // create where caluse
        if let Some(where_clause_syntax) = where_clause_syntax {
            self.create_where_clause(
                generic_item_ref,
                where_clause_syntax,
                &checking_storage,
                handler,
            );
        }
    }
}
