use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{self, item::Constraint};
use pernixc_system::diagnostic::{Handler, Storage};

use crate::{
    error::{
        self, ConstantParameterDuplication, LifetimeParameterDeclaredAfterTypeOrConstantParameter,
        LifetimeParameterDuplication, TypeParameterDeclaredAfterConstantParameter,
        TypeParameterDuplication,
    },
    symbol::{ConstantParameter, GenericItemRef, LifetimeParameter, TypeParameter},
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
    fn create_where_clause(
        &mut self,
        generic_item_ref: GenericItemRef,
        where_clause_syntax: syntax_tree::item::WhereClause,
        checking_handler: &impl Handler<CheckingWithSpan>,
        handler: &impl Handler<error::Error>,
    ) {
        let (where_clause_keyword, colon, constraint_list) = where_clause_syntax.dissolve();

        // construct the associated type bounds first
        for bound in constraint_list.elements() {
            let Constraint::TraitAssociation(trait_association) = bound else {
                continue;
            };
        }
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
                    index,
                    parent_generic_item_ref: generic_item_ref,
                    span: Some(lt_syn.span()),
                },
            ) {
                handler.receive(error::Error::LifetimeParameterDuplication(
                    LifetimeParameterDuplication {
                        duplicate_span: lt_syn.span(),
                        existing_lifetime_parameter_span: generic_item
                            .generic_parameters()
                            .lifetimes[existing_index]
                            .span
                            .clone()
                            .unwrap(),
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
                    index,
                    parent_generic_item_ref: generic_item_ref,
                    span: Some(ty_parameter.span()),
                },
            ) {
                handler.receive(error::Error::TypeParameterDuplication(
                    TypeParameterDuplication {
                        duplicate_span: ty_parameter.span(),
                        existing_type_parameter: generic_item.generic_parameters().types
                            [existing_index]
                            .span
                            .clone()
                            .unwrap(),
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
                        index,
                        ty,
                        parent_generic_item_ref: generic_item_ref,
                        span: Some(constant_parameter.span()),
                    },
                )
            {
                handler.receive(error::Error::ConstantParameterDuplication(
                    ConstantParameterDuplication {
                        duplicate_span: constant_parameter.span(),
                        existing_constant_parameter: self
                            .get_generic_item(generic_item_ref)
                            .unwrap()
                            .generic_parameters()
                            .constants[existing_index]
                            .span
                            .clone()
                            .unwrap(),
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
        if let Some(where_clause_syntax) = where_clause_syntax {}
    }
}
