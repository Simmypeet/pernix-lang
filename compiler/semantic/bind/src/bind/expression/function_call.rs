use pernixc_handler::Handler;
use pernixc_ir::value::{
    register::{Assignment, FunctionCall, Variant},
    Value,
};
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::{
    parameter::get_parameters, variant::get_variant_associated_type,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    instantiation::{
        get_instantiation, get_instantiation_for_associated_symbol,
        Instantiation,
    },
};

use crate::bind::expression::function_call::diagnostic::{
    Diagnostic, ExtraneousArgumentsToAssociatedValue, MismatchedArgumentsCount,
    SymbolIsNotCallable, VariantAssociatedValueExpected,
    VariantDoesntHaveAssociatedValue,
};

pub mod diagnostic;

use crate::{
    bind::{Bind, Config, Expression},
    binder::{Binder, BindingError, Error},
};

async fn get_function_instantiation(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Global<pernixc_symbol::ID>, Instantiation), Error> {
    let Some(qualified_identifier) = syntax_tree.qualified_identifier() else {
        return Err(Error::Binding(BindingError(syntax_tree.span())));
    };

    let resolution = binder
        .resolve_qualified_identifier_with_inference(
            &qualified_identifier,
            handler,
        )
        .await?;

    // get the function id and instantation
    let (id, instantation) = match resolution {
        Resolution::Variant(variant) => (
            variant.variant_id,
            binder
                .engine()
                .get_instantiation(
                    variant.variant_id.target_id.make_global(
                        binder
                            .engine()
                            .get_parent(variant.variant_id)
                            .await
                            .unwrap(),
                    ),
                    variant.generic_arguments,
                )
                .await?,
        ),
        Resolution::Generic(generic)
            if {
                let kind = binder.engine().get_kind(generic.id).await;
                kind.has_function_signature()
            } =>
        {
            (
                generic.id,
                binder
                    .engine()
                    .get_instantiation(generic.id, generic.generic_arguments)
                    .await?,
            )
        }

        Resolution::MemberGeneric(member_generic)
            if {
                let kind = binder.engine().get_kind(member_generic.id).await;
                kind.has_function_signature()
            } =>
        {
            (
                member_generic.id,
                binder
                    .engine()
                    .get_instantiation_for_associated_symbol(
                        member_generic.id,
                        member_generic.parent_generic_arguments,
                        member_generic.member_generic_arguments,
                    )
                    .await?,
            )
        }

        resolution => {
            // the symbol can't be called as a function
            handler.receive(
                Diagnostic::SymbolIsNotCallable(SymbolIsNotCallable {
                    symbol_id: resolution.global_id(),
                    span: syntax_tree.span(),
                })
                .into(),
            );

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }
    };

    Ok((id, instantation.expect("should have correct generic arguments count")))
}

async fn get_callable_expected_types(
    binder: &mut Binder<'_>,
    callable_id: Global<pernixc_symbol::ID>,
    instantiation: &Instantiation,
) -> Result<Vec<pernixc_term::r#type::Type>, Error> {
    // can either be a function or an enum variant
    let kind = binder.engine().get_kind(callable_id).await;

    if kind == Kind::Variant {
        let associated_type = binder
            .engine()
            .get_variant_associated_type(callable_id)
            .await?
            .as_deref()
            .cloned();

        let Some(mut associated_type) = associated_type else {
            return Ok(Vec::new());
        };

        instantiation.instantiate(&mut associated_type);

        Ok(vec![associated_type])
    } else {
        let parameters = binder.engine().get_parameters(callable_id).await?;

        let mut expected_types =
            Vec::with_capacity(parameters.parameters.len());

        for (_, parameter) in parameters.parameters_as_order() {
            let mut ty = parameter.r#type.clone();
            instantiation.instantiate(&mut ty);
            expected_types.push(ty);
        }

        Ok(expected_types)
    }
}

impl Bind<&pernixc_syntax::expression::unit::FunctionCall> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
        _config: &Config<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let (callable_id, instantiation) =
            get_function_instantiation(self, syntax_tree, handler).await?;

        let mut expected_types =
            get_callable_expected_types(self, callable_id, &instantiation)
                .await?;

        let kind = self.engine().get_kind(callable_id).await;
        let arguments = syntax_tree
            .call()
            .map(|x| x.expressions().collect::<Vec<_>>())
            .unwrap_or_default();

        match (kind, expected_types.len()) {
            (Kind::Variant, 0) => {
                handler.receive(
                    Diagnostic::VariantDoesntHaveAssociatedValue(
                        VariantDoesntHaveAssociatedValue {
                            variant_id: callable_id,
                            span: syntax_tree.span(),
                            supplied_count: arguments.len(),
                        },
                    )
                    .into(),
                );

                Err(Error::Binding(BindingError(syntax_tree.span())))
            }

            (Kind::Variant, 1) => {
                let inner_value = match arguments.len() {
                    0 => {
                        handler.receive(
                            Diagnostic::VariantAssociatedValueExpected(
                                VariantAssociatedValueExpected {
                                    variant_id: callable_id,
                                    span: syntax_tree.span(),
                                },
                            )
                            .into(),
                        );

                        Value::error(
                            expected_types.pop().unwrap(),
                            Some(syntax_tree.span()),
                        )
                    }

                    1 => {
                        Box::pin(self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        ))
                        .await?
                    }

                    _ => {
                        handler.receive(
                            Diagnostic::ExtraneousArgumentsToAssociatedValue(
                                ExtraneousArgumentsToAssociatedValue {
                                    variant_id: callable_id,
                                    span: syntax_tree.call().map_or_else(
                                        || syntax_tree.span(),
                                        |c| c.span(),
                                    ),
                                    supplied_count: arguments.len(),
                                },
                            )
                            .into(),
                        );

                        Box::pin(self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        ))
                        .await?
                    }
                };

                let enum_id = callable_id.target_id.make_global(
                    self.engine().get_parent(callable_id).await.unwrap(),
                );
                let generic_parameters =
                    self.engine().get_generic_parameters(enum_id).await?;

                let register_id = self.create_register_assignment(
                    Assignment::Variant(Variant {
                        variant_id: callable_id,
                        associated_value: Some(inner_value),
                        generic_arguments: instantiation
                            .convert_to_generic_arguments(
                                &generic_parameters,
                                enum_id,
                            )
                            .expect(
                                "should have correct generic arguments count",
                            ),
                    }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            _ => {
                if expected_types.len() != arguments.len() {
                    handler.receive(
                        Diagnostic::MismatchedArgumentsCount(
                            MismatchedArgumentsCount {
                                function_id: callable_id,
                                expected: expected_types.len(),
                                supplied: arguments.len(),
                                span: syntax_tree.call().map_or_else(
                                    || syntax_tree.span(),
                                    |c| c.span(),
                                ),
                            },
                        )
                        .into(),
                    );
                }

                // bind the argument and type-check
                let mut argument_values = Vec::with_capacity(arguments.len());
                for (expr, ty) in arguments.iter().zip(expected_types.iter()) {
                    argument_values.push(
                        Box::pin(self.bind_value_or_error(
                            expr,
                            Some(ty),
                            handler,
                        ))
                        .await?,
                    );
                }

                // truncuate or fill the arguments to match the expected types
                match arguments.len().cmp(&expected_types.len()) {
                    std::cmp::Ordering::Less => {
                        for ty in
                            expected_types.iter().skip(argument_values.len())
                        {
                            argument_values.push(Value::error(
                                ty.clone(),
                                Some(syntax_tree.span()),
                            ));
                        }
                    }
                    std::cmp::Ordering::Greater => {
                        argument_values.truncate(expected_types.len());
                    }
                    std::cmp::Ordering::Equal => {}
                }

                Ok(Expression::RValue(Value::Register(
                    self.create_register_assignment(
                        Assignment::FunctionCall(FunctionCall {
                            callable_id,
                            arguments: argument_values,
                            instantiation,
                        }),
                        syntax_tree.span(),
                    ),
                )))
            }
        }
    }
}
