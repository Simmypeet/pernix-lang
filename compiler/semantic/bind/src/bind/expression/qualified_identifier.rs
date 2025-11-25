use std::ops::Deref;

use pernixc_handler::Handler;
use pernixc_ir::value::{
    Value,
    literal::{self, Literal},
    register::{Assignment, Variant},
};
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::variant::get_variant_associated_type;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent,
};
use pernixc_syntax::QualifiedIdentifierRoot;
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters, instantiation::Instantiation,
    r#type::Qualifier,
};

use crate::{
    bind::{
        Bind, Expression, Guidance, LValue,
        expression::qualified_identifier::diagnostic::{
            Diagnostic, ExpectedAssociatedValue,
            SymbolCannotBeUsedAsAnExpression,
        },
    },
    binder::{Binder, BindingError, Error},
};

pub mod diagnostic;

impl Bind<&pernixc_syntax::QualifiedIdentifier> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
        config: &Guidance<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, crate::binder::Error> {
        // search for the variable/parameter in the stack
        if let Some(expression) =
            bind_simple_identifier(self, syntax_tree, config, handler).await?
        {
            return Ok(expression);
        }

        let Ok(resolution) = self
            .resolve_qualified_identifier_with_inference(syntax_tree, handler)
            .await
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let id = match resolution {
            Resolution::Variant(variant_res) => {
                let variant = self
                    .engine()
                    .get_variant_associated_type(variant_res.variant_id)
                    .await?;

                let parent_enum_id = Global::new(
                    variant_res.variant_id.target_id,
                    self.engine()
                        .get_parent(variant_res.variant_id)
                        .await
                        .unwrap(),
                );

                let enum_generic_parameters = self
                    .engine()
                    .get_generic_parameters(parent_enum_id)
                    .await?;

                // expected a variant type
                if variant.is_some() {
                    handler.receive(
                        Diagnostic::ExpectedAssociatedValue(
                            ExpectedAssociatedValue {
                                span: syntax_tree.span(),
                                variant_id: variant_res.variant_id,
                            },
                        )
                        .into(),
                    );
                }

                let associated_value = if let Some(associated_type) = variant {
                    let mut associated_type = associated_type.deref().clone();

                    let instantiation = Instantiation::from_generic_arguments(
                        variant_res.generic_arguments.clone(),
                        parent_enum_id,
                        &enum_generic_parameters,
                    )
                    .unwrap();

                    instantiation.instantiate(&mut associated_type);

                    let associated_value =
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: associated_type,
                            span: syntax_tree.span(),
                        }));

                    Some(associated_value)
                } else {
                    None
                };

                let register_id = self.create_register_assignment(
                    Assignment::Variant(Variant {
                        variant_id: variant_res.variant_id,
                        associated_value,
                        generic_arguments: variant_res.generic_arguments,
                    }),
                    syntax_tree.span(),
                );

                return Ok(Expression::RValue(Value::Register(register_id)));
            }

            Resolution::Generic(generic) => {
                let symbol_kind = self.engine().get_kind(generic.id).await;

                if symbol_kind == Kind::Constant {
                    todo!("handle constant evaluation");
                }

                generic.id
            }

            Resolution::MemberGeneric(member_generic) => {
                let symbol_kind =
                    self.engine().get_kind(member_generic.id).await;

                if symbol_kind == Kind::ImplementationConstant {
                    todo!("handle constant evaluation");
                }

                member_generic.id
            }

            resolution @ Resolution::Module(_) => resolution.global_id(),
        };

        handler.receive(
            Diagnostic::SymbolCannotBeUsedAsAnExpression(
                SymbolCannotBeUsedAsAnExpression {
                    span: syntax_tree.span(),
                    symbol: id,
                },
            )
            .into(),
        );

        Err(Error::Binding(BindingError(syntax_tree.span())))
    }
}

async fn bind_simple_identifier(
    binder: &mut Binder<'_>,
    syn: &pernixc_syntax::QualifiedIdentifier,
    _: &Guidance<'_>,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<Option<Expression>, crate::binder::Error> {
    let Some(QualifiedIdentifierRoot::GenericIdentifier(ident)) = syn.root()
    else {
        return Ok(None);
    };

    let Some(identifier) = ident.identifier() else {
        return Ok(None);
    };

    // should have no generic arguments
    if ident.generic_arguments().is_some() {
        return Ok(None);
    }

    // should have no subsequences
    if syn.subsequences().count() > 0 {
        return Ok(None);
    }

    let Some(name) = binder.stack().search(identifier.kind.0.as_str()) else {
        return Ok(None);
    };

    let name_qualifier =
        if name.mutable { Qualifier::Mutable } else { Qualifier::Immutable };

    let final_qualifier = binder
        .get_behind_reference_qualifier(&name.load_address, handler)
        .await?
        .map_or(name_qualifier, |x| x.min(name_qualifier));

    Ok(Some(Expression::LValue(LValue {
        address: name.load_address.clone(),
        span: syn.span(),
        qualifier: final_qualifier,
    })))
}
