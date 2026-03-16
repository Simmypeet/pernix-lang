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
use pernixc_syntax::QualifiedIdentifierRoot;
use pernixc_term::r#type::Qualifier;

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

        match resolution {
            Resolution::Variant(variant_res) => {
                let variant = self
                    .engine()
                    .get_variant_associated_type(variant_res.variant_id())
                    .await;

                // expected a variant type
                if variant.is_some() {
                    handler.receive(
                        Diagnostic::ExpectedAssociatedValue(
                            ExpectedAssociatedValue {
                                span: syntax_tree.span(),
                                variant_id: variant_res.variant_id(),
                            },
                        )
                        .into(),
                    );
                }

                let associated_value = if let Some(associated_type) = variant {
                    let mut associated_type = associated_type.deref().clone();
                    let instantiation =
                        variant_res.create_instantiation(self.engine()).await;

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
                    Assignment::Variant(Variant::new(
                        variant_res,
                        associated_value,
                    )),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            resolution => {
                handler.receive(
                    Diagnostic::SymbolCannotBeUsedAsAnExpression(
                        SymbolCannotBeUsedAsAnExpression {
                            span: syntax_tree.span(),
                            found: resolution,
                        },
                    )
                    .into(),
                );

                Err(Error::Binding(BindingError(syntax_tree.span())))
            }
        }
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

    let Some(name) = binder.stack().search(identifier.kind.0.as_ref()) else {
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
