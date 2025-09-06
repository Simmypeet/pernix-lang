use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::value::Value;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::{Generic, Resolution};
use pernixc_semantic_element::fields::{get_fields, Field};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::{
        is_accessible_from, is_accessible_from_globally, symbol_accessible,
    },
    kind::{get_kind, Kind},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters, instantiation::Instantiation,
};

use crate::{
    bind::{
        expression::r#struct::diagnostic::{
            Diagnostic, ExpectedStructSymbol, FieldIsNotAccessible,
            FieldNotFound,
        },
        Bind, Config, Expression,
    },
    binder::{Binder, BindingError, Error},
};

pub mod diagnostic;

impl Bind<&pernixc_syntax::expression::unit::Struct>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Struct,
        config: Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(qualified_identifier) = syntax_tree.qualified_identifier()
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let resolution = self
            .resolve_qualified_identifier_with_inference(
                &qualified_identifier,
                handler,
            )
            .await?;

        // must be struct type
        let Resolution::Generic(Generic { id: struct_id, generic_arguments }) =
            resolution
        else {
            handler.receive(
                Diagnostic::ExpectedStructSymbol(ExpectedStructSymbol {
                    span: qualified_identifier.span(),
                    id: resolution.global_id(),
                })
                .into(),
            );
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let symbol_kind = self.engine().get_kind(struct_id).await;
        if symbol_kind != Kind::Struct {
            handler.receive(
                Diagnostic::ExpectedStructSymbol(ExpectedStructSymbol {
                    span: syntax_tree.span(),
                    id: resolution.global_id(),
                })
                .into(),
            );
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }

        let struct_generic_parameters =
            self.engine().get_generic_parameters(struct_id).await?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id,
            &struct_generic_parameters,
        )
        .unwrap();

        // check for uninitialized fields
        let uninitialized_fields = fields
            .fields
            .ids()
            .filter(|field_id| !initializers_by_field_id.contains_key(field_id))
            .collect::<HashSet<_>>();

        if !uninitialized_fields.is_empty() {
            handler.receive(Box::new(UninitializedFields {
                struct_id,
                uninitialized_fields,
                struct_expression_span: syntax_tree.span(),
            }));
        }

        let value = Value::Register(
            self.create_register_assignmnet(
                Assignment::Struct(Struct {
                    struct_id,
                    initializers_by_field_id: initializers_by_field_id
                        .into_iter()
                        .map(|(id, (register_id, _))| (id, register_id))
                        .collect(),
                    generic_arguments,
                }),
                syntax_tree.span(),
            ),
        );

        Ok(Expression::RValue(value))
    }
}

async fn collect_fields(
    binder: &mut Binder<'_>,
    struct_id: Global<pernixc_symbol::ID>,
    instantiation: &Instantiation,
    syntax_tree: &pernixc_syntax::expression::unit::Struct,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<HashMap<ID<Field>, (Value, RelativeSpan)>, Error> {
    let fields = binder.engine().get_fields(struct_id).await?;

    let Some(field_initializers) = syntax_tree.field_initializer_body() else {
        // no fields to initialize
        return Ok(HashMap::default());
    };

    let mut initializers_by_field_id =
        HashMap::<ID<Field>, (Value, RelativeSpan)>::default();

    for field_syn in field_initializers.initializers() {
        let (Some(field_ident), Some(field_expr)) =
            (field_syn.identifier(), field_syn.expression())
        else {
            continue;
        };

        // get the field ID by name
        let value = binder.bind_value_or_error(&field_expr, handler).await?;

        let (field_id, field_ty, field_accessibility) = {
            let Some(field_id) = fields
                .field_ids_by_name
                .get(field_ident.kind.0.as_str())
                .copied()
            else {
                handler.receive(
                    Diagnostic::FieldNotFound(FieldNotFound {
                        identifier_span: field_ident.span,
                        struct_id,
                        field_name: field_ident.kind.0.clone(),
                    })
                    .into(),
                );
                continue;
            };

            let field = &fields.fields[field_id];
            let mut field_ty = field.r#type.clone();

            instantiation.instantiate(&mut field_ty);

            (field_id, field_ty, field.accessibility)
        };

        // field accessibility check
        if !binder
            .engine()
            .is_accessible_from_globally(
                binder.current_site(),
                field_accessibility.into_global(struct_id.target_id),
            )
            .await
        {
            handler.receive(
                Diagnostic::FieldIsNotAccessible(FieldIsNotAccessible {
                    field_id,
                    struct_id,
                    referring_site: binder.current_site(),
                    referring_identifier_span: field_ident.span,
                })
                .into(),
            );
        }

        // type check the field
        let _ = self.type_check(
            &self.type_of_value(&value, handler)?,
            Expected::Known(field_ty),
            field_syn.expression.span(),
            handler,
        )?;

        match initializers_by_field_id.entry(field_id) {
            Entry::Occupied(entry) => {
                handler.receive(Box::new(DuplicatedFieldInitialization {
                    field_id,
                    struct_id,
                    prior_initialization_span: entry.get().1.clone(),
                    duplicate_initialization_span: field_syn.span(),
                }));
            }
            Entry::Vacant(entry) => {
                entry.insert((value, field_syn.span()));
            }
        }
    }

    todo!()
}
