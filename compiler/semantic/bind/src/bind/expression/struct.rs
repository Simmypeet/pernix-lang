use std::collections::hash_map::Entry;

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_ir::value::{
    Value,
    register::{Assignment, Struct},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::fields::{Field, Fields, get_fields};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::is_accessible_from_globally,
    kind::{Kind, get_kind},
};
use pernixc_target::Global;
use pernixc_term::instantiation::Instantiation;

use crate::{
    bind::{
        Bind, Expression, Guidance,
        expression::r#struct::diagnostic::{
            Diagnostic, DuplicatedFieldInitialization, ExpectedStructSymbol,
            FieldIsNotAccessible, FieldNotFound, UninitializedFields,
        },
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
        _: &Guidance<'_>,
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
        let generic_symbol = match resolution {
            Resolution::GenericSymbol(generic)
                if {
                    self.engine().get_kind(generic.id()).await == Kind::Struct
                } =>
            {
                generic
            }

            found => {
                handler.receive(
                    Diagnostic::ExpectedStructSymbol(ExpectedStructSymbol {
                        span: qualified_identifier.span(),
                        found,
                    })
                    .into(),
                );

                return Err(Error::Binding(BindingError(syntax_tree.span())));
            }
        };

        let inst = generic_symbol.create_instantiation(self.engine()).await;
        let fields = self.engine().get_fields(generic_symbol.id()).await;

        let fields_initializers = (collect_fields(
            self,
            generic_symbol.id(),
            &inst,
            &fields,
            syntax_tree,
            handler,
        ))
        .await?;

        // check for uninitialized fields
        let uninitialized_fields = fields
            .fields
            .ids()
            .filter(|field_id| !fields_initializers.contains_key(field_id))
            .collect::<FxHashSet<_>>();

        if !uninitialized_fields.is_empty() {
            handler.receive(
                Diagnostic::UninitializedFields(UninitializedFields {
                    struct_id: generic_symbol.id(),
                    uninitialized_fields,
                    struct_expression_span: syntax_tree.span(),
                })
                .into(),
            );
        }

        let value = Value::Register(
            self.create_register_assignment(
                Assignment::Struct(Struct::new(
                    generic_symbol,
                    fields_initializers
                        .into_iter()
                        .map(|(id, (register_id, _))| (id, register_id))
                        .collect(),
                )),
                syntax_tree.span(),
            ),
        );

        Ok(Expression::RValue(value))
    }
}

async fn collect_fields(
    binder: &mut Binder<'_>,
    struct_id: Global<pernixc_symbol::SymbolID>,
    instantiation: &Instantiation,
    fields: &Fields,
    syntax_tree: &pernixc_syntax::expression::unit::Struct,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<FxHashMap<ID<Field>, (Value, RelativeSpan)>, Error> {
    let Some(field_initializers) = syntax_tree.field_initializer_body() else {
        // no fields to initialize
        return Ok(FxHashMap::default());
    };

    let mut initializers_by_field_id =
        FxHashMap::<ID<Field>, (Value, RelativeSpan)>::default();

    for field_syn in field_initializers.initializers() {
        let (Some(field_ident), Some(field_expr)) =
            (field_syn.identifier(), field_syn.expression())
        else {
            continue;
        };

        let (field_id, field_ty, field_accessibility) = {
            let Some(field_id) = fields
                .field_ids_by_name
                .get(field_ident.kind.0.as_ref())
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

        // get the field ID by name
        let value = binder
            .bind_value_or_error(&field_expr, Some(&field_ty), handler)
            .await?;

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

        match initializers_by_field_id.entry(field_id) {
            Entry::Occupied(entry) => {
                handler.receive(
                    Diagnostic::DuplicatedFieldInitialization(
                        DuplicatedFieldInitialization {
                            field_id,
                            struct_id,
                            prior_initialization_span: entry.get().1,
                            duplicate_initialization_span: field_syn.span(),
                        },
                    )
                    .into(),
                );
            }
            Entry::Vacant(entry) => {
                entry.insert((value, field_syn.span()));
            }
        }
    }

    Ok(initializers_by_field_id)
}
