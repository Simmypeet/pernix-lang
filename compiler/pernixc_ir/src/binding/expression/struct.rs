use std::collections::{hash_map::Entry, HashMap, HashSet};

use pernixc_arena::ID;
use pernixc_component::fields::Field;
use pernixc_handler::Handler;
use pernixc_resolution::qualified_identifier::{Generic, Resolution};
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, ConnectedList};
use pernixc_table::{component::SymbolKind, diagnostic::Diagnostic};
use pernixc_term::{
    generic_parameter::GenericParameters,
    instantiation::{self, Instantiation},
    Model,
};

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::{
            DuplicatedFieldInitialization, ExpectedStruct,
            FieldIsNotAccessible, FieldNotFound, UninitializedFields,
        },
        infer::{self, Expected},
        Binder, BindingError, Error,
    },
    value::{
        register::{Assignment, Struct},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Struct> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Struct,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let resolution = self
            .resolve_qualified_identifier_with_inference(
                syntax_tree.qualified_identifier(),
                handler,
            )
            .map_err(|_| Error::Binding(BindingError(syntax_tree.span())))?;

        // must be struct type
        let Resolution::Generic(Generic { id: struct_id, generic_arguments }) =
            resolution
        else {
            handler
                .receive(Box::new(ExpectedStruct { span: syntax_tree.span() }));
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let symbol_kind = *self.table.get::<SymbolKind>(struct_id);
        if symbol_kind != SymbolKind::Struct {
            handler
                .receive(Box::new(ExpectedStruct { span: syntax_tree.span() }));
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }

        let struct_generic_parameters =
            self.table.query::<GenericParameters>(struct_id)?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id,
            &struct_generic_parameters,
        )
        .unwrap();

        let fields =
            self.table.query::<pernixc_component::fields::Fields>(struct_id)?;
        let mut initializers_by_field_id =
            HashMap::<ID<Field>, (Value<infer::Model>, Span)>::new();

        for field_syn in syntax_tree
            .field_initializers()
            .connected_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
        {
            // get the field ID by name
            let value =
                self.bind_value_or_error(&**field_syn.expression(), handler)?;

            let (field_id, field_ty, field_accessibility) = {
                let Some(field_id) = fields
                    .field_ids_by_name
                    .get(field_syn.identifier().span.str())
                    .copied()
                else {
                    handler.receive(Box::new(FieldNotFound {
                        identifier_span: field_syn.identifier().span.clone(),
                        struct_id,
                    }));
                    continue;
                };

                let field = &fields.fields[field_id];
                let mut field_ty =
                    infer::Model::from_default_type(field.r#type.clone());

                instantiation::instantiate(&mut field_ty, &instantiation);

                (field_id, field_ty, field.accessibility)
            };

            // field accessibility check
            if !self.table.is_accessible_from_globally(
                self.current_site,
                field_accessibility.into_global(struct_id.target_id),
            ) {
                handler.receive(Box::new(FieldIsNotAccessible {
                    field_id,
                    struct_id,
                    referring_site: self.current_site,
                    referring_identifier_span: field_syn
                        .identifier()
                        .span
                        .clone(),
                }));
            }

            // type check the field
            let _ = self.type_check(
                &self.type_of_value(&value, handler)?,
                Expected::Known(field_ty),
                field_syn.expression().span(),
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

#[cfg(test)]
mod test;
