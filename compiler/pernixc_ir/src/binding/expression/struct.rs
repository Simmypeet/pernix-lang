use std::collections::{hash_map::Entry, HashMap, HashSet};

use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{Bind, Config, Expression};
use crate::{
    arena::ID,
    error::{
        self, DuplicatedFieldInitialization, FieldIsNotAccessible,
        FieldNotFound,
    },
    ir::{
        self,
        representation::{
            binding::{infer, Binder, Error, SemanticError},
            borrow,
        },
        value::{
            register::{Assignment, Struct},
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index, resolution},
        Field,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::Model,
        term::r#type::Expected,
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Struct> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Struct,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let resolution = self
            .resolve_with_inference(syntax_tree.qualified_identifier(), handler)
            .ok_or(Error::Semantic(SemanticError(syntax_tree.span())))?;

        // must be struct type
        let resolution::Resolution::Generic(resolution::Generic {
            id: resolution::GenericID::Struct(struct_id),
            generic_arguments,
        }) = resolution
        else {
            self.create_handler_wrapper(handler).receive(Box::new(
                error::ExpectStruct { span: syntax_tree.span() },
            ));
            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &self.table.get(struct_id).unwrap().generic_declaration.parameters,
        )
        .unwrap();

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
                let struct_sym = self.table.get(struct_id).unwrap();
                let Some(field_id) = struct_sym
                    .fields()
                    .get_id(field_syn.identifier().span.str())
                else {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        FieldNotFound {
                            identifier_span: field_syn
                                .identifier()
                                .span
                                .clone(),
                            struct_id,
                        },
                    ));
                    continue;
                };

                let field = struct_sym.fields().get(field_id).unwrap();
                let mut field_ty =
                    infer::Model::from_default_type(field.r#type.clone());

                instantiation::instantiate(&mut field_ty, &instantiation);

                (field_id, field_ty, field.accessibility)
            };

            // field accessibility check
            if !self
                .table
                .is_accessible_from(self.current_site, field_accessibility)
                .unwrap()
            {
                self.create_handler_wrapper(handler).receive(Box::new(
                    FieldIsNotAccessible {
                        field_id,
                        struct_id,
                        referring_site: self.current_site,
                        referring_identifier_span: field_syn
                            .identifier()
                            .span
                            .clone(),
                    },
                ));
            }

            // type check the field
            let _ = self.type_check(
                &self.type_of_value(&value)?,
                Expected::Known(field_ty),
                field_syn.expression().span(),
                true,
                handler,
            )?;

            match initializers_by_field_id.entry(field_id) {
                Entry::Occupied(entry) => self
                    .create_handler_wrapper(handler)
                    .receive(Box::new(DuplicatedFieldInitialization {
                        field_id,
                        struct_id,
                        prior_initialization_span: entry.get().1.clone(),
                        duplicate_initialization_span: field_syn.span(),
                    })),
                Entry::Vacant(entry) => {
                    entry.insert((value, field_syn.span()));
                }
            }
        }

        // check for uninitialized fields
        let struct_sym = self.table.get(struct_id).unwrap();
        let uninitialized_fields = struct_sym
            .fields()
            .ids()
            .filter(|field_id| !initializers_by_field_id.contains_key(field_id))
            .collect::<HashSet<_>>();

        if !uninitialized_fields.is_empty() {
            self.create_handler_wrapper(handler).receive(Box::new(
                error::UninitializedFields {
                    struct_id,
                    uninitialized_fields,
                    struct_expression_span: syntax_tree.span(),
                },
            ));
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
