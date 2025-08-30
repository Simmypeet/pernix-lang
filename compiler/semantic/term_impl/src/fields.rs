use std::{borrow::Cow, sync::Arc};

use pernixc_arena::Arena;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_query::runtime::executor;
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::{get_accessibility, Accessibility},
    parent::get_closest_module_id,
    syntax::get_fields_syntax,
};
use pernixc_term::{
    fields::{Field, Fields},
    r#type::Type,
};
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
};

use crate::{
    build::{self, Output},
    fields::diagnostic::{
        Diagnostic, FieldMoreAccessibleThanStruct, FieldRedefinition,
    },
    occurrences::Occurrences,
};

pub mod diagnostic;

#[derive(Debug, Default)]
pub struct BuildExecutor;

impl build::Build for pernixc_term::fields::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<build::Output<Self>, executor::CyclicError> {
        let syntax = engine.get_fields_syntax(key.0).await;

        let Some(syntax_tree) = syntax else {
            return Ok(Output {
                item: Fields {
                    fields: Arena::default(),
                    field_ids_by_name: HashMap::default(),
                    field_declaration_order: Vec::default(),
                },
                diagnostics: Arc::default(),
                occurrences: Arc::default(),
            });
        };

        let storage = Storage::<Diagnostic>::default();
        let mut occurrences = Occurrences::default();
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;

        let struct_accessibility = engine.get_accessibility(key.0).await;

        let mut fields = Arena::new();
        let mut field_ids_by_name = HashMap::default();
        let mut field_declaration_order = Vec::new();

        for field_syntax in &syntax_tree.members.members {
            let field_name = field_syntax.identifier.to_string();

            // Check for field redefinition
            if let Some(&existing_field_id) = field_ids_by_name.get(&field_name)
            {
                let existing_field = fields.get(existing_field_id).unwrap();

                storage.receive(Diagnostic::FieldRedefinition(
                    FieldRedefinition {
                        field_name: field_name.clone(),
                        redefinition_span: field_syntax.span(),
                        original_span: existing_field.span,
                        struct_id: key.0.id,
                        target_id: key.0.target_id,
                    },
                ));
                continue;
            }

            // Resolve field accessibility
            let field_accessibility =
                if let Some(access_modifier) = &field_syntax.access_modifier {
                    create_accessibility_from_modifier(
                        access_modifier,
                        key.0.id,
                        key.0.target_id,
                        engine,
                    )
                    .await
                } else {
                    struct_accessibility
                };

            // Resolve field type
            let mut field_type = engine
                .resolve_type(
                    &field_syntax.r#type,
                    Config::builder()
                        .observer(&mut occurrences)
                        .extra_namespace(&extra_namespace)
                        .referring_site(key.0)
                        .build(),
                    &storage,
                )
                .await?;

            let premise = engine.get_active_premise(key.0).await?;
            let env = Environment::new(
                Cow::Borrowed(&premise),
                Cow::Borrowed(engine),
                normalizer::NO_OP,
            );

            field_type = match env
                .simplify_and_check_lifetime_constraints(
                    &field_type,
                    &field_syntax.r#type.span(),
                    &storage,
                )
                .await
            {
                Ok(result) => result,
                Err(error) => match error {
                    pernixc_type_system::Error::Overflow(_) => {
                        Type::Error(pernixc_term::error::Error)
                    }
                    pernixc_type_system::Error::CyclicDependency(
                        cyclic_error,
                    ) => return Err(cyclic_error),
                },
            };

            let field = Field {
                accessibility: field_accessibility,
                name: field_name.clone(),
                r#type: field_type,
                span: Some(field_syntax.span()),
            };

            // Check if field is more accessible than struct
            if is_more_accessible(field_accessibility, struct_accessibility) {
                storage.receive(Diagnostic::FieldMoreAccessibleThanStruct(
                    FieldMoreAccessibleThanStruct {
                        field: field.clone(),
                        struct_accessibility,
                        struct_id: key.0.id,
                        target_id: key.0.target_id,
                    },
                ));
            }

            let field_id = fields.insert(field);
            field_ids_by_name.insert(field_name, field_id);
            field_declaration_order.push(field_id);
        }

        Ok(Output {
            item: Fields { fields, field_ids_by_name, field_declaration_order },
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

/// Creates accessibility from an access modifier syntax.
async fn create_accessibility_from_modifier(
    access_modifier: &pernixc_syntax::AccessModifier,
    struct_id: pernixc_symbol::ID,
    target_id: pernixc_target::TargetID,
    engine: &pernixc_query::TrackedEngine,
) -> Accessibility<pernixc_symbol::ID> {
    match access_modifier {
        pernixc_syntax::AccessModifier::Private(_) => {
            let module_id = engine
                .get_closest_module_id(pernixc_target::Global::new(
                    target_id, struct_id,
                ))
                .await;
            Accessibility::Scoped(module_id)
        }
        pernixc_syntax::AccessModifier::Public(_) => Accessibility::Public,
        pernixc_syntax::AccessModifier::Internal(_) => {
            // Internal means accessible within the compilation target
            // For now, treat as public - this might need refinement
            Accessibility::Public
        }
    }
}

/// Checks if the first accessibility is more accessible than the second.
fn is_more_accessible(
    first: pernixc_symbol::accessibility::Accessibility<pernixc_symbol::ID>,
    second: pernixc_symbol::accessibility::Accessibility<pernixc_symbol::ID>,
) -> bool {
    use pernixc_symbol::accessibility::Accessibility;

    match (first, second) {
        (Accessibility::Public, Accessibility::Scoped(_)) => true,
        _ => false,
    }
}
