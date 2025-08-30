use std::{borrow::Cow, sync::Arc};

use pernixc_arena::Arena;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config, ExtraNamespace,
};
use pernixc_semantic_element::fields::{Field, Fields};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::{get_accessibility, Accessibility},
    get_target_root_module_id,
    parent::get_closest_module_id,
    syntax::get_fields_syntax,
};
use pernixc_target::Global;
use pernixc_term::r#type::Type;
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

impl build::Build for pernixc_semantic_element::fields::Key {
    type Diagnostic = diagnostic::Diagnostic;

    #[allow(clippy::too_many_lines)]
    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<build::Output<Self>, executor::CyclicError> {
        let syntax = engine.get_fields_syntax(key.0).await;

        let Some(syntax_tree) = syntax.and_then(|x| x.members()) else {
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

        let mut fields = Fields::default();

        for field in syntax_tree.members().filter_map(|x| x.into_line().ok()) {
            process_field(
                engine,
                key.0,
                struct_accessibility,
                &extra_namespace,
                field,
                &mut fields,
                &storage,
                &mut occurrences,
            )
            .await?;
        }

        Ok(Output {
            item: fields,
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
async fn process_field(
    engine: &TrackedEngine,
    struct_id: Global<pernixc_symbol::ID>,
    struct_access: Accessibility<pernixc_symbol::ID>,
    extra_namespace: &ExtraNamespace,
    field_syntax: pernixc_syntax::item::r#struct::Field,
    fields: &mut Fields,
    storage: &Storage<Diagnostic>,
    occurrences: &mut Occurrences,
) -> Result<(), executor::CyclicError> {
    // Get field identifier, skip if missing
    let Some(field_identifier) = field_syntax.identifier() else {
        return Ok(());
    };

    let field_name = field_identifier.kind.0.clone();

    // Check for field redefinition
    let redefinition = if let Some(&existing_field_id) =
        fields.field_ids_by_name.get(&field_name)
    {
        let existing_field: &Field =
            fields.fields.get(existing_field_id).unwrap();

        storage.receive(Diagnostic::FieldRedefinition(FieldRedefinition {
            field_name: field_name.clone(),
            redefinition_span: field_syntax.span(),
            original_span: existing_field.span,
            struct_id,
        }));

        true
    } else {
        false
    };

    // Resolve field accessibility
    let field_accessibility =
        if let Some(access_modifier) = field_syntax.access_modifier() {
            create_accessibility_from_modifier(
                &access_modifier,
                struct_id.id,
                struct_id.target_id,
                engine,
            )
            .await
        } else {
            struct_access
        };

    let field_type = if let Some(field_type_syntax) = field_syntax.r#type() {
        // Resolve field type
        let field_type = engine
            .resolve_type(
                &field_type_syntax,
                Config::builder()
                    .observer(occurrences)
                    .extra_namespace(extra_namespace)
                    .referring_site(struct_id)
                    .build(),
                storage,
            )
            .await?;

        let premise = engine.get_active_premise(struct_id).await?;
        let env = Environment::new(
            Cow::Borrowed(&premise),
            Cow::Borrowed(engine),
            normalizer::NO_OP,
        );

        env.simplify_and_check_lifetime_constraints(
            &field_type,
            &field_type_syntax.span(),
            storage,
        )
        .await?
    } else {
        Type::Error(pernixc_term::error::Error)
    };

    // Check if field is more accessible than struct
    if is_more_accessible(
        field_accessibility,
        struct_access,
        struct_id.target_id,
        engine,
    )
    .await?
    {
        storage.receive(Diagnostic::FieldMoreAccessibleThanStruct(
            FieldMoreAccessibleThanStruct {
                struct_accessibility: struct_access,
                struct_id,
                field_span: field_syntax.span(),
                field_accessibility,
                field_name: field_name.clone(),
            },
        ));
    }

    let field = Field {
        accessibility: field_accessibility,
        name: field_name.clone(),
        r#type: field_type,
        span: Some(field_syntax.span()),
    };

    let field_id = fields.fields.insert(field);

    if !redefinition {
        fields.field_ids_by_name.insert(field_name, field_id);
    }

    fields.field_declaration_order.push(field_id);

    Ok(())
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
            let root_module_id =
                engine.get_target_root_module_id(target_id).await;
            Accessibility::Scoped(root_module_id)
        }
    }
}

/// Checks if the first accessibility is more accessible than the second.
async fn is_more_accessible(
    first: pernixc_symbol::accessibility::Accessibility<pernixc_symbol::ID>,
    second: pernixc_symbol::accessibility::Accessibility<pernixc_symbol::ID>,
    target_id: pernixc_target::TargetID,
    engine: &pernixc_query::TrackedEngine,
) -> Result<bool, executor::CyclicError> {
    use pernixc_symbol::{
        accessibility::Accessibility,
        parent::{symbol_hierarchy_relationship, HierarchyRelationship},
    };

    match (first, second) {
        (Accessibility::Public, Accessibility::Scoped(_)) => Ok(true),

        (
            Accessibility::Scoped(_) | Accessibility::Public,
            Accessibility::Public,
        ) => Ok(false),

        (
            Accessibility::Scoped(first_module),
            Accessibility::Scoped(second_module),
        ) => {
            // If first is a child of second, then first is more accessible
            let relationship = engine
                .symbol_hierarchy_relationship(
                    target_id,
                    first_module,
                    second_module,
                )
                .await;

            Ok(matches!(relationship, HierarchyRelationship::Parent))
        }
    }
}
