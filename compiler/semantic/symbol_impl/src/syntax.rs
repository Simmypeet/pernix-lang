use std::sync::Arc;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::{
    name::get_qualified_name,
    syntax::{
        FieldsKey, FunctionBodyKey, FunctionDoEffectKey, FunctionSignatureKey,
        GenericParametersKey, ImplementsFinalKeywordKey,
        ImplementsMemberAccessModifierKey, ImplementsQualifiedIdentifierKey,
        ImportKey, TypeAliasKey, VariantAssociatedTypeKey, WhereClauseKey,
    },
};
use pernixc_syntax::QualifiedIdentifier;

use crate::table::get_table_of_symbol;

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::executor(key(ImportKey), name(ImportExecutor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn import_syntax_executor(
    &ImportKey(id): &ImportKey,
    engine: &TrackedEngine,
) -> Result<Arc<[pernixc_syntax::item::module::Import]>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .import_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!("No import syntax found for symbol ID: {:?}", id.id)
        })
        .clone())
}

pernixc_register::register!(ImportKey, ImportExecutor);

#[pernixc_query::executor(
    key(ImplementsQualifiedIdentifierKey),
    name(ImplementsQualifiedIdentifierExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn implements_qualified_identifier_executor(
    &ImplementsQualifiedIdentifierKey(id): &ImplementsQualifiedIdentifierKey,
    engine: &TrackedEngine,
) -> Result<QualifiedIdentifier, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .implements_qualified_identifier_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No implements qualified identifier syntax found for symbol \
                 ID: {:?}",
                id.id
            )
        })
        .clone())
}

pernixc_register::register!(
    ImplementsQualifiedIdentifierKey,
    ImplementsQualifiedIdentifierExecutor
);

/// Implementation of the `get_generic_parameters_syntax` method
#[pernixc_query::executor(
    key(GenericParametersKey),
    name(GenericParametersExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn generic_parameters_syntax(
    &GenericParametersKey(id): &GenericParametersKey,
    engine: &TrackedEngine,
) -> Result<
    Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    CyclicError,
> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(if let Some(value) = table.generic_parameter_syntaxes.get(&id.id) {
        value.clone()
    } else {
        let qualified_name = engine.get_qualified_name(id).await;
        panic!(
            "No generic parameters syntax found for symbol ID: {:?} \
             ({qualified_name})",
            id.id
        )
    })
}

pernixc_register::register!(GenericParametersKey, GenericParametersExecutor);

/// Implementation of the `get_where_clause_syntax` method
#[pernixc_query::executor(key(WhereClauseKey), name(WhereClauseExecutor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn where_clause_syntax(
    &WhereClauseKey(id): &WhereClauseKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::item::where_clause::Predicates>, CyclicError>
{
    let table = engine.get_table_of_symbol(id).await;

    if let Some(value) = table.where_clause_syntaxes.get(&id.id) {
        return Ok(value.clone());
    }

    let qualified_name = engine.get_qualified_name(id).await;
    panic!(
        "No where clause syntax found for symbol ID: {:?} ({qualified_name})",
        id.id
    );
}

pernixc_register::register!(WhereClauseKey, WhereClauseExecutor);

/// Implementation of the `get_type_alias_syntax` method
#[pernixc_query::executor(key(TypeAliasKey), name(TypeAliasExecutor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_type_alias_syntax(
    &TypeAliasKey(id): &TypeAliasKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::r#type::Type>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .type_alias_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!("No type alias syntax found for symbol ID: {:?}", id.id)
        })
        .clone())
}

pernixc_register::register!(TypeAliasKey, TypeAliasExecutor);

/// Implementation of the `get_implements_final_keyword` method
#[pernixc_query::executor(
    key(ImplementsFinalKeywordKey),
    name(ImplementsFinalKeywordExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_implements_final_keyword(
    &ImplementsFinalKeywordKey(id): &ImplementsFinalKeywordKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::Keyword>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(*table.final_keywords.get(&id.id).unwrap_or_else(|| {
        panic!("No final keyword found for symbol ID: {:?}", id.id)
    }))
}

pernixc_register::register!(
    ImplementsFinalKeywordKey,
    ImplementsFinalKeywordExecutor
);

/// Implementation of the `get_implements_member_access_modifier` method
#[pernixc_query::executor(
    key(ImplementsMemberAccessModifierKey),
    name(ImplementsMemberAccessModifierExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_implements_member_access_modifier(
    &ImplementsMemberAccessModifierKey(id): &ImplementsMemberAccessModifierKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::AccessModifier>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .implements_access_modifier_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No implements member access modifier syntax found for symbol \
                 ID: {:?}",
                id.id
            )
        })
        .clone())
}

pernixc_register::register!(
    ImplementsMemberAccessModifierKey,
    ImplementsMemberAccessModifierExecutor
);

/// Implementation of the `get_variant_associated_type_syntax` method
#[pernixc_query::executor(
    key(VariantAssociatedTypeKey),
    name(VariantAssociatedTypeExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_variant_associated_type_syntax(
    &VariantAssociatedTypeKey(id): &VariantAssociatedTypeKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::r#type::Type>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .variant_associated_type_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No variant associated type syntax found for symbol ID: {:?}",
                id.id
            )
        })
        .clone())
}

pernixc_register::register!(
    VariantAssociatedTypeKey,
    VariantAssociatedTypeExecutor
);

/// Implementation of the `get_fields_syntax` method
#[pernixc_query::executor(key(FieldsKey), name(FieldsExecutor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_fields_syntax(
    &FieldsKey(id): &FieldsKey,
    engine: &TrackedEngine,
) -> Result<
    Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>,
    CyclicError,
> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .fields_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!("No fields syntax found for symbol ID: {:?}", id.id)
        })
        .clone())
}

pernixc_register::register!(FieldsKey, FieldsExecutor);

/// Implementation of the `get_function_signature` method
#[pernixc_query::executor(
    key(FunctionSignatureKey),
    name(FunctionSignatureExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_function_signature_syntax(
    &FunctionSignatureKey(id): &FunctionSignatureKey,
    engine: &TrackedEngine,
) -> Result<
    (
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    ),
    CyclicError,
> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .function_signature_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No function signature syntax found for symbol ID: {:?}",
                id.id
            )
        })
        .clone())
}

pernixc_register::register!(FunctionSignatureKey, FunctionSignatureExecutor);

/// Implementation of the `get_fields_syntax` method
#[pernixc_query::executor(key(FunctionBodyKey), name(FunctionBodyExecutor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_function_body_syntax(
    &FunctionBodyKey(id): &FunctionBodyKey,
    engine: &TrackedEngine,
) -> Result<
    Option<pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>>,
    CyclicError,
> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .function_body_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!(
                "No function signature syntax found for symbol ID: {:?}",
                id.id
            )
        })
        .clone())
}

pernixc_register::register!(FunctionBodyKey, FunctionBodyExecutor);

/// Implementation of the `get_fields_syntax` method
#[pernixc_query::executor(
    key(FunctionDoEffectKey),
    name(FunctionDoEffectExecutor)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_function_do_effect_syntax(
    &FunctionDoEffectKey(id): &FunctionDoEffectKey,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::item::function::DoEffect>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    if let Some(value) = table.function_do_effect_syntaxes.get(&id.id) {
        return Ok(value.clone());
    }

    let qualified_identifier = engine.get_qualified_name(id).await;

    panic!(
        "No function do effect syntax found for symbol ID: {:?}, qualified \
         identifier: {:?}",
        id.id, qualified_identifier
    );
}

pernixc_register::register!(FunctionDoEffectKey, FunctionDoEffectExecutor);
