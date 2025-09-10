//! Contains the queries for retrieving syntax items defined to a particular
//! symbol.

use std::sync::Arc;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;

use crate::{get_table_of_symbol, name::get_qualified_name, ID};

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::query(
    key(ImportKey),
    id(Global<ID>),
    value(Arc<[pernixc_syntax::item::module::Import]>),
    executor(ImportExecutor),
    extend(method(get_module_imports_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn import_syntax_executor(
    id: Global<ID>,
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

/// Implementation of the `get_module_imports_syntax` method
#[pernixc_query::query(
    key(ImplementsQualifiedIdentifierKey),
    id(Global<ID>),
    value(QualifiedIdentifier),
    executor(ImplementsQualifiedIdentifierExecutor),
    extend(method(get_implements_qualified_identifier), no_cyclic)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn implements_qualified_identifier_executor(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(GenericParametersKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::item::generic_parameters::GenericParameters>),
    executor(GenericParametersExecutor),
    extend(method(get_generic_parameters_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn generic_parameters_syntax(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(WhereClauseKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::item::where_clause::Predicates>),
    executor(WhereClauseExecutor),
    extend(method(get_where_clause_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn where_clause_syntax(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<Option<pernixc_syntax::item::where_clause::Predicates>, CyclicError>
{
    let table = engine.get_table_of_symbol(id).await;
    Ok(table
        .where_clause_syntaxes
        .get(&id.id)
        .unwrap_or_else(|| {
            panic!("No where clause syntax found for symbol ID: {:?}", id.id)
        })
        .clone())
}

pernixc_register::register!(WhereClauseKey, WhereClauseExecutor);

/// Implementation of the `get_type_alias_syntax` method
#[pernixc_query::query(
    key(TypeAliasKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::r#type::Type>),
    executor(TypeAliasExecutor),
    extend(method(get_type_alias_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_type_alias_syntax(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(ImplementsFinalKeywordKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::Keyword>),
    executor(ImplementsFinalKeywordExecutor),
    extend(method(get_implements_final_keyword), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_implements_final_keyword(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(ImplementsMemberAccessModifierKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::AccessModifier>),
    executor(ImplementsMemberAccessModifierExecutor),
    extend(method(get_implements_member_access_modifier), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_implements_member_access_modifier(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(VariantAssociatedTypeKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::r#type::Type>),
    executor(VariantAssociatedTypeExecutor),
    extend(method(get_variant_associated_type_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_variant_associated_type_syntax(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(FieldsKey),
    id(Global<ID>),
    value(Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>),
    executor(FieldsExecutor),
    extend(method(get_fields_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_fields_syntax(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(FunctionSignatureKey),
    id(Global<ID>),
    value((
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    )),
    executor(FunctionSignatureExecutor),
    extend(method(get_function_signature_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_function_signature_syntax(
    id: Global<ID>,
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
#[pernixc_query::query(
    key(FunctionBodyKey),
    id(Global<ID>),
    value(Option<
        pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
    >),
    executor(FunctionBodyExecutor),
    extend(method(get_function_body_syntax), no_cyclic),
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn get_function_body_syntax(
    id: Global<ID>,
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
