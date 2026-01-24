use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    name::get_qualified_name,
    syntax::{
        FieldsKey, FunctionBodyKey, FunctionEffectAnnotationKey,
        FunctionSignatureKey, FunctionUnsafeKeywordKey, GenericParametersKey,
        ImplementsFinalKeywordKey, ImplementsMemberAccessModifierKey,
        ImplementsQualifiedIdentifierKey, ImportKey, TypeAliasKey,
        VariantAssociatedTypeKey, WhereClauseKey,
    },
};
use pernixc_syntax::QualifiedIdentifier;
use qbice::{executor, program::Registration, storage::intern::Interned};

use crate::table::get_table_of_symbol;

/// Implementation of the `get_module_imports_syntax` method
#[executor(config = Config)]
async fn import_syntax_executor(
    key: &ImportKey,
    engine: &TrackedEngine,
) -> Interned<[pernixc_syntax::item::module::Import]> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .import_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No import syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPORT_SYNTAX_EXECUTOR: Registration<Config> =
    Registration::new::<ImportKey, ImportSyntaxExecutor>();

#[executor(config = Config)]
async fn implements_qualified_identifier_executor(
    key: &ImplementsQualifiedIdentifierKey,
    engine: &TrackedEngine,
) -> QualifiedIdentifier {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .implements_qualified_identifier_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No implements qualified identifier syntax found for symbol \
                 ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_QUALIFIED_IDENTIFIER_EXECUTOR: Registration<Config> =
    Registration::new::<
        ImplementsQualifiedIdentifierKey,
        ImplementsQualifiedIdentifierExecutor,
    >();

/// Implementation of the `get_generic_parameters_syntax` method
#[executor(config = Config)]
async fn generic_parameters_syntax(
    key: &GenericParametersKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::generic_parameters::GenericParameters> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;

    if let Some(value) = table.generic_parameter_syntaxes.get(&key.symbol_id.id)
    {
        value.clone()
    } else {
        let qualified_name = engine.get_qualified_name(key.symbol_id).await;
        panic!(
            "No generic parameters syntax found for symbol ID: {:?} \
             ({qualified_name})",
            key.symbol_id.id
        )
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static GENERIC_PARAMETERS_EXECUTOR: Registration<Config> =
    Registration::new::<GenericParametersKey, GenericParametersSyntax>();

/// Implementation of the `get_where_clause_syntax` method
#[executor(config = Config)]
async fn where_clause_syntax(
    key: &WhereClauseKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::where_clause::Predicates> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;

    if let Some(value) = table.where_clause_syntaxes.get(&key.symbol_id.id) {
        return value.clone();
    }

    let qualified_name = engine.get_qualified_name(key.symbol_id).await;
    panic!(
        "No where clause syntax found for symbol ID: {:?} ({qualified_name})",
        key.symbol_id.id
    );
}

#[distributed_slice(PERNIX_PROGRAM)]
static WHERE_CLAUSE_EXECUTOR: Registration<Config> =
    Registration::new::<WhereClauseKey, WhereClauseSyntax>();

/// Implementation of the `get_type_alias_syntax` method
#[executor(config = Config)]
async fn get_type_alias_syntax(
    key: &TypeAliasKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::r#type::Type> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .type_alias_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No type alias syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static TYPE_ALIAS_EXECUTOR: Registration<Config> =
    Registration::new::<TypeAliasKey, GetTypeAliasSyntax>();

/// Implementation of the `get_implements_final_keyword` method
#[executor(config = Config)]
async fn get_implements_final_keyword(
    key: &ImplementsFinalKeywordKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::Keyword> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    *table.final_keywords.get(&key.symbol_id.id).unwrap_or_else(|| {
        panic!("No final keyword found for symbol ID: {:?}", key.symbol_id.id)
    })
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_FINAL_KEYWORD_EXECUTOR: Registration<Config> =
    Registration::new::<ImplementsFinalKeywordKey, GetImplementsFinalKeyword>();

/// Implementation of the `get_function_unsafe_keyword` method
#[executor(config = Config)]
async fn get_function_unsafe_keyword(
    key: &FunctionUnsafeKeywordKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::Keyword> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    *table.function_unsafe_keywords.get(&key.symbol_id.id).unwrap_or_else(
        || {
            panic!(
                "No function unsafe keyword found for symbol ID: {:?}",
                key.symbol_id.id
            )
        },
    )
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_UNSAFE_KEYWORD_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionUnsafeKeywordKey, GetFunctionUnsafeKeyword>();

/// Implementation of the `get_implements_member_access_modifier` method
#[executor(config = Config)]
async fn get_implements_member_access_modifier(
    key: &ImplementsMemberAccessModifierKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::AccessModifier> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .implements_access_modifier_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No implements member access modifier syntax found for symbol \
                 ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_MEMBER_ACCESS_MODIFIER_EXECUTOR: Registration<Config> =
    Registration::new::<
        ImplementsMemberAccessModifierKey,
        GetImplementsMemberAccessModifier,
    >();

/// Implementation of the `get_variant_associated_type_syntax` method
#[executor(config = Config)]
async fn get_variant_associated_type_syntax(
    key: &VariantAssociatedTypeKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::r#type::Type> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .variant_associated_type_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No variant associated type syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static VARIANT_ASSOCIATED_TYPE_EXECUTOR: Registration<Config> =
    Registration::new::<VariantAssociatedTypeKey, GetVariantAssociatedTypeSyntax>(
    );

/// Implementation of the `get_fields_syntax` method
#[executor(config = Config)]
async fn get_fields_syntax(
    key: &FieldsKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .fields_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No fields syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FIELDS_EXECUTOR: Registration<Config> =
    Registration::new::<FieldsKey, GetFieldsSyntax>();

/// Implementation of the `get_function_signature` method
#[executor(config = Config)]
async fn get_function_signature_syntax(
    key: &FunctionSignatureKey,
    engine: &TrackedEngine,
) -> (
    Option<pernixc_syntax::item::function::Parameters>,
    Option<pernixc_syntax::item::function::ReturnType>,
) {
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .function_signature_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No function signature syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_SIGNATURE_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionSignatureKey, GetFunctionSignatureSyntax>();

/// Implementation of the `get_fields_syntax` method
#[executor(config = Config)]
async fn get_function_body_syntax(
    key: &FunctionBodyKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>>
{
    let table = engine.get_table_of_symbol(key.symbol_id).await;
    table
        .function_body_syntaxes
        .get(&key.symbol_id.id)
        .unwrap_or_else(|| {
            panic!(
                "No function signature syntax found for symbol ID: {:?}",
                key.symbol_id.id
            )
        })
        .clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_BODY_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionBodyKey, GetFunctionBodySyntax>();

/// Implementation of the `get_fields_syntax` method
#[executor(config = Config)]
async fn get_function_effect_annotation_syntax(
    key: &FunctionEffectAnnotationKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::function::EffectAnnotation> {
    let table = engine.get_table_of_symbol(key.symbol_id).await;

    if let Some(value) =
        table.function_effect_annotation_syntaxes.get(&key.symbol_id.id)
    {
        return value.clone();
    }

    let qualified_identifier = engine.get_qualified_name(key.symbol_id).await;

    panic!(
        "No function do effect syntax found for symbol ID: {:?}, qualified \
         identifier: {:?}",
        key.symbol_id.id, qualified_identifier
    );
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_EFFECT_ANNOTATION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionEffectAnnotationKey,
        GetFunctionEffectAnnotationSyntax,
    >();
