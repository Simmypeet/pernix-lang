use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::syntax::{
    FieldsKey, FunctionBodyKey, FunctionEffectAnnotationKey,
    FunctionSignatureKey, FunctionUnsafeKeywordKey, GenericParametersKey,
    ImplementsFinalKeywordKey, ImplementsMemberAccessModifierKey,
    ImplementsQualifiedIdentifierKey, ImportKey, TypeAliasKey,
    VariantAssociatedTypeKey, WhereClauseKey,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::table::get_table_of_symbol;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Interned<[pernixc_syntax::item::module::Import]>>)]
pub struct ImportProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn import_projection_executor(
    key: &ImportProjectionKey,
    engine: &TrackedEngine,
) -> Option<Interned<[pernixc_syntax::item::module::Import]>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.import_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPORT_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ImportProjectionKey, ImportProjectionExecutor>();

/// Implementation of the `get_module_imports_syntax` method
#[executor(config = Config)]
async fn import_syntax_executor(
    key: &ImportKey,
    engine: &TrackedEngine,
) -> Interned<[pernixc_syntax::item::module::Import]> {
    engine
        .query(&ImportProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPORT_SYNTAX_EXECUTOR: Registration<Config> =
    Registration::new::<ImportKey, ImportSyntaxExecutor>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<QualifiedIdentifier>)]
pub struct ImplementsQualifiedIdentifierProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn implements_qualified_identifier_projection_executor(
    key: &ImplementsQualifiedIdentifierProjectionKey,
    engine: &TrackedEngine,
) -> Option<QualifiedIdentifier> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table
        .implements_qualified_identifier_syntaxes
        .get(&key.symbol_id.id)
        .cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_QUALIFIED_IDENTIFIER_PROJECTION_EXECUTOR: Registration<
    Config,
> = Registration::new::<
    ImplementsQualifiedIdentifierProjectionKey,
    ImplementsQualifiedIdentifierProjectionExecutor,
>();

#[executor(config = Config)]
async fn implements_qualified_identifier_executor(
    key: &ImplementsQualifiedIdentifierKey,
    engine: &TrackedEngine,
) -> QualifiedIdentifier {
    engine
        .query(&ImplementsQualifiedIdentifierProjectionKey {
            symbol_id: key.symbol_id,
        })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_QUALIFIED_IDENTIFIER_EXECUTOR: Registration<Config> =
    Registration::new::<
        ImplementsQualifiedIdentifierKey,
        ImplementsQualifiedIdentifierExecutor,
    >();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::item::generic_parameters::GenericParameters>>)]
pub struct GenericParametersProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn generic_parameters_projection_executor(
    key: &GenericParametersProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::item::generic_parameters::GenericParameters>>
{
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.generic_parameter_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static GENERIC_PARAMETERS_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        GenericParametersProjectionKey,
        GenericParametersProjectionExecutor,
    >();

/// Implementation of the `get_generic_parameters_syntax` method
#[executor(config = Config)]
async fn generic_parameters_syntax(
    key: &GenericParametersKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::generic_parameters::GenericParameters> {
    engine
        .query(&GenericParametersProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static GENERIC_PARAMETERS_EXECUTOR: Registration<Config> =
    Registration::new::<GenericParametersKey, GenericParametersSyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::item::where_clause::Predicates>>)]
pub struct WhereClauseProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn where_clause_projection_executor(
    key: &WhereClauseProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::item::where_clause::Predicates>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.where_clause_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static WHERE_CLAUSE_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<WhereClauseProjectionKey, WhereClauseProjectionExecutor>(
    );

/// Implementation of the `get_where_clause_syntax` method
#[executor(config = Config)]
async fn where_clause_syntax(
    key: &WhereClauseKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::where_clause::Predicates> {
    engine
        .query(&WhereClauseProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static WHERE_CLAUSE_EXECUTOR: Registration<Config> =
    Registration::new::<WhereClauseKey, WhereClauseSyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::r#type::Type>>)]
pub struct TypeAliasProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn type_alias_projection_executor(
    key: &TypeAliasProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::r#type::Type>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.type_alias_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static TYPE_ALIAS_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<TypeAliasProjectionKey, TypeAliasProjectionExecutor>();

/// Implementation of the `get_type_alias_syntax` method
#[executor(config = Config)]
async fn get_type_alias_syntax(
    key: &TypeAliasKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::r#type::Type> {
    engine
        .query(&TypeAliasProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static TYPE_ALIAS_EXECUTOR: Registration<Config> =
    Registration::new::<TypeAliasKey, GetTypeAliasSyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::Keyword>>)]
pub struct ImplementsFinalKeywordProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn implements_final_keyword_projection_executor(
    key: &ImplementsFinalKeywordProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::Keyword>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.final_keywords.get(&key.symbol_id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_FINAL_KEYWORD_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        ImplementsFinalKeywordProjectionKey,
        ImplementsFinalKeywordProjectionExecutor,
    >();

/// Implementation of the `get_implements_final_keyword` method
#[executor(config = Config)]
async fn get_implements_final_keyword(
    key: &ImplementsFinalKeywordKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::Keyword> {
    engine
        .query(&ImplementsFinalKeywordProjectionKey {
            symbol_id: key.symbol_id,
        })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_FINAL_KEYWORD_EXECUTOR: Registration<Config> =
    Registration::new::<ImplementsFinalKeywordKey, GetImplementsFinalKeyword>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::Keyword>>)]
pub struct FunctionUnsafeKeywordProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn function_unsafe_keyword_projection_executor(
    key: &FunctionUnsafeKeywordProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::Keyword>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.function_unsafe_keywords.get(&key.symbol_id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_UNSAFE_KEYWORD_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionUnsafeKeywordProjectionKey,
        FunctionUnsafeKeywordProjectionExecutor,
    >();

/// Implementation of the `get_function_unsafe_keyword` method
#[executor(config = Config)]
async fn get_function_unsafe_keyword(
    key: &FunctionUnsafeKeywordKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::Keyword> {
    engine
        .query(&FunctionUnsafeKeywordProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_UNSAFE_KEYWORD_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionUnsafeKeywordKey, GetFunctionUnsafeKeyword>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::AccessModifier>>)]
pub struct ImplementsMemberAccessModifierProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn implements_member_access_modifier_projection_executor(
    key: &ImplementsMemberAccessModifierProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::AccessModifier>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.implements_access_modifier_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_MEMBER_ACCESS_MODIFIER_PROJECTION_EXECUTOR: Registration<
    Config,
> = Registration::new::<
    ImplementsMemberAccessModifierProjectionKey,
    ImplementsMemberAccessModifierProjectionExecutor,
>();

/// Implementation of the `get_implements_member_access_modifier` method
#[executor(config = Config)]
async fn get_implements_member_access_modifier(
    key: &ImplementsMemberAccessModifierKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::AccessModifier> {
    engine
        .query(&ImplementsMemberAccessModifierProjectionKey {
            symbol_id: key.symbol_id,
        })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_MEMBER_ACCESS_MODIFIER_EXECUTOR: Registration<Config> =
    Registration::new::<
        ImplementsMemberAccessModifierKey,
        GetImplementsMemberAccessModifier,
    >();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::r#type::Type>>)]
pub struct VariantAssociatedTypeProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn variant_associated_type_projection_executor(
    key: &VariantAssociatedTypeProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::r#type::Type>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.variant_associated_type_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static VARIANT_ASSOCIATED_TYPE_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        VariantAssociatedTypeProjectionKey,
        VariantAssociatedTypeProjectionExecutor,
    >();

/// Implementation of the `get_variant_associated_type_syntax` method
#[executor(config = Config)]
async fn get_variant_associated_type_syntax(
    key: &VariantAssociatedTypeKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::r#type::Type> {
    engine
        .query(&VariantAssociatedTypeProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static VARIANT_ASSOCIATED_TYPE_EXECUTOR: Registration<Config> =
    Registration::new::<VariantAssociatedTypeKey, GetVariantAssociatedTypeSyntax>(
    );

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>>)]
pub struct FieldsProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn fields_projection_executor(
    key: &FieldsProjectionKey,
    engine: &TrackedEngine,
) -> Option<
    Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>,
> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.fields_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FIELDS_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<FieldsProjectionKey, FieldsProjectionExecutor>();

/// Implementation of the `get_fields_syntax` method
#[executor(config = Config)]
async fn get_fields_syntax(
    key: &FieldsKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>> {
    engine
        .query(&FieldsProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FIELDS_EXECUTOR: Registration<Config> =
    Registration::new::<FieldsKey, GetFieldsSyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<(Option<pernixc_syntax::item::function::Parameters>, Option<pernixc_syntax::item::function::ReturnType>)>)]
pub struct FunctionSignatureProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn function_signature_projection_executor(
    key: &FunctionSignatureProjectionKey,
    engine: &TrackedEngine,
) -> Option<(
    Option<pernixc_syntax::item::function::Parameters>,
    Option<pernixc_syntax::item::function::ReturnType>,
)> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.function_signature_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_SIGNATURE_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionSignatureProjectionKey,
        FunctionSignatureProjectionExecutor,
    >();

/// Implementation of the `get_function_signature` method
#[executor(config = Config)]
async fn get_function_signature_syntax(
    key: &FunctionSignatureKey,
    engine: &TrackedEngine,
) -> (
    Option<pernixc_syntax::item::function::Parameters>,
    Option<pernixc_syntax::item::function::ReturnType>,
) {
    engine
        .query(&FunctionSignatureProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_SIGNATURE_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionSignatureKey, GetFunctionSignatureSyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>>>)]
pub struct FunctionBodyProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn function_body_projection_executor(
    key: &FunctionBodyProjectionKey,
    engine: &TrackedEngine,
) -> Option<
    Option<pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>>,
> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.function_body_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_BODY_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionBodyProjectionKey,
        FunctionBodyProjectionExecutor,
    >();

/// Implementation of the `get_function_body_syntax` method
#[executor(config = Config)]
async fn get_function_body_syntax(
    key: &FunctionBodyKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>>
{
    engine
        .query(&FunctionBodyProjectionKey { symbol_id: key.symbol_id })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_BODY_EXECUTOR: Registration<Config> =
    Registration::new::<FunctionBodyKey, GetFunctionBodySyntax>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_syntax::item::function::EffectAnnotation>>)]
pub struct FunctionEffectAnnotationProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn function_effect_annotation_projection_executor(
    key: &FunctionEffectAnnotationProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_syntax::item::function::EffectAnnotation>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;

    table.function_effect_annotation_syntaxes.get(&key.symbol_id.id).cloned()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_EFFECT_ANNOTATION_PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionEffectAnnotationProjectionKey,
        FunctionEffectAnnotationProjectionExecutor,
    >();

/// Implementation of the `get_function_effect_annotation_syntax` method
#[executor(config = Config)]
async fn get_function_effect_annotation_syntax(
    key: &FunctionEffectAnnotationKey,
    engine: &TrackedEngine,
) -> Option<pernixc_syntax::item::function::EffectAnnotation> {
    engine
        .query(&FunctionEffectAnnotationProjectionKey {
            symbol_id: key.symbol_id,
        })
        .await
        .unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static FUNCTION_EFFECT_ANNOTATION_EXECUTOR: Registration<Config> =
    Registration::new::<
        FunctionEffectAnnotationKey,
        GetFunctionEffectAnnotationSyntax,
    >();
