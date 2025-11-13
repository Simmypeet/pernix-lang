//! Contains the queries for retrieving syntax items defined to a particular
//! symbol.

use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;

use crate::ID;

/// Retrieves a list of import syntax defined in a module.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<[pernixc_syntax::item::module::Import]>)]
#[extend(method(get_module_imports_syntax), no_cyclic)]
pub struct ImportKey(pub Global<ID>);

/// Retrieves the qualified identifier of a trait/adt/marker that is being
/// implemented.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(QualifiedIdentifier)]
#[extend(method(get_implements_qualified_identifier), no_cyclic)]
pub struct ImplementsQualifiedIdentifierKey(pub Global<ID>);

/// Retrieves the generic parameters syntax defined to a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::item::generic_parameters::GenericParameters>)]
#[extend(method(get_generic_parameters_syntax), no_cyclic)]
pub struct GenericParametersKey(pub Global<ID>);

/// Retrieves the where clause syntax defined to a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::item::where_clause::Predicates>)]
#[extend(method(get_where_clause_syntax), no_cyclic)]
pub struct WhereClauseKey(pub Global<ID>);

/// Retrieves the type alias syntax defined to a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::r#type::Type>)]
#[extend(method(get_type_alias_syntax), no_cyclic)]
pub struct TypeAliasKey(pub Global<ID>);

/// Retrieves the final keyword defined to a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::Keyword>)]
#[extend(method(get_implements_final_keyword), no_cyclic)]
pub struct ImplementsFinalKeywordKey(pub Global<ID>);

/// Retrieves the access modifier defined to an implements member.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::AccessModifier>)]
#[extend(method(get_implements_member_access_modifier), no_cyclic)]
pub struct ImplementsMemberAccessModifierKey(pub Global<ID>);

/// Retrieves the access modifier defined to an implements member.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::r#type::Type>)]
#[extend(method(get_variant_associated_type_syntax), no_cyclic)]
pub struct VariantAssociatedTypeKey(pub Global<ID>);

/// Retrieves the fields syntax defined to a struct.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>)]
#[extend(method(get_fields_syntax), no_cyclic)]
pub struct FieldsKey(pub Global<ID>);

/// Retrieves the function signature syntax defined to a function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value((
    Option<pernixc_syntax::item::function::Parameters>,
    Option<pernixc_syntax::item::function::ReturnType>,
))]
#[extend(method(get_function_signature_syntax), no_cyclic)]
pub struct FunctionSignatureKey(pub Global<ID>);

/// Retrieves the function body syntax defined to a function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<
     pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
>)]
#[extend(method(get_function_body_syntax), no_cyclic)]
pub struct FunctionBodyKey(pub Global<ID>);

/// Retrieves the function body syntax defined to a function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::item::function::EffectAnnotation>)]
#[extend(method(get_function_effect_annotation_syntax), no_cyclic)]
pub struct FunctionEffectAnnotationKey(pub Global<ID>);

/// Retrieves the unsafe keyword defined to a function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Option<pernixc_syntax::Keyword>)]
#[extend(method(get_function_unsafe_keyword), no_cyclic)]
pub struct FunctionUnsafeKeywordKey(pub Global<ID>);
