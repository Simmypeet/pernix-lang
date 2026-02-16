//! Contains the queries for retrieving syntax items defined to a particular
//! symbol.

use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<[pernixc_syntax::item::module::Import]>)]
#[extend(name = get_module_imports_syntax, by_val)]
pub struct ImportKey {
    /// The global identifier of the module symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(QualifiedIdentifier)]
#[extend(name = get_implements_qualified_identifier, by_val)]
pub struct ImplementsQualifiedIdentifierKey {
    /// The global identifier of the implements symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::item::generic_parameters::GenericParameters>)]
#[extend(name = get_generic_parameters_syntax, by_val)]
pub struct GenericParametersKey {
    /// The global identifier of the symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::item::where_clause::Predicates>)]
#[extend(name = get_where_clause_syntax, by_val)]
pub struct WhereClauseKey {
    /// The global identifier of the symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::r#type::Type>)]
#[extend(name = get_type_alias_syntax, by_val)]
pub struct TypeAliasKey {
    /// The global identifier of the type alias symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::Keyword>)]
#[extend(name = get_implements_final_keyword, by_val)]
pub struct ImplementsFinalKeywordKey {
    /// The global identifier of the implements symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::AccessModifier>)]
#[extend(name = get_implements_member_access_modifier, by_val)]
pub struct ImplementsMemberAccessModifierKey {
    /// The global identifier of the implements member symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::r#type::Type>)]
#[extend(name = get_variant_associated_type_syntax, by_val)]
pub struct VariantAssociatedTypeKey {
    /// The global identifier of the variant symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>>)]
#[extend(name = get_fields_syntax, by_val)]
pub struct FieldsKey {
    /// The global identifier of the struct symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value((
    Option<pernixc_syntax::item::function::Parameters>,
    Option<pernixc_syntax::item::function::ReturnType>,
))]
#[extend(name = get_function_signature_syntax, by_val)]
pub struct FunctionSignatureKey {
    /// The global identifier of the function symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<
     pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
>)]
#[extend(name = get_function_body_syntax, by_val)]
pub struct FunctionBodyKey {
    /// The global identifier of the function symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::item::function::EffectAnnotation>)]
#[extend(name = get_function_effect_annotation_syntax, by_val)]
pub struct FunctionEffectAnnotationKey {
    /// The global identifier of the function symbol.
    pub symbol_id: Global<ID>,
}

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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<pernixc_syntax::Keyword>)]
#[extend(name = get_function_unsafe_keyword, by_val)]
pub struct FunctionUnsafeKeywordKey {
    /// The global identifier of the function symbol.
    pub symbol_id: Global<ID>,
}
