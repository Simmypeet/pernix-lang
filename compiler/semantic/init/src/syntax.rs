use derive_more::{Deref, DerefMut};
use pernixc_query::Value;
use pernixc_serialize::{
    extension::{SharedPointerDeserialize, SharedPointerSerialize},
    Deserialize, Serialize,
};
use pernixc_syntax::item::Members;
use pernixc_target::Global;

use crate::symbol;

/// Represents the generic parameters syntax that can be found in various kinds
/// of symbols, such as functions, structs, and enums.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(GenericParametersKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_generic_parameters_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(GenericParametersExt)
)]
pub struct GenericParameters(
    pub Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
);

/// Represents the type alias syntax `= Type` that can be found in various
/// kind of type alias symbols such as module-level type aliases or
/// trait-implementation associated type aliases.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(TypeAliasKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_type_alias_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(TypeAliasExt)
)]
pub struct TypeAlias(pub pernixc_syntax::r#type::Type);

/// The qualified identifier that can be found in the `implements[...]
/// QUALIFIED_IDENTIFIER` syntax of the implementation symbols such as trait
/// implementations or ADT implementations.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[ext(
    method(get_implementation_qualified_identifier),
    unwrap("should have no cyclic dependencies"),
    trait(ImplementationQualifiedIdentifierExt)
)]
#[key(ImplementationQualifiedIdentifierKey)]
#[id(Global<symbol::ID>)]
pub struct ImplementationQualifiedIdentifier(
    pub pernixc_syntax::QualifiedIdentifier,
);

/// Represents the where clause syntax that can be found in various kinds
/// of symbols, such as functions, structs, and enums.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(WhereClauseKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_where_clause_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(WhereClauseExt)
)]
pub struct WhereClause(
    pub Option<pernixc_syntax::item::where_clause::Predicates>,
);

/// Represents the function signature syntax `(PARAMETERS) -> RETURN_TYPE`
/// that can be found in all kinds of function symbols, such as module-level
/// functions, trait methods, and implementation associated functions.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(FunctionSignatureKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_function_signature_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(FunctionSignatureExt)
)]
pub struct FunctionSignature {
    /// The syntax tree that represents the parameters of the function.
    pub parameters: pernixc_syntax::item::function::Parameters,

    /// The return type of the function.
    pub return_type: Option<pernixc_syntax::item::function::ReturnType>,
}

/// Represents a list of statements inside the function body.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(StatementsKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_statements_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(StatementsExt)
)]
pub struct Statements(
    pub Option<Members<pernixc_syntax::statement::Statement>>,
);

/// Represents the list of field syntaxes inside the struct body.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(FieldsKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_fields_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(FieldsExt)
)]
pub struct Fields(
    pub  Option<
        pernixc_syntax::item::Members<pernixc_syntax::item::r#struct::Field>,
    >,
);

/// Represents the optional variant association syntax
/// `VARRIANT_IDENT(ASSOCIATION)`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
    Value,
)]
#[serde(
    ser_extension(SharedPointerSerialize),
    de_extension(SharedPointerDeserialize)
)]
#[key(VariantKey)]
#[id(Global<symbol::ID>)]
#[ext(
    method(get_variant_syntax),
    unwrap("should have no cyclic dependencies"),
    trait(VariantExt)
)]
pub struct Variant(
    pub Option<pernixc_syntax::item::r#enum::VariantAssociation>,
);
