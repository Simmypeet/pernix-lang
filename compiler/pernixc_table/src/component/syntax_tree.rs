//! Contains the definitions of various syntax tree components used for further
//! construction of more derived components.
use derive_more::{Deref, DerefMut};
use pernixc_syntax::syntax_tree::{self, item::Parameters};

use super::Input;

/// A **local-input** component used for creating a generic parameters.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct GenericParameters(pub Option<syntax_tree::item::GenericParameters>);

impl Input for GenericParameters {}

/// A **local-input** component used for creating a where clause.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct WhereClause(pub Option<syntax_tree::item::WhereClause>);

impl Input for WhereClause {}

/// A **local-input** component used for creating a generic arguments for the
/// implementation symbols.
///
/// The syntax tree is `implements QUALIFIED_IDENTIFIER[ARGS]`.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct ImplementationQualifiedIdentifier(
    pub syntax_tree::QualifiedIdentifier,
);

impl Input for ImplementationQualifiedIdentifier {}

/// A **local-input** component used for creating a type alias definition for
/// the `type T = ...` symbol.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct TypeAlias(pub syntax_tree::r#type::Type);

impl Input for TypeAlias {}

/// A **local-input** component used for creating a function signature.
/// The syntax tree is `function NAME(PARAMS) -> RETURN_TYPE`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    /// The syntax tree that represents the parameters of the function.
    pub parameters: Parameters,

    /// The return type of the function.
    pub return_type: Option<syntax_tree::item::ReturnType>,
}

impl Input for FunctionSignature {}

/// A **local-input** component used for creating fields of a struct.
/// The syntax tree is `struct NAME { FIELDS }`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fields {
    /// The syntax tree that represents the fields of the struct.
    pub fields: syntax_tree::item::StructBody,
}

impl Input for Fields {}

/// A **local-input** component used for creating a variant of an enum.
/// The syntax tree is `VARIANT(ASSOCIATION)`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The syntax tree that represents the variant association.
    pub variant_association: Option<syntax_tree::item::VariantAssociation>,
}

impl Input for Variant {}
