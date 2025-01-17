//! Contains the definitions of various syntax tree components used for further
//! construction of more derived components.
use derive_more::{Deref, DerefMut};
use pernixc_syntax::syntax_tree;

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
