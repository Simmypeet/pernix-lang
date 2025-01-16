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
