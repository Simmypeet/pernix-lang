//! Contains the definition of the [`Tree`] and [`Node`] representing the
//! concrete tree which contains the full source code fidelity.

use std::{any::TypeId, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::{token, tree::RelativeSpan};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Node {
    Leaf(token::Kind<RelativeSpan>),
    Branch(Arc<Tree>),
}

/// A typeless concrete syntax tree built by the parser.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tree {
    /// The [`TypeId`] of the AST node that this tree represents.
    ///
    /// If `None`, then this tree is errorneous and does not represent a valid
    /// tree.
    pub ast_type_id: Option<TypeId>,

    /// List of nodes this tree contains.
    pub nodes: Vec<Node>,
}
