//! This module contains the finalizing logic for the semantic analysis.
//!
//! The finalizing phase is the last phase and follows after the drafting phase.
//! It builds the symbol table to its final form and having all the correct
//! information.

mod state;
mod symbol;
mod utility;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionKind {
    Normal(syntax_tree::item::Function),
    Extern(syntax_tree::item::ExternFunction),
}

impl FunctionKind {
    pub fn signature(&self) -> &syntax_tree::item::FunctionSignature {
        match self {
            Self::Normal(function) => function.signature(),
            Self::Extern(extern_function) => extern_function.signature(),
        }
    }
}

use pernixc_syntax::syntax_tree;
pub use state::{Element, Finalize, Finalizer};
