//! This module contains the finalizing logic for the semantic analysis.
//!
//! The finalizing phase is the last phase and follows after the drafting phase.
//! It builds the symbol table to its final form and having all the correct
//! information.

mod check;
mod finalize;
mod finalizer;
mod generic_parameters;
mod implementation;
mod occurrences;
mod variance;
mod where_caluse;

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

pub use finalize::Finalize;
pub use finalizer::{Element, Finalizer};
use pernixc_syntax::syntax_tree;
