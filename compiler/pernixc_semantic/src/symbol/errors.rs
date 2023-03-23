use pernixc_common::source_file::Span;
use thiserror::Error;

use super::{AccessModifier, SymbolID};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolIsMoreAccessibleThanParent {
    pub symbol:                 SymbolID,
    pub parent:                 SymbolID,
    pub symbol_access_modifier: AccessModifier,
    pub parent_access_modifier: AccessModifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolRedifinition {
    pub available_symbol: SymbolID,
    pub new_symbol_span:  Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Error)]
#[error("Encountered a semantic error while constructing the symbol table.")]
pub enum SemanticSymbolError {
    SymbolIsMoreAccessibleThanParent(SymbolIsMoreAccessibleThanParent),
    SymbolRedifinition(SymbolRedifinition),
}
