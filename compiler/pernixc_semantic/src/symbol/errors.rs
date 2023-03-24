use enum_as_inner::EnumAsInner;
use thiserror::Error;

use super::{AccessModifier, SymbolID};
use crate::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AccessibilityLeaking {
    pub symbol:                 SymbolID,
    pub parent:                 SymbolID,
    pub symbol_access_modifier: AccessModifier,
    pub parent_access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRedifinition {
    pub available_symbol: SymbolID,
    pub new_symbol_span:  SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error, EnumAsInner)]
#[error("Encountered a semantic error while constructing the symbol table.")]
pub enum SemanticSymbolError {
    AccessibilityLeaking(AccessibilityLeaking),
    SymbolRedifinition(SymbolRedifinition),
}
