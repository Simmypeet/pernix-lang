use enum_as_inner::EnumAsInner;
use thiserror::Error;

use crate::{symbol::AccessModifier, SourceSpan};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccessibilityLeaking {
    pub symbol_span: SourceSpan,
    pub symbol_access_modifier: AccessModifier,
    pub parent_access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldRedefinition {
    pub new_field_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRedifinition {
    pub available_symbol_index: usize,
    pub new_symbol_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotAccessible {
    pub referencing_site: SourceSpan,
    pub symbol_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolNotFound {
    pub referencing_site: SourceSpan,
    pub in_scope: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeExpected {
    pub span: SourceSpan,
    pub symbol_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterRedifinition {
    pub new_parameter_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error, EnumAsInner)]
#[error("Encountered a semantic error while constructing the symbol table.")]
pub enum SemanticError {
    AccessibilityLeaking(AccessibilityLeaking),
    SymbolRedifinition(SymbolRedifinition),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    SymbolNotFound(SymbolNotFound),
    TypeExpected(TypeExpected),
    ParameterRedifinition(ParameterRedifinition),
    FieldRedefinition(FieldRedefinition),
}
