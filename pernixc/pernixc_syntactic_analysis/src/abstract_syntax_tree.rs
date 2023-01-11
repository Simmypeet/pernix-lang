use std::ops::Range;

use pernixc_common::source_file::TextPosition;

pub mod declaration;
pub mod expression;
pub mod statement;

/// Is a wrapper around a value that also contains the position of the value in the source file.
#[derive(Debug, Clone)]
pub struct PositionWrapper<T> {
    pub position_range: Range<TextPosition>,
    pub value: T,
}

/// Reprsent a module declaration that is defined at the top of every file.
///
/// ANTLR4 grammar:
/// ``` txt
/// Module:
///    'module' QualifiedName ';';
/// ```
pub struct ModuleAST<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
}

/// Reprsent an import declaration that is defined at the top of every file.
///
/// ANTLR4 grammar:
/// ``` txt
/// ImportModule:
///    'import' QualifiedName ';';
/// ```
pub struct ImportModuleAST<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
}

/// Represent a file abstract syntax tree node.
///
/// ANTLR4 grammar:
/// ``` txt
/// File:
///    Module ImportModule* Declaration*;
/// ```
pub struct FileAST<'src> {
    pub module: PositionWrapper<ModuleAST<'src>>,
    pub import_modules: Vec<PositionWrapper<ImportModuleAST<'src>>>,
    pub declarations: Vec<PositionWrapper<declaration::DeclarationAST<'src>>>,
}
