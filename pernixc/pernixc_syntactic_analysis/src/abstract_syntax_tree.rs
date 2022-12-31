use std::ops::Range;

use pernixc_common::source_file::{SourceFile, TextPosition};
use pernixc_lexical_analysis::token_stream::TokenStream;

use crate::{error::SyntacticError, parser::Parser};

use self::declaration::{NamespaceDeclarationAST, UsingDirectiveAST};

pub mod declaration;
pub mod expression;
pub mod statement;

/// Is a wrapper around a value that also contains the position of the value in the
/// source file.
#[derive(Clone)]
pub struct PositionWrapper<T> {
    pub position: Range<TextPosition>,
    pub value: T,
}

/// Represent an enumeration containing all type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// TypeUnit:
///     PrimitiveTypeUnit
///     | QualifiedName;
/// ```
#[derive(Clone)]
pub enum TypeUnitAST<'src> {
    PrimitiveTypeUnit(PrimitiveTypeUnit),
    QualifiedName(&'src str),
}

/// Represent an enumeration containing all primitive type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// PrimitiveTypeUnit:
///     'void'
///     | 'bool'
///     | 'int8'
///     | 'int16'
///     | 'int32'
///     | 'int64'
///     | 'uint8'
///     | 'uint16'
///     | 'uint32'
///     | 'uint64'
///     | 'float32'
///     | 'float64'
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveTypeUnit {
    Void = 0,
    Bool = 1,
    Int8 = 2,
    Int16 = 3,
    Int32 = 4,
    Int64 = 5,
    Uint8 = 6,
    Uint16 = 7,
    Uint32 = 8,
    Uint64 = 9,
    Float32 = 10,
    Float64 = 11,
}

/// Is an abstract syntax tree that contains all the declarations and using
/// directives of a source file
pub struct FileAST<'src> {
    using_directives: Vec<PositionWrapper<UsingDirectiveAST<'src>>>,
    declarations: Vec<PositionWrapper<NamespaceDeclarationAST<'src>>>,
    source_file: &'src SourceFile,
}

impl<'src> FileAST<'src> {
    /// Return a reference to the using directives of this [`FileAST`].
    pub fn using_directives(&self) -> &[PositionWrapper<UsingDirectiveAST<'src>>] {
        self.using_directives.as_ref()
    }

    /// Return a reference to the declarations of this [`FileAST`].
    pub fn declarations(&self) -> &[PositionWrapper<NamespaceDeclarationAST<'src>>] {
        self.declarations.as_ref()
    }

    /// Return a reference to the source file of this [`FileAST`].
    pub fn source_file(&self) -> &'src SourceFile {
        self.source_file
    }

    /// Parse a [`TokenStream`] into a [`FileAST`].
    pub fn parse(token_stream: &TokenStream<'src>) -> (Self, Vec<SyntacticError>) {
        let mut parser = Parser::new(token_stream);

        let (using_directives, declarations) = parser.parse_file_level_declaration();

        let source_file = token_stream.source_file();

        (
            Self {
                using_directives,
                declarations,
                source_file,
            },
            parser.pop_errors(),
        )
    }
}
