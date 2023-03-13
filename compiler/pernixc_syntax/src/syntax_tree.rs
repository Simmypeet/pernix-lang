use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, KeywordToken, PunctuationToken};

pub mod declaration;
pub mod expression;
pub mod statement;

/// Is a trait that all syntax tree types must implement.
pub trait SyntaxTree {
    /// Returns a [`Span`] representing the location of the syntax tree node in the source code.
    fn span(&self) -> Span;
}

/// Represents a single identifier syntax tree node.
pub struct IdentifierSyntaxTree(pub IdentifierToken);

impl SyntaxTree for IdentifierSyntaxTree {
    fn span(&self) -> Span { self.0.span }
}

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    pub first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    pub rest: Vec<(Separator, Element)>,
}

impl<Element: SyntaxTree, Separator: SyntaxTree> SyntaxTree for ConnectedList<Element, Separator> {
    fn span(&self) -> Span {
        Span::new(
            self.first.span().start,
            self.rest
                .last()
                .map_or(self.first.span().end, |(_, element)| element.span().end),
        )
    }
}

/// Represents a syntax tree node of two consecutive colon tokens.
///
/// This syntax tree is used to represent the scope separator `::` in the qualified identifier
/// syntax
pub struct ScopeSeparatorSyntaxTree(pub PunctuationToken, pub PunctuationToken);

impl SyntaxTree for ScopeSeparatorSyntaxTree {
    fn span(&self) -> Span { Span::new(self.0.span.start, self.1.span.end) }
}

/// Represents a syntax tree node of identifiers separated by scope separators.
///
/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifierSyntaxTree:
///     IdentifierSyntaxTree ('::' IdentifierSyntaxTree)*
///     ;
/// ```
pub type QualifiedIdentifierSyntaxTree =
    ConnectedList<IdentifierSyntaxTree, ScopeSeparatorSyntaxTree>;

/// Represents a syntax tree node of primitive type identifiers.
///
/// Syntax Synopsis:
/// ``` txt
/// PrimitiveTypeIdentifierSyntaxTree:
///     Bool
///     | Void
///     | Float32
///     | Float64
///     | Int8
///     | Int16
///     | Int32
///     | Int64
///     | Uint8
///     | Uint16
///     | Uint32
///     | Uint64
///     ;
/// ```
pub enum PrimitiveTypeSpecifierSyntaxTree {
    Bool(KeywordToken),
    Void(KeywordToken),
    Float32(KeywordToken),
    Float64(KeywordToken),
    Int8(KeywordToken),
    Int16(KeywordToken),
    Int32(KeywordToken),
    Int64(KeywordToken),
    Uint8(KeywordToken),
    Uint16(KeywordToken),
    Uint32(KeywordToken),
    Uint64(KeywordToken),
}

impl SyntaxTree for PrimitiveTypeSpecifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::Bool(token) => token.span,
            Self::Void(token) => token.span,
            Self::Float32(token) => token.span,
            Self::Float64(token) => token.span,
            Self::Int8(token) => token.span,
            Self::Int16(token) => token.span,
            Self::Int32(token) => token.span,
            Self::Int64(token) => token.span,
            Self::Uint8(token) => token.span,
            Self::Uint16(token) => token.span,
            Self::Uint32(token) => token.span,
            Self::Uint64(token) => token.span,
        }
    }
}

/// Represents a syntax tree node of type specifier.
///
/// The type specifier is used to annotate the type of various symbols in the syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// TypeSpecifierSyntaxTree:
///     PrimitiveTypeIdentifierSyntaxTree
///     | QualifiedIdentifierSyntaxTree
///     ;
/// ```
pub enum TypeSpecifierSyntaxTree {
    Primitive(PrimitiveTypeSpecifierSyntaxTree),
    Qualified(QualifiedIdentifierSyntaxTree),
}

impl SyntaxTree for TypeSpecifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::Qualified(qualified) => qualified.span(),
        }
    }
}
