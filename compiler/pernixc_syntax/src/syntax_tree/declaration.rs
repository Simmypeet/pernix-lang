use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, KeywordToken, PunctuationToken};

use super::{
    expression::ExpressionSyntaxTree, statement::SemiColonStatementSyntaxTree, ConnectedList,
    SyntaxTree, TypeSpecifierSyntaxTree,
};

/// Represents an access modifier syntax tree node.
///
/// The access modifier is used to specify the visibility of a declaration.
///
/// Syntax Synopsis:
/// ``` txt
/// AccessModifierSyntaxTree:
///     'public'
///     | 'private'
///     | 'internal'
///     ;
/// ```
pub enum AccessModifierSyntaxTree {
    Public(KeywordToken),
    Private(KeywordToken),
    Internal(KeywordToken),
}

impl SyntaxTree for AccessModifierSyntaxTree {
    fn span(&self) -> Span {
        match self {
            AccessModifierSyntaxTree::Public(keyword) => keyword.span,
            AccessModifierSyntaxTree::Private(keyword) => keyword.span,
            AccessModifierSyntaxTree::Internal(keyword) => keyword.span,
        }
    }
}

/// Represents a syntax tree node of a single field declaration.
///
/// Syntax Synopsis:
/// ``` txt
/// FieldSyntaxTree:
///     TypeSpecifierSyntaxTree IdentifierToken ';'
///     ;
/// ```
pub struct FieldSyntaxTree {
    pub type_specifier: TypeSpecifierSyntaxTree,
    pub identifier:     IdentifierToken,
    pub semicolon:      PunctuationToken,
}

impl SyntaxTree for FieldSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_specifier.span().start, self.semicolon.span.end) }
}

/// Represents a group of field declarations with the same access modifier.
///
/// Syntax Synopsis:
/// ``` txt
/// FieldGroupSyntaxTree:
///     AccessModifierSyntaxTree ':' FieldSyntaxTree*
///     ;
/// ```
pub struct FieldGroupSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub colon:           PunctuationToken,
    pub fields:          Vec<FieldSyntaxTree>,
}

impl SyntaxTree for FieldGroupSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.access_modifier.span().start,
            self.fields.last().unwrap().span().end,
        )
    }
}

/// Represents a struct declaration syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// StructSyntaxTree:
///     'struct' IdentifierToken '{' FieldGroupSyntaxTree* '}'
/// ```
pub struct StructSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub struct_keyword:  KeywordToken,
    pub identifier:      IdentifierToken,
    pub open_brace:      PunctuationToken,
    pub field_groups:    Vec<FieldGroupSyntaxTree>,
    pub close_brace:     PunctuationToken,
}

impl SyntaxTree for StructSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.access_modifier.span().start, self.close_brace.span.end)
    }
}

/// Is used to represent a list of identifiers separated by a comma token.
///
/// It is used to represent the list of variants of an enum.
pub type EnumVariantListSyntaxTree = ConnectedList<IdentifierToken, PunctuationToken>;

/// Represents an enum declaration syntax tree node.
///
/// ``` txt
/// EnumSyntaxTree:
///     'enum' IdentifierToken '{' EnumVariantListSyntaxTree '}'
///     ;
/// ```
pub struct EnumSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub enum_keyword:    KeywordToken,
    pub identifier:      IdentifierToken,
    pub open_brace:      PunctuationToken,
    pub variants:        EnumVariantListSyntaxTree,
    pub close_brace:     PunctuationToken,
}

impl SyntaxTree for EnumSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.access_modifier.span().start, self.close_brace.span.end)
    }
}

/// Represents a function parameter syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionParameterSyntaxTree:
///     TypeSpecifierSyntaxTree IdentifierToken
///     ;
/// ```
pub struct FunctionParameterSyntaxTree {
    pub type_specifier: TypeSpecifierSyntaxTree,
    pub identifier:     IdentifierToken,
}

impl SyntaxTree for FunctionParameterSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_specifier.span().start, self.identifier.span.end) }
}

/// Represents a list of function parameters separated by a comma token.
pub type FunctionParameterListSyntaxTree =
    ConnectedList<FunctionParameterSyntaxTree, PunctuationToken>;

/// Represents a function declaration syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionSyntaxTree:
///     AccessModifierSyntaxTree IdentifierToken '(' FunctionParameterListSyntaxTree? ')'
///     FunctionBodySyntaxTree
///     ;
/// ```
pub struct FunctionSyntaxTree {
    pub access_modifier: AccessModifierSyntaxTree,
    pub identifier:      IdentifierToken,
    pub open_paren:      PunctuationToken,
    pub parameters:      FunctionParameterListSyntaxTree,
    pub close_paren:     PunctuationToken,
    pub body:            FunctionBodySyntaxTree,
}

impl SyntaxTree for FunctionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.access_modifier.span().start, self.body.span().end) }
}

/// Represents a function body that consists of a single expression as its return value.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionExpressionBodySyntaxTree:
///     '=' ExpressionSyntaxTree
///     ;
/// ```
pub struct FunctionExpressionBodySyntaxTree {
    pub equal:             PunctuationToken,
    pub return_expression: ExpressionSyntaxTree,
}

impl SyntaxTree for FunctionExpressionBodySyntaxTree {
    fn span(&self) -> Span { Span::new(self.equal.span.start, self.return_expression.span().end) }
}

/// Represents a function body that consists of a list of statements.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionImperativeBodySyntaxTree:
///     '{' SemiColonStatementSyntaxTree* '}'
///     ;
/// ```
pub struct FunctionImperativeBodySyntaxTree {
    pub open_brace:  PunctuationToken,
    pub statements:  Vec<SemiColonStatementSyntaxTree>,
    pub close_brace: PunctuationToken,
}

impl SyntaxTree for FunctionImperativeBodySyntaxTree {
    fn span(&self) -> Span { Span::new(self.open_brace.span.start, self.close_brace.span.end) }
}

/// Represents a function body syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionBodySyntaxTree:
///     FunctionExpressionBodySyntaxTree
///     | FunctionImperativeBodySyntaxTree
///     ;
/// ```
pub enum FunctionBodySyntaxTree {
    Expression(FunctionExpressionBodySyntaxTree),
    Imperative(FunctionImperativeBodySyntaxTree),
}

impl SyntaxTree for FunctionBodySyntaxTree {
    fn span(&self) -> Span {
        match self {
            FunctionBodySyntaxTree::Expression(body) => body.span(),
            FunctionBodySyntaxTree::Imperative(body) => body.span(),
        }
    }
}
 