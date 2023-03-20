use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, KeywordToken, PunctuationToken};

use super::{
    expression::ExpressionSyntaxTree, ConnectedList, SyntaxTree, TypeBindingSyntaxTree,
    TypeSpecifierSyntaxTree,
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumSyntaxTree {
    pub enum_keyword: KeywordToken,
    pub identifier:   IdentifierToken,
    pub open_brace:   PunctuationToken,
    pub variants:     EnumVariantListSyntaxTree,
    pub close_brace:  PunctuationToken,
}

impl SyntaxTree for EnumSyntaxTree {
    fn span(&self) -> Span { Span::new(self.enum_keyword.span().start, self.close_brace.span.end) }
}

/// Represents a function parameter syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionParameterSyntaxTree:
///     TypeBindingSyntaxTree IdentifierToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionParameterSyntaxTree {
    pub type_binding: TypeBindingSyntaxTree,
    pub identifier:   IdentifierToken,
}

impl SyntaxTree for FunctionParameterSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_binding.span().start, self.identifier.span.end) }
}

/// Represents a list of function parameters separated by a comma token.
pub type FunctionParameterListSyntaxTree =
    ConnectedList<FunctionParameterSyntaxTree, PunctuationToken>;

/// Represents a function declaration syntax tree node.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionSyntaxTree:
///     TypeSpecifierSyntaxTree IdentifierToken '(' FunctionParameterListSyntaxTree? ')'
///     ExpressionSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSyntaxTree {
    pub type_specifier: TypeSpecifierSyntaxTree,
    pub identifier:     IdentifierToken,
    pub open_paren:     PunctuationToken,
    pub parameters:     FunctionParameterListSyntaxTree,
    pub close_paren:    PunctuationToken,
    pub body:           ExpressionSyntaxTree,
}

impl SyntaxTree for FunctionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_specifier.span().start, self.body.span().end) }
}

/// Is an enum that represents all possible item syntax tree nodes.
///
/// Syntax Synopsis:
/// ``` txt
/// ItemSyntaxTree:
///     StructSyntaxTree
///     | EnumSyntaxTree
///     | FunctionSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ItemSyntaxTree {
    Struct(StructSyntaxTree),
    Enum(EnumSyntaxTree),
    Function(FunctionSyntaxTree),
}

impl SyntaxTree for ItemSyntaxTree {
    fn span(&self) -> Span {
        match self {
            ItemSyntaxTree::Struct(item) => item.span(),
            ItemSyntaxTree::Enum(item) => item.span(),
            ItemSyntaxTree::Function(item) => item.span(),
        }
    }
}
