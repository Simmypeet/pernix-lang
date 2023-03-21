use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, KeywordToken, PunctuationToken};

use super::{
    expression::ExpressionSyntaxTree, ConnectedList, SyntaxTree, TypeBindingSyntaxTree,
    TypeSpecifierSyntaxTree,
};

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldSyntaxTree {
    pub type_specifier: TypeSpecifierSyntaxTree,
    pub identifier:     IdentifierToken,
    pub semicolon:      PunctuationToken,
}

impl SyntaxTree for FieldSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_specifier.span().start, self.semicolon.span.end) }
}

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

pub type EnumVariantListSyntaxTree = ConnectedList<IdentifierToken, PunctuationToken>;

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionParameterSyntaxTree {
    pub type_binding: TypeBindingSyntaxTree,
    pub identifier:   IdentifierToken,
}

impl SyntaxTree for FunctionParameterSyntaxTree {
    fn span(&self) -> Span { Span::new(self.type_binding.span().start, self.identifier.span.end) }
}

pub type FunctionParameterListSyntaxTree =
    ConnectedList<FunctionParameterSyntaxTree, PunctuationToken>;

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
