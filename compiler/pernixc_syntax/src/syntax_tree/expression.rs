use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{
    IdentifierToken, KeywordToken, NumericLiteralToken, PunctuationToken,
};

use super::{
    statement::StatementSyntaxTree, ConnectedList, QualifiedIdentifierSyntaxTree, SyntaxTree,
};
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ExpressionSyntaxTree {
    FunctionalExpression(FunctionalExpressionSyntaxTree),
    ImperativeExpression(ImperativeExpressionSyntaxTree),
}

impl SyntaxTree for ExpressionSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::FunctionalExpression(expression) => expression.span(),
            Self::ImperativeExpression(expression) => expression.span(),
        }
    }
}

/// Is an enum of all the possible functional expressions.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionalExpressionSyntaxTree:
///     NumericLiteral
///     | BooleanLiteral
///     | BinaryExpression
///     | PrefixExpression
///     | IdentifierExpression
///     | FunctionCallExpression
///     | ParenthesizedExpression
///     | StructLiteralSyntaxTree
///     | MemberAccessExpression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum FunctionalExpressionSyntaxTree {
    NumericLiteral(NumericLiteralSyntaxTree),
    BooleanLiteral(BooleanLiteralSyntaxTree),
    BinaryExpression(BinaryExpressionSyntaxTree),
    PrefixExpression(PrefixExpressionSyntaxTree),
    IdentifierExpression(IdentifierExpressionSyntaxTree),
    FunctionCallExpression(FunctionCallExpressionSyntaxTree),
    ParenthesizedExpression(ParenthesizedExpressionSyntaxTree),
    StructLiteralSyntaxTree(StructLiteralSyntaxTree),
    MemberAccessExpression(MemberAccessExpressionSyntaxTree),
}

impl SyntaxTree for FunctionalExpressionSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::NumericLiteral(literal) => literal.span(),
            Self::BooleanLiteral(literal) => literal.span(),
            Self::BinaryExpression(expression) => expression.span(),
            Self::PrefixExpression(expression) => expression.span(),
            Self::IdentifierExpression(expression) => expression.span(),
            Self::FunctionCallExpression(expression) => expression.span(),
            Self::ParenthesizedExpression(expression) => expression.span(),
            Self::StructLiteralSyntaxTree(expression) => expression.span(),
            Self::MemberAccessExpression(expression) => expression.span(),
        }
    }
}

/// Represents a single numeric literal token.
///
/// Syntax Synopsis:
/// ``` text
/// NumericLiteralSyntaxTree:
///     NumericLiteral
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteralSyntaxTree(pub NumericLiteralToken);

impl SyntaxTree for NumericLiteralSyntaxTree {
    fn span(&self) -> Span { self.0.span }
}

/// Represents either a `true` or `false` keyword token.
///
/// Syntax Synopsis:
/// ``` text
/// BooleanLiteralSyntaxTree:
///     'true' | 'false'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum BooleanLiteralSyntaxTree {
    True(KeywordToken),
    False(KeywordToken),
}

impl SyntaxTree for BooleanLiteralSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::True(keyword) => keyword.span,
            Self::False(keyword) => keyword.span,
        }
    }
}

/// Is an enumeration of all binary operator syntax tree nodes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum BinaryOperatorSyntaxTree {
    Add(PunctuationToken),
    Subtract(PunctuationToken),
    Multiply(PunctuationToken),
    Divide(PunctuationToken),
    Modulo(PunctuationToken),
    Assign(PunctuationToken),
    CompoundAdd(PunctuationToken, PunctuationToken),
    CompoundSubtract(PunctuationToken, PunctuationToken),
    CompoundMultiply(PunctuationToken, PunctuationToken),
    CompoundDivide(PunctuationToken, PunctuationToken),
    CompoundModulo(PunctuationToken, PunctuationToken),
    Equal(PunctuationToken, PunctuationToken),
    NotEqual(PunctuationToken, PunctuationToken),
    LessThan(PunctuationToken),
    LessThanOrEqual(PunctuationToken, PunctuationToken),
    GreaterThan(PunctuationToken),
    GreaterThanOrEqual(PunctuationToken, PunctuationToken),
    LogicalAnd(KeywordToken),
    LogicalOr(KeywordToken),
}

impl SyntaxTree for BinaryOperatorSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::Add(token) => token.span,
            Self::Subtract(token) => token.span,
            Self::Multiply(token) => token.span,
            Self::Divide(token) => token.span,
            Self::Modulo(token) => token.span,
            Self::Assign(token) => token.span,
            Self::CompoundAdd(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::CompoundSubtract(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::CompoundMultiply(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::CompoundDivide(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::CompoundModulo(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::Equal(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::NotEqual(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::LessThan(token) => token.span,
            Self::LessThanOrEqual(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::GreaterThan(token) => token.span,
            Self::GreaterThanOrEqual(token, token1) => Span::new(token.span.start, token1.span.end),
            Self::LogicalAnd(token) => token.span,
            Self::LogicalOr(token) => token.span,
        }
    }
}

/// Represents an expression that takes two expressions and applies a binary operator to them.
///
/// Syntax Synopsis:
/// ``` text
/// BinaryExpressionSyntaxTree:
///     ExpressionSyntaxTree BinaryOperatorSyntaxTree ExpressionSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryExpressionSyntaxTree {
    pub left:     Box<ExpressionSyntaxTree>,
    pub operator: BinaryOperatorSyntaxTree,
    pub right:    Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for BinaryExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.left.span().start, self.right.span().end) }
}

/// Is an enumeration of all prefix operator syntax tree nodes.
///
/// Syntax Synopsis:
/// ``` text
/// PerfixOperatorSyntaxTree:
///     '!' | '-'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PerfixOperatorSyntaxTree {
    LogicalNot(PunctuationToken),
    Negate(PunctuationToken),
}

impl SyntaxTree for PerfixOperatorSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token) => token.span,
            Self::Negate(token) => token.span,
        }
    }
}

/// Is an expression that takes a single expression and applies a prefix operator to it.
///
/// Syntax Synopsis:
/// ``` text
/// PrefixExpressionSyntaxTree:
///     PerfixOperatorSyntaxTree ExpressionSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrefixExpressionSyntaxTree {
    pub operator: PerfixOperatorSyntaxTree,
    pub operand:  Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for PrefixExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.operator.span().start, self.operand.span().end) }
}

/// Is an expression that yields a value by referencing a named value.
///
/// Syntax Synopsis:
/// ``` text
/// IdentifierExpressionSyntaxTree:
///     QualifiedIdentifierSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentifierExpressionSyntaxTree(pub QualifiedIdentifierSyntaxTree);

impl SyntaxTree for IdentifierExpressionSyntaxTree {
    fn span(&self) -> Span { self.0.span() }
}

pub type ArgumentListSyntaxTree = ConnectedList<Box<ExpressionSyntaxTree>, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCallExpressionSyntaxTree {
    pub identifier:  IdentifierExpressionSyntaxTree,
    pub left_paren:  PunctuationToken,
    pub arguments:   ArgumentListSyntaxTree,
    pub right_paren: PunctuationToken,
}

impl SyntaxTree for FunctionCallExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.identifier.span().start, self.right_paren.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParenthesizedExpressionSyntaxTree {
    pub left_paren:  PunctuationToken,
    pub expression:  Box<ExpressionSyntaxTree>,
    pub right_paren: PunctuationToken,
}

impl SyntaxTree for ParenthesizedExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.left_paren.span.start, self.right_paren.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldInitializeSyntaxTree {
    pub identifier: IdentifierExpressionSyntaxTree,
    pub colon:      PunctuationToken,
    pub expression: Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for FieldInitializeSyntaxTree {
    fn span(&self) -> Span { Span::new(self.identifier.span().start, self.expression.span().end) }
}

pub type FieldInitializeListSyntaxTree = ConnectedList<FieldInitializeSyntaxTree, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructLiteralSyntaxTree {
    pub identifier:            IdentifierExpressionSyntaxTree,
    pub left_brace:            PunctuationToken,
    pub field_initializations: FieldInitializeListSyntaxTree,
    pub right_brace:           PunctuationToken,
}

impl SyntaxTree for StructLiteralSyntaxTree {
    fn span(&self) -> Span { Span::new(self.identifier.span().start, self.right_brace.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberAccessExpressionSyntaxTree {
    pub expression: Box<ExpressionSyntaxTree>,
    pub dot:        PunctuationToken,
    pub identifier: IdentifierToken,
}

impl SyntaxTree for MemberAccessExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.expression.span().start, self.identifier.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ImperativeExpressionSyntaxTree {
    BlockExpression(BlockExpressionSyntaxTree),
    IfElseExpression(IfElseExpressionSyntaxTree),
    LoopExpression(LoopExpressionSyntaxTree),
}

impl SyntaxTree for ImperativeExpressionSyntaxTree {
    fn span(&self) -> Span {
        match self {
            Self::BlockExpression(block) => block.span(),
            Self::IfElseExpression(if_else) => if_else.span(),
            Self::LoopExpression(loop_) => loop_.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemiColonStatementSyntaxTree {
    pub expression: Box<StatementSyntaxTree>,
    pub semi_colon: PunctuationToken,
}

impl SyntaxTree for SemiColonStatementSyntaxTree {
    fn span(&self) -> Span { Span::new(self.expression.span().start, self.semi_colon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopLabelSyntaxTree {
    pub single_quote: PunctuationToken,
    pub identifier:   IdentifierToken,
    pub colon:        PunctuationToken,
}

impl SyntaxTree for LoopLabelSyntaxTree {
    fn span(&self) -> Span { Span::new(self.single_quote.span.start, self.colon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockExpressionSyntaxTree {
    pub left_brace:  PunctuationToken,
    pub statements:  Vec<StatementSyntaxTree>,
    pub right_brace: PunctuationToken,
}

impl SyntaxTree for BlockExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.left_brace.span.start, self.right_brace.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElseExpressionSyntaxTree {
    pub else_keyword: KeywordToken,
    pub expression:   Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for ElseExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.else_keyword.span.start, self.expression.span().end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfElseExpressionSyntaxTree {
    pub if_keyword:      KeywordToken,
    pub left_paren:      PunctuationToken,
    pub condition:       Box<ExpressionSyntaxTree>,
    pub right_paren:     PunctuationToken,
    pub then_expression: Box<ExpressionSyntaxTree>,
    pub else_expression: Option<ElseExpressionSyntaxTree>,
}

impl SyntaxTree for IfElseExpressionSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.if_keyword.span.start,
            self.else_expression
                .as_ref()
                .map(|else_expression| else_expression.span().end)
                .unwrap_or_else(|| self.then_expression.span().end),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopExpressionSyntaxTree {
    pub loop_label:   Option<LoopLabelSyntaxTree>,
    pub loop_keyword: KeywordToken,
    pub expression:   Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for LoopExpressionSyntaxTree {
    fn span(&self) -> Span {
        let start = self
            .loop_label
            .as_ref()
            .map(|label| label.span().start)
            .unwrap_or(self.loop_keyword.span.start);
        Span::new(start, self.expression.span().end)
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`ExpressionSyntaxTree`]
    ///
    /// Returns `None` if the current token cannot be parsed as an expression.
    pub fn parse_expression(&mut self) -> Option<ExpressionSyntaxTree> { todo!() }
}
