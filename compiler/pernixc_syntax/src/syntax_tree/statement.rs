use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, KeywordToken, PunctuationToken};

use super::{
    expression::{
        ExpressionSyntaxTree, FunctionalExpressionSyntaxTree, ImperativeExpressionSyntaxTree,
    },
    LabelSyntaxTree, SyntaxTree, TypeBindingSyntaxTree,
};
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum StatementSyntaxTree {
    Declaration(DeclarationStatementSyntaxTree),
    Expression(ExpressionStatementSyntaxTree),
    Control(ControlStatementSyntaxTree),
}

impl SyntaxTree for StatementSyntaxTree {
    fn span(&self) -> Span {
        match self {
            StatementSyntaxTree::Declaration(declaration) => declaration.span(),
            StatementSyntaxTree::Expression(expression) => expression.span(),
            StatementSyntaxTree::Control(control) => control.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum DeclarationStatementSyntaxTree {
    VariableDeclaration(VariableDeclarationStatementSyntaxTree),
}

impl SyntaxTree for DeclarationStatementSyntaxTree {
    fn span(&self) -> Span {
        match self {
            DeclarationStatementSyntaxTree::VariableDeclaration(declaration) => declaration.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetBindingSyntaxTree {
    pub mutable_keyword: Option<KeywordToken>,
    pub let_keyword:     KeywordToken,
}

impl SyntaxTree for LetBindingSyntaxTree {
    fn span(&self) -> Span {
        match &self.mutable_keyword {
            Some(keyword) => Span::new(keyword.span.start, self.let_keyword.span.end),
            None => self.let_keyword.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum VariableTypeBindingSyntaxTree {
    TypeBinding(TypeBindingSyntaxTree),
    LetBinding(LetBindingSyntaxTree),
}

impl SyntaxTree for VariableTypeBindingSyntaxTree {
    fn span(&self) -> Span {
        match self {
            VariableTypeBindingSyntaxTree::TypeBinding(binding) => binding.span(),
            VariableTypeBindingSyntaxTree::LetBinding(binding) => binding.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclarationStatementSyntaxTree {
    pub variable_type_binding: VariableTypeBindingSyntaxTree,
    pub identifier:            IdentifierToken,
    pub equals:                PunctuationToken,
    pub expression:            ExpressionSyntaxTree,
    pub semicolon:             PunctuationToken,
}

impl SyntaxTree for VariableDeclarationStatementSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.variable_type_binding.span().start,
            self.semicolon.span.end,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ExpressionStatementSyntaxTree {
    FunctionalExpresion(FunctionalExpressionStatementSyntaxTree),
    ImperativeExpression(ImperativeExpressionSyntaxTree),
}

impl SyntaxTree for ExpressionStatementSyntaxTree {
    fn span(&self) -> Span {
        match self {
            ExpressionStatementSyntaxTree::FunctionalExpresion(expression) => expression.span(),
            ExpressionStatementSyntaxTree::ImperativeExpression(expression) => expression.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionalExpressionStatementSyntaxTree {
    pub expression: FunctionalExpressionSyntaxTree,
    pub semicolon:  PunctuationToken,
}

impl SyntaxTree for FunctionalExpressionStatementSyntaxTree {
    fn span(&self) -> Span { Span::new(self.expression.span().start, self.semicolon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ControlStatementSyntaxTree {
    Break(BreakStatementSyntaxTree),
    Continue(ContinueStatementSyntaxTree),
    Express(ExpressStatementSyntaxTree),
    Return(ReturnStatementSyntaxTree),
}

impl SyntaxTree for ControlStatementSyntaxTree {
    fn span(&self) -> Span {
        match self {
            ControlStatementSyntaxTree::Break(statement) => statement.span(),
            ControlStatementSyntaxTree::Continue(statement) => statement.span(),
            ControlStatementSyntaxTree::Express(statement) => statement.span(),
            ControlStatementSyntaxTree::Return(statement) => statement.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BreakStatementSyntaxTree {
    pub break_keyword: PunctuationToken,
    pub label:         Option<LabelSyntaxTree>,
    pub expression:    Option<ExpressionSyntaxTree>,
}

impl SyntaxTree for BreakStatementSyntaxTree {
    fn span(&self) -> Span {
        match &self.expression {
            Some(expression) => Span::new(self.break_keyword.span.start, expression.span().end),
            None => match &self.label {
                Some(label) => Span::new(self.break_keyword.span.start, label.span().end),
                None => self.break_keyword.span,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ContinueStatementSyntaxTree {
    pub continue_keyword: PunctuationToken,
    pub label:            Option<LabelSyntaxTree>,
}

impl SyntaxTree for ContinueStatementSyntaxTree {
    fn span(&self) -> Span {
        match &self.label {
            Some(label) => Span::new(self.continue_keyword.span.start, label.span().end),
            None => self.continue_keyword.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressStatementSyntaxTree {
    pub express_keyword: KeywordToken,
    pub expression:      ExpressionSyntaxTree,
}

impl SyntaxTree for ExpressStatementSyntaxTree {
    fn span(&self) -> Span {
        Span::new(self.express_keyword.span.start, self.expression.span().end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnStatementSyntaxTree {
    pub return_keyword: KeywordToken,
    pub expression:     Option<ExpressionSyntaxTree>,
}

impl SyntaxTree for ReturnStatementSyntaxTree {
    fn span(&self) -> Span {
        match &self.expression {
            Some(expression) => Span::new(self.return_keyword.span.start, expression.span().end),
            None => self.return_keyword.span,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Option<StatementSyntaxTree> { todo!() }
}
