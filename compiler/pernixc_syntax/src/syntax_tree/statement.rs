use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{IdentifierToken, Keyword, KeywordToken, PunctuationToken, Token};

use super::{
    expression::{
        ExpressionSyntaxTree, FunctionalExpressionSyntaxTree, ImperativeExpressionSyntaxTree,
    },
    SyntaxTree, TypeBindingSyntaxTree,
};
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum StatementSyntaxTree {
    Declaration(DeclarationStatementSyntaxTree),
    Expression(ExpressionStatementSyntaxTree),
}

impl SyntaxTree for StatementSyntaxTree {
    fn span(&self) -> Span {
        match self {
            StatementSyntaxTree::Declaration(declaration) => declaration.span(),
            StatementSyntaxTree::Expression(expression) => expression.span(),
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

impl<'a> Parser<'a> {
    /// Parses a [StatementSyntaxTree]
    pub fn parse_statement(&mut self) -> Option<StatementSyntaxTree> {
        match self.peek_significant_token() {
            // Handles variable declaration statements
            Some(Token::Keyword(mutable_keyword))
                if mutable_keyword.keyword == Keyword::Mutable =>
            {
                // eat mutable keyword
                self.next_token();

                match self.peek_significant_token() {
                    Some(Token::Keyword(let_keyword)) if let_keyword.keyword == Keyword::Let => {
                        // eat let keyword
                        self.next_token();

                        self.parse_variable_declaration_statement(
                            VariableTypeBindingSyntaxTree::LetBinding(LetBindingSyntaxTree {
                                mutable_keyword: Some(mutable_keyword.clone()),
                                let_keyword:     let_keyword.clone(),
                            }),
                        )
                    }
                    _ => {
                        let type_specifier = self.parse_type_specifier()?;
                        self.parse_variable_declaration_statement(
                            VariableTypeBindingSyntaxTree::TypeBinding(TypeBindingSyntaxTree {
                                mutable_keyword: Some(mutable_keyword.clone()),
                                type_specifier,
                            }),
                        )
                    }
                }
            }

            // Handles variable declaration statements
            Some(Token::Keyword(let_keyword)) if let_keyword.keyword == Keyword::Let => {
                // eat let keyword
                self.next_token();

                self.parse_variable_declaration_statement(
                    VariableTypeBindingSyntaxTree::LetBinding(LetBindingSyntaxTree {
                        mutable_keyword: None,
                        let_keyword:     let_keyword.clone(),
                    }),
                )
            }
            // Might be either a variable declaration statement or identifier expression statement
            Some(Token::Identifier(_)) => {
                // try to parse either a variable declaration statement or identifier expression
                // statement.
                //
                // the syntax tree that eats most tokens will be the one that is returned.

                let mut variable_parser = Parser {
                    cursor:         self.cursor,
                    produce_errors: self.produce_errors,
                    errors:         Vec::new(),
                };
                let mut expression_parser = Parser {
                    cursor:         self.cursor,
                    produce_errors: self.produce_errors,
                    errors:         Vec::new(),
                };

                

                let variable_declaration_statement_fn =
                    |this: &mut Self| -> Option<StatementSyntaxTree> {
                        let type_specifier = this.parse_type_specifier()?;
                        this.parse_variable_declaration_statement(
                            VariableTypeBindingSyntaxTree::TypeBinding(TypeBindingSyntaxTree {
                                mutable_keyword: None,
                                type_specifier,
                            }),
                        )
                    };
                let expression_statement_fn = |this: &mut Self| -> Option<StatementSyntaxTree> {
                    let expression = this.parse_expression()?;
                    match expression {
                        ExpressionSyntaxTree::FunctionalExpression(expression) => {
                            // expect semi-colon
                            let semicolon = this.expect_punctuation(';')?;

                            Some(StatementSyntaxTree::Expression(
                                ExpressionStatementSyntaxTree::FunctionalExpresion(
                                    FunctionalExpressionStatementSyntaxTree {
                                        expression,
                                        semicolon: semicolon.clone(),
                                    },
                                ),
                            ))
                        }
                        ExpressionSyntaxTree::ImperativeExpression(expression) => {
                            Some(StatementSyntaxTree::Expression(
                                ExpressionStatementSyntaxTree::ImperativeExpression(expression),
                            ))
                        }
                    }
                };



                todo!()
            }
            _ => todo!(),
        }
    }

    fn parse_variable_declaration_statement(
        &mut self,
        variable_type_binding: VariableTypeBindingSyntaxTree,
    ) -> Option<StatementSyntaxTree> {
        let identifier = self.expect_identifier()?;
        let equals = self.expect_punctuation('=')?;
        let expression = self.parse_expression()?;
        let semicolon = self.expect_punctuation(';')?;

        Some(StatementSyntaxTree::Declaration(
            DeclarationStatementSyntaxTree::VariableDeclaration(
                VariableDeclarationStatementSyntaxTree {
                    variable_type_binding,
                    identifier: identifier.clone(),
                    equals: equals.clone(),
                    expression,
                    semicolon: semicolon.clone(),
                },
            ),
        ))
    }
}
