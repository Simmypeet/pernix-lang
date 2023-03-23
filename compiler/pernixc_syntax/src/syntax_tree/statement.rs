use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::{
    token::{IdentifierToken, Keyword, KeywordToken, PunctuationToken, Token},
    token_stream::TokenStreamCursor,
};

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

                // count the number of significant tokens that are eaten by each parser
                let count_fn = |original_cursor: &mut TokenStreamCursor,
                                comparing_cursor: &TokenStreamCursor|
                 -> usize {
                    let mut significant_token_eaten = 0;
                    let starting_cursor_position = original_cursor.position();
                    while original_cursor.position() < comparing_cursor.position() {
                        if let Some(token) = original_cursor.next_token() {
                            if !matches!(token, Token::WhiteSpace(_) | Token::Comment(_)) {
                                significant_token_eaten += 1;
                            }
                        }
                    }
                    original_cursor.set_position(starting_cursor_position);
                    significant_token_eaten
                };

                let variable_declaration_statement =
                    variable_declaration_statement_fn(&mut variable_parser);
                let expression_statement = expression_statement_fn(&mut expression_parser);

                let variable_declaration_statement_significant_token_eaten =
                    count_fn(&mut self.cursor, &variable_parser.cursor);
                let expression_statement_significant_token_eaten =
                    count_fn(&mut self.cursor, &expression_parser.cursor);

                if variable_declaration_statement_significant_token_eaten
                    > expression_statement_significant_token_eaten
                {
                    self.cursor = variable_parser.cursor;
                    self.errors.append(&mut variable_parser.errors);
                    variable_declaration_statement
                } else {
                    self.cursor = expression_parser.cursor;
                    self.errors.append(&mut expression_parser.errors);
                    expression_statement
                }
            }
            // Handles variable declaration statements
            Some(Token::Keyword(primitive_type))
                if matches!(
                    primitive_type.keyword,
                    Keyword::Void
                        | Keyword::Float32
                        | Keyword::Float64
                        | Keyword::Int8
                        | Keyword::Int16
                        | Keyword::Int32
                        | Keyword::Int64
                        | Keyword::Uint8
                        | Keyword::Uint16
                        | Keyword::Uint32
                        | Keyword::Uint64
                        | Keyword::Bool
                ) =>
            {
                // parse variable declaration statement
                let type_specifier = self
                    .parse_type_specifier()
                    .expect("should be a valid primitive type of something.");

                let identifier = self.expect_identifier()?;
                let equals = self.expect_punctuation('=')?;
                let expression = self.parse_expression()?;
                let semicolon = self.expect_punctuation(';')?;

                Some(StatementSyntaxTree::Declaration(
                    DeclarationStatementSyntaxTree::VariableDeclaration(
                        VariableDeclarationStatementSyntaxTree {
                            variable_type_binding: VariableTypeBindingSyntaxTree::TypeBinding(
                                TypeBindingSyntaxTree {
                                    mutable_keyword: None,
                                    type_specifier,
                                },
                            ),
                            identifier: identifier.clone(),
                            equals: equals.clone(),
                            expression,
                            semicolon: semicolon.clone(),
                        },
                    ),
                ))
            }
            _ => {
                let expression = self.parse_expression()?;

                match expression {
                    ExpressionSyntaxTree::FunctionalExpression(expression) => {
                        // expect semi-colon
                        let semicolon = self.expect_punctuation(';')?;

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
            }
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

#[cfg(test)]
mod tests;
