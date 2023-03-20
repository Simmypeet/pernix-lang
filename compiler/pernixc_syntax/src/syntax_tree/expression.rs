use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{
    IdentifierToken, Keyword, KeywordToken, NumericLiteralToken, PunctuationToken, Token,
};

use super::{
    statement::StatementSyntaxTree, ConnectedList, LabelSyntaxTree, QualifiedIdentifierSyntaxTree,
    SyntaxTree,
};
use crate::{
    errors::{PunctuationExpected, SyntacticError},
    parser::Parser,
};

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

/// Is a list of expressions separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// ArgumentListSyntaxTree:
///     ExpressionSyntaxTree (',' ExpressionSyntaxTree)*
///     ;
/// ```
pub type ArgumentListSyntaxTree = ConnectedList<Box<ExpressionSyntaxTree>, PunctuationToken>;

/// Is an expression that yields a value by calling a function.
///
/// Syntax Synopsis:
/// ``` text
/// FunctionCallExpressionSyntaxTree:
///     IdentifierExpressionSyntaxTree '(' ArgumentListSyntaxTree ')'
///     ;
/// ```
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

/// Represents an expression that is surrounded by parentheses.
///
/// Syntax Synopsis:
/// ``` text
/// ParenthesizedExpressionSyntaxTree:
///     '(' ExpressionSyntaxTree ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParenthesizedExpressionSyntaxTree {
    pub left_paren:  PunctuationToken,
    pub expression:  Box<ExpressionSyntaxTree>,
    pub right_paren: PunctuationToken,
}

impl SyntaxTree for ParenthesizedExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.left_paren.span.start, self.right_paren.span.end) }
}

/// Is a syntax tree node that represents a field initialization in a struct literal.
///
/// Syntax Synopsis:
/// ``` text
/// FieldInitializeSyntaxTree:
///     IdentifierExpressionSyntaxTree ':' ExpressionSyntaxTree
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldInitializeSyntaxTree {
    pub identifier: IdentifierExpressionSyntaxTree,
    pub colon:      PunctuationToken,
    pub expression: Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for FieldInitializeSyntaxTree {
    fn span(&self) -> Span { Span::new(self.identifier.span().start, self.expression.span().end) }
}

/// Is a list of field initializations separated by commas.
///
/// Syntax Synopsis:
/// ``` text
/// FieldInitializeListSyntaxTree:
///     FieldInitializeSyntaxTree (',' FieldInitializeSyntaxTree)*
///    ;
/// ```
pub type FieldInitializeListSyntaxTree = ConnectedList<FieldInitializeSyntaxTree, PunctuationToken>;

/// Is an expression that yields a value by initializing a struct.
///
/// Syntax Synopsis:
/// ``` text
/// StructLiteralSyntaxTree:
///     IdentifierExpressionSyntaxTree '{' FieldInitializeListSyntaxTree '}'
///     ;
/// ```
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

/// Represents an expression that yields a value by accessing a field of a struct.
///
/// Syntax Synopsis:
/// ``` text
/// MemberAccessExpressionSyntaxTree:
///     ExpressionSyntaxTree '.' IdentifierExpressionSyntaxTree
///     ;
/// ```
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
pub struct LabelSpecifierSyntaxTree {
    pub label: LabelSyntaxTree,
    pub colon: PunctuationToken,
}

impl SyntaxTree for LabelSpecifierSyntaxTree {
    fn span(&self) -> Span { Span::new(self.label.single_quote.span.start, self.colon.span.end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockExpressionSyntaxTree {
    pub label_specifier: Option<LabelSpecifierSyntaxTree>,
    pub left_brace:      PunctuationToken,
    pub statements:      Vec<StatementSyntaxTree>,
    pub right_brace:     PunctuationToken,
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
    pub label_specifier: Option<LabelSpecifierSyntaxTree>,
    pub loop_keyword:    KeywordToken,
    pub expression:      Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for LoopExpressionSyntaxTree {
    fn span(&self) -> Span {
        let start = self
            .label_specifier
            .as_ref()
            .map(|label| label.span().start)
            .unwrap_or(self.loop_keyword.span.start);
        Span::new(start, self.expression.span().end)
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`ExpressionSyntaxTree`]
    pub fn parse_expression(&mut self) -> Option<ExpressionSyntaxTree> { todo!() }

    fn parse_block_or_loop_expression(
        &mut self,
        label_specifier: Option<LabelSpecifierSyntaxTree>,
    ) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            Some(Token::Keyword(loop_keyword)) if loop_keyword.keyword == Keyword::Loop => {
                // eat the loop keyword
                self.next_token();

                // parse the loop expression
                let expression = self.parse_expression()?;

                Some(ExpressionSyntaxTree::ImperativeExpression(
                    ImperativeExpressionSyntaxTree::LoopExpression(LoopExpressionSyntaxTree {
                        label_specifier: None,
                        loop_keyword:    loop_keyword.clone(),
                        expression:      Box::new(expression),
                    }),
                ))
            }
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => {
                // eat the left brace
                self.next_token();

                // parse the statements until we reach the right brace or EOF
                let mut statements = Vec::new();
                let right_brace = loop {
                    match self.peek_significant_token() {
                        // found closing brace
                        Some(Token::Punctuation(punc)) if punc.punctuation == '}' => break punc,

                        // unexpected eof
                        None => {
                            self.report_error(SyntacticError::PunctuationExpected(
                                PunctuationExpected {
                                    expected: '}',
                                    found:    None,
                                },
                            ));
                            return None;
                        }

                        // parse the statement
                        _ => {
                            let statement = self.parse_statement();

                            if let Some(statement) = statement {
                                statements.push(statement);
                            } else {
                                // skip to either the next semi-colon or the next right brace
                                let token = self.next_token_until(|token| {
                                    matches!(
                                        token,
                                        Token::Punctuation(punc)
                                            if punc.punctuation == ';' || punc.punctuation == '}'
                                    )
                                });

                                // handle the token
                                match token {
                                    Some(Token::Punctuation(punc)) if punc.punctuation == '}' => {
                                        break punc
                                    }
                                    None => {
                                        self.report_error(SyntacticError::PunctuationExpected(
                                            PunctuationExpected {
                                                expected: '}',
                                                found:    None,
                                            },
                                        ));
                                        return None;
                                    }
                                    _ => (),
                                }
                            }
                        }
                    }
                };

                Some(ExpressionSyntaxTree::ImperativeExpression(
                    ImperativeExpressionSyntaxTree::BlockExpression(BlockExpressionSyntaxTree {
                        label_specifier,
                        left_brace: left_brace.clone(),
                        statements,
                        right_brace: right_brace.clone(),
                    }),
                ))
            }
            token => {
                self.report_error(SyntacticError::ExpressionExpected(token.cloned()));
                None
            }
        }
    }

    /// Parses a primary expression without prefix operators
    fn parse_primary_expression_raw(&mut self) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            Some(Token::Keyword(if_keyword)) if if_keyword.keyword == Keyword::If => {
                // eat the if keyword
                self.next_token();

                // parse condition
                let left_paren = self.expect_punctuation('(')?;
                let condition = self.parse_expression()?;
                let right_paren = self.expect_punctuation(')')?;

                // parse then expression
                let them_expression = self.parse_expression()?;

                // parse else expression
                let else_expression = match self.peek_significant_token() {
                    Some(Token::Keyword(else_keyword)) if else_keyword.keyword == Keyword::Else => {
                        // eat the else keyword
                        self.next_token();

                        // parse the else expression
                        let else_expression = self.parse_expression()?;

                        Some(ElseExpressionSyntaxTree {
                            else_keyword: else_keyword.clone(),
                            expression:   Box::new(else_expression),
                        })
                    }
                    _ => None,
                };

                Some(ExpressionSyntaxTree::ImperativeExpression(
                    ImperativeExpressionSyntaxTree::IfElseExpression(IfElseExpressionSyntaxTree {
                        if_keyword: if_keyword.clone(),
                        left_paren: left_paren.clone(),
                        condition: Box::new(condition),
                        right_paren: right_paren.clone(),
                        then_expression: Box::new(them_expression),
                        else_expression,
                    }),
                ))
            }

            Some(Token::NumericLiteral(numeric_literal)) => {
                // eat the numeric literal
                self.next_token();

                Some(ExpressionSyntaxTree::FunctionalExpression(
                    FunctionalExpressionSyntaxTree::NumericLiteral(NumericLiteralSyntaxTree(
                        numeric_literal.clone(),
                    )),
                ))
            }

            Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                // eat the left parenthesis
                self.next_token();

                // parse the expression
                let expression = self.parse_expression()?;

                // expect a right parenthesis
                let right_paren = self.expect_punctuation(')')?;

                Some(ExpressionSyntaxTree::FunctionalExpression(
                    FunctionalExpressionSyntaxTree::ParenthesizedExpression(
                        ParenthesizedExpressionSyntaxTree {
                            left_paren:  left_paren.clone(),
                            expression:  Box::new(expression),
                            right_paren: right_paren.clone(),
                        },
                    ),
                ))
            }

            Some(Token::Punctuation(single_quote)) if single_quote.punctuation == '\'' => {
                // eat the single quote
                self.next_token();

                // expect an identifier
                let name = self.expect_identifier()?;

                // expect a colon
                let colon = self.expect_punctuation(':')?;

                let label = LabelSpecifierSyntaxTree {
                    label: LabelSyntaxTree {
                        single_quote: single_quote.clone(),
                        name:         name.clone(),
                    },
                    colon: colon.clone(),
                };

                // parse the block or loop expression
                self.parse_block_or_loop_expression(Some(label))
            }

            _ => self.parse_block_or_loop_expression(None),
        }
    }

    fn parse_primary_expression(&mut self) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(punc))
                if punc.punctuation == '!' || punc.punctuation == '-' =>
            {
                // eat the punctuation
                self.next_token();

                // parse the operand
                let operand = self.parse_primary_expression()?;

                // return the unary expression syntax tree
                return Some(ExpressionSyntaxTree::FunctionalExpression(
                    FunctionalExpressionSyntaxTree::PrefixExpression(PrefixExpressionSyntaxTree {
                        operator: match punc.punctuation {
                            '!' => PerfixOperatorSyntaxTree::LogicalNot(punc.clone()),
                            '-' => PerfixOperatorSyntaxTree::Negate(punc.clone()),
                            _ => unreachable!(),
                        },
                        operand:  Box::new(operand),
                    }),
                ));
            }
            _ => (),
        }

        let mut primary_expression = self.parse_primary_expression_raw()?;

        loop {
            match self.peek_significant_token() {
                Some(Token::Punctuation(dot)) if dot.punctuation == '.' => {
                    // eat the dot
                    self.next_token();

                    // expect an identifier
                    let identifier = self.expect_identifier()?.clone();

                    // create the member access expression
                    primary_expression = ExpressionSyntaxTree::FunctionalExpression(
                        FunctionalExpressionSyntaxTree::MemberAccessExpression(
                            MemberAccessExpressionSyntaxTree {
                                expression: Box::new(primary_expression),
                                dot: dot.clone(),
                                identifier,
                            },
                        ),
                    );
                }
                _ => break Some(primary_expression),
            }
        }
    }
}
