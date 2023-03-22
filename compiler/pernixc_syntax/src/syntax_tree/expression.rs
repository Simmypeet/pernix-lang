use std::cmp::Ordering;

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NumericLiteralSyntaxTree(pub NumericLiteralToken);

impl SyntaxTree for NumericLiteralSyntaxTree {
    fn span(&self) -> Span { self.0.span }
}

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

impl BinaryOperatorSyntaxTree {
    /// Returns `true` if the operator is assignment (including compound assignment)
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            Self::Assign(_)
                | Self::CompoundAdd(_, _)
                | Self::CompoundSubtract(_, _)
                | Self::CompoundMultiply(_, _)
                | Self::CompoundDivide(_, _)
                | Self::CompoundModulo(_, _)
        )
    }

    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    pub fn get_precedence(&self) -> u32 {
        match self {
            Self::Assign(_)
            | Self::CompoundAdd(_, _)
            | Self::CompoundSubtract(_, _)
            | Self::CompoundMultiply(_, _)
            | Self::CompoundDivide(_, _)
            | Self::CompoundModulo(_, _) => 1,
            Self::LogicalOr(_) => 2,
            Self::LogicalAnd(_) => 3,
            Self::Equal(_, _) | Self::NotEqual(_, _) => 4,
            Self::LessThan(_)
            | Self::LessThanOrEqual(_, _)
            | Self::GreaterThan(_)
            | Self::GreaterThanOrEqual(_, _) => 5,
            Self::Add(_) | Self::Subtract(_) => 6,
            Self::Multiply(_) | Self::Divide(_) | Self::Modulo(_) => 7,
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryExpressionSyntaxTree {
    pub left:     Box<ExpressionSyntaxTree>,
    pub operator: BinaryOperatorSyntaxTree,
    pub right:    Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for BinaryExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.left.span().start, self.right.span().end) }
}

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrefixExpressionSyntaxTree {
    pub operator: PerfixOperatorSyntaxTree,
    pub operand:  Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for PrefixExpressionSyntaxTree {
    fn span(&self) -> Span { Span::new(self.operator.span().start, self.operand.span().end) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentifierExpressionSyntaxTree(pub QualifiedIdentifierSyntaxTree);

impl SyntaxTree for IdentifierExpressionSyntaxTree {
    fn span(&self) -> Span { self.0.span() }
}

pub type ArgumentListSyntaxTree = ConnectedList<Box<ExpressionSyntaxTree>, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCallExpressionSyntaxTree {
    pub qualified_identifier: QualifiedIdentifierSyntaxTree,
    pub left_paren:           PunctuationToken,
    pub arguments:            Option<ArgumentListSyntaxTree>,
    pub right_paren:          PunctuationToken,
}

impl SyntaxTree for FunctionCallExpressionSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.qualified_identifier.span().start,
            self.right_paren.span.end,
        )
    }
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
    pub identifier: IdentifierToken,
    pub colon:      PunctuationToken,
    pub expression: Box<ExpressionSyntaxTree>,
}

impl SyntaxTree for FieldInitializeSyntaxTree {
    fn span(&self) -> Span { Span::new(self.identifier.span().start, self.expression.span().end) }
}

pub type FieldInitializeListSyntaxTree = ConnectedList<FieldInitializeSyntaxTree, PunctuationToken>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructLiteralSyntaxTree {
    pub qualified_identifier:  QualifiedIdentifierSyntaxTree,
    pub left_brace:            PunctuationToken,
    pub field_initializations: Option<FieldInitializeListSyntaxTree>,
    pub right_brace:           PunctuationToken,
}

impl SyntaxTree for StructLiteralSyntaxTree {
    fn span(&self) -> Span {
        Span::new(
            self.qualified_identifier.span().start,
            self.right_brace.span.end,
        )
    }
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
    /// Parses an [ExpressionSyntaxTree].
    pub fn parse_expression(&mut self) -> Option<ExpressionSyntaxTree> {
        // Gets the first primary expression
        let mut first_expression = self.parse_primary_expression()?;

        let mut expressions = Vec::new();

        // Parses a list of binary operators and expressions
        while let Some(binary_operator) = self.try_parse_binary_operator() {
            expressions.push((binary_operator, Some(self.parse_primary_expression()?)));
        }

        // We have to fold the expressions based on the precedence of the binary operators and the
        // associativity of the binary operators.

        // This a vector of indices of the expressions that are candidates for folding.
        let mut candidate_index = 0;
        let mut current_precedence;

        while !expressions.is_empty() {
            // Reset the current precedence and the candidate indices
            current_precedence = 0;

            for (index, (binary_operator, _)) in expressions.iter().enumerate() {
                let new_precedence = binary_operator.get_precedence();
                match new_precedence.cmp(&current_precedence) {
                    // Push the index of the binary operator to the candidate indices
                    Ordering::Equal => {
                        if binary_operator.is_assignment() {
                            candidate_index = index;
                        }
                    }

                    // Clear the candidate indices and set the current precedence to the
                    // precedence of the current binary operator.
                    Ordering::Greater => {
                        current_precedence = new_precedence;
                        candidate_index = index;
                    }

                    _ => (),
                }
            }

            // ASSUMPTION: The assignments have 1 precedence and are right associative.
            assert!(current_precedence > 0);

            if candidate_index == 0 {
                let (binary_operator, right_expression) = expressions.remove(0);

                // Replace the first expression with the folded expression.
                first_expression = ExpressionSyntaxTree::FunctionalExpression(
                    FunctionalExpressionSyntaxTree::BinaryExpression(BinaryExpressionSyntaxTree {
                        left:     Box::new(first_expression),
                        operator: binary_operator,
                        right:    Box::new(right_expression.unwrap()),
                    }),
                )
            } else {
                let (binary_operator, right_expression) = expressions.remove(candidate_index);

                // Replace the expression at the index with the folded expression.
                expressions[candidate_index - 1].1 =
                    Some(ExpressionSyntaxTree::FunctionalExpression(
                        FunctionalExpressionSyntaxTree::BinaryExpression(
                            BinaryExpressionSyntaxTree {
                                left:     Box::new(
                                    expressions[candidate_index - 1].1.take().unwrap(),
                                ),
                                operator: binary_operator,
                                right:    Box::new(right_expression.unwrap()),
                            },
                        ),
                    ))
            }
        }

        Some(first_expression)
    }

    fn try_parse_first_punctuation_binary_operator(&mut self) -> Option<BinaryOperatorSyntaxTree> {
        let starting_cursor_position = self.cursor.position();
        let next_token = self.next_significant_token();
        match next_token {
            Some(Token::Punctuation(punctuation)) => match punctuation.punctuation {
                '+' => return Some(BinaryOperatorSyntaxTree::Add(punctuation.clone())),
                '-' => return Some(BinaryOperatorSyntaxTree::Subtract(punctuation.clone())),
                '*' => return Some(BinaryOperatorSyntaxTree::Multiply(punctuation.clone())),
                '/' => return Some(BinaryOperatorSyntaxTree::Divide(punctuation.clone())),
                '%' => return Some(BinaryOperatorSyntaxTree::Modulo(punctuation.clone())),
                '=' => return Some(BinaryOperatorSyntaxTree::Assign(punctuation.clone())),
                '!' => {
                    if let Some(Token::Punctuation(punctuation1)) = self.peek_significant_token() {
                        if punctuation1.punctuation == '=' {
                            self.next_significant_token();
                            return Some(BinaryOperatorSyntaxTree::NotEqual(
                                punctuation.clone(),
                                punctuation1.clone(),
                            ));
                        }
                    }
                }
                '<' => return Some(BinaryOperatorSyntaxTree::LessThan(punctuation.clone())),
                '>' => return Some(BinaryOperatorSyntaxTree::GreaterThan(punctuation.clone())),
                _ => (),
            },
            Some(Token::Keyword(and_keyword)) if and_keyword.keyword == Keyword::And => {
                return Some(BinaryOperatorSyntaxTree::LogicalAnd(and_keyword.clone()))
            }
            Some(Token::Keyword(or_keyword)) if or_keyword.keyword == Keyword::Or => {
                return Some(BinaryOperatorSyntaxTree::LogicalOr(or_keyword.clone()))
            }
            _ => (),
        }
        self.cursor.set_position(starting_cursor_position);
        None
    }

    fn try_parse_second_punctuation_binary_operator(
        &mut self,
        first_punctuation_binary_operator: BinaryOperatorSyntaxTree,
    ) -> BinaryOperatorSyntaxTree {
        let starting_cursor_position = self.cursor.position();

        match self.next_significant_token() {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation == '=' => {
                match first_punctuation_binary_operator {
                    BinaryOperatorSyntaxTree::Add(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::CompoundAdd(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperatorSyntaxTree::Subtract(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::CompoundSubtract(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    BinaryOperatorSyntaxTree::Multiply(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::CompoundMultiply(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    BinaryOperatorSyntaxTree::Divide(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::CompoundDivide(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    BinaryOperatorSyntaxTree::Modulo(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::CompoundModulo(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    BinaryOperatorSyntaxTree::Assign(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::Equal(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperatorSyntaxTree::LessThan(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::LessThanOrEqual(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    BinaryOperatorSyntaxTree::GreaterThan(prev_punctuation) => {
                        BinaryOperatorSyntaxTree::GreaterThanOrEqual(
                            prev_punctuation,
                            punctuation.clone(),
                        )
                    }
                    _ => {
                        self.cursor.set_position(starting_cursor_position);
                        first_punctuation_binary_operator
                    }
                }
            }
            _ => {
                self.cursor.set_position(starting_cursor_position);
                first_punctuation_binary_operator
            }
        }
    }

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperatorSyntaxTree> {
        let first_punctuation_binary_operator =
            self.try_parse_first_punctuation_binary_operator()?;
        Some(self.try_parse_second_punctuation_binary_operator(first_punctuation_binary_operator))
    }

    // Parses either a block expression or a loop expression.
    fn parse_block_or_loop_expression(
        &mut self,
        label_specifier: Option<LabelSpecifierSyntaxTree>,
    ) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            // Handles loop
            Some(Token::Keyword(loop_keyword)) if loop_keyword.keyword == Keyword::Loop => {
                self.next_token();

                let expression = self.parse_expression()?;

                Some(ExpressionSyntaxTree::ImperativeExpression(
                    ImperativeExpressionSyntaxTree::LoopExpression(LoopExpressionSyntaxTree {
                        label_specifier: None,
                        loop_keyword:    loop_keyword.clone(),
                        expression:      Box::new(expression),
                    }),
                ))
            }
            // Handles block
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => {
                self.next_token();

                let mut statements = Vec::new();

                // Parses statements until a right brace is found.
                let right_brace = loop {
                    match self.peek_significant_token() {
                        Some(Token::Punctuation(punc)) if punc.punctuation == '}' => break punc,

                        None => {
                            self.report_error(SyntacticError::PunctuationExpected(
                                PunctuationExpected {
                                    expected: '}',
                                    found:    None,
                                },
                            ));
                            return None;
                        }

                        _ => {
                            let statement = self.parse_statement();

                            if let Some(statement) = statement {
                                statements.push(statement);
                            } else {
                                // Skips to either the next semicolon or the next right brace.
                                let token = self.next_token_until(|token| {
                                    matches!(
                                        token,
                                        Token::Punctuation(punc)
                                            if punc.punctuation == ';' || punc.punctuation == '}'
                                    )
                                });

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

    // Parses an primary expression without any prefix operators.
    fn parse_primary_expression_raw(&mut self) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            // Handles if expressions
            Some(Token::Keyword(if_keyword)) if if_keyword.keyword == Keyword::If => {
                self.next_token();

                let left_paren = self.expect_punctuation('(')?;
                let condition = self.parse_expression()?;
                let right_paren = self.expect_punctuation(')')?;

                let them_expression = self.parse_expression()?;

                // Parses an else expression if it exists.
                let else_expression = match self.peek_significant_token() {
                    Some(Token::Keyword(else_keyword)) if else_keyword.keyword == Keyword::Else => {
                        self.next_token();

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

            // Handles numeric literal
            Some(Token::NumericLiteral(numeric_literal)) => {
                self.next_token();

                Some(ExpressionSyntaxTree::FunctionalExpression(
                    FunctionalExpressionSyntaxTree::NumericLiteral(NumericLiteralSyntaxTree(
                        numeric_literal.clone(),
                    )),
                ))
            }

            // Handles parenthesis
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                self.next_token();

                let expression = self.parse_expression()?;

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

            // Handles label specifier
            Some(Token::Punctuation(single_quote)) if single_quote.punctuation == '\'' => {
                self.next_token();

                let name = self.expect_identifier()?;

                let colon = self.expect_punctuation(':')?;

                let label = LabelSpecifierSyntaxTree {
                    label: LabelSyntaxTree {
                        single_quote: single_quote.clone(),
                        name:         name.clone(),
                    },
                    colon: colon.clone(),
                };

                self.parse_block_or_loop_expression(Some(label))
            }

            // Handles identifier
            Some(Token::Identifier(_)) => {
                let qualified_identifier = self.parse_qualified_identifier()?;

                match self.peek_significant_token() {
                    // Function call
                    Some(Token::Punctuation(left_paren)) if left_paren.punctuation == '(' => {
                        // eat the left parenthesis
                        self.next_token();

                        let (arguments, right_paren) =
                            self.parse_enclosed_list(')', ',', |this| {
                                this.parse_expression().map(Box::new)
                            })?;

                        Some(ExpressionSyntaxTree::FunctionalExpression(
                            FunctionalExpressionSyntaxTree::FunctionCallExpression(
                                FunctionCallExpressionSyntaxTree {
                                    qualified_identifier,
                                    left_paren: left_paren.clone(),
                                    arguments,
                                    right_paren,
                                },
                            ),
                        ))
                    }

                    // Struct literal
                    Some(Token::Punctuation(left_brace)) if left_brace.punctuation == '{' => {
                        // eat the left brace
                        self.next_token();
                        
                        let (field_initializations, right_brace) =
                            self.parse_enclosed_list('}', ',', |this| {
                                let identifier = this.expect_identifier()?;

                                let colon = this.expect_punctuation(':')?;

                                let expression = this.parse_expression()?;

                                Some(FieldInitializeSyntaxTree {
                                    identifier: identifier.clone(),
                                    colon:      colon.clone(),
                                    expression: Box::new(expression),
                                })
                            })?;

                        Some(ExpressionSyntaxTree::FunctionalExpression(
                            FunctionalExpressionSyntaxTree::StructLiteralSyntaxTree(
                                StructLiteralSyntaxTree {
                                    qualified_identifier,
                                    left_brace: left_brace.clone(),
                                    field_initializations,
                                    right_brace,
                                },
                            ),
                        ))
                    }

                    // Simple identifier expression
                    _ => Some(ExpressionSyntaxTree::FunctionalExpression(
                        FunctionalExpressionSyntaxTree::IdentifierExpression(
                            IdentifierExpressionSyntaxTree(qualified_identifier),
                        ),
                    )),
                }
            }

            _ => self.parse_block_or_loop_expression(None),
        }
    }

    // Parses a primary expression with prefix operators and postfix operators.
    fn parse_primary_expression(&mut self) -> Option<ExpressionSyntaxTree> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(punc))
                if punc.punctuation == '!' || punc.punctuation == '-' =>
            {
                self.next_token();

                let operand = self.parse_primary_expression()?;

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
                    self.next_token();

                    let identifier = self.expect_identifier()?.clone();

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

#[cfg(test)]
mod tests;
