//! Contains the definitions of expression syntax tree.

use std::cmp::Ordering;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{
        Identifier, Keyword, KeywordKind, NumericLiteral as NumericLiteralToken, Punctuation, Token,
    },
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    statement::Statement, ConnectedList, EnclosedList, Label, QualifiedIdentifier, TypeSpecifier,
};
use crate::{
    error::{Error, ExpressionExpected},
    parser::{Error as ParserError, Parser, Result as ParserResult},
};

pub mod input;

/// Is an enumeration of all kinds of expressions.
///
/// ``` txt
/// Expression:
///     Terminator
///     | Functional
///     | Primary
///     ;
///  ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Expression {
    Functional(Functional),
    Terminator(Terminator),
    Imperative(Imperative),
}

impl SourceElement for Expression {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Functional(functional) => functional.span(),
            Self::Terminator(terminator) => terminator.span(),
            Self::Imperative(imperative) => imperative.span(),
        }
    }
}

/// Is an enumeration of all terminator expressions.
///
/// ``` txt
/// Terminator:
///     Return
///     | Continue
///     | Express
///     | Break
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl SourceElement for Terminator {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Return(return_) => return_.span(),
            Self::Continue(continue_) => continue_.span(),
            Self::Express(express) => express.span(),
            Self::Break(break_) => break_.span(),
        }
    }
}

/// Is an enumeration of all kinds of functional expressions.
///
/// Functional epxressions are expressions immediately evaluated to a value without introducing
/// control flow.
///
/// Syntax Synopsis:
/// ``` txt
/// Functional:
///     NumericLiteral
///     | BooleanLiteral
///     | BinaryList
///     | Prefix
///     | Named
///     | FunctionCall
///     | Parenthesized
///     | StructLiteral
///     | MemberAccess
///     | Cast
/// ```
#[derive(Debug, Clone, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Functional {
    NumericLiteral(NumericLiteral),
    BooleanLiteral(BooleanLiteral),
    Binary(Binary),
    Prefix(Prefix),
    Named(Named),
    FunctionCall(FunctionCall),
    Parenthesized(Parenthesized),
    StructLiteral(StructLiteral),
    MemberAccess(MemberAccess),
    Cast(Cast),
}

impl SourceElement for Functional {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::NumericLiteral(numeric_literal) => {
                Ok(numeric_literal.numeric_literal_token.span.clone())
            }
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::Binary(binary_expression) => binary_expression.span(),
            Self::Prefix(prefix_expression) => prefix_expression.span(),
            Self::Named(identifier_expression) => identifier_expression.span(),
            Self::FunctionCall(function_call_expression) => function_call_expression.span(),
            Self::Parenthesized(parenthesized_expression) => parenthesized_expression.span(),
            Self::StructLiteral(struct_literal) => struct_literal.span(),
            Self::MemberAccess(member_access_expression) => member_access_expression.span(),
            Self::Cast(cast) => cast.span(),
        }
    }
}

/// Represents a numeric literal syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// NumericLiteral:
///     NumericLiteralToken
///     ;
/// ````
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct NumericLiteral {
    pub numeric_literal_token: NumericLiteralToken,
}

impl SourceElement for NumericLiteral {
    fn span(&self) -> Result<Span, SpanError> { Ok(self.numeric_literal_token.span.clone()) }
}

/// Represents a cast expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// CastExpression:
///     Functional 'as' '(' TypeSpecifier ')'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Cast {
    pub operand: Box<Functional>,
    pub as_keyword: Keyword,
    pub left_paren: Punctuation,
    pub type_specifier: TypeSpecifier,
    pub right_paren: Punctuation,
}

impl SourceElement for Cast {
    fn span(&self) -> Result<Span, SpanError> { self.operand.span()?.join(&self.right_paren.span) }
}

/// Represents a boolean literal syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// BooleanLiteral:
///     'true'
///     | 'false'
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum BooleanLiteral {
    True(Keyword),
    False(Keyword),
}

impl SourceElement for BooleanLiteral {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::True(keyword) | Self::False(keyword) => Ok(keyword.span.clone()),
        }
    }
}

/// Represents a binary expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Binary:
///     Functional BinaryOperator Functional
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Binary {
    pub left_operand: Box<Functional>,
    pub operator: BinaryOperator,
    pub right_operand: Box<Functional>,
}

impl SourceElement for Binary {
    fn span(&self) -> Result<Span, SpanError> {
        self.left_operand.span()?.join(&self.right_operand.span()?)
    }
}

/// Represents a binary operator syntax tree
///
/// Syntax Synopsis:
/// ``` txt
/// BinaryOperator:
///     '+'
///     | '-'
///     | '*'
///     | '/'
///     | '%'
///     | '='
///     | '+='
///     | '-='
///     | '*='
///     | '/='
///     | '%='
///     | '=='
///     | '!='
///     | '<'
///     | '<='
///     | '>'
///     | '>='
///     | 'and'
///     | 'or'
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add(Punctuation),
    Subtract(Punctuation),
    Multiply(Punctuation),
    Divide(Punctuation),
    Modulo(Punctuation),
    Assign(Punctuation),
    CompoundAdd(Punctuation, Punctuation),
    CompoundSubtract(Punctuation, Punctuation),
    CompoundMultiply(Punctuation, Punctuation),
    CompoundDivide(Punctuation, Punctuation),
    CompoundModulo(Punctuation, Punctuation),
    Equal(Punctuation, Punctuation),
    NotEqual(Punctuation, Punctuation),
    LessThan(Punctuation),
    LessThanOrEqual(Punctuation, Punctuation),
    GreaterThan(Punctuation),
    GreaterThanOrEqual(Punctuation, Punctuation),
    LogicalAnd(Keyword),
    LogicalOr(Keyword),
}

impl BinaryOperator {
    /// Returns `true` if the operator is assignment (including compound assignment)
    #[must_use]
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            Self::Assign(..)
                | Self::CompoundAdd(..)
                | Self::CompoundSubtract(..)
                | Self::CompoundMultiply(..)
                | Self::CompoundDivide(..)
                | Self::CompoundModulo(..)
        )
    }

    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    #[must_use]
    pub fn get_precedence(&self) -> u32 {
        match self {
            Self::Assign(..)
            | Self::CompoundAdd(..)
            | Self::CompoundSubtract(..)
            | Self::CompoundMultiply(..)
            | Self::CompoundDivide(..)
            | Self::CompoundModulo(..) => 1,
            Self::LogicalOr(..) => 2,
            Self::LogicalAnd(..) => 3,
            Self::Equal(..) | Self::NotEqual(..) => 4,
            Self::LessThan(..)
            | Self::LessThanOrEqual(..)
            | Self::GreaterThan(..)
            | Self::GreaterThanOrEqual(..) => 5,
            Self::Add(..) | Self::Subtract(..) => 6,
            Self::Multiply(..) | Self::Divide(..) | Self::Modulo(..) => 7,
        }
    }
}

impl SourceElement for BinaryOperator {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Add(token)
            | Self::Subtract(token)
            | Self::Multiply(token)
            | Self::Divide(token)
            | Self::Modulo(token)
            | Self::Assign(token)
            | Self::LessThan(token)
            | Self::GreaterThan(token) => Ok(token.span.clone()),
            Self::CompoundAdd(token, token1)
            | Self::CompoundSubtract(token, token1)
            | Self::CompoundMultiply(token, token1)
            | Self::CompoundDivide(token, token1)
            | Self::CompoundModulo(token, token1)
            | Self::Equal(token, token1)
            | Self::NotEqual(token, token1)
            | Self::LessThanOrEqual(token, token1)
            | Self::GreaterThanOrEqual(token, token1) => token.span()?.join(&token1.span),
            Self::LogicalAnd(token) | Self::LogicalOr(token) => Ok(token.span.clone()),
        }
    }
}

/// Represents a prefix operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// PrefixOperator:
///     '!'
///     | '-'
///     | '&'
///     | '*'
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
    ReferenceOf(Punctuation),
    Dereference(Punctuation),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::LogicalNot(token)
            | Self::Negate(token)
            | Self::ReferenceOf(token)
            | Self::Dereference(token) => Ok(token.span.clone()),
        }
    }
}

/// Represents a prefix expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Prefix:
///     PrefixOperator Functional
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Prefix {
    pub operator: PrefixOperator,
    pub operand: Box<Functional>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Result<Span, SpanError> { self.operator.span()?.join(&self.operand.span()?) }
}

/// Represents a postfix operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Named:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Named {
    pub qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for Named {
    fn span(&self) -> Result<Span, SpanError> { self.qualified_identifier.span() }
}

/// Represents a list of expressions separated by commas.
///
/// Syntax Synopsis:
/// ``` txt
/// ArgumentList:
///     Expression (',' Expression)*
///     ;
/// ```
pub type ArgumentList = ConnectedList<Box<Expression>, Punctuation>;

/// Represents a function call syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// FunctionCall:
///     QualifiedIdentifier '(' ArgumentList? ')'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct FunctionCall {
    pub qualified_identifier: QualifiedIdentifier,
    pub left_paren: Punctuation,
    pub arguments: Option<ArgumentList>,
    pub right_paren: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Result<Span, SpanError> {
        self.qualified_identifier
            .span()?
            .join(&self.right_paren.span)
    }
}

/// Represents an expression that is surrounded by parentheses.
///
/// Syntax Synopsis:
/// ``` txt
/// Parenthesized:
///     '(' Expression ')'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Parenthesized {
    pub left_paren: Punctuation,
    pub expression: Box<Expression>,
    pub right_paren: Punctuation,
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Result<Span, SpanError> {
        self.left_paren.span()?.join(&self.right_paren.span)
    }
}

/// Represents a field initializer syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// FieldInitializer:
///     Identifier ':' Expression
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct FieldInitializer {
    pub identifier: Identifier,
    pub colon: Punctuation,
    pub expression: Box<Expression>,
}

impl SourceElement for FieldInitializer {
    fn span(&self) -> Result<Span, SpanError> {
        self.identifier.span()?.join(&self.expression.span()?)
    }
}

/// Represents a list of field initializers separated by commas.
///
/// Syntax Synopsis:
/// ``` txt
/// FieldInitializeList:
///     FieldInitializer (',' FieldInitializer)*
///     ;
/// ```
pub type FieldInitializerList = ConnectedList<FieldInitializer, Punctuation>;

/// Represents a struct literal syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// StructLiteral:
///     QualifiedIdentifier '{' FieldInitializerList? '}'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct StructLiteral {
    pub qualified_identifier: QualifiedIdentifier,
    pub left_brace: Punctuation,
    pub field_initializers: Option<FieldInitializerList>,
    pub right_brace: Punctuation,
}

impl SourceElement for StructLiteral {
    fn span(&self) -> Result<Span, SpanError> {
        self.qualified_identifier
            .span()?
            .join(&self.right_brace.span)
    }
}

/// Represents a member access syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// MemberAccess:
///     Functional '.' Identifier
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct MemberAccess {
    pub operand: Box<Functional>,
    pub dot: Punctuation,
    pub identifier: Identifier,
}

impl SourceElement for MemberAccess {
    fn span(&self) -> Result<Span, SpanError> { self.operand.span()?.join(&self.identifier.span) }
}

/// Is an enumeration of all kinds of imperative expressions.
///
/// Imperative expressions are expressions that yield a value by executing a list of statements.
///
/// Syntax Synopsis:
/// ``` txt
/// Imperative:
///     Block
///     | IfElse
///     | Loop
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum Imperative {
    Block(Block),
    IfElse(IfElse),
    Loop(Loop),
}

impl SourceElement for Imperative {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Block(block) => block.span(),
            Self::IfElse(if_else) => if_else.span(),
            Self::Loop(loop_) => loop_.span(),
        }
    }
}

/// Represents a label specifier syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// LabelSpecifier:
///     Label ':'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct LabelSpecifier {
    pub label: Label,
    pub colon: Punctuation,
}

impl SourceElement for LabelSpecifier {
    fn span(&self) -> Result<Span, SpanError> { self.label.span()?.join(&self.colon.span) }
}

/// Represents a block syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// BlockWithoutLabel:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct BlockWithoutLabel {
    pub left_brace: Punctuation,
    pub statements: Vec<Statement>,
    pub right_brace: Punctuation,
}

impl SourceElement for BlockWithoutLabel {
    fn span(&self) -> Result<Span, SpanError> {
        self.left_brace.span()?.join(&self.right_brace.span)
    }
}

/// Represents a block syntax tree with an optional label specifier.
///
/// Syntax Synopsis:
/// ``` txt
/// Block:
///     LabelSpecifier? '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Block {
    pub label_specifier: Option<LabelSpecifier>,
    pub block_without_label: BlockWithoutLabel,
}

impl SourceElement for Block {
    fn span(&self) -> Result<Span, SpanError> {
        self.label_specifier.as_ref().map_or_else(
            || self.block_without_label.span(),
            |label_specifier| {
                label_specifier
                    .span()?
                    .join(&self.block_without_label.span()?)
            },
        )
    }
}

/// Is an enumeration of either a block or an if-else expression.
///
/// Syntax Synopsis:
/// ``` txt
/// BlockOrIfElse:
///     Block
///     | IfElse
///     ;
/// ```
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum BlockOrIfElse {
    Block(Block),
    IfElse(IfElse),
}

impl SourceElement for BlockOrIfElse {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Block(block) => block.span(),
            Self::IfElse(if_else) => if_else.span(),
        }
    }
}

/// Represents an else portion of an if-else expression.
///
/// Syntax Synopsis:
/// ``` txt
/// Else:
///     'else' BlockOrIfElse
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Else {
    pub else_keyword: Keyword,
    pub expression: Box<BlockOrIfElse>,
}

impl SourceElement for Else {
    fn span(&self) -> Result<Span, SpanError> {
        self.else_keyword.span()?.join(&self.expression.span()?)
    }
}

/// Represents an if-else expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// IfElse:
///     'if' '(' Expression ')' Block Else?
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct IfElse {
    pub if_keyword: Keyword,
    pub left_paren: Punctuation,
    pub condition: Box<Expression>,
    pub right_paren: Punctuation,
    pub then_expression: Block,
    pub else_expression: Option<Else>,
}

impl SourceElement for IfElse {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.if_keyword.span()?;
        let end = match &self.else_expression {
            Some(else_expression) => else_expression.span()?,
            None => self.then_expression.span()?,
        };

        start.join(&end)
    }
}

/// Represents a loop expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Loop:
///     LabelSpecifier? 'loop' BlockWithoutLabel
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Loop {
    pub label_specifier: Option<LabelSpecifier>,
    pub loop_keyword: Keyword,
    pub block_without_label: BlockWithoutLabel,
}

impl SourceElement for Loop {
    fn span(&self) -> Result<Span, SpanError> {
        let start = match &self.label_specifier {
            Some(label_specifier) => label_specifier.span()?,
            None => self.loop_keyword.span.clone(),
        };
        let end = self.block_without_label.span()?;

        start.join(&end)
    }
}

/// Represents a continue expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Continue:
///     'continue' Label?
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Continue {
    pub continue_keyword: Keyword,
    pub label: Option<Label>,
}

impl SourceElement for Continue {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.continue_keyword.span.clone();
        let end = match &self.label {
            Some(label) => label.span()?,
            None => self.continue_keyword.span.clone(),
        };

        start.join(&end)
    }
}

/// Represents an express expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Express:
///     'express' Label? Functional?
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Express {
    pub express_keyword: Keyword,
    pub label: Option<Label>,
    pub expression: Option<Functional>,
}

impl SourceElement for Express {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.express_keyword.span.clone();
        let end = match self.label {
            Some(ref label) => label.span()?,
            None => match self.expression {
                Some(ref expression) => expression.span()?,
                None => self.express_keyword.span.clone(),
            },
        };

        start.join(&end)
    }
}

/// Represents a break expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Break:
///     'break' Label? Functional?
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Break {
    pub break_keyword: Keyword,
    pub label: Option<Label>,
    pub expression: Option<Functional>,
}

impl SourceElement for Break {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.break_keyword.span.clone();
        let end = match self.label {
            Some(ref label) => label.span()?,
            None => match self.expression {
                Some(ref expression) => expression.span()?,
                None => self.break_keyword.span.clone(),
            },
        };

        start.join(&end)
    }
}

/// Represents a return expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Return:
///     'return' Functional?
///     ;
/// ```
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Return {
    pub return_keyword: Keyword,
    pub expression: Option<Functional>,
}

impl SourceElement for Return {
    fn span(&self) -> Result<Span, SpanError> {
        let start = self.return_keyword.span.clone();
        let end = match self.expression {
            Some(ref expression) => expression.span()?,
            None => self.return_keyword.span.clone(),
        };

        start.join(&end)
    }
}

impl<'a> Parser<'a> {
    fn parse_binary_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        let mut first_functional = self.parse_primary_expression(handler)?;
        let mut expressions = Vec::new();

        // Parses a list of binary operators and expressions
        while let Some(binary_operator) = self.try_parse_binary_operator() {
            expressions.push((
                binary_operator,
                Some(self.parse_primary_expression(handler)?),
            ));
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

                    Ordering::Less => (),
                }
            }

            // ASSUMPTION: The assignments have 1 precedence and are right associative.
            assert!(current_precedence > 0);

            if candidate_index == 0 {
                let (binary_operator, right_expression) = expressions.remove(0);

                // Replace the first expression with the folded expression.
                first_functional = Functional::Binary(Binary {
                    left_operand: Box::new(first_functional),
                    operator: binary_operator,
                    right_operand: Box::new(right_expression.unwrap()),
                });
            } else {
                let (binary_operator, right_expression) = expressions.remove(candidate_index);

                // Replace the expression at the index with the folded expression.
                expressions[candidate_index - 1].1 = Some(Functional::Binary(Binary {
                    left_operand: Box::new(expressions[candidate_index - 1].1.take().unwrap()),
                    operator: binary_operator,
                    right_operand: Box::new(right_expression.unwrap()),
                }));
            }
        }

        Ok(first_functional)
    }

    fn parse_loop_and_block(
        &mut self,
        label_specifier: Option<LabelSpecifier>,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Imperative> {
        Ok(match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == '{' => {
                let block_without_label = self.parse_block_without_label(handler)?;

                // parse block
                Imperative::Block(Block {
                    label_specifier,
                    block_without_label,
                })
            }
            Some(Token::Keyword(loop_keyword)) if loop_keyword.keyword == KeywordKind::Loop => {
                // eat loop keyword
                self.forward();

                let block_without_label = self.parse_block_without_label(handler)?;

                // parse loop
                Imperative::Loop(Loop {
                    label_specifier,
                    loop_keyword,
                    block_without_label,
                })
            }

            found => {
                // forward/make progress
                self.forward();
                handler.recieve(Error::ExpressionExpected(ExpressionExpected { found }));
                return Err(ParserError);
            }
        })
    }

    /// Parses an [`Expression`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_expression(&mut self, handler: &impl Handler<Error>) -> ParserResult<Expression> {
        match self.stop_at_significant() {
            // parse return expression
            Some(Token::Keyword(return_keyword))
                if return_keyword.keyword == KeywordKind::Return =>
            {
                // eat return keyword
                self.next_token();

                let expression = self.try_parse_functional();

                Ok(Expression::Terminator(Terminator::Return(Return {
                    return_keyword,
                    expression,
                })))
            }

            // parse continue expression
            Some(Token::Keyword(continue_keyword))
                if continue_keyword.keyword == KeywordKind::Continue =>
            {
                // eat return keyword
                self.next_token();

                let label = self.try_parse_label(handler)?;

                Ok(Expression::Terminator(Terminator::Continue(Continue {
                    continue_keyword,
                    label,
                })))
            }

            // parse break expression
            Some(Token::Keyword(break_keyword)) if break_keyword.keyword == KeywordKind::Break => {
                // eat return keyword
                self.next_token();

                let label = self.try_parse_label(handler)?;
                let expression = self.try_parse_functional();

                Ok(Expression::Terminator(Terminator::Break(Break {
                    break_keyword,
                    label,
                    expression,
                })))
            }

            // parse express expression
            Some(Token::Keyword(express_keyword))
                if express_keyword.keyword == KeywordKind::Express =>
            {
                // eat return keyword
                self.next_token();

                let label = self.try_parse_label(handler)?;
                let expression = self.try_parse_functional();

                Ok(Expression::Terminator(Terminator::Express(Express {
                    express_keyword,
                    label,
                    expression,
                })))
            }

            // parse if else expression
            Some(Token::Keyword(if_keyword)) if if_keyword.keyword == KeywordKind::If => self
                .parse_if_else(handler)
                .map(|x| Expression::Imperative(Imperative::IfElse(x))),

            // parse loop or block expression with additional label sepcifier
            Some(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.next_token();

                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;

                Ok(Expression::Imperative(self.parse_loop_and_block(
                    Some(LabelSpecifier {
                        label: Label {
                            apostrophe,
                            identifier,
                        },
                        colon,
                    }),
                    handler,
                )?))
            }

            // parse loop or block expression
            Some(token)
                if matches!(&token, Token::Punctuation(p) if p.punctuation == '{')
                    || matches!(&token, Token::Keyword(loop_keyword) if loop_keyword.keyword == KeywordKind::Loop) =>
            {
                Ok(Expression::Imperative(
                    self.parse_loop_and_block(None, handler)?,
                ))
            }

            _ => self
                .parse_binary_expression(handler)
                .map(Expression::Functional),
        }
    }

    fn parse_label(&mut self, handler: &impl Handler<Error>) -> ParserResult<Label> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Ok(Label {
            apostrophe,
            identifier,
        })
    }

    fn parse_label_specifier(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<LabelSpecifier> {
        let label = self.parse_label(handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;

        Ok(LabelSpecifier { label, colon })
    }

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_level = self
            .try_parse(|parser| match parser.next_significant_token() {
                Some(Token::Punctuation(p)) => match p.punctuation {
                    '+' => Ok(BinaryOperator::Add(p)),
                    '-' => Ok(BinaryOperator::Subtract(p)),
                    '*' => Ok(BinaryOperator::Multiply(p)),
                    '/' => Ok(BinaryOperator::Divide(p)),
                    '%' => Ok(BinaryOperator::Modulo(p)),
                    '=' => Ok(BinaryOperator::Assign(p)),
                    '!' => {
                        let equal = parser.parse_punctuation('=', false, &Dummy)?;
                        Ok(BinaryOperator::NotEqual(p, equal))
                    }
                    '>' => Ok(BinaryOperator::GreaterThan(p)),
                    '<' => Ok(BinaryOperator::LessThan(p)),
                    _ => Err(ParserError),
                },
                Some(Token::Keyword(k)) => match k.keyword {
                    KeywordKind::And => Ok(BinaryOperator::LogicalAnd(k)),
                    KeywordKind::Or => Ok(BinaryOperator::LogicalOr(k)),
                    _ => Err(ParserError),
                },
                _ => Err(ParserError),
            })
            .ok()?;

        Some(
            self.try_parse(|parser| match (first_level.clone(), parser.next_token()) {
                (first_level, Some(Token::Punctuation(n))) => match (first_level, n.punctuation) {
                    (BinaryOperator::Add(p), '=') => Ok(BinaryOperator::CompoundAdd(p, n)),
                    (BinaryOperator::Subtract(p), '=') => {
                        Ok(BinaryOperator::CompoundSubtract(p, n))
                    }
                    (BinaryOperator::Multiply(p), '=') => {
                        Ok(BinaryOperator::CompoundMultiply(p, n))
                    }
                    (BinaryOperator::Divide(p), '=') => Ok(BinaryOperator::CompoundDivide(p, n)),
                    (BinaryOperator::Modulo(p), '=') => Ok(BinaryOperator::CompoundModulo(p, n)),
                    (BinaryOperator::Assign(p), '=') => Ok(BinaryOperator::Equal(p, n)),
                    (BinaryOperator::GreaterThan(p), '=') => {
                        Ok(BinaryOperator::GreaterThanOrEqual(p, n))
                    }
                    (BinaryOperator::LessThan(p), '=') => Ok(BinaryOperator::LessThanOrEqual(p, n)),
                    _ => Err(ParserError),
                },
                _ => Err(ParserError),
            })
            .unwrap_or(first_level),
        )
    }

    fn try_parse_prefix_operator(&mut self) -> Option<PrefixOperator> {
        self.try_parse(|parser| match parser.next_significant_token() {
            Some(Token::Punctuation(p)) if p.punctuation == '!' => {
                Ok(PrefixOperator::LogicalNot(p))
            }
            Some(Token::Punctuation(p)) if p.punctuation == '-' => Ok(PrefixOperator::Negate(p)),
            Some(Token::Punctuation(p)) if p.punctuation == '&' => {
                Ok(PrefixOperator::ReferenceOf(p))
            }
            Some(Token::Punctuation(p)) if p.punctuation == '*' => {
                Ok(PrefixOperator::Dereference(p))
            }
            _ => Err(ParserError),
        })
        .ok()
    }

    fn handle_struct_literal(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        let EnclosedList {
            open: left_brace,
            list: field_initializers,
            close: right_brace,
        } = self.parse_enclosed_tree(
            Delimiter::Brace,
            ',',
            |this, handler| {
                let identifier = this.parse_identifier(handler)?;
                let colon = this.parse_punctuation(':', true, handler)?;
                let expression = Box::new(this.parse_expression(handler)?);

                // field initializer
                Ok(FieldInitializer {
                    identifier,
                    colon,
                    expression,
                })
            },
            handler,
        )?;

        Ok(Functional::StructLiteral(StructLiteral {
            qualified_identifier,
            left_brace,
            field_initializers,
            right_brace,
        }))
    }

    fn handle_function_call(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        let EnclosedList {
            open: left_paren,
            list: arguments,
            close: right_paren,
        } = self.parse_enclosed_tree(
            Delimiter::Parenthesis,
            ',',
            |this, handler| Ok(Box::new(this.parse_expression(handler)?)),
            handler,
        )?;

        Ok(Functional::FunctionCall(FunctionCall {
            qualified_identifier,
            left_paren,
            arguments,
            right_paren,
        }))
    }

    fn parse_identifier_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        let qualified_identifier = self.parse_qualified_identifier(true, handler)?;

        match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == '(' => {
                self.handle_function_call(qualified_identifier, handler)
            }

            Some(Token::Punctuation(p)) if p.punctuation == '{' => {
                self.handle_struct_literal(qualified_identifier, handler)
            }

            _ => Ok(Functional::Named(Named {
                qualified_identifier,
            })),
        }
    }

    fn parse_parenthesized_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        let left_paren = self.step_into(Delimiter::Parenthesis, handler)?;
        let expression = Box::new(self.parse_expression(handler)?);
        let right_paren = self.step_out(handler)?;

        Ok(Functional::Parenthesized(Parenthesized {
            left_paren,
            expression,
            right_paren,
        }))
    }

    fn parse_block_without_label(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<BlockWithoutLabel> {
        fn skip_to_next_statement(this: &mut Parser) {
            this.stop_at(|token| matches!(token, Token::Punctuation(p) if p.punctuation == ';'));

            if matches!(this.peek(), Some(Token::Punctuation(p)) if p.punctuation == ';') {
                this.forward();
            }
        }

        self.step_into(Delimiter::Brace, handler)?;

        let (left_brace, right_brace) = {
            let delimited = self.token_provider.as_delimited().unwrap();
            (delimited.open.clone(), delimited.close.clone())
        };

        let mut statements = Vec::new();

        while !self.is_exhausted() {
            // parse the statement
            let Ok(statement) = self.parse_statement(handler) else {
                skip_to_next_statement(self);
                continue;
            };

            statements.push(statement);
        }

        self.step_out(handler)?;

        Ok(BlockWithoutLabel {
            left_brace,
            statements,
            right_brace,
        })
    }

    fn parse_block(&mut self, handler: &impl Handler<Error>) -> ParserResult<Block> {
        // parse optional label specifier
        let label_specifier = if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '\'')
        {
            Some(self.parse_label_specifier(handler)?)
        } else {
            None
        };

        // parse the block
        let block_without_label = self.parse_block_without_label(handler)?;

        Ok(Block {
            label_specifier,
            block_without_label,
        })
    }

    fn parse_else(&mut self, handler: &impl Handler<Error>) -> ParserResult<Else> {
        let else_keyword = self.parse_keyword(KeywordKind::Else, handler)?;
        let expression = Box::new(
            if matches!(self.stop_at_significant(), Some(Token::Keyword(k)) if k.keyword == KeywordKind::If)
            {
                BlockOrIfElse::IfElse(self.parse_if_else(handler)?)
            } else {
                BlockOrIfElse::Block(self.parse_block(handler)?)
            },
        );

        Ok(Else {
            else_keyword,
            expression,
        })
    }

    fn parse_if_else(&mut self, handler: &impl Handler<Error>) -> ParserResult<IfElse> {
        let if_keyword = self.parse_keyword(KeywordKind::If, handler)?;
        let left_paren = self.step_into(Delimiter::Parenthesis, handler)?;
        let condition = Box::new(self.parse_expression(handler)?);
        let right_paren = self.step_out(handler)?;
        let then_expression = self.parse_block(handler)?;
        let else_expression = if matches!(self.stop_at_significant(),
                Some(Token::Keyword(else_keyword))
                    if else_keyword.keyword == KeywordKind::Else)
        {
            Some(self.parse_else(handler)?)
        } else {
            None
        };

        Ok(IfElse {
            if_keyword,
            left_paren,
            condition,
            right_paren,
            then_expression,
            else_expression,
        })
    }

    fn try_parse_functional(&mut self) -> Option<Functional> {
        self.try_parse(|parser| parser.parse_binary_expression(&Dummy))
            .ok()
    }

    fn try_parse_label(&mut self, handler: &impl Handler<Error>) -> ParserResult<Option<Label>> {
        // parse optional label
        Ok(
            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '\'')
            {
                Some(self.parse_label(handler)?)
            } else {
                None
            },
        )
    }

    /// Parses a primary [`Expression`]
    #[allow(clippy::missing_errors_doc, clippy::too_many_lines)]
    pub fn parse_primary_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Functional> {
        // early return for prefix expression
        if let Some(prefix_operator) = self.try_parse_prefix_operator() {
            return Ok(Functional::Prefix(Prefix {
                operator: prefix_operator,
                operand: Box::new(self.parse_primary_expression(handler)?),
            }));
        }

        let mut expression = match self.stop_at_significant() {
            // parse numeric literal
            Some(Token::NumericLiteral(numeric_literal_token)) => {
                // eat numericl iteral
                self.forward();

                Functional::NumericLiteral(NumericLiteral {
                    numeric_literal_token,
                })
            }

            // parse boolean literal
            Some(Token::Keyword(boolean))
                if matches!(boolean.keyword, KeywordKind::True | KeywordKind::False) =>
            {
                // eat token
                self.forward();

                let boolean_literal = match boolean.keyword {
                    KeywordKind::True => BooleanLiteral::True,
                    KeywordKind::False => BooleanLiteral::False,
                    _ => unreachable!(),
                };

                Functional::BooleanLiteral(boolean_literal(boolean))
            }

            // parse qualified identifier expression
            Some(Token::Identifier(..)) => self.parse_identifier_expression(handler)?,
            Some(Token::Punctuation(p))
                if p.punctuation == ':'
                    && self.peek_offset(1).map_or(
                        false,
                        |x| matches!(x, Token::Punctuation(p) if p.punctuation == ':'),
                    ) =>
            {
                self.parse_identifier_expression(handler)?
            }

            // parenthesized
            Some(Token::Punctuation(p)) if p.punctuation == '(' => {
                self.parse_parenthesized_expression(handler)?
            }

            found => {
                // forward/make progress
                self.forward();
                handler.recieve(Error::ExpressionExpected(ExpressionExpected { found }));
                return Err(ParserError);
            }
        };

        loop {
            match self.stop_at_significant() {
                Some(Token::Punctuation(dot)) if dot.punctuation == '.' => {
                    // eat token
                    self.forward();

                    let identifier = self.parse_identifier(handler)?;

                    // update expression
                    expression = Functional::MemberAccess(MemberAccess {
                        operand: Box::new(expression),
                        dot,
                        identifier,
                    });
                }
                Some(Token::Keyword(as_keyword)) if as_keyword.keyword == KeywordKind::As => {
                    // eat token
                    self.forward();

                    let left_paren = self.step_into(Delimiter::Parenthesis, handler)?;
                    let type_specifier = self.parse_type_specifier(handler)?;
                    let right_paren = self.step_out(handler)?;

                    // update expression
                    expression = Functional::Cast(Cast {
                        operand: Box::new(expression),
                        as_keyword,
                        left_paren,
                        type_specifier,
                        right_paren,
                    });
                }
                _ => break,
            };
        }

        Ok(expression)
    }
}

#[cfg(test)]
mod tests;
