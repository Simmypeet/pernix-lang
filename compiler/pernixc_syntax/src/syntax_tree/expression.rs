use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{
        Identifier, Keyword, KeywordKind, NumericLiteral as NumericLiteralToken, Punctuation, Token,
    },
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span, SpanError};
use pernixc_system::diagnostic::Handler;

use super::{statement::Statement, ConnectedList, Label, QualifiedIdentifier, TypeSpecifier};
use crate::{
    error::{Error, ExpressionExpected},
    parser::{Error as ParserError, Parser, Result as ParserResult},
};

/// Is an enumeration of all kinds of expressions.
///
/// ``` txt
/// Expression:
///     Functional
///     | ImperativeExpression
///     ;
///  ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Expression {
    Functional(Functional),
    Imperative(Imperative),
}

impl SourceElement for Expression {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Functional(functional_expression) => functional_expression.span(),
            Self::Imperative(imperative_expression) => imperative_expression.span(),
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
///     | Continue
///     | Break
///     | Return
///     | Express
///     | Cast
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Functional {
    NumericLiteral(NumericLiteral),
    BooleanLiteral(BooleanLiteral),
    BinaryList(BinaryList),
    Prefix(Prefix),
    Named(Named),
    FunctionCall(FunctionCall),
    Parenthesized(Parenthesized),
    StructLiteral(StructLiteral),
    MemberAccess(MemberAccess),
    Continue(Continue),
    Break(Break),
    Return(Return),
    Express(Express),
    Cast(Cast),
}

impl SourceElement for Functional {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::NumericLiteral(numeric_literal) => {
                Ok(numeric_literal.numeric_literal_token.span.clone())
            }
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::BinaryList(binary_expression) => binary_expression.span(),
            Self::Prefix(prefix_expression) => prefix_expression.span(),
            Self::Named(identifier_expression) => identifier_expression.span(),
            Self::FunctionCall(function_call_expression) => function_call_expression.span(),
            Self::Parenthesized(parenthesized_expression) => parenthesized_expression.span(),
            Self::StructLiteral(struct_literal) => struct_literal.span(),
            Self::MemberAccess(member_access_expression) => member_access_expression.span(),
            Self::Continue(continue_) => continue_.span(),
            Self::Break(break_) => break_.span(),
            Self::Return(return_) => return_.span(),
            Self::Express(express) => express.span(),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
///     Expression 'as' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast {
    pub operand: Box<Expression>,
    pub as_keyword: Keyword,
    pub type_specifier: TypeSpecifier,
}

impl SourceElement for Cast {
    fn span(&self) -> Result<Span, SpanError> {
        self.operand.span()?.join(&self.type_specifier.span()?)
    }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
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

/// Represents a binary expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Binary:
///     Expression (BinaryOperator Expression)+
///     ;
/// ```
pub type BinaryList = ConnectedList<Box<Expression>, BinaryOperator>;

/// Represents a prefix operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// PrefixOperator:
///     '!'
///     | '-'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::LogicalNot(token) | Self::Negate(token) => Ok(token.span.clone()),
        }
    }
}

/// Represents a prefix expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Prefix:
///     PrefixOperator Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prefix {
    pub prefix_operator: PrefixOperator,
    pub operand: Box<Expression>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Result<Span, SpanError> {
        self.prefix_operator.span()?.join(&self.operand.span()?)
    }
}

/// Represents a postfix operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Named:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
///     Expression '.' Identifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberAccess {
    pub operand: Box<Expression>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
///     'express' Label? Expression?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Express {
    pub express_keyword: Keyword,
    pub label: Option<Label>,
    pub expression: Option<Box<Expression>>,
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
///     'break' Label? Expression?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Break {
    pub break_keyword: Keyword,
    pub label: Option<Label>,
    pub expression: Option<Box<Expression>>,
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
///     'return' Expression?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Return {
    pub return_keyword: Keyword,
    pub expression: Option<Box<Expression>>,
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
    /// Parses an [`Expression`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_expression(&mut self, handler: &impl Handler<Error>) -> ParserResult<Expression> {
        self.parse_primary_expression(handler)
    }

    fn try_parse_prefix_operator(&mut self) -> Option<PrefixOperator> {
        self.try_parse(|parser| match parser.next_significant_token() {
            Some(Token::Punctuation(p)) if p.punctuation == '!' => {
                Ok(PrefixOperator::LogicalNot(p))
            }
            Some(Token::Punctuation(p)) if p.punctuation == '-' => Ok(PrefixOperator::Negate(p)),
            _ => Err(ParserError),
        })
        .ok()
    }

    fn handle_struct_literal(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Expression> {
        let (left_brace, field_initializations, right_brace) = self.parse_enclosed_frame(
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

        Ok(Expression::Functional(Functional::StructLiteral(
            StructLiteral {
                qualified_identifier,
                left_brace,
                field_initializers: field_initializations,
                right_brace,
            },
        )))
    }

    fn handle_function_call(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Expression> {
        let (left_paren, arguments, right_paren) = self.parse_enclosed_frame(
            Delimiter::Parenthesis,
            ',',
            |this, handler| Ok(Box::new(this.parse_expression(handler)?)),
            handler,
        )?;

        Ok(Expression::Functional(Functional::FunctionCall(
            FunctionCall {
                qualified_identifier,
                left_paren,
                arguments,
                right_paren,
            },
        )))
    }

    fn parse_identifier_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Expression> {
        let qualified_identifier = self.parse_qualified_identifier(true, handler)?;

        match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == '(' => {
                self.handle_function_call(qualified_identifier, handler)
            }

            Some(Token::Punctuation(p)) if p.punctuation == '{' => {
                self.handle_struct_literal(qualified_identifier, handler)
            }

            _ => Ok(Expression::Functional(Functional::Named(Named {
                qualified_identifier,
            }))),
        }
    }

    /// Parses a primary [`Expression`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_primary_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> ParserResult<Expression> {
        // early return for prefix expression
        if let Some(prefix_operator) = self.try_parse_prefix_operator() {
            return Ok(Expression::Functional(Functional::Prefix(Prefix {
                prefix_operator,
                operand: Box::new(self.parse_primary_expression(handler)?),
            })));
        }

        let mut expression = match self.stop_at_significant() {
            Some(Token::NumericLiteral(numeric_literal_token)) => {
                // eat numericl iteral
                self.forward();

                Expression::Functional(Functional::NumericLiteral(NumericLiteral {
                    numeric_literal_token,
                }))
            }

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

                Expression::Functional(Functional::BooleanLiteral(boolean_literal(boolean)))
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

            found => {
                // forward/make progress
                self.forward();

                handler.recieve(Error::ExpressionExpected(ExpressionExpected { found }));

                return Err(ParserError);
            }
        };

        loop {
            let dot = match self.stop_at_significant() {
                Some(Token::Punctuation(p)) if p.punctuation == '.' => {
                    // eat token
                    self.forward();
                    p
                }
                _ => break,
            };

            let identifier = self.parse_identifier(handler)?;

            // update expression
            expression = Expression::Functional(Functional::MemberAccess(MemberAccess {
                operand: Box::new(expression),
                dot,
                identifier,
            }));
        }

        Ok(expression)
    }
}

pub mod strategy;

#[cfg(test)]
mod tests;
