//! Contains the definitions of expression syntax tree.

use std::cmp::Ordering;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_lexical::{
    token::{
        Identifier, Keyword, KeywordKind, NumericLiteral as NumericLiteralToken, Punctuation, Token,
    },
    token_stream::Delimiter,
};
use pernixc_source::{SourceElement, Span};
use pernixc_system::diagnostic::{Dummy, Handler};

use super::{
    statement::Statement, ConnectedList, EnclosedList, Label, QualifiedIdentifier, TypeSpecifier,
};
use crate::{
    error::{Error, ExpressionExpected},
    parser::Parser,
};

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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
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
    ArrowOperator(ArrowOperator),
    Cast(Cast),
}

impl SourceElement for Functional {
    fn span(&self) -> Span {
        match self {
            Self::NumericLiteral(numeric_literal) => {
                numeric_literal.numeric_literal_token.span.clone()
            }
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::Binary(binary_expression) => binary_expression.span(),
            Self::Prefix(prefix_expression) => prefix_expression.span(),
            Self::Named(identifier_expression) => identifier_expression.span(),
            Self::FunctionCall(function_call_expression) => function_call_expression.span(),
            Self::Parenthesized(parenthesized_expression) => parenthesized_expression.span(),
            Self::StructLiteral(struct_literal) => struct_literal.span(),
            Self::MemberAccess(member_access_expression) => member_access_expression.span(),
            Self::ArrowOperator(arrow_operator_expression) => arrow_operator_expression.span(),
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct NumericLiteral {
    #[get = "pub"]
    numeric_literal_token: NumericLiteralToken,
}

impl SourceElement for NumericLiteral {
    fn span(&self) -> Span { self.numeric_literal_token.span.clone() }
}

/// Represents a cast expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// CastExpression:
///     Functional 'as' '(' TypeSpecifier ')'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Cast {
    #[get = "pub"]
    operand: Box<Functional>,
    #[get = "pub"]
    as_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    type_specifier: TypeSpecifier,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for Cast {
    fn span(&self) -> Span { self.operand.span().join(&self.right_paren.span).unwrap() }
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
    fn span(&self) -> Span {
        match self {
            Self::True(keyword) | Self::False(keyword) => keyword.span.clone(),
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Binary {
    #[get = "pub"]
    left_operand: Box<Functional>,
    #[get = "pub"]
    operator: BinaryOperator,
    #[get = "pub"]
    right_operand: Box<Functional>,
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.left_operand
            .span()
            .join(&self.right_operand.span())
            .unwrap()
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
    fn span(&self) -> Span {
        match self {
            Self::Add(token)
            | Self::Subtract(token)
            | Self::Multiply(token)
            | Self::Divide(token)
            | Self::Modulo(token)
            | Self::Assign(token)
            | Self::LessThan(token)
            | Self::GreaterThan(token) => token.span.clone(),
            Self::CompoundAdd(token, token1)
            | Self::CompoundSubtract(token, token1)
            | Self::CompoundMultiply(token, token1)
            | Self::CompoundDivide(token, token1)
            | Self::CompoundModulo(token, token1)
            | Self::Equal(token, token1)
            | Self::NotEqual(token, token1)
            | Self::LessThanOrEqual(token, token1)
            | Self::GreaterThanOrEqual(token, token1) => token.span().join(&token1.span).unwrap(),
            Self::LogicalAnd(token) | Self::LogicalOr(token) => token.span.clone(),
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
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token)
            | Self::Negate(token)
            | Self::ReferenceOf(token)
            | Self::Dereference(token) => token.span.clone(),
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Prefix {
    #[get = "pub"]
    operator: PrefixOperator,

    #[get = "pub"]
    operand: Box<Functional>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span { self.operator.span().join(&self.operand.span()).unwrap() }
}

/// Represents a postfix operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Named:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Named {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for Named {
    fn span(&self) -> Span { self.qualified_identifier.span() }
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct FunctionCall {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    arguments: Option<ArgumentList>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.right_paren.span)
            .unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Parenthesized {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Span { self.left_paren.span().join(&self.right_paren.span).unwrap() }
}

/// Represents a field initializer syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// FieldInitializer:
///     Identifier ':' Expression
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct FieldInitializer {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
}

impl SourceElement for FieldInitializer {
    fn span(&self) -> Span {
        self.identifier
            .span()
            .join(&self.expression.span())
            .unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct StructLiteral {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    field_initializers: Option<FieldInitializerList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for StructLiteral {
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.right_brace.span)
            .unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct MemberAccess {
    #[get = "pub"]
    operand: Box<Functional>,
    #[get = "pub"]
    dot: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for MemberAccess {
    fn span(&self) -> Span { self.operand.span().join(&self.identifier.span).unwrap() }
}

/// Represents an arrow syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Arrow:
///     '->'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Arrow {
    #[get = "pub"]
    hyphen: Punctuation,
    #[get = "pub"]
    right_angle: Punctuation,
}

impl SourceElement for Arrow {
    fn span(&self) -> Span { self.hyphen.span.join(&self.right_angle.span).unwrap() }
}

/// Represents an arrow operator syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// ArrowOperator:
///     Functional Arrow Identifier
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct ArrowOperator {
    #[get = "pub"]
    operand: Box<Functional>,
    #[get = "pub"]
    arrow: Arrow,
    #[get = "pub"]
    identifier: Identifier,
}

impl SourceElement for ArrowOperator {
    fn span(&self) -> Span { self.operand.span().join(&self.identifier.span).unwrap() }
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
    fn span(&self) -> Span {
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct LabelSpecifier {
    #[get = "pub"]
    label: Label,
    #[get = "pub"]
    colon: Punctuation,
}

impl SourceElement for LabelSpecifier {
    fn span(&self) -> Span { self.label.span().join(&self.colon.span).unwrap() }
}

/// Represents a block syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// BlockWithoutLabel:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct BlockWithoutLabel {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    statements: Vec<Statement>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for BlockWithoutLabel {
    fn span(&self) -> Span { self.left_brace.span().join(&self.right_brace.span).unwrap() }
}

/// Represents a block syntax tree with an optional label specifier.
///
/// Syntax Synopsis:
/// ``` txt
/// Block:
///     LabelSpecifier? '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Block {
    #[get = "pub"]
    label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    block_without_label: BlockWithoutLabel,
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        self.label_specifier.as_ref().map_or_else(
            || self.block_without_label.span(),
            |label_specifier| {
                label_specifier
                    .span()
                    .join(&self.block_without_label.span())
                    .unwrap()
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
    fn span(&self) -> Span {
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Else {
    #[get = "pub"]
    else_keyword: Keyword,
    #[get = "pub"]
    expression: Box<BlockOrIfElse>,
}

impl SourceElement for Else {
    fn span(&self) -> Span {
        self.else_keyword
            .span()
            .join(&self.expression.span())
            .unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct IfElse {
    #[get = "pub"]
    if_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    condition: Box<Expression>,
    #[get = "pub"]
    right_paren: Punctuation,
    #[get = "pub"]
    then_expression: Block,
    #[get = "pub"]
    else_expression: Option<Else>,
}

impl SourceElement for IfElse {
    fn span(&self) -> Span {
        let start = self.if_keyword.span();
        let end = self
            .else_expression
            .as_ref()
            .map_or(self.then_expression.span(), |else_expression| {
                else_expression.span()
            });

        start.join(&end).unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Loop {
    #[get = "pub"]
    label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    loop_keyword: Keyword,
    #[get = "pub"]
    block_without_label: BlockWithoutLabel,
}

impl SourceElement for Loop {
    fn span(&self) -> Span {
        let start = self.label_specifier.as_ref().map_or_else(
            || self.loop_keyword.span.clone(),
            pernixc_source::SourceElement::span,
        );
        let end = self.block_without_label.span();

        start.join(&end).unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Continue {
    #[get = "pub"]
    continue_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
}

impl SourceElement for Continue {
    fn span(&self) -> Span {
        let start = self.continue_keyword.span.clone();
        let end = self.label.as_ref().map_or_else(
            || self.continue_keyword.span.clone(),
            pernixc_source::SourceElement::span,
        );

        start.join(&end).unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Express {
    #[get = "pub"]
    express_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    expression: Option<Functional>,
}

impl SourceElement for Express {
    fn span(&self) -> Span {
        let start = self.express_keyword.span.clone();
        let end = self.label.as_ref().map_or_else(
            || {
                self.expression
                    .as_ref()
                    .map_or(self.express_keyword.span.clone(), |expression| {
                        expression.span()
                    })
            },
            pernixc_source::SourceElement::span,
        );

        start.join(&end).unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Break {
    #[get = "pub"]
    break_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    expression: Option<Functional>,
}

impl SourceElement for Break {
    fn span(&self) -> Span {
        let start = self.break_keyword.span.clone();
        let end = self.label.as_ref().map_or_else(
            || {
                self.expression.as_ref().map_or_else(
                    || self.break_keyword.span.clone(),
                    pernixc_source::SourceElement::span,
                )
            },
            pernixc_source::SourceElement::span,
        );

        start.join(&end).unwrap()
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
#[derive(Debug, Clone, Getters)]
#[allow(missing_docs)]
pub struct Return {
    #[get = "pub"]
    return_keyword: Keyword,
    #[get = "pub"]
    expression: Option<Functional>,
}

impl SourceElement for Return {
    fn span(&self) -> Span {
        let start = self.return_keyword.span.clone();
        let end = self.expression.as_ref().map_or_else(
            || self.return_keyword.span.clone(),
            pernixc_source::SourceElement::span,
        );

        start.join(&end).unwrap()
    }
}

impl<'a> Parser<'a> {
    fn parse_binary_expression(&mut self, handler: &impl Handler<Error>) -> Option<Functional> {
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

        Some(first_functional)
    }

    fn parse_loop_and_block(
        &mut self,
        label_specifier: Option<LabelSpecifier>,
        handler: &impl Handler<Error>,
    ) -> Option<Imperative> {
        Some(match self.stop_at_significant() {
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
                handler.receive(Error::ExpressionExpected(ExpressionExpected {
                    found: self.get_actual_found_token(found),
                }));
                return None;
            }
        })
    }

    /// Parses an [`Expression`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_expression(&mut self, handler: &impl Handler<Error>) -> Option<Expression> {
        match self.stop_at_significant() {
            // parse return expression
            Some(Token::Keyword(return_keyword))
                if return_keyword.keyword == KeywordKind::Return =>
            {
                // eat return keyword
                self.next_token();

                let expression = self.try_parse_functional();

                Some(Expression::Terminator(Terminator::Return(Return {
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

                Some(Expression::Terminator(Terminator::Continue(Continue {
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

                Some(Expression::Terminator(Terminator::Break(Break {
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

                Some(Expression::Terminator(Terminator::Express(Express {
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

                Some(Expression::Imperative(self.parse_loop_and_block(
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
                Some(Expression::Imperative(
                    self.parse_loop_and_block(None, handler)?,
                ))
            }

            _ => self
                .parse_binary_expression(handler)
                .map(Expression::Functional),
        }
    }

    fn parse_label(&mut self, handler: &impl Handler<Error>) -> Option<Label> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(Label {
            apostrophe,
            identifier,
        })
    }

    fn parse_label_specifier(&mut self, handler: &impl Handler<Error>) -> Option<LabelSpecifier> {
        let label = self.parse_label(handler)?;
        let colon = self.parse_punctuation(':', true, handler)?;

        Some(LabelSpecifier { label, colon })
    }

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_level = self.try_parse(|parser| match parser.next_significant_token() {
            Some(Token::Punctuation(p)) => match p.punctuation {
                '+' => Some(BinaryOperator::Add(p)),
                '-' => Some(BinaryOperator::Subtract(p)),
                '*' => Some(BinaryOperator::Multiply(p)),
                '/' => Some(BinaryOperator::Divide(p)),
                '%' => Some(BinaryOperator::Modulo(p)),
                '=' => Some(BinaryOperator::Assign(p)),
                '!' => {
                    let equal = parser.parse_punctuation('=', false, &Dummy)?;
                    Some(BinaryOperator::NotEqual(p, equal))
                }
                '>' => Some(BinaryOperator::GreaterThan(p)),
                '<' => Some(BinaryOperator::LessThan(p)),
                _ => None,
            },
            Some(Token::Keyword(k)) => match k.keyword {
                KeywordKind::And => Some(BinaryOperator::LogicalAnd(k)),
                KeywordKind::Or => Some(BinaryOperator::LogicalOr(k)),
                _ => None,
            },
            _ => None,
        })?;

        Some(
            self.try_parse(|parser| match (first_level.clone(), parser.next_token()) {
                (first_level, Some(Token::Punctuation(n))) => match (first_level, n.punctuation) {
                    (BinaryOperator::Add(p), '=') => Some(BinaryOperator::CompoundAdd(p, n)),
                    (BinaryOperator::Subtract(p), '=') => {
                        Some(BinaryOperator::CompoundSubtract(p, n))
                    }
                    (BinaryOperator::Multiply(p), '=') => {
                        Some(BinaryOperator::CompoundMultiply(p, n))
                    }
                    (BinaryOperator::Divide(p), '=') => Some(BinaryOperator::CompoundDivide(p, n)),
                    (BinaryOperator::Modulo(p), '=') => Some(BinaryOperator::CompoundModulo(p, n)),
                    (BinaryOperator::Assign(p), '=') => Some(BinaryOperator::Equal(p, n)),
                    (BinaryOperator::GreaterThan(p), '=') => {
                        Some(BinaryOperator::GreaterThanOrEqual(p, n))
                    }
                    (BinaryOperator::LessThan(p), '=') => {
                        Some(BinaryOperator::LessThanOrEqual(p, n))
                    }
                    _ => None,
                },
                _ => None,
            })
            .unwrap_or(first_level),
        )
    }

    fn try_parse_prefix_operator(&mut self) -> Option<PrefixOperator> {
        self.try_parse(|parser| match parser.next_significant_token() {
            Some(Token::Punctuation(p)) if p.punctuation == '!' => {
                Some(PrefixOperator::LogicalNot(p))
            }
            Some(Token::Punctuation(p)) if p.punctuation == '-' => Some(PrefixOperator::Negate(p)),
            Some(Token::Punctuation(p)) if p.punctuation == '&' => {
                Some(PrefixOperator::ReferenceOf(p))
            }
            Some(Token::Punctuation(p)) if p.punctuation == '*' => {
                Some(PrefixOperator::Dereference(p))
            }
            _ => None,
        })
    }

    fn handle_struct_literal(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> Option<Functional> {
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
                Some(FieldInitializer {
                    identifier,
                    colon,
                    expression,
                })
            },
            handler,
        )?;

        Some(Functional::StructLiteral(StructLiteral {
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
    ) -> Option<Functional> {
        let EnclosedList {
            open: left_paren,
            list: arguments,
            close: right_paren,
        } = self.parse_enclosed_tree(
            Delimiter::Parenthesis,
            ',',
            |this, handler| Some(Box::new(this.parse_expression(handler)?)),
            handler,
        )?;

        Some(Functional::FunctionCall(FunctionCall {
            qualified_identifier,
            left_paren,
            arguments,
            right_paren,
        }))
    }

    fn parse_identifier_expression(&mut self, handler: &impl Handler<Error>) -> Option<Functional> {
        let qualified_identifier = self.parse_qualified_identifier(true, handler)?;

        match self.stop_at_significant() {
            Some(Token::Punctuation(p)) if p.punctuation == '(' => {
                self.handle_function_call(qualified_identifier, handler)
            }

            Some(Token::Punctuation(p)) if p.punctuation == '{' => {
                self.handle_struct_literal(qualified_identifier, handler)
            }

            _ => Some(Functional::Named(Named {
                qualified_identifier,
            })),
        }
    }

    fn parse_parenthesized_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<Functional> {
        let left_paren = self.step_into(Delimiter::Parenthesis, handler)?;
        let expression = Box::new(self.parse_expression(handler)?);
        let right_paren = self.step_out(handler)?;

        Some(Functional::Parenthesized(Parenthesized {
            left_paren,
            expression,
            right_paren,
        }))
    }

    fn parse_block_without_label(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<BlockWithoutLabel> {
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
            let Some(statement) = self.parse_statement(handler) else {
                skip_to_next_statement(self);
                continue;
            };

            statements.push(statement);
        }

        self.step_out(handler)?;

        Some(BlockWithoutLabel {
            left_brace,
            statements,
            right_brace,
        })
    }

    fn parse_block(&mut self, handler: &impl Handler<Error>) -> Option<Block> {
        // parse optional label specifier
        let label_specifier = if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '\'')
        {
            Some(self.parse_label_specifier(handler)?)
        } else {
            None
        };

        // parse the block
        let block_without_label = self.parse_block_without_label(handler)?;

        Some(Block {
            label_specifier,
            block_without_label,
        })
    }

    fn parse_else(&mut self, handler: &impl Handler<Error>) -> Option<Else> {
        let else_keyword = self.parse_keyword(KeywordKind::Else, handler)?;
        let expression = Box::new(
            if matches!(self.stop_at_significant(), Some(Token::Keyword(k)) if k.keyword == KeywordKind::If)
            {
                BlockOrIfElse::IfElse(self.parse_if_else(handler)?)
            } else {
                BlockOrIfElse::Block(self.parse_block(handler)?)
            },
        );

        Some(Else {
            else_keyword,
            expression,
        })
    }

    fn parse_if_else(&mut self, handler: &impl Handler<Error>) -> Option<IfElse> {
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

        Some(IfElse {
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
    }

    #[allow(clippy::option_option)]
    fn try_parse_label(&mut self, handler: &impl Handler<Error>) -> Option<Option<Label>> {
        // parse optional label
        Some(
            if matches!(self.stop_at_significant(), Some(Token::Punctuation(p)) if p.punctuation == '\'')
            {
                Some(self.parse_label(handler)?)
            } else {
                None
            },
        )
    }

    fn try_parse_arrow(&mut self) -> Option<Arrow> {
        self.try_parse(|parser| {
            let hyphen = parser.parse_punctuation('-', true, &Dummy)?;
            let right_angle = parser.parse_punctuation('>', false, &Dummy)?;

            Some(Arrow {
                hyphen,
                right_angle,
            })
        })
    }

    /// Parses a primary [`Expression`]
    #[allow(clippy::missing_errors_doc, clippy::too_many_lines)]
    pub fn parse_primary_expression(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<Functional> {
        // early return for prefix expression
        if let Some(prefix_operator) = self.try_parse_prefix_operator() {
            return Some(Functional::Prefix(Prefix {
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
                handler.receive(Error::ExpressionExpected(ExpressionExpected {
                    found: self.get_actual_found_token(found),
                }));
                return None;
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
                _ => {
                    let Some(arrow) = self.try_parse_arrow() else {
                        break;
                    };

                    let identifier = self.parse_identifier(handler)?;

                    // update expression
                    expression = Functional::ArrowOperator(ArrowOperator {
                        operand: Box::new(expression),
                        arrow,
                        identifier,
                    });
                }
            };
        }

        Some(expression)
    }
}
