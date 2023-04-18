//! Contains the syntax tree for expressions.

use std::cmp::Ordering;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{
    Identifier, Keyword, KeywordKind, NumericLiteral, Punctuation, Token,
};

use super::{
    statement::Statement, ConnectedList, Label, QualifiedIdentifier, SourceElement, TypeSpecifier,
};
use crate::{
    errors::{ExpressionExpected, PunctuationExpected, SyntacticError},
    parser::Parser,
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
#[allow(missing_docs)]
pub enum Expression {
    Functional(Functional),
    Imperative(Imperative),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
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
///     | Binary
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
    Continue(Continue),
    Break(Break),
    Return(Return),
    Express(Express),
    Cast(Cast),
}

impl SourceElement for Functional {
    fn span(&self) -> Span {
        match self {
            Self::NumericLiteral(numeric_literal) => numeric_literal.span().clone(),
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::Binary(binary_expression) => binary_expression.span(),
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

/// Represents a cast expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// CastExpression:
///     Expression 'as' TypeSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Cast {
    #[get = "pub"]
    pub(super) operand: Box<Expression>,
    #[get = "pub"]
    pub(super) as_keyword: Keyword,
    #[get = "pub"]
    pub(super) type_specifier: TypeSpecifier,
}

impl SourceElement for Cast {
    fn span(&self) -> Span {
        self.operand
            .span()
            .join(&self.type_specifier.span())
            .unwrap()
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
#[allow(missing_docs)]
pub enum BooleanLiteral {
    True(Keyword),
    False(Keyword),
}

impl SourceElement for BooleanLiteral {
    fn span(&self) -> Span {
        match self {
            Self::True(keyword) | Self::False(keyword) => keyword.span().clone(),
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
            | Self::GreaterThan(token) => token.span().clone(),
            Self::CompoundAdd(token, token1)
            | Self::CompoundSubtract(token, token1)
            | Self::CompoundMultiply(token, token1)
            | Self::CompoundDivide(token, token1)
            | Self::CompoundModulo(token, token1)
            | Self::Equal(token, token1)
            | Self::NotEqual(token, token1)
            | Self::LessThanOrEqual(token, token1)
            | Self::GreaterThanOrEqual(token, token1) => token.span().join(token1.span()).unwrap(),
            Self::LogicalAnd(token) | Self::LogicalOr(token) => token.span().clone(),
        }
    }
}

/// Represents a binary expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Binary:
///     Expression BinaryOperator Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Binary {
    #[get = "pub"]
    pub(super) left_operand: Box<Expression>,
    #[get = "pub"]
    pub(super) binary_operator: BinaryOperator,
    #[get = "pub"]
    pub(super) right_operand: Box<Expression>,
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.left_operand
            .span()
            .join(&self.right_operand.span())
            .unwrap()
    }
}

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
#[allow(missing_docs)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token) | Self::Negate(token) => token.span().clone(),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Prefix {
    #[get = "pub"]
    pub(super) prefix_operator: PrefixOperator,
    #[get = "pub"]
    pub(super) operand: Box<Expression>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span {
        self.prefix_operator
            .span()
            .join(&self.operand.span())
            .unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Named {
    #[get = "pub"]
    pub(super) qualified_identifier: QualifiedIdentifier,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct FunctionCall {
    #[get = "pub"]
    pub(super) qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    pub(super) left_paren: Punctuation,
    #[get = "pub"]
    pub(super) arguments: Option<ArgumentList>,
    #[get = "pub"]
    pub(super) right_paren: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.right_paren.span())
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Parenthesized {
    #[get = "pub"]
    pub(super) left_paren: Punctuation,
    #[get = "pub"]
    pub(super) expression: Box<Expression>,
    #[get = "pub"]
    pub(super) right_paren: Punctuation,
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Span {
        self.left_paren
            .span()
            .join(&self.right_paren.span())
            .unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct FieldInitializer {
    #[get = "pub"]
    pub(super) identifier: Identifier,
    #[get = "pub"]
    pub(super) colon: Punctuation,
    #[get = "pub"]
    pub(super) expression: Box<Expression>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct StructLiteral {
    #[get = "pub"]
    pub(super) qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    pub(super) left_brace: Punctuation,
    #[get = "pub"]
    pub(super) field_initializations: Option<FieldInitializerList>,
    #[get = "pub"]
    pub(super) right_brace: Punctuation,
}

impl SourceElement for StructLiteral {
    fn span(&self) -> Span {
        self.qualified_identifier
            .span()
            .join(&self.right_brace.span())
            .unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct MemberAccess {
    #[get = "pub"]
    pub(super) operand: Box<Expression>,
    #[get = "pub"]
    pub(super) dot: Punctuation,
    #[get = "pub"]
    pub(super) identifier: Identifier,
}

impl SourceElement for MemberAccess {
    fn span(&self) -> Span { self.operand.span().join(&self.identifier.span()).unwrap() }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct LabelSpecifier {
    #[get = "pub"]
    pub(super) label: Label,
    #[get = "pub"]
    pub(super) colon: Punctuation,
}

impl SourceElement for LabelSpecifier {
    fn span(&self) -> Span { self.label.span().join(&self.colon.span()).unwrap() }
}

/// Represents a block syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// BlockWithoutLabel:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct BlockWithoutLabel {
    #[get = "pub"]
    pub(super) left_brace: Punctuation,
    #[get = "pub"]
    pub(super) statements: Vec<Statement>,
    #[get = "pub"]
    pub(super) right_brace: Punctuation,
}

impl SourceElement for BlockWithoutLabel {
    fn span(&self) -> Span {
        self.left_brace
            .span()
            .join(&self.right_brace.span())
            .unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Block {
    #[get = "pub"]
    pub(super) label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    pub(super) block_without_label: BlockWithoutLabel,
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        match &self.label_specifier {
            Some(label_specifier) => label_specifier
                .span()
                .join(&self.block_without_label.span())
                .unwrap(),
            None => self.block_without_label.span(),
        }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Else {
    #[get = "pub"]
    pub(super) else_keyword: Keyword,
    #[get = "pub"]
    pub(super) expression: Box<BlockOrIfElse>,
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct IfElse {
    #[get = "pub"]
    pub(super) if_keyword: Keyword,
    #[get = "pub"]
    pub(super) left_paren: Punctuation,
    #[get = "pub"]
    pub(super) condition: Box<Expression>,
    #[get = "pub"]
    pub(super) right_paren: Punctuation,
    #[get = "pub"]
    pub(super) then_expression: Block,
    #[get = "pub"]
    pub(super) else_expression: Option<Else>,
}

impl SourceElement for IfElse {
    fn span(&self) -> Span {
        self.if_keyword
            .span()
            .join(
                &self
                    .else_expression
                    .as_ref()
                    .map_or(self.then_expression.span(), |else_expression| {
                        else_expression.span()
                    }),
            )
            .unwrap()
    }
}

/// Represents a loop expression syntax tree.
///
/// Syntax Synopsis:
/// ``` txt
/// Loop:
///     LabelSpecifier? 'loop' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Loop {
    #[get = "pub"]
    pub(super) label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    pub(super) loop_keyword: Keyword,
    #[get = "pub"]
    pub(super) expression: Block,
}

impl SourceElement for Loop {
    fn span(&self) -> Span {
        match &self.label_specifier {
            Some(label_specifier) => label_specifier
                .span()
                .join(&self.loop_keyword.span())
                .unwrap()
                .join(&self.expression.span())
                .unwrap(),
            None => self
                .loop_keyword
                .span()
                .join(&self.expression.span())
                .unwrap(),
        }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Continue {
    #[get = "pub"]
    pub(super) continue_keyword: Keyword,
    #[get = "pub"]
    pub(super) label: Option<Label>,
}

impl SourceElement for Continue {
    fn span(&self) -> Span {
        let end = self.label.as_ref().map_or_else(
            || self.continue_keyword.span().clone(),
            |label| label.span(),
        );
        self.continue_keyword.span().join(&end).unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Express {
    #[get = "pub"]
    pub(super) express_keyword: Keyword,
    #[get = "pub"]
    pub(super) label: Option<Label>,
    #[get = "pub"]
    pub(super) expression: Option<Box<Expression>>,
}

impl SourceElement for Express {
    fn span(&self) -> Span {
        let end = self.expression.as_ref().map_or_else(
            || {
                self.label
                    .as_ref()
                    .map_or_else(|| self.express_keyword.span().clone(), |label| label.span())
            },
            |expression| expression.span(),
        );
        self.express_keyword.span().join(&end).unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Break {
    #[get = "pub"]
    pub(super) break_keyword: Keyword,
    #[get = "pub"]
    pub(super) label: Option<Label>,
    #[get = "pub"]
    pub(super) expression: Option<Box<Expression>>,
}

impl SourceElement for Break {
    fn span(&self) -> Span {
        let end = self.expression.as_ref().map_or_else(
            || {
                self.label
                    .as_ref()
                    .map_or_else(|| self.break_keyword.span().clone(), |label| label.span())
            },
            |expression| expression.span(),
        );
        self.break_keyword.span().join(&end).unwrap()
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
#[allow(missing_docs)]
pub struct Return {
    #[get = "pub"]
    pub(super) return_keyword: Keyword,
    #[get = "pub"]
    pub(super) expression: Option<Box<Expression>>,
}

impl SourceElement for Return {
    fn span(&self) -> Span {
        let end = self.expression.as_ref().map_or_else(
            || self.return_keyword.span().clone(),
            |expression| expression.span(),
        );
        self.return_keyword.span().join(&end).unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`Expression`].
    pub fn parse_expression(&mut self) -> Option<Expression> {
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

                    Ordering::Less => (),
                }
            }

            // ASSUMPTION: The assignments have 1 precedence and are right associative.
            assert!(current_precedence > 0);

            if candidate_index == 0 {
                let (binary_operator, right_expression) = expressions.remove(0);

                // Replace the first expression with the folded expression.
                first_expression = Expression::Functional(
                    Binary {
                        left_operand: Box::new(first_expression),
                        binary_operator,
                        right_operand: Box::new(right_expression.unwrap()),
                    }
                    .into(),
                );
            } else {
                let (binary_operator, right_expression) = expressions.remove(candidate_index);

                // Replace the expression at the index with the folded expression.
                expressions[candidate_index - 1].1 = Some(Expression::Functional(
                    Binary {
                        left_operand: Box::new(expressions[candidate_index - 1].1.take().unwrap()),
                        binary_operator,
                        right_operand: Box::new(right_expression.unwrap()),
                    }
                    .into(),
                ));
            }
        }

        Some(first_expression)
    }

    fn try_parse_first_punctuation_binary_operator(&mut self) -> Option<BinaryOperator> {
        let starting_cursor_position = self.cursor.position();
        let next_token = self.next_significant_token();
        match next_token {
            Some(Token::Punctuation(punctuation)) => match punctuation.punctuation() {
                '+' => return Some(BinaryOperator::Add(punctuation.clone())),
                '-' => return Some(BinaryOperator::Subtract(punctuation.clone())),
                '*' => return Some(BinaryOperator::Multiply(punctuation.clone())),
                '/' => return Some(BinaryOperator::Divide(punctuation.clone())),
                '%' => return Some(BinaryOperator::Modulo(punctuation.clone())),
                '=' => return Some(BinaryOperator::Assign(punctuation.clone())),
                '!' => {
                    if let Some(Token::Punctuation(punctuation1)) = self.peek_significant_token() {
                        if punctuation1.punctuation() == '=' {
                            self.next_significant_token();
                            return Some(BinaryOperator::NotEqual(
                                punctuation.clone(),
                                punctuation1.clone(),
                            ));
                        }
                    }
                }
                '<' => return Some(BinaryOperator::LessThan(punctuation.clone())),
                '>' => return Some(BinaryOperator::GreaterThan(punctuation.clone())),
                _ => (),
            },
            Some(Token::Keyword(and_keyword)) if and_keyword.keyword() == KeywordKind::And => {
                return Some(BinaryOperator::LogicalAnd(and_keyword.clone()))
            }
            Some(Token::Keyword(or_keyword)) if or_keyword.keyword() == KeywordKind::Or => {
                return Some(BinaryOperator::LogicalOr(or_keyword.clone()))
            }
            _ => (),
        }
        self.cursor.set_position(starting_cursor_position);
        None
    }

    fn try_parse_second_punctuation_binary_operator(
        &mut self,
        first_punctuation_binary_operator: BinaryOperator,
    ) -> BinaryOperator {
        let starting_cursor_position = self.cursor.position();

        match self.next_significant_token() {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation() == '=' => {
                match first_punctuation_binary_operator {
                    BinaryOperator::Add(prev_punctuation) => {
                        BinaryOperator::CompoundAdd(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::Subtract(prev_punctuation) => {
                        BinaryOperator::CompoundSubtract(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::Multiply(prev_punctuation) => {
                        BinaryOperator::CompoundMultiply(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::Divide(prev_punctuation) => {
                        BinaryOperator::CompoundDivide(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::Modulo(prev_punctuation) => {
                        BinaryOperator::CompoundModulo(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::Assign(prev_punctuation) => {
                        BinaryOperator::Equal(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::LessThan(prev_punctuation) => {
                        BinaryOperator::LessThanOrEqual(prev_punctuation, punctuation.clone())
                    }
                    BinaryOperator::GreaterThan(prev_punctuation) => {
                        BinaryOperator::GreaterThanOrEqual(prev_punctuation, punctuation.clone())
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

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_punctuation_binary_operator =
            self.try_parse_first_punctuation_binary_operator()?;
        Some(self.try_parse_second_punctuation_binary_operator(first_punctuation_binary_operator))
    }

    pub(super) fn parse_block_without_label(&mut self) -> Option<BlockWithoutLabel> {
        let left_brace = self.expect_punctuation('{')?;

        let mut statements = Vec::new();

        // Parses statements until a right brace is found.
        let right_brace = loop {
            match self.peek_significant_token() {
                Some(Token::Punctuation(punc)) if punc.punctuation() == '}' => {
                    self.next_token();
                    break punc;
                }

                None => {
                    self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                        expected: '}',
                        found: None,
                    }));
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
                                    if punc.punctuation() == ';' || punc.punctuation() == '}'
                            )
                        });

                        match token {
                            Some(Token::Punctuation(punc)) if punc.punctuation() == '}' => {
                                break punc
                            }
                            None => {
                                self.report_error(SyntacticError::PunctuationExpected(
                                    PunctuationExpected {
                                        expected: '}',
                                        found: None,
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

        Some(BlockWithoutLabel {
            left_brace: left_brace.clone(),
            statements,
            right_brace: right_brace.clone(),
        })
    }

    // Parses either a block expression or a loop expression.
    fn parse_block_or_loop_expression(
        &mut self,
        label_specifier: Option<LabelSpecifier>,
    ) -> Option<Expression> {
        match self.peek_significant_token() {
            // Handles loop
            Some(Token::Keyword(loop_keyword)) if loop_keyword.keyword() == KeywordKind::Loop => {
                self.next_token();

                let expression = self.parse_block()?;

                Some(Expression::Imperative(Imperative::Loop(Loop {
                    label_specifier: None,
                    loop_keyword: loop_keyword.clone(),
                    expression,
                })))
            }
            // Handles block
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation() == '{' => {
                let block_without_label = self.parse_block_without_label()?;
                Some(Expression::Imperative(Imperative::Block(Block {
                    label_specifier,
                    block_without_label,
                })))
            }
            token => {
                self.report_error(
                    ExpressionExpected {
                        found: token.cloned(),
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn parse_block(&mut self) -> Option<Block> {
        let label_specifier = if matches!(self.peek_significant_token(), Some(Token::Punctuation(punc)) if punc.punctuation() == '\'')
        {
            let single_quote = self.expect_punctuation('\'')?;
            let identifier = self.expect_identifier()?;
            let colon = self.expect_punctuation(':')?;
            Some(LabelSpecifier {
                colon: colon.clone(),
                label: Label {
                    single_quote: single_quote.clone(),
                    identifier: identifier.clone(),
                },
            })
        } else {
            None
        };

        let block = self.parse_block_without_label()?;
        Some(Block {
            label_specifier,
            block_without_label: block,
        })
    }

    fn handle_if_keyword(&mut self, if_keyword: Keyword) -> Option<Expression> {
        let left_paren = self.expect_punctuation('(')?;
        let condition = self.parse_expression()?;
        let right_paren = self.expect_punctuation(')')?;
        let then_expression = self.parse_block()?;

        // Parses an else expression if it exists.
        let else_expression = match self.peek_significant_token() {
            Some(Token::Keyword(else_keyword)) if else_keyword.keyword() == KeywordKind::Else => {
                self.next_token();
                let block_of_if_else = match self.peek_significant_token().cloned() {
                    Some(Token::Keyword(if_keyword)) if if_keyword.keyword() == KeywordKind::If => {
                        self.next_token();

                        BlockOrIfElse::IfElse(
                            self.handle_if_keyword(if_keyword)?
                                .into_imperative()
                                .unwrap()
                                .into_if_else()
                                .unwrap(),
                        )
                    }
                    _ => BlockOrIfElse::Block(self.parse_block()?),
                };

                Some(Else {
                    else_keyword: else_keyword.clone(),
                    expression: Box::new(block_of_if_else),
                })
            }
            _ => None,
        };

        Some(Expression::Imperative(Imperative::IfElse(IfElse {
            if_keyword,
            left_paren: left_paren.clone(),
            condition: Box::new(condition),
            right_paren: right_paren.clone(),
            then_expression,
            else_expression,
        })))
    }

    fn handle_identifier(&mut self) -> Option<Expression> {
        let qualified_identifier = self.parse_qualified_identifier()?;

        match self.peek_significant_token() {
            // Function call
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation() == '(' => {
                // eat the left parenthesis
                self.next_token();

                let (arguments, right_paren) = self
                    .parse_enclosed_list(')', ',', |this| this.parse_expression().map(Box::new))?;

                Some(Expression::Functional(
                    FunctionCall {
                        qualified_identifier,
                        left_paren: left_paren.clone(),
                        arguments,
                        right_paren,
                    }
                    .into(),
                ))
            }

            // Struct literal
            Some(Token::Punctuation(left_brace)) if left_brace.punctuation() == '{' => {
                // eat the left brace
                self.next_token();

                let (field_initializations, right_brace) =
                    self.parse_enclosed_list('}', ',', |this| {
                        let identifier = this.expect_identifier()?;

                        let colon = this.expect_punctuation(':')?;

                        let expression = this.parse_expression()?;

                        Some(FieldInitializer {
                            identifier: identifier.clone(),
                            colon: colon.clone(),
                            expression: Box::new(expression),
                        })
                    })?;

                Some(Expression::Functional(
                    StructLiteral {
                        qualified_identifier,
                        left_brace: left_brace.clone(),
                        field_initializations,
                        right_brace,
                    }
                    .into(),
                ))
            }

            // Simple identifier expression
            _ => Some(Expression::Functional(
                Named {
                    qualified_identifier,
                }
                .into(),
            )),
        }
    }

    fn try_parse_label(&mut self) -> Option<Label> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(single_quote)) if single_quote.punctuation() == '\'' => {
                // eat the single quote
                self.next_token();

                let identifier = self.expect_identifier()?.clone();

                Some(Label {
                    single_quote: single_quote.clone(),
                    identifier,
                })
            }
            _ => None,
        }
    }

    fn try_parse_expression_for_control_expression(&mut self) -> Option<Box<Expression>> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(semicolon)) if semicolon.punctuation() == ';' => None,
            _ => {
                let current_position = self.cursor.position();

                if self.try_parse_binary_operator().is_some() {
                    self.cursor.set_position(current_position);
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                }
            }
        }
    }

    fn handle_parenthesized(&mut self, left_paren: Punctuation) -> Option<Expression> {
        let expression = self.parse_expression()?;
        let right_paren = self.expect_punctuation(')')?;

        Some(Expression::Functional(
            Parenthesized {
                left_paren,
                expression: Box::new(expression),
                right_paren: right_paren.clone(),
            }
            .into(),
        ))
    }

    // Parses an primary expression without any prefix operators.
    fn parse_primary_expression_raw(&mut self) -> Option<Expression> {
        match self.peek_significant_token() {
            // Handles if expressions
            Some(Token::Keyword(if_keyword)) if if_keyword.keyword() == KeywordKind::If => {
                self.next_token();
                self.handle_if_keyword(if_keyword.clone())
            }

            // Handles numeric literal
            Some(Token::NumericLiteral(numeric_literal)) => {
                self.next_token();
                Some(Expression::Functional((numeric_literal.clone()).into()))
            }

            // Handles parenthesis
            Some(Token::Punctuation(left_paren)) if left_paren.punctuation() == '(' => {
                self.next_token();
                self.handle_parenthesized(left_paren.clone())
            }

            // Handles label specifier
            Some(Token::Punctuation(single_quote)) if single_quote.punctuation() == '\'' => {
                self.next_token();

                let name = self.expect_identifier()?;
                let colon = self.expect_punctuation(':')?;

                let label = LabelSpecifier {
                    label: Label {
                        single_quote: single_quote.clone(),
                        identifier: name.clone(),
                    },
                    colon: colon.clone(),
                };

                self.parse_block_or_loop_expression(Some(label))
            }

            // Handle continue expression
            Some(Token::Keyword(continue_keyword))
                if continue_keyword.keyword() == KeywordKind::Continue =>
            {
                // eat the continue keyword
                self.next_token();

                let label = self.try_parse_label();

                Some(Expression::Functional(
                    Continue {
                        continue_keyword: continue_keyword.clone(),
                        label,
                    }
                    .into(),
                ))
            }

            // Handle break expression
            Some(Token::Keyword(break_keyword))
                if break_keyword.keyword() == KeywordKind::Break =>
            {
                // eat the break keyword
                self.next_token();

                let label = self.try_parse_label();
                let expression = self.try_parse_expression_for_control_expression();

                Some(Expression::Functional(
                    Break {
                        break_keyword: break_keyword.clone(),
                        label,
                        expression,
                    }
                    .into(),
                ))
            }

            // Handle express expression
            Some(Token::Keyword(express_keyword))
                if express_keyword.keyword() == KeywordKind::Express =>
            {
                // eat the express keyword
                self.next_token();

                let label = self.try_parse_label();
                let expression = self.try_parse_expression_for_control_expression();

                Some(Expression::Functional(
                    Express {
                        express_keyword: express_keyword.clone(),
                        label,
                        expression,
                    }
                    .into(),
                ))
            }

            // Handles return expression
            Some(Token::Keyword(return_keyword))
                if return_keyword.keyword() == KeywordKind::Return =>
            {
                // eat the return keyword
                self.next_token();

                let expression = self.try_parse_expression_for_control_expression();

                Some(Expression::Functional(
                    Return {
                        return_keyword: return_keyword.clone(),
                        expression,
                    }
                    .into(),
                ))
            }

            // Handles identifier
            Some(Token::Identifier(..)) => self.handle_identifier(),

            // Handles boolean literal
            Some(Token::Keyword(keyword)) if keyword.keyword() == KeywordKind::True => {
                self.next_token();
                Some(Expression::Functional(
                    BooleanLiteral::True(keyword.clone()).into(),
                ))
            }
            Some(Token::Keyword(keyword)) if keyword.keyword() == KeywordKind::False => {
                self.next_token();
                Some(Expression::Functional(
                    BooleanLiteral::False(keyword.clone()).into(),
                ))
            }

            _ => self.parse_block_or_loop_expression(None),
        }
    }

    // Parses a primary expression with prefix operators and postfix operators.
    fn parse_primary_expression(&mut self) -> Option<Expression> {
        match self.peek_significant_token() {
            Some(Token::Punctuation(punc))
                if punc.punctuation() == '!' || punc.punctuation() == '-' =>
            {
                self.next_token();

                let operand = self.parse_primary_expression()?;

                return Some(Expression::Functional(
                    Prefix {
                        prefix_operator: match punc.punctuation() {
                            '!' => PrefixOperator::LogicalNot(punc.clone()),
                            '-' => PrefixOperator::Negate(punc.clone()),
                            _ => unreachable!(),
                        },
                        operand: Box::new(operand),
                    }
                    .into(),
                ));
            }
            _ => (),
        }

        let mut primary_expression = self.parse_primary_expression_raw()?;

        loop {
            match self.peek_significant_token() {
                Some(Token::Punctuation(dot)) if dot.punctuation() == '.' => {
                    self.next_token();

                    let identifier = self.expect_identifier()?.clone();

                    primary_expression = Expression::Functional(
                        MemberAccess {
                            operand: Box::new(primary_expression),
                            dot: dot.clone(),
                            identifier,
                        }
                        .into(),
                    );
                }
                Some(Token::Keyword(as_keyword)) if as_keyword.keyword() == KeywordKind::As => {
                    self.next_token();

                    let type_specifier = self.parse_type_specifier()?;

                    primary_expression = Expression::Functional(
                        Cast {
                            operand: Box::new(primary_expression),
                            as_keyword: as_keyword.clone(),
                            type_specifier,
                        }
                        .into(),
                    );
                }
                _ => break Some(primary_expression),
            }
        }
    }
}

// #[cfg(test)]
// mod tests;
