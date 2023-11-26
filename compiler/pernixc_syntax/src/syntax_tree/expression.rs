use std::cmp::Ordering;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Numeric, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{
    pattern, r#type::Type, statement::Statement, ConnectedList, DelimitedList, Label,
    QualifiedIdentifier, Qualifier,
};
use crate::{
    error::{Error, SyntaxKind, UnexpectedSyntax},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
/// ``` txt
/// Expression:
///     Terminator
///     | Functional
///     | Imperative
///     ;
///  ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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

/// Syntax Synopsis:
/// ``` txt
/// Terminator:
///     Return
///     | Continue
///     | Express
///     | Break
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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

/// Syntax Synopsis:
/// ``` txt
/// Access:
///     Identifier
///     | Numeric
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum Access {
    Struct(Identifier),
    Tuple(Numeric),
}

impl SourceElement for Access {
    fn span(&self) -> Span {
        match self {
            Self::Struct(identifier) => identifier.span.clone(),
            Self::Tuple(numeric) => numeric.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Dot:
///     Functional '.' Access
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Dot {
    #[get = "pub"]
    pub(super) operand: Box<Functional>,
    #[get = "pub"]
    pub(super) dot: Punctuation,
    #[get = "pub"]
    pub(super) access: Access,
}

impl SourceElement for Dot {
    fn span(&self) -> Span { self.operand.span().join(&self.access.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Arrow:
///     Functional '->' Access
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Arrow {
    #[get = "pub"]
    pub(super) operand: Box<Functional>,
    #[get = "pub"]
    pub(super) hyphen: Punctuation,
    #[get = "pub"]
    pub(super) right_angle_bracket: Punctuation,
    #[get = "pub"]
    pub(super) access: Access,
}

impl SourceElement for Arrow {
    fn span(&self) -> Span { self.operand.span().join(&self.access.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Copy:
///     Functional '\''
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Copy {
    #[get = "pub"]
    pub(super) operand: Box<Functional>,
    #[get = "pub"]
    pub(super) single_quote: Punctuation,
}

impl SourceElement for Copy {
    fn span(&self) -> Span { self.operand.span().join(&self.single_quote.span).unwrap() }
}

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
///     | ArrayLiteral
///     | Subscript
///     | Dot
///     | Cast
///     | Copy
///     | Arrow
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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
    Subscript(Subscript),
    ArrayLiteral(ArrayLiteral),
    Copy(Copy),
    Cast(Cast),
    Dot(Dot),
    Arrow(Arrow),
}

/// Syntax Synopsis:
/// ``` txt
/// Subscript:
///     Functional '[' Expression ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Subscript {
    #[get = "pub"]
    operand: Box<Functional>,
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Subscript {
    fn span(&self) -> Span { self.operand.span().join(&self.right_bracket.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// ArrayLiteral:
///     '[' ArgumentList? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ArrayLiteral {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    arguments: Option<ArgumentList>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for ArrayLiteral {
    fn span(&self) -> Span {
        self.left_bracket
            .span
            .join(&self.right_bracket.span)
            .unwrap()
    }
}

impl SourceElement for Functional {
    fn span(&self) -> Span {
        match self {
            Self::NumericLiteral(numeric_literal) => numeric_literal.numeric.span.clone(),
            Self::BooleanLiteral(boolean_literal) => boolean_literal.span(),
            Self::Cast(cast) => cast.span(),
            Self::Binary(binary_expression) => binary_expression.span(),
            Self::Prefix(prefix_expression) => prefix_expression.span(),
            Self::Named(identifier_expression) => identifier_expression.span(),
            Self::FunctionCall(function_call_expression) => function_call_expression.span(),
            Self::Parenthesized(parenthesized_expression) => parenthesized_expression.span(),
            Self::Subscript(subscript_expression) => subscript_expression.span(),
            Self::StructLiteral(struct_literal) => struct_literal.span(),
            Self::ArrayLiteral(array_literal) => array_literal.span(),
            Self::Copy(copy) => copy.span(),
            Self::Dot(dot) => dot.span(),
            Self::Arrow(arrow) => arrow.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Decimal:
///     '.' Numeric
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Decimal {
    #[get = "pub"]
    pub(super) dot: Punctuation,
    #[get = "pub"]
    pub(super) numeric: Numeric,
}

impl SourceElement for Decimal {
    fn span(&self) -> Span { self.dot.span().join(&self.numeric.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// NumericLiteral:
///     NumericLiteralToken
///     ;
/// ````
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct NumericLiteral {
    #[get = "pub"]
    pub(super) numeric: Numeric,
    #[get = "pub"]
    pub(super) decimal: Option<Decimal>,
    #[get = "pub"]
    pub(super) suffix: Option<Identifier>,
}

impl SourceElement for NumericLiteral {
    fn span(&self) -> Span {
        let end = self.suffix.as_ref().map_or_else(
            || {
                self.decimal
                    .as_ref()
                    .map_or_else(|| self.numeric.span.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        self.numeric.span().join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// CastExpression:
///     Functional 'as' '(' Type ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Cast {
    #[get = "pub"]
    operand: Box<Functional>,
    #[get = "pub"]
    as_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    ty: Type,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for Cast {
    fn span(&self) -> Span { self.operand.span().join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// BooleanLiteral:
///     'true'
///     | 'false'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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

/// Syntax Synopsis:
/// ``` txt
/// Binary:
///     Functional BinaryOperator Functional
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
///     | ':='
///     | 'and'
///     | 'or'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
    RestrictAssign(Punctuation, Punctuation),
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
            | Self::RestrictAssign(..)
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
            | Self::RestrictAssign(token, token1)
            | Self::GreaterThanOrEqual(token, token1) => token.span().join(&token1.span).unwrap(),
            Self::LogicalAnd(token) | Self::LogicalOr(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:    
///
/// ``` txt
/// ReferenceOfKind:
///     '&'
///     | '@'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferenceOfKind {
    Local(Punctuation),
    Regular(Punctuation),
}

impl SourceElement for ReferenceOfKind {
    fn span(&self) -> Span {
        match self {
            Self::Local(p) | Self::Regular(p) => p.span.clone(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// ReferenceOf:
///     ReferenceOfKind Qualifier?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ReferenceOf {
    #[get = "pub"]
    kind: ReferenceOfKind,
    #[get = "pub"]
    qualifier: Option<Qualifier>,
}

impl SourceElement for ReferenceOf {
    fn span(&self) -> Span {
        self.qualifier.as_ref().map_or_else(
            || self.kind.span(),
            |qualifier| self.kind.span().join(&qualifier.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// PrefixOperator:
///     '!'
///     | '-'
///     | ReferenceOf
///     | '*'
///     | 'local'
///     | 'unlocal'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
    ReferenceOf(ReferenceOf),
    Dereference(Punctuation),
    Local(Keyword),
    Unlocal(Keyword),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(p) | Self::Negate(p) | Self::Dereference(p) => p.span.clone(),
            Self::ReferenceOf(p) => p.span(),
            Self::Local(k) | Self::Unlocal(k) => k.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Prefix:
///     PrefixOperator Functional
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// Named:
///     QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Named {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for Named {
    fn span(&self) -> Span { self.qualified_identifier.span() }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpackable:
///     '...'? Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Unpackable {
    #[get = "pub"]
    ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    expression: Box<Expression>,
}

impl SourceElement for Unpackable {
    fn span(&self) -> Span {
        match &self.ellipsis {
            Some((start, ..)) => start.span().join(&self.expression.span()).unwrap(),
            None => self.expression.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ArgumentList:
///     Expression (',' Expression)*
///     ;
/// ```
pub type ArgumentList = ConnectedList<Box<Expression>, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// FunctionCall:
///     QualifiedIdentifier '(' ArgumentList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// Parenthesized:
///     '(' (Unpackable (',' Unpackable)* ','? )? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Parenthesized {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    expression: Option<ConnectedList<Unpackable, Punctuation>>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Span { self.left_paren.span().join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// FieldInitializer:
///     Identifier ':' Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// FieldInitializeList:
///     FieldInitializer (',' FieldInitializer)*
///     ;
/// ```
pub type FieldInitializerList = ConnectedList<FieldInitializer, Punctuation>;

/// Syntax Synopsis:
/// ``` txt
/// StructLiteral:
///     QualifiedIdentifier '{' FieldInitializerList? '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Imperative {
    Block(Block),
    IfElse(IfElse),
    Loop(Loop),
    Match(Match),
}

impl SourceElement for Imperative {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::IfElse(if_else) => if_else.span(),
            Self::Loop(loop_) => loop_.span(),
            Self::Match(match_) => match_.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ArmGuard:
///     'if' '(' Expression ')'
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MatchArmGuard {
    #[get = "pub"]
    if_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for MatchArmGuard {
    fn span(&self) -> Span { self.if_keyword.span.join(&self.right_paren.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// MatchArm:
///     RefutablePattern ArmGuard? ':' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MatchArm {
    #[get = "pub"]
    refutable_pattern: pattern::Refutable,
    #[get = "pub"]
    guard: Option<MatchArmGuard>,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    block: Block,
}

impl SourceElement for MatchArm {
    fn span(&self) -> Span {
        self.refutable_pattern
            .span()
            .join(&self.block.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Match:
///     'match' '(' Expression ')' '{' MatchArm* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Match {
    #[get = "pub"]
    match_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_paren: Punctuation,
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    arms: Vec<MatchArm>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Match {
    fn span(&self) -> Span {
        self.match_keyword
            .span
            .join(&self.right_brace.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// LabelSpecifier:
///     Label ':'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// Statements:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Statements {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    statements: Vec<Statement>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Statements {
    fn span(&self) -> Span { self.left_brace.span().join(&self.right_brace.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Block:
///     LabelSpecifier? 'unsafe'? Statements*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Block {
    #[get = "pub"]
    label_specifier: Option<LabelSpecifier>,
    #[get = "pub"]
    unsafe_keyword: Option<Keyword>,
    #[get = "pub"]
    statements: Statements,
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        let start = self.label_specifier.as_ref().map_or_else(
            || {
                self.unsafe_keyword
                    .as_ref()
                    .map_or_else(|| self.statements.span(), SourceElement::span)
            },
            SourceElement::span,
        );
        let end = self.statements.span();
        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// BlockOrIfElse:
///     Block
///     | IfElse
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
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

/// Syntax Synopsis:
/// ``` txt
/// Else:
///     'else' BlockOrIfElse
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// IfElse:
///     'if' '(' Expression ')' Block Else?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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

/// Syntax Synopsis:
/// ``` txt
/// Loop:
///     'loop' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Loop {
    #[get = "pub"]
    loop_keyword: Keyword,
    #[get = "pub"]
    block: Block,
}

impl SourceElement for Loop {
    fn span(&self) -> Span { self.loop_keyword.span.join(&self.block.span()).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Continue:
///     'continue' Label?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
        let end = self
            .label
            .as_ref()
            .map_or_else(|| self.continue_keyword.span.clone(), SourceElement::span);

        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Express:
///     'express' Label? Functional?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
            SourceElement::span,
        );

        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Break:
///     'break' Label? Functional?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
                self.expression
                    .as_ref()
                    .map_or_else(|| self.break_keyword.span.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        start.join(&end).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Return:
///     'return' Functional?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
        let end = self
            .expression
            .as_ref()
            .map_or_else(|| self.return_keyword.span.clone(), SourceElement::span);

        start.join(&end).unwrap()
    }
}

impl<'a> Parser<'a> {
    fn parse_binary_expression(&mut self, handler: &dyn Handler<Error>) -> Option<Functional> {
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

    fn parse_block(&mut self, handler: &dyn Handler<Error>) -> Option<Block> {
        let label_specifier = match self.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                // eat apostrophe
                self.forward();

                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;

                Some(LabelSpecifier {
                    label: Label {
                        apostrophe,
                        identifier,
                    },
                    colon,
                })
            }
            _ => None,
        };

        let unsafe_keyword = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(unsafe_keyword))
                if unsafe_keyword.keyword == KeywordKind::Unsafe =>
            {
                self.forward();
                Some(unsafe_keyword)
            }
            _ => None,
        };
        let statements = self.parse_statements(handler)?;

        // parse block
        Some(Block {
            label_specifier,
            unsafe_keyword,
            statements,
        })
    }

    pub fn try_parse_functional(&mut self) -> Option<Functional> {
        self.try_parse(|parser| parser.parse_binary_expression(&Dummy))
    }

    /// Parses an [`Expression`]
    #[allow(clippy::missing_errors_doc)]
    pub fn parse_expression(&mut self, handler: &dyn Handler<Error>) -> Option<Expression> {
        match self.stop_at_significant() {
            // parse return expression
            Reading::Atomic(Token::Keyword(return_keyword))
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
            Reading::Atomic(Token::Keyword(continue_keyword))
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
            Reading::Atomic(Token::Keyword(break_keyword))
                if break_keyword.keyword == KeywordKind::Break =>
            {
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
            Reading::Atomic(Token::Keyword(express_keyword))
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

            // parse match expression
            Reading::Atomic(Token::Keyword(match_keyword))
                if match_keyword.keyword == KeywordKind::Match =>
            {
                self.parse_match(handler)
                    .map(|x| Expression::Imperative(Imperative::Match(x)))
            }

            // parse if else expression
            Reading::Atomic(Token::Keyword(if_keyword))
                if if_keyword.keyword == KeywordKind::If =>
            {
                self.parse_if_else(handler)
                    .map(|x| Expression::Imperative(Imperative::IfElse(x)))
            }

            Reading::Atomic(Token::Keyword(unsafe_keyword))
                if unsafe_keyword.keyword == KeywordKind::Unsafe =>
            {
                self.parse_block(handler)
                    .map(|x| Expression::Imperative(Imperative::Block(x)))
            }
            Reading::Atomic(Token::Punctuation(apostrophe)) if apostrophe.punctuation == '\'' => {
                self.parse_block(handler)
                    .map(|x| Expression::Imperative(Imperative::Block(x)))
            }
            Reading::IntoDelimited(left_brace) if left_brace.punctuation == '{' => self
                .parse_block(handler)
                .map(|x| Expression::Imperative(Imperative::Block(x))),
            Reading::Atomic(Token::Keyword(loop_keyword))
                if loop_keyword.keyword == KeywordKind::Loop =>
            {
                // eat loop
                self.forward();

                let block = self.parse_block(handler)?;

                Some(Expression::Imperative(Imperative::Loop(Loop {
                    loop_keyword,
                    block,
                })))
            }

            _ => self
                .parse_binary_expression(handler)
                .map(Expression::Functional),
        }
    }

    fn parse_label(&mut self, handler: &dyn Handler<Error>) -> Option<Label> {
        let apostrophe = self.parse_punctuation('\'', true, handler)?;
        let identifier = self.parse_identifier(handler)?;

        Some(Label {
            apostrophe,
            identifier,
        })
    }

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_level = self.try_parse(|parser| match parser.next_significant_token() {
            Reading::Atomic(Token::Punctuation(p)) => match p.punctuation {
                '+' => Some(BinaryOperator::Add(p)),
                '-' => Some(BinaryOperator::Subtract(p)),
                '*' => Some(BinaryOperator::Multiply(p)),
                '/' => Some(BinaryOperator::Divide(p)),
                '%' => Some(BinaryOperator::Modulo(p)),
                '=' => Some(BinaryOperator::Assign(p)),
                ':' => {
                    let equal = parser.parse_punctuation('=', false, &Dummy)?;
                    Some(BinaryOperator::RestrictAssign(p, equal))
                }
                '!' => {
                    let equal = parser.parse_punctuation('=', false, &Dummy)?;
                    Some(BinaryOperator::NotEqual(p, equal))
                }
                '>' => Some(BinaryOperator::GreaterThan(p)),
                '<' => Some(BinaryOperator::LessThan(p)),
                _ => None,
            },
            Reading::Atomic(Token::Keyword(k)) => match k.keyword {
                KeywordKind::And => Some(BinaryOperator::LogicalAnd(k)),
                KeywordKind::Or => Some(BinaryOperator::LogicalOr(k)),
                _ => None,
            },
            _ => None,
        })?;

        Some(
            self.try_parse(|parser| match (first_level.clone(), parser.next_token()) {
                (first_level, Reading::Atomic(Token::Punctuation(n))) => {
                    match (first_level, n.punctuation) {
                        (BinaryOperator::Add(p), '=') => Some(BinaryOperator::CompoundAdd(p, n)),
                        (BinaryOperator::Subtract(p), '=') => {
                            Some(BinaryOperator::CompoundSubtract(p, n))
                        }
                        (BinaryOperator::Multiply(p), '=') => {
                            Some(BinaryOperator::CompoundMultiply(p, n))
                        }
                        (BinaryOperator::Divide(p), '=') => {
                            Some(BinaryOperator::CompoundDivide(p, n))
                        }
                        (BinaryOperator::Modulo(p), '=') => {
                            Some(BinaryOperator::CompoundModulo(p, n))
                        }
                        (BinaryOperator::Assign(p), '=') => Some(BinaryOperator::Equal(p, n)),
                        (BinaryOperator::GreaterThan(p), '=') => {
                            Some(BinaryOperator::GreaterThanOrEqual(p, n))
                        }
                        (BinaryOperator::LessThan(p), '=') => {
                            Some(BinaryOperator::LessThanOrEqual(p, n))
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
            .unwrap_or(first_level),
        )
    }

    fn try_parse_prefix_operator(&mut self) -> Option<PrefixOperator> {
        self.try_parse(|parser| match parser.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == '!' => {
                parser.forward();
                Some(PrefixOperator::LogicalNot(p))
            }
            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == '-' => {
                parser.forward();
                Some(PrefixOperator::Negate(p))
            }
            Reading::Atomic(Token::Punctuation(p))
                if p.punctuation == '&' || p.punctuation == '@' =>
            {
                parser.forward();
                let kind = match p.punctuation {
                    '&' => ReferenceOfKind::Regular(p),
                    '@' => ReferenceOfKind::Local(p),
                    _ => unreachable!(),
                };

                let qualifier = match parser.stop_at_significant() {
                    Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Mutable => {
                        // eat mutable keyword
                        parser.forward();

                        Some(Qualifier::Mutable(k))
                    }

                    Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Restrict => {
                        // eat immutable keyword
                        parser.forward();

                        Some(Qualifier::Restrict(k))
                    }

                    _ => None,
                };

                Some(PrefixOperator::ReferenceOf(ReferenceOf { kind, qualifier }))
            }

            Reading::Atomic(Token::Keyword(p)) if p.keyword == KeywordKind::Local => {
                parser.forward();
                Some(PrefixOperator::Local(p))
            }

            Reading::Atomic(Token::Keyword(p)) if p.keyword == KeywordKind::Unlocal => {
                parser.forward();
                Some(PrefixOperator::Unlocal(p))
            }

            Reading::Atomic(Token::Punctuation(p)) if p.punctuation == '*' => {
                parser.forward();
                Some(PrefixOperator::Dereference(p))
            }

            _ => None,
        })
    }

    fn handle_struct_literal(
        &mut self,
        qualified_identifier: QualifiedIdentifier,
        handler: &dyn Handler<Error>,
    ) -> Option<Functional> {
        let DelimitedList {
            open: left_brace,
            list: field_initializers,
            close: right_brace,
        } = self.parse_enclosed_list(
            Delimiter::Brace,
            ',',
            |this| {
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
        handler: &dyn Handler<Error>,
    ) -> Option<Functional> {
        let DelimitedList {
            open: left_paren,
            list: arguments,
            close: right_paren,
        } = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |this| Some(Box::new(this.parse_expression(handler)?)),
            handler,
        )?;

        Some(Functional::FunctionCall(FunctionCall {
            qualified_identifier,
            left_paren,
            arguments,
            right_paren,
        }))
    }

    fn parse_identifier_expression(&mut self, handler: &dyn Handler<Error>) -> Option<Functional> {
        let qualified_identifier = self.parse_qualified_identifier(true, handler)?;

        match self.stop_at_significant() {
            Reading::IntoDelimited(p) if p.punctuation == '(' => {
                self.handle_function_call(qualified_identifier, handler)
            }

            Reading::IntoDelimited(p) if p.punctuation == '{' => {
                self.handle_struct_literal(qualified_identifier, handler)
            }

            _ => Some(Functional::Named(Named {
                qualified_identifier,
            })),
        }
    }

    fn parse_parenthesized_expression(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Functional> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                parser.stop_at_significant();

                let ellipsis = match (parser.peek(), parser.peek_offset(1), parser.peek_offset(2)) {
                    (
                        Reading::Atomic(Token::Punctuation(p1)),
                        Some(Reading::Atomic(Token::Punctuation(p2))),
                        Some(Reading::Atomic(Token::Punctuation(p3))),
                    ) if matches!(
                        (p1.punctuation, p2.punctuation, p3.punctuation),
                        ('.', '.', '.')
                    ) =>
                    {
                        // eat the three dots
                        parser.forward();
                        parser.forward();
                        parser.forward();

                        Some((p1, p2, p3))
                    }
                    _ => None,
                };

                let expression = Box::new(parser.parse_expression(handler)?);

                Some(Unpackable {
                    ellipsis,
                    expression,
                })
            },
            handler,
        )?;

        Some(Functional::Parenthesized(Parenthesized {
            left_paren: enclosed_tree.open,
            expression: enclosed_tree.list,
            right_paren: enclosed_tree.close,
        }))
    }

    fn parse_array_literal_expression(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Functional> {
        let enclosed_tree = self.parse_enclosed_list(
            Delimiter::Bracket,
            ',',
            |parser| parser.parse_expression(handler).map(Box::new),
            handler,
        )?;

        Some(Functional::ArrayLiteral(ArrayLiteral {
            left_bracket: enclosed_tree.open,
            arguments: enclosed_tree.list,
            right_bracket: enclosed_tree.close,
        }))
    }

    fn parse_statements(&mut self, handler: &dyn Handler<Error>) -> Option<Statements> {
        fn skip_to_next_statement(this: &mut Parser) {
            this.stop_at(|token| matches!(token, Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';'));

            if matches!(this.peek(), Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ';')
            {
                this.forward();
            }
        }

        let delimited_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    // parse the statement
                    let Some(statement) = parser.parse_statement(handler) else {
                        skip_to_next_statement(parser);
                        continue;
                    };

                    statements.push(statement);
                }

                Some(statements)
            },
            handler,
        )?;

        Some(Statements {
            left_brace: delimited_tree.open,
            statements: delimited_tree.tree?,
            right_brace: delimited_tree.close,
        })
    }

    fn parse_else(&mut self, handler: &dyn Handler<Error>) -> Option<Else> {
        let else_keyword = self.parse_keyword(KeywordKind::Else, handler)?;
        let expression = Box::new(
            if matches!(self.stop_at_significant(), Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::If)
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

    fn parse_match_arm(&mut self, handler: &dyn Handler<Error>) -> Option<MatchArm> {
        let refutable_pattern = self.parse_refutable_pattern(handler)?;

        let guard = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(if_keyword))
                if if_keyword.keyword == KeywordKind::If =>
            {
                // eat if keyword
                self.forward();

                let delimited_tree = self.step_into(
                    Delimiter::Parenthesis,
                    |parser| parser.parse_expression(handler).map(Box::new),
                    handler,
                )?;

                Some(MatchArmGuard {
                    if_keyword,
                    left_paren: delimited_tree.open,
                    expression: delimited_tree.tree?,
                    right_paren: delimited_tree.close,
                })
            }
            _ => None,
        };

        let colon = self.parse_punctuation(':', true, handler)?;
        let block = self.parse_block(handler)?;

        Some(MatchArm {
            refutable_pattern,
            guard,
            colon,
            block,
        })
    }

    fn parse_match(&mut self, handler: &dyn Handler<Error>) -> Option<Match> {
        let match_keyword = self.parse_keyword(KeywordKind::Match, handler)?;

        let delimited_tree_expression = self.step_into(
            Delimiter::Parenthesis,
            |parser| parser.parse_expression(handler).map(Box::new),
            handler,
        )?;

        let delimited_tree_arms = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut arms = Vec::new();

                while !parser.is_exhausted() {
                    let Some(arm) = parser.parse_match_arm(handler) else {
                        // forward to the next {}
                        parser.stop_at(|token| {
                            matches!(
                                token, Reading::Atomic(Token::Punctuation(p))
                                if p.punctuation == '{'
                            )
                        });
                        parser.forward();

                        continue;
                    };

                    arms.push(arm);
                }

                Some(arms)
            },
            handler,
        )?;

        Some(Match {
            match_keyword,
            left_paren: delimited_tree_expression.open,
            expression: delimited_tree_expression.tree?,
            right_paren: delimited_tree_expression.close,
            left_brace: delimited_tree_arms.open,
            arms: delimited_tree_arms.tree?,
            right_brace: delimited_tree_arms.close,
        })
    }

    fn parse_if_else(&mut self, handler: &dyn Handler<Error>) -> Option<IfElse> {
        let if_keyword = self.parse_keyword(KeywordKind::If, handler)?;

        let delimited_tree_condition = self.step_into(
            Delimiter::Parenthesis,
            |parser| parser.parse_expression(handler).map(Box::new),
            handler,
        )?;

        let then_expression = self.parse_block(handler)?;
        let else_expression = if matches!(
            self.stop_at_significant(),
            Reading::Atomic(Token::Keyword(else_keyword))
                if else_keyword.keyword == KeywordKind::Else
        ) {
            Some(self.parse_else(handler)?)
        } else {
            None
        };

        Some(IfElse {
            if_keyword,
            left_paren: delimited_tree_condition.open,
            condition: delimited_tree_condition.tree?,
            right_paren: delimited_tree_condition.close,
            then_expression,
            else_expression,
        })
    }

    #[allow(clippy::option_option)]
    fn try_parse_label(&mut self, handler: &dyn Handler<Error>) -> Option<Option<Label>> {
        // parse optional label
        Some(
            if matches!(
                self.stop_at_significant(),
                Reading::Atomic(Token::Punctuation(p)) if p.punctuation == '\''
            ) {
                Some(self.parse_label(handler)?)
            } else {
                None
            },
        )
    }

    pub(super) fn parse_numeric_literal(&mut self) -> Option<NumericLiteral> {
        let Reading::Atomic(Token::Numeric(numeric)) = self.next_significant_token() else {
            return None;
        };

        let decimal = match (self.peek(), self.peek_offset(1)) {
            (
                Reading::Atomic(Token::Punctuation(dot)),
                Some(Reading::Atomic(Token::Numeric(numeric))),
            ) if dot.punctuation == '.' => {
                self.forward();
                self.forward();
                Some(Decimal { dot, numeric })
            }
            _ => None,
        };

        let suffix = if let Reading::Atomic(Token::Identifier(identifier)) = self.peek() {
            self.forward();
            Some(identifier)
        } else {
            None
        };

        Some(NumericLiteral {
            numeric,
            decimal,
            suffix,
        })
    }

    fn parse_access(&mut self, handler: &dyn Handler<Error>) -> Option<Access> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Numeric(numeric)) => {
                // eat numeric token
                self.forward();

                Some(Access::Tuple(numeric))
            }
            _ => Some(Access::Struct(self.parse_identifier(handler)?)),
        }
    }

    /// Parses a primary [`Expression`]
    #[allow(clippy::missing_errors_doc, clippy::too_many_lines)]
    pub fn parse_primary_expression(&mut self, handler: &dyn Handler<Error>) -> Option<Functional> {
        // early return for prefix expression
        if let Some(prefix_operator) = self.try_parse_prefix_operator() {
            return Some(Functional::Prefix(Prefix {
                operator: prefix_operator,
                operand: Box::new(self.parse_primary_expression(handler)?),
            }));
        }

        let mut expression = match self.stop_at_significant() {
            // parse numeric literal
            Reading::Atomic(Token::Numeric(_)) => {
                Functional::NumericLiteral(self.parse_numeric_literal()?)
            }

            // parse boolean literal
            Reading::Atomic(Token::Keyword(boolean))
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

            Reading::IntoDelimited(p) if p.punctuation == '[' => {
                self.parse_array_literal_expression(handler)?
            }

            // parse qualified identifier expression
            Reading::Atomic(Token::Identifier(..)) => self.parse_identifier_expression(handler)?,
            Reading::Atomic(Token::Punctuation(p))
                if p.punctuation == ':'
                    && self.peek_offset(1).map_or(
                        false,
                        |x| matches!(x, Reading::Atomic(Token::Punctuation(p)) if p.punctuation == ':'),
                    ) =>
            {
                self.parse_identifier_expression(handler)?
            }

            // parenthesized
            Reading::IntoDelimited(p) if p.punctuation == '(' => {
                self.parse_parenthesized_expression(handler)?
            }

            found => {
                // forward/make progress
                self.forward();
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Expression,
                    found: found.into_token(),
                }));
                return None;
            }
        };

        loop {
            match self.stop_at_significant() {
                // parse dot op
                Reading::Atomic(Token::Punctuation(dot)) if dot.punctuation == '.' => {
                    // eat token
                    self.forward();
                    let access = self.parse_access(handler)?;

                    expression = Functional::Dot(Dot {
                        operand: Box::new(expression),
                        dot,
                        access,
                    });
                }

                // parse copy
                Reading::Atomic(Token::Punctuation(single_quote))
                    if single_quote.punctuation == '\'' =>
                {
                    // eat token
                    self.forward();

                    expression = Functional::Copy(Copy {
                        operand: Box::new(expression),
                        single_quote,
                    });
                }

                // parse arrow op
                Reading::Atomic(Token::Punctuation(hyphen))
                    if hyphen.punctuation == '-'
                        && matches!(
                            self.peek_offset(1),
                            Some(Reading::Atomic(Token::Punctuation(right_angle_bracket)))
                                if right_angle_bracket.punctuation == '>'
                        ) =>
                {
                    // eat token
                    self.forward();

                    let right_angle_bracket = self
                        .parse_punctuation('>', false, handler)
                        .expect("should be valid");

                    let access = self.parse_access(handler)?;

                    expression = Functional::Arrow(Arrow {
                        operand: Box::new(expression),
                        hyphen,
                        right_angle_bracket,
                        access,
                    });
                }

                Reading::IntoDelimited(p) if p.punctuation == '[' => {
                    let delimited_tree = self.step_into(
                        Delimiter::Bracket,
                        |parser| parser.parse_expression(handler).map(Box::new),
                        handler,
                    )?;

                    // update expression
                    expression = Functional::Subscript(Subscript {
                        operand: Box::new(expression),
                        left_bracket: delimited_tree.open,
                        expression: delimited_tree.tree?,
                        right_bracket: delimited_tree.close,
                    });
                }
                Reading::Atomic(Token::Keyword(as_keyword))
                    if as_keyword.keyword == KeywordKind::As =>
                {
                    // eat token
                    self.forward();

                    let delimited_tree = self.step_into(
                        Delimiter::Parenthesis,
                        |parser| parser.parse_type(handler),
                        handler,
                    )?;

                    // update expression
                    expression = Functional::Cast(Cast {
                        operand: Box::new(expression),
                        as_keyword,
                        left_paren: delimited_tree.open,
                        ty: delimited_tree.tree?,
                        right_paren: delimited_tree.close,
                    });
                }
                _ => break,
            };
        }

        Some(expression)
    }
}

#[cfg(test)]
pub(super) mod tests;
