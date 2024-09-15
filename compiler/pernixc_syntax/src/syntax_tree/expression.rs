//! Contains all definition of expression syntax trees.

#![allow(missing_docs)]

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    handler::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{
        self, Character, Identifier, Keyword, KeywordKind, Punctuation, Token,
    },
    token_stream::Delimiter,
};

use super::{
    pattern::Refutable, r#type::Type, statement::Statement, ConnectedList,
    GenericIdentifier, Label, QualifiedIdentifier, ReferenceOf,
};
use crate::{
    error::{Error, SyntaxKind},
    parser::{Parser, Reading},
};

pub mod strategy;

/// Syntax Synopsis:
///
/// ``` txt
/// Expression:
///     Binary
///     | Terminator
///     | Brace
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Binary(Binary),
    Terminator(Terminator),
    Brace(Brace),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Binary(syn) => syn.span(),
            Self::Terminator(syn) => syn.span(),
            Self::Brace(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Brace:
///     Block
///     | IfElse
///     | Loop
///     | Match
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Brace {
    Block(Block),
    IfElse(IfElse),
    Loop(Loop),
    Match(Match),
    While(While),
}

impl SourceElement for Brace {
    fn span(&self) -> Span {
        match self {
            Self::Block(syn) => syn.span(),
            Self::IfElse(syn) => syn.span(),
            Self::Loop(syn) => syn.span(),
            Self::Match(syn) => syn.span(),
            Self::While(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// LabelSpecifier:
///     Label ':'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
pub struct Statements {
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    statements: Vec<Statement>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Statements {
    fn span(&self) -> Span {
        self.left_brace.span().join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// MatchArm:
///     RefutablePattern ':' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MatchArm {
    #[get = "pub"]
    refutable_pattern: Refutable,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    block: Block,
}

impl SourceElement for MatchArm {
    fn span(&self) -> Span {
        self.refutable_pattern.span().join(&self.block.span()).unwrap()
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
    parenthesized: Parenthesized,
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    arms: Option<ConnectedList<MatchArm, Punctuation>>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Match {
    fn span(&self) -> Span {
        self.match_keyword.span.join(&self.right_brace.span).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Block:
///     LabelSpecifier? 'unsafe'? Statements
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
        self.else_keyword.span().join(&self.expression.span()).unwrap()
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
/// While:
///     'while' '(' Expression ')' Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct While {
    #[get = "pub"]
    while_keyword: Keyword,
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    condition: Box<Expression>,
    #[get = "pub"]
    right_paren: Punctuation,
    #[get = "pub"]
    block: Block,
}

impl SourceElement for While {
    fn span(&self) -> Span {
        self.while_keyword.span().join(&self.block.span()).unwrap()
    }
}

impl While {
    /// Destructs the while into its components
    #[must_use]
    pub fn destruct(
        self,
    ) -> (Keyword, Punctuation, Box<Expression>, Punctuation, Block) {
        (
            self.while_keyword,
            self.left_paren,
            self.condition,
            self.right_paren,
            self.block,
        )
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
    fn span(&self) -> Span {
        self.loop_keyword.span.join(&self.block.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Terminator:
///     Return
///     | Continue
///     | Express
///     | Break
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl SourceElement for Terminator {
    fn span(&self) -> Span {
        match self {
            Self::Return(syn) => syn.span(),
            Self::Continue(syn) => syn.span(),
            Self::Express(syn) => syn.span(),
            Self::Break(syn) => syn.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ```txt
/// Return:
///     'return' Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Return {
    #[get = "pub"]
    return_keyword: Keyword,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SourceElement for Return {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || self.return_keyword.span(),
            |expression| {
                self.return_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Continue:
///     'continue' Label?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Continue {
    #[get = "pub"]
    continue_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
}

impl SourceElement for Continue {
    fn span(&self) -> Span {
        self.label.as_ref().map_or_else(
            || self.continue_keyword.span(),
            |label| self.continue_keyword.span().join(&label.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Express:
///     'express' Label? Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Express {
    #[get = "pub"]
    express_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SourceElement for Express {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.express_keyword.span(),
                    |label| {
                        self.express_keyword.span().join(&label.span()).unwrap()
                    },
                )
            },
            |expression| {
                self.express_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Break:
///     'express' Label? Binary?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Break {
    #[get = "pub"]
    break_keyword: Keyword,
    #[get = "pub"]
    label: Option<Label>,
    #[get = "pub"]
    binary: Option<Binary>,
}

impl SourceElement for Break {
    fn span(&self) -> Span {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.break_keyword.span(),
                    |label| {
                        self.break_keyword.span().join(&label.span()).unwrap()
                    },
                )
            },
            |expression| {
                self.break_keyword.span().join(&expression.span()).unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// BooleanLiteral:
///     'true'
///     | 'false'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Boolean {
    True(Keyword),
    False(Keyword),
}

impl SourceElement for Boolean {
    fn span(&self) -> Span {
        match self {
            Self::True(keyword) | Self::False(keyword) => keyword.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Decimal:
///     '.' NumericToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Decimal {
    #[get = "pub"]
    pub(super) dot: Punctuation,
    #[get = "pub"]
    pub(super) numeric: token::Numeric,
}

impl SourceElement for Decimal {
    fn span(&self) -> Span { self.dot.span().join(&self.numeric.span).unwrap() }
}

/// Syntax Synopsis:
/// ``` txt
/// Numeric:
///     NumericToken Decimal? Identifier?
///     ;
/// ````
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Numeric {
    #[get = "pub"]
    pub(super) numeric: token::Numeric,
    #[get = "pub"]
    pub(super) decimal: Option<Decimal>,
    #[get = "pub"]
    pub(super) suffix: Option<Identifier>,
}

impl SourceElement for Numeric {
    fn span(&self) -> Span {
        let end = self.suffix.as_ref().map_or_else(
            || {
                self.decimal.as_ref().map_or_else(
                    || self.numeric.span.clone(),
                    SourceElement::span,
                )
            },
            SourceElement::span,
        );

        self.numeric.span().join(&end).unwrap()
    }
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
            Some((start, ..)) => {
                start.span().join(&self.expression.span()).unwrap()
            }
            None => self.expression.span(),
        }
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
    fn span(&self) -> Span {
        self.left_paren.span().join(&self.right_paren.span).unwrap()
    }
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
        self.identifier.span().join(&self.expression.span()).unwrap()
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
pub struct Struct {
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    left_brace: Punctuation,
    #[get = "pub"]
    field_initializers: Option<FieldInitializerList>,
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for Struct {
    fn span(&self) -> Span {
        self.qualified_identifier.span().join(&self.right_brace.span).unwrap()
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
/// ArrayLiteral:
///     '[' ArgumentList? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Array {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    arguments: Option<ArgumentList>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Array {
    fn span(&self) -> Span {
        self.left_bracket.span.join(&self.right_bracket.span).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Phantom:
///     'phantom'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Phantom {
    #[get = "pub"]
    phantom_keyword: Keyword,
}

impl SourceElement for Phantom {
    fn span(&self) -> Span { self.phantom_keyword.span() }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Unit:
///     Boolean
///     | Numeric
///     | QualifiedIdentifier
///     | Parenthesized
///     | Struct
///     | Array
///     | Phantom
///     | String
///     | Character
///    ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Unit {
    Boolean(Boolean),
    Numeric(Numeric),
    QualifiedIdentifier(QualifiedIdentifier),
    Parenthesized(Parenthesized),
    Struct(Struct),
    Array(Array),
    Phantom(Phantom),
    String(token::String),
    Character(Character),
}

impl SourceElement for Unit {
    fn span(&self) -> Span {
        match self {
            Self::Boolean(unit) => unit.span(),
            Self::Numeric(unit) => unit.span(),
            Self::QualifiedIdentifier(unit) => unit.span(),
            Self::Parenthesized(unit) => unit.span(),
            Self::Struct(unit) => unit.span(),
            Self::Array(unit) => unit.span(),
            Self::Phantom(unit) => unit.span(),
            Self::String(unit) => unit.span(),
            Self::Character(unit) => unit.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Call:
///     '(' ArgumentList? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Call {
    #[get = "pub"]
    left_paren: Punctuation,
    #[get = "pub"]
    arguments: Option<ArgumentList>,
    #[get = "pub"]
    right_paren: Punctuation,
}

impl SourceElement for Call {
    fn span(&self) -> Span {
        self.left_paren.span().join(&self.right_paren.span).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Cast:
///     'as' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Cast {
    #[get = "pub"]
    as_keyword: Keyword,
    #[get = "pub"]
    r#type: Type,
}

impl SourceElement for Cast {
    fn span(&self) -> Span {
        self.as_keyword.span().join(&self.r#type.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ``` txt
/// AccessOperator:
///     '.'
///     | '->'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessOperator {
    Dot(Punctuation),
    Arrow(Punctuation, Punctuation),
}

impl SourceElement for AccessOperator {
    fn span(&self) -> Span {
        match self {
            Self::Dot(punctuation) => punctuation.span(),
            Self::Arrow(start, end) => start.span().join(&end.span).unwrap(),
        }
    }
}

/// Syntax Synopsis:
/// ```txt
/// Index:
///     '[' Expression ']'
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Index {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for Index {
    fn span(&self) -> Span {
        self.left_bracket.span().join(&self.right_bracket.span).unwrap()
    }
}

impl Index {
    /// Destructs the index into its components
    #[must_use]
    pub fn destruct(self) -> (Punctuation, Box<Expression>, Punctuation) {
        (self.left_bracket, self.expression, self.right_bracket)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleIndex:
///     '-'? Numeric
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleIndex {
    #[get = "pub"]
    minus: Option<Punctuation>,
    #[get = "pub"]
    index: token::Numeric,
}

impl SourceElement for TupleIndex {
    fn span(&self) -> Span {
        self.minus.as_ref().map_or_else(
            || self.index.span(),
            |minus| minus.span.join(&self.index.span).unwrap(),
        )
    }
}

impl TupleIndex {
    /// Destructs the tuple index into its components
    #[must_use]
    pub fn destruct(self) -> (Option<Punctuation>, token::Numeric) {
        (self.minus, self.index)
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// AccessKind:
///     Identifier
///     | Numeric
///     | Index
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessKind {
    GenericIdentifier(GenericIdentifier),
    Tuple(TupleIndex),
    Index(Index),
}

impl SourceElement for AccessKind {
    fn span(&self) -> Span {
        match self {
            Self::GenericIdentifier(identifier) => identifier.span(),
            Self::Tuple(index) => index.span(),
            Self::Index(index) => index.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Access:
///    AccessOperator AccessKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Access {
    #[get = "pub"]
    operator: AccessOperator,
    #[get = "pub"]
    kind: AccessKind,
}

impl SourceElement for Access {
    fn span(&self) -> Span {
        self.operator.span().join(&self.kind.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// PostfixOperator:
///     Call
///     | Cast
///     | Access
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PostfixOperator {
    Call(Call),
    Cast(Cast),
    Access(Access),
}

impl SourceElement for PostfixOperator {
    fn span(&self) -> Span {
        match self {
            Self::Call(operator) => operator.span(),
            Self::Cast(operator) => operator.span(),
            Self::Access(operator) => operator.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Postfix:
///     Postfixable PostfixOperator
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Postfix {
    #[get = "pub"]
    postfixable: Box<Postfixable>,
    #[get = "pub"]
    operator: PostfixOperator,
}

impl SourceElement for Postfix {
    fn span(&self) -> Span {
        self.postfixable.span().join(&self.operator.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Postfix:
///     Unit
///     | Postfix
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Postfixable {
    Unit(Unit),
    Postfix(Postfix),
}

impl SourceElement for Postfixable {
    fn span(&self) -> Span {
        match self {
            Self::Unit(unit) => unit.span(),
            Self::Postfix(postfix) => postfix.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// PrefixOperator:
///     '!'
///     | '-'
///     | '~'
///     | '*'
///     | 'local'
///     | 'unlocal'
///     | ReferenceOf
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
    BitwiseNot(Punctuation),
    Dereference(Punctuation),
    Local(Keyword),
    Unlocal(Keyword),
    ReferenceOf(ReferenceOf),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::Negate(p)
            | Self::BitwiseNot(p)
            | Self::Dereference(p)
            | Self::LogicalNot(p) => p.span.clone(),
            Self::Local(k) | Self::Unlocal(k) => k.span(),
            Self::ReferenceOf(k) => k.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Prefix:
///     PrefixOperator Prefixable
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Prefix {
    #[get = "pub"]
    prefixable: Box<Prefixable>,
    #[get = "pub"]
    operator: PrefixOperator,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span {
        self.operator.span().join(&self.prefixable.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Prefixable:
///     Postfixable
///     | Prefix
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Prefixable {
    Postfixable(Postfixable),
    Prefix(Prefix),
}

impl SourceElement for Prefixable {
    fn span(&self) -> Span {
        match self {
            Self::Postfixable(postfixable) => postfixable.span(),
            Self::Prefix(prefix) => prefix.span(),
        }
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
///     | '&'
///     | '&='
///     | '|'
///     | '|='
///     | '^'
///     | '^='
///     | '<<'
///     | '<<='
///     | '>>'
///     | '>>='
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    BitwiseAnd(Punctuation),
    CompoundBitwiseAnd(Punctuation, Punctuation),
    BitwiseOr(Punctuation),
    CompoundBitwiseOr(Punctuation, Punctuation),
    BitwiseXor(Punctuation),
    CompoundBitwiseXor(Punctuation, Punctuation),
    BitwiseLeftShift(Punctuation, Punctuation),
    CompoundBitwiseLeftShift(Punctuation, Punctuation, Punctuation),
    BitwiseRightShift(Punctuation, Punctuation),
    CompoundBitwiseRightShift(Punctuation, Punctuation, Punctuation),
}

/// Syntax Synopsis:
///
/// ``` txt
/// Binary:
///     Prefixable (BinaryOperator Prefixable)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Binary {
    #[get = "pub"]
    first: Box<Prefixable>,
    #[get = "pub"]
    chain: Vec<(BinaryOperator, Prefixable)>,
}

impl Binary {
    /// Destructs the binary into its components
    #[must_use]
    pub fn destruct(
        self,
    ) -> (Box<Prefixable>, Vec<(BinaryOperator, Prefixable)>) {
        (self.first, self.chain)
    }
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.chain.last().map_or_else(
            || self.first.span(),
            |(_, right)| self.first.span().join(&right.span()).unwrap(),
        )
    }
}

impl Parser<'_> {
    fn try_parse_label(&mut self) -> Option<Label> {
        self.try_parse(|parser| {
            let apostrophe = parser.parse_punctuation('\'', true, &Dummy)?;
            let Reading::Unit(Token::Identifier(identifier)) =
                parser.next_token()
            else {
                return None;
            };

            Some(Label { apostrophe, identifier })
        })
    }

    fn parse_statements(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Statements> {
        fn skip_to_next_statement(this: &mut Parser) {
            this.stop_at(|token| matches!(token, Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';'));

            if matches!(this.peek(), Reading::Unit(Token::Punctuation(p)) if p.punctuation == ';')
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
                    let Some(statement) = parser.parse_statement(handler)
                    else {
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

    fn parse_block(&mut self, handler: &dyn Handler<Error>) -> Option<Block> {
        let label_specifier = match self.stop_at_significant() {
            Reading::Unit(Token::Punctuation(apostrophe))
                if apostrophe.punctuation == '\'' =>
            {
                // eat apostrophe
                self.forward();

                let identifier = self.parse_identifier(handler)?;
                let colon = self.parse_punctuation(':', true, handler)?;

                Some(LabelSpecifier {
                    label: Label { apostrophe, identifier },
                    colon,
                })
            }
            _ => None,
        };

        let unsafe_keyword = match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(unsafe_keyword))
                if unsafe_keyword.kind == KeywordKind::Unsafe =>
            {
                self.forward();
                Some(unsafe_keyword)
            }
            _ => None,
        };
        let statements = self.parse_statements(handler)?;

        // parse block
        Some(Block { label_specifier, unsafe_keyword, statements })
    }

    fn parse_else(&mut self, handler: &dyn Handler<Error>) -> Option<Else> {
        let else_keyword = self.parse_keyword(KeywordKind::Else, handler)?;
        let expression = Box::new(
            if matches!(self.stop_at_significant(), Reading::Unit(Token::Keyword(k)) if k.kind == KeywordKind::If)
            {
                BlockOrIfElse::IfElse(self.parse_if_else(handler)?)
            } else {
                BlockOrIfElse::Block(self.parse_block(handler)?)
            },
        );

        Some(Else { else_keyword, expression })
    }

    fn parse_if_else(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<IfElse> {
        let if_keyword = self.parse_keyword(KeywordKind::If, handler)?;

        let delimited_tree_condition = self.step_into(
            Delimiter::Parenthesis,
            |parser| parser.parse_expression(handler).map(Box::new),
            handler,
        )?;

        let then_expression = self.parse_block(handler)?;
        let else_expression = if matches!(
            self.stop_at_significant(),
            Reading::Unit(Token::Keyword(else_keyword))
                if else_keyword.kind == KeywordKind::Else
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

    fn parse_match_arm(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<MatchArm> {
        let refutable_pattern = self.parse_refutable_pattern(handler)?;

        let colon = self.parse_punctuation(':', true, handler)?;
        let block = self.parse_block(handler)?;

        Some(MatchArm { refutable_pattern, colon, block })
    }

    fn parse_match(&mut self, handler: &dyn Handler<Error>) -> Option<Match> {
        let match_keyword = self.parse_keyword(KeywordKind::Match, handler)?;
        let parenthesized = self.parse_parenthesized_expression(handler)?;

        let delimited_tree_arms = self.parse_delimited_list(
            Delimiter::Brace,
            ',',
            |parser| parser.parse_match_arm(handler),
            handler,
        )?;

        Some(Match {
            match_keyword,
            parenthesized,
            left_brace: delimited_tree_arms.open,
            arms: delimited_tree_arms.list,
            right_brace: delimited_tree_arms.close,
        })
    }

    /// Parses a binary expression.
    #[allow(clippy::too_many_lines)]
    pub fn parse_expression(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Expression> {
        match self.stop_at_significant() {
            // parse continue
            Reading::Unit(Token::Keyword(continue_keyword))
                if continue_keyword.kind == KeywordKind::Continue =>
            {
                // eat continue keyword
                self.forward();

                let label = self.try_parse_label();

                Some(Expression::Terminator(Terminator::Continue(Continue {
                    continue_keyword,
                    label,
                })))
            }

            // parse return
            Reading::Unit(Token::Keyword(return_keyword))
                if return_keyword.kind == KeywordKind::Return =>
            {
                // eat return keyword
                self.forward();

                let binary =
                    self.try_parse(|parser| parser.parse_binary(&Dummy));

                Some(Expression::Terminator(Terminator::Return(Return {
                    return_keyword,
                    binary,
                })))
            }

            // parse express
            Reading::Unit(Token::Keyword(express_keyword))
                if express_keyword.kind == KeywordKind::Express =>
            {
                // eat express keyword
                self.forward();

                let label = self.try_parse_label();
                let binary =
                    self.try_parse(|parser| parser.parse_binary(&Dummy));

                Some(Expression::Terminator(Terminator::Express(Express {
                    express_keyword,
                    label,
                    binary,
                })))
            }

            // parse break
            Reading::Unit(Token::Keyword(break_keyword))
                if break_keyword.kind == KeywordKind::Break =>
            {
                // eat break keyword
                self.forward();

                let label = self.try_parse_label();
                let binary =
                    self.try_parse(|parser| parser.parse_binary(&Dummy));

                Some(Expression::Terminator(Terminator::Break(Break {
                    break_keyword,
                    label,
                    binary,
                })))
            }

            // parse block
            Reading::Unit(Token::Punctuation(p)) if p.punctuation == '\'' => {
                self.parse_block(handler)
                    .map(|x| Expression::Brace(Brace::Block(x)))
            }

            // parse block
            Reading::Unit(Token::Keyword(keyword))
                if keyword.kind == KeywordKind::Unsafe =>
            {
                self.parse_block(handler)
                    .map(|x| Expression::Brace(Brace::Block(x)))
            }

            // parse block
            Reading::IntoDelimited(Delimiter::Brace, _) => self
                .parse_block(handler)
                .map(|x| Expression::Brace(Brace::Block(x))),

            // parse if
            Reading::Unit(Token::Keyword(keyword))
                if keyword.kind == KeywordKind::If =>
            {
                self.parse_if_else(handler)
                    .map(|x| Expression::Brace(Brace::IfElse(x)))
            }

            // parse match
            Reading::Unit(Token::Keyword(keyword))
                if keyword.kind == KeywordKind::Match =>
            {
                self.parse_match(handler)
                    .map(|x| Expression::Brace(Brace::Match(x)))
            }

            // parse while
            Reading::Unit(Token::Keyword(while_keyword))
                if while_keyword.kind == KeywordKind::While =>
            {
                // eat while keyword
                self.forward();

                let delimited_tree_condition = self.step_into(
                    Delimiter::Parenthesis,
                    |parser| parser.parse_expression(handler).map(Box::new),
                    handler,
                )?;

                let block = self.parse_block(handler)?;

                Some(Expression::Brace(Brace::While(While {
                    while_keyword,
                    left_paren: delimited_tree_condition.open,
                    condition: delimited_tree_condition.tree?,
                    right_paren: delimited_tree_condition.close,
                    block,
                })))
            }

            // parse loop
            Reading::Unit(Token::Keyword(loop_keyword))
                if loop_keyword.kind == KeywordKind::Loop =>
            {
                // eat loop keyword
                self.forward();

                let block = self.parse_block(handler)?;

                Some(Expression::Brace(Brace::Loop(Loop {
                    loop_keyword,
                    block,
                })))
            }

            _ => self.parse_binary(handler).map(Expression::Binary),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_level =
            self.try_parse(|parser| match parser.next_significant_token() {
                Reading::Unit(Token::Punctuation(p)) => match p.punctuation {
                    '+' => Some(BinaryOperator::Add(p)),
                    '-' => Some(BinaryOperator::Subtract(p)),
                    '*' => Some(BinaryOperator::Multiply(p)),
                    '/' => Some(BinaryOperator::Divide(p)),
                    '%' => Some(BinaryOperator::Modulo(p)),
                    '=' => Some(BinaryOperator::Assign(p)),
                    '&' => Some(BinaryOperator::BitwiseAnd(p)),
                    '|' => Some(BinaryOperator::BitwiseOr(p)),
                    '^' => Some(BinaryOperator::BitwiseXor(p)),
                    '!' => {
                        let equal =
                            parser.parse_punctuation('=', false, &Dummy)?;
                        Some(BinaryOperator::NotEqual(p, equal))
                    }
                    '>' => Some(BinaryOperator::GreaterThan(p)),
                    '<' => Some(BinaryOperator::LessThan(p)),
                    _ => None,
                },
                Reading::Unit(Token::Keyword(k)) => match k.kind {
                    KeywordKind::And => Some(BinaryOperator::LogicalAnd(k)),
                    KeywordKind::Or => Some(BinaryOperator::LogicalOr(k)),
                    _ => None,
                },
                _ => None,
            })?;

        let Some(second_level) = self.try_parse(|parser| {
            match (first_level.clone(), parser.next_token()) {
                (first_level, Reading::Unit(Token::Punctuation(s))) => {
                    match (first_level, s.punctuation) {
                        (BinaryOperator::Add(p), '=') => {
                            Some(BinaryOperator::CompoundAdd(p, s))
                        }
                        (BinaryOperator::Subtract(p), '=') => {
                            Some(BinaryOperator::CompoundSubtract(p, s))
                        }
                        (BinaryOperator::Multiply(p), '=') => {
                            Some(BinaryOperator::CompoundMultiply(p, s))
                        }
                        (BinaryOperator::Divide(p), '=') => {
                            Some(BinaryOperator::CompoundDivide(p, s))
                        }
                        (BinaryOperator::Modulo(p), '=') => {
                            Some(BinaryOperator::CompoundModulo(p, s))
                        }
                        (BinaryOperator::BitwiseAnd(p), '=') => {
                            Some(BinaryOperator::CompoundBitwiseAnd(p, s))
                        }
                        (BinaryOperator::BitwiseOr(p), '=') => {
                            Some(BinaryOperator::CompoundBitwiseOr(p, s))
                        }
                        (BinaryOperator::BitwiseXor(p), '=') => {
                            Some(BinaryOperator::CompoundBitwiseXor(p, s))
                        }

                        (BinaryOperator::Assign(p), '=') => {
                            Some(BinaryOperator::Equal(p, s))
                        }

                        (BinaryOperator::GreaterThan(p), '=') => {
                            Some(BinaryOperator::GreaterThanOrEqual(p, s))
                        }
                        (BinaryOperator::LessThan(p), '=') => {
                            Some(BinaryOperator::LessThanOrEqual(p, s))
                        }

                        (BinaryOperator::GreaterThan(p), '>') => {
                            Some(BinaryOperator::BitwiseRightShift(p, s))
                        }
                        (BinaryOperator::LessThan(p), '<') => {
                            Some(BinaryOperator::BitwiseLeftShift(p, s))
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        }) else {
            return Some(first_level);
        };

        Some(
            self.try_parse(|parser| {
                match (second_level.clone(), parser.next_token()) {
                    (second_level, Reading::Unit(Token::Punctuation(s))) => {
                        match (second_level, s.punctuation) {
                            (BinaryOperator::BitwiseLeftShift(p1, p2), '=') => {
                                Some(BinaryOperator::CompoundBitwiseLeftShift(
                                    p1, p2, s,
                                ))
                            }
                            (
                                BinaryOperator::BitwiseRightShift(p1, p2),
                                '=',
                            ) => {
                                Some(BinaryOperator::CompoundBitwiseRightShift(
                                    p1, p2, s,
                                ))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            })
            .unwrap_or(second_level),
        )
    }

    /// Parses [`Binary`]
    pub fn parse_binary(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Binary> {
        let first = self.parse_prefixable(handler)?;
        let mut chain = Vec::new();

        while let Some(operator) = self.try_parse_binary_operator() {
            let right = self.parse_prefixable(handler)?;
            chain.push((operator, right));
        }

        Some(Binary { first: Box::new(first), chain })
    }

    pub fn parse_numeric_literal(&mut self) -> Option<Numeric> {
        let Reading::Unit(Token::Numeric(numeric)) =
            self.next_significant_token()
        else {
            return None;
        };

        let decimal = match (self.peek(), self.peek_offset(1)) {
            (
                Reading::Unit(Token::Punctuation(dot)),
                Some(Reading::Unit(Token::Numeric(numeric))),
            ) if dot.punctuation == '.' => {
                self.forward();
                self.forward();
                Some(Decimal { dot, numeric })
            }
            _ => None,
        };

        let suffix =
            if let Reading::Unit(Token::Identifier(identifier)) = self.peek() {
                self.forward();
                Some(identifier)
            } else {
                None
            };

        Some(Numeric { numeric, decimal, suffix })
    }

    fn parse_parenthesized_expression(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Parenthesized> {
        let enclosed_tree = self.parse_delimited_list(
            Delimiter::Parenthesis,
            ',',
            |parser| {
                parser.stop_at_significant();

                let ellipsis = match (
                    parser.peek(),
                    parser.peek_offset(1),
                    parser.peek_offset(2),
                ) {
                    (
                        Reading::Unit(Token::Punctuation(p1)),
                        Some(Reading::Unit(Token::Punctuation(p2))),
                        Some(Reading::Unit(Token::Punctuation(p3))),
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

                Some(Unpackable { ellipsis, expression })
            },
            handler,
        )?;

        Some(Parenthesized {
            left_paren: enclosed_tree.open,
            expression: enclosed_tree.list,
            right_paren: enclosed_tree.close,
        })
    }

    fn parse_identifier_expression(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Unit> {
        let qualified_identifier = self.parse_qualified_identifier(handler)?;

        match self.stop_at_significant() {
            Reading::IntoDelimited(Delimiter::Brace, _) => {
                // handle struct literal

                let delimited_list = self.parse_delimited_list(
                    Delimiter::Brace,
                    ',',
                    |this| {
                        let identifier = this.parse_identifier(handler)?;
                        let colon =
                            this.parse_punctuation(':', true, handler)?;
                        let expression =
                            Box::new(this.parse_expression(handler)?);

                        // field initializer
                        Some(FieldInitializer { identifier, colon, expression })
                    },
                    handler,
                )?;

                Some(Unit::Struct(Struct {
                    qualified_identifier,
                    left_brace: delimited_list.open,
                    field_initializers: delimited_list.list,
                    right_brace: delimited_list.close,
                }))
            }

            _ => Some(Unit::QualifiedIdentifier(qualified_identifier)),
        }
    }

    pub fn parse_prefixable(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Prefixable> {
        let prefix_op = match self.stop_at_significant() {
            Reading::Unit(Token::Punctuation(p))
                if matches!(p.punctuation, '!' | '-' | '~' | '*') =>
            {
                // eat the token
                self.forward();

                match p.punctuation {
                    '!' => PrefixOperator::LogicalNot(p),
                    '-' => PrefixOperator::Negate(p),
                    '~' => PrefixOperator::BitwiseNot(p),
                    '*' => PrefixOperator::Dereference(p),
                    _ => unreachable!(),
                }
            }

            Reading::Unit(Token::Keyword(k))
                if matches!(
                    k.kind,
                    KeywordKind::Local | KeywordKind::Unlocal
                ) =>
            {
                // eat the token
                self.forward();

                match k.kind {
                    KeywordKind::Local => PrefixOperator::Local(k),
                    KeywordKind::Unlocal => PrefixOperator::Unlocal(k),
                    _ => unreachable!(),
                }
            }

            Reading::Unit(Token::Punctuation(
                ampersand @ Punctuation { punctuation: '&', .. },
            )) => {
                self.forward();

                let mutable_keyword = match self.stop_at_significant() {
                    Reading::Unit(Token::Keyword(
                        mutable_keyword @ Keyword {
                            kind: KeywordKind::Mutable,
                            ..
                        },
                    )) => {
                        self.forward();
                        Some(mutable_keyword)
                    }
                    _ => None,
                };

                PrefixOperator::ReferenceOf(ReferenceOf {
                    ampersand,
                    mutable_keyword,
                })
            }

            _ => {
                return self
                    .parse_postfixable(handler)
                    .map(Prefixable::Postfixable)
            }
        };

        let prefixable = Box::new(self.parse_prefixable(handler)?);

        Some(Prefixable::Prefix(Prefix { prefixable, operator: prefix_op }))
    }

    #[allow(clippy::too_many_lines)]
    pub fn parse_postfixable(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Postfixable> {
        let mut current = Postfixable::Unit(self.parse_unit(handler)?);

        loop {
            match self.stop_at_significant() {
                Reading::Unit(Token::Punctuation(p))
                    if p.punctuation == '.' =>
                {
                    // eat the token
                    self.forward();

                    let kind = match self.stop_at_significant() {
                        Reading::Unit(Token::Numeric(n)) => {
                            // eat the access kind
                            self.forward();
                            AccessKind::Tuple(TupleIndex {
                                minus: None,
                                index: n,
                            })
                        }
                        Reading::Unit(Token::Identifier(_)) => {
                            let generic_identifier =
                                self.parse_generic_identifier(handler)?;

                            AccessKind::GenericIdentifier(generic_identifier)
                        }

                        Reading::Unit(Token::Punctuation(
                            minus @ Punctuation { punctuation: '-', .. },
                        )) => {
                            // eat the access kind
                            self.forward();

                            AccessKind::Tuple(TupleIndex {
                                minus: Some(minus),
                                index: self.parse_numeric(handler)?,
                            })
                        }

                        Reading::IntoDelimited(Delimiter::Bracket, _) => {
                            let delimited_tree = self.step_into(
                                Delimiter::Bracket,
                                |parser| {
                                    parser
                                        .parse_expression(handler)
                                        .map(Box::new)
                                },
                                handler,
                            )?;

                            AccessKind::Index(Index {
                                left_bracket: delimited_tree.open,
                                expression: delimited_tree.tree?,
                                right_bracket: delimited_tree.close,
                            })
                        }

                        found => {
                            handler.receive(Error {
                                expected: SyntaxKind::Identifier,
                                alternatives: vec![
                                    SyntaxKind::Numeric,
                                    SyntaxKind::Punctuation('-'),
                                    SyntaxKind::Punctuation('['),
                                ],
                                found: self.reading_to_found(found),
                            });
                            self.forward();
                            return None;
                        }
                    };

                    current = Postfixable::Postfix(Postfix {
                        postfixable: Box::new(current),
                        operator: PostfixOperator::Access(Access {
                            operator: AccessOperator::Dot(p),
                            kind,
                        }),
                    });
                }

                Reading::Unit(Token::Punctuation(hyphen))
                    if hyphen.punctuation == '-'
                        && matches!(
                            self.peek_offset(1),
                            Some(Reading::Unit(Token::Punctuation(p2)))
                            if p2.punctuation == '>'
                        ) =>
                {
                    // eat two tokens
                    self.forward();
                    let right_angle = self
                        .next_token()
                        .into_unit()
                        .unwrap()
                        .into_punctuation()
                        .unwrap();

                    let kind = match self.stop_at_significant() {
                        Reading::Unit(Token::Numeric(n)) => {
                            // eat the access kind
                            self.forward();
                            AccessKind::Tuple(TupleIndex {
                                minus: None,
                                index: n,
                            })
                        }

                        Reading::Unit(Token::Identifier(_)) => {
                            let generic_identifier =
                                self.parse_generic_identifier(handler)?;

                            AccessKind::GenericIdentifier(generic_identifier)
                        }

                        Reading::Unit(Token::Punctuation(
                            minus @ Punctuation { punctuation: '-', .. },
                        )) => {
                            // eat the access kind
                            self.forward();

                            AccessKind::Tuple(TupleIndex {
                                minus: Some(minus),
                                index: self.parse_numeric(handler)?,
                            })
                        }

                        Reading::IntoDelimited(Delimiter::Bracket, _) => {
                            let delimited_tree = self.step_into(
                                Delimiter::Bracket,
                                |parser| {
                                    parser
                                        .parse_expression(handler)
                                        .map(Box::new)
                                },
                                handler,
                            )?;

                            AccessKind::Index(Index {
                                left_bracket: delimited_tree.open,
                                expression: delimited_tree.tree?,
                                right_bracket: delimited_tree.close,
                            })
                        }

                        found => {
                            handler.receive(Error {
                                expected: SyntaxKind::Identifier,
                                alternatives: vec![SyntaxKind::Numeric],
                                found: self.reading_to_found(found),
                            });
                            self.forward();
                            return None;
                        }
                    };

                    current = Postfixable::Postfix(Postfix {
                        postfixable: Box::new(current),
                        operator: PostfixOperator::Access(Access {
                            operator: AccessOperator::Arrow(
                                hyphen,
                                right_angle,
                            ),
                            kind,
                        }),
                    });
                }

                Reading::IntoDelimited(Delimiter::Parenthesis, _) => {
                    // handle call
                    let delimited_list = self.parse_delimited_list(
                        Delimiter::Parenthesis,
                        ',',
                        |this| this.parse_expression(handler).map(Box::new),
                        handler,
                    )?;

                    current = Postfixable::Postfix(Postfix {
                        postfixable: Box::new(current),
                        operator: PostfixOperator::Call(Call {
                            left_paren: delimited_list.open,
                            arguments: delimited_list.list,
                            right_paren: delimited_list.close,
                        }),
                    });
                }

                Reading::Unit(Token::Keyword(k))
                    if k.kind == KeywordKind::As =>
                {
                    // eat the token
                    self.forward();

                    let ty = self.parse_type(handler)?;

                    current = Postfixable::Postfix(Postfix {
                        postfixable: Box::new(current),
                        operator: PostfixOperator::Cast(Cast {
                            as_keyword: k,
                            r#type: ty,
                        }),
                    });
                }
                _ => break,
            }
        }

        Some(current)
    }

    pub fn parse_unit(&mut self, handler: &dyn Handler<Error>) -> Option<Unit> {
        match self.stop_at_significant() {
            Reading::Unit(Token::Keyword(bool_keyword))
                if matches!(
                    bool_keyword.kind,
                    KeywordKind::True | KeywordKind::False
                ) =>
            {
                // eat the token
                self.forward();

                Some(Unit::Boolean(match bool_keyword.kind {
                    KeywordKind::True => Boolean::True,
                    KeywordKind::False => Boolean::False,
                    _ => unreachable!(),
                }(bool_keyword)))
            }

            Reading::Unit(Token::Keyword(keyword))
                if keyword.kind == KeywordKind::Phantom =>
            {
                // eat the token
                self.forward();

                Some(Unit::Phantom(Phantom { phantom_keyword: keyword }))
            }

            Reading::Unit(Token::Character(character)) => {
                // eat the token
                self.forward();

                Some(Unit::Character(character))
            }

            Reading::Unit(Token::String(string)) => {
                // eat the token
                self.forward();

                Some(Unit::String(string))
            }

            Reading::Unit(Token::Numeric(_)) => {
                Some(Unit::Numeric(self.parse_numeric_literal()?))
            }

            Reading::Unit(
                Token::Identifier(_)
                | Token::Keyword(Keyword {
                    kind:
                        KeywordKind::This | KeywordKind::Target | KeywordKind::Super,
                    ..
                }),
            ) => self.parse_identifier_expression(handler),

            Reading::IntoDelimited(Delimiter::Bracket, _) => {
                let delimited_list = self.parse_delimited_list(
                    Delimiter::Bracket,
                    ',',
                    |this| {
                        let expression =
                            Box::new(this.parse_expression(handler)?);
                        Some(expression)
                    },
                    handler,
                )?;

                Some(Unit::Array(Array {
                    left_bracket: delimited_list.open,
                    arguments: delimited_list.list,
                    right_bracket: delimited_list.close,
                }))
            }

            Reading::IntoDelimited(Delimiter::Parenthesis, _) => self
                .parse_parenthesized_expression(handler)
                .map(Unit::Parenthesized),

            found => {
                handler.receive(Error {
                    expected: SyntaxKind::Expression,
                    alternatives: Vec::new(),
                    found: self.reading_to_found(found),
                });
                self.forward();
                None
            }
        }
    }
}

#[cfg(test)]
mod test;
