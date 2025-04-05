use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{self, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{unit::Unit, Expression};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse},
        StateMachine,
    },
    syntax_tree::{
        r#type::Type, EnclosedConnectedList, EnclosedTree, GenericIdentifier,
        ParseExt, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Postfix {
    pub postfixable: Box<Postfixable>,
    pub operator: PostfixOperator,
}

impl SourceElement for Postfix {
    fn span(&self) -> GlobalSpan {
        self.postfixable.span().join(&self.operator.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Postfixable {
    Unit(Unit),
    Postfix(Postfix),
}

impl SyntaxTree for Postfixable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Unit::parse
            .map(Self::Unit)
            .keep_fold(PostfixOperator::parse, |operand, operator| {
                Self::Postfix(Postfix {
                    postfixable: Box::new(operand),
                    operator,
                })
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Postfixable {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Unit(unit) => unit.span(),
            Self::Postfix(postfix) => postfix.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PostfixOperator {
    Call(Call),
    Cast(Cast),
    Access(Access),
}

impl SyntaxTree for PostfixOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Call::parse.map(Self::Call),
            Cast::parse.map(Self::Cast),
            Access::parse.map(Self::Access),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for PostfixOperator {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Call(operator) => operator.span(),
            Self::Cast(operator) => operator.span(),
            Self::Access(operator) => operator.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Call {
    pub arguments: EnclosedConnectedList<Box<Expression>, Punctuation>,
}

impl SyntaxTree for Call {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Parenthesis)
            .map(|arguments| Self { arguments })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Call {
    fn span(&self) -> GlobalSpan { self.arguments.span() }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cast {
    pub as_keyword: Keyword,
    pub r#type: Type,
}

impl SyntaxTree for Cast {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::As.to_owned(), Type::parse)
            .map(|(as_keyword, r#type)| Self { as_keyword, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Cast {
    fn span(&self) -> GlobalSpan { self.as_keyword.span().join(&self.r#type.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessOperator {
    Dot(Punctuation),
    Arrow(Punctuation, Punctuation),
}

impl SyntaxTree for AccessOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '.'.to_owned().map(Self::Dot),
            ('-'.to_owned(), '>'.no_skip().to_owned())
                .map(|(start, end)| Self::Arrow(start, end))
                .commit_in(2),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for AccessOperator {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Dot(punctuation) => punctuation.span(),
            Self::Arrow(start, end) => start.span().join(&end.span),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index {
    pub expression: EnclosedTree<Box<Expression>>,
}

impl SyntaxTree for Index {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Expression::parse
            .map(Box::new)
            .enclosed_tree(DelimiterKind::Bracket)
            .map(|expression| Self { expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Index {
    fn span(&self) -> GlobalSpan { self.expression.span() }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleIndex {
    pub minus: Option<Punctuation>,
    pub index: token::Numeric,
}

impl SyntaxTree for TupleIndex {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('-'.to_owned().or_none(), expect::Numeric.to_owned())
            .map(|(minus, index)| Self { minus, index })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TupleIndex {
    fn span(&self) -> GlobalSpan {
        self.minus.as_ref().map_or_else(
            || self.index.span(),
            |minus| minus.span.join(&self.index.span),
        )
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

impl SyntaxTree for AccessKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            GenericIdentifier::parse.map(Self::GenericIdentifier),
            TupleIndex::parse.map(Self::Tuple),
            Index::parse.map(Self::Index),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for AccessKind {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::GenericIdentifier(identifier) => identifier.span(),
            Self::Tuple(index) => index.span(),
            Self::Index(index) => index.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Access {
    pub operator: AccessOperator,
    pub kind: AccessKind,
}

impl SyntaxTree for Access {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessOperator::parse, AccessKind::parse)
            .map(|(operator, kind)| Self { operator, kind })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Access {
    fn span(&self) -> GlobalSpan { self.operator.span().join(&self.kind.span()) }
}
