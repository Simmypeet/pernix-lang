use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::postfix::Postfixable;
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{ReferenceOf, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix {
    pub operator: PrefixOperator,
    pub prefixable: Box<Prefixable>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span {
        self.operator.span().join(&self.prefixable.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Prefixable {
    Postfixable(Postfixable),
    Prefix(Prefix),
}

impl SyntaxTree for Prefixable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (PrefixOperator::parse, Self::parse.map(Box::new))
                .map(|(operator, prefixable)| Prefix { operator, prefixable })
                .map(Self::Prefix),
            Postfixable::parse.map(Self::Postfixable),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Prefixable {
    fn span(&self) -> Span {
        match self {
            Self::Postfixable(postfixable) => postfixable.span(),
            Self::Prefix(prefix) => prefix.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot(Keyword),
    Negate(Punctuation),
    BitwiseNot(Punctuation),
    Dereference(Punctuation),
    ReferenceOf(ReferenceOf),
}

impl SyntaxTree for PrefixOperator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Not.to_owned().map(Self::LogicalNot),
            '-'.to_owned().map(Self::Negate),
            '~'.to_owned().map(Self::BitwiseNot),
            '*'.to_owned().map(Self::Dereference),
            ReferenceOf::parse.map(Self::ReferenceOf),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::Negate(p) | Self::BitwiseNot(p) | Self::Dereference(p) => {
                p.span()
            }

            Self::LogicalNot(k) => k.span(),

            Self::ReferenceOf(k) => k.span(),
        }
    }
}
