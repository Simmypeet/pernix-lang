use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::binary::Binary;
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{Label, SyntaxTree},
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Terminator {
    Return(Return),
    Continue(Continue),
    Express(Express),
    Break(Break),
}

impl SyntaxTree for Terminator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Return::parse.map(Terminator::Return),
            Continue::parse.map(Terminator::Continue),
            Express::parse.map(Terminator::Express),
            Break::parse.map(Terminator::Break),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Terminator {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Return(syn) => syn.span(),
            Self::Continue(syn) => syn.span(),
            Self::Express(syn) => syn.span(),
            Self::Break(syn) => syn.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    pub return_keyword: Keyword,
    pub binary: Option<Binary>,
}

impl SyntaxTree for Return {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Return.to_owned(), Binary::parse.or_none())
            .map(|(return_keyword, binary)| Self { return_keyword, binary })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Return {
    fn span(&self) -> GlobalSpan {
        self.binary.as_ref().map_or_else(
            || self.return_keyword.span(),
            |expression| self.return_keyword.span().join(&expression.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Continue {
    pub continue_keyword: Keyword,
    pub label: Option<Label>,
}

impl SyntaxTree for Continue {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Continue.to_owned(), Label::parse.or_none())
            .map(|(continue_keyword, label)| Self { continue_keyword, label })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Continue {
    fn span(&self) -> GlobalSpan {
        self.label.as_ref().map_or_else(
            || self.continue_keyword.span(),
            |label| self.continue_keyword.span().join(&label.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TerminatorTarget(Option<Label>, Option<Binary>);

impl SyntaxTree for TerminatorTarget {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Label::parse.or_none(), Binary::parse.or_none())
            .map(|(label, binary)| Self(label, binary))
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Express {
    pub express_keyword: Keyword,
    pub label: Option<Label>,
    pub binary: Option<Binary>,
}

impl SyntaxTree for Express {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Express.to_owned(), TerminatorTarget::parse)
            .map(|(express_keyword, TerminatorTarget(label, binary))| Self {
                express_keyword,
                label,
                binary,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Express {
    fn span(&self) -> GlobalSpan {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.express_keyword.span(),
                    |label| self.express_keyword.span().join(&label.span()),
                )
            },
            |expression| self.express_keyword.span().join(&expression.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Break {
    pub break_keyword: Keyword,
    pub label: Option<Label>,
    pub binary: Option<Binary>,
}

impl SyntaxTree for Break {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Break.to_owned(), TerminatorTarget::parse)
            .map(|(break_keyword, TerminatorTarget(label, binary))| Self {
                break_keyword,
                label,
                binary,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Break {
    fn span(&self) -> GlobalSpan {
        self.binary.as_ref().map_or_else(
            || {
                self.label.as_ref().map_or_else(
                    || self.break_keyword.span(),
                    |label| self.break_keyword.span().join(&label.span()),
                )
            },
            |expression| self.break_keyword.span().join(&expression.span()),
        )
    }
}
