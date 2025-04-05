use pernixc_handler::Handler;
use pernixc_lexical::token::{self, Keyword, Punctuation};
use pernixc_source_file::SourceElement;

use super::{function, TrailingWhereClause};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{AccessModifier, SyntaxTree},
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub signature: function::Signature,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl SourceElement for Function {
    fn span(&self) -> pernixc_source_file::GlobalSpan {
        self.access_modifier.span().join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or(self.signature.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for Function {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            function::Signature::parse,
            TrailingWhereClause::parse.or_none(),
        )
            .map(|(access_modifier, signature, trailing_where_clause)| Self {
                access_modifier,
                signature,
                trailing_where_clause,
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Extern {
    pub extern_keyword: Keyword,
    pub convention: token::String,
    pub colon: Punctuation,
    pub functions: Vec<Passable<Function>>,
}

impl SourceElement for Extern {
    fn span(&self) -> pernixc_source_file::GlobalSpan {
        self.extern_keyword.span().join(
            &self
                .functions
                .last()
                .map_or(self.colon.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for Extern {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            token::KeywordKind::Extern.to_owned(),
            expect::String.to_owned(),
            Function::parse
                .indentation_item()
                .keep_take_all()
                .step_into_indentation(),
        )
            .map(|(extern_keyword, convention, (colon, functions))| Self {
                extern_keyword,
                convention,
                colon: colon.to_owned(),
                functions,
            })
            .parse(state_machine, handler)
    }
}

#[cfg(test)]
mod test;
