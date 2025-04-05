//! Contains all definition of item syntax trees.

#![allow(missing_docs)]

use pernixc_handler::Handler;
use pernixc_lexical::token::Punctuation;
use pernixc_source_file::{SourceElement, GlobalSpan};
use where_clause::WhereClause;

use super::SyntaxTree;
use crate::{
    error,
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
};

pub mod constant;
pub mod r#enum;
pub mod r#extern;
pub mod function;
pub mod generic_parameter;
pub mod implements;
pub mod marker;
pub mod module;
pub mod r#struct;
pub mod r#trait;
pub mod r#type;
pub mod where_clause;

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TrailingWhereClause {
    pub colon: Punctuation,
    pub where_clause: WhereClause,
}

impl SyntaxTree for TrailingWhereClause {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        WhereClause::parse
            .non_passable_indentation_item()
            .step_into_indentation()
            .map(|(colon, where_clause)| Self {
                colon: colon.clone(),
                where_clause,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TrailingWhereClause {
    fn span(&self) -> GlobalSpan { self.colon.span.join(&self.where_clause.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body<T> {
    pub colon: Punctuation,
    pub where_clause: Option<WhereClause>,
    pub members: Vec<Passable<T>>,
}

impl<T> SyntaxTree for Body<T>
where
    T: SyntaxTree,
{
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            WhereClause::parse.non_passable_indentation_item().or_none(),
            T::parse.indentation_item().keep_take_all(),
        )
            .step_into_indentation()
            .map(|(colon, (where_clause, members))| Self {
                colon: colon.clone(),
                where_clause,
                members,
            })
            .parse(state_machine, handler)
    }
}

impl<T> SourceElement for Body<T>
where
    T: SourceElement,
{
    fn span(&self) -> GlobalSpan {
        let being = self.colon.span.clone();
        let end = self.members.last().map_or_else(
            || {
                self.where_clause
                    .as_ref()
                    .map_or_else(|| being.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        being.join(&end)
    }
}
