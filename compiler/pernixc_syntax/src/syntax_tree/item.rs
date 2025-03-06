//! Contains all definition of item syntax trees.

#![allow(missing_docs)]

use pernixc_handler::Handler;
use pernixc_lexical::token::Punctuation;
use pernixc_source_file::{SourceElement, Span};
use where_clause::WhereClause;

use super::SyntaxTree;
use crate::{
    error,
    state_machine::{
        parse::{self, Parse},
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
    fn span(&self) -> Span { self.colon.span.join(&self.where_clause.span()) }
}
