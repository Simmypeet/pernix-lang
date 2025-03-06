use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};
use pernixc_source_file::SourceElement;

use crate::{
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{predicate::Predicate, SyntaxTree},
};

//  pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct WhereClause {
    pub where_keyword: Keyword,
    pub colon: Punctuation,
    pub predicates: Vec<Passable<Predicate>>,
}

impl SyntaxTree for WhereClause {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<crate::error::Error>,
    ) -> parse::Result<Self>
    where
        Self: Sized,
    {
        (
            KeywordKind::Where.to_owned(),
            (Predicate::parse.indentation_item().keep_take_all())
                .step_into_indentation(),
        )
            .map(|(where_keyword, (colon, predicates))| Self {
                where_keyword,
                colon: colon.clone(),
                predicates,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for WhereClause {
    fn span(&self) -> pernixc_source_file::Span {
        let end = self
            .predicates
            .last()
            .map_or_else(|| self.colon.span.clone(), SourceElement::span);

        self.where_keyword.span.join(&end)
    }
}

// #[cfg(test)]
// mod test;
