use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind, Punctuation};

use crate::{
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{predicate::Predicate, SyntaxTree},
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct WhereClause {
    #[get = "pub"]
    where_keyword: Keyword,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    predicates: Vec<Passable<Predicate>>,
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

/*
from std::memory import Allocator, Sized, write, forget
from std::option import Option, Some

public class Vector[T, A]:
    private begin:     *T
    private end:       *T
    private capacity:  *T
    private allocator: A


implements[T: Sized, A: Allocator] Vector[T, A]:
    public function new(allocator: A) this:
        this {
            begin:     allocator.allocate(1),
            end:       begin,
            capacity:  begin + 1,
            allocator: allocator,
        }

    public function pushBack(self: &mut this, value: Option[T]):
        let Some(value) = value else:
            return

        if this.end == this.capacity:
            this.grow()

        write(this.end, value)
        forget(value)

        this.end += 1


*/

#[cfg(test)]
mod test;
