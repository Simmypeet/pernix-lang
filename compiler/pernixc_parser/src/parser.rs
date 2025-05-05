//! Contains the [`Parser`] trait and various combinators.
use std::marker::PhantomData;

use crate::{
    abstract_tree::AbstractTree,
    expect::{self, Expect},
    output::{One, Output},
    state::{Cursor, State},
};

/// A struct returned
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected;

/// Represents a parser that manipulates the state machine and produces an
/// syntax tree.
pub trait Parser {
    /// Starts the parsing process.
    fn parse(&self, state: &mut State) -> Result<(), Unexpected>;

    /// Optionally parse the given parser, if successful, the result of the
    /// parsing will be used, otherwise, skip this parser.
    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional(self)
    }
}

impl<F: Fn(&mut State) -> Result<(), Unexpected>> Parser for F {
    fn parse(&self, state_machine: &mut State) -> Result<(), Unexpected> {
        self(state_machine)
    }
}

macro_rules! expect_impl_parser {
    { $(~$inner:ident)? $name:ty } => {
        impl Parser for $name {
            fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
                expect_impl_parser! { #self #state $(#$inner)?}
            }
        }
    };

    { #$self:ident #$state:ident } => {
        expect_impl_parser! { !$state !$self }
    };

    { #$self:ident #$state:ident #$inner:ident } => {
        $state.set_new_line_significant(
            false,
            |$inner| {
                expect_impl_parser! { !$inner !$self }
            }
        )
    };

    { !$state:ident !$self:ident } => {
        let current_index = $state.cursor().node_index;

        let Some((node, node_index)) = $state.peek() else {
            // end of the stream
            $state.add_error(std::iter::once((*$self).into()), Cursor {
                branch_id: $state.branch_id(),
                node_index: $state.branch().nodes.len(),
            });
            return Err(Unexpected);
        };

        let Some(leaf) = node.as_leaf() else {
            // not a leaf
            $state.add_error(std::iter::once((*$self).into()), Cursor {
                branch_id: $state.branch_id(),
                node_index,
            });
            return Err(Unexpected);
        };

        if !$self.expect(leaf) {
            $state.add_error(std::iter::once((*$self).into()), Cursor {
                branch_id: $state.branch_id(),
                node_index,
            });
            return Err(Unexpected);
        }

        $state.eat_token((node_index - current_index) + 1);

        Ok(())
    }
}

// macro for implementing the Parser trait for all expect types
expect_impl_parser! {expect::Identifier}
expect_impl_parser! {expect::IdentifierValue}
expect_impl_parser! {expect::String}
expect_impl_parser! {expect::Character}
expect_impl_parser! {expect::Numeric}
expect_impl_parser! {expect::Punctuation}
expect_impl_parser! {expect::Keyword}
expect_impl_parser! {~inner_state expect::NewLine}

/// See [`ast`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Ast<T>(pub PhantomData<T>);

impl<T: AbstractTree> Parser for Ast<T> {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
        let result = state.start_ndoe::<T, _>(|state| {
            let parser = T::parser();
            parser.parse(state)
        });

        match result {
            Some(Ok(())) => Ok(()),

            Some(Err(Unexpected)) | None => Err(Unexpected),
        }
    }
}

impl<T: AbstractTree> Output for Ast<T> {
    type Extract = One;
    type Output<'x> = T;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        T::from_node(node)
    }
}

/// Start parsing a node of the given AST type.
#[must_use]
pub const fn ast<A: AbstractTree>() -> Ast<A> { Ast(PhantomData) }

/// See [`Parser::optional`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Optional<T>(pub T);

impl<T: Parser> Parser for Optional<T> {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
        let checkpoint = state.checkpoint();

        if self.0.parse(state) == Err(Unexpected) {
            state.restore(checkpoint);
        }

        Ok(())
    }
}

impl<T: Output> Output for Optional<T> {
    type Extract = One;
    type Output<'a> = T::Output<'a>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        T::output(&self.0, node)
    }
}

#[cfg(test)]
mod test;
