//! Contains the [`Parser`] trait and various combinators.
use std::marker::PhantomData;

use crate::{
    abstract_tree::AbstractTree,
    expect::{self, Expect},
    output::{self, Output},
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

/// See [`parse_ast`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParseAst<T>(pub PhantomData<T>);

impl<T: AbstractTree> Parser for ParseAst<T> {
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

impl<T: AbstractTree> Output for ParseAst<T> {
    type Extract = output::One;
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
pub const fn parse_ast<A: AbstractTree>() -> ParseAst<A> {
    ParseAst(PhantomData)
}
