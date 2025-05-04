//! Contains the [`Parser`] trait and various combinators.
use crate::{
    expect,
    expect::Expect,
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

        $state.set_node_index(node_index + 1);
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
