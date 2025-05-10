//! Contains the [`Parser`] trait and various combinators.
use std::marker::PhantomData;

use crate::{
    abstract_tree::AbstractTree,
    expect::{self, Expect},
    output::{Multiple, One, Output},
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

    /// Repeats the given parser until it fails, creating a list (zero or more)
    /// of the successful trees.
    fn repeat(self) -> Repeat<Self>
    where
        Self: Sized,
    {
        Repeat(self)
    }

    /// Repeats the given parser until all of the tokens are consumed in the
    /// branch.
    ///
    /// In case of failure, the parser will try parsing the next valid sequence.
    /// and so on.
    fn repeat_all(self) -> RepeatAll<Self>
    where
        Self: Sized,
    {
        RepeatAll(self)
    }

    /// Repeats the `self` parser withe a `separator` parser in between. The
    /// parser will be ran until all of the tokens are consumed in the
    /// branch.
    ///
    /// This parser allows trailing separators
    fn repeat_all_with_separator<S: Parser>(
        self,
        separator: S,
    ) -> RepeatAllWithSeparator<Self, S>
    where
        Self: Sized,
    {
        RepeatAllWithSeparator(self, separator)
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

/// See [`IntoChoice::into_choice`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Choice<T>(pub T);

/// A utility for converting tuple of parser into [`Choice`] parser.
pub trait IntoChoice {
    /// The result of converting to [`Choice`] parser.
    type Output;

    /// Converts a tuple of parsers into a [`Choice`] parser which will attempt
    /// to parse each of the parser one by one unitl it finds the first matching
    /// parser.
    fn into_choice(self) -> Self::Output;
}

macro_rules! implements_choice {
    () => {};

    ($head:ident $($rest:ident)*) => {
        implements_choice!($($rest)*);
        implements_choice!(~ $head $($rest)*);
    };

    (~ $head:ident $($rest:ident)+) => {
        impl<$head: Parser, $($rest: Parser),*> Parser
            for Choice<($head, $($rest),*)> {

            #[allow(non_snake_case, unused_assignments)]
            fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
                let Choice(($head, $($rest),*)) = self;

                let starting_state = state.state_checkpoint();
                let starting_result = state.result_checkpoint();

                if $head.parse(state) == Ok(()) {
                    return Ok(());
                }

                let mut most_eaten_token_state = state.state_checkpoint();
                let mut most_eaten_tokens = state.node_index()
                    - starting_state.node_index();

                $(
                    state.restore_state(starting_state);
                    let alternative_checkpoint = state.checkpoint();

                    if $rest.parse(state) == Ok(()) {
                        // remove the previous furthest result
                        state.remove_middle(
                            starting_result,
                            alternative_checkpoint.result,
                        );
                        return Ok(());
                    }

                    let alternative_eaten = state.node_index()
                        - alternative_checkpoint.state.node_index();

                    // this new alternative is better than the previous one,
                    // so remove the previous one
                    if alternative_eaten > most_eaten_tokens {
                        state.remove_middle(
                            starting_result,
                            alternative_checkpoint.result,
                        );

                        most_eaten_tokens = alternative_eaten;
                        most_eaten_token_state = state.state_checkpoint();
                    } else {
                        state.restore_result(
                            alternative_checkpoint.result
                        );
                    }
                )*

                // if we reach here, it means that all the alternatives
                // failed, so we need to restore the state to the furthest
                // alternative
                state.restore_state(most_eaten_token_state);

                Err(Unexpected)
            }
        }

        impl<
            $head: Output<Extract = One>,
            $($rest: for<'x> Output<
                Extract = One,
                Output<'x> = $head::Output<'x>
            >),*> Output
            for Choice<($head, $($rest),*)>
        {
            type Extract = $head::Extract;
            type Output<'x> = $head::Output<'x>;

            #[allow(non_snake_case)]
            fn output<'a>(
                &self,
                node: &'a crate::concrete_tree::Node,
            ) -> Option<Self::Output<'a>> {
                let Choice(($head, $($rest),*)) = self;

                if let Some(output) = $head.output(node) {
                    return Some(output)
                }

                $(
                    if let Some(output) = $rest.output(node) {
                        return Some(output)
                    }
                )*

                None
            }
        }

        impl<$head: Parser, $($rest: Parser),*>
            IntoChoice for ($head, $($rest),*) {
            type Output = Choice<Self>;

            fn into_choice(self) -> Choice<Self> {
                Choice(self)
            }
        }
    };

    (~ $head:ident) => {
        impl<$head: Parser> Parser for Choice<($head,)> {
            fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
                self.0.0.parse(state)
            }
        }

        impl<$head: Output<Extract = One>> Output for Choice<($head,)> {
            type Extract = $head::Extract;
            type Output<'x> = $head::Output<'x>;

            #[allow(non_snake_case)]
            fn output<'a>(
                &self,
                node: &'a crate::concrete_tree::Node,
            ) -> Option<Self::Output<'a>> {
                self.0.0.output(node)
            }
        }

        impl<$head> IntoChoice for Choice<($head,)> {
            type Output = Choice<Self>;

            fn into_choice(self) -> Self::Output {
                Choice(self)
            }
        }
    };
}

implements_choice!(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);

/// See [`Parser::repeat`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Repeat<T>(pub T);

impl<T: Parser> Parser for Repeat<T> {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
        let mut checkpoint = state.checkpoint();

        loop {
            if self.0.parse(state) == Ok(()) {
                checkpoint = state.checkpoint();
            } else {
                state.restore(checkpoint);
                break;
            }
        }

        Ok(())
    }
}

impl<T: Output<Extract = One>> Output for Repeat<T> {
    type Extract = Multiple;
    type Output<'a> = T::Output<'a>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        T::output(&self.0, node)
    }
}

/// See [`Parser::repeat_all`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RepeatAll<T>(pub T);

impl<T: Parser> Parser for RepeatAll<T> {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
        while state.peek().is_some() {
            let Err(Unexpected) = self.0.parse(state) else {
                // if the parser is successful, continue parsing
                continue;
            };

            state.emit_error();

            while state.peek().is_some() {
                let checkpoint = state.checkpoint();

                // try to parse the next token
                if self.0.parse(state) == Ok(()) {
                    // break out and continue parsing the next element
                    break;
                }

                // if failed, restore the checkpoint
                state.restore(checkpoint);
                state.eat_error(1);
            }
        }

        Ok(())
    }
}

impl<T: Output<Extract = One>> Output for RepeatAll<T> {
    type Extract = Multiple;
    type Output<'a> = T::Output<'a>;

    fn output<'a>(
        &self,
        node: &'a crate::concrete_tree::Node,
    ) -> Option<Self::Output<'a>> {
        T::output(&self.0, node)
    }
}

/// See [`Parser::repeat_all_with_separator`] for more information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RepeatAllWithSeparator<T, S>(pub T, pub S);

impl<T: Parser, S: Parser> Parser for RepeatAllWithSeparator<T, S> {
    fn parse(&self, state: &mut State) -> Result<(), Unexpected> {
        fn parse_with_separator<T: Parser, S: Parser>(
            repeat: &RepeatAllWithSeparator<T, S>,
            state: &mut State,
            expect_separator: bool,
        ) -> Result<(), Unexpected> {
            if expect_separator {
                repeat.1.parse(state)?;
            }

            if state.peek().is_some() {
                repeat.0.parse(state)
            } else {
                // no more tokens to parse, the last separator was a trailing
                // separator
                Ok(())
            }
        }

        let mut expect_separator = false;

        while state.peek().is_some() {
            let Err(Unexpected) =
                parse_with_separator(self, state, expect_separator)
            else {
                expect_separator = true;

                // if the parser is successful, continue parsing
                continue;
            };

            state.emit_error();

            while state.peek().is_some() {
                let checkpoint = state.checkpoint();

                // try to parse the next token
                if self.1.parse(state) == Ok(()) {
                    // break out and continue parsing the next element
                    expect_separator = false;
                    break;
                }

                // if failed, restore the checkpoint
                state.restore(checkpoint);
                state.eat_error(1);
            }
        }

        Ok(())
    }
}

impl<T: Output<Extract = One>, S> Output for RepeatAllWithSeparator<T, S> {
    type Extract = Multiple;
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
