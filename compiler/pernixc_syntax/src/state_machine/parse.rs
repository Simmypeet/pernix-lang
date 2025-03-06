//! Contains the [`Parse`] trait and various implementations for parser
//! combinators.

use std::{borrow, cmp::Ordering, fmt::Debug, vec::Drain};

use paste::paste;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::{DelimiterKind, FragmentKind, NodeKind, Tree},
};
use pernixc_source_file::SourceElement;

use super::{StateMachine, StepIntoError};
use crate::{
    error,
    expect::{self, Expect, Expected, Fragment, NewLine},
};

/// A shorthand for the [`Result`] type that is used by the parser.
pub type Result<T> = std::result::Result<T, Unexpected>;

/// An extension trait for the [`Expect`] trait allowing for more parser
/// combinators.
pub trait ExpectExt: Expect {
    /// Creates a parser that does not skip insignificant tokens.
    fn no_skip(self) -> NoSkip<Self>
    where
        Self: Sized,
    {
        NoSkip(self)
    }
}

impl<T: Expect> ExpectExt for T {}

/// Created by the [`ExpectExt::no_skip`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoSkip<E>(pub E);

impl<'a, E: Expect> Parse<'a> for NoSkip<E>
where
    E::Output: 'static,
{
    type Output = &'a E::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        _: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        if let Some(token) = state_machine.next_no_skip() {
            self.0.expect(token).ok_or_else(|| {
                state_machine.expected.push(self.0.into());

                Unexpected {
                    token_index: Some(state_machine.current_token_index() - 1),
                    node_index: state_machine.current_node_index(),
                    commit_count: 1,
                }
            })
        } else {
            state_machine.expected.push(self.0.into());

            Err(Unexpected {
                token_index: None,
                node_index: state_machine.current_node_index(),
                commit_count: 1,
            })
        }
    }
}

/// An unexpected token was found.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unexpected {
    /// The index of the token in the token tree where the unexpected token was
    /// found. If `None`, then the end of the token stream was reached.
    pub token_index: Option<usize>,

    /// The node index in the token tree where the unexpected token was found.
    pub node_index: usize,

    /// The commit token count
    pub commit_count: usize,
}

/// A trait for parsing the syntax tree.
pub trait Parse<'a> {
    /// The syntax tree output of the parser.
    type Output;

    /// Parses the syntax tree. This is meant to be called by the implementation
    /// of the parser.
    ///
    /// # Errors
    ///
    /// Returns an [`Unexpected`] error if the parser fails to parse the syntax
    /// tree.
    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output>;

    /// Changes the number of commiting tokens, which is most likely 1 for
    /// most parsers.
    fn commit_in(self, count: usize) -> CommitIn<Self>
    where
        Self: Sized,
    {
        assert!(count > 0, "commit count must be greater than 0");

        CommitIn { parser: self, count }
    }

    /// Parses the syntax tree and returns the output if the parser is
    /// successful.
    fn parse_syntax(
        self,
        tree: &'a Tree<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self::Output>
    where
        Self: Sized,
    {
        let mut state_machine = StateMachine::new(tree);

        match self.parse(&mut state_machine, handler) {
            Ok(output) => {
                // if you see this error, then some of the parser errors were
                // discarded or not properly handled
                assert!(state_machine.expected.is_empty(), "should be empty");

                Some(output)
            }
            Err(unexpected) => {
                let expected = state_machine.take_expected();

                handler.receive(error::Error::new(tree, unexpected, expected));
                None
            }
        }
    }

    /// Maps the output of the parser to another output.
    fn map<O, F: FnOnce(Self::Output) -> O>(self, map: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { parser: self, map }
    }

    /// Converts the output of the parser to an owned type.
    fn to_owned<O: borrow::ToOwned + 'static>(self) -> ToOwned<Self>
    where
        Self: Sized + Parse<'a, Output = &'a O>,
    {
        ToOwned(self)
    }

    /// Returns `None` if the parser fails to parse and has not consumed any
    /// sucessful tokens.
    ///
    /// If the parser has consumed any sucessful tokens and then fails, then
    /// the error is returned instead of `None`.
    fn or_none(self) -> OrNone<Self>
    where
        Self: Sized,
    {
        OrNone(self)
    }

    /// Keeps parsing the parser while the **first** token matches the parser.
    ///
    /// The parser will keep parsing the token until the **first** token does
    /// not match the parser.
    fn keep_take(self) -> KeepTake<Self>
    where
        Self: Sized,
    {
        KeepTake(self)
    }

    /// Parses all the token in the current token stream and returns a list
    /// of the parsed tokens. The parser will recover from errors and continue
    /// parsing after the error. The error will be reported to the handler.
    fn keep_take_all(self) -> KeepTakeAll<Self>
    where
        Self: Sized,
    {
        KeepTakeAll { element_parser: self }
    }

    /// Steps into an indentation fragment and parses the inner tokens.
    ///
    /// The parser expected to consume all the tokens inside the indentation.
    fn step_into_indentation(self) -> StepIntoIndentation<Self>
    where
        Self: Sized,
    {
        StepIntoIndentation { parser: self }
    }

    /// Steps into a delimited token stream and parses the inner tokens.
    ///
    /// The parser expected to consume all the tokens inside the delimiter.
    fn step_into_delimited(
        self,
        delimiter: DelimiterKind,
    ) -> StepIntoDelimited<Self>
    where
        Self: Sized,
    {
        StepIntoDelimited { parser: self, delimiter }
    }

    /// Keeps parsing the parser while the **first** token matches the parser
    /// and folds the output.
    fn keep_fold<R, F>(self, rest: R, fold: F) -> KeepFold<Self, R, F>
    where
        Self: Sized,
    {
        KeepFold { initial: self, rest, fold }
    }

    /// Parses the syntax tree and reports an error if the parser fails.
    fn parse_and_report(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Self::Output>
    where
        Self: Sized,
    {
        let tree = state_machine.tree;
        match self.parse_and_drain(state_machine, handler) {
            Ok(result) => Some(result),
            Err((unexpected, expected)) => {
                handler.receive(error::Error::new(
                    tree,
                    unexpected,
                    expected.collect::<Vec<_>>(),
                ));
                None
            }
        }
    }

    /// Starts parsing the syntax tree as an indentation item.
    ///
    /// The parser will skip to the nearest significant token and then parse
    /// the syntax tree as usual except that the new line is now a significant.
    ///
    /// The parser should end at a new line token or end of the token stream to
    /// be considered as a successful indentation item.
    fn indentation_item(self) -> IndentationItem<Self, true>
    where
        Self: Sized,
    {
        IndentationItem(self)
    }

    /// Similar to [`Parse::indentation_item`] but it does not allow `pass`
    /// tokens to be considered as a successful indentation item.
    fn non_passable_indentation_item(self) -> IndentationItem<Self, false>
    where
        Self: Sized,
    {
        IndentationItem(self)
    }

    /// Changes the flag of the new line token to be significant or not.
    fn new_line_significant(self, significant: bool) -> NewLineSignificant<Self>
    where
        Self: Sized,
    {
        NewLineSignificant { parser: self, new_line_significant: significant }
    }

    /// Parses the given parser and drains the expected tokens if failed.
    ///
    /// # Errors
    ///
    /// Returns the [`Unexpected`] and the iterator of the expected tokens at
    /// the time of the error.
    fn parse_and_drain<'s>(
        self,
        state_machine: &'s mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> std::result::Result<Self::Output, (Unexpected, Drain<'s, Expected>)>
    where
        Self: Sized,
    {
        let current_expected_len = state_machine.expected_len();

        let result = self.parse(state_machine, handler);

        match result {
            Ok(result) => {
                assert_eq!(
                    state_machine.expected_len(),
                    current_expected_len,
                    "expected tokens were not consumed"
                );

                Ok(result)
            }
            Err(unexpected) => {
                state_machine.correct_expected_len = current_expected_len;

                Err((
                    unexpected,
                    state_machine.expected.drain(current_expected_len..),
                ))
            }
        }
    }
}

macro_rules! expect_implements_parse {
    ($t:ty) => {
        impl<'a> Parse<'a> for $t {
            type Output = &'a <$t as Expect>::Output;

            fn parse(
                self,
                state_machine: &mut StateMachine<'a>,
                _: &dyn Handler<error::Error>,
            ) -> Result<Self::Output> {
                if let Some((token, tok_index)) = state_machine.next() {
                    self.expect(token).ok_or_else(|| {
                        state_machine.expected.push(self.into());
                        Unexpected {
                            token_index: Some(tok_index),
                            node_index: state_machine.current_node_index(),
                            commit_count: 1,
                        }
                    })
                } else {
                    state_machine.expected.push(self.into());

                    return Err(Unexpected {
                        token_index: None,
                        node_index: state_machine.current_node_index(),
                        commit_count: 1,
                    });
                }
            }
        }
    };
}

expect_implements_parse!(expect::Identifier);
expect_implements_parse!(expect::Numeric);
expect_implements_parse!(expect::String);
expect_implements_parse!(expect::Character);
expect_implements_parse!(KeywordKind);
expect_implements_parse!(char);
expect_implements_parse!(Fragment);
expect_implements_parse!(NewLine);

impl<
        'a,
        O,
        F: FnOnce(&mut StateMachine<'a>, &dyn Handler<error::Error>) -> Result<O>,
    > Parse<'a> for F
{
    type Output = O;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self(state_machine, handler)
    }
}

/// Created by the [`Parse::to_owned`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToOwned<T>(T);

impl<'a, O: std::borrow::ToOwned + 'a, T: Parse<'a, Output = &'a O>> Parse<'a>
    for ToOwned<T>
{
    type Output = O::Owned;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self.0.parse(state_machine, handler).map(std::borrow::ToOwned::to_owned)
    }
}

/// Created by the [`Parse::map`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map<P, F> {
    parser: P,
    map: F,
}

impl<'a, P: Parse<'a>, F: FnOnce(P::Output) -> O, O> Parse<'a> for Map<P, F> {
    type Output = O;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        Ok((self.map)(self.parser.parse(state_machine, handler)?))
    }
}

macro_rules! tuple_implements_syntax {
    ($t:ident, $($i:ident),*) => {
        impl<'a, $t: Parse<'a>, $($i: Parse<'a>),*> Parse<'a> for ($t, $($i),*) {
            type Output = ($t::Output, $($i::Output),*);

            #[allow(non_snake_case)]
            fn parse(
                self,
                state_machine: &mut StateMachine<'a>,
                handler: &dyn Handler<error::Error>,
            ) -> Result<Self::Output> {
                let ($t, $($i),*) = self;

                Ok((
                    $t.parse(state_machine, handler)?,
                    $(
                        $i.parse(state_machine, handler)?
                    ),*
                ))
            }
        }
    };
}

tuple_implements_syntax!(A, B);
tuple_implements_syntax!(A, B, C);
tuple_implements_syntax!(A, B, C, D);
tuple_implements_syntax!(A, B, C, D, E);
tuple_implements_syntax!(A, B, C, D, E, F);
tuple_implements_syntax!(A, B, C, D, E, F, G);
tuple_implements_syntax!(A, B, C, D, E, F, G, H);
tuple_implements_syntax!(A, B, C, D, E, F, G, H, I);
tuple_implements_syntax!(A, B, C, D, E, F, G, H, I, J);

/// Created by the [`Parse::or_none`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrNone<T>(T);

impl<'a, T: Parse<'a>> Parse<'a> for OrNone<T> {
    type Output = Option<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let current_location = state_machine.location;
        let current_eaten = state_machine.eaten_tokens;
        let expected_len = state_machine.expected.len();

        match self.0.parse(state_machine, handler) {
            Ok(output) => Ok(Some(output)),
            Err(err) => {
                // if the parser has consumed token more than the commit count,
                // then return the error
                if state_machine.eaten_tokens > current_eaten + err.commit_count
                {
                    return Err(err);
                }

                state_machine.location = current_location;
                state_machine.eaten_tokens = current_eaten;
                state_machine.expected.truncate(expected_len);
                state_machine.correct_expected_len = expected_len;

                Ok(None)
            }
        }
    }
}

/// Created by the [`Parse::keep_take`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepTake<T>(T);

impl<'a, T: Parse<'a> + Clone> Parse<'a> for KeepTake<T> {
    type Output = Vec<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let mut output = Vec::new();

        while let Some(value) =
            self.0.clone().or_none().parse(state_machine, handler)?
        {
            output.push(value);
        }

        Ok(output)
    }
}

/// Used for stepping into the token stream fragment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepInto<P> {
    parser: P,
    fragment: Fragment,
}

impl<'a, P: Parse<'a>> Parse<'a> for StepInto<P> {
    type Output = (&'a FragmentKind, P::Output);

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let result = state_machine.next_step_into(
            |state_machine, location| -> Result<Self::Output> {
                match state_machine.current_node().kind {
                    NodeKind::Fragment { fragment, .. } => {
                        let kind_match = match self.fragment {
                            Fragment::Delimited(delimiter_kind) => fragment
                                .kind
                                .as_delimiter()
                                .is_some_and(|x| x.delimiter == delimiter_kind),
                            Fragment::Indetation => {
                                fragment.kind.is_indentation()
                            }
                        };

                        if !kind_match {
                            state_machine.expected.push(self.fragment.into());
                            return Err(Unexpected {
                                token_index: Some(location.token_index),
                                node_index: location.node_index,
                                commit_count: 1,
                            });
                        }

                        let tree = self.parser.parse(state_machine, handler)?;

                        // should be none, no more tokens to parse
                        if let Some((_, index)) = state_machine.next() {
                            // this is a soft error, the tree is still valid
                            handler.receive(error::Error::new(
                                state_machine.tree,
                                Unexpected {
                                    token_index: Some(index),
                                    node_index: state_machine
                                        .current_node_index(),
                                    commit_count: 1,
                                },
                                vec![match self.fragment {
                                    Fragment::Delimited(delimiter_kind) => {
                                        match delimiter_kind {
                                            DelimiterKind::Parenthesis => {
                                                ')'.into()
                                            }
                                            DelimiterKind::Brace => '}'.into(),
                                            DelimiterKind::Bracket => {
                                                ']'.into()
                                            }
                                        }
                                    }
                                    Fragment::Indetation => todo!(),
                                }],
                            ));
                        }

                        let fragment_kind = &state_machine
                            .tree
                            .get_node(state_machine.current_node_index())
                            .unwrap()
                            .as_fragment()
                            .unwrap()
                            .0
                            .kind;

                        Ok((fragment_kind, tree))
                    }

                    NodeKind::Root(_) => unreachable!(),
                }
            },
        );

        match result {
            Ok(result) => result,
            Err(error) => {
                state_machine.expected.push(self.fragment.into());

                match error {
                    StepIntoError::EndOfStream => Err(Unexpected {
                        token_index: None,
                        node_index: state_machine.current_node_index(),
                        commit_count: 1,
                    }),
                    StepIntoError::NotFragment => Err(Unexpected {
                        token_index: Some(
                            state_machine.current_token_index() - 1,
                        ),
                        node_index: state_machine.current_node_index(),
                        commit_count: 1,
                    }),
                }
            }
        }
    }
}

/// Created by the [`Parse::step_into_indentation`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepIntoIndentation<P> {
    parser: P,
}

impl<'a, P: Parse<'a>> Parse<'a> for StepIntoIndentation<P> {
    type Output = (&'a Punctuation, P::Output);

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        StepInto { parser: self.parser, fragment: Fragment::Indetation }
            .map(|(fragment, tree)| {
                (&fragment.as_indentation().unwrap().colon, tree)
            })
            .parse(state_machine, handler)
    }
}

/// Created by the [`Parse::step_into_delimited`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepIntoDelimited<P> {
    parser: P,
    delimiter: DelimiterKind,
}

impl<'a, P: Parse<'a>> Parse<'a> for StepIntoDelimited<P> {
    type Output = (&'a Punctuation, P::Output, &'a Punctuation);

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        StepInto {
            parser: self.parser,
            fragment: Fragment::Delimited(self.delimiter),
        }
        .map(|(x, tree)| {
            let delimiter = x.as_delimiter().unwrap();

            (&delimiter.open, tree, &delimiter.close)
        })
        .parse(state_machine, handler)
    }
}

/// Created by the [`Parse::keep_fold`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepFold<I, R, F> {
    initial: I,
    rest: R,
    fold: F,
}

impl<
        'a,
        I: Parse<'a>,
        R: Parse<'a> + Clone,
        F: FnMut(I::Output, R::Output) -> I::Output,
    > Parse<'a> for KeepFold<I, R, F>
{
    type Output = I::Output;

    fn parse(
        mut self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let initial = self.initial.parse(state_machine, handler)?;

        let mut output = initial;

        loop {
            let current =
                self.rest.clone().or_none().parse(state_machine, handler)?;

            if let Some(current) = current {
                output = (self.fold)(output, current);
            } else {
                break;
            }
        }

        Ok(output)
    }
}

/// Created by the [`Parse::keep_take_all`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeepTakeAll<T> {
    element_parser: T,
}

impl<'a, T: Parse<'a> + Clone> Parse<'a> for KeepTakeAll<T> {
    type Output = Vec<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let mut list = Vec::new();
        let mut recovering = false;
        let tree = state_machine.tree;

        while state_machine.peek().is_some() {
            match self
                .element_parser
                .clone()
                .parse_and_drain(state_machine, handler)
            {
                Ok(element) => {
                    list.push(element);
                    recovering = false;
                }

                Err((unexpected, expected)) => {
                    // not in recovery mode, report the error
                    if !recovering {
                        let this_expecteds = expected.collect::<Vec<_>>();

                        handler.receive(error::Error::new(
                            tree,
                            unexpected,
                            this_expecteds,
                        ));
                    }

                    recovering = true;
                }
            }
        }

        Ok(list)
    }
}

/// Created by the [`Branch::branch`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleBranch<T>(T);

/// A trait for branching parsers.
pub trait Branch {
    /// Creates a branch parser that will try to parse multiple choices of
    /// parsers and return the output of the first successful parser.
    fn branch(self) -> TupleBranch<Self>
    where
        Self: Sized,
    {
        TupleBranch(self)
    }
}

macro_rules! implements_tuple_branch {
    ($t:ident, $($i:ident),*) => {
        paste!{
            impl<'a, $t: Parse<'a>, $($i: Parse<'a, Output = $t::Output>),*>
                Branch for ($t, $($i),*) {}

            impl<'a, $t: Parse<'a>, $($i: Parse<'a, Output = $t::Output>),*>
                Parse<'a> for TupleBranch<($t, $($i),*)> {
                type Output = $t::Output;

                #[allow(non_snake_case, unused_assignments)]
                fn parse(
                    self,
                    state_machine: &mut StateMachine<'a>,
                    handler: &dyn Handler<error::Error>,
                ) -> Result<Self::Output> {
                    let starting_location = state_machine.location;
                    let starting_eaten = state_machine.eaten_tokens;
                    let starting_expected_len = state_machine.expected.len();

                    let ($t, $($i),*) = self.0;

                    let current_len = state_machine.expected.len();
                    let mut max_err =
                        match $t.parse(state_machine, handler) {
                            // return the output now
                            Ok(output) => return Ok(output),
                            Err(err) => err,
                        };


                    let mut current_expected_range =
                        current_len..state_machine.expected.len();

                    let mut max_eaten = state_machine.eaten_tokens;
                    let mut max_location = state_machine.location;

                    $(
                        // reset the state machine to the previous state
                        state_machine.location = starting_location;
                        state_machine.eaten_tokens = starting_eaten;

                        state_machine.correct_expected_len
                            = current_expected_range.end;

                        let next_err = match $i.parse(state_machine, handler) {
                            // return the output now
                            Ok(output) => {
                                state_machine
                                    .expected
                                    .truncate(starting_expected_len);

                                state_machine
                                    .correct_expected_len
                                    = starting_expected_len;

                                return Ok(output);
                            }
                            Err(err) => err,
                        };

                        match state_machine.eaten_tokens.cmp(&max_eaten) {
                            Ordering::Equal => {
                                max_err = next_err;
                                max_location = state_machine.location;
                                max_eaten = state_machine.eaten_tokens;

                                current_expected_range =
                                    current_expected_range.start..
                                        state_machine.expected.len();
                            }

                            Ordering::Less => {
                                // remove new expected token that was added by
                                // the next parser
                                state_machine
                                    .expected
                                    .truncate(current_expected_range.end);
                            }

                            Ordering::Greater => {
                                // remove the expected tokens that were added by
                                // the previous parser
                                state_machine
                                    .expected
                                    .drain(current_expected_range.clone());

                                max_err = next_err;
                                max_location = state_machine.location;
                                max_eaten = state_machine.eaten_tokens;

                                current_expected_range =
                                    current_expected_range.start..
                                        state_machine.expected.len();
                            }
                        }
                    )*

                    state_machine.location = max_location;
                    state_machine.eaten_tokens = max_eaten;

                    return Err(max_err);
                }
            }
        }
    };
}

/// Created by the [`Parse::commit_in`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CommitIn<T> {
    parser: T,
    count: usize,
}

impl<'a, T: Parse<'a> + Clone> Parse<'a> for CommitIn<T> {
    type Output = T::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        self.parser.parse(state_machine, handler).map_err(|mut err| {
            err.commit_count = self.count;
            err
        })
    }
}

implements_tuple_branch!(A, B);
implements_tuple_branch!(A, B, C);
implements_tuple_branch!(A, B, C, D);
implements_tuple_branch!(A, B, C, D, E);
implements_tuple_branch!(A, B, C, D, E, F);
implements_tuple_branch!(A, B, C, D, E, F, G);
implements_tuple_branch!(A, B, C, D, E, F, G, H);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
implements_tuple_branch!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y
);
implements_tuple_branch!(
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y,
    Z
);

/// Represents an item that is defined in the indentation block which can be
/// skipped by using the `pass` keyword.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Passable<T> {
    Pass(Keyword),
    SyntaxTree(T),
}

impl<T> Passable<T> {
    /// Converts the passable item into an option.
    #[must_use]
    pub fn into_option(self) -> Option<T> {
        match self {
            Self::Pass(_) => None,
            Self::SyntaxTree(tree) => Some(tree),
        }
    }

    /// Converts the passable item into an option.
    #[must_use]
    pub const fn as_option(&self) -> Option<&T> {
        match self {
            Self::Pass(_) => None,
            Self::SyntaxTree(tree) => Some(tree),
        }
    }
}

impl<T: SourceElement> SourceElement for Passable<T> {
    fn span(&self) -> pernixc_source_file::Span {
        match self {
            Self::Pass(keyword) => keyword.span(),
            Self::SyntaxTree(tree) => tree.span(),
        }
    }
}

/// Created by the [`Parse::indentation_item`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentationItem<T, const PASSABLE: bool>(T);

impl<'a, T: Parse<'a>> Parse<'a> for IndentationItem<T, true> {
    type Output = Passable<T::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let current_new_line_significant = state_machine.new_line_significant;

        // skip to the nearest significant token
        if let Some((_, tok_index)) = state_machine.peek() {
            state_machine.location.token_index = tok_index;
        }

        state_machine.new_line_significant = true;

        let result = (
            KeywordKind::Pass.to_owned().map(Passable::Pass),
            self.0.map(Passable::SyntaxTree),
        )
            .branch()
            .parse(state_machine, handler);

        let current = state_machine.peek();

        // it must end on a new line or just end of the token stream
        if let Some((token, mut index)) = current {
            if token.as_token().is_none_or(|x| !x.is_new_line()) {
                if result.is_ok() {
                    handler.receive(error::Error::new(
                        state_machine.tree,
                        Unexpected {
                            token_index: Some(index),
                            node_index: state_machine.current_node_index(),
                            commit_count: 1,
                        },
                        vec![Expected::NewLine(NewLine)],
                    ));
                }

                // find the nearest line
                while index < state_machine.current_node().token_stream().len()
                {
                    if state_machine
                        .current_node()
                        .token_stream()
                        .get(index)
                        .and_then(|x| x.as_token())
                        .and_then(|x| x.as_new_line())
                        .is_some()
                    {
                        break;
                    }
                    index += 1;
                }
            }

            state_machine.location.token_index = index;

            if state_machine.location.token_index
                < state_machine.current_node().token_stream().len()
            {
                // eat new line
                state_machine.location.token_index += 1;
            }
        }

        state_machine.new_line_significant = current_new_line_significant;

        result
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for IndentationItem<T, false> {
    type Output = T::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let current_new_line_significant = state_machine.new_line_significant;

        // skip to the nearest significant token
        if let Some((_, tok_index)) = state_machine.peek() {
            state_machine.location.token_index = tok_index;
        }

        state_machine.new_line_significant = true;

        let result = self.0.parse(state_machine, handler);

        let current = state_machine.peek();

        // it must end on a new line or just end of the token stream
        if let Some((token, mut index)) = current {
            if token.as_token().is_none_or(|x| !x.is_new_line()) {
                if result.is_ok() {
                    handler.receive(error::Error::new(
                        state_machine.tree,
                        Unexpected {
                            token_index: Some(index),
                            node_index: state_machine.current_node_index(),
                            commit_count: 1,
                        },
                        vec![Expected::NewLine(NewLine)],
                    ));
                }

                // find the nearest line
                while index < state_machine.current_node().token_stream().len()
                {
                    if state_machine
                        .current_node()
                        .token_stream()
                        .get(index)
                        .and_then(|x| x.as_token())
                        .and_then(|x| x.as_new_line())
                        .is_some()
                    {
                        break;
                    }
                    index += 1;
                }
            }

            state_machine.location.token_index = index;

            if state_machine.location.token_index
                < state_machine.current_node().token_stream().len()
            {
                // eat new line
                state_machine.location.token_index += 1;
            }
        }

        state_machine.new_line_significant = current_new_line_significant;

        result
    }
}

/// Created by the [`Parse::new_line_significant`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NewLineSignificant<P> {
    parser: P,
    new_line_significant: bool,
}

impl<'a, P: Parse<'a>> Parse<'a> for NewLineSignificant<P> {
    type Output = P::Output;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self::Output> {
        let current_new_line_significant = state_machine.new_line_significant;

        state_machine.new_line_significant = self.new_line_significant;

        let result = self.parser.parse(state_machine, handler);

        state_machine.new_line_significant = current_new_line_significant;

        result
    }
}

#[cfg(test)]
mod tests;
