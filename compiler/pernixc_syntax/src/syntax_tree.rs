//! Contains all the definition of syntax trees and their parsing functions.

#![allow(missing_docs)]

use std::fmt::Debug;

use enum_as_inner::EnumAsInner;
use expression::Expression;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};
use r#type::Type;

use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse, StepIntoDelimited},
        StateMachine,
    },
};

pub mod expression;
pub mod item;
pub mod json;
pub mod pattern;
pub mod predicate;
pub mod statement;
pub mod target;
pub mod r#type;

pub mod strategy;

/// An extension trait for the [`Parse`] trait allowing for more complex parsing
/// operations.
pub trait ParseExt<'a>: Parse<'a> {
    /// Parses a syntax tree node enclosed within a pair of delimiter tokens.
    #[allow(clippy::type_complexity)]
    fn enclosed_tree(
        self,
        delimiter: DelimiterKind,
    ) -> parse::Map<
        StepIntoDelimited<Self>,
        fn(
            (&'a Punctuation, Self::Output, &'a Punctuation),
        ) -> EnclosedTree<Self::Output>,
    >
    where
        Self: Sized,
    {
        self.step_into_delimited(delimiter).map(|(open, tree, close)| {
            EnclosedTree { open: open.clone(), tree, close: close.clone() }
        })
    }

    /// Parses a syntax tree node for [`EnclosedConnectedList`]. The parser
    /// will keep parsing elements until it reaches the closing delimiter.
    fn enclosed_connected_list<SeparatorParser>(
        self,
        separator_parser: SeparatorParser,
        delimiter: DelimiterKind,
    ) -> EnclosedConnectedListParser<Self, SeparatorParser>
    where
        Self: Sized,
    {
        EnclosedConnectedListParser {
            delimiter,
            element_parser: self,
            separator_parser,
        }
    }

    /// Parses a syntax tree node for [`ConnectedList`]. The parser will keep
    /// trying to parse elements until it no longer can parse any more elements
    /// without encountering an error.
    fn connected_list<SeparatorParser>(
        self,
        separator_parser: SeparatorParser,
    ) -> ConnectedListParser<Self, SeparatorParser>
    where
        Self: Sized,
    {
        ConnectedListParser { element_parser: self, separator_parser }
    }

    /// Parses a syntax tree node for [`UnionList`]. The parser will keep trying
    /// to parse elements until it no longer can parse any more elements without
    /// encountering an error.
    #[allow(clippy::type_complexity)]
    fn union_list(
        self,
    ) -> parse::Map<
        (Self, parse::KeepTake<(parse::ToOwned<char>, Self)>),
        fn(
            (Self::Output, Vec<(Punctuation, Self::Output)>),
        ) -> UnionList<Self::Output>,
    >
    where
        Self: Sized + Clone,
    {
        (self.clone(), ('+'.to_owned(), self).keep_take())
            .map(|(first, rest)| UnionList { first, rest })
    }
}

impl<'a, T: Parse<'a>> ParseExt<'a> for T {}

/// Represents a syntax tree that can be parsed.
pub trait SyntaxTree {
    /// Parses the syntax tree at the current state
    #[allow(clippy::missing_errors_doc)]
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self>
    where
        Self: Sized;
}

/// Represents a syntax tree node with a pattern of syntax tree node enclosed
/// within a pair of delimiter tokens.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnclosedTree<T> {
    /// The opening delimiter token.
    pub open: Punctuation,

    /// The enclosed syntax tree node.
    pub tree: T,

    /// The closing delimiter token.
    pub close: Punctuation,
}

impl<T> SourceElement for EnclosedTree<T> {
    fn span(&self) -> GlobalSpan { self.open.span().join(&self.close.span()) }
}

/// Represents a syntax tree node with a pattern of syntax tree nodes separated
/// by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated
/// by a separator. For example, a comma separated list of expressions such as
/// `1, 2, 3` can be represented by a [`ConnectedList`] with the separator being
/// a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    pub first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the
    /// element. The separator is the token/syntax tree node that separates
    /// the current element from the prior one.
    pub rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    pub trailing_separator: Option<Separator>,
}

impl<Element: SourceElement, Separator: SourceElement> SourceElement
    for ConnectedList<Element, Separator>
{
    fn span(&self) -> GlobalSpan {
        let end = self.trailing_separator.as_ref().map_or_else(
            || {
                self.rest.last().map_or_else(
                    || self.first.span(),
                    |(_, element)| element.span(),
                )
            },
            SourceElement::span,
        );

        self.first.span().join(&end)
    }
}

impl<Element, Separator> ConnectedList<Element, Separator> {
    /// Returns an iterator over the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &Element> {
        std::iter::once(&self.first)
            .chain(self.rest.iter().map(|(_, element)| element))
    }

    /// Returns an iterator over the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = Element> {
        std::iter::once(self.first)
            .chain(self.rest.into_iter().map(|(_, element)| element))
    }

    /// Gets the number of elements in the list.
    pub fn len(&self) -> usize { self.rest.len() + 1 }

    /// Returns `true` if the list is empty.
    ///
    /// The function will never return `false`.
    pub const fn is_empty(&self) -> bool { false }

    /// Destructs the [`ConnectedList`] into its components.
    #[must_use]
    pub fn destruct(
        self,
    ) -> (Element, Vec<(Separator, Element)>, Option<Separator>) {
        (self.first, self.rest, self.trailing_separator)
    }
}

/// Created by the [`ParseExt::connected_list`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedListParser<Element, Separator> {
    element_parser: Element,
    separator_parser: Separator,
}

impl<'a, Element: Parse<'a> + Clone, Separator: Parse<'a> + Clone> Parse<'a>
    for ConnectedListParser<Element, Separator>
{
    type Output = ConnectedList<Element::Output, Separator::Output>;

    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self::Output> {
        let mut first = None;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        loop {
            let element = if first.is_none() {
                self.element_parser.clone().parse(state_machine, handler)?
            } else {
                let Some(element) = self
                    .element_parser
                    .clone()
                    .or_none()
                    .parse(state_machine, handler)?
                else {
                    break;
                };

                element
            };

            if let Some(separator) = self
                .separator_parser
                .clone()
                .or_none()
                .parse(state_machine, handler)?
            {
                if first.is_none() {
                    first = Some(element);
                    trailing_separator = Some(separator);
                } else {
                    rest.push((
                        trailing_separator.replace(separator).expect(
                            "should have a separator from previous iteration",
                        ),
                        element,
                    ));
                }
            } else {
                if first.is_none() {
                    first = Some(element);
                } else {
                    rest.push((
                        std::mem::take(&mut trailing_separator).expect(
                            "should have a separator from previous iteration",
                        ),
                        element,
                    ));
                }

                break;
            };
        }

        Ok(ConnectedList { first: first.unwrap(), rest, trailing_separator })
    }
}

/// Represents a pattern of [`ConnectedList`] enclosed within a pair of
/// delimiter tokens. For example, `(1, 2, 3)`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnclosedConnectedList<Element, Separator> {
    /// The open delimiter of the list.
    pub open: Punctuation,

    /// The inner list of elements. If `None` then the list is empty
    /// (immediately closed with the close delimiter).
    pub connected_list: Option<ConnectedList<Element, Separator>>,

    /// The close delimiter of the list.
    pub close: Punctuation,
}

impl<Element, Separator> SourceElement
    for EnclosedConnectedList<Element, Separator>
{
    fn span(&self) -> GlobalSpan { self.open.span().join(&self.close.span()) }
}

/// Created by the [`ParseExt::enclosed_connected_list`] method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnclosedConnectedListParser<Element, Separator> {
    delimiter: DelimiterKind,
    element_parser: Element,
    separator_parser: Separator,
}

impl<'a, Element: Parse<'a> + Clone, Separator: Parse<'a> + Clone> Parse<'a>
    for EnclosedConnectedListParser<Element, Separator>
{
    type Output = EnclosedConnectedList<Element::Output, Separator::Output>;

    #[allow(clippy::too_many_lines)]
    fn parse(
        self,
        state_machine: &mut StateMachine<'a>,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self::Output> {
        let inner_connected_list =
            |state_machine: &mut StateMachine<'a>,
             handler: &dyn Handler<error::Error>| {
                let mut first = None;
                let mut rest = Vec::new();
                let mut trailing_separator = None;

                let find_closet_separator =
                    |state_machine: &mut StateMachine<'a>,
                     handler: &dyn Handler<error::Error>| {
                        loop {
                            // no more tokens
                            if state_machine.peek().is_none() {
                                return false;
                            }

                            // find the next separator
                            if self
                                .separator_parser
                                .clone()
                                .parse_and_drain(state_machine, handler)
                                .is_ok()
                            {
                                return true;
                            }
                        }
                    };

                // keep parse until we hit the end
                while state_machine.peek().is_some() {
                    let Some(element) = self
                        .element_parser
                        .clone()
                        .parse_and_report(state_machine, handler)
                    else {
                        // error recovery, find the closest separator and
                        // start from there
                        if !find_closet_separator(state_machine, handler) {
                            continue;
                        }

                        break;
                    };

                    // parse the separator
                    let separator = if state_machine.peek().is_some() {
                        if let Some(separator) = self
                            .separator_parser
                            .clone()
                            .parse_and_report(state_machine, handler)
                        {
                            separator
                        } else {
                            // error recovery, find the closest separator
                            // and start from there
                            if find_closet_separator(state_machine, handler) {
                                continue;
                            }

                            break;
                        }
                    } else {
                        if first.is_none() {
                            first = Some(element);
                        } else {
                            rest.push((
                                std::mem::take(&mut trailing_separator).expect(
                                    "should have a separator from previous \
                                     iteration",
                                ),
                                element,
                            ));
                        }

                        break;
                    };

                    // assign the value
                    if first.is_none() {
                        first = Some(element);
                        trailing_separator = Some(separator);
                    } else {
                        rest.push((
                            trailing_separator.replace(separator).expect(
                                "should have a separator from previous \
                                 iteration",
                            ),
                            element,
                        ));
                    }
                }

                Ok(first.map(|first| ConnectedList {
                    first,
                    rest,
                    trailing_separator,
                }))
            };

        inner_connected_list
            .step_into_delimited(self.delimiter)
            .map(|(open, tree, close)| EnclosedConnectedList {
                open: open.clone(),
                connected_list: tree,
                close: close.clone(),
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessModifier {
    Public(Keyword),
    Private(Keyword),
    Internal(Keyword),
}

impl SyntaxTree for AccessModifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Public.to_owned().map(Self::Public),
            KeywordKind::Private.to_owned().map(Self::Private),
            KeywordKind::Internal.to_owned().map(Self::Internal),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for AccessModifier {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Public(k) | Self::Private(k) | Self::Internal(k) => {
                k.span.clone()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeSeparator {
    pub first: Punctuation,
    pub second: Punctuation,
}

impl SyntaxTree for ScopeSeparator {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (':'.to_owned(), ':'.no_skip().to_owned())
            .map(|(first, second)| Self { first, second })
            .commit_in(2)
            .parse(state_machine, handler)
    }
}

impl SourceElement for ScopeSeparator {
    fn span(&self) -> GlobalSpan { self.first.span.join(&self.second.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Elided {
    pub first_dot: Punctuation,
    pub second_dot: Punctuation,
}

impl SyntaxTree for Elided {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            first_dot: '.'.to_owned().parse(state_machine, handler)?,
            second_dot: '.'
                .no_skip()
                .to_owned()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for Elided {
    fn span(&self) -> GlobalSpan { self.first_dot.span.join(&self.second_dot.span) }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum LifetimeIdentifier {
    Identifier(Identifier),
    Static(Keyword),
    Elided(Elided),
}

impl SourceElement for LifetimeIdentifier {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Identifier(ident) => ident.span.clone(),
            Self::Static(keyword) => keyword.span.clone(),
            Self::Elided(elided) => elided.span(),
        }
    }
}

impl SyntaxTree for LifetimeIdentifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Identifier.to_owned().map(Self::Identifier),
            KeywordKind::Static.to_owned().map(Self::Static),
            Elided::parse.map(Self::Elided),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lifetime {
    pub apostrophe: Punctuation,
    pub identifier: LifetimeIdentifier,
}

impl SourceElement for Lifetime {
    fn span(&self) -> GlobalSpan {
        self.apostrophe.span.join(&self.identifier.span())
    }
}

impl SyntaxTree for Lifetime {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            apostrophe: '\''.to_owned().parse(state_machine, handler)?,
            identifier: LifetimeIdentifier::parse(state_machine, handler)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Constant {
    Expression(Box<Expression>),
    Elided(Elided),
}

impl SyntaxTree for Constant {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Expression::parse.map(Box::new).map(Constant::Expression),
            Elided::parse.map(Constant::Elided),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Constant {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Expression(expr) => expr.span(),
            Self::Elided(elided) => elided.span(),
        }
    }
}

pub type ConstantArgument = EnclosedTree<Constant>;

impl SyntaxTree for ConstantArgument {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Constant::parse
            .enclosed_tree(DelimiterKind::Brace)
            .parse(state_machine, handler)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
pub enum GenericArgument {
    Type(Box<Type>),
    Constant(ConstantArgument),
    Lifetime(Lifetime),
}

impl SyntaxTree for GenericArgument {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Type::parse.map(Box::new).map(Self::Type),
            ConstantArgument::parse.map(Self::Constant),
            Lifetime::parse.map(Self::Lifetime),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for GenericArgument {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Type(type_specifier) => type_specifier.span(),
            Self::Lifetime(lifetime_argument) => lifetime_argument.span(),
            Self::Constant(const_argument) => const_argument.span(),
        }
    }
}

pub type GenericArguments = EnclosedConnectedList<GenericArgument, Punctuation>;

impl SyntaxTree for GenericArguments {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        GenericArgument::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericIdentifier {
    pub identifier: Identifier,
    pub generic_arguments: Option<GenericArguments>,
}

impl SyntaxTree for GenericIdentifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            identifier: expect::Identifier
                .to_owned()
                .parse(state_machine, handler)?,
            generic_arguments: GenericArguments::parse
                .or_none()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for GenericIdentifier {
    fn span(&self) -> GlobalSpan {
        self.generic_arguments.as_ref().map_or_else(
            || self.identifier.span(),
            |generic_arguments| {
                self.identifier.span.join(&generic_arguments.span())
            },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameter {
    pub apostrophe: Punctuation,
    pub identifier: Identifier,
}

impl SyntaxTree for LifetimeParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            apostrophe: '\''.to_owned().parse(state_machine, handler)?,
            identifier: expect::Identifier
                .to_owned()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for LifetimeParameter {
    fn span(&self) -> GlobalSpan { self.apostrophe.span.join(&self.identifier.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum SimplePathRoot {
    Target(Keyword),
    Identifier(Identifier),
}

impl SyntaxTree for SimplePathRoot {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Target.to_owned().map(Self::Target),
            expect::Identifier.to_owned().map(Self::Identifier),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for SimplePathRoot {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Target(target) => target.span.clone(),
            Self::Identifier(identifier) => identifier.span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SimplePath {
    pub root: SimplePathRoot,
    pub rest: Vec<(ScopeSeparator, Identifier)>,
}

impl SyntaxTree for SimplePath {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            root: SimplePathRoot::parse(state_machine, handler)?,
            rest: (ScopeSeparator::parse, expect::Identifier.to_owned())
                .keep_take()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for SimplePath {
    fn span(&self) -> GlobalSpan {
        self.rest.last().map_or_else(
            || self.root.span(),
            |last| self.root.span().join(&last.1.span),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum QualifiedIdentifierRoot {
    Target(Keyword),
    This(Keyword),
    GenericIdentifier(GenericIdentifier),
}

impl SyntaxTree for QualifiedIdentifierRoot {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Target.to_owned().map(Self::Target),
            KeywordKind::This.to_owned().map(Self::This),
            GenericIdentifier::parse.map(Self::GenericIdentifier),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for QualifiedIdentifierRoot {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Target(keyword) | Self::This(keyword) => keyword.span.clone(),
            Self::GenericIdentifier(ident) => ident.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifier {
    pub root: QualifiedIdentifierRoot,
    pub rest: Vec<(ScopeSeparator, GenericIdentifier)>,
}

impl SyntaxTree for QualifiedIdentifier {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            root: QualifiedIdentifierRoot::parse(state_machine, handler)?,
            rest: (ScopeSeparator::parse, GenericIdentifier::parse)
                .keep_take()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for QualifiedIdentifier {
    fn span(&self) -> GlobalSpan {
        self.rest.last().map_or_else(
            || self.root.span(),
            |(_, identifier)| self.root.span().join(&identifier.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label {
    pub apostrophe: Punctuation,
    pub identifier: Identifier,
}

impl SyntaxTree for Label {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            apostrophe: '\''.to_owned().parse(state_machine, handler)?,
            identifier: expect::Identifier
                .to_owned()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for Label {
    fn span(&self) -> GlobalSpan { self.apostrophe.span.join(&self.identifier.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf {
    pub ampersand: Punctuation,
    pub mutable_keyword: Option<Keyword>,
}

impl SyntaxTree for ReferenceOf {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Ok(Self {
            ampersand: '&'.to_owned().parse(state_machine, handler)?,
            mutable_keyword: KeywordKind::Mut
                .to_owned()
                .or_none()
                .parse(state_machine, handler)?,
        })
    }
}

impl SourceElement for ReferenceOf {
    fn span(&self) -> GlobalSpan {
        self.mutable_keyword.as_ref().map_or_else(
            || self.ampersand.span(),
            |keyword| self.ampersand.span().join(&keyword.span()),
        )
    }
}

/// Similar to [`ConnectedList`] but specifically for list of arguments
/// separated by plus sings and has no trailing separator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnionList<T> {
    /// The first element of the list.
    pub first: T,

    /// The rest of the elements of the list.
    pub rest: Vec<(Punctuation, T)>,
}

impl<T> UnionList<T> {
    /// Returns an iterator containing references to the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &T> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, t)| t))
    }

    /// Returns an iterator containing the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = T> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, t)| t))
    }
}

impl<T: SourceElement> SourceElement for UnionList<T> {
    fn span(&self) -> GlobalSpan {
        let first = self.first.span();
        match self.rest.last() {
            Some(last) => first.join(&last.1.span()),
            None => first,
        }
    }
}

#[cfg(test)]
mod test;
