use pernixc_handler::Handler;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::{generic_parameter::GenericParameters, TrailingWhereClause};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse},
        StateMachine,
    },
    syntax_tree::{r#type::Type as TypeTerm, AccessModifier, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub type_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Type.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(type_keyword, identifier, generic_parameters)| Self {
                type_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> Span {
        self.generic_parameters.as_ref().map_or_else(
            || self.type_keyword.span.join(&self.identifier.span()),
            |generic_parameters| {
                self.type_keyword.span.join(&generic_parameters.span())
            },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub equals: Punctuation,
    pub r#type: TypeTerm,
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('='.to_owned(), TypeTerm::parse)
            .map(|(equals, r#type)| Self { equals, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Body {
    fn span(&self) -> Span { self.equals.span.join(&self.r#type.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl SyntaxTree for Type {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            Signature::parse,
            Body::parse,
            TrailingWhereClause::parse.or_none(),
        )
            .map(
                |(
                    access_modifier,
                    signature,
                    definition,
                    trailing_where_clause,
                )| Self {
                    access_modifier,
                    signature,
                    body: definition,
                    trailing_where_clause,
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        self.access_modifier.span().join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or_else(|| self.body.span(), SourceElement::span),
        )
    }
}
