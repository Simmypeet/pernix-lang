use pernixc_handler::Handler;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind};
use pernixc_source_file::{SourceElement, Span};

use super::{generic_parameter::GenericParameters, TrailingWhereClause};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse},
        StateMachine,
    },
    syntax_tree::{AccessModifier, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub marker_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SourceElement for Signature {
    fn span(&self) -> Span {
        self.marker_keyword.span().join(
            &self
                .generic_parameters
                .as_ref()
                .map_or(self.identifier.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Marker.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(marker_keyword, identifier, generic_parameters)| Self {
                marker_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Marker {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl SourceElement for Marker {
    fn span(&self) -> Span {
        self.access_modifier.span().join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or(self.signature.span(), SourceElement::span),
        )
    }
}

impl SyntaxTree for Marker {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            Signature::parse,
            TrailingWhereClause::parse.or_none(),
        )
            .map(|(access_modifier, signature, trailing_where_clause)| Self {
                access_modifier,
                signature,
                trailing_where_clause,
            })
            .parse(state_machine, handler)
    }
}
