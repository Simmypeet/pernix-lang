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
    syntax_tree::{
        expression::Expression, r#type::Type, AccessModifier, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub const_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub colon: Punctuation,
    pub r#type: Type,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Const.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            ':'.to_owned(),
            Type::parse,
        )
            .map(
                |(
                    const_keyword,
                    identifier,
                    generic_parameters,
                    colon,
                    r#type,
                )| {
                    Self {
                        const_keyword,
                        identifier,
                        generic_parameters,
                        colon,
                        r#type,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> Span { self.const_keyword.span.join(&self.r#type.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub equals: Punctuation,
    pub expression: Expression,
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('='.to_owned(), Expression::parse)
            .map(|(equals, expression)| Self { equals, expression })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Body {
    fn span(&self) -> Span { self.equals.span.join(&self.expression.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl SyntaxTree for Constant {
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
            .map(|(access_modifier, signature, body, trailing_where_clause)| {
                Self { access_modifier, signature, body, trailing_where_clause }
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Constant {
    fn span(&self) -> Span {
        self.access_modifier.span().join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or_else(|| self.body.span(), SourceElement::span),
        )
    }
}

#[cfg(test)]
mod test;
