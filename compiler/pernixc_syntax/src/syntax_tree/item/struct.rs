use pernixc_handler::Handler;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation};
use pernixc_source_file::{SourceElement, Span};

use super::{generic_parameter::GenericParameters, where_clause::WhereClause};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{r#type::Type, AccessModifier, SyntaxTree},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub struct_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Struct.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(struct_keyword, identifier, generic_parameters)| Self {
                struct_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> Span {
        self.struct_keyword.span.join(
            &self.generic_parameters.as_ref().map_or_else(
                || self.identifier.span.clone(),
                SourceElement::span,
            ),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub access_modifier: AccessModifier,
    pub identifier: Identifier,
    pub colon: Punctuation,
    pub r#type: Type,
}

impl SyntaxTree for Field {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            Type::parse,
        )
            .map(|(access_modifier, identifier, colon, r#type)| Self {
                access_modifier,
                identifier,
                colon,
                r#type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Field {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.r#type.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub colon: Punctuation,
    pub where_clause: Option<WhereClause>,
    pub fields: Vec<Passable<Field>>,
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            WhereClause::parse.non_passable_indentation_item().or_none(),
            Field::parse.indentation_item().keep_take_all(),
        )
            .step_into_indentation()
            .map(|(colon, (where_clause, fields))| Self {
                colon: colon.clone(),
                where_clause,
                fields,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Body {
    fn span(&self) -> Span {
        let end = self
            .fields
            .last()
            .map_or_else(|| self.colon.span.clone(), SourceElement::span);

        self.colon.span.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
}

impl SyntaxTree for Struct {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, Signature::parse, Body::parse)
            .map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Struct {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span())
    }
}
