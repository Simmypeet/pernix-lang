use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{generic_parameter::GenericParameters, where_clause::WhereClause};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{
        r#type::Type, AccessModifier, EnclosedTree, ParseExt, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub enum_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Enum.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(enum_keyword, identifier, generic_parameters)| Self {
                enum_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> Span {
        self.enum_keyword.span.join(&self.identifier.span)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// VariantAssociation:
///     '(' Type ')'
///     ;
/// ```
pub type VariantAssociation = EnclosedTree<Type>;

impl SyntaxTree for VariantAssociation {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Type::parse
            .enclosed_tree(DelimiterKind::Parenthesis)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub identifier: Identifier,
    pub association: Option<VariantAssociation>,
}

impl SyntaxTree for Variant {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (expect::Identifier.to_owned(), VariantAssociation::parse.or_none())
            .map(|(identifier, association)| Self { identifier, association })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Variant {
    fn span(&self) -> Span {
        let end = self
            .association
            .as_ref()
            .map_or_else(|| self.identifier.span.clone(), SourceElement::span);

        self.identifier.span.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub colon: Punctuation,
    pub where_clause: Option<WhereClause>,
    pub variants: Vec<Passable<Variant>>,
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            WhereClause::parse.non_passable_indentation_item().or_none(),
            Variant::parse.indentation_item().keep_take_all(),
        )
            .step_into_indentation()
            .map(|(colon, (where_clause, variants))| Self {
                colon: colon.clone(),
                where_clause,
                variants,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Body {
    fn span(&self) -> Span {
        let end = self
            .variants
            .last()
            .map_or_else(|| self.colon.span.clone(), SourceElement::span);

        self.colon.span.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
}

impl SyntaxTree for Enum {
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

impl SourceElement for Enum {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span())
    }
}

#[cfg(test)]
mod test;
