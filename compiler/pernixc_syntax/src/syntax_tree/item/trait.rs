use pernixc_handler::Handler;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{
    constant, function, generic_parameter::GenericParameters, r#type, Body,
    TrailingWhereClause,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{AccessModifier, SyntaxTree},
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub trait_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Trait.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
        )
            .map(|(trait_keyword, identifier, generic_parameters)| Self {
                trait_keyword,
                identifier,
                generic_parameters,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> GlobalSpan {
        self.trait_keyword.span().join(&self.identifier.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberTemplate<T> {
    pub access_modifier: AccessModifier,
    pub signature: T,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl<T: SyntaxTree> SyntaxTree for MemberTemplate<T> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse, T::parse, TrailingWhereClause::parse.or_none())
            .map(|(access_modifier, signature, trailing_where_clause)| Self {
                access_modifier,
                signature,
                trailing_where_clause,
            })
            .parse(state_machine, handler)
    }
}

impl<T: SourceElement> SourceElement for MemberTemplate<T> {
    fn span(&self) -> GlobalSpan {
        self.access_modifier.span().join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or_else(|| self.signature.span(), SourceElement::span),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Member {
    Type(MemberTemplate<r#type::Signature>),
    Function(MemberTemplate<function::Signature>),
    Constant(MemberTemplate<constant::Signature>),
}

impl SyntaxTree for Member {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            MemberTemplate::<r#type::Signature>::parse.map(Member::Type),
            MemberTemplate::<function::Signature>::parse.map(Member::Function),
            MemberTemplate::<constant::Signature>::parse.map(Member::Constant),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Member {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Type(signature) => signature.span(),
            Self::Function(signature) => signature.span(),
            Self::Constant(signature) => signature.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body<Member>,
}

impl SyntaxTree for Trait {
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

impl SourceElement for Trait {
    fn span(&self) -> GlobalSpan {
        self.access_modifier.span().join(&self.body.span())
    }
}

#[cfg(test)]
mod test;
