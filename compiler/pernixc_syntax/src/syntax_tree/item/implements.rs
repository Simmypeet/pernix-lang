use pernixc_handler::Handler;
use pernixc_lexical::token::{Keyword, KeywordKind};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{
    constant, function, generic_parameter::GenericParameters, r#type,
    TrailingWhereClause,
};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{
        statement::Statement, AccessModifier, QualifiedIdentifier, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub final_keyword: Option<Keyword>,
    pub const_keyword: Option<Keyword>,
    pub implements_keyword: Keyword,
    pub generic_parameters: Option<GenericParameters>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for Signature {
    fn span(&self) -> GlobalSpan {
        let begin = self.final_keyword.as_ref().map_or_else(
            || {
                self.const_keyword.as_ref().map_or_else(
                    || self.implements_keyword.span(),
                    SourceElement::span,
                )
            },
            SourceElement::span,
        );

        begin.join(&self.qualified_identifier.span())
    }
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Final.to_owned().or_none(),
            KeywordKind::Const.to_owned().or_none(),
            KeywordKind::Implements.to_owned(),
            GenericParameters::parse.or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    final_keyword,
                    const_keyword,
                    implements_keyword,
                    generic_parameters,
                    qualified_identifier,
                )| Self {
                    final_keyword,
                    const_keyword,
                    implements_keyword,
                    generic_parameters,
                    qualified_identifier,
                },
            )
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberTemplate<S, B> {
    pub access_modifier: Option<AccessModifier>,
    pub signature: S,
    pub body: B,
}

impl<S: SourceElement, B: SourceElement> SourceElement
    for MemberTemplate<S, B>
{
    fn span(&self) -> GlobalSpan {
        self.access_modifier
            .as_ref()
            .map_or_else(
                || self.signature.span(),
                |access_modifier| {
                    access_modifier.span().join(&self.signature.span())
                },
            )
            .join(&self.body.span())
    }
}

impl<S: SyntaxTree, B: SyntaxTree> SyntaxTree for MemberTemplate<S, B> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (AccessModifier::parse.or_none(), S::parse, B::parse)
            .map(|(access_modifier, signature, body)| Self {
                access_modifier,
                signature,
                body,
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionSignature {
    pub unsafe_keyword: Option<Keyword>,
    pub const_keyword: Option<Keyword>,
    pub signature: function::Signature,
}

impl SourceElement for FunctionSignature {
    fn span(&self) -> GlobalSpan {
        let begin = self.const_keyword.as_ref().map_or_else(
            || {
                self.unsafe_keyword.as_ref().map_or_else(
                    || self.signature.function_keyword.span(),
                    |x| x.span.clone(),
                )
            },
            |x| x.span.clone(),
        );

        begin.join(&self.signature.span())
    }
}

impl SyntaxTree for FunctionSignature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Unsafe.to_owned().or_none(),
            KeywordKind::Const.to_owned().or_none(),
            function::Signature::parse,
        )
            .map(|(unsafe_keyword, const_keyword, signature)| Self {
                unsafe_keyword,
                const_keyword,
                signature,
            })
            .parse(state_machine, handler)
    }
}

/// Represents a member of an implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Member {
    Constant(MemberTemplate<constant::Signature, constant::Body>),
    Function(MemberTemplate<FunctionSignature, super::Body<Statement>>),
    Type(MemberTemplate<r#type::Signature, r#type::Body>),
}

impl SyntaxTree for Member {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        enum MemberWithoutAccessModifier {
            Constant((constant::Signature, constant::Body)),
            Function((FunctionSignature, super::Body<Statement>)),
            Type((r#type::Signature, r#type::Body)),
        }

        (
            AccessModifier::parse.or_none(),
            (
                (constant::Signature::parse, constant::Body::parse)
                    .map(MemberWithoutAccessModifier::Constant),
                (FunctionSignature::parse, super::Body::parse)
                    .map(MemberWithoutAccessModifier::Function),
                (r#type::Signature::parse, r#type::Body::parse)
                    .map(MemberWithoutAccessModifier::Type),
            )
                .branch(),
        )
            .map(|(access_mod, kind)| match kind {
                MemberWithoutAccessModifier::Constant((a, b)) => {
                    Self::Constant(MemberTemplate {
                        access_modifier: access_mod,
                        signature: a,
                        body: b,
                    })
                }
                MemberWithoutAccessModifier::Function((a, b)) => {
                    Self::Function(MemberTemplate {
                        access_modifier: access_mod,
                        signature: a,
                        body: b,
                    })
                }
                MemberWithoutAccessModifier::Type((a, b)) => {
                    Self::Type(MemberTemplate {
                        access_modifier: access_mod,
                        signature: a,
                        body: b,
                    })
                }
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Member {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Constant(member) => member.span(),
            Self::Function(member) => member.span(),
            Self::Type(member) => member.span(),
        }
    }
}

pub type PositiveBody = super::Body<Member>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Body {
    Negative(NegativeBody),
    Positive(PositiveBody),
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            NegativeBody::parse.map(Body::Negative),
            PositiveBody::parse.map(Body::Positive),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Body {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Negative(body) => body.span(),
            Self::Positive(body) => body.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegativeBody {
    pub delete_keyword: Keyword,
    pub trailing_where_clause: Option<TrailingWhereClause>,
}

impl SyntaxTree for NegativeBody {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Delete.to_owned(), TrailingWhereClause::parse.or_none())
            .map(|(delete_keyword, trailing_where_clause)| Self {
                delete_keyword,
                trailing_where_clause,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for NegativeBody {
    fn span(&self) -> GlobalSpan {
        let begin = self.delete_keyword.span();

        begin.join(
            &self
                .trailing_where_clause
                .as_ref()
                .map_or_else(|| begin.clone(), SourceElement::span),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implements {
    pub signature: Signature,
    pub body: Body,
}

impl SyntaxTree for Implements {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Signature::parse, Body::parse)
            .map(|(signature, body)| Self { signature, body })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Implements {
    fn span(&self) -> GlobalSpan { self.signature.span().join(&self.body.span()) }
}

#[cfg(test)]
mod test;
