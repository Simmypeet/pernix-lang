use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{generic_parameter::GenericParameters, Body};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse},
        StateMachine,
    },
    syntax_tree::{
        pattern::Irrefutable, r#type::Type, statement::Statement,
        AccessModifier, EnclosedConnectedList, ParseExt, SyntaxTree,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ellipsis {
    pub first: Punctuation,
    pub second: Punctuation,
    pub third: Punctuation,
}

impl SourceElement for Ellipsis {
    fn span(&self) -> GlobalSpan { self.first.span.join(&self.third.span) }
}

impl SyntaxTree for Ellipsis {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('.'.to_owned(), '.'.no_skip().to_owned(), '.'.no_skip().to_owned())
            .commit_in(3)
            .map(|(f, s, t)| Self { first: f, second: s, third: t })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ParameterKind {
    Regular(Parameter),
    VarArgs(Ellipsis),
}

impl SourceElement for ParameterKind {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::Regular(parameter) => parameter.span(),
            Self::VarArgs(ellipsis) => ellipsis.span(),
        }
    }
}

impl SyntaxTree for ParameterKind {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Parameter::parse.map(Self::Regular),
            Ellipsis::parse.map(Self::VarArgs),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parameter {
    pub irrefutable_pattern: Irrefutable,
    pub colon: Punctuation,
    pub r#type: Type,
}

impl SyntaxTree for Parameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Irrefutable::parse, ':'.to_owned(), Type::parse)
            .map(|(irrefutable_pattern, colon, r#type)| Self {
                irrefutable_pattern,
                colon,
                r#type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Parameter {
    fn span(&self) -> GlobalSpan {
        self.irrefutable_pattern.span().join(&self.r#type.span())
    }
}

pub type Parameters = EnclosedConnectedList<ParameterKind, Punctuation>;

impl SyntaxTree for Parameters {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ParameterKind::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Parenthesis)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Arrow {
    pub dash: Punctuation,
    pub right_angle: Punctuation,
}

impl SyntaxTree for Arrow {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('-'.to_owned(), '>'.to_owned())
            .map(|(dash, right_angle)| Self { dash, right_angle })
            .commit_in(2)
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnType {
    pub arrow: Arrow,
    pub r#type: Type,
}

impl SyntaxTree for ReturnType {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Arrow::parse, Type::parse)
            .map(|(arrow, r#type)| Self { arrow, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for ReturnType {
    fn span(&self) -> GlobalSpan { self.arrow.dash.span.join(&self.r#type.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub function_keyword: Keyword,
    pub identifier: Identifier,
    pub generic_parameters: Option<GenericParameters>,
    pub parameters: Parameters,
    pub return_type: Option<ReturnType>,
}

impl SyntaxTree for Signature {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Function.to_owned(),
            expect::Identifier.to_owned(),
            GenericParameters::parse.or_none(),
            Parameters::parse,
            ReturnType::parse.or_none(),
        )
            .map(
                |(
                    function_keyword,
                    identifier,
                    generic_parameters,
                    parameters,
                    return_type,
                )| {
                    Self {
                        function_keyword,
                        identifier,
                        generic_parameters,
                        parameters,
                        return_type,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for Signature {
    fn span(&self) -> GlobalSpan {
        let being = self.function_keyword.span();

        let end = self
            .return_type
            .as_ref()
            .map_or_else(|| self.parameters.span(), SourceElement::span);

        being.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub unsafe_keyword: Option<Keyword>,
    pub const_keyword: Option<Keyword>,
    pub signature: Signature,
    pub body: Body<Statement>,
}

impl SyntaxTree for Function {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            AccessModifier::parse,
            KeywordKind::Unsafe.to_owned().or_none(),
            KeywordKind::Const.to_owned().or_none(),
            Signature::parse,
            Body::parse,
        )
            .map(
                |(
                    access_modifier,
                    unsafe_keyword,
                    const_keyword,
                    signature,
                    body,
                )| Self {
                    access_modifier,
                    unsafe_keyword,
                    const_keyword,
                    signature,
                    body,
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for Function {
    fn span(&self) -> GlobalSpan {
        self.access_modifier.span().join(&self.body.span())
    }
}

#[cfg(test)]
mod test;
