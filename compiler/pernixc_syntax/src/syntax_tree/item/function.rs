use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    generic_parameter::GenericParameters, r#type::Type,
    where_clause::WhereClause,
};
use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, ExpectExt, Parse, Passable},
        StateMachine,
    },
    syntax_tree::{
        pattern::Irrefutable, statement::Statement, AccessModifier,
        EnclosedConnectedList, ParseExt, SyntaxTree,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ellipsis {
    pub first: Punctuation,
    pub second: Punctuation,
    pub third: Punctuation,
}

impl SourceElement for Ellipsis {
    fn span(&self) -> Span { self.first.span.join(&self.third.span) }
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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span { self.arrow.dash.span.join(&self.r#type.span()) }
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
    fn span(&self) -> Span {
        let being = self.function_keyword.span.clone();
        let end = self
            .return_type
            .as_ref()
            .map_or_else(|| self.parameters.span(), SourceElement::span);

        being.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub colon: Punctuation,
    pub where_clause: Option<WhereClause>,
    pub statements: Vec<Passable<Statement>>,
}

impl SourceElement for Body {
    fn span(&self) -> Span {
        let being = self.colon.span.clone();
        let end = self.statements.last().map_or_else(
            || {
                self.where_clause
                    .as_ref()
                    .map_or_else(|| being.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        being.join(&end)
    }
}

impl SyntaxTree for Body {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            WhereClause::parse.or_none().non_passable_indentation_item(),
            Statement::parse.indentation_item().keep_take_all(),
        )
            .step_into_indentation()
            .map(|(colon, (where_clause, statements))| Self {
                colon: colon.to_owned(),
                where_clause,
                statements,
            })
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub access_modifier: AccessModifier,
    pub signature: Signature,
    pub body: Body,
}

impl SyntaxTree for Function {
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

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.access_modifier.span().join(&self.body.span())
    }
}
