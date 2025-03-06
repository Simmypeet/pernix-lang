use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

use crate::{
    error, expect,
    state_machine::{
        parse::{self, Branch, Parse},
        StateMachine,
    },
    syntax_tree::{
        expression::Expression, predicate::TypeBound, r#type,
        EnclosedConnectedList, LifetimeParameter, ParseExt, SyntaxTree,
        UnionList,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultGenericParameter<Value> {
    pub equals: Punctuation,
    pub value: Value,
}

impl<Value: SyntaxTree> SyntaxTree for DefaultGenericParameter<Value> {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        ('='.to_owned(), Value::parse)
            .map(|(equals, value)| Self { equals, value })
            .parse(state_machine, handler)
    }
}

impl<Value: SourceElement> SourceElement for DefaultGenericParameter<Value> {
    fn span(&self) -> Span { self.equals.span.join(&self.value.span()) }
}

pub type DefaultTypeParameter = DefaultGenericParameter<r#type::Type>;
pub type DefaultConstantParameter = DefaultGenericParameter<Expression>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameterBound {
    pub colon: Punctuation,
    pub bounds: UnionList<TypeBound>,
}

impl SyntaxTree for TypeParameterBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (':'.to_owned(), TypeBound::parse.union_list())
            .map(|(colon, bounds)| Self { colon, bounds })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TypeParameterBound {
    fn span(&self) -> Span { self.colon.span.join(&self.bounds.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeParameter {
    pub identifier: Identifier,
    pub bounds: Option<TypeParameterBound>,
    pub default: Option<DefaultTypeParameter>,
}

impl SyntaxTree for TypeParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            expect::Identifier.to_owned(),
            TypeParameterBound::parse.or_none(),
            DefaultTypeParameter::parse.or_none(),
        )
            .map(|(identifier, bounds, default)| Self {
                identifier,
                bounds,
                default,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TypeParameter {
    fn span(&self) -> Span {
        let begin = self.identifier.span();

        let end = self.default.as_ref().map_or_else(
            || {
                self.bounds
                    .as_ref()
                    .map_or_else(|| begin.clone(), SourceElement::span)
            },
            SourceElement::span,
        );

        begin.join(&end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantParameter {
    pub const_keyword: Keyword,
    pub identifier: Identifier,
    pub colon: Punctuation,
    pub r#type: r#type::Type,
    pub default: Option<DefaultConstantParameter>,
}

impl SyntaxTree for ConstantParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Const.to_owned(),
            expect::Identifier.to_owned(),
            ':'.to_owned(),
            r#type::Type::parse,
            DefaultConstantParameter::parse.or_none(),
        )
            .map(|(const_keyword, identifier, colon, r#type, default)| Self {
                const_keyword,
                identifier,
                colon,
                r#type,
                default,
            })
            .parse(state_machine, handler)
    }
}

impl ConstantParameter {
    /// Dissolves the [`ConstantParameter`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Punctuation,
        r#type::Type,
        Option<DefaultConstantParameter>,
    ) {
        (
            self.const_keyword,
            self.identifier,
            self.colon,
            self.r#type,
            self.default,
        )
    }
}

impl SourceElement for ConstantParameter {
    fn span(&self) -> Span {
        self.const_keyword.span.join(
            &self
                .default
                .as_ref()
                .map_or_else(|| self.r#type.span(), SourceElement::span),
        )
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
pub enum GenericParameter {
    Lifetime(LifetimeParameter),
    Type(TypeParameter),
    Constant(ConstantParameter),
}

impl SyntaxTree for GenericParameter {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            LifetimeParameter::parse.map(GenericParameter::Lifetime),
            TypeParameter::parse.map(GenericParameter::Type),
            ConstantParameter::parse.map(GenericParameter::Constant),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for GenericParameter {
    fn span(&self) -> Span {
        match self {
            Self::Lifetime(lifetime_parameter) => lifetime_parameter.span(),
            Self::Type(type_parameter) => type_parameter.span(),
            Self::Constant(const_parameter) => const_parameter.span(),
        }
    }
}

pub type GenericParameters =
    EnclosedConnectedList<GenericParameter, Punctuation>;

impl SyntaxTree for GenericParameters {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        GenericParameter::parse
            .enclosed_connected_list(','.to_owned(), DelimiterKind::Bracket)
            .parse(state_machine, handler)
    }
}

#[cfg(test)]
mod test;
