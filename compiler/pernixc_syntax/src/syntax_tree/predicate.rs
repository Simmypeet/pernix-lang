//! Contains the definition of all predicates syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, GlobalSpan};

use super::{
    r#type, EnclosedConnectedList, Lifetime, LifetimeParameter, Parse,
    ParseExt, QualifiedIdentifier, SyntaxTree, UnionList,
};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch},
        StateMachine,
    },
};

pub mod strategy;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimes {
    pub for_keyword: Keyword,
    pub lifetime_parameters:
        EnclosedConnectedList<LifetimeParameter, Punctuation>,
}

impl SyntaxTree for HigherRankedLifetimes {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::For.to_owned(),
            LifetimeParameter::parse.enclosed_connected_list(
                ','.to_owned(),
                DelimiterKind::Bracket,
            ),
        )
            .map(|(for_keyword, lifetime_parameters)| Self {
                for_keyword,
                lifetime_parameters,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for HigherRankedLifetimes {
    fn span(&self) -> GlobalSpan {
        self.for_keyword.span.join(&self.lifetime_parameters.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifierBound {
    pub not_keyword: Option<Keyword>,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub const_keyword: Option<Keyword>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for QualifiedIdentifierBound {
    fn span(&self) -> GlobalSpan {
        let first = match (
            &self.not_keyword,
            &self.higher_ranked_lifetimes,
            &self.const_keyword,
        ) {
            (Some(negation), _, _) => negation.span(),
            (_, Some(higher_rankded_lifetimes), _) => {
                higher_rankded_lifetimes.span()
            }
            (_, _, Some(const_keyword)) => const_keyword.span(),
            (_, _, _) => self.qualified_identifier.span(),
        };

        first.join(&self.qualified_identifier.span())
    }
}

impl SyntaxTree for QualifiedIdentifierBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Not.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            KeywordKind::Const.to_owned().or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    not_keyword,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| {
                    Self {
                        not_keyword,
                        higher_ranked_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TypeBound {
    /// Represent either trait or marker bound.
    QualifiedIdentifier(QualifiedIdentifierBound),

    /// Represents a constant type bound.
    Const(Keyword),

    /// Represents a tuple type bound.
    Tuple(Keyword),

    /// Represents a lifetime outlives bound.
    Outlives(Lifetime),
}

impl SourceElement for TypeBound {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::QualifiedIdentifier(s) => s.span(),
            Self::Const(s) | Self::Tuple(s) => s.span(),
            Self::Outlives(s) => s.span(),
        }
    }
}

impl SyntaxTree for TypeBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            QualifiedIdentifierBound::parse.map(TypeBound::QualifiedIdentifier),
            KeywordKind::Const.to_owned().map(TypeBound::Const),
            KeywordKind::Tuple.to_owned().map(TypeBound::Tuple),
            Lifetime::parse.map(TypeBound::Outlives),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub r#type: r#type::Type,
    pub colon: Punctuation,
    pub bounds: UnionList<TypeBound>,
}

impl SyntaxTree for Type {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            HigherRankedLifetimes::parse.or_none(),
            r#type::Type::parse,
            ':'.to_owned(),
            TypeBound::parse.union_list(),
        )
            .map(|(higher_ranked_lifetimes, r#type, colon, bounds)| Self {
                higher_ranked_lifetimes,
                r#type,
                colon,
                bounds,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Type {
    fn span(&self) -> GlobalSpan {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.r#type.span().join(&self.bounds.span()),
            |h| h.span().join(&self.bounds.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitTypeEquality {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub lhs_type: r#type::Type,
    pub equals: Punctuation,
    pub rhs_type: r#type::Type,
}

impl SyntaxTree for TraitTypeEquality {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            HigherRankedLifetimes::parse.or_none(),
            r#type::Type::parse,
            '='.to_owned(),
            r#type::Type::parse,
        )
            .map(|(higher_ranked_lifetimes, lhs_type, equals, rhs_type)| Self {
                higher_ranked_lifetimes,
                lhs_type,
                equals,
                rhs_type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TraitTypeEquality {
    fn span(&self) -> GlobalSpan {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.lhs_type.span().join(&self.lhs_type.span()),
            |h| h.span().join(&self.lhs_type.span()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    pub not_keyword: Option<Keyword>,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub const_keyword: Option<Keyword>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl SyntaxTree for TraitBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Not.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            KeywordKind::Const.to_owned().or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    not_keyword,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| {
                    Self {
                        not_keyword,
                        higher_ranked_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for TraitBound {
    fn span(&self) -> GlobalSpan {
        let first = match (
            &self.not_keyword,
            &self.higher_ranked_lifetimes,
            &self.const_keyword,
        ) {
            (Some(negation), _, _) => negation.span(),
            (_, Some(higher_rankded_lifetimes), _) => {
                higher_rankded_lifetimes.span()
            }
            (_, _, Some(const_keyword)) => const_keyword.span(),
            (_, _, _) => self.qualified_identifier.span(),
        };

        first.join(&self.qualified_identifier.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub trait_keyword: Keyword,
    pub bounds: UnionList<TraitBound>,
}

impl SyntaxTree for Trait {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Trait.to_owned(), TraitBound::parse.union_list())
            .map(|(trait_keyword, bounds)| Self { trait_keyword, bounds })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Trait {
    fn span(&self) -> GlobalSpan { self.trait_keyword.span.join(&self.bounds.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MarkerBound {
    pub not_keyword: Option<Keyword>,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl SyntaxTree for MarkerBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Not.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    not_keyword,
                    higher_ranked_lifetimes,
                    qualified_identifier,
                )| {
                    Self {
                        not_keyword,
                        higher_ranked_lifetimes,
                        qualified_identifier,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl SourceElement for MarkerBound {
    fn span(&self) -> GlobalSpan {
        let begin = self.not_keyword.as_ref().map_or_else(
            || {
                self.higher_ranked_lifetimes.as_ref().map_or_else(
                    || self.qualified_identifier.span(),
                    SourceElement::span,
                )
            },
            SourceElement::span,
        );

        begin.join(&self.qualified_identifier.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Marker {
    pub marker_keyword: Keyword,
    pub bounds: UnionList<MarkerBound>,
}

impl SyntaxTree for Marker {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Marker.to_owned(), MarkerBound::parse.union_list())
            .map(|(marker_keyword, bounds)| Self { marker_keyword, bounds })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Marker {
    fn span(&self) -> GlobalSpan {
        self.marker_keyword.span.join(&self.bounds.span())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeOutlives {
    pub operand: LifetimeParameter,
    pub colon: Punctuation,
    pub bounds: UnionList<Lifetime>,
}

impl SyntaxTree for LifetimeOutlives {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (LifetimeParameter::parse, ':'.to_owned(), Lifetime::parse.union_list())
            .map(|(operand, colon, bounds)| Self { operand, colon, bounds })
            .parse(state_machine, handler)
    }
}

impl SourceElement for LifetimeOutlives {
    fn span(&self) -> GlobalSpan { self.operand.span().join(&self.bounds.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleOperand {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub r#type: r#type::Type,
}

impl SyntaxTree for TupleOperand {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (HigherRankedLifetimes::parse.or_none(), r#type::Type::parse)
            .map(|(higher_ranked_lifetimes, r#type)| Self {
                higher_ranked_lifetimes,
                r#type,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for TupleOperand {
    fn span(&self) -> GlobalSpan {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.r#type.span(),
            |h| h.span().join(&self.r#type.span()),
        )
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Predicate {
    TraitTypeEquality(TraitTypeEquality),
    Type(Type),
    Trait(Trait),
    Marker(Marker),
    LifetimeOutlives(LifetimeOutlives),
}

fn parse_predicate_starts_with_type(
    state_machine: &mut StateMachine,
    handler: &dyn Handler<error::Error>,
) -> parse::Result<Predicate> {
    enum Kind {
        Equality(r#type::Type),
        Bounds(UnionList<TypeBound>),
    }

    (
        (HigherRankedLifetimes::parse.or_none(), r#type::Type::parse),
        (
            ('='.to_owned(), r#type::Type::parse)
                .map(|(f, s)| (f, Kind::Equality(s))),
            (':'.to_owned(), TypeBound::parse.union_list())
                .map(|(f, s)| (f, Kind::Bounds(s))),
        )
            .branch(),
    )
        .map(|((hlt, ty), (punc, kind))| match kind {
            Kind::Equality(rhs_type) => {
                Predicate::TraitTypeEquality(TraitTypeEquality {
                    higher_ranked_lifetimes: hlt,
                    lhs_type: ty,
                    equals: punc,
                    rhs_type,
                })
            }
            Kind::Bounds(union_list) => Predicate::Type(Type {
                higher_ranked_lifetimes: hlt,
                r#type: ty,
                colon: punc,
                bounds: union_list,
            }),
        })
        .parse(state_machine, handler)
}

impl SyntaxTree for Predicate {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            parse_predicate_starts_with_type,
            Trait::parse.map(Self::Trait),
            Marker::parse.map(Self::Marker),
            LifetimeOutlives::parse.map(Self::LifetimeOutlives),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Predicate {
    fn span(&self) -> GlobalSpan {
        match self {
            Self::TraitTypeEquality(s) => s.span(),
            Self::Trait(s) => s.span(),
            Self::LifetimeOutlives(s) => s.span(),
            Self::Marker(s) => s.span(),
            Self::Type(s) => s.span(),
        }
    }
}

#[cfg(test)]
mod test;
