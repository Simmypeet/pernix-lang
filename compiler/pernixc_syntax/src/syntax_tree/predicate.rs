//! Contains the definition of all predicates syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::DelimiterKind,
};
use pernixc_source_file::{SourceElement, Span};

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

/// Syntax Synopsis:
/// ``` txt
/// HigherRankedLifetimes:
///     'for' '[' (LifetimeParameter (',' LifetimeParameter)* ','?)? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct HigherRankedLifetimes {
    #[get = "pub"]
    for_keyword: Keyword,
    #[get = "pub"]
    lifetime_parameters: EnclosedConnectedList<LifetimeParameter, Punctuation>,
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
    fn span(&self) -> Span {
        self.for_keyword.span.join(&self.lifetime_parameters.span())
    }
}

/// Syntax Synopsis:
/// ``` txt
/// QualifiedIdentifierBound:
///     '!'? HigherRankedLifetimes? `const`? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct QualifiedIdentifierBound {
    #[get = "pub"]
    negation: Option<Punctuation>,
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for QualifiedIdentifierBound {
    fn span(&self) -> Span {
        let first = match (
            &self.negation,
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

impl QualifiedIdentifierBound {
    /// Dissolves the [`QualifiedIdentifierBound`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Option<Punctuation>,
        Option<HigherRankedLifetimes>,
        Option<Keyword>,
        QualifiedIdentifier,
    ) {
        (
            self.negation,
            self.higher_ranked_lifetimes,
            self.const_keyword,
            self.qualified_identifier,
        )
    }
}

impl SyntaxTree for QualifiedIdentifierBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '!'.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            KeywordKind::Const.to_owned().or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    negation,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| {
                    Self {
                        negation,
                        higher_ranked_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TypeBound:
///     QualifiedIdentifierBound
///     | 'const'
///     | 'tuple'
///     | Lifetime
///     ;
/// ```
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
    fn span(&self) -> Span {
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

/// Syntax Synopsis:
/// ``` txt
/// Type:
///     HigherRankedLifetimes? Type ':' (TypeBound ('+' TypeBound)*)?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Type {
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    r#type: r#type::Type,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    bounds: UnionList<TypeBound>,
}

impl Type {
    /// Dissolves the [`Type`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Option<HigherRankedLifetimes>,
        r#type::Type,
        Punctuation,
        UnionList<TypeBound>,
    ) {
        (self.higher_ranked_lifetimes, self.r#type, self.colon, self.bounds)
    }
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
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.r#type.span().join(&self.bounds.span()),
            |h| h.span().join(&self.bounds.span()),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// TraitTypeEquality:
///     HigherRankedLifetimes? QualifiedIdentifier '=' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitTypeEquality {
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    lhs_type: r#type::Type,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    rhs_type: r#type::Type,
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

impl TraitTypeEquality {
    /// Dissolves the [`TraitTypeEquality`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Option<HigherRankedLifetimes>, r#type::Type, Punctuation, r#type::Type)
    {
        (
            self.higher_ranked_lifetimes,
            self.lhs_type,
            self.equals,
            self.rhs_type,
        )
    }
}

impl SourceElement for TraitTypeEquality {
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.lhs_type.span().join(&self.lhs_type.span()),
            |h| h.span().join(&self.lhs_type.span()),
        )
    }
}

/// Syntax Synopsis:
/// ```txt
/// TraitBound:
///     '!'? HigherRankedLifetimes? `const`? QualifiedIdentifier?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitBound {
    #[get = "pub"]
    negation: Option<Punctuation>,
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SyntaxTree for TraitBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '!'.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            KeywordKind::Const.to_owned().or_none(),
            QualifiedIdentifier::parse,
        )
            .map(
                |(
                    negation,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| {
                    Self {
                        negation,
                        higher_ranked_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl TraitBound {
    /// Dissolves the [`TraitBound`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Option<HigherRankedLifetimes>, Option<Keyword>, QualifiedIdentifier)
    {
        (
            self.higher_ranked_lifetimes,
            self.const_keyword,
            self.qualified_identifier,
        )
    }
}

impl SourceElement for TraitBound {
    fn span(&self) -> Span {
        let first = match (
            &self.negation,
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

/// Syntax Synopsis:
/// ``` txt
/// TraitBound:
///     'trait' TraitBound ('+' TraitBound)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Trait {
    #[get = "pub"]
    trait_keyword: Keyword,
    #[get = "pub"]
    bounds: UnionList<TraitBound>,
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

impl Trait {
    /// Dissolves the [`Trait`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UnionList<TraitBound>) {
        (self.trait_keyword, self.bounds)
    }
}

impl SourceElement for Trait {
    fn span(&self) -> Span { self.trait_keyword.span.join(&self.bounds.span()) }
}

/// ```txt
/// MarkerBound:
///     '!'? HigherRankedLifetimes? QualifiedIdentifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MarkerBound {
    #[get = "pub"]
    negation: Option<Punctuation>,
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SyntaxTree for MarkerBound {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '!'.to_owned().or_none(),
            HigherRankedLifetimes::parse.or_none(),
            QualifiedIdentifier::parse,
        )
            .map(|(negation, higher_ranked_lifetimes, qualified_identifier)| {
                Self { negation, higher_ranked_lifetimes, qualified_identifier }
            })
            .parse(state_machine, handler)
    }
}

impl MarkerBound {
    /// Dissolves the [`MarkerBound`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (Option<Punctuation>, Option<HigherRankedLifetimes>, QualifiedIdentifier)
    {
        (self.negation, self.higher_ranked_lifetimes, self.qualified_identifier)
    }
}

impl SourceElement for MarkerBound {
    fn span(&self) -> Span {
        let begin = self.negation.as_ref().map_or_else(
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

/// Syntax Synopsis:
/// ``` txt
/// Marker:
///     'marker' MarkerBound ('+' MarkerBound)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Marker {
    #[get = "pub"]
    marker_keyword: Keyword,
    #[get = "pub"]
    bounds: UnionList<MarkerBound>,
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
    fn span(&self) -> Span {
        self.marker_keyword.span.join(&self.bounds.span())
    }
}

impl Marker {
    /// Dissolves the [`Marker`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UnionList<MarkerBound>) {
        (self.marker_keyword, self.bounds)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Outlives:
///     OutlivesOperand ':' Lifetime ('+' Lifetime)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LifetimeOutlives {
    #[get = "pub"]
    operand: Lifetime,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    bounds: UnionList<Lifetime>,
}

impl SyntaxTree for LifetimeOutlives {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Lifetime::parse, ':'.to_owned(), Lifetime::parse.union_list())
            .map(|(operand, colon, bounds)| Self { operand, colon, bounds })
            .parse(state_machine, handler)
    }
}

impl LifetimeOutlives {
    /// Dissolves the [`LifetimeOutlives`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Lifetime, Punctuation, UnionList<Lifetime>) {
        (self.operand, self.colon, self.bounds)
    }
}

impl SourceElement for LifetimeOutlives {
    fn span(&self) -> Span { self.operand.span().join(&self.bounds.span()) }
}

/// Syntax Synopsis:
/// ``` txt
/// TupleOperand:
///     HigherRankedLifetimes? TupleOperandKind
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleOperand {
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    r#type: r#type::Type,
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

impl TupleOperand {
    /// Dissolves the [`TupleOperand`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Option<HigherRankedLifetimes>, r#type::Type) {
        (self.higher_ranked_lifetimes, self.r#type)
    }
}

impl SourceElement for TupleOperand {
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.r#type.span(),
            |h| h.span().join(&self.r#type.span()),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Predicate:
///     TraitTypeEquality
///     | Trait
///     | Outlives
///     | ConstantType
///     | Tuple
///     ;
/// ```
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
    fn span(&self) -> Span {
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
