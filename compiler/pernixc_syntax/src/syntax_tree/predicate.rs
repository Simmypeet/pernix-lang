//! Contains the definition of all predicates syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
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
            LifetimeParameter::parse
                .enclosed_connected_list(','.to_owned(), Delimiter::Bracket),
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
        self.for_keyword.span.join(&self.lifetime_parameters.span()).unwrap()
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
    qualified_identifier: QualifiedIdentifier,
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    r#type: r#type::Type,
}

impl SyntaxTree for TraitTypeEquality {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            HigherRankedLifetimes::parse.or_none(),
            QualifiedIdentifier::parse,
            '='.to_owned(),
            r#type::Type::parse,
        )
            .map(
                |(
                    higher_ranked_lifetimes,
                    qualified_identifier,
                    equals,
                    r#type,
                )| {
                    Self {
                        higher_ranked_lifetimes,
                        qualified_identifier,
                        equals,
                        r#type,
                    }
                },
            )
            .parse(state_machine, handler)
    }
}

impl TraitTypeEquality {
    /// Dissolves the [`TraitTypeEquality`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Option<HigherRankedLifetimes>,
        QualifiedIdentifier,
        Punctuation,
        r#type::Type,
    ) {
        (
            self.higher_ranked_lifetimes,
            self.qualified_identifier,
            self.equals,
            self.r#type,
        )
    }
}

impl SourceElement for TraitTypeEquality {
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || {
                self.qualified_identifier
                    .span()
                    .join(&self.r#type.span())
                    .unwrap()
            },
            |h| h.span().join(&self.r#type.span()).unwrap(),
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

        first.join(&self.qualified_identifier.span()).unwrap()
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
    fn span(&self) -> Span {
        self.trait_keyword.span.join(&self.bounds.span()).unwrap()
    }
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

        begin.join(&self.qualified_identifier.span()).unwrap()
    }
}

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
        self.marker_keyword.span.join(&self.bounds.span()).unwrap()
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
/// OutlivesOperand:
///     LifetimeParameter
///     | Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum OutlivesOperand {
    LifetimeParameter(LifetimeParameter),
    Type(r#type::Type),
}

impl SyntaxTree for OutlivesOperand {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            LifetimeParameter::parse.map(OutlivesOperand::LifetimeParameter),
            r#type::Type::parse.map(OutlivesOperand::Type),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for OutlivesOperand {
    fn span(&self) -> Span {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => {
                lifetime_parameter.span()
            }
            Self::Type(ty) => ty.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Outlives:
///     OutlivesOperand ':' Lifetime ('+' Lifetime)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Outlives {
    #[get = "pub"]
    operand: OutlivesOperand,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    bounds: UnionList<Lifetime>,
}

impl SyntaxTree for Outlives {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (OutlivesOperand::parse, ':'.to_owned(), Lifetime::parse.union_list())
            .map(|(operand, colon, bounds)| Self { operand, colon, bounds })
            .parse(state_machine, handler)
    }
}

impl Outlives {
    /// Dissolves the [`Outlives`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (OutlivesOperand, Punctuation, UnionList<Lifetime>) {
        (self.operand, self.colon, self.bounds)
    }
}

impl SourceElement for Outlives {
    fn span(&self) -> Span {
        self.operand.span().join(&self.bounds.span()).unwrap()
    }
}

/// Syntax Synopsis:
/// ```txt
/// ConstantTypeBound:
///     HigherRankedLifetimes? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantTypeBound {
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    r#type: r#type::Type,
}

impl SyntaxTree for ConstantTypeBound {
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

impl SourceElement for ConstantTypeBound {
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.r#type.span(),
            |h| h.span().join(&self.r#type.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// ConstantType:
///     'const' Type ('+' Type)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConstantType {
    #[get = "pub"]
    const_keyword: Keyword,
    #[get = "pub"]
    bounds: UnionList<ConstantTypeBound>,
}

impl SyntaxTree for ConstantType {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Const.to_owned(), ConstantTypeBound::parse.union_list())
            .map(|(const_keyword, bounds)| Self { const_keyword, bounds })
            .parse(state_machine, handler)
    }
}

impl ConstantType {
    /// Dissolves the [`ConstantType`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UnionList<ConstantTypeBound>) {
        (self.const_keyword, self.bounds)
    }
}

impl SourceElement for ConstantType {
    fn span(&self) -> Span {
        self.const_keyword.span.join(&self.bounds.span()).unwrap()
    }
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
            |h| h.span().join(&self.r#type.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     'tuple' TupleOperand ('+' TupleOperand)*
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Tuple {
    #[get = "pub"]
    pub(super) tuple_keyword: Keyword,
    #[get = "pub"]
    pub(super) operands: UnionList<TupleOperand>,
}

impl SyntaxTree for Tuple {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Tuple.to_owned(), TupleOperand::parse.union_list())
            .map(|(tuple_keyword, operands)| Self { tuple_keyword, operands })
            .parse(state_machine, handler)
    }
}

impl Tuple {
    /// Dissolves the [`Tuple`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UnionList<TupleOperand>) {
        (self.tuple_keyword, self.operands)
    }
}

impl SourceElement for Tuple {
    fn span(&self) -> Span {
        self.tuple_keyword.span.join(&self.operands.span()).unwrap()
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
pub enum Predicate {
    TraitTypeEquality(TraitTypeEquality),
    Trait(Trait),
    Outlives(Outlives),
    ConstantType(ConstantType),
    Tuple(Tuple),
    Marker(Marker),
}

impl SyntaxTree for Predicate {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            TraitTypeEquality::parse.map(Self::TraitTypeEquality),
            Trait::parse.map(Self::Trait),
            Outlives::parse.map(Self::Outlives),
            ConstantType::parse.map(Self::ConstantType),
            Tuple::parse.map(Self::Tuple),
            Marker::parse.map(Self::Marker),
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
            Self::Outlives(s) => s.span(),
            Self::ConstantType(s) => s.span(),
            Self::Tuple(s) => s.span(),
            Self::Marker(s) => s.span(),
        }
    }
}

#[cfg(test)]
mod test;
