//! Contains the definition of all predicates syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::source_file::{SourceElement, Span};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};

use super::{
    delimited_list, r#type, DelimitedList, Lifetime, LifetimeParameter, Parse,
    QualifiedIdentifier, UnionList,
};
use crate::parser::Syntax;

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
    lifetime_parameters: DelimitedList<LifetimeParameter>,
}

impl Parse for HigherRankedLifetimes {
    fn syntax() -> impl Syntax<Output = Self> {
        (KeywordKind::For, delimited_list(Delimiter::Bracket, ',')).map(
            |(for_keyword, lifetime_parameters)| HigherRankedLifetimes {
                for_keyword,
                lifetime_parameters,
            },
        )
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

impl Parse for TraitTypeEquality {
    fn syntax() -> impl Syntax<Output = Self> {
        (
            HigherRankedLifetimes::syntax().or_none(),
            QualifiedIdentifier::syntax(),
            '=',
            r#type::Type::syntax(),
        )
            .map(
                |(
                    higher_ranked_lifetimes,
                    qualified_identifier,
                    equals,
                    r#type,
                )| TraitTypeEquality {
                    higher_ranked_lifetimes,
                    qualified_identifier,
                    equals,
                    r#type,
                },
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

impl Parse for TraitBound {
    fn syntax() -> impl Syntax<Output = Self> {
        (
            '!'.or_none(),
            HigherRankedLifetimes::syntax().or_none(),
            KeywordKind::Const.or_none(),
            QualifiedIdentifier::syntax(),
        )
            .map(
                |(
                    negation,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| {
                    TraitBound {
                        negation,
                        higher_ranked_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    }
                },
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

impl Parse for Trait {
    fn syntax() -> impl Syntax<Output = Self> {
        (KeywordKind::Trait, UnionList::syntax())
            .map(|(trait_keyword, bounds)| Trait { trait_keyword, bounds })
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

impl Parse for MarkerBound {
    fn syntax() -> impl Syntax<Output = Self> {
        (
            '!'.or_none(),
            HigherRankedLifetimes::syntax().or_none(),
            QualifiedIdentifier::syntax(),
        )
            .map(
                |(negation, higher_ranked_lifetimes, qualified_identifier)| {
                    MarkerBound {
                        negation,
                        higher_ranked_lifetimes,
                        qualified_identifier,
                    }
                },
            )
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

impl Parse for Marker {
    fn syntax() -> impl Syntax<Output = Self> {
        (KeywordKind::Marker, UnionList::syntax())
            .map(|(marker_keyword, bounds)| Marker { marker_keyword, bounds })
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

impl Parse for Outlives {
    fn syntax() -> impl Syntax<Output = Self> {
        (OutlivesOperand::syntax(), ':', UnionList::syntax())
            .map(|(operand, colon, bounds)| Outlives { operand, colon, bounds })
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

impl Parse for OutlivesOperand {
    fn syntax() -> impl Syntax<Output = Self> {
        LifetimeParameter::syntax()
            .map(OutlivesOperand::LifetimeParameter)
            .or_else(r#type::Type::syntax().map(OutlivesOperand::Type))
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

impl Parse for ConstantTypeBound {
    fn syntax() -> impl Syntax<Output = Self> {
        (HigherRankedLifetimes::syntax().or_none(), r#type::Type::syntax()).map(
            |(higher_ranked_lifetimes, r#type)| ConstantTypeBound {
                higher_ranked_lifetimes,
                r#type,
            },
        )
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

impl Parse for ConstantType {
    fn syntax() -> impl Syntax<Output = Self> {
        (KeywordKind::Const, UnionList::syntax()).map(
            |(const_keyword, bounds)| ConstantType { const_keyword, bounds },
        )
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

impl Parse for TupleOperand {
    fn syntax() -> impl Syntax<Output = Self> {
        (HigherRankedLifetimes::syntax().or_none(), r#type::Type::syntax()).map(
            |(higher_ranked_lifetimes, r#type)| TupleOperand {
                higher_ranked_lifetimes,
                r#type,
            },
        )
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

impl TupleOperand {
    /// Dissolves the [`TupleOperand`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Option<HigherRankedLifetimes>, r#type::Type) {
        (self.higher_ranked_lifetimes, self.r#type)
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

impl Parse for Tuple {
    fn syntax() -> impl Syntax<Output = Self> {
        (KeywordKind::Tuple, UnionList::syntax())
            .map(|(tuple_keyword, operands)| Tuple { tuple_keyword, operands })
    }
}

impl SourceElement for Tuple {
    fn span(&self) -> Span {
        self.tuple_keyword.span.join(&self.operands.span()).unwrap()
    }
}

impl Tuple {
    /// Dissolves the [`Tuple`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, UnionList<TupleOperand>) {
        (self.tuple_keyword, self.operands)
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

impl Parse for Predicate {
    fn syntax() -> impl Syntax<Output = Self> {
        TraitTypeEquality::syntax()
            .map(Predicate::TraitTypeEquality)
            .or_else(Trait::syntax().map(Predicate::Trait))
            .or_else(Outlives::syntax().map(Predicate::Outlives))
            .or_else(ConstantType::syntax().map(Predicate::ConstantType))
            .or_else(Tuple::syntax().map(Predicate::Tuple))
            .or_else(Marker::syntax().map(Predicate::Marker))
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
