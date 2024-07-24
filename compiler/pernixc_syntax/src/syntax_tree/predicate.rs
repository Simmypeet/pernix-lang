//! Contains the definition of all predicates syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};

use super::{
    r#type, ConnectedList, ConstantArgument, DelimitedList, Lifetime,
    LifetimeParameter, QualifiedIdentifier,
};
use crate::{
    error::Error,
    parser::{DelimitedTree, Parser, Reading},
};

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
    /// Dissolves the [`TraitMemberBound`] into a tuple of its fields.
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
    left_bracket: Punctuation,
    #[get = "pub"]
    lifetime_parameter_list:
        Option<ConnectedList<LifetimeParameter, Punctuation>>,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for HigherRankedLifetimes {
    fn span(&self) -> Span {
        self.for_keyword.span.join(&self.right_bracket.span).unwrap()
    }
}

/// Syntax Synopsis:
/// ```txt
/// TraitBound:
///     HigherRankedLifetimes? `const`? QualifiedIdentifier?
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TraitBound {
    #[get = "pub"]
    higher_rankded_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    const_keyword: Option<Keyword>,
    #[get = "pub"]
    qualified_identifier: QualifiedIdentifier,
}

impl SourceElement for TraitBound {
    fn span(&self) -> Span {
        let first = self.higher_rankded_lifetimes.as_ref().map_or_else(
            || {
                self.const_keyword.as_ref().map_or_else(
                    || self.qualified_identifier.span(),
                    |k| k.span.clone(),
                )
            },
            SourceElement::span,
        );

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
            self.higher_rankded_lifetimes,
            self.const_keyword,
            self.qualified_identifier,
        )
    }
}

/// Similar to [`ConnectedList`] but specifically for list of constraints
/// separated by plus sings and has no trailing separator.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct BoundList<T> {
    /// The first element of the list.
    #[get = "pub"]
    first: T,

    /// The rest of the elements of the list.
    #[get = "pub"]
    rest: Vec<(Punctuation, T)>,
}

impl<T> BoundList<T> {
    /// Returns an iterator containing references to the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &T> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, t)| t))
    }

    /// Returns an iterator containing the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = T> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, t)| t))
    }
}

impl<T: SourceElement> SourceElement for BoundList<T> {
    fn span(&self) -> Span {
        let first = self.first.span();
        match self.rest.last() {
            Some(last) => first.join(&last.1.span()).unwrap(),
            None => first,
        }
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
    bounds: BoundList<TraitBound>,
}

impl Trait {
    /// Dissolves the [`Trait`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, BoundList<TraitBound>) {
        (self.trait_keyword, self.bounds)
    }
}

impl SourceElement for Trait {
    fn span(&self) -> Span {
        self.trait_keyword.span.join(&self.bounds.span()).unwrap()
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
    bounds: BoundList<Lifetime>,
}

impl Outlives {
    /// Dissolves the [`Outlives`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (OutlivesOperand, Punctuation, BoundList<Lifetime>) {
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
    bounds: BoundList<ConstantTypeBound>,
}

impl SourceElement for ConstantType {
    fn span(&self) -> Span {
        self.const_keyword.span.join(&self.bounds.span()).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TupleBound {
    #[get = "pub"]
    higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    #[get = "pub"]
    r#type: r#type::Type,
}

/// Syntax Synopsis:
/// ``` txt
/// TupleOperandKind:
///     Type
///     | ConstantArgument
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleOperandKind {
    Type(r#type::Type),
    Constant(ConstantArgument),
}

impl SourceElement for TupleOperandKind {
    fn span(&self) -> Span {
        match self {
            Self::Type(ty) => ty.span(),
            Self::Constant(c) => c.span(),
        }
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
    kind: TupleOperandKind,
}

impl SourceElement for TupleOperand {
    fn span(&self) -> Span {
        self.higher_ranked_lifetimes.as_ref().map_or_else(
            || self.kind.span(),
            |h| h.span().join(&self.kind.span()).unwrap(),
        )
    }
}

impl TupleOperand {
    /// Dissolves the [`TupleOperand`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Option<HigherRankedLifetimes>, TupleOperandKind) {
        (self.higher_ranked_lifetimes, self.kind)
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
    pub(super) operands: BoundList<TupleOperand>,
}

impl SourceElement for Tuple {
    fn span(&self) -> Span {
        self.tuple_keyword.span.join(&self.operands.span()).unwrap()
    }
}

impl Tuple {
    /// Dissolves the [`Tuple`] into a tuple of its fields.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, BoundList<TupleOperand>) {
        (self.tuple_keyword, self.operands)
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
        }
    }
}

impl<'a> Parser<'a> {
    fn parse_bound_list<T>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<BoundList<T>> {
        let first = parser(self)?;
        let mut rest = Vec::new();

        while let Some(plus) =
            self.try_parse(|parser| parser.parse_punctuation('+', true, &Dummy))
        {
            rest.push((plus, parser(self)?));
        }

        Some(BoundList { first, rest })
    }

    #[allow(clippy::option_option)]
    fn try_parse_higher_ranked_lifetimes(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Option<HigherRankedLifetimes>> {
        if let Reading::Unit(Token::Keyword(
            for_keyword @ Keyword { kind: KeywordKind::For, .. },
        )) = self.stop_at_significant()
        {
            // eat for keyword
            self.forward();

            let DelimitedList {
                open: left_bracket,
                list: lifetime_parameter_list,
                close: right_bracket,
            } = self.parse_delimited_list(
                Delimiter::Bracket,
                ',',
                |parser| {
                    let apostrophe =
                        parser.parse_punctuation('\'', true, handler)?;
                    let identifier = parser.parse_identifier(handler)?;

                    Some(LifetimeParameter { apostrophe, identifier })
                },
                handler,
            )?;

            Some(Some(HigherRankedLifetimes {
                for_keyword,
                left_bracket,
                lifetime_parameter_list,
                right_bracket,
            }))
        } else {
            Some(None)
        }
    }

    /// Parses a [`Predicate`]
    #[allow(clippy::too_many_lines)]
    pub fn parse_predicate(
        &mut self,
        handler: &dyn Handler<Error>,
    ) -> Option<Predicate> {
        match self.stop_at_significant() {
            // parse constant type predicate
            Reading::Unit(Token::Keyword(
                const_keyword @ Keyword { kind: KeywordKind::Const, .. },
            )) => {
                // eat const keyword
                self.forward();

                let bounds = self.parse_bound_list(|parser| {
                    let higher_ranked_lifetimes =
                        parser.try_parse_higher_ranked_lifetimes(handler)?;
                    let r#type = parser.parse_type(handler)?;

                    Some(ConstantTypeBound { higher_ranked_lifetimes, r#type })
                })?;

                Some(Predicate::ConstantType(ConstantType {
                    const_keyword,
                    bounds,
                }))
            }

            // parse tuple predicate
            Reading::Unit(Token::Keyword(
                tuple_keyword @ Keyword { kind: KeywordKind::Tuple, .. },
            )) => {
                // eat tuple keyword
                self.forward();

                let operands = self.parse_bound_list(|parser| {
                    let higher_ranked_lifetimes =
                        parser.try_parse_higher_ranked_lifetimes(handler)?;

                    let kind = match parser.stop_at_significant() {
                        Reading::IntoDelimited(Delimiter::Brace, _) => {
                            let DelimitedTree {
                                open: left_brace,
                                tree,
                                close: right_brace,
                            } = parser.step_into(
                                Delimiter::Brace,
                                |parser| {
                                    parser
                                        .parse_expression(handler)
                                        .map(Box::new)
                                },
                                handler,
                            )?;

                            TupleOperandKind::Constant(ConstantArgument {
                                left_brace,
                                expression: tree?,
                                right_brace,
                            })
                        }

                        _ => parser
                            .parse_type(handler)
                            .map(TupleOperandKind::Type)?,
                    };

                    Some(TupleOperand { higher_ranked_lifetimes, kind })
                })?;

                Some(Predicate::Tuple(Tuple { tuple_keyword, operands }))
            }

            // parse trait predicate
            Reading::Unit(Token::Keyword(
                trait_keyword @ Keyword { kind: KeywordKind::Trait, .. },
            )) => {
                // eat trait keyword
                self.forward();

                let bounds = self.parse_bound_list(|parser| {
                    let higher_rankded_lifetimes =
                        parser.try_parse_higher_ranked_lifetimes(handler)?;

                    let const_keyword = match parser.stop_at_significant() {
                        Reading::Unit(Token::Keyword(
                            const_keyword @ Keyword {
                                kind: KeywordKind::Const,
                                ..
                            },
                        )) => {
                            // eat const keyword
                            parser.forward();

                            Some(const_keyword)
                        }

                        _ => None,
                    };

                    let qualified_identifier =
                        parser.parse_qualified_identifier(handler)?;

                    Some(TraitBound {
                        higher_rankded_lifetimes,
                        const_keyword,
                        qualified_identifier,
                    })
                })?;

                Some(Predicate::Trait(Trait { trait_keyword, bounds }))
            }

            // parse outlives predicate starting with lifetime
            Reading::Unit(Token::Punctuation(Punctuation {
                punctuation: '\'',
                ..
            })) => {
                let lifetime = self.parse_lifetime_parameter(handler)?;

                let colon = self.parse_punctuation(':', true, handler)?;
                let bounds = self.parse_bound_list(|parser| {
                    parser.parse_lifetime(handler)
                })?;

                Some(Predicate::Outlives(Outlives {
                    operand: OutlivesOperand::LifetimeParameter(lifetime),
                    colon,
                    bounds,
                }))
            }

            // parse trait type equality starting with for all lifetimes
            Reading::Unit(Token::Keyword(Keyword {
                kind: KeywordKind::For,
                ..
            })) => {
                let higher_ranked_lifetimes =
                    self.try_parse_higher_ranked_lifetimes(handler)?;

                let qualified_identifier =
                    self.parse_qualified_identifier(handler)?;

                let equals = self.parse_punctuation('=', true, handler)?;
                let ty = self.parse_type(handler)?;

                Some(Predicate::TraitTypeEquality(TraitTypeEquality {
                    higher_ranked_lifetimes,
                    qualified_identifier,
                    equals,
                    r#type: ty,
                }))
            }

            _ => {
                let first_ty = self.parse_type(handler)?;

                match (first_ty, self.stop_at_significant()) {
                    // parse member constraint
                    (
                        r#type::Type::QualifiedIdentifier(qualified_identifier),
                        Reading::Unit(Token::Punctuation(equals)),
                    ) if equals.punctuation == '=' => {
                        // eat equals
                        self.forward();
                        let ty = self.parse_type(handler)?;

                        Some(Predicate::TraitTypeEquality(TraitTypeEquality {
                            higher_ranked_lifetimes: None,
                            qualified_identifier,
                            equals,
                            r#type: ty,
                        }))
                    }

                    (first_ty, _) => {
                        let colon =
                            self.parse_punctuation(':', true, handler)?;
                        let bounds = self.parse_bound_list(|parser| {
                            parser.parse_lifetime(handler)
                        })?;

                        Some(Predicate::Outlives(Outlives {
                            operand: OutlivesOperand::Type(first_ty),
                            colon,
                            bounds,
                        }))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
