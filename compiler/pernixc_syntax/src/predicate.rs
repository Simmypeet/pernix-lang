use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree::{self, Tag},
    expect,
    parser::{ast, Parser as _},
};

use crate::{
    r#type, Keyword, Lifetime, LifetimeParameter, Punctuation,
    QualifiedIdentifier,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct HigherRankedLifetimes {
        pub for_keyword: Keyword = expect::Keyword::For,
        pub lifetimes: LifetimeParameters = ast::<LifetimeParameters>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct LifetimeParameters {
        pub lifetimes: #[multi] Lifetime
            = ast::<Lifetime>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct QualifiedIdentifierBound {
        pub not_keyword: Keyword = expect::Keyword::Not.optional(),
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub const_keyword: Keyword = expect::Keyword::Const.optional(),
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum TypeBound {
        QualifiedIdentifier(
            QualifiedIdentifierBound = ast::<QualifiedIdentifierBound>()
        ),
        Const(Keyword = expect::Keyword::Const),
        Outlives(Lifetime = ast::<Lifetime>()),
        Tuple(Keyword = expect::Keyword::Tuple),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Type {
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub r#type: r#type::Type = ast::<r#type::Type>(),
        pub colon: Punctuation = ':',
        pub bounds: #[multi] TypeBound
            = ast::<TypeBound>().repeat_with_separator_at_least_once('+'),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TraitTypeEquality {
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub lhs: Tag<r#type::Type, 1> = ast::<Tag<r#type::Type, 1>>(),
        pub equals: Punctuation = '=',
        pub rhs: Tag<r#type::Type, 2> = ast::<Tag<r#type::Type, 2>>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TraitBound {
        pub not_keyword: Keyword = expect::Keyword::Not.optional(),
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub const_keyword: Keyword = expect::Keyword::Const.optional(),
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Trait {
        pub trait_keyword: Keyword = expect::Keyword::Trait,
        pub bounds: #[multi] TraitBound
            = ast::<TraitBound>().repeat_with_separator_at_least_once('+'),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MarkerBound {
        pub not_keyword: Keyword = expect::Keyword::Not.optional(),
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Marker {
        pub marker_keyword: Keyword = expect::Keyword::Marker,
        pub bounds: #[multi] MarkerBound
            = ast::<MarkerBound>().repeat_with_separator_at_least_once('+'),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LifetimeOutlives {
        pub operand: LifetimeParameter = ast::<LifetimeParameter>(),
        pub colon: Punctuation = ':',
        pub bounds: #[multi] Lifetime
            = ast::<Lifetime>().repeat_with_separator_at_least_once('+'),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Predicate {
        TraitTypeEquality(
            TraitTypeEquality = ast::<TraitTypeEquality>()
        ),
        Type(
            Type = ast::<Type>()
        ),
        Trait(
            Trait = ast::<Trait>()
        ),
        Marker(
            Marker = ast::<Marker>()
        ),
        LifetimeOutlives(
            LifetimeOutlives = ast::<LifetimeOutlives>()
        ),
    }
}

#[cfg(test)]
mod test;
