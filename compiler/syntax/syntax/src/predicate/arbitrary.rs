use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{
        IndentDisplay, IntoSeparated, Lifetime, LifetimeParameter,
        QualifiedIdentifier,
    },
    reference, r#type,
};

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "[{}]",
        self.lifetimes
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )]
    pub struct LifetimeParameters for super::LifetimeParameters {
        pub lifetimes (Vec<LifetimeParameter>)
    }
}

impl Arbitrary for LifetimeParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(LifetimeParameter::arbitrary(), 1..10)
            .prop_map(|lifetimes| Self { lifetimes })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("for{lifetimes}")]
    pub struct HigherRankedLifetimes for super::HigherRankedLifetimes {
        pub lifetimes (LifetimeParameters)
    }
}

impl Arbitrary for HigherRankedLifetimes {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        LifetimeParameters::arbitrary()
            .prop_map(|lifetimes| Self { lifetimes })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct QualifiedIdentifierBound for super::QualifiedIdentifierBound {
        pub not_keyword (bool),
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub const_keyword (bool),
        pub qualified_identifier (QualifiedIdentifier)
    }
}

impl IndentDisplay for QualifiedIdentifierBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.not_keyword {
            write!(f, "not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        if self.const_keyword {
            write!(f, "const ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)?;

        Ok(())
    }
}

impl Arbitrary for QualifiedIdentifierBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            proptest::bool::ANY,
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(
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
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct TraitBound for super::TraitBound {
        pub not_keyword (bool),
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub const_keyword (bool),
        pub qualified_identifier (QualifiedIdentifier)
    }
}

impl IndentDisplay for TraitBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.not_keyword {
            write!(f, "not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        if self.const_keyword {
            write!(f, "const ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)?;

        Ok(())
    }
}

impl Arbitrary for TraitBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            proptest::bool::ANY,
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(
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
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Trait for super::Trait {
        pub bounds (Vec<TraitBound>)
    }
}

impl IndentDisplay for Trait {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "trait ")?;
        self.bounds.into_separated(" + ").indent_fmt(f, indent)
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(TraitBound::arbitrary(), 1..10)
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum TypeBound for super::TypeBound {
        QualifiedIdentifier(QualifiedIdentifierBound),
        Outlives(Lifetime),

        #{prop_assert(|x| true)}
        Tuple,

        #{prop_assert(|x| true)}
        Const,
    }
}

impl IndentDisplay for TypeBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::QualifiedIdentifier(bound) => bound.indent_fmt(f, indent),
            Self::Outlives(lifetime) => write!(f, "{lifetime}"),
            Self::Tuple => write!(f, "tuple"),
            Self::Const => write!(f, "const"),
        }
    }
}

impl Arbitrary for TypeBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            QualifiedIdentifierBound::arbitrary()
                .prop_map(Self::QualifiedIdentifier),
            Lifetime::arbitrary().prop_map(Self::Outlives),
            Just(Self::Tuple),
            Just(Self::Const),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Type for super::Type {
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub r#type (r#type::arbitrary::Type),
        pub bounds (Vec<TypeBound>)
    }
}

impl IndentDisplay for Type {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.r#type.indent_fmt(f, indent)?;

        write!(f, ": ")?;

        self.bounds.into_separated(" + ").indent_fmt(f, indent)
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            r#type::arbitrary::Type::arbitrary(),
            proptest::collection::vec(TypeBound::arbitrary(), 1..10),
        )
            .prop_map(|(higher_ranked_lifetimes, r#type, bounds)| Self {
                higher_ranked_lifetimes,
                r#type,
                bounds,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct TraitTypeEquality for super::TraitTypeEquality {
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        #{map_input_assert(lhs, &**lhs)}
        pub lhs (r#type::arbitrary::Type),
        #{map_input_assert(rhs, &**rhs)}
        pub rhs (r#type::arbitrary::Type),
    }
}

impl Arbitrary for TraitTypeEquality {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            r#type::arbitrary::Type::arbitrary(),
            r#type::arbitrary::Type::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, lhs, rhs)| Self {
                higher_ranked_lifetimes,
                lhs,
                rhs,
            })
            .boxed()
    }
}

impl IndentDisplay for TraitTypeEquality {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.lhs.indent_fmt(f, indent)?;

        write!(f, " = ")?;

        self.rhs.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct MarkerBound for super::MarkerBound {
        pub not_keyword (bool),
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub qualified_identifier (QualifiedIdentifier)
    }
}

impl IndentDisplay for MarkerBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.not_keyword {
            write!(f, "not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)?;

        Ok(())
    }
}

impl Arbitrary for MarkerBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::bool::ANY,
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(
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
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Marker for super::Marker {
        pub bounds (Vec<MarkerBound>)
    }
}

impl IndentDisplay for Marker {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "marker ")?;
        self.bounds.into_separated(" + ").indent_fmt(f, indent)
    }
}

impl Arbitrary for Marker {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(MarkerBound::arbitrary(), 1..10)
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct LifetimeOutlives for super::LifetimeOutlives {
        pub operand (LifetimeParameter),
        pub bounds (Vec<Lifetime>),
    }
}

impl Display for LifetimeOutlives {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.operand, self.bounds.into_separated(" + "))
    }
}

impl Arbitrary for LifetimeOutlives {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            LifetimeParameter::arbitrary(),
            proptest::collection::vec(Lifetime::arbitrary(), 1..10),
        )
            .prop_map(|(operand, bounds)| Self { operand, bounds })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum Predicate for super::Predicate {
        LifetimeOutlives(LifetimeOutlives),
        Trait(Trait),
        Marker(Marker),
        TraitTypeEquality(TraitTypeEquality),
    }
}

impl IndentDisplay for Predicate {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::LifetimeOutlives(lifetime_outlives) => {
                write!(f, "{lifetime_outlives}")
            }
            Self::Trait(trait_) => trait_.indent_fmt(f, indent),
            Self::Marker(marker) => marker.indent_fmt(f, indent),
            Self::TraitTypeEquality(trait_type_equality) => {
                trait_type_equality.indent_fmt(f, indent)
            }
        }
    }
}

impl Arbitrary for Predicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeOutlives::arbitrary().prop_map(Self::LifetimeOutlives),
            Trait::arbitrary().prop_map(Self::Trait),
            Marker::arbitrary().prop_map(Self::Marker),
            TraitTypeEquality::arbitrary().prop_map(Self::TraitTypeEquality),
        ]
        .boxed()
    }
}
