use pernixc_lexical::kind;
use proptest::prelude::{Arbitrary, BoxedStrategy, Strategy as _};

use crate::{
    arbitrary::{
        GenericIdentifier, IndentDisplay, IntoSeparated, QualifiedIdentifier,
        QualifiedIdentifierRoot, reference,
    },
    predicate::arbitrary::HigherRankedLifetimes,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Trait for super::Trait {
        pub higher_ranked_lifetimes (Option<HigherRankedLifetimes>),
        pub qualified_identifier (QualifiedIdentifier),
    }
}

impl IndentDisplay for Trait {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, qualified_identifier)| Self {
                higher_ranked_lifetimes,
                qualified_identifier,
            })
            .boxed()
    }
}

impl Trait {
    /// Create a strategy that generates a simple `Trait` with no `::` path
    /// in the qualified identifier. This avoids ambiguity in regular
    /// (non-parenthesized) context parameters.
    pub fn arbitrary_simple() -> BoxedStrategy<Self> {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            // Generate only GenericIdentifier root with no subsequences
            GenericIdentifier::arbitrary().prop_map(|gi| QualifiedIdentifier {
                root: QualifiedIdentifierRoot::GenericIdentifier(gi),
                subsequences: vec![],
            }),
        )
            .prop_map(|(higher_ranked_lifetimes, qualified_identifier)| Self {
                higher_ranked_lifetimes,
                qualified_identifier,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ContextParameterWithName for super::ContextParameterWithName {
        #{map_input_assert(name, &name.kind)}
        pub name (kind::Identifier),
        pub r#trait (Trait),
    }
}

impl IndentDisplay for ContextParameterWithName {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}: ", self.name)?;
        self.r#trait.indent_fmt(f, indent)
    }
}

impl Arbitrary for ContextParameterWithName {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (kind::Identifier::arbitrary(), Trait::arbitrary())
            .prop_map(|(name, r#trait)| Self { name, r#trait })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ContextParameterWithoutName for super::ContextParameterWithoutName {
        pub r#trait (Trait),
    }
}

impl IndentDisplay for ContextParameterWithoutName {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.r#trait.indent_fmt(f, indent)
    }
}

impl Arbitrary for ContextParameterWithoutName {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Trait::arbitrary().prop_map(|r#trait| Self { r#trait }).boxed()
    }
}

impl ContextParameterWithoutName {
    /// Create a strategy that generates a `ContextParameterWithoutName` with
    /// a simple path (no `::`) to avoid grammar ambiguity.
    pub fn arbitrary_simple() -> BoxedStrategy<Self> {
        Trait::arbitrary_simple().prop_map(|r#trait| Self { r#trait }).boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum ContextParameter for super::ContextParameter {
        WithoutName(ContextParameterWithoutName),
        WithName(ContextParameterWithName),
    }
}

impl IndentDisplay for ContextParameter {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::WithoutName(p) => p.indent_fmt(f, indent),
            Self::WithName(p) => p.indent_fmt(f, indent),
        }
    }
}

impl Arbitrary for ContextParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            ContextParameterWithoutName::arbitrary()
                .prop_map(Self::WithoutName),
            ContextParameterWithName::arbitrary().prop_map(Self::WithName),
        ]
        .boxed()
    }
}

impl ContextParameter {
    /// Create a strategy for use in regular (non-parenthesized) context
    /// parameters. To avoid ambiguity where `a::b` could be misinterpreted
    /// as `a:` `:b`, we either:
    /// 1. Use `WithName` variant (explicit `name: trait`), OR
    /// 2. Use `WithoutName` with simple paths without `::`
    pub fn arbitrary_for_regular() -> BoxedStrategy<Self> {
        proptest::prop_oneof![
            ContextParameterWithName::arbitrary().prop_map(Self::WithName),
            ContextParameterWithoutName::arbitrary_simple()
                .prop_map(Self::WithoutName),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ContextParameters for super::ContextParameters {
        pub parameters (Vec<ContextParameter>),
    }
}

impl IndentDisplay for ContextParameters {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.parameters.into_separated(", ").indent_fmt(f, indent)
    }
}

impl Arbitrary for ContextParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        // For regular (non-parenthesized) context parameters, we need to avoid
        // ambiguity where `a::b` could be misinterpreted as `a:` followed by
        // `:b`. We do this by either:
        // 1. Using `WithName` variant (`name: trait`), OR
        // 2. Using `WithoutName` with simple paths without `::`
        proptest::collection::vec(
            ContextParameter::arbitrary_for_regular(),
            1..=6,
        )
        .prop_map(|parameters| Self { parameters })
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ParenthesizedContextParameters for super::ParenthesizedContextParameters {
        pub parameters (Vec<ContextParameter>),
    }
}

impl IndentDisplay for ParenthesizedContextParameters {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "(")?;
        self.parameters.into_separated(", ").indent_fmt(f, indent)?;
        write!(f, ")")
    }
}

impl Arbitrary for ParenthesizedContextParameters {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        // Same constraint as ContextParameters - when there's no name, we need
        // simple paths to avoid the `a::b` being misinterpreted as `a:` `:b`.
        proptest::collection::vec(
            ContextParameter::arbitrary_for_regular(),
            1..=6,
        )
        .prop_map(|parameters| Self { parameters })
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum ContextParametersKind for super::ContextParametersKind {
        Regular(ContextParameters),
        Parenthesized(ParenthesizedContextParameters),
    }
}

impl IndentDisplay for ContextParametersKind {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Regular(params) => params.indent_fmt(f, indent),
            Self::Parenthesized(params) => params.indent_fmt(f, indent),
        }
    }
}

impl Arbitrary for ContextParametersKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![
            ContextParameters::arbitrary().prop_map(Self::Regular),
            ParenthesizedContextParameters::arbitrary()
                .prop_map(Self::Parenthesized),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct UsingClause for super::UsingClause {
        pub context_parameters (ContextParametersKind),
    }
}

impl IndentDisplay for UsingClause {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "using ")?;
        self.context_parameters.indent_fmt(f, indent)
    }
}

impl Arbitrary for UsingClause {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        ContextParametersKind::arbitrary()
            .prop_map(|context_parameters| Self { context_parameters })
            .boxed()
    }
}
