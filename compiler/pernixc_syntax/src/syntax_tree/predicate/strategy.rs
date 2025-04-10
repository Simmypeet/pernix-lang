use std::fmt::{Debug, Display, Write};

use pernixc_test_input::Input;
use proptest::{
    arbitrary::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    r#type::strategy::Type as TypeTerm,
    strategy::{
        ConnectedList, ConstantPunctuation, IndentDisplay, Lifetime,
        LifetimeParameter, QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimes {
    pub lifetime_parameter_list:
        Option<ConnectedList<LifetimeParameter, ConstantPunctuation<','>>>,
}

impl Input<&super::HigherRankedLifetimes> for &HigherRankedLifetimes {
    fn assert(self, output: &super::HigherRankedLifetimes) -> TestCaseResult {
        self.lifetime_parameter_list
            .as_ref()
            .assert(output.lifetime_parameters.connected_list.as_ref())
    }
}

impl Arbitrary for HigherRankedLifetimes {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            LifetimeParameter::arbitrary(),
            ConstantPunctuation::arbitrary(),
        ))
        .prop_map(|lifetime_parameter_list| Self { lifetime_parameter_list })
        .boxed()
    }
}

impl IndentDisplay for HigherRankedLifetimes {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        f.write_str("for[")?;

        if let Some(lifetime_parameter_list) = &self.lifetime_parameter_list {
            Display::fmt(lifetime_parameter_list, f)?;
        }

        f.write_char(']')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    pub negation: bool,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub const_keyword: bool,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input<&super::TraitBound> for &TraitBound {
    fn assert(self, output: &super::TraitBound) -> TestCaseResult {
        prop_assert_eq!(self.negation, output.not_keyword.is_some());
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes.as_ref())?;
        prop_assert_eq!(self.const_keyword, output.const_keyword.is_some());
        self.qualified_identifier.assert(&output.qualified_identifier)
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
                    negation,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| Self {
                    negation,
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                },
            )
            .boxed()
    }
}

impl IndentDisplay for TraitBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.negation {
            f.write_str("not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            higher_ranked_lifetimes.indent_fmt(f, indent)?;
        }

        if self.const_keyword {
            f.write_str("const ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MarkerBound {
    pub negation: bool,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input<&super::MarkerBound> for &MarkerBound {
    fn assert(self, output: &super::MarkerBound) -> TestCaseResult {
        prop_assert_eq!(self.negation, output.not_keyword.is_some());
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes.as_ref())?;
        self.qualified_identifier.assert(&output.qualified_identifier)
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
                |(negation, higher_ranked_lifetimes, qualified_identifier)| {
                    Self {
                        negation,
                        higher_ranked_lifetimes,
                        qualified_identifier,
                    }
                },
            )
            .boxed()
    }
}

impl IndentDisplay for MarkerBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.negation {
            f.write_str("not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            higher_ranked_lifetimes.indent_fmt(f, indent)?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Marker {
    pub bounds: BoundList<MarkerBound>,
}

impl Input<&super::Marker> for &Marker {
    fn assert(self, output: &super::Marker) -> TestCaseResult {
        self.bounds.assert(&output.bounds)
    }
}

impl Arbitrary for Marker {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        BoundList::arbitrary_with(MarkerBound::arbitrary())
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

impl IndentDisplay for Marker {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("marker ")?;
        self.bounds.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundList<T> {
    pub first: T,
    pub rest: Vec<T>,
}

impl<I: Debug, O: Debug> Input<&super::UnionList<O>> for &BoundList<I>
where
    for<'i, 'o> &'i I: Input<&'o O>,
{
    fn assert(self, output: &super::UnionList<O>) -> TestCaseResult {
        self.first.assert(&output.first)?;

        prop_assert_eq!(self.rest.len(), output.rest.len());

        for (input, output) in self.rest.iter().zip(output.rest.iter()) {
            input.assert(&output.1)?;
        }

        Ok(())
    }
}

impl<T: IndentDisplay> IndentDisplay for BoundList<T> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.first.indent_fmt(f, indent)?;

        for bound in &self.rest {
            f.write_str(" + ")?;
            bound.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

impl<T: std::fmt::Debug> BoundList<T> {
    pub fn arbitrary_with(
        args: impl Strategy<Value = T> + Clone + 'static,
    ) -> BoxedStrategy<Self> {
        (args.clone(), proptest::collection::vec(args, 0..=6))
            .prop_map(|(first, rest)| Self { first, rest })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    bounds: BoundList<TraitBound>,
}

impl Input<&super::Trait> for &Trait {
    fn assert(self, output: &super::Trait) -> TestCaseResult {
        self.bounds.assert(&output.bounds)
    }
}

impl Arbitrary for Trait {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        BoundList::arbitrary_with(TraitBound::arbitrary())
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

impl IndentDisplay for Trait {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("trait ")?;
        self.bounds.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifierBound {
    pub negation: bool,
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub is_const: bool,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input<&super::QualifiedIdentifierBound> for &QualifiedIdentifierBound {
    fn assert(
        self,
        output: &super::QualifiedIdentifierBound,
    ) -> TestCaseResult {
        prop_assert_eq!(self.negation, output.not_keyword.is_some());
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes.as_ref())?;
        prop_assert_eq!(self.is_const, output.const_keyword.is_some());
        self.qualified_identifier.assert(&output.qualified_identifier)
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
                    negation,
                    higher_ranked_lifetimes,
                    is_const,
                    qualified_identifier,
                )| Self {
                    negation,
                    higher_ranked_lifetimes,
                    is_const,
                    qualified_identifier,
                },
            )
            .boxed()
    }
}

impl IndentDisplay for QualifiedIdentifierBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if self.negation {
            f.write_str("not ")?;
        }

        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            higher_ranked_lifetimes.indent_fmt(f, indent)?;
        }

        if self.is_const {
            f.write_str("const ")?;
        }

        self.qualified_identifier.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeBound {
    QualifiedIdentifier(QualifiedIdentifierBound),
    Const,
    Tuple,
    Outlives(Lifetime),
}

impl Input<&super::TypeBound> for &TypeBound {
    fn assert(self, output: &super::TypeBound) -> TestCaseResult {
        match (self, output) {
            (
                TypeBound::QualifiedIdentifier(a),
                super::TypeBound::QualifiedIdentifier(b),
            ) => a.assert(b),

            (TypeBound::Const, super::TypeBound::Const(_))
            | (TypeBound::Tuple, super::TypeBound::Tuple(_)) => Ok(()),

            (TypeBound::Outlives(a), super::TypeBound::Outlives(b)) => {
                a.assert(b)
            }

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
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
            proptest::strategy::Just(Self::Const),
            proptest::strategy::Just(Self::Tuple),
            Lifetime::arbitrary().prop_map(Self::Outlives),
        ]
        .boxed()
    }
}

impl IndentDisplay for TypeBound {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::QualifiedIdentifier(qualified_identifier_bound) => {
                qualified_identifier_bound.indent_fmt(f, indent)
            }

            Self::Const => f.write_str("const"),

            Self::Tuple => f.write_str("tuple"),

            Self::Outlives(lifetime) => Display::fmt(lifetime, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub r#type: TypeTerm,
    pub bounds: BoundList<TypeBound>,
}

impl Input<&super::Type> for &Type {
    fn assert(self, output: &super::Type) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes.as_ref())?;
        self.r#type.assert(&output.r#type)?;
        self.bounds.assert(&output.bounds)
    }
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            TypeTerm::arbitrary(),
            BoundList::arbitrary_with(TypeBound::arbitrary()),
        )
            .prop_map(|(higher_ranked_lifetimes, r#type, bounds)| Self {
                higher_ranked_lifetimes,
                r#type,
                bounds,
            })
            .boxed()
    }
}

impl IndentDisplay for Type {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            higher_ranked_lifetimes.indent_fmt(f, indent)?;
        }

        self.r#type.indent_fmt(f, indent)?;
        f.write_str(": ")?;
        self.bounds.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitTypeEquality {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub lhs_type: TypeTerm,
    pub rhs_type: TypeTerm,
}

impl Input<&super::TraitTypeEquality> for &TraitTypeEquality {
    fn assert(self, output: &super::TraitTypeEquality) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes.as_ref())?;
        self.lhs_type.assert(&output.lhs_type)?;
        self.rhs_type.assert(&output.rhs_type)
    }
}

impl Arbitrary for TraitTypeEquality {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            TypeTerm::arbitrary(),
            TypeTerm::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, lhs_type, rhs_type)| Self {
                higher_ranked_lifetimes,
                lhs_type,
                rhs_type,
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
            higher_ranked_lifetimes.indent_fmt(f, indent)?;
        }

        self.lhs_type.indent_fmt(f, indent)?;
        f.write_str(" = ")?;
        self.rhs_type.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeOutlives {
    pub operand: LifetimeParameter,
    pub bounds: BoundList<Lifetime>,
}

impl Input<&super::LifetimeOutlives> for &LifetimeOutlives {
    fn assert(self, output: &super::LifetimeOutlives) -> TestCaseResult {
        self.operand.assert(&output.operand)?;
        self.bounds.assert(&output.bounds)
    }
}

impl Arbitrary for LifetimeOutlives {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            LifetimeParameter::arbitrary(),
            BoundList::arbitrary_with(Lifetime::arbitrary()),
        )
            .prop_map(|(operand, bounds)| Self { operand, bounds })
            .boxed()
    }
}

impl IndentDisplay for LifetimeOutlives {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "{}: ", self.operand)?;
        self.bounds.indent_fmt(f, indent)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Trait(Trait),
    TraitTypeEquality(TraitTypeEquality),
    LifetimeOutlives(LifetimeOutlives),
    Marker(Marker),
}

impl Input<&super::Predicate> for &Predicate {
    fn assert(self, output: &super::Predicate) -> TestCaseResult {
        match (self, output) {
            (
                Predicate::TraitTypeEquality(a),
                super::Predicate::TraitTypeEquality(b),
            ) => a.assert(b),

            (Predicate::Trait(a), super::Predicate::Trait(b)) => a.assert(b),

            (
                Predicate::LifetimeOutlives(a),
                super::Predicate::LifetimeOutlives(b),
            ) => a.assert(b),

            (Predicate::Marker(a), super::Predicate::Marker(b)) => a.assert(b),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for Predicate {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Trait::arbitrary().prop_map(Predicate::Trait),
            TraitTypeEquality::arbitrary()
                .prop_map(Predicate::TraitTypeEquality),
            LifetimeOutlives::arbitrary().prop_map(Predicate::LifetimeOutlives),
            Marker::arbitrary().prop_map(Predicate::Marker),
        ]
        .boxed()
    }
}

impl IndentDisplay for Predicate {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Trait(trait_) => trait_.indent_fmt(f, indent),
            Self::TraitTypeEquality(trait_type_equality) => {
                trait_type_equality.indent_fmt(f, indent)
            }

            Self::LifetimeOutlives(outlives) => outlives.indent_fmt(f, indent),
            Self::Marker(marker) => marker.indent_fmt(f, indent),
        }
    }
}
