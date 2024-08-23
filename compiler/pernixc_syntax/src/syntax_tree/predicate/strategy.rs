use std::fmt::{Debug, Display, Write};

use pernixc_tests::input::Input;
use proptest::{
    arbitrary::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    r#type::strategy::Type,
    strategy::{
        ConnectedList, ConstantArgument, ConstantPunctuation, Lifetime,
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
            .assert(output.lifetime_parameter_list().as_ref())
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

impl Display for HigherRankedLifetimes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("for[")?;

        if let Some(lifetime_parameter_list) = &self.lifetime_parameter_list {
            Display::fmt(lifetime_parameter_list, f)?;
        }

        f.write_char(']')
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBound {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub const_keyword: bool,
    pub qualified_identifier: QualifiedIdentifier,
}

impl Input<&super::TraitBound> for &TraitBound {
    fn assert(self, output: &super::TraitBound) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_rankded_lifetimes().as_ref())?;
        prop_assert_eq!(self.const_keyword, output.const_keyword().is_some());
        self.qualified_identifier.assert(output.qualified_identifier())
    }
}

impl Arbitrary for TraitBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            proptest::bool::ANY,
            QualifiedIdentifier::arbitrary(),
        )
            .prop_map(
                |(
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                )| Self {
                    higher_ranked_lifetimes,
                    const_keyword,
                    qualified_identifier,
                },
            )
            .boxed()
    }
}

impl Display for TraitBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        if self.const_keyword {
            f.write_str("const ")?;
        }

        Display::fmt(&self.qualified_identifier, f)
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

impl<T: Display> Display for BoundList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for bound in &self.rest {
            write!(f, " + {bound}")?;
        }

        Ok(())
    }
}

impl<T: std::fmt::Debug> BoundList<T> {
    fn arbitrary_with(
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

impl Display for Trait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "trait {}", self.bounds)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitTypeEquality {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub qualified_identifier: QualifiedIdentifier,
    pub r#type: Type,
}

impl Input<&super::TraitTypeEquality> for &TraitTypeEquality {
    fn assert(self, output: &super::TraitTypeEquality) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes().as_ref())?;
        self.qualified_identifier.assert(output.qualified_identifier())?;
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for TraitTypeEquality {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            QualifiedIdentifier::arbitrary(),
            Type::arbitrary(),
        )
            .prop_map(
                |(higher_ranked_lifetimes, qualified_identifier, r#type)| {
                    Self {
                        higher_ranked_lifetimes,
                        qualified_identifier,
                        r#type,
                    }
                },
            )
            .boxed()
    }
}

impl Display for TraitTypeEquality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        Display::fmt(&self.qualified_identifier, f)?;
        f.write_str(" = ")?;
        Display::fmt(&self.r#type, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OutlivesOperand {
    LifetimeParameter(LifetimeParameter),
    Type(Type),
}

impl Input<&super::OutlivesOperand> for &OutlivesOperand {
    fn assert(self, output: &super::OutlivesOperand) -> TestCaseResult {
        match (self, output) {
            (
                OutlivesOperand::LifetimeParameter(a),
                super::OutlivesOperand::LifetimeParameter(b),
            ) => a.assert(b),

            (OutlivesOperand::Type(a), super::OutlivesOperand::Type(b)) => {
                a.assert(b)
            }

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for OutlivesOperand {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeParameter::arbitrary()
                .prop_map(OutlivesOperand::LifetimeParameter),
            Type::arbitrary().prop_map(OutlivesOperand::Type),
        ]
        .boxed()
    }
}

impl Display for OutlivesOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LifetimeParameter(lifetime_parameter) => {
                Display::fmt(lifetime_parameter, f)
            }

            Self::Type(r#type) => Display::fmt(r#type, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Outlives {
    pub operand: OutlivesOperand,
    pub bounds: BoundList<Lifetime>,
}

impl Input<&super::Outlives> for &Outlives {
    fn assert(self, output: &super::Outlives) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.bounds.assert(output.bounds())
    }
}

impl Arbitrary for Outlives {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            OutlivesOperand::arbitrary(),
            BoundList::arbitrary_with(Lifetime::arbitrary()),
        )
            .prop_map(|(operand, bounds)| Self { operand, bounds })
            .boxed()
    }
}

impl Display for Outlives {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.operand, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.bounds, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantTypeBound {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub r#type: Type,
}

impl Input<&super::ConstantTypeBound> for &ConstantTypeBound {
    fn assert(self, output: &super::ConstantTypeBound) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes().as_ref())?;
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for ConstantTypeBound {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            Type::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, r#type)| Self {
                higher_ranked_lifetimes,
                r#type,
            })
            .boxed()
    }
}

impl Display for ConstantTypeBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        Display::fmt(&self.r#type, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType {
    pub bounds: BoundList<ConstantTypeBound>,
}

impl Input<&super::ConstantType> for &ConstantType {
    fn assert(self, output: &super::ConstantType) -> TestCaseResult {
        self.bounds.assert(&output.bounds)
    }
}

impl Arbitrary for ConstantType {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        BoundList::arbitrary_with(ConstantTypeBound::arbitrary())
            .prop_map(|bounds| Self { bounds })
            .boxed()
    }
}

impl Display for ConstantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}", self.bounds)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleOperandKind {
    Type(Type),
    Constant(ConstantArgument),
}

impl Input<&super::TupleOperandKind> for &TupleOperandKind {
    fn assert(self, output: &super::TupleOperandKind) -> TestCaseResult {
        match (self, output) {
            (TupleOperandKind::Type(a), super::TupleOperandKind::Type(b)) => {
                a.assert(b)
            }

            (
                TupleOperandKind::Constant(a),
                super::TupleOperandKind::Constant(b),
            ) => a.assert(b),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?}, got {output:?}"
            ))),
        }
    }
}

impl Arbitrary for TupleOperandKind {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Type::arbitrary().prop_map(TupleOperandKind::Type),
            ConstantArgument::arbitrary().prop_map(TupleOperandKind::Constant),
        ]
        .boxed()
    }
}

impl Display for TupleOperandKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(r#type) => Display::fmt(r#type, f),
            Self::Constant(constant) => Display::fmt(constant, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleOperand {
    pub higher_ranked_lifetimes: Option<HigherRankedLifetimes>,
    pub kind: TupleOperandKind,
}

impl Input<&super::TupleOperand> for &TupleOperand {
    fn assert(self, output: &super::TupleOperand) -> TestCaseResult {
        self.higher_ranked_lifetimes
            .as_ref()
            .assert(output.higher_ranked_lifetimes().as_ref())?;
        self.kind.assert(output.kind())
    }
}

impl Arbitrary for TupleOperand {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(HigherRankedLifetimes::arbitrary()),
            TupleOperandKind::arbitrary(),
        )
            .prop_map(|(higher_ranked_lifetimes, kind)| Self {
                higher_ranked_lifetimes,
                kind,
            })
            .boxed()
    }
}

impl Display for TupleOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(higher_ranked_lifetimes) = &self.higher_ranked_lifetimes {
            write!(f, "{higher_ranked_lifetimes} ")?;
        }

        Display::fmt(&self.kind, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    pub operands: BoundList<TupleOperand>,
}

impl Input<&super::Tuple> for &Tuple {
    fn assert(self, output: &super::Tuple) -> TestCaseResult {
        self.operands.assert(&output.operands)
    }
}

impl Arbitrary for Tuple {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        BoundList::arbitrary_with(TupleOperand::arbitrary())
            .prop_map(|operands| Self { operands })
            .boxed()
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tuple {}", self.operands)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Trait(Trait),
    TraitTypeEquality(TraitTypeEquality),
    Outlives(Outlives),
    ConstantType(ConstantType),
    Tuple(Tuple),
}

impl Input<&super::Predicate> for &Predicate {
    fn assert(self, output: &super::Predicate) -> TestCaseResult {
        match (self, output) {
            (
                Predicate::TraitTypeEquality(a),
                super::Predicate::TraitTypeEquality(b),
            ) => a.assert(b),

            (Predicate::Trait(a), super::Predicate::Trait(b)) => a.assert(b),

            (Predicate::Outlives(a), super::Predicate::Outlives(b)) => {
                a.assert(b)
            }

            (Predicate::ConstantType(a), super::Predicate::ConstantType(b)) => {
                a.assert(b)
            }

            (Predicate::Tuple(a), super::Predicate::Tuple(b)) => a.assert(b),

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
            Outlives::arbitrary().prop_map(Predicate::Outlives),
            ConstantType::arbitrary().prop_map(Predicate::ConstantType),
            Tuple::arbitrary().prop_map(Predicate::Tuple),
        ]
        .boxed()
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait(trait_) => Display::fmt(trait_, f),
            Self::TraitTypeEquality(trait_type_equality) => {
                Display::fmt(trait_type_equality, f)
            }

            Self::Outlives(outlives) => Display::fmt(outlives, f),
            Self::ConstantType(constant_type) => Display::fmt(constant_type, f),
            Self::Tuple(tuple) => Display::fmt(tuple, f),
        }
    }
}
