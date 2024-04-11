use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    semantic::{
        equality,
        predicate::{Outlives, Predicate},
        session::{self, ExceedLimitError, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        tests::State,
        Environment, Premise,
    },
    table::Table,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error(transparent)]
    ExceedLimitError(#[from] ExceedLimitError),
    #[error("failed to apply the environment")]
    TypeAliasIDCollision,
}

impl From<equality::tests::ApplyPropertyError> for ApplyPropertyError {
    fn from(value: equality::tests::ApplyPropertyError) -> Self {
        match value {
            equality::tests::ApplyPropertyError::ExceedLimitError(e) => {
                Self::ExceedLimitError(e)
            }
            equality::tests::ApplyPropertyError::TypeAliasIDCollision => {
                Self::TypeAliasIDCollision
            }
        }
    }
}

/// A trait for generating term for checking predicate
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError>;

    /// Generates the term for testing.
    #[must_use]
    fn generate(&self) -> (T, Lifetime);
}

#[derive(Debug)]
pub struct Reflexive {
    pub term: Box<dyn equality::tests::Property<Lifetime>>,
}

impl Property<Lifetime> for Reflexive {
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if Outlives::satisfies(
            &lhs,
            &rhs,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.term.apply(table, premise)?;
        Ok(())
    }

    fn generate(&self) -> (Lifetime, Lifetime) {
        let (term, bound) = self.term.generate();
        (term, bound)
    }
}

impl Arbitrary for Reflexive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Box::<dyn equality::tests::Property<Lifetime>>::arbitrary()
            .prop_map(|term| Self { term })
            .boxed()
    }
}

#[derive(Debug)]
pub struct Transitive {
    pub inner_property: Box<dyn Property<Lifetime>>,
    pub term: Lifetime,
    pub at_lhs: bool,
}

impl Property<Lifetime> for Transitive {
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();
        let (inner_lhs, inner_rhs) = self.inner_property.generate();

        if Outlives::satisfies(
            &lhs,
            &rhs,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        let outlives_premise = if self.at_lhs {
            Outlives { operand: self.term, bound: inner_lhs }
        } else {
            Outlives { operand: inner_rhs, bound: self.term }
        };

        premise.predicates.push(Predicate::LifetimeOutlives(outlives_premise));

        if Outlives::satisfies(
            &lhs,
            &rhs,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.inner_property.apply(table, premise)?;

        Ok(())
    }

    fn generate(&self) -> (Lifetime, Lifetime) {
        let (lhs, rhs) = self.inner_property.generate();

        if self.at_lhs {
            (self.term, rhs)
        } else {
            (lhs, self.term)
        }
    }
}

impl Arbitrary for Transitive {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args =
            args.unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary);

        (args, Lifetime::arbitrary(), proptest::bool::ANY)
            .prop_map(|(inner_property, term, at_lhs)| Self {
                inner_property,
                term,
                at_lhs,
            })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Reflexive::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(4, 4, 1, |inner| {
            Transitive::arbitrary_with(Some(inner))
                .prop_map(|x| Box::new(x) as _)
        })
        .boxed()
    }
}

fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult
where
    session::Default: Session<T>,
{
    let (term1, term2) = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<State>::default();

    property.apply(&mut table, &mut premise).map_err(|x| match x {
        ApplyPropertyError::ExceedLimitError(_) => {
            TestCaseError::reject("too complex property")
        }
        ApplyPropertyError::TypeAliasIDCollision => {
            TestCaseError::reject("type alias id collision")
        }
    })?;

    let environment = &Environment { table: &table, premise: &premise };
    prop_assert!(Outlives::satisfies(
        &term1,
        &term2,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    {
        let mut premise_cloned = premise.clone();

        if premise_cloned.equivalent.remove_class::<Lifetime>(0).is_some()
            || premise_cloned.equivalent.remove_class::<Type>(0).is_some()
            || premise_cloned.equivalent.remove_class::<Constant>(0).is_some()
        {
            let environment =
                &Environment { table: &table, premise: &premise_cloned };
            prop_assert!(!Outlives::satisfies(
                &term1,
                &term2,
                environment,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);
        }
    }

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        cases: 8192,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary()
    ) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn constant_always_outlives(
        constant in Constant::arbitrary(),
        lifetime in Lifetime::arbitrary()
    ) {
        let environment = &Environment {
            table: &Table::<State>::default(),
            premise: &Premise::default()
        };
        prop_assert!(
            Outlives::satisfies(
                &constant,
                &lifetime,
                environment,
                &mut Limit::new(&mut session::Default::default())
            ).unwrap()
        );
    }
}
