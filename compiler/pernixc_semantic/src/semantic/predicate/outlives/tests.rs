use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    semantic::{
        equality,
        predicate::{Outlives, Predicate, TraitMemberEquality},
        session::{self, ExceedLimitError, Limit, Session},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            GenericArguments, Symbol, Term,
        },
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
pub struct ByEquality<T: Term> {
    pub equality: T::TraitMember,
    pub property: Box<dyn Property<T>>,
}

impl<T: Term> Property<T> for ByEquality<T>
where
    session::Default: Session<T>,
    TraitMemberEquality<T>: Into<Predicate>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        if Outlives::satisfies(
            &T::from(self.equality.clone()),
            &self.property.generate().1,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        premise.append_from_predicates(std::iter::once(
            TraitMemberEquality {
                trait_member: self.equality.clone(),
                equivalent: self.property.generate().0,
            }
            .into(),
        ));

        self.property.apply(table, premise)?;

        Ok(())
    }

    fn generate(&self) -> (T, Lifetime) {
        let (_, bound) = self.property.generate();

        (T::from(self.equality.clone()), bound)
    }
}

impl<T: Term> Arbitrary for ByEquality<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    T::TraitMember: Arbitrary<Strategy = BoxedStrategy<T::TraitMember>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (T::TraitMember::arbitrary(), args)
            .prop_map(|(equality, property)| Self { equality, property })
            .boxed()
    }
}

#[derive(Debug)]
pub struct LifetimeMatching {
    pub id: r#type::SymbolID,
    pub lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    pub bound: Lifetime,
}

impl Property<Type> for LifetimeMatching {
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (ty, bound) = self.generate();
        let mut bound_lifetimes = Vec::new();
        for lifetime in &self.lifetime_properties {
            if Outlives::satisfies(
                &ty,
                &bound,
                &Environment { premise, table },
                &mut Limit::new(&mut session::Default::default()),
            )? {
                return Ok(());
            }

            lifetime.apply(table, premise)?;
            bound_lifetimes.push(lifetime.generate().1);
        }

        if !Outlives::satisfies(
            &ty,
            &bound,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            premise.append_from_predicates(std::iter::once(
                Predicate::TypeOutlives(Outlives {
                    operand: Type::Symbol(Symbol {
                        id: self.id,
                        generic_arguments: GenericArguments {
                            lifetimes: bound_lifetimes,
                            types: Vec::new(),
                            constants: Vec::new(),
                        },
                    }),
                    bound: self.bound,
                }),
            ));
        }

        Ok(())
    }

    fn generate(&self) -> (Type, Lifetime) {
        let mut operand_lifetimes = Vec::new();

        for lifetime in &self.lifetime_properties {
            operand_lifetimes.push(lifetime.generate().0);
        }

        (
            Type::Symbol(Symbol {
                id: self.id,
                generic_arguments: GenericArguments {
                    lifetimes: operand_lifetimes,
                    types: Vec::new(),
                    constants: Vec::new(),
                },
            }),
            self.bound,
        )
    }
}

impl Arbitrary for LifetimeMatching {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            r#type::SymbolID::arbitrary(),
            proptest::collection::vec(
                Box::<dyn Property<Lifetime>>::arbitrary(),
                1..=8,
            ),
            Lifetime::arbitrary(),
        )
            .prop_map(|(id, lifetime_properties, bound)| Self {
                id,
                lifetime_properties,
                bound,
            })
            .boxed()
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByPremise<T> {
    pub term: T,
    pub bound: Lifetime,
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for ByPremise<T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (T::arbitrary(), Lifetime::arbitrary())
            .prop_map(|(term, bound)| Self { term, bound })
            .boxed()
    }
}

impl<T: Term> Property<T> for ByPremise<T>
where
    session::Default: Session<T>,
    Outlives<T>: Into<Predicate>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        if Outlives::satisfies(
            &self.term,
            &self.bound,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        premise.append_from_predicates(std::iter::once(
            Outlives { operand: self.term.clone(), bound: self.bound }.into(),
        ));

        Ok(())
    }

    fn generate(&self) -> (T, Lifetime) { (self.term.clone(), self.bound) }
}

#[derive(Debug)]
pub struct Transitive<T> {
    pub inner_property: Box<dyn Property<T>>,
    pub bound: Lifetime,
}

impl<T: Term> Property<T> for Transitive<T>
where
    session::Default: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (operand, bound) = self.generate();
        let (inner_operand, inner_bound) = self.inner_property.generate();

        if Outlives::satisfies(
            &operand,
            &bound,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        if !Outlives::satisfies(
            &inner_bound,
            &self.bound,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            premise.append_from_predicates(std::iter::once(
                Predicate::LifetimeOutlives(Outlives {
                    operand: inner_bound,
                    bound: self.bound,
                }),
            ));
        }

        if !Outlives::satisfies(
            &inner_operand,
            &inner_bound,
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            self.inner_property.apply(table, premise)?;
        }

        println!("pass!");
        Ok(())
    }

    fn generate(&self) -> (T, Lifetime) {
        let (operand, _) = self.inner_property.generate();

        (operand, self.bound)
    }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for Transitive<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (args, Lifetime::arbitrary())
            .prop_map(|(inner_property, bound)| Self { inner_property, bound })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = ByPremise::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(4, 4, 1, |inner| {
            prop_oneof![
                Transitive::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                ByEquality::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                LifetimeMatching::arbitrary().prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Reflexive::arbitrary().prop_map(|x| Box::new(x) as _),
            ByPremise::arbitrary().prop_map(|x| Box::new(x) as _),
        ];

        leaf.prop_recursive(4, 8, 2, |inner| {
            Transitive::arbitrary_with(Some(inner.clone()))
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
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary()
    ) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary()
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
