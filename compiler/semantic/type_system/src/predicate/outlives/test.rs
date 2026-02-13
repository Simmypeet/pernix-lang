use std::{borrow::Cow, fmt::Debug, future::Future, pin::Pin, sync::Arc};

use pernixc_hash::HashSet;
use pernixc_qbice::Engine;
use pernixc_semantic_element::variance::{Variance, Variances};
use pernixc_symbol::kind::Kind;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, MemberSymbol, Symbol, TraitMember},
    generic_parameters::{GenericParameters, LifetimeParameter},
    lifetime::Lifetime,
    predicate::{Compatible, Outlives, Predicate},
    r#type::Type,
};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    Error,
    environment::{Environment, Premise},
    equality, normalizer,
    term::Term,
    test::{create_test_engine, purge_trait_associated_type},
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum AbortError {
    #[error(transparent)]
    Abrupt(#[from] Error),

    #[error("collision to the ID generated on the table")]
    IDCollision,
}

pub type BoxedFuture<'x, T> =
    Pin<Box<dyn Future<Output = Result<(T, Lifetime), AbortError>> + 'x>>;

/// A trait for generating term for checking predicate
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, T>;

    fn node_count(&self) -> usize;
}

#[derive(Debug)]
pub struct ByEquality {
    pub equality: TraitMember,
    pub trait_id: pernixc_symbol::ID,
    pub property: Box<dyn Property<Type>>,
}

impl Property<Type> for ByEquality {
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, Type> {
        Box::pin(async move {
            {
                let mut input_session = engine.input_session().await;
                input_session
                    .set_input(
                        pernixc_symbol::parent::Key {
                            symbol_id: self.equality.id,
                        },
                        Some(self.trait_id),
                    )
                    .await;

                input_session
                    .set_input(
                        pernixc_symbol::kind::Key {
                            symbol_id: self
                                .equality
                                .id
                                .target_id
                                .make_global(self.trait_id),
                        },
                        Kind::Trait,
                    )
                    .await;

                input_session
                    .set_input(
                        pernixc_semantic_element::implemented::Key {
                            symbol_id: self
                                .equality
                                .id
                                .target_id
                                .make_global(self.trait_id),
                        },
                        engine.intern(HashSet::default()),
                    )
                    .await;
            }

            let (inner_operand, inner_bound) =
                self.property.generate(engine, premise).await?;

            let outlives = Outlives::new(
                Type::TraitMember(self.equality.clone()),
                inner_bound.clone(),
            );

            let environment = Environment::new(
                Cow::Borrowed(premise),
                Cow::Owned(engine.clone().tracked().await),
                normalizer::NO_OP,
            );

            if environment.query(&outlives).await?.is_none() {
                premise.predicates.insert(
                    Compatible {
                        lhs: self.equality.clone(),
                        rhs: inner_operand,
                    }
                    .into(),
                );
            }

            Ok((Type::TraitMember(self.equality.clone()), inner_bound))
        })
    }

    fn node_count(&self) -> usize { self.property.node_count() + 1 }
}

impl Arbitrary for ByEquality {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (TraitMember::arbitrary(), pernixc_symbol::ID::arbitrary(), args)
            .prop_map(|(equality, trait_id, property)| Self {
                equality: TraitMember(MemberSymbol {
                    id: equality.0.id,
                    member_generic_arguments: GenericArguments {
                        lifetimes: equality
                            .0
                            .member_generic_arguments
                            .lifetimes
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                        types: equality
                            .0
                            .member_generic_arguments
                            .types
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                        constants: equality
                            .0
                            .member_generic_arguments
                            .constants
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                    },
                    parent_generic_arguments: GenericArguments {
                        lifetimes: equality
                            .0
                            .parent_generic_arguments
                            .lifetimes
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                        types: equality
                            .0
                            .parent_generic_arguments
                            .types
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                        constants: equality
                            .0
                            .parent_generic_arguments
                            .constants
                            .into_iter()
                            .map(purge_trait_associated_type)
                            .collect(),
                    },
                }),
                trait_id,
                property,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct LifetimeMatching {
    pub struct_id: Global<pernixc_symbol::ID>,
    pub lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    pub bound: Lifetime,
}

impl Property<Type> for LifetimeMatching {
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, Type> {
        Box::pin(async move {
            let mut operand_lifetimes = Vec::new();
            let mut bound_lifetimes = Vec::new();

            for lifetime_prop in &self.lifetime_properties {
                let (operand, bound) =
                    lifetime_prop.generate(engine, premise).await?;

                operand_lifetimes.push(operand);
                bound_lifetimes.push(bound);
            }

            // create lifetime generic parmaeters and variances
            let mut generic_parameter = GenericParameters::default();
            let mut variance_map = Variances::default();
            {
                for i in 0..self.lifetime_properties.len() {
                    let lifetime_param = generic_parameter
                        .add_lifetime_parameter(LifetimeParameter {
                            name: engine.intern_unsized(format!("_{i}")),
                            span: None,
                        })
                        .unwrap();

                    variance_map
                        .variances_by_lifetime_ids
                        .insert(lifetime_param, Variance::Covariant);
                }
            }

            {
                let mut input_session = engine.input_session().await;
                input_session
                    .set_input(
                        pernixc_symbol::kind::Key { symbol_id: self.struct_id },
                        Kind::Struct,
                    )
                    .await;

                input_session
                    .set_input(
                        pernixc_semantic_element::variance::Key {
                            symbol_id: self.struct_id,
                        },
                        engine.intern(variance_map),
                    )
                    .await;

                input_session
                    .set_input(
                        pernixc_term::generic_parameters::Key {
                            symbol_id: self.struct_id,
                        },
                        engine.intern(generic_parameter),
                    )
                    .await;
            }

            let ty_operand = Type::Symbol(Symbol {
                id: self.struct_id,
                generic_arguments: GenericArguments {
                    lifetimes: operand_lifetimes,
                    types: Vec::new(),
                    constants: Vec::new(),
                },
            });

            let environment = Environment::new(
                Cow::Borrowed(premise),
                Cow::Owned(engine.clone().tracked().await),
                normalizer::NO_OP,
            );

            if environment
                .query(&Outlives::new(ty_operand.clone(), self.bound.clone()))
                .await?
                .is_none()
            {
                premise.predicates.insert(Predicate::TypeOutlives(Outlives {
                    operand: Type::Symbol(Symbol {
                        id: self.struct_id,
                        generic_arguments: GenericArguments {
                            lifetimes: bound_lifetimes,
                            types: Vec::new(),
                            constants: Vec::new(),
                        },
                    }),
                    bound: self.bound.clone(),
                }));
            }

            Ok((ty_operand, self.bound.clone()))
        })
    }

    fn node_count(&self) -> usize {
        1 + self
            .lifetime_properties
            .iter()
            .map(|x| x.node_count())
            .sum::<usize>()
    }
}

impl Arbitrary for LifetimeMatching {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            Global::arbitrary(),
            proptest::collection::vec(
                Box::<dyn Property<Lifetime>>::arbitrary(),
                1..=8,
            ),
            Lifetime::arbitrary(),
        )
            .prop_map(|(struct_id, lifetime_properties, bound)| Self {
                struct_id,
                lifetime_properties,
                bound,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct Reflexive {
    pub term: Box<dyn equality::test::Property<Lifetime>>,
}

impl Property<Lifetime> for Reflexive {
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, Lifetime> {
        Box::pin(async move {
            let (term, bound) =
                self.term.generate(engine, premise).await.map_err(
                    |x| match x {
                        equality::test::AbortError::Abrupt(abrupt_error) => {
                            AbortError::Abrupt(abrupt_error)
                        }
                        equality::test::AbortError::IDCollision => {
                            AbortError::IDCollision
                        }
                    },
                )?;

            Ok((term, bound))
        })
    }

    fn node_count(&self) -> usize { 1 }
}

impl Arbitrary for Reflexive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Box::<dyn equality::test::Property<Lifetime>>::arbitrary()
            .prop_map(|term| Self { term })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByPremise<T> {
    pub term: T,
    pub bound: Lifetime,
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + Term + 'static> Arbitrary
    for ByPremise<T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            T::arbitrary().prop_map(purge_trait_associated_type),
            Lifetime::arbitrary(),
        )
            .prop_map(|(term, bound)| Self { term, bound })
            .boxed()
    }
}

impl<T: Term> Property<T> for ByPremise<T>
where
    Outlives<T>: Into<Predicate>,
{
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, T> {
        Box::pin(async move {
            let environment = Environment::new(
                Cow::Borrowed(premise),
                Cow::Owned(engine.clone().tracked().await),
                normalizer::NO_OP,
            );

            if environment
                .query(&Outlives::new(self.term.clone(), self.bound.clone()))
                .await?
                .is_none()
            {
                premise.predicates.insert(
                    Outlives {
                        operand: self.term.clone(),
                        bound: self.bound.clone(),
                    }
                    .into(),
                );
            }

            Ok((self.term.clone(), self.bound.clone()))
        })
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct Transitive<T> {
    pub inner_property: Box<dyn Property<T>>,
    pub final_bound: Lifetime,
}

impl<T: Term> Property<T> for Transitive<T> {
    fn generate<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x, T> {
        Box::pin(async move {
            let (inner_operand, inner_bound) =
                self.inner_property.generate(engine, premise).await?;

            let environment = Environment::new(
                Cow::Borrowed(premise),
                Cow::Owned(engine.clone().tracked().await),
                normalizer::NO_OP,
            );

            if environment
                .query(&Outlives::new(
                    inner_operand.clone(),
                    self.final_bound.clone(),
                ))
                .await?
                .is_none()
            {
                premise.predicates.insert(Predicate::LifetimeOutlives(
                    Outlives {
                        operand: inner_bound,
                        bound: self.final_bound.clone(),
                    },
                ));
            }

            Ok((inner_operand, self.final_bound.clone()))
        })
    }

    fn node_count(&self) -> usize { 1 + self.inner_property.node_count() }
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
            .prop_map(|(inner_property, bound)| Self {
                inner_property,
                final_bound: bound,
            })
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

        leaf.prop_recursive(20, 20, 1, |inner| {
            Transitive::arbitrary_with(Some(inner.clone()))
                .prop_map(|x| Box::new(x) as _)
        })
        .boxed()
    }
}

async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let (engine, _dir) = create_test_engine().await;

    println!("node count: {}", property.node_count());

    let (term1, term2) =
        property.generate(&engine, &mut premise).await.map_err(|x| {
            println!("premise count: {}", premise.predicates.len());
            TestCaseError::reject(format!("{x}"))
        })?;

    println!("premise count: {}", premise.predicates.len());

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );
    let result = environment
        .query(&Outlives::new(term1, term2))
        .await
        .map_err(|x| TestCaseError::fail(format!("{x}")))?;

    prop_assert!(result.is_some());

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary()
    ) {
        let result = tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property));

        println!("{result:?}");

        result?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary()
    ) {
        let result = tokio::runtime::Builder::new_multi_thread().build()
            .unwrap()
            .block_on(property_based_testing(&*property));

        println!("{result:?}");

        result?;
    }

    #[test]
    fn constant_always_outlives(
        constant in Constant::arbitrary(),
        lifetime in Lifetime::arbitrary()
    ) {
        let test = async move {
            let (engine, _dir) = create_test_engine().await;
            let premise = Premise::default();

            let environment = Environment::new(
                Cow::Borrowed(&premise),
                Cow::Owned(engine.tracked().await),
                normalizer::NO_OP
            );

            prop_assert!(
                environment.query(&Outlives::new(constant, lifetime)).await?.is_some()
            );

            Ok(())
        };


        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(test)?;
    }
}
