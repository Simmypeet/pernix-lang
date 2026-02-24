#![allow(missing_docs)]

use std::{borrow::Cow, collections::BTreeSet, pin::Pin, sync::Arc};

use pernixc_qbice::Engine;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::InstanceParameterID,
    instance::{Instance, InstanceAssociated},
    lifetime::Lifetime,
    predicate::{Compatible, Predicate},
    r#type::{Primitive, Type},
};
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    prop_assert, prop_oneof, proptest,
    test_runner::TestCaseResult,
};

use crate::{
    OverflowError,
    environment::{Environment, Premise},
    equality::Equality,
    normalizer,
    term::Term,
    test::{
        create_test_engine, purge_instance_associated,
        purge_instance_associated_in_generic_args,
    },
};

#[tokio::test]
async fn reflexive() {
    let (engine, _dir) = create_test_engine().await;
    let term = Type::Primitive(Primitive::Bool);
    let premise = Premise::default();

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Equality::new(term.clone(), term))
        .await
        .unwrap()
        .unwrap();

    assert!(result.constraints.is_empty());

    environment.assert_call_stack_empty();
}

#[tokio::test]
async fn symmetric() {
    let (engine, _dir) = create_test_engine().await;

    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated.clone(), equivalence.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    assert!(
        environment
            .query(&Equality::new(
                Type::InstanceAssociated(instance_associated.clone()),
                equivalence.clone(),
            ))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    assert!(
        environment
            .query(&Equality::new(
                equivalence,
                Type::InstanceAssociated(instance_associated)
            ))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    environment.assert_call_stack_empty();
}

#[tokio::test]
async fn not_equal() {
    let (engine, _dir) = create_test_engine().await;

    let instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(0),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::InstanceAssociatedTypeEquality(
        Compatible::new(instance_associated.clone(), equivalence.clone()),
    ));

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let non_equivalence = Type::Primitive(Primitive::Int32);

    assert!(
        environment
            .query(&Equality::new(
                Type::InstanceAssociated(instance_associated.clone()),
                non_equivalence.clone()
            ))
            .await
            .unwrap()
            .is_none()
    );

    assert!(
        environment
            .query(&Equality::new(
                non_equivalence,
                Type::InstanceAssociated(instance_associated)
            ))
            .await
            .unwrap()
            .is_none()
    );

    environment.assert_call_stack_empty();
}

#[tokio::test]
async fn transitivity() {
    let first_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(1),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );
    let second_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(2),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );

    let equivalence = Type::Primitive(Primitive::Bool);

    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            first_instance_associated.clone(),
            Type::InstanceAssociated(second_instance_associated.clone()),
        )),
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            second_instance_associated.clone(),
            equivalence.clone(),
        )),
    ]);

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    assert!(
        environment
            .query(&Equality::new(
                Type::InstanceAssociated(first_instance_associated.clone()),
                equivalence.clone(),
            ))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    assert!(
        environment
            .query(&Equality::new(
                equivalence,
                Type::InstanceAssociated(first_instance_associated)
            ))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    environment.assert_call_stack_empty();
}

#[tokio::test]
#[allow(clippy::too_many_lines)]
async fn congruence() {
    let first_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(1),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );
    let second_instance_associated = InstanceAssociated::new(
        Box::new(Instance::Parameter(InstanceParameterID::new(
            TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
            pernixc_arena::ID::new(2),
        ))),
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(2)),
        GenericArguments::default(),
    );
    let first_equivalence = Type::Primitive(Primitive::Bool);
    let second_equivalence = Type::Primitive(Primitive::Int32);

    let (engine, _dir) = create_test_engine().await;

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            first_instance_associated.clone(),
            first_equivalence.clone(),
        )),
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(
            second_instance_associated.clone(),
            second_equivalence.clone(),
        )),
    ]);

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    let lhs = Type::Symbol(Symbol::new(
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(3)),
        GenericArguments::new(
            Vec::new(),
            vec![
                Type::InstanceAssociated(first_instance_associated.clone()),
                Type::InstanceAssociated(second_instance_associated.clone()),
            ],
            Vec::new(),
            Vec::new(),
        ),
    ));
    let rhs = Type::Symbol(Symbol::new(
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(3)),
        GenericArguments::new(
            Vec::new(),
            vec![first_equivalence.clone(), second_equivalence.clone()],
            Vec::new(),
            Vec::new(),
        ),
    ));

    assert!(
        environment
            .query(&Equality::new(lhs.clone(), rhs.clone()))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    assert!(
        environment
            .query(&Equality::new(rhs, lhs))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    environment.assert_call_stack_empty();
}

#[tokio::test]
async fn symbol() {
    let symbol = Type::Symbol(Symbol::new(
        TargetID::TEST.make_global(pernixc_symbol::ID::from_u128(1)),
        GenericArguments::new(
            Vec::new(),
            vec![Type::Primitive(Primitive::Bool)],
            Vec::new(),
            Vec::new(),
        ),
    ));

    let premise = Premise::default();
    let (engine, _dir) = create_test_engine().await;
    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    assert!(
        environment
            .query(&Equality::new(symbol.clone(), symbol))
            .await
            .unwrap()
            .unwrap()
            .constraints
            .is_empty()
    );

    environment.assert_call_stack_empty();
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum AbortError {
    #[error(transparent)]
    Abrupt(#[from] OverflowError),

    #[error("collision to the ID generated on the table")]
    IDCollision,
}

pub type BoxedFuture<'x, T> =
    Pin<Box<dyn Future<Output = Result<(T, T), AbortError>> + 'x>>;

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + std::fmt::Debug {
    /// Applies this property to the environment.
    fn generate<'s>(
        &'s self,
        table: &'s Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, T>;

    /// Returns the number of nodes in the property.
    fn node_count(&self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identity<T> {
    pub term: T,
}

impl<T: Arbitrary + Term> Arbitrary for Identity<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary()
            .prop_map(|term| Self { term: purge_instance_associated(term) })
            .boxed()
    }
}

impl<T: Clone + std::fmt::Debug + 'static> Property<T> for Identity<T> {
    fn generate<'s>(
        &'s self,
        _: &Arc<Engine>,
        _: &mut Premise,
    ) -> Pin<
        Box<dyn std::future::Future<Output = Result<(T, T), AbortError>> + 's>,
    > {
        Box::pin(async move { Ok((self.term.clone(), self.term.clone())) })
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct Mapping {
    pub property: Box<dyn Property<Type>>,
    pub instance_parameter_id: InstanceParameterID,
    pub trait_associated_symbol_id: Global<pernixc_symbol::ID>,
    pub trait_associated_symbol_generic_arguments: GenericArguments,
    pub map_at_lhs: bool,
}

impl Arbitrary for Mapping {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy =
            args.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (
            strategy,
            InstanceParameterID::arbitrary(),
            Global::arbitrary(),
            GenericArguments::arbitrary(),
            proptest::bool::ANY,
        )
            .prop_map(
                |(
                    property,
                    instance_parameter_id,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments,
                    map_at_lhs,
                )| Self {
                    property,
                    instance_parameter_id,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments:
                        trait_associated_symbol_generic_arguments
                            .purge_instance_associated_in_generic_args(),
                    map_at_lhs,
                },
            )
            .boxed()
    }
}

impl Mapping {
    #[must_use]
    pub fn create_instance_associated(&self) -> InstanceAssociated {
        InstanceAssociated::new(
            Box::new(Instance::Parameter(self.instance_parameter_id)),
            self.trait_associated_symbol_id,
            self.trait_associated_symbol_generic_arguments.clone(),
        )
    }

    #[must_use]
    pub fn create_instance_associated_type(&self) -> Type {
        Type::InstanceAssociated(self.create_instance_associated())
    }
}

impl Property<Type> for Mapping {
    fn generate<'s>(
        &'s self,
        engine: &'s Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, Type> {
        Box::pin(async move {
            let (inner_lhs, inner_rhs) =
                self.property.generate(engine, premise).await?;

            let should_map = if self.map_at_lhs {
                Environment::new(
                    Cow::Borrowed(premise),
                    Cow::Owned(engine.clone().tracked().await),
                    normalizer::NO_OP,
                )
                .query(&Equality::new(
                    self.create_instance_associated_type(),
                    inner_rhs.clone(),
                ))
                .await?
                .is_none()
            } else {
                Environment::new(
                    Cow::Borrowed(premise),
                    Cow::Owned(engine.clone().tracked().await),
                    normalizer::NO_OP,
                )
                .query(&Equality::new(
                    inner_lhs.clone(),
                    self.create_instance_associated_type(),
                ))
                .await?
                .is_none()
            };

            if should_map {
                premise.predicates.insert(
                    Compatible {
                        lhs: self.create_instance_associated(),
                        rhs: if self.map_at_lhs {
                            inner_lhs.clone()
                        } else {
                            inner_rhs.clone()
                        },
                    }
                    .into(),
                );
            }

            if self.map_at_lhs {
                Ok((self.create_instance_associated_type(), inner_rhs))
            } else {
                Ok((inner_lhs, self.create_instance_associated_type()))
            }
        })
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

#[derive(Debug)]
pub struct SymbolCongruence {
    lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    type_properties: Vec<Box<dyn Property<Type>>>,
    constant_properties: Vec<Box<dyn Property<Constant>>>,
    instance_properties: Vec<Box<dyn Property<Instance>>>,

    id: Global<pernixc_symbol::ID>,
}

impl Arbitrary for SymbolCongruence {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
        Option<BoxedStrategy<Box<dyn Property<Instance>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.1.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
                0..=6,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                0..=6,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=6,
            ),
            proptest::collection::vec(
                args.3.unwrap_or_else(Box::<dyn Property<Instance>>::arbitrary),
                0..=6,
            ),
            Global::arbitrary(),
        )
            .prop_map(
                |(
                    type_properties,
                    lifetime_properties,
                    constant_properties,
                    instance_properties,
                    id,
                )| Self {
                    lifetime_properties,
                    type_properties,
                    constant_properties,
                    instance_properties,
                    id,
                },
            )
            .boxed()
    }
}

impl<T: Term + From<Symbol> + 'static> Property<T> for SymbolCongruence {
    fn generate<'s>(
        &'s self,
        engine: &'s Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, T> {
        Box::pin(async move {
            let mut lhs_generic_arguments = GenericArguments::default();
            let mut rhs_generic_arguments = GenericArguments::default();

            for strategy in &self.lifetime_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.push_lifetime(lhs);
                rhs_generic_arguments.push_lifetime(rhs);
            }

            for strategy in &self.type_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.push_type(lhs);
                rhs_generic_arguments.push_type(rhs);
            }

            for strategy in &self.constant_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.push_constant(lhs);
                rhs_generic_arguments.push_constant(rhs);
            }

            for strategy in &self.instance_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.push_instance(lhs);
                rhs_generic_arguments.push_instance(rhs);
            }

            Ok((
                Symbol::new(self.id, lhs_generic_arguments).into(),
                Symbol::new(self.id, rhs_generic_arguments).into(),
            ))
        })
    }

    fn node_count(&self) -> usize {
        1 + self
            .lifetime_properties
            .iter()
            .map(|x| x.node_count())
            .sum::<usize>()
            + self.type_properties.iter().map(|x| x.node_count()).sum::<usize>()
            + self
                .constant_properties
                .iter()
                .map(|x| x.node_count())
                .sum::<usize>()
            + self
                .instance_properties
                .iter()
                .map(|x| x.node_count())
                .sum::<usize>()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Instance>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(inst: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let constant_strategy = Box::<dyn Property<Constant>>::arbitrary();
            let lifetime_strategy = Box::<dyn Property<Lifetime>>::arbitrary();
            let inst = inst.clone().unwrap_or_else(|| {
                Box::<dyn Property<Instance>>::arbitrary_with(Some(
                    inner.clone(),
                ))
            });

            prop_oneof![
                4 => Mapping::arbitrary_with(Some(inner.clone()))
                .prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(inner.clone()),
                    Some(constant_strategy),
                    Some(inst),
                ))
                .prop_map(|x| Box::new(x) as Box<dyn Property<Type>>),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Instance>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(8, 48, 6, move |inner| {
            let ty = args.clone().unwrap_or_else(|| {
                Box::<dyn Property<Type>>::arbitrary_with(Some(inner.clone()))
            });

            prop_oneof![
                1 => SymbolCongruence::arbitrary_with((
                    Some(Box::<dyn Property<Lifetime>>::arbitrary()),
                    Some(ty),
                    Some(Box::<dyn Property<Constant>>::arbitrary()),
                    Some(inner)
                ))
                .prop_map(|x| Box::new(x) as _)
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identity::arbitrary().prop_map(|x| Box::new(x) as _).boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identity::arbitrary().prop_map(|x| Box::new(x) as _).boxed()
    }
}

fn remove_equality_recursive(
    predicates: &mut BTreeSet<Predicate>,
    instance_associated: &InstanceAssociated,
) {
    let to_be_removeds = predicates
        .iter()
        .filter(|predicate| {
            let Predicate::InstanceAssociatedTypeEquality(equality) = predicate
            else {
                return false;
            };

            &equality.lhs == instance_associated
        })
        .cloned()
        .collect::<Vec<_>>();

    for removed in to_be_removeds {
        predicates.remove(&removed);

        let Some(Type::InstanceAssociated(next)) =
            removed.as_instance_associated_type_equality().map(|x| &x.rhs)
        else {
            continue;
        };

        remove_equality_recursive(predicates, next);
    }
}

#[allow(clippy::too_many_lines)]
async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let (engine, _dir) = create_test_engine().await;

    let (term1, term2) =
        property.generate(&engine, &mut premise).await.map_err(|error| {
            TestCaseError::reject(format!(
                "abrupt error {error}; skipping now .."
            ))
        })?;

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.clone().tracked().await),
        normalizer::NO_OP,
    );

    let first_result = environment
        .query(&Equality::new(term1.clone(), term2.clone()))
        .await
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;

    let second_result = environment
        .query(&Equality::new(term2.clone(), term1.clone()))
        .await
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;

    environment.assert_call_stack_empty();

    prop_assert!(first_result.constraints.is_empty());
    prop_assert!(second_result.constraints.is_empty());

    // remove the equality mapping should make the terms not equal.
    'out: {
        let mut predicates_cloned = premise.predicates.clone();
        let Some(to_remove) = predicates_cloned.iter().find_map(|predicate| {
            predicate
                .as_instance_associated_type_equality()
                .map(|x| x.lhs.clone())
        }) else {
            break 'out;
        };

        remove_equality_recursive(&mut predicates_cloned, &to_remove);

        let modified_premise = Premise {
            predicates: predicates_cloned,
            query_site: premise.query_site,
        };

        let modified_environment = Environment::new(
            Cow::Borrowed(&modified_premise),
            Cow::Owned(engine.clone().tracked().await),
            normalizer::NO_OP,
        );

        let first_result = modified_environment
            .query(&Equality::new(term1.clone(), term2.clone()))
            .await
            .map_err(|x| {
                TestCaseError::reject(format!(
                    "abrupt error {x}; skipping now .."
                ))
            })?;
        let second_result = modified_environment
            .query(&Equality::new(term2.clone(), term1.clone()))
            .await
            .map_err(|x| {
                TestCaseError::reject(format!(
                    "abrupt error {x}; skipping now .."
                ))
            })?;

        environment.assert_call_stack_empty();

        prop_assert!(first_result.is_none());
        prop_assert!(second_result.is_none());
    }

    Ok(())
}

proptest! {
    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
    ) {
        let test = tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property));

        println!("property count: {} ran with: {:?}", property.node_count(), &test);

        test?;
    }

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
    ) {
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property))?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary(),
    ) {
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property))?;
    }
}
