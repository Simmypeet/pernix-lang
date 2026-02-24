#![allow(missing_docs)]

use std::borrow::Cow;

use pernixc_target::TargetID;
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::InstanceParameterID,
    instance::{Instance, InstanceAssociated},
    predicate::{Compatible, Predicate},
    r#type::{Primitive, Type},
};

use crate::{
    environment::{Environment, Premise},
    equality::Equality,
    normalizer,
    test::create_test_engine,
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

/*


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
    Pin<Box<dyn Future<Output = Result<(T, T), AbortError>> + 'x>>;

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
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

impl<T: Clone + Debug + 'static> Property<T> for Identity<T> {
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
    pub target_trait_member: TraitMember,
    pub trait_id: pernixc_symbol::ID,
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
            TraitMember::arbitrary(),
            pernixc_symbol::ID::arbitrary(),
            proptest::bool::ANY,
        )
            .prop_map(
                |(property, target_trait_member, trait_id, map_at_lhs)| Self {
                    property,
                    target_trait_member: TraitMember(AssociatedSymbol {
                        id: target_trait_member.id,
                        member_generic_arguments: purge_instance_associated(
                            target_trait_member.0.member_generic_arguments,
                        ),
                        parent_generic_arguments: purge_instance_associated(
                            target_trait_member.0.parent_generic_arguments,
                        ),
                    }),
                    trait_id,
                    map_at_lhs,
                },
            )
            .boxed()
    }
}

impl Property<Type> for Mapping {
    fn generate<'s>(
        &'s self,
        engine: &'s Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, Type> {
        Box::pin(async move {
            let added = {
                let mut input_session = engine.input_session().await;
                let add_parent = input_session
                    .set_input(
                        pernixc_symbol::parent::Key {
                            symbol_id: self.target_trait_member.id,
                        },
                        Some(self.trait_id),
                    )
                    .await
                    == SetInputResult::Fresh;

                let add_kind = input_session
                    .set_input(
                        pernixc_symbol::kind::Key {
                            symbol_id: self
                                .target_trait_member
                                .id
                                .target_id
                                .make_global(self.trait_id),
                        },
                        pernixc_symbol::kind::Kind::Trait,
                    )
                    .await
                    == SetInputResult::Fresh;

                let add_implemented = input_session
                    .set_input(
                        pernixc_semantic_element::implemented::Key {
                            symbol_id: self
                                .target_trait_member
                                .id
                                .target_id
                                .make_global(self.trait_id),
                        },
                        engine.intern(HashSet::default()),
                    )
                    .await
                    == SetInputResult::Fresh;

                add_parent && add_kind && add_implemented
            };

            if !added {
                return Err(AbortError::IDCollision);
            }

            let (inner_lhs, inner_rhs) =
                self.property.generate(engine, premise).await?;

            let should_map = if self.map_at_lhs {
                Environment::new(
                    Cow::Borrowed(premise),
                    Cow::Owned(engine.clone().tracked().await),
                    normalizer::NO_OP,
                )
                .query(&Equality::new(
                    self.target_trait_member.clone().into(),
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
                    self.target_trait_member.clone().into(),
                ))
                .await?
                .is_none()
            };

            if should_map {
                premise.predicates.insert(
                    Compatible {
                        lhs: self.target_trait_member.clone(),
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
                Ok((self.target_trait_member.clone().into(), inner_rhs))
            } else {
                Ok((inner_lhs, self.target_trait_member.clone().into()))
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

    id: Global<pernixc_symbol::ID>,
}

impl Arbitrary for SymbolCongruence {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
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
            Global::arbitrary(),
        )
            .prop_map(
                |(
                    type_properties,
                    lifetime_properties,
                    constant_properties,
                    id,
                )| Self {
                    lifetime_properties,
                    type_properties,
                    constant_properties,
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

                lhs_generic_arguments.lifetimes.push(lhs);
                rhs_generic_arguments.lifetimes.push(rhs);
            }

            for strategy in &self.type_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.types.push(lhs);
                rhs_generic_arguments.types.push(rhs);
            }

            for strategy in &self.constant_properties {
                let (lhs, rhs) = strategy.generate(engine, premise).await?;

                lhs_generic_arguments.constants.push(lhs);
                rhs_generic_arguments.constants.push(rhs);
            }

            Ok((
                Symbol {
                    id: self.id,
                    generic_arguments: lhs_generic_arguments,
                }
                .into(),
                Symbol {
                    id: self.id,
                    generic_arguments: rhs_generic_arguments,
                }
                .into(),
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
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(16, 96, 6, move |inner| {
            let constant_strategy = Box::<dyn Property<Constant>>::arbitrary();
            let lifetime_strategy = Box::<dyn Property<Lifetime>>::arbitrary();

            prop_oneof![
                4 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(inner.clone()),
                    Some(constant_strategy)
                ))
                .prop_map(|x| Box::new(x) as _),
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
    trait_member: &TraitMember,
) {
    let to_be_removeds = predicates
        .iter()
        .filter(|predicate| {
            let Predicate::TraitTypeCompatible(equality) = predicate else {
                return false;
            };

            &equality.lhs == trait_member
        })
        .cloned()
        .collect::<Vec<_>>();

    for removed in to_be_removeds {
        predicates.remove(&removed);

        let Some(Type::TraitMember(next)) =
            removed.as_trait_type_compatible().map(|x| &x.rhs)
        else {
            continue;
        };

        remove_equality_recursive(predicates, next);
    }
}

#[allow(clippy::too_many_lines)]
async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
    decoy: Decoy,
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
            predicate.as_trait_type_compatible().map(|x| x.lhs.clone())
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

    /*
    // remove the type alias symbol should make the terms not equal.
    {
        let id = table.types().ids().next();
        if let Some(id) = id {
            let removed = table.types().remove(id).unwrap();
            let environment = &Environment {
                premise: &premise,
                table: &table,
                normalizer: normalizer::NO_OP,
            };

            prop_assert!(!equals(
                &term1,
                &term2,
                environment,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);
            prop_assert!(!equals(
                &term2,
                &term1,
                environment,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);

            table.types().insert_with_id(id, removed).unwrap();
        }
    }
    */

    // drop the previous environment to reduce `engine` strong count.
    drop(environment);

    for (trait_member, trait_id) in &decoy.types {
        let added = {
            let mut input_session = engine.input_session().await;
            let add_parent = input_session
                .set_input(
                    pernixc_symbol::parent::Key {
                        symbol_id: trait_member.lhs.id,
                    },
                    Some(*trait_id),
                )
                .await
                == SetInputResult::Fresh;

            let add_kind = input_session
                .set_input(
                    pernixc_symbol::kind::Key {
                        symbol_id: trait_member
                            .lhs
                            .id
                            .target_id
                            .make_global(*trait_id),
                    },
                    pernixc_symbol::kind::Kind::Trait,
                )
                .await
                == SetInputResult::Fresh;

            let add_implemented = input_session
                .set_input(
                    pernixc_semantic_element::implemented::Key {
                        symbol_id: trait_member
                            .lhs
                            .id
                            .target_id
                            .make_global(*trait_id),
                    },
                    engine.intern(HashSet::default()),
                )
                .await
                == SetInputResult::Fresh;

            add_parent && add_kind && add_implemented
        };

        if !added {
            return Err(TestCaseError::reject("ID collision"));
        }
    }

    premise.predicates.extend(
        decoy
            .types
            .into_iter()
            .map(|x| x.0)
            .map(Predicate::TraitTypeCompatible),
    );

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
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
        .query(&Equality::new(term2, term1))
        .await
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;

    environment.assert_call_stack_empty();

    prop_assert!(first_result.constraints.is_empty());
    prop_assert!(second_result.constraints.is_empty());

    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    types: Vec<(Compatible<TraitMember, Type>, pernixc_symbol::ID)>,
}

impl Arbitrary for Decoy {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let equality = (
            AssociatedSymbol::arbitrary(),
            Type::arbitrary().prop_map(purge_instance_associated),
            pernixc_symbol::ID::arbitrary(),
        )
            .prop_map(|(lhs, rhs, id)| {
                (
                    Compatible::new(
                        TraitMember(AssociatedSymbol {
                            id: lhs.id,
                            member_generic_arguments: purge_instance_associated(
                                lhs.member_generic_arguments,
                            ),
                            parent_generic_arguments: purge_instance_associated(
                                lhs.parent_generic_arguments,
                            ),
                        }),
                        rhs,
                    ),
                    id,
                )
            });

        proptest::collection::vec(equality, 0..=4)
            .prop_map(|types| Self { types })
            .boxed()
    }
}

proptest! {
    #[test]
   fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property, decoy))?;
    }

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property, decoy))?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(property_based_testing(&*property, decoy))?;
    }
}

*/
