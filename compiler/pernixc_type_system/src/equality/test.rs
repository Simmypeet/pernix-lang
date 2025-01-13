use std::{collections::BTreeSet, fmt::Debug, sync::Arc};

use pernixc_base::handler;
use pernixc_table::{GlobalID, Table, TargetID, ID};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    predicate::{Compatible, Predicate},
    r#type::{Primitive, TraitMember, Type},
    Default, MemberSymbol, Symbol,
};
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy, TestCaseError},
    prop_assert, prop_oneof, proptest,
    test_runner::TestCaseResult,
};

use crate::{
    environment::{Environment, Premise},
    equality::Equality,
    normalizer,
    term::Term,
    AbruptError,
};

#[test]
fn reflexive() {
    let table = Table::new(Arc::new(handler::Panic));
    let term = Type::<Default>::Primitive(Primitive::Bool);
    let premise = Premise::default();

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let result =
        environment.query(&Equality::new(term.clone(), term)).unwrap().unwrap();

    assert!(result.constraints.is_empty());

    environment.assert_call_stack_empty();
}

#[test]
fn symmetric() {
    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalence.clone(),
    }));

    let table = Table::new(Arc::new(handler::Panic));
    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    assert!(environment
        .query(&Equality::new(
            Type::TraitMember(trait_member.clone()),
            equivalence.clone(),
        ))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    assert!(environment
        .query(&Equality::new(equivalence, Type::TraitMember(trait_member),))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    environment.assert_call_stack_empty();
}

#[test]
fn not_equal() {
    let trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.predicates.insert(Predicate::TraitTypeCompatible(Compatible {
        lhs: trait_member.clone(),
        rhs: equivalence,
    }));

    let table = Table::new(Arc::new(handler::Panic));
    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    assert!(environment
        .query(&Equality::new(
            Type::TraitMember(trait_member.clone()),
            Type::Primitive(Primitive::Float32),
        ))
        .unwrap()
        .is_none());

    assert!(environment
        .query(&Equality::new(
            Type::Primitive(Primitive::Float32),
            Type::TraitMember(trait_member),
        ))
        .unwrap()
        .is_none());

    environment.assert_call_stack_empty();
}

#[test]
fn transitivity() {
    let first_trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let second_trait_member = TraitMember(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::TraitTypeCompatible(Compatible {
            lhs: first_trait_member.clone(),
            rhs: Type::TraitMember(second_trait_member.clone()),
        }),
        Predicate::TraitTypeCompatible(Compatible {
            lhs: second_trait_member,
            rhs: equivalence.clone(),
        }),
    ]);

    let table = Table::new(Arc::new(handler::Panic));
    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    assert!(environment
        .query(&Equality::new(
            Type::TraitMember(first_trait_member.clone()),
            equivalence.clone(),
        ))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    assert!(environment
        .query(&Equality::new(
            equivalence,
            Type::TraitMember(first_trait_member),
        ))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    environment.assert_call_stack_empty();
}

#[test]
fn congruence() {
    let first_trait_member = TraitMember::<Default>(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(1)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let second_trait_member = TraitMember(MemberSymbol {
        id: GlobalID::new(TargetID(1), ID(2)),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    });
    let first_equivalence = Type::Primitive(Primitive::Bool);
    let second_equivalence = Type::Primitive(Primitive::Int32);

    let mut premise = Premise::default();
    premise.predicates.extend([
        Predicate::TraitTypeCompatible(Compatible {
            lhs: first_trait_member.clone(),
            rhs: first_equivalence.clone(),
        }),
        Predicate::TraitTypeCompatible(Compatible {
            lhs: second_trait_member.clone(),
            rhs: second_equivalence.clone(),
        }),
    ]);

    let table = Table::new(Arc::new(handler::Panic));
    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let lhs = Type::Symbol(Symbol {
        id: GlobalID::new(TargetID(1), ID(3)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::TraitMember(first_trait_member),
                Type::TraitMember(second_trait_member),
            ],
            constants: Vec::new(),
        },
    });
    let rhs = Type::Symbol(Symbol {
        id: GlobalID::new(TargetID(1), ID(3)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![first_equivalence, second_equivalence],
            constants: Vec::new(),
        },
    });

    assert!(environment
        .query(&Equality::new(lhs.clone(), rhs.clone()))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    assert!(environment
        .query(&Equality::new(rhs, lhs))
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());

    environment.assert_call_stack_empty();
}

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn generate(
        &self,
        table: &mut Table,
        premise: &mut Premise<Default>,
    ) -> Result<(T, T), AbruptError>;

    /// Returns the number of nodes in the property.
    fn node_count(&self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identity<T> {
    pub term: T,
}

impl<T: Arbitrary> Arbitrary for Identity<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary().prop_map(|term| Self { term }).boxed()
    }
}

impl<T: Clone + Debug + 'static> Property<T> for Identity<T> {
    fn generate(
        &self,
        _: &mut Table,
        _: &mut Premise<Default>,
    ) -> Result<(T, T), AbruptError> {
        Ok((self.term.clone(), self.term.clone()))
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct Mapping<T: Term> {
    pub property: Box<dyn Property<T>>,
    pub target_trait_member: T::TraitMember,
    pub map_at_lhs: bool,
}

impl<T: 'static + Debug + Term + Arbitrary<Strategy = BoxedStrategy<T>>>
    Arbitrary for Mapping<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    T::TraitMember: Arbitrary<Strategy = BoxedStrategy<T::TraitMember>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strategy, T::TraitMember::arbitrary(), proptest::bool::ANY)
            .prop_map(|(property, target_trait_member, target_at_lhs)| Self {
                property,
                target_trait_member,
                map_at_lhs: target_at_lhs,
            })
            .boxed()
    }
}

impl<T: Term<Model = Default> + Debug + 'static> Property<T> for Mapping<T>
where
    Compatible<T::TraitMember, T>: Into<Predicate<Default>>,
{
    fn generate(
        &self,
        table: &mut Table,
        premise: &mut Premise<Default>,
    ) -> Result<(T, T), AbruptError> {
        let (inner_lhs, inner_rhs) = self.property.generate(table, premise)?;

        let should_map = if self.map_at_lhs {
            Environment::new(premise, table, normalizer::NO_OP)
                .query(&Equality::new(
                    self.target_trait_member.clone().into(),
                    inner_rhs.clone(),
                ))?
                .is_none()
        } else {
            Environment::new(premise, table, normalizer::NO_OP)
                .query(&Equality::new(
                    inner_lhs.clone(),
                    self.target_trait_member.clone().into(),
                ))?
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
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

#[derive(Debug)]
pub struct SymbolCongruence {
    lifetime_properties: Vec<Box<dyn Property<Lifetime<Default>>>>,
    type_properties: Vec<Box<dyn Property<Type<Default>>>>,
    constant_properties: Vec<Box<dyn Property<Constant<Default>>>>,

    id: GlobalID,
}

impl Arbitrary for SymbolCongruence {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime<Default>>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.1.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(
                    Box::<dyn Property<Lifetime<_>>>::arbitrary,
                ),
                0..=2,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(
                    Box::<dyn Property<Constant<_>>>::arbitrary,
                ),
                0..=2,
            ),
            GlobalID::arbitrary(),
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

impl<T: Term<Model = Default> + From<Symbol<Default>> + 'static> Property<T>
    for SymbolCongruence
{
    fn generate(
        &self,
        table: &mut Table,
        premise: &mut Premise<Default>,
    ) -> Result<(T, T), AbruptError> {
        let mut lhs_generic_arguments = GenericArguments::default();
        let mut rhs_generic_arguments = GenericArguments::default();

        for strategy in &self.lifetime_properties {
            let (lhs, rhs) = strategy.generate(table, premise)?;

            lhs_generic_arguments.lifetimes.push(lhs);
            rhs_generic_arguments.lifetimes.push(rhs);
        }

        for strategy in &self.type_properties {
            let (lhs, rhs) = strategy.generate(table, premise)?;

            lhs_generic_arguments.types.push(lhs);
            rhs_generic_arguments.types.push(rhs);
        }

        for strategy in &self.constant_properties {
            let (lhs, rhs) = strategy.generate(table, premise)?;

            lhs_generic_arguments.constants.push(lhs);
            rhs_generic_arguments.constants.push(rhs);
        }

        Ok((
            Symbol { id: self.id, generic_arguments: lhs_generic_arguments }
                .into(),
            Symbol { id: self.id, generic_arguments: rhs_generic_arguments }
                .into(),
        ))
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

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(50, 100, 2, move |inner| {
            let constant_strategy = Box::<dyn Property<Constant<Default>>>::arbitrary();
            let lifetime_strategy = Box::<dyn Property<Lifetime<Default>>>::arbitrary();

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

impl Arbitrary for Box<dyn Property<Lifetime<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identity::arbitrary().prop_map(|x| Box::new(x) as _).boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Identity::arbitrary().prop_map(|x| Box::new(x) as _).boxed()
    }
}

fn remove_equality_recursive(
    predicates: &mut BTreeSet<Predicate<Default>>,
    trait_member: &TraitMember<Default>,
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
fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
    decoy: Decoy,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let mut table = Table::new(Arc::new(handler::Panic));

    let (term1, term2) =
        property.generate(&mut table, &mut premise).map_err(|error| {
            TestCaseError::reject(format!(
                "abrupt error {error}; skipping now .."
            ))
        })?;

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let first_result = environment
        .query(&Equality::new(term1.clone(), term2.clone()))
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;

    let second_result = environment
        .query(&Equality::new(term2.clone(), term1.clone()))
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

        let modified_environment =
            Environment::new(&modified_premise, &table, normalizer::NO_OP);

        let first_result = modified_environment
            .query(&Equality::new(term1.clone(), term2.clone()))
            .map_err(|x| {
                TestCaseError::reject(format!(
                    "abrupt error {x}; skipping now .."
                ))
            })?;
        let second_result = modified_environment
            .query(&Equality::new(term2.clone(), term1.clone()))
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

    premise
        .predicates
        .extend(decoy.types.into_iter().map(Predicate::TraitTypeCompatible));

    let environment = Environment::new(&premise, &table, normalizer::NO_OP);

    let first_result = environment
        .query(&Equality::new(term1.clone(), term2.clone()))
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;
    let second_result = environment
        .query(&Equality::new(term2, term1))
        .map_err(|x| {
            TestCaseError::reject(format!("abrupt error {x}; skipping now .."))
        })?
        .ok_or_else(|| TestCaseError::fail("equality failed"))?;

    environment.assert_call_stack_empty();

    prop_assert!(first_result.constraints.is_empty());
    prop_assert!(second_result.constraints.is_empty());

    println!("success with {} nodes", property.node_count());
    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    types: Vec<Compatible<TraitMember<Default>, Type<Default>>>,
}

impl Arbitrary for Decoy {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;
    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let equality = (MemberSymbol::arbitrary(), Type::arbitrary())
            .prop_map(|(lhs, rhs)| Compatible::new(TraitMember(lhs), rhs));

        proptest::collection::vec(equality, 0..=4)
            .prop_map(|types| Self { types })
            .boxed()
    }
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        cases: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type<Default>>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant<Default>>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime<Default>>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }
}
