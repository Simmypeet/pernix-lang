use std::{
    borrow::Cow, fmt::Debug, future::Future, pin::Pin, result::Result,
    sync::Arc,
};

use pernixc_qbice::Engine;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        ConstantParameterID, InstanceParameterID, LifetimeParameterID,
        TypeParameterID,
    },
    instance::{Instance, InstanceAssociated},
    lifetime::Lifetime,
    predicate::{Compatible, Predicate},
    sub_term::Location,
    tuple::{self, Tuple},
    r#type::Type,
};
use proptest::{
    arbitrary::Arbitrary,
    bits::usize,
    prop_assert, prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{Predicate as _, Unification, Unifier};
use crate::{
    OverflowError, Satisfied, Succeeded,
    environment::{Environment, Premise, QueryResult},
    equality::Equality,
    normalizer,
    term::Term,
    test::{
        create_test_engine, purge_instance_associated,
        purge_instance_associated_in_generic_args,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameterUnifyConfig;

impl super::Predicate<Lifetime> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Lifetime,
        _: &Lifetime,
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Type> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Type,
        _: &Type,
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Constant> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Constant,
        _: &Constant,
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Instance> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Instance,
        _: &Instance,
    ) -> QueryResult<Option<Succeeded<Satisfied>>> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum AbortError {
    #[error(transparent)]
    Overflow(#[from] OverflowError),
    #[error("collision to the ID generated on the table")]
    IDCollision,
}

pub type BoxedFuture<'x> =
    Pin<Box<dyn Future<Output = Result<(), AbortError>> + 'x>>;

pub trait Property<T>: 'static + Debug {
    fn apply<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x>;

    fn generate(&self) -> (T, T);

    fn node_count(&self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Basic<Param, T> {
    pub parameter: Param,
    pub rhs: T,
}

impl<
    Param: Debug + Clone + 'static,
    T: Debug + Clone + Term + From<Param> + 'static,
> Property<T> for Basic<Param, T>
{
    fn apply(&self, _: &Arc<Engine>, _: &mut Premise) -> BoxedFuture<'_> {
        Box::pin(async move { Ok(()) })
    }

    fn generate(&self) -> (T, T) {
        (T::from(self.parameter.clone()), self.rhs.clone())
    }

    fn node_count(&self) -> usize { 1 }
}

impl<
    Param: Debug + Arbitrary<Strategy = BoxedStrategy<Param>> + Clone + 'static,
    T: Debug
        + Term
        + From<Param>
        + Arbitrary<Strategy = BoxedStrategy<T>>
        + 'static,
> Arbitrary for Basic<Param, T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (Param::arbitrary(), T::arbitrary().prop_map(purge_instance_associated))
            .prop_map(|(parameter, rhs)| Self { parameter, rhs })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Basic::<LifetimeParameterID, Lifetime>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Instance>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(ty: Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<InstanceParameterID, Instance>::arbitrary()
            .prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(6, 36, 6, move |inner| {
            let ty = ty.clone().unwrap_or_else(|| {
                Box::<dyn Property<Type>>::arbitrary_with(Some(inner.clone()))
            });

            prop_oneof![
                1=> SymbolCongruence::arbitrary_with((
                    Some(Box::<dyn Property<Lifetime>>::arbitrary()),
                    Some(ty),
                    Some(Box::<dyn Property<Constant>>::arbitrary()),
                    Some(inner.clone())
                )).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Instance>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(inst: Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<TypeParameterID, Type>::arbitrary()
            .prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(6, 36, 6, move |inner| {
            let inst = inst.clone().unwrap_or_else(|| {
                Box::<dyn Property<Instance>>::arbitrary_with(Some(
                    inner.clone(),
                ))
            });

            prop_oneof![
                1=> SymbolCongruence::arbitrary_with((
                    Some(Box::<dyn Property<Lifetime>>::arbitrary()),
                    Some(inner.clone()),
                    Some(Box::<dyn Property<Constant>>::arbitrary()),
                    Some(inst)
                ))
                .prop_map(|x| Box::new(x) as _),
                3 => Mapping::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                1 => TupleCongruence::arbitrary_with(Some(inner))
                    .prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<ConstantParameterID, Constant>::arbitrary()
            .prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(6, 60, 6, move |inner| {
            prop_oneof![
                1 => TupleCongruence::arbitrary_with(Some(inner))
                    .prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

#[derive(Debug)]
pub struct TupleCongruence<T> {
    pub terms: Vec<Box<dyn Property<T>>>,
}

impl<T: Term> Property<T> for TupleCongruence<T>
where
    Tuple<T>: Into<T>,
{
    fn apply<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x> {
        Box::pin(async move {
            for property in &self.terms {
                property.apply(engine, premise).await?;
            }

            Ok(())
        })
    }

    fn generate(&self) -> (T, T) {
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();

        for property in &self.terms {
            let (lhs_term, rhs_term) = property.generate();

            lhs.push(tuple::Element::new_regular(lhs_term));
            rhs.push(tuple::Element::new_regular(rhs_term));
        }

        (Tuple::new(lhs).into(), Tuple::new(rhs).into())
    }

    fn node_count(&self) -> usize {
        self.terms.iter().map(|x| x.node_count()).sum()
    }
}

impl<T: Debug + 'static> Arbitrary for TupleCongruence<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strategy: Self::Parameters) -> Self::Strategy {
        let strategy =
            strategy.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        proptest::collection::vec(strategy, 1..=6)
            .prop_map(|terms| Self { terms })
            .boxed()
    }
}

#[derive(Debug)]
pub struct SymbolCongruence {
    lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    type_properties: Vec<Box<dyn Property<Type>>>,
    constant_properties: Vec<Box<dyn Property<Constant>>>,
    instance_properties: Vec<Box<dyn Property<Instance>>>,

    id: Global<pernixc_symbol::ID>,
}

impl<T: Term + 'static> Property<T> for SymbolCongruence
where
    Symbol: Into<T>,
{
    fn apply<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x> {
        Box::pin(async move {
            for property in &self.lifetime_properties {
                property.apply(engine, premise).await?;
            }

            for property in &self.type_properties {
                property.apply(engine, premise).await?;
            }

            for property in &self.constant_properties {
                property.apply(engine, premise).await?;
            }

            for property in &self.instance_properties {
                property.apply(engine, premise).await?;
            }

            Ok(())
        })
    }

    fn generate(&self) -> (T, T) {
        let mut lhs = GenericArguments::default();
        let mut rhs = GenericArguments::default();

        for property in &self.lifetime_properties {
            let (lhs_lifetime, rhs_lifetime) = property.generate();

            lhs.push_lifetime(lhs_lifetime);
            rhs.push_lifetime(rhs_lifetime);
        }

        for property in &self.type_properties {
            let (lhs_type, rhs_type) = property.generate();

            lhs.push_type(lhs_type);
            rhs.push_type(rhs_type);
        }

        for property in &self.constant_properties {
            let (lhs_constant, rhs_constant) = property.generate();

            lhs.push_constant(lhs_constant);
            rhs.push_constant(rhs_constant);
        }

        for property in &self.instance_properties {
            let (lhs_instance, rhs_instance) = property.generate();

            lhs.push_instance(lhs_instance);
            rhs.push_instance(rhs_instance);
        }

        (Symbol::new(self.id, lhs).into(), Symbol::new(self.id, rhs).into())
    }

    fn node_count(&self) -> usize {
        self.lifetime_properties.iter().map(|x| x.node_count()).sum::<usize>()
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
                1..=6,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                1..=6,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                1..=6,
            ),
            proptest::collection::vec(
                args.3.unwrap_or_else(Box::<dyn Property<Instance>>::arbitrary),
                1..=6,
            ),
            Global::arbitrary(),
        )
            .prop_map(|(tys, lts, constant_properties, is, id)| Self {
                lifetime_properties: lts,
                type_properties: tys,
                constant_properties,
                instance_properties: is,
                id,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct Mapping {
    pub property: Box<dyn Property<Type>>,
    pub instance_parameter_id: InstanceParameterID,
    pub trait_associated_symbol_id: Global<pernixc_symbol::ID>,
    pub trait_associated_symbol_generic_arguments: GenericArguments,
}

impl Property<Type> for Mapping {
    fn apply<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x> {
        Box::pin(async move {
            let (from, to) = self.generate();

            if GenericParameterUnifyConfig.unifiable(&from, &to)?.is_some() {
                println!("skip added predicate");
                return Ok(());
            }

            self.property.apply(engine, premise).await?;

            let mapped = self.property.generate().0;

            premise.predicates.insert(
                Predicate::InstanceAssociatedTypeEquality(Compatible::new(
                    InstanceAssociated::new(
                        Box::new(Instance::Parameter(
                            self.instance_parameter_id,
                        )),
                        self.trait_associated_symbol_id,
                        self.trait_associated_symbol_generic_arguments.clone(),
                    ),
                    mapped,
                )),
            );

            Ok(())
        })
    }

    fn generate(&self) -> (Type, Type) {
        let term = self.property.generate().1;

        (
            Type::InstanceAssociated(InstanceAssociated::new(
                Box::new(Instance::Parameter(self.instance_parameter_id)),
                self.trait_associated_symbol_id,
                self.trait_associated_symbol_generic_arguments.clone(),
            )),
            term,
        )
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

impl Arbitrary for Mapping {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strategy: Self::Parameters) -> Self::Strategy {
        let strategy =
            strategy.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (
            strategy,
            InstanceParameterID::arbitrary(),
            Global::arbitrary(),
            GenericArguments::arbitrary(),
        )
            .prop_map(
                |(
                    property,
                    instance_parameter_id,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments,
                )| Self {
                    property,
                    instance_parameter_id,
                    trait_associated_symbol_id,
                    trait_associated_symbol_generic_arguments:
                        trait_associated_symbol_generic_arguments
                            .purge_instance_associated_in_generic_args(),
                },
            )
            .boxed()
    }
}

fn rewrite_term<T: Term + 'static>(
    lhs: &mut T,
    unifier: Unifier<T>,
) -> TestCaseResult {
    if let Some(rewritten_from) = unifier.rewritten_from {
        *lhs = rewritten_from;
    }

    match unifier.matching {
        super::Matching::Unifiable(new_lhs, rhs) => {
            prop_assert_eq!(&*lhs, &new_lhs);
            *lhs = rhs;
            Ok(())
        }
        super::Matching::Substructural(substructural) => {
            for (lifetime_location, lifetime_unifier) in substructural.lifetimes
            {
                let mut sub_lifetime = lifetime_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_lifetime, lifetime_unifier)?;

                lifetime_location.assign_sub_term(lhs, sub_lifetime);
            }

            for (type_location, type_unifier) in substructural.types {
                let mut sub_type = type_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_type, type_unifier)?;

                type_location.assign_sub_term(lhs, sub_type);
            }

            for (constant_location, constant_unifier) in substructural.constants
            {
                let mut sub_constant = constant_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_constant, constant_unifier)?;

                constant_location.assign_sub_term(lhs, sub_constant);
            }

            for (instance_location, instance_unifier) in substructural.instances
            {
                let mut sub_instance = instance_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_instance, instance_unifier)?;

                instance_location.assign_sub_term(lhs, sub_instance);
            }

            Ok(())
        }
        super::Matching::Equality => Ok(()),
    }
}

async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let (engine, _dir) = create_test_engine().await;
    let mut premise = Premise::default();
    let config = GenericParameterUnifyConfig;

    let (mut lhs, rhs) = property.generate();

    property.apply(&engine, &mut premise).await?;

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked().await),
        normalizer::NO_OP,
    );

    if environment
        .query(&Equality::new(lhs.clone(), rhs.clone()))
        .await
        .map_err(|x| TestCaseError::reject(format!("{x}")))?
        .is_some()
    {
        return Err(TestCaseError::reject("trivially equal"));
    }

    let Some(result) = environment
        .query(&Unification::new(lhs.clone(), rhs.clone(), config))
        .await
        .map_err(|x| TestCaseError::reject(format!("{x}")))?
    else {
        return Err(TestCaseError::fail("unification failed"));
    };

    prop_assert!(result.constraints.is_empty());

    // the terms will equal by rewriting the lhs
    {
        rewrite_term(&mut lhs, result.result.clone())?;

        let Some(satisfied) = environment
            .query(&Equality::new(lhs, rhs))
            .await
            .map_err(|x| TestCaseError::reject(format!("{x}")))?
        else {
            return Err(TestCaseError::fail("should be equal"));
        };

        prop_assert!(satisfied.constraints.is_empty());
    }

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary()
    ) {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(property_based_testing(&*property))?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary()
    ) {
        let test = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(property_based_testing(&*property));

        println!("ran property with {} nodes, got {:?}", property.node_count(), test);
    }

    #[test]
    fn property_based_testing_constant(
        property in Box::<dyn Property<Constant>>::arbitrary()
    ) {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(property_based_testing(&*property))?;
    }
}
