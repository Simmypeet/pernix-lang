use std::{
    borrow::Cow, fmt::Debug, future::Future, pin::Pin, result::Result,
    sync::Arc,
};

use pernixc_hash::HashSet;
use pernixc_qbice::Engine;
use pernixc_symbol::kind::Kind;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, MemberSymbol, Symbol, TraitMember},
    generic_parameters::{
        ConstantParameterID, LifetimeParameterID, TypeParameterID,
    },
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

use super::{Log, Predicate as _, Unification, Unifier};
use crate::{
    Error, Satisfied, Succeeded,
    environment::{Environment, Premise},
    equality::Equality,
    normalizer,
    term::Term,
    test::{
        create_test_engine, purge_trait_associated_type,
        purge_trait_associated_type_in_generic_arguments,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameterUnifyConfig;

impl super::Predicate<Lifetime> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Lifetime,
        _: &Lifetime,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Type> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Type,
        _: &Type,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Constant> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Constant,
        _: &Constant,
        _: &[Log],
        _: &[Log],
    ) -> crate::Result<Satisfied> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum AbortError {
    #[error(transparent)]
    Abrupt(#[from] Error),
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
        (
            Param::arbitrary(),
            T::arbitrary().prop_map(purge_trait_associated_type),
        )
            .prop_map(|(parameter, rhs)| Self { parameter, rhs })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Basic::<LifetimeParameterID, Lifetime>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<TypeParameterID, Type>::arbitrary()
            .prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(20, 120, 6, move |inner| {
            prop_oneof![
                1=> SymbolCongruence::arbitrary_with((
                    Some(Box::<dyn Property<Lifetime>>::arbitrary()),
                    Some(inner.clone()),
                    Some(Box::<dyn Property<Constant>>::arbitrary())
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

            lhs.push(tuple::Element { term: lhs_term, is_unpacked: false });
            rhs.push(tuple::Element { term: rhs_term, is_unpacked: false });
        }

        (Tuple { elements: lhs }.into(), Tuple { elements: rhs }.into())
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

            Ok(())
        })
    }

    fn generate(&self) -> (T, T) {
        let mut lhs = Symbol {
            id: self.id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: Vec::new(),
                constants: Vec::new(),
            },
        };
        let mut rhs = Symbol {
            id: self.id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: Vec::new(),
                constants: Vec::new(),
            },
        };

        for property in &self.lifetime_properties {
            let (lhs_lifetime, rhs_lifetime) = property.generate();

            lhs.generic_arguments.lifetimes.push(lhs_lifetime);
            rhs.generic_arguments.lifetimes.push(rhs_lifetime);
        }

        for property in &self.type_properties {
            let (lhs_type, rhs_type) = property.generate();

            lhs.generic_arguments.types.push(lhs_type);
            rhs.generic_arguments.types.push(rhs_type);
        }

        for property in &self.constant_properties {
            let (lhs_constant, rhs_constant) = property.generate();

            lhs.generic_arguments.constants.push(lhs_constant);
            rhs.generic_arguments.constants.push(rhs_constant);
        }

        (lhs.into(), rhs.into())
    }

    fn node_count(&self) -> usize {
        self.lifetime_properties.iter().map(|x| x.node_count()).sum::<usize>()
            + self.type_properties.iter().map(|x| x.node_count()).sum::<usize>()
            + self
                .constant_properties
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
            Global::arbitrary(),
        )
            .prop_map(|(tys, lts, constant_properties, id)| Self {
                lifetime_properties: lts,
                type_properties: tys,
                constant_properties,
                id,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct Mapping {
    pub property: Box<dyn Property<Type>>,
    pub trait_member: TraitMember,
    pub trait_id: pernixc_symbol::ID,
}

impl Property<Type> for Mapping {
    fn apply<'x>(
        &'x self,
        engine: &'x Arc<Engine>,
        premise: &'x mut Premise,
    ) -> BoxedFuture<'x> {
        Box::pin(async move {
            {
                let mut input_session = engine.input_session();
                input_session.set_input(
                    pernixc_symbol::parent::Key {
                        symbol_id: self.trait_member.id,
                    },
                    Some(self.trait_id),
                );

                input_session.set_input(
                    pernixc_symbol::kind::Key {
                        symbol_id: self
                            .trait_member
                            .id
                            .target_id
                            .make_global(self.trait_id),
                    },
                    Kind::Trait,
                );

                input_session.set_input(
                    pernixc_semantic_element::implemented::Key {
                        symbol_id: self
                            .trait_member
                            .id
                            .target_id
                            .make_global(self.trait_id),
                    },
                    engine.intern(HashSet::default()),
                );
            }

            let (from, to) = self.generate();

            if GenericParameterUnifyConfig
                .unifiable(&from, &to, &Vec::new(), &Vec::new())?
                .is_some()
            {
                println!("skip added predicate");
                return Ok(());
            }

            self.property.apply(engine, premise).await?;

            let mapped = self.property.generate().0;

            premise.predicates.insert(Predicate::TraitTypeCompatible(
                Compatible::new(self.trait_member.clone(), mapped),
            ));

            Ok(())
        })
    }

    fn generate(&self) -> (Type, Type) {
        let term = self.property.generate().1;

        (Type::TraitMember(self.trait_member.clone()), term)
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

impl Arbitrary for Mapping {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strategy: Self::Parameters) -> Self::Strategy {
        let strategy =
            strategy.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (strategy, TraitMember::arbitrary(), pernixc_symbol::ID::arbitrary())
            .prop_map(|(property, trait_member, trait_id)| Self {
                property,
                trait_id,
                trait_member: TraitMember(MemberSymbol {
                    id: trait_member.0.id,
                    member_generic_arguments:
                        purge_trait_associated_type_in_generic_arguments(
                            trait_member.0.member_generic_arguments,
                        ),
                    parent_generic_arguments:
                        purge_trait_associated_type_in_generic_arguments(
                            trait_member.0.parent_generic_arguments,
                        ),
                }),
            })
            .boxed()
    }
}

fn rewrite_term<T: Term + 'static>(
    lhs: &mut T,
    unifier: Unifier<T>,
) -> TestCaseResult {
    if let Some(rewritten) = unifier.rewritten_from {
        *lhs = rewritten;
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

            Ok(())
        }
        super::Matching::Equality => Ok(()),
    }
}

async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let (engine, _dir) = create_test_engine();
    let mut premise = Premise::default();
    let config = GenericParameterUnifyConfig;

    let (mut lhs, rhs) = property.generate();

    property.apply(&engine, &mut premise).await?;

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
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
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(property_based_testing(&*property))?;
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
