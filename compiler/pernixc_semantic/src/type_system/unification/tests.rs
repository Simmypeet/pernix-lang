use std::{fmt::Debug, result::Result, sync::Arc};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{Log, Predicate as _, Unification, Unifier};
use crate::{
    symbol::{
        table::{Building, Table},
        ConstantParameterID, LifetimeParameterID, TypeParameterID,
    },
    type_system::{
        equality::Equality,
        model::Default,
        normalizer::NoOp,
        predicate::Predicate,
        sub_term::Location,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            GenericArguments, Symbol, Term, Tuple, TupleElement,
        },
        Compute, Environment, Output, OverflowError, Premise, Satisfied,
        Succeeded,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameterUnifyConfig;

impl super::Predicate<Lifetime<Default>> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Lifetime<Default>,
        _: &Lifetime<Default>,
        _: &Vec<Log<Default>>,
        _: &Vec<Log<Default>>,
    ) -> Result<Output<Satisfied, Default>, OverflowError> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Type<Default>> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Type<Default>,
        _: &Type<Default>,
        _: &Vec<Log<Default>>,
        _: &Vec<Log<Default>>,
    ) -> Result<Output<Satisfied, Default>, OverflowError> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

impl super::Predicate<Constant<Default>> for GenericParameterUnifyConfig {
    fn unifiable(
        &self,
        from: &Constant<Default>,
        _: &Constant<Default>,
        _: &Vec<Log<Default>>,
        _: &Vec<Log<Default>>,
    ) -> Result<Output<Satisfied, Default>, OverflowError> {
        Ok(from.is_parameter().then_some(Succeeded::satisfied()))
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    ExceedLimitError(#[from] OverflowError),
    #[error("failed to apply the environment")]
    TypeAliasIDCollision,
}

pub trait Property<T>: 'static + Debug {
    fn apply(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError>;
    fn generate(&self) -> (T, T);
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
    fn apply(
        &self,
        _: &mut Table<Building>,
        _: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        Ok(())
    }

    fn generate(&self) -> (T, T) {
        (T::from(self.parameter.clone()), self.rhs.clone())
    }
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
        (Param::arbitrary(), T::arbitrary())
            .prop_map(|(parameter, rhs)| Self { parameter, rhs })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime<Default>>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Basic::<LifetimeParameterID, Lifetime<_>>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<TypeParameterID, Type<_>>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed();

        leaf.prop_recursive(6, 10, 60, move |inner| {

            prop_oneof![
                6 => SymbolCongruence::arbitrary_with(
                    (
                        Some(Box::<dyn Property<Lifetime<_>>>::arbitrary()),
                        Some(inner.clone()),
                        Some(Box::<dyn Property<Constant<_>>>::arbitrary())
                    )
                ).prop_map(|x| Box::new(x) as _),
                1 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                4 => TupleCongruence::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<ConstantParameterID, Constant<_>>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed();

        leaf.prop_recursive(6, 24, 4, move |inner| {
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
    fn apply(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        for property in &self.terms {
            property.apply(table, premise)?;
        }

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();

        for property in &self.terms {
            let (lhs_term, rhs_term) = property.generate();

            lhs.push(TupleElement { term: lhs_term, is_unpacked: false });
            rhs.push(TupleElement { term: rhs_term, is_unpacked: false });
        }

        (Tuple { elements: lhs }.into(), Tuple { elements: rhs }.into())
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

        proptest::collection::vec(strategy, 1..=4)
            .prop_map(|terms| Self { terms })
            .boxed()
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    lifetime_properties: Vec<Box<dyn Property<Lifetime<Default>>>>,
    type_properties: Vec<Box<dyn Property<Type<Default>>>>,
    constant_properties: Vec<Box<dyn Property<Constant<Default>>>>,

    id: ID,
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T>
    for SymbolCongruence<ID>
where
    Symbol<Default, ID>: Into<T>,
{
    fn apply(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        for property in &self.lifetime_properties {
            property.apply(table, premise)?;
        }

        for property in &self.type_properties {
            property.apply(table, premise)?;
        }

        for property in &self.constant_properties {
            property.apply(table, premise)?;
        }

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        let mut lhs = Symbol {
            id: self.id.clone(),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: Vec::new(),
                constants: Vec::new(),
            },
        };
        let mut rhs = Symbol {
            id: self.id.clone(),
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
}

impl<ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Clone>
    Arbitrary for SymbolCongruence<ID>
{
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
                1..=2,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(
                    Box::<dyn Property<Lifetime<_>>>::arbitrary,
                ),
                1..=2,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(
                    Box::<dyn Property<Constant<_>>>::arbitrary,
                ),
                1..=2,
            ),
            ID::arbitrary(),
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
    pub property: Box<dyn Property<Type<Default>>>,
    pub trait_member: r#type::TraitMember<Default>,
}

impl Property<Type<Default>> for Mapping {
    fn apply(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        let (from, to) = self.generate();

        if GenericParameterUnifyConfig
            .unifiable(&from, &to, &Vec::new(), &Vec::new())?
            .is_some()
        {
            return Ok(());
        }

        self.property.apply(table, premise)?;

        let mapped = self.property.generate().1;

        premise.predicates.insert(Predicate::TraitTypeEquality(Equality::new(
            self.trait_member.clone(),
            mapped,
        )));

        Ok(())
    }

    fn generate(&self) -> (Type<Default>, Type<Default>) {
        let term = self.property.generate().1;

        (term, Type::TraitMember(self.trait_member.clone()))
    }
}

impl Arbitrary for Mapping {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strategy: Self::Parameters) -> Self::Strategy {
        let strategy = strategy
            .unwrap_or_else(Box::<dyn Property<Type<Default>>>::arbitrary);

        (strategy, r#type::TraitMember::arbitrary())
            .prop_map(|(property, trait_member)| Self {
                property,
                trait_member,
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

                lifetime_location
                    .assign_sub_term(lhs, sub_lifetime)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            for (type_location, type_unifier) in substructural.types {
                let mut sub_type = type_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_type, type_unifier)?;

                type_location
                    .assign_sub_term(lhs, sub_type)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            for (constant_location, constant_unifier) in substructural.constants
            {
                let mut sub_constant = constant_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_constant, constant_unifier)?;

                constant_location
                    .assign_sub_term(lhs, sub_constant)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            Ok(())
        }
        super::Matching::Equality => Ok(()),
    }
}

fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut table = Table::default();
    let mut premise = Premise::default();
    let config = GenericParameterUnifyConfig;

    let (mut lhs, rhs) = property.generate();

    property.apply(&mut table, &mut premise)?;

    let environment =
        Environment { premise: &premise, table: &table, normalizer: &NoOp };

    if Equality::new(lhs.clone(), rhs.clone())
        .query(&environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .is_some()
    {
        return Err(TestCaseError::reject("trivially equalities"));
    }

    let Succeeded { result: unifier, constraints } =
        Unification::new(lhs.clone(), rhs.clone(), Arc::new(config))
            .query(&environment)
            .map_err(|_| TestCaseError::reject("too complex property"))?
            .unwrap();

    prop_assert!(constraints.is_empty());

    // the terms will equal by rewriting the lhs
    {
        rewrite_term(&mut lhs, unifier)?;

        let environment =
            Environment { premise: &premise, table: &table, normalizer: &NoOp };

        let Some(satisfied) = Equality::new(lhs, rhs)
            .query(&environment)
            .map_err(|_| TestCaseError::reject("too complex property"))?
        else {
            return Err(TestCaseError::reject("should be equal"));
        };

        prop_assert!(satisfied.constraints.is_empty());
    }

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        cases: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(property in Box::<dyn Property<Lifetime<_>>>::arbitrary()) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_type(property in Box::<dyn Property<Type<_>>>::arbitrary()) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_constant(property in Box::<dyn Property<Constant<_>>>::arbitrary()) {
        property_based_testing(&*property)?;
    }
}
