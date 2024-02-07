use std::{fmt::Debug, result::Result};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{unify, Config, Unification};
use crate::{
    semantic::{
        self,
        equality::equals,
        mapping,
        session::{self, ExceedLimitError, Limit, Session},
        subterm::Location,
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments, Symbol, Term,
        },
        Premise, Semantic,
    },
    symbol::{ConstantParameterID, LifetimeParameterID, TypeParameterID},
    table::{Success, Table},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameterUnifyConfig;

impl Config for GenericParameterUnifyConfig {
    fn lifetime_unifiable(&mut self, from: &Lifetime, _: &Lifetime) -> bool {
        from.is_parameter()
    }

    fn type_unifiable(&mut self, from: &Type, _: &Type) -> bool {
        from.is_parameter()
    }

    fn constant_unifiable(&mut self, from: &Constant, _: &Constant) -> bool {
        from.is_parameter()
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    ExceedLimitError(#[from] ExceedLimitError),
    #[error("failed to apply the environment")]
    TypeAliasIDCollision,
}

pub trait Property<T>: 'static + Debug {
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
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
        _: &mut Table<Success>,
        _: &mut Premise,
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
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<TypeParameterID, Type>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed();

        leaf.prop_recursive(6, 10, 60, move |inner| {
            let lifetime_prop = args.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((Some(inner.clone()), args.1.clone()))
            });
            let constant_prop = args.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Constant>>::arbitrary_with((args.0.clone(), Some(inner.clone())))
            });

            prop_oneof![
                6 => SymbolCongruence::arbitrary_with(
                    (Some(lifetime_prop), Some(inner.clone()), Some(constant_prop))
                ).prop_map(|x| Box::new(x) as _),
                1 => Mapping::<_, TypeParameterID>::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = Basic::<ConstantParameterID, Constant>::arbitrary()
            .prop_map(|x| Box::new(x) as _)
            .boxed();

        leaf.prop_recursive(6, 60, 10, move |inner| {
            let lifetime_prop = args.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((args.1.clone(), Some(inner.clone())))
            });
            let type_prop = args.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Type>>::arbitrary_with((args.0.clone(), Some(inner.clone())))
            });

            prop_oneof![
                6 => SymbolCongruence::arbitrary_with(
                    (Some(lifetime_prop), Some(type_prop), Some(inner.clone()))
                ).prop_map(|x| Box::new(x) as _),
                1 => Mapping::<_, ConstantParameterID>::arbitrary_with(Some(inner))
                .prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    type_properties: Vec<Box<dyn Property<Type>>>,
    constant_properties: Vec<Box<dyn Property<Constant>>>,

    id: ID,
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T>
    for SymbolCongruence<ID>
where
    Symbol<ID>: Into<T>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
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
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.1.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
                1..=2,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                1..=2,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
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
pub struct Mapping<T, Param> {
    pub property: Box<dyn Property<T>>,
    pub mapping: Param,
}

impl<
        T: Debug + Term + From<Param> + 'static,
        Param: Debug + Clone + 'static,
    > Property<T> for Mapping<T, Param>
{
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if T::unifiable(&lhs, &rhs, &mut GenericParameterUnifyConfig) {
            return Ok(());
        }

        self.property.apply(table, premise)?;

        let mapped = self.property.generate().1;

        premise
            .equalities_mapping
            .insert(T::from(self.mapping.clone()), mapped);

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        let term = self.property.generate().1;

        (term, T::from(self.mapping.clone()))
    }
}

impl<
        T: Debug + Arbitrary<Strategy = BoxedStrategy<T>> + 'static,
        Param: Debug + Arbitrary<Strategy = BoxedStrategy<Param>> + 'static,
    > Arbitrary for Mapping<T, Param>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strategy: Self::Parameters) -> Self::Strategy {
        let strategy =
            strategy.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strategy, Param::arbitrary())
            .prop_map(|(property, mapping)| Self { property, mapping })
            .boxed()
    }
}

fn add_equality_mapping_from_unification<T: Term>(
    unification: &Unification<T>,
    equality_mapping: &mut mapping::Mapping,
) -> TestCaseResult {
    match &unification.r#match {
        super::Matching::Unifiable(lhs, rhs) => {
            let mut config = GenericParameterUnifyConfig;

            prop_assert!(T::unifiable(lhs, rhs, &mut config));
            equality_mapping.insert(lhs.clone(), rhs.clone());

            Ok(())
        }
        super::Matching::Substructural(substructural) => {
            for lifetime_unification in substructural.lifetimes.values() {
                add_equality_mapping_from_unification::<Lifetime>(
                    lifetime_unification,
                    equality_mapping,
                )?;
            }

            for type_unification in substructural.types.values() {
                add_equality_mapping_from_unification::<Type>(
                    type_unification,
                    equality_mapping,
                )?;
            }

            for constant_unification in substructural.constants.values() {
                add_equality_mapping_from_unification::<Constant>(
                    constant_unification,
                    equality_mapping,
                )?;
            }

            Ok(())
        }
        super::Matching::Equality => Ok(()),
    }
}

fn rewrite_term<T: Term + 'static>(
    lhs: &mut T,
    unification: Unification<T>,
) -> TestCaseResult {
    if let Some(rewritten) = unification.rewritten_lhs {
        *lhs = rewritten;
    }

    match unification.r#match {
        super::Matching::Unifiable(new_lhs, rhs) => {
            prop_assert_eq!(&*lhs, &new_lhs);
            *lhs = rhs;
            Ok(())
        }
        super::Matching::Substructural(substructural) => {
            for (lifetime_location, lifetime_unification) in
                substructural.lifetimes
            {
                let mut sub_lifetime = lifetime_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_lifetime, lifetime_unification)?;

                lifetime_location
                    .assign_sub_term(lhs, sub_lifetime)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            for (type_location, type_unification) in substructural.types {
                let mut sub_type = type_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_type, type_unification)?;

                type_location
                    .assign_sub_term(lhs, sub_type)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            for (constant_location, constant_unification) in
                substructural.constants
            {
                let mut sub_constant = constant_location
                    .get_sub_term(lhs)
                    .ok_or_else(|| TestCaseError::fail("invalid location"))?;

                rewrite_term(&mut sub_constant, constant_unification)?;

                constant_location
                    .assign_sub_term(lhs, sub_constant)
                    .map_err(|_| TestCaseError::fail("invalid location"))?;
            }

            Ok(())
        }
        super::Matching::Equality => Ok(()),
    }
}

pub fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let mut table = Table::default();
    let mut premise = Premise::default();
    let mut config = GenericParameterUnifyConfig;

    let (mut lhs, rhs) = property.generate();

    if equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )? {
        return Err(TestCaseError::reject("trivially equalities"));
    }

    property.apply(&mut table, &mut premise)?;

    let unification = unify(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut config,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?
    .unwrap();

    // the terms will equal when adding the equality mapping
    {
        let mut premise_cloned = premise.clone();
        add_equality_mapping_from_unification(
            &unification,
            &mut premise_cloned.equalities_mapping,
        )?;

        prop_assert!(equals(
            &lhs,
            &rhs,
            &premise_cloned,
            &table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )?);
    }

    // the terms will equal by rewriting the lhs
    {
        rewrite_term(&mut lhs, unification)?;

        prop_assert!(equals(
            &lhs,
            &rhs,
            &premise,
            &table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )?);
    }

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        cases: 8192,
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(property in Box::<dyn Property<Lifetime>>::arbitrary()) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_type(property in Box::<dyn Property<Type>>::arbitrary()) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_constant(property in Box::<dyn Property<Constant>>::arbitrary()) {
        property_based_testing(&*property)?;
    }
}
