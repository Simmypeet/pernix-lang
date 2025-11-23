use std::{borrow::Cow, fmt::Debug, future::Future, pin::Pin, sync::Arc};

use pernixc_query::Engine;
use pernixc_target::Global;
use pernixc_term::{
    constant::{self, Constant},
    generic_arguments::{GenericArguments, Symbol},
    r#type::{self, Type},
};
use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::Definite;
use crate::{
    Error,
    environment::{Environment, Premise},
    normalizer,
    term::Term,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    Abrupt(#[from] Error),
}

pub type BoxedFuture<'s, T> =
    Pin<Box<dyn Future<Output = Result<T, ApplyPropertyError>> + 's>>;

pub trait Property<T>: 'static + Debug {
    fn generate<'s>(
        &'s self,
        table: &'s mut Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, T>;

    fn node_count(&self) -> usize;
}

/// The term is trivially satisfiable without the environment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TriviallySatisfiable<T>(pub T);

impl<T: Arbitrary + 'static> Arbitrary for TriviallySatisfiable<T>
where
    T::Strategy: 'static,
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        T::arbitrary().prop_map(TriviallySatisfiable).boxed()
    }
}

impl<T: Into<U> + Clone + Debug + 'static, U> Property<U>
    for TriviallySatisfiable<T>
{
    fn generate<'s>(
        &'s self,
        _: &'s mut Arc<Engine>,
        _: &'s mut Premise,
    ) -> BoxedFuture<'s, U> {
        Box::pin(async move { Ok(self.0.clone().into()) })
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct SymbolCongruence {
    type_strategies: Vec<Box<dyn Property<Type>>>,
    constant_strategies: Vec<Box<dyn Property<Constant>>>,

    id: Global<pernixc_symbol::ID>,
}

impl Arbitrary for SymbolCongruence {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.1.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=2,
            ),
            Global::arbitrary(),
        )
            .prop_map(|(type_strategies, constant_strategies, id)| Self {
                type_strategies,
                constant_strategies,
                id,
            })
            .boxed()
    }
}

impl<T: Term + 'static> Property<T> for SymbolCongruence
where
    Symbol: Into<T>,
{
    fn generate<'s>(
        &'s self,
        table: &'s mut Arc<Engine>,
        premise: &'s mut Premise,
    ) -> BoxedFuture<'s, T> {
        Box::pin(async move {
            let mut generic_arguments = GenericArguments::default();

            for type_strategy in &self.type_strategies {
                generic_arguments
                    .types
                    .push(type_strategy.generate(table, premise).await?);
            }

            for constant_strategy in &self.constant_strategies {
                generic_arguments
                    .constants
                    .push(constant_strategy.generate(table, premise).await?);
            }

            Ok(Symbol { id: self.id, generic_arguments }.into())
        })
    }

    fn node_count(&self) -> usize {
        1 + self.type_strategies.iter().map(|x| x.node_count()).sum::<usize>()
            + self
                .constant_strategies
                .iter()
                .map(|x| x.node_count())
                .sum::<usize>()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Constant>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            TriviallySatisfiable::<r#type::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
        ];

        leaf.prop_recursive(16, 32, 2, move |inner| {
            let const_strat = args
                .clone()
                .unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary);

            prop_oneof![
                4 => SymbolCongruence::arbitrary_with((
                    Some(inner.clone()),
                    Some(const_strat.clone())
                )).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            TriviallySatisfiable::<constant::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
        ]
        .boxed()
    }
}

async fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let mut engine = Arc::new(Engine::default());

    let term = property.generate(&mut engine, &mut premise).await?;

    let environment = Environment::new(
        Cow::Borrowed(&premise),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment
        .query(&Definite(term))
        .await
        .map_err(|x| TestCaseError::reject(format!("{x}")))?;

    prop_assert!(result.is_some());

    Ok(())
}

proptest! {
    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
    ) {
        tokio::runtime::Runtime::new()
            .expect("Failed to create Tokio runtime")
            .block_on(property_based_testing::<Constant>(&*property))?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
    ) {
        tokio::runtime::Runtime::new()
            .expect("Failed to create Tokio runtime")
            .block_on(property_based_testing::<Type>(&*property))?;
    }
}
