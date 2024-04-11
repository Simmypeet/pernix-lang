use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::ID,
    semantic::{
        predicate::definite,
        session::{self, ExceedLimitError, Limit, Session},
        term::{
            constant::{self, Constant},
            r#type::{self, SymbolID, Type},
            GenericArguments, Symbol, Term,
        },
        tests::State,
        Environment, Premise,
    },
    symbol::{
        self, ConstantParameterID, GenericDeclaration, GenericID,
        GenericParameters, TypeParameter, TypeParameterID,
    },
    table::Table,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    ExceedLimitError(#[from] ExceedLimitError),

    #[error("type alias id collision")]
    TypeAliasIDCollision,
}

/// A trait for generating term for checking definite predicate satisfiability.
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    ///
    /// # Returns
    ///
    /// Returns `false` if failed to apply the environment to the
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError>;

    /// Generate a term for checking
    #[must_use]
    fn generate(&self) -> T;
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
    fn apply(
        &self,
        _: &mut Table<State>,
        _: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        Ok(())
    }

    fn generate(&self) -> U { self.0.clone().into() }
}

#[derive(Debug)]
pub struct Mapping<Param, T> {
    generic_parameter: Param,
    property: Box<dyn Property<T>>,
}

impl<Param: Debug + Arbitrary, T: Debug + 'static> Arbitrary
    for Mapping<Param, T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    Param::Strategy: 'static,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::arbitrary);

        (Param::arbitrary(), args)
            .prop_map(|(generic_parameter, property)| Self {
                generic_parameter,
                property,
            })
            .boxed()
    }
}

impl<Param: Debug + Into<T> + Clone + 'static, T: Term + Debug + 'static>
    Property<T> for Mapping<Param, T>
where
    session::Default: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        if definite(
            &self.generate(),
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.property.apply(table, premise)?;

        if definite(
            &self.generate(),
            &Environment { premise, table },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        premise.equivalent.insert(self.generate(), self.property.generate());

        Ok(())
    }

    fn generate(&self) -> T { self.generic_parameter.clone().into() }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    type_strategies: Vec<Box<dyn Property<Type>>>,
    constant_strategies: Vec<Box<dyn Property<Constant>>>,

    id: ID,
}

impl<ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Clone>
    Arbitrary for SymbolCongruence<ID>
{
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
            ID::arbitrary(),
        )
            .prop_map(|(type_strategies, constant_strategies, id)| Self {
                type_strategies,
                constant_strategies,
                id,
            })
            .boxed()
    }
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T>
    for SymbolCongruence<ID>
where
    Symbol<ID>: Into<T>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        for type_strategy in &self.type_strategies {
            type_strategy.apply(table, premise)?;
        }

        for constant_strategy in &self.constant_strategies {
            constant_strategy.apply(table, premise)?;
        }

        Ok(())
    }

    fn generate(&self) -> T {
        Symbol {
            id: self.id.clone(),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: self
                    .type_strategies
                    .iter()
                    .map(|x| x.generate())
                    .collect(),
                constants: self
                    .constant_strategies
                    .iter()
                    .map(|x| x.generate())
                    .collect(),
            },
        }
        .into()
    }
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Constant>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![TriviallySatisfiable::<r#type::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),];

        leaf.prop_recursive(16, 64, 4, move |inner| {
            let const_strat = args.clone().unwrap_or_else(|| {
                Box::<dyn Property<Constant>>::arbitrary_with(Some(inner.clone()))
            });

            prop_oneof![
                1 => Mapping::<TypeParameterID, Type>::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                4 => SymbolCongruence::<SymbolID>::arbitrary_with((
                    Some(inner.clone()),
                    Some(const_strat.clone())
                )).prop_map(|x| Box::new(x) as _),
                1 => TypeAlias::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Constant>> {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            TriviallySatisfiable::<constant::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),
        ];

        leaf.prop_recursive(16, 64, 4, move |inner| {
            let ty_strat = args
                .clone()
                .unwrap_or_else(|| Box::<dyn Property<Type>>::arbitrary_with(Some(inner.clone())));

            prop_oneof![
                1 => Mapping::<ConstantParameterID, Constant>::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                4 => SymbolCongruence::<ID<symbol::Constant>>::arbitrary_with((
                    Some(ty_strat),
                    Some(inner.clone()),
                )).prop_map(|x| Box::new(x) as _)
            ]
        })
        .boxed()
    }
}

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type>>,
    type_id: ID<symbol::Type>,
    type_parameter: TypeParameterID,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary),
            ID::arbitrary(),
            TypeParameterID::arbitrary(),
        )
            .prop_map(|(property, type_id, type_parameter)| Self {
                property,
                type_id,
                type_parameter,
            })
            .boxed()
    }
}

impl Property<Type> for TypeAlias {
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        self.property.apply(table, premise)?;

        let type_symbol = symbol::Type {
            id: self.type_id,
            generic_declaration: GenericDeclaration {
                parameters: {
                    let mut generic_parameters = GenericParameters::default();

                    assert!(generic_parameters
                        .add_type_parameter(TypeParameter {
                            name: "T".to_string(),
                            parent_generic_id: GenericID::Type(self.type_id),
                            span: None,
                        })
                        .is_ok());

                    generic_parameters
                },
                predicates: Vec::new(),
            },
            parent_id: ID::new(0),
            span: None,
            name: "Test".to_string(),
            data: symbol::TypeData {
                accessibility: symbol::Accessibility::Public,
                r#type: self.property.generate(),
            },
        };

        if table
            .representation
            .types
            .insert_with_id(self.type_id, type_symbol)
            .is_err()
        {
            return Err(ApplyPropertyError::TypeAliasIDCollision);
        }

        Ok(())
    }

    fn generate(&self) -> Type {
        Type::Symbol(Symbol {
            id: SymbolID::Type(self.type_id),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(self.type_parameter)],
                constants: Vec::new(),
            },
        })
    }
}

fn property_based_testing<T: Term + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult
where
    session::Default: Session<T>,
{
    let term = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<State>::default();

    property.apply(&mut table, &mut premise).map_err(|x| match x {
        ApplyPropertyError::ExceedLimitError(_) => {
            TestCaseError::reject("too complex property to test")
        }
        ApplyPropertyError::TypeAliasIDCollision => {
            TestCaseError::reject("type alias id collision")
        }
    })?;

    let environment = &Environment { table: &table, premise: &premise };
    prop_assert!(definite(
        &term,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property to test"))?);

    // remove one of the mappings and check if the term is still definite
    {
        let mut premise_removed = premise.clone();
        if premise_removed.equivalent.remove_class::<Type>(0).is_some()
            || premise_removed.equivalent.remove_class::<Constant>(0).is_some()
        {
            let environment =
                &Environment { table: &table, premise: &premise_removed };
            prop_assert!(!definite(
                &term,
                environment,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject(
                "too complex property to test"
            ))?);
        }
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
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
    ) {
        property_based_testing::<Constant>(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
    ) {
        property_based_testing::<Type>(&*property)?;
    }
}
