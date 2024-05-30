use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::{self, Key, ID},
    semantic::{
        model::Default,
        normalizer::NoOp,
        predicate::definite,
        session::{self, Limit, Session},
        term::{
            constant::{self, Constant},
            r#type::{self, SymbolID, Type},
            GenericArguments, Symbol, Term,
        },
        Environment, ExceedLimitError, Premise,
    },
    symbol::{
        table::{representation::Insertion, Building, Table},
        Accessibility, GenericDeclaration, MemberID, Module, TypeDefinition,
        TypeParameter,
    },
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum ApplyPropertyError {
    #[error("{0}")]
    ExceedLimitError(#[from] ExceedLimitError),
}

pub trait Property<T>: 'static + Debug {
    fn generate(
        &self,
        module_id: ID<Module>,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<T, ApplyPropertyError>;
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
    fn generate(
        &self,
        _: ID<Module>,
        _: &mut Table<Building>,
        _: &mut Premise<Default>,
    ) -> Result<U, ApplyPropertyError> {
        Ok(self.0.clone().into())
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    type_strategies: Vec<Box<dyn Property<Type<Default>>>>,
    constant_strategies: Vec<Box<dyn Property<Constant<Default>>>>,

    id: ID,
}

impl<ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Copy>
    Arbitrary for SymbolCongruence<ID>
{
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.1.unwrap_or_else(
                    Box::<dyn Property<Constant<_>>>::arbitrary,
                ),
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

impl<ID: Debug + 'static + Copy, T: Term + 'static> Property<T>
    for SymbolCongruence<ID>
where
    Symbol<Default, ID>: Into<T>,
{
    fn generate(
        &self,
        module_id: arena::ID<Module>,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<T, ApplyPropertyError> {
        let mut generic_arguments = GenericArguments::default();

        for type_strategy in &self.type_strategies {
            generic_arguments
                .types
                .push(type_strategy.generate(module_id, table, premise)?);
        }

        for constant_strategy in &self.constant_strategies {
            generic_arguments
                .constants
                .push(constant_strategy.generate(module_id, table, premise)?);
        }

        Ok(Symbol { id: self.id, generic_arguments }.into())
    }
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters =
        Option<BoxedStrategy<Box<dyn Property<Constant<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf =
            prop_oneof![TriviallySatisfiable::<r#type::Primitive>::arbitrary()
                .prop_map(|x| Box::new(x) as _),];

        leaf.prop_recursive(16, 64, 4, move |inner| {
            let const_strat = args.clone().unwrap_or_else(
                Box::<dyn Property<Constant<_>>>::arbitrary
            );

            prop_oneof![
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

impl Arbitrary for Box<dyn Property<Constant<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![TriviallySatisfiable::<constant::Primitive>::arbitrary()
            .prop_map(|x| Box::new(x) as _),]
        .boxed()
    }
}

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type<Default>>>,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary)
            .prop_map(|property| Self { property })
            .boxed()
    }
}

impl Property<Type<Default>> for TypeAlias {
    fn generate(
        &self,
        module_id: ID<Module>,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
    ) -> Result<Type<Default>, ApplyPropertyError> {
        let inner_term = self.property.generate(module_id, table, premise)?;

        let mut generic_declaration = GenericDeclaration::default();
        let type_parameter_id = generic_declaration
            .parameters
            .add_type_parameter(TypeParameter { name: None, span: None })
            .unwrap();

        let expected_type_id = table.types().get_available_id();

        let generated_term = Type::Symbol(Symbol {
            id: SymbolID::Type(expected_type_id),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Parameter(MemberID {
                    parent: expected_type_id.into(),
                    id: type_parameter_id,
                })],
                constants: Vec::new(),
            },
        });

        let should_add = !definite(
            &generated_term,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )?;

        if should_add {
            let Insertion { id, duplication } = table
                .insert_member(
                    format!("T{}", expected_type_id.into_index()),
                    Accessibility::Public,
                    module_id,
                    None,
                    generic_declaration,
                    TypeDefinition { r#type: inner_term },
                )
                .unwrap();

            assert_eq!(id, expected_type_id);
            assert!(duplication.is_none());
        }

        Ok(generated_term)
    }
}

fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult
where
    session::Default<Default>: Session<T>,
{
    let mut premise = Premise::default();
    let mut table = Table::<Building>::default();

    let Insertion { id: module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let term = property.generate(module_id, &mut table, &mut premise)?;

    let environment =
        &Environment { table: &table, premise: &premise, normalizer: &NoOp };

    prop_assert!(definite(
        &term,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property to test"))?);

    // remove one of the mappings and check if the term is still definite
    {
        let mut premise_removed = premise.clone();
        if premise_removed.equivalent.remove_class::<Type<_>>(0).is_some()
            || premise_removed
                .equivalent
                .remove_class::<Constant<_>>(0)
                .is_some()
        {
            let environment = &Environment {
                table: &table,
                premise: &premise_removed,
                normalizer: &NoOp,
            };
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
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant<Default>>>::arbitrary(),
    ) {
        property_based_testing::<Constant<_>>(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type<Default>>>::arbitrary(),
    ) {
        property_based_testing::<Type<_>>(&*property)?;
    }
}
