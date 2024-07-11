use std::fmt::Debug;

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::ID,
    symbol::{
        table::{
            representation::{IndexMut, Insertion},
            Building, Table,
        },
        Accessibility, AdtTemplate, GenericDeclaration, LifetimeParameter,
        Module, StructDefinition, Variance,
    },
    type_system::{
        equality,
        model::Default,
        normalizer::NoOp,
        predicate::{Equality, Outlives, Predicate},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{SymbolID, Type},
            GenericArguments, Symbol, Term,
        },
        Compute, Environment, OverflowError, Premise,
    },
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum GenerateError {
    #[error(transparent)]
    Overflow(#[from] OverflowError),

    #[error(
        "duplicated struct name found, it most likely comes from \
         `LifetimeMatching` property"
    )]
    DuplicatedStructName,
}

/// A trait for generating term for checking predicate
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, Lifetime<Default>), GenerateError>;

    fn node_count(&self) -> usize;
}

#[derive(Debug)]
pub struct ByEquality<T: Term> {
    pub equality: T::TraitMember,
    pub property: Box<dyn Property<T>>,
}

impl<T: Term<Model = Default>> Property<T> for ByEquality<T>
where
    Equality<T::TraitMember, T>: Into<Predicate<Default>>,
{
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, Lifetime<Default>), GenerateError> {
        let (inner_operand, inner_bound) =
            self.property.generate(table, premise, root_module_id)?;

        let outlives =
            Outlives::new(T::from(self.equality.clone()), inner_bound);
        let environment =
            Environment { premise: premise.clone(), table, normalizer: &NoOp };

        if outlives.query(&environment)?.is_none() {
            premise.predicates.insert(
                Equality { lhs: self.equality.clone(), rhs: inner_operand }
                    .into(),
            );
        }

        Ok((T::from(self.equality.clone()), inner_bound))
    }

    fn node_count(&self) -> usize { self.property.node_count() + 1 }
}

impl<T: Term> Arbitrary for ByEquality<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    T::TraitMember: Arbitrary<Strategy = BoxedStrategy<T::TraitMember>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (T::TraitMember::arbitrary(), args)
            .prop_map(|(equality, property)| Self { equality, property })
            .boxed()
    }
}

#[derive(Debug)]
pub struct LifetimeMatching {
    pub struct_name: String,
    pub lifetime_properties: Vec<Box<dyn Property<Lifetime<Default>>>>,
    pub bound: Lifetime<Default>,
}

impl Property<Type<Default>> for LifetimeMatching {
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(Type<Default>, Lifetime<Default>), GenerateError> {
        let mut operand_lifetimes = Vec::new();
        let mut bound_lifetimes = Vec::new();

        for lifetime_prop in &self.lifetime_properties {
            let (operand, bound) =
                lifetime_prop.generate(table, premise, root_module_id)?;

            operand_lifetimes.push(operand);
            bound_lifetimes.push(bound);
        }

        let Insertion { id, duplication } = table
            .insert_member(
                self.struct_name.clone(),
                Accessibility::Public,
                root_module_id,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        if duplication.is_some() {
            return Err(GenerateError::DuplicatedStructName);
        }

        // create lifetime generic parmaeters and variances
        {
            let struct_sym = table.get_mut(id).unwrap();

            for _ in 0..self.lifetime_properties.len() {
                let lifetime_param = struct_sym
                    .generic_declaration
                    .parameters
                    .add_lifetime_parameter(LifetimeParameter {
                        name: None,
                        span: None,
                    })
                    .unwrap();

                struct_sym
                    .generic_parameter_variances
                    .variances_by_lifetime_ids
                    .insert(lifetime_param, Variance::Covariant);
            }
        }

        let ty_operand = Type::Symbol(Symbol {
            id: SymbolID::Struct(id),
            generic_arguments: GenericArguments {
                lifetimes: operand_lifetimes,
                types: Vec::new(),
                constants: Vec::new(),
            },
        });

        if Outlives::new(ty_operand.clone(), self.bound)
            .query(&Environment {
                premise: premise.clone(),
                table,
                normalizer: &NoOp,
            })?
            .is_none()
        {
            premise.predicates.insert(Predicate::TypeOutlives(Outlives {
                operand: Type::Symbol(Symbol {
                    id: SymbolID::Struct(id),
                    generic_arguments: GenericArguments {
                        lifetimes: bound_lifetimes,
                        types: Vec::new(),
                        constants: Vec::new(),
                    },
                }),
                bound: self.bound,
            }));
        }

        Ok((ty_operand, self.bound))
    }

    fn node_count(&self) -> usize {
        1 + self
            .lifetime_properties
            .iter()
            .map(|x| x.node_count())
            .sum::<usize>()
    }
}

impl Arbitrary for LifetimeMatching {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            "[a-zA-Z_][a-zA-Z0-9_]*",
            proptest::collection::vec(
                Box::<dyn Property<Lifetime<_>>>::arbitrary(),
                1..=8,
            ),
            Lifetime::arbitrary(),
        )
            .prop_map(|(struct_name, lifetime_properties, bound)| Self {
                struct_name,
                lifetime_properties,
                bound,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct Reflexive {
    pub term: Box<dyn equality::tests::Property<Lifetime<Default>>>,
}

impl Property<Lifetime<Default>> for Reflexive {
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(Lifetime<Default>, Lifetime<Default>), GenerateError> {
        let (term, bound) =
            self.term.generate(table, premise, root_module_id)?;

        Ok((term, bound))
    }

    fn node_count(&self) -> usize { 1 }
}

impl Arbitrary for Reflexive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Box::<dyn equality::tests::Property<Lifetime<_>>>::arbitrary()
            .prop_map(|term| Self { term })
            .boxed()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByPremise<T> {
    pub term: T,
    pub bound: Lifetime<Default>,
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for ByPremise<T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (T::arbitrary(), Lifetime::arbitrary())
            .prop_map(|(term, bound)| Self { term, bound })
            .boxed()
    }
}

impl<T: Term<Model = Default>> Property<T> for ByPremise<T>
where
    Outlives<T>: Into<Predicate<Default>>,
{
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        _: ID<Module>,
    ) -> Result<(T, Lifetime<Default>), GenerateError> {
        if Outlives::new(self.term.clone(), self.bound)
            .query(&Environment {
                premise: premise.clone(),
                table,
                normalizer: &NoOp,
            })?
            .is_none()
        {
            premise.predicates.insert(
                Outlives { operand: self.term.clone(), bound: self.bound }
                    .into(),
            );
        }

        Ok((self.term.clone(), self.bound))
    }

    fn node_count(&self) -> usize { 1 }
}

#[derive(Debug)]
pub struct Transitive<T> {
    pub inner_property: Box<dyn Property<T>>,
    pub final_bound: Lifetime<Default>,
}

impl<T: Term<Model = Default>> Property<T> for Transitive<T> {
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, Lifetime<Default>), GenerateError> {
        let (inner_operand, inner_bound) =
            self.inner_property.generate(table, premise, root_module_id)?;

        if Outlives::new(inner_operand.clone(), self.final_bound)
            .query(&Environment {
                premise: premise.clone(),
                table,
                normalizer: &NoOp,
            })?
            .is_none()
        {
            premise.predicates.insert(Predicate::LifetimeOutlives(Outlives {
                operand: inner_bound,
                bound: self.final_bound,
            }));
        }

        Ok((inner_operand, self.final_bound.clone()))
    }

    fn node_count(&self) -> usize { 1 + self.inner_property.node_count() }
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for Transitive<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let args = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (args, Lifetime::arbitrary())
            .prop_map(|(inner_property, bound)| Self {
                inner_property,
                final_bound: bound,
            })
            .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = ByPremise::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(4, 4, 1, |inner| {
            prop_oneof![
                Transitive::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                ByEquality::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Box::new(x) as _),
                LifetimeMatching::arbitrary().prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

impl Arbitrary for Box<dyn Property<Lifetime<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Reflexive::arbitrary().prop_map(|x| Box::new(x) as _),
            ByPremise::arbitrary().prop_map(|x| Box::new(x) as _),
        ];

        leaf.prop_recursive(4, 8, 2, |inner| {
            Transitive::arbitrary_with(Some(inner.clone()))
                .prop_map(|x| Box::new(x) as _)
        })
        .boxed()
    }
}

fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
) -> TestCaseResult {
    let mut premise = Premise::default();
    let mut table = Table::<Building>::default();

    let Insertion { id: root_module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let (term1, term2) = property
        .generate(&mut table, &mut premise, root_module_id)
        .map_err(|_| TestCaseError::reject("too complex property"))?;

    println!("{term1:#?}: {term2:#?}");
    println!("{premise:#?}");

    let environment =
        &Environment { table: &table, premise, normalizer: &NoOp };

    prop_assert!(Outlives::new(term1, term2)
        .query(environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .is_some());

    Ok(())
}

proptest! {
    #![proptest_config(proptest::test_runner::Config {
        max_shrink_iters: 100_000,
        max_global_rejects: 8192,
        ..std::default::Default::default()
    })]

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime<Default>>>::arbitrary()
    ) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type<Default>>>::arbitrary()
    ) {
        property_based_testing(&*property)?;
    }

    #[test]
    fn constant_always_outlives(
        constant in Constant::arbitrary(),
        lifetime in Lifetime::arbitrary()
    ) {
        let environment = &Environment {
            table: &Table::<Building>::default(),
            premise: Premise::default(),
            normalizer: &NoOp
        };
        prop_assert!(
            Outlives::new(constant, lifetime).query(environment)?.is_some()
        );
    }
}
