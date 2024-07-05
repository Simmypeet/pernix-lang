use std::{collections::HashSet, fmt::Debug};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{
    arena::{self, Key, ID},
    symbol::{
        table::{representation::Insertion, Building, Table},
        Accessibility, GenericDeclaration, Module, TypeDefinition,
    },
    type_system::{
        equality::Equality,
        model::Default,
        normalizer::NoOp,
        predicate::Predicate,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Primitive, SymbolID, TraitMember, Type},
            GenericArguments, Local, Symbol, Term,
        },
        Compute, Environment, OverflowError, Premise,
    },
};

#[test]
fn reflexive() {
    let premise = Premise::<Default>::default();
    let table = Table::<Building>::default();

    let term = Type::Primitive(Primitive::Bool);

    let result = Equality::new(term.clone(), term)
        .query(&Environment {
            premise: &premise,
            table: &table,
            normalizer: &NoOp,
        })
        .unwrap()
        .unwrap();

    assert!(result.constraints.is_empty());
}

#[test]
fn symmetric() {
    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.append_from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(Equality {
            lhs: trait_member.clone(),
            rhs: equivalence.clone(),
        }),
    ));

    let environment = Environment {
        premise: &premise,
        table: &Table::<Building>::default(),
        normalizer: &NoOp,
    };

    assert!(Equality::new(
        Type::TraitMember(trait_member.clone()),
        equivalence.clone(),
    )
    .query(&environment)
    .unwrap()
    .unwrap()
    .constraints
    .is_empty());

    assert!(Equality::new(equivalence, Type::TraitMember(trait_member))
        .query(&environment)
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());
}

#[test]
fn not_equal() {
    let trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.append_from_predicates(std::iter::once(
        Predicate::TraitTypeEquality(Equality {
            lhs: trait_member.clone(),
            rhs: equivalence.clone(),
        }),
    ));

    let environment = Environment {
        premise: &premise,
        table: &Table::<Building>::default(),
        normalizer: &NoOp,
    };

    assert!(Equality::new(
        Type::TraitMember(trait_member.clone()),
        Type::Primitive(Primitive::Float32),
    )
    .query(&environment)
    .unwrap()
    .is_none());

    assert!(Equality::new(
        Type::Primitive(Primitive::Float32),
        Type::TraitMember(trait_member),
    )
    .query(&environment)
    .unwrap()
    .is_none())
}

#[test]
fn transitivity() {
    let first_trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let second_trait_member = TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let equivalence = Type::Primitive(Primitive::Bool);

    let mut premise = Premise::default();
    premise.append_from_predicates(
        [
            Predicate::TraitTypeEquality(Equality {
                lhs: first_trait_member.clone(),
                rhs: Type::TraitMember(second_trait_member.clone()),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: second_trait_member,
                rhs: equivalence.clone(),
            }),
        ]
        .into_iter(),
    );

    let environment = Environment {
        premise: &premise,
        table: &Table::<Building>::default(),
        normalizer: &NoOp,
    };

    assert!(Equality::new(
        Type::TraitMember(first_trait_member.clone()),
        equivalence.clone()
    )
    .query(&environment)
    .unwrap()
    .unwrap()
    .constraints
    .is_empty());

    assert!(Equality::new(equivalence, Type::TraitMember(first_trait_member),)
        .query(&environment)
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());
}

#[test]
fn congruence() {
    let first_trait_member = TraitMember::<Default> {
        id: ID::new(0),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let second_trait_member = TraitMember {
        id: ID::new(1),
        member_generic_arguments: GenericArguments::default(),
        parent_generic_arguments: GenericArguments::default(),
    };
    let first_equivalence = Type::Primitive(Primitive::Bool);
    let second_equivalence = Type::Primitive(Primitive::Int32);

    let mut premise = Premise::default();
    premise.append_from_predicates(
        [
            Predicate::TraitTypeEquality(Equality {
                lhs: first_trait_member.clone(),
                rhs: first_equivalence.clone(),
            }),
            Predicate::TraitTypeEquality(Equality {
                lhs: second_trait_member.clone(),
                rhs: second_equivalence.clone(),
            }),
        ]
        .into_iter(),
    );

    let table = Table::<Building>::default();
    let lhs = Type::Symbol(Symbol {
        id: SymbolID::Struct(ID::new(0)),
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
        id: SymbolID::Struct(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![first_equivalence, second_equivalence],
            constants: Vec::new(),
        },
    });

    let environment =
        Environment { premise: &premise, table: &table, normalizer: &NoOp };

    assert!(Equality::new(lhs.clone(), rhs.clone())
        .query(&environment)
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());
    assert!(Equality::new(rhs.clone(), lhs.clone())
        .query(&environment)
        .unwrap()
        .unwrap()
        .constraints
        .is_empty());
}

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, T), OverflowError>;

    /// Returns the number of nodes in the property.
    fn node_count(&self) -> usize;
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(50, 100, 2, move |inner| {
            let constant_strategy = Box::<dyn Property<Constant::<Default>>>::arbitrary();
            let lifetime_strategy = Box::<dyn Property<Lifetime::<Default>>>::arbitrary();

            prop_oneof![
                4 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(inner.clone()),
                    Some(constant_strategy)
                ))
                .prop_map(|x| Box::new(x) as _),
                1 => LocalCongruence::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                1 => TypeAlias::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
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
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(10, 60, 6, move |inner| {
            prop_oneof![
                1 => LocalCongruence::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
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

impl<T: Term + Debug + 'static> Property<T> for Identity<T> {
    fn generate(
        &self,
        _: &mut Table<Building>,
        _: &mut Premise<Default>,
        _: ID<Module>,
    ) -> Result<(T, T), OverflowError> {
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
    Equality<T::TraitMember, T>: Into<Predicate<Default>>,
{
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, T), OverflowError> {
        let (inner_lhs, inner_rhs) =
            self.property.generate(table, premise, root_module_id)?;

        let should_map = if self.map_at_lhs {
            Equality::new(
                self.target_trait_member.clone().into(),
                inner_rhs.clone(),
            )
            .query(&Environment { premise, table, normalizer: &NoOp })?
            .is_none()
        } else {
            Equality::new(
                inner_lhs.clone(),
                self.target_trait_member.clone().into(),
            )
            .query(&Environment { premise, table, normalizer: &NoOp })?
            .is_none()
        };

        if should_map {
            premise.append_from_predicates(std::iter::once(
                Equality {
                    lhs: self.target_trait_member.clone(),
                    rhs: if self.map_at_lhs {
                        inner_lhs.clone()
                    } else {
                        inner_rhs.clone()
                    },
                }
                .into(),
            ));
        }

        if self.map_at_lhs {
            Ok((self.target_trait_member.clone().into(), inner_rhs))
        } else {
            Ok((inner_lhs, self.target_trait_member.clone().into()))
        }
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

/// A property that generates `local` terms which will be used to test the
/// congruence.
#[derive(Debug)]
pub struct LocalCongruence<T> {
    strategy: Box<dyn Property<T>>,
}

impl<T: 'static + Debug + Term> Arbitrary for LocalCongruence<T>
where
    Local<T>: Into<T>,
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        strategy.prop_map(|strategy| Self { strategy }).boxed()
    }
}

impl<T: Term<Model = Default> + Debug + 'static> Property<T>
    for LocalCongruence<T>
where
    Local<T>: Into<T>,
{
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(T, T), OverflowError> {
        let (lhs, rhs) =
            self.strategy.generate(table, premise, root_module_id)?;

        Ok((Local(Box::new(lhs)).into(), Local(Box::new(rhs)).into()))
    }

    fn node_count(&self) -> usize { 1 + self.strategy.node_count() }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    lifetime_properties: Vec<Box<dyn Property<Lifetime<Default>>>>,
    type_properties: Vec<Box<dyn Property<Type<Default>>>>,
    constant_properties: Vec<Box<dyn Property<Constant<Default>>>>,

    id: ID,
}

impl<ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Copy>
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
            ID::arbitrary(),
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

impl<ID: Debug + 'static + Copy, T: Term<Model = Default> + 'static> Property<T>
    for SymbolCongruence<ID>
where
    Symbol<Default, ID>: Into<T>,
{
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: arena::ID<Module>,
    ) -> Result<(T, T), OverflowError> {
        let mut lhs_generic_arguments = GenericArguments::default();
        let mut rhs_generic_arguments = GenericArguments::default();

        for strategy in &self.lifetime_properties {
            let (lhs, rhs) =
                strategy.generate(table, premise, root_module_id)?;

            lhs_generic_arguments.lifetimes.push(lhs);
            rhs_generic_arguments.lifetimes.push(rhs);
        }

        for strategy in &self.type_properties {
            let (lhs, rhs) =
                strategy.generate(table, premise, root_module_id)?;

            lhs_generic_arguments.types.push(lhs);
            rhs_generic_arguments.types.push(rhs);
        }

        for strategy in &self.constant_properties {
            let (lhs, rhs) =
                strategy.generate(table, premise, root_module_id)?;

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

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type<Default>>>,
    aliased_at_lhs: bool,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy =
            args.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary);

        (strategy, proptest::bool::ANY)
            .prop_map(|(property, aliased_at_lhs)| TypeAlias {
                property,
                aliased_at_lhs,
            })
            .boxed()
    }
}

impl Property<Type<Default>> for TypeAlias {
    fn generate(
        &self,
        table: &mut Table<Building>,
        premise: &mut Premise<Default>,
        root_module_id: ID<Module>,
    ) -> Result<(Type<Default>, Type<Default>), OverflowError> where {
        let (inner_lhs, inner_rhs) =
            self.property.generate(table, premise, root_module_id)?;

        let expected_id = table.types().get_available_id();
        let ty_alias = Type::Symbol(Symbol {
            id: SymbolID::Type(expected_id),
            generic_arguments: GenericArguments::default(),
        });

        let should_add_symbol = if self.aliased_at_lhs {
            Equality::new(ty_alias.clone(), inner_rhs.clone())
                .query(&Environment { premise, table, normalizer: &NoOp })?
                .is_none()
        } else {
            Equality::new(inner_lhs.clone(), ty_alias.clone())
                .query(&Environment { premise, table, normalizer: &NoOp })?
                .is_none()
        };

        if should_add_symbol {
            let module_id = table
                .get_by_qualified_name(std::iter::once("test"))
                .unwrap()
                .into_module()
                .unwrap();

            let Insertion { id, duplication } = table
                .insert_member(
                    format!("T{}", expected_id.into_index()),
                    Accessibility::Public,
                    module_id,
                    None,
                    GenericDeclaration::default(),
                    TypeDefinition {
                        r#type: if self.aliased_at_lhs {
                            inner_lhs.clone()
                        } else {
                            inner_rhs.clone()
                        },
                    },
                )
                .unwrap();

            assert!(duplication.is_none());
            assert_eq!(id, expected_id);
        }

        Ok(if self.aliased_at_lhs {
            (ty_alias, inner_rhs)
        } else {
            (inner_lhs, ty_alias)
        })
    }

    fn node_count(&self) -> usize { 1 + self.property.node_count() }
}

fn remove_equality_recursive(
    predicates: &mut HashSet<Predicate<Default>>,
    trait_member: &TraitMember<Default>,
) {
    let to_be_removeds = predicates
        .iter()
        .filter(|predicate| {
            let Predicate::TraitTypeEquality(equality) = predicate else {
                return false;
            };

            &equality.lhs == trait_member
        })
        .cloned()
        .collect::<Vec<_>>();

    for removed in to_be_removeds {
        predicates.remove(&removed);

        let Some(Type::TraitMember(next)) =
            removed.as_trait_type_equality().map(|x| &x.rhs)
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
    let mut table = Table::<Building>::default();

    let module_id = table.create_root_module("test".to_string());

    let (term1, term2) = property
        .generate(&mut table, &mut premise, module_id.id)
        .map_err(|_| TestCaseError::reject("too complex property"))?;

    let environment =
        &Environment { premise: &premise, table: &table, normalizer: &NoOp };
    prop_assert!(Equality::new(term1.clone(), term2.clone())
        .query(&environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .unwrap()
        .constraints
        .is_empty());
    prop_assert!(Equality::new(term2.clone(), term1.clone())
        .query(&environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .unwrap()
        .constraints
        .is_empty());

    // remove the equality mapping should make the terms not equal.
    'out: {
        let mut predicates_cloned = premise.predicates.clone();
        let Some(to_remove) = predicates_cloned.iter().find_map(|predicate| {
            predicate.as_trait_type_equality().map(|x| x.lhs.clone())
        }) else {
            break 'out;
        };

        remove_equality_recursive(&mut predicates_cloned, &to_remove);

        let mut modified_premise =
            Premise::from_predicates(predicates_cloned.into_iter());
        modified_premise.trait_context = premise.trait_context.clone();

        let modified_environment = &Environment {
            premise: &modified_premise,
            table: &table,
            normalizer: &NoOp,
        };

        prop_assert!(Equality::new(term1.clone(), term2.clone())
            .query(&modified_environment)
            .map_err(|_| TestCaseError::reject("too complex property"))?
            .is_none());
        prop_assert!(Equality::new(term2.clone(), term1.clone())
            .query(&modified_environment)
            .map_err(|_| TestCaseError::reject("too complex property"))?
            .is_none());
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
                normalizer: &NoOp,
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

    premise.append_from_predicates(
        decoy.types.into_iter().map(Predicate::TraitTypeEquality),
    );

    let environment =
        &Environment { premise: &premise, table: &table, normalizer: &NoOp };
    prop_assert!(Equality::new(term1.clone(), term2.clone())
        .query(&environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .unwrap()
        .constraints
        .is_empty());
    prop_assert!(Equality::new(term2.clone(), term1.clone())
        .query(&environment)
        .map_err(|_| TestCaseError::reject("too complex property"))?
        .unwrap()
        .constraints
        .is_empty());

    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    types: Vec<Equality<r#type::TraitMember<Default>, Type<Default>>>,
}

impl Arbitrary for Decoy {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let equality = (r#type::TraitMember::arbitrary(), Type::arbitrary())
            .prop_map(|(lhs, rhs)| Equality::new(lhs, rhs));

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
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant<Default>>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type<Default>>>::arbitrary(),
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
