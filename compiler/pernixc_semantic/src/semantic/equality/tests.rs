use std::{collections::HashMap, fmt::Debug};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::equals;
use crate::{
    arena::ID,
    semantic::{
        instantiation::{self, Instantiation},
        model::Default,
        normalizer::NoOp,
        predicate::{Equality, Predicate},
        session::{self, ExceedLimitError, Limit, Session},
        sub_term::TermLocation,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolID, TraitMember, Type},
            GenericArguments, Local, Symbol, Term,
        },
        tests::State,
        visitor::{self, Recursive},
        Environment, Premise,
    },
    symbol::{
        self, GenericDeclaration, GenericID, GenericParameters, TypeParameter,
        TypeParameterID,
    },
    table::Table,
};

#[test]
fn reflexive() {
    let premise = Premise::<Default>::default();
    let table = Table::<State>::default();

    let term = Type::Primitive(Primitive::Bool);

    assert!(equals(
        &term,
        &term,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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

    let table = Table::<State>::default();

    assert!(equals(
        &Type::TraitMember(trait_member.clone()),
        &equivalence,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &Type::TraitMember(trait_member),
        &equivalence,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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

    let table = Table::<State>::default();

    assert!(equals(
        &Type::TraitMember(first_trait_member.clone()),
        &equivalence,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &equivalence,
        &Type::TraitMember(first_trait_member),
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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

    let table = Table::<State>::default();
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

    assert!(equals(
        &lhs,
        &rhs,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &rhs,
        &lhs,
        &Environment { premise: &premise, table: &table, normalizer: &NoOp },
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
    /// Applies this property to the environment.
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError>;

    /// Generates the term for testing.
    #[must_use]
    fn generate(&self) -> (T, T);
}

impl Arbitrary for Box<dyn Property<Type<Default>>> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(10, 60, 6, move |inner| {
            let constant_strategy = Box::<dyn Property<Constant::<Default>>>::arbitrary();
            let lifetime_strategy = Box::<dyn Property<Lifetime::<Default>>>::arbitrary();

            prop_oneof![
                2 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strategy),
                    Some(inner.clone()),
                    Some(constant_strategy)
                ))
                .prop_map(|x| Box::new(x) as _),
                1 => LocalCongruence::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                1 => TypeAlias::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
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
    fn apply(
        &self,
        _: &mut Table<State>,
        _: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        Ok(())
    }

    fn generate(&self) -> (T, T) { (self.term.clone(), self.term.clone()) }
}

#[derive(Debug)]
pub struct Mapping<T: Term> {
    pub property: Box<dyn Property<T>>,
    pub target_trait_member: T::TraitMember,
    pub target_at_lhs: bool,
}

impl<T: 'static + Debug + Term + Arbitrary<Strategy = BoxedStrategy<T>>>
    Arbitrary for Mapping<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    T::TraitMember: Arbitrary<Strategy = BoxedStrategy<T::TraitMember>>,
    session::Default<Default>: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strategy, T::TraitMember::arbitrary(), proptest::bool::ANY)
            .prop_map(|(property, target_trait_member, target_at_lhs)| Self {
                property,
                target_trait_member,
                target_at_lhs,
            })
            .boxed()
    }
}

impl<T: Term<Model = Default> + Debug + 'static> Property<T> for Mapping<T>
where
    session::Default<Default>: Session<T>,
    Equality<T::TraitMember, T>: Into<Predicate<Default>>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if equals(
            &lhs,
            &rhs,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        let (property_lhs, property_rhs) = self.property.generate();

        let to_be_mapped =
            if self.target_at_lhs { property_lhs } else { property_rhs };
        self.property.apply(table, premise)?;

        if equals(
            &lhs,
            &rhs,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? || equals(
            &to_be_mapped,
            &T::from(self.target_trait_member.clone()),
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        premise.append_from_predicates(std::iter::once(
            Equality {
                lhs: self.target_trait_member.clone(),
                rhs: to_be_mapped,
            }
            .into(),
        ));

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        if self.target_at_lhs {
            (
                T::from(self.target_trait_member.clone()),
                self.property.generate().1,
            )
        } else {
            (
                self.property.generate().0,
                T::from(self.target_trait_member.clone()),
            )
        }
    }
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
    session::Default<Default>: Session<T>,
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
    session::Default<Default>: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();
        if equals(
            &lhs,
            &rhs,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.strategy.apply(table, premise)
    }

    fn generate(&self) -> (T, T) {
        let (lhs, rhs) = self.strategy.generate();

        (Local(Box::new(lhs)).into(), Local(Box::new(rhs)).into())
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID> {
    lifetime_properties: Vec<Box<dyn Property<Lifetime<Default>>>>,
    type_properties: Vec<Box<dyn Property<Type<Default>>>>,
    constant_properties: Vec<Box<dyn Property<Constant<Default>>>>,

    id: ID,
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

impl<ID: Debug + 'static + Clone, T: Term<Model = Default> + 'static>
    Property<T> for SymbolCongruence<ID>
where
    Symbol<Default, ID>: Into<T>,
    session::Default<Default>: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        for strategy in &self.type_properties {
            strategy.apply(table, premise)?;
        }

        for strategy in &self.lifetime_properties {
            strategy.apply(table, premise)?;
        }

        for strategy in &self.constant_properties {
            strategy.apply(table, premise)?;
        }

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        let mut lhs_generic_arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
        };
        let mut rhs_generic_arguments = GenericArguments {
            lifetimes: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
        };

        for strategy in &self.lifetime_properties {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.lifetimes.push(lhs);
            rhs_generic_arguments.lifetimes.push(rhs);
        }

        for strategy in &self.type_properties {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.types.push(lhs);
            rhs_generic_arguments.types.push(rhs);
        }

        for strategy in &self.constant_properties {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.constants.push(lhs);
            rhs_generic_arguments.constants.push(rhs);
        }

        (
            Symbol {
                id: self.id.clone(),
                generic_arguments: lhs_generic_arguments,
            }
            .into(),
            Symbol {
                id: self.id.clone(),
                generic_arguments: rhs_generic_arguments,
            }
            .into(),
        )
    }
}

struct TermCollector {
    terms: Vec<Type<Default>>,
}

impl<'v> Recursive<'v, Lifetime<Default>> for TermCollector {
    fn visit(
        &mut self,
        _: &'v Lifetime<Default>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'v> Recursive<'v, Type<Default>> for TermCollector {
    fn visit(
        &mut self,
        term: &'v Type<Default>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        self.terms.push(term.clone());
        true
    }
}

impl<'v> Recursive<'v, Constant<Default>> for TermCollector {
    fn visit(
        &mut self,
        _: &'v Constant<Default>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type<Default>>>,
    type_id: ID<symbol::Type>,
    argument: Type<Default>,
    aliased_at_lhs: bool,
    at_lhs: bool,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type<Default>>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy =
            args.unwrap_or_else(Box::<dyn Property<Type<_>>>::arbitrary);

        (
            (proptest::bool::ANY, strategy).prop_ind_flat_map2(
                |(aliased_at_lhs, prop)| {
                    let (lhs, rhs) = prop.generate();
                    let sampled = if aliased_at_lhs { lhs } else { rhs };

                    let mut term_collector =
                        TermCollector { terms: Vec::new() };

                    visitor::accept_recursive(&sampled, &mut term_collector);

                    proptest::sample::select(term_collector.terms)
                },
            ),
            ID::arbitrary(),
            proptest::bool::ANY,
        )
            .prop_map(
                |(((aliased_at_lhs, property), argument), type_id, at_lhs)| {
                    Self { property, type_id, argument, aliased_at_lhs, at_lhs }
                },
            )
            .boxed()
    }
}

impl Property<Type<Default>> for TypeAlias {
    fn apply(
        &self,
        table: &mut Table<State>,
        premise: &mut Premise<Default>,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if equals(
            &lhs,
            &rhs,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.property.apply(table, premise)?;

        if !equals(
            &lhs,
            &rhs,
            &Environment { premise, table, normalizer: &NoOp },
            &mut Limit::new(&mut session::Default::default()),
        )? {
            let type_symbol = symbol::Type {
                generic_declaration: GenericDeclaration {
                    parameters: {
                        let mut parameters = GenericParameters::default();

                        let _ = parameters.add_type_parameter(TypeParameter {
                            name: Some("T".to_string()),
                            span: None,
                        });

                        parameters
                    },
                    predicates: Vec::new(),
                },
                parent_id: ID::new(0),
                span: None,
                name: "Test".to_string(),
                definition: symbol::TypeDefinition {
                    accessibility: symbol::Accessibility::Public,
                    r#type: {
                        let (lhs, rhs) = self.property.generate();

                        let mut sampled =
                            if self.aliased_at_lhs { lhs } else { rhs };

                        let instantiation = Instantiation {
                            lifetimes: HashMap::new(),
                            types: std::iter::once((
                                self.argument.clone(),
                                Type::Parameter(TypeParameterID {
                                    parent: GenericID::Type(self.type_id),
                                    id: ID::new(0),
                                }),
                            ))
                            .collect(),
                            constants: HashMap::new(),
                        };

                        instantiation::instantiate(
                            &mut sampled,
                            &instantiation,
                        );

                        sampled
                    },
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
        }

        Ok(())
    }

    fn generate(&self) -> (Type<Default>, Type<Default>) {
        let (lhs, rhs) = self.property.generate();
        let non_aliased = if self.aliased_at_lhs { rhs } else { lhs };
        let aliased = Type::Symbol(Symbol {
            id: SymbolID::Type(self.type_id),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![self.argument.clone()],
                constants: Vec::new(),
            },
        });

        if self.at_lhs {
            (aliased, non_aliased)
        } else {
            (non_aliased, aliased)
        }
    }
}

#[allow(clippy::too_many_lines)]
fn property_based_testing<T: Term<Model = Default> + 'static>(
    property: &dyn Property<T>,
    decoy: Decoy,
) -> TestCaseResult
where
    session::Default<Default>: Session<T>,
{
    let (term1, term2) = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<State>::default();

    property.apply(&mut table, &mut premise).map_err(|x| match x {
        ApplyPropertyError::ExceedLimitError(_) => {
            TestCaseError::reject("too complex property")
        }
        ApplyPropertyError::TypeAliasIDCollision => {
            TestCaseError::reject("type alias id collision")
        }
    })?;

    let environment =
        &Environment { premise: &premise, table: &table, normalizer: &NoOp };
    prop_assert!(equals(
        &term1,
        &term2,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    prop_assert!(equals(
        &term2,
        &term1,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    // println!(
    //     "mapping count: {}",
    //     premise.equalities_mapping.mapping_count()
    // );

    // remove the equality mapping should make the terms not equal.
    {
        let mut premise_removed = premise.clone();

        if premise_removed.equivalent.remove_class::<Lifetime<_>>(0).is_some()
            || premise_removed.equivalent.remove_class::<Type<_>>(0).is_some()
            || premise_removed
                .equivalent
                .remove_class::<Constant<_>>(0)
                .is_some()
        {
            let environment = &Environment {
                premise: &premise_removed,
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
        }
    }

    // remove the type alias symbol should make the terms not equal.
    {
        let id = table.representation.types.ids().next();
        if let Some(id) = id {
            let removed = table.representation.types.remove(id).unwrap();
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

            table.representation.types.insert_with_id(id, removed).unwrap();
        }
    }

    // adding unrelated equalities should not affect the result.
    for decoy_lifetime_equalities in decoy.lifetimes {
        premise.equivalent.insert(
            decoy_lifetime_equalities.lhs,
            decoy_lifetime_equalities.rhs,
        );
    }

    for decoy_type_equalities in decoy.types {
        premise
            .equivalent
            .insert(decoy_type_equalities.lhs, decoy_type_equalities.rhs);
    }

    for decoy_constant_equalities in decoy.constants {
        premise.equivalent.insert(
            decoy_constant_equalities.lhs,
            decoy_constant_equalities.rhs,
        );
    }

    let environment =
        &Environment { premise: &premise, table: &table, normalizer: &NoOp };
    prop_assert!(equals(
        &term1,
        &term2,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);
    prop_assert!(equals(
        &term2,
        &term1,
        environment,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    lifetimes: Vec<DecoyEquality<Lifetime<Default>>>,
    types: Vec<DecoyEquality<Type<Default>>>,
    constants: Vec<DecoyEquality<Constant<Default>>>,
}

impl Arbitrary for Decoy {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            proptest::collection::vec(DecoyEquality::arbitrary(), 0..=2),
            proptest::collection::vec(DecoyEquality::arbitrary(), 0..=2),
            proptest::collection::vec(DecoyEquality::arbitrary(), 0..=2),
        )
            .prop_map(|(lifetimes, types, constants)| Self {
                lifetimes,
                types,
                constants,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct DecoyEquality<T> {
    lhs: T,
    rhs: T,
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary
    for DecoyEquality<T>
{
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (T::arbitrary(), T::arbitrary())
            .prop_map(|(lhs, rhs)| Self { lhs, rhs })
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
