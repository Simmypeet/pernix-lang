use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    marker::PhantomData,
};

use proptest::{
    arbitrary::Arbitrary,
    prop_assert, prop_oneof, proptest,
    strategy::{BoxedStrategy, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::equals;
use crate::{
    arena::{Arena, ID},
    semantic::{
        self,
        mapping::{self, Map},
        session::{self, Session},
        substitution::{Substitute, Substitution},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolKindID, Type},
            GenericArguments, Local, Symbol, Term,
        },
        visitor::{self, VisitMode, Visitor},
        Premise, Semantic,
    },
    symbol::{
        self, GenericDeclaration, GenericID, GenericParameters, TypeParameter, TypeParameterID,
    },
    table::{Success, Table},
};

#[test]
fn reflexive() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::default(),
    };
    let table = Table::<Success>::default();

    let term = Type::Primitive(Primitive::Bool);

    assert!(equals(
        &term,
        &term,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

#[test]
fn symmetric() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            std::iter::once((
                Type::Primitive(Primitive::Bool),
                Type::Primitive(Primitive::Float32),
            )),
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Primitive(Primitive::Bool);
    let rhs = Type::Primitive(Primitive::Float32);

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}
#[test]
fn transitivity() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            [
                (
                    Type::Primitive(Primitive::Bool),
                    Type::Primitive(Primitive::Float32),
                ),
                (
                    Type::Primitive(Primitive::Float32),
                    Type::Primitive(Primitive::Float64),
                ),
            ],
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Primitive(Primitive::Bool);
    let rhs = Type::Primitive(Primitive::Float64);

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

#[test]
fn congruence() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            [
                (
                    Type::Primitive(Primitive::Int32),
                    Type::Primitive(Primitive::Float32),
                ),
                (
                    Type::Primitive(Primitive::Int64),
                    Type::Primitive(Primitive::Float64),
                ),
            ],
            std::iter::empty(),
        ),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Symbol(Symbol {
        id: SymbolKindID::Struct(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Int32),
                Type::Primitive(Primitive::Int64),
            ],
            constants: Vec::new(),
        },
    });
    let rhs = Type::Symbol(Symbol {
        id: SymbolKindID::Struct(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![
                Type::Primitive(Primitive::Float32),
                Type::Primitive(Primitive::Float64),
            ],
            constants: Vec::new(),
        },
    });

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default(),
    ));
}

/// A trait for generating term for checking equality.
pub trait Property<T>: 'static + Debug {
    /// Determines whether this property requires [`Self::apply`] to be called in order to make the
    /// terms equal.
    fn requires_environment(&self) -> bool;

    /// Applies this property to the environment.
    ///
    /// # Returns
    ///
    /// Returns `false` if failed to apply the environment to the
    #[must_use]
    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool;

    /// Generates the term for testing.
    fn generate(&self) -> (T, T);
}

impl Arbitrary for Box<dyn Property<Type>> {
    type Parameters = (
        Option<BoxedStrategy<Box<dyn Property<Lifetime>>>>,
        Option<BoxedStrategy<Box<dyn Property<Constant>>>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(16, 96, 6, move |inner| {
            let constant_start = strat.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Constant>>::arbitrary_with((
                    strat.0.clone(),
                    Some(inner.clone()),
                ))
            });
            let lifetime_strat = strat.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((
                    Some(inner.clone()),
                    strat.1.clone(),
                ))
            });

            prop_oneof![
                2 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strat),
                    Some(inner.clone()),
                    Some(constant_start)
                ))
                .prop_map(|x| Box::new(x) as _),
                1 => LocalCongruence::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                1 => TypeAlias::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
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
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(4, 8, 2, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _)]
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

    fn arbitrary_with(strat: Self::Parameters) -> Self::Strategy {
        let leaf = Identity::arbitrary().prop_map(|x| Box::new(x) as _);

        leaf.prop_recursive(16, 96, 6, move |inner| {
            let type_strat = strat.1.clone().unwrap_or_else(|| {
                Box::<dyn Property<Type>>::arbitrary_with((strat.0.clone(), Some(inner.clone())))
            });
            let lifetime_strat = strat.0.clone().unwrap_or_else(|| {
                Box::<dyn Property<Lifetime>>::arbitrary_with((
                    strat.1.clone(),
                    Some(inner.clone()),
                ))
            });

            prop_oneof![
                2 => Mapping::arbitrary_with(Some(inner.clone())).prop_map(|x| Box::new(x) as _),
                6 => SymbolCongruence::arbitrary_with((
                    Some(lifetime_strat),
                    Some(type_strat),
                    Some(inner.clone())
                ))
                .prop_map(|x| Box::new(x) as _),
                1 => LocalCongruence::arbitrary_with(Some(inner)).prop_map(|x| Box::new(x) as _),
            ]
        })
        .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identity<T> {
    term: T,
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
    fn requires_environment(&self) -> bool { false }

    fn apply(&self, _: &mut Table<Success>, _: &mut Premise) -> bool { true }

    fn generate(&self) -> (T, T) { (self.term.clone(), self.term.clone()) }
}

#[derive(Debug)]
pub struct Mapping<T> {
    lhs: Box<dyn Property<T>>,
    rhs: Box<dyn Property<T>>,
}

impl<T: 'static + Debug + Term> Arbitrary for Mapping<T>
where
    Box<dyn Property<T>>: Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strat.clone(), strat)
            .prop_map(|(lhs, rhs)| Self { lhs, rhs })
            .boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for Mapping<T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    fn requires_environment(&self) -> bool { true }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        let (l_lhs, l_rhs) = self.lhs.generate();
        let (r_lhs, r_rhs) = self.rhs.generate();

        if !equals(
            &l_lhs,
            &r_rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) && (!self.lhs.apply(table, premise) || !self.rhs.apply(table, premise))
        {
            return false;
        }

        if !equals(
            &l_lhs,
            &r_rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) {
            premise.equalities_mapping.insert(l_rhs, r_lhs);
        }

        true
    }

    fn generate(&self) -> (T, T) {
        let (l_lhs, _) = self.lhs.generate();
        let (_, r_rhs) = self.rhs.generate();
        (l_lhs, r_rhs)
    }
}

/// A property that generates `local` terms which will be used to test the congruence.
#[derive(Debug)]
pub struct LocalCongruence<T> {
    strategy: Box<dyn Property<T>>,
}

impl<T: 'static + Debug + Term> Arbitrary for LocalCongruence<T>
where
    Local<T>: Into<T>,
    Box<dyn Property<T>>: Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        strat.prop_map(|strategy| Self { strategy }).boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for LocalCongruence<T>
where
    Local<T>: Into<T>,
{
    fn requires_environment(&self) -> bool { self.strategy.requires_environment() }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        self.strategy.apply(table, premise)
    }

    fn generate(&self) -> (T, T) {
        let (lhs, rhs) = self.strategy.generate();

        (Local(Box::new(lhs)).into(), Local(Box::new(rhs)).into())
    }
}

#[derive(Debug)]
pub struct SymbolCongruence<ID, T> {
    lifetime_strategies: Vec<Box<dyn Property<Lifetime>>>,
    type_strategies: Vec<Box<dyn Property<Type>>>,
    constant_strategies: Vec<Box<dyn Property<Constant>>>,

    id: ID,

    term: PhantomData<T>,
}

impl<
        ID: 'static + Arbitrary<Strategy = BoxedStrategy<ID>> + Debug + Clone,
        T: Term + From<Symbol<ID>> + 'static,
    > Arbitrary for SymbolCongruence<ID, T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
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
                0..=2,
            ),
            proptest::collection::vec(
                args.0
                    .unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.2
                    .unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=2,
            ),
            ID::arbitrary(),
        )
            .prop_map(|(tys, lts, consts, id)| Self {
                lifetime_strategies: lts,
                type_strategies: tys,
                constant_strategies: consts,
                id,
                term: PhantomData,
            })
            .boxed()
    }
}

impl<ID: Debug + 'static + Clone, T: Term + 'static> Property<T> for SymbolCongruence<ID, T>
where
    Symbol<ID>: Into<T>,
{
    fn requires_environment(&self) -> bool {
        self.lifetime_strategies
            .iter()
            .any(|x| x.requires_environment())
            || self
                .type_strategies
                .iter()
                .any(|x| x.requires_environment())
            || self
                .constant_strategies
                .iter()
                .any(|x| x.requires_environment())
    }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        for strategy in &self.type_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        for strategy in &self.lifetime_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        for strategy in &self.constant_strategies {
            if !strategy.apply(table, premise) {
                return false;
            }
        }

        true
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

        for strategy in &self.lifetime_strategies {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.lifetimes.push(lhs);
            rhs_generic_arguments.lifetimes.push(rhs);
        }

        for strategy in &self.type_strategies {
            let (lhs, rhs) = strategy.generate();

            lhs_generic_arguments.types.push(lhs);
            rhs_generic_arguments.types.push(rhs);
        }

        for strategy in &self.constant_strategies {
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
    terms: Vec<Type>,
}

impl Visitor for TermCollector {
    fn visit_type(&mut self, ty: &Type, _: semantic::visitor::Source) -> bool {
        self.terms.push(ty.clone());
        true
    }

    fn visit_lifetime(&mut self, _: &Lifetime, _: semantic::visitor::Source) -> bool { true }

    fn visit_constant(&mut self, _: &Constant, _: semantic::visitor::Source) -> bool { true }
}

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type>>,
    type_id: ID<symbol::Type>,
    argument: Type,
    aliased_at_lhs: bool,
    type_alias_at_lhs: bool,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (
            (proptest::bool::ANY, strat).prop_ind_flat_map2(|(aliased_at_lhs, prop)| {
                let (lhs, rhs) = prop.generate();
                let sampled = if aliased_at_lhs { lhs } else { rhs };

                let mut term_collector = TermCollector { terms: Vec::new() };

                visitor::accept_recursive(
                    &sampled,
                    &mut term_collector,
                    VisitMode::<Success>::OnlySubTerms,
                );

                proptest::sample::select(term_collector.terms)
            }),
            ID::arbitrary(),
            proptest::bool::ANY,
        )
            .prop_map(
                |(((aliased_at_lhs, ty_strat), argument), type_id, type_alias_at_lhs)| Self {
                    property: ty_strat,
                    type_id,
                    argument,
                    aliased_at_lhs,
                    type_alias_at_lhs,
                },
            )
            .boxed()
    }
}

impl Property<Type> for TypeAlias {
    fn requires_environment(&self) -> bool { true }

    fn apply(&self, table: &mut Table<Success>, premise: &mut Premise) -> bool {
        let (lhs, rhs) = self.generate();

        if !equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) && !self.property.apply(table, premise)
        {
            return false;
        }

        if !equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut session::Default::default(),
        ) {
            let type_symbol = symbol::Type {
                id: self.type_id,
                generic_declaration: GenericDeclaration {
                    parameters: GenericParameters {
                        lifetimes: Arena::new(),
                        types: {
                            let mut arena = Arena::new();

                            arena
                                .insert_with_id(ID::new(0), TypeParameter {
                                    name: "T".to_string(),
                                    parent_generic_id: GenericID::Type(self.type_id),
                                    span: None,
                                })
                                .unwrap();

                            arena
                        },
                        constants: Arena::new(),
                        lifetime_parameter_ids_by_name: HashMap::new(),
                        type_parameter_ids_by_name: {
                            let mut map = HashMap::new();
                            map.insert("T".to_string(), ID::new(0));
                            map
                        },
                        constant_parameter_ids_by_name: HashMap::new(),
                        default_type_parameters: Vec::new(),
                        default_constant_parameters: Vec::new(),
                        lifetime_order: Vec::new(),
                        type_order: vec![ID::new(0)],
                        constant_order: Vec::new(),
                    },
                    predicates: Vec::new(),
                },
                parent_id: ID::new(0),
                span: None,
                name: "Test".to_string(),
                data: symbol::TypeData {
                    accessibility: symbol::Accessibility::Public,
                    r#type: {
                        let (lhs, rhs) = self.property.generate();

                        let mut sampled = if self.aliased_at_lhs { lhs } else { rhs };

                        sampled.apply(&Substitution {
                            types: {
                                let mut map = BTreeMap::new();

                                map.insert(
                                    self.argument.clone(),
                                    Type::Parameter(TypeParameterID {
                                        parent: GenericID::Type(self.type_id),
                                        id: ID::new(0),
                                    }),
                                );

                                map
                            },
                            constants: BTreeMap::new(),
                            lifetimes: BTreeMap::new(),
                        });

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
                return false;
            }
        }

        true
    }

    fn generate(&self) -> (Type, Type) {
        let (lhs, rhs) = self.property.generate();
        let non_aliased = if self.aliased_at_lhs { rhs } else { lhs };
        let aliased = Type::Symbol(Symbol {
            id: SymbolKindID::Type(self.type_id),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![self.argument.clone()],
                constants: Vec::new(),
            },
        });

        if self.type_alias_at_lhs {
            (aliased, non_aliased)
        } else {
            (non_aliased, aliased)
        }
    }
}

fn remove<T: Term>(equalities: &mut mapping::Mapping, term: &T) {
    let removed_terms = equalities.remove_equality(term);

    for removed_term in removed_terms.into_iter().flatten() {
        remove(equalities, &removed_term);
    }
}

fn remove_sampled<T: Term>(equalities: &mut mapping::Mapping) -> bool {
    #[allow(clippy::option_if_let_else)]
    if let Some(sampled) = <T as Map>::get(equalities).keys().next() {
        remove(equalities, &sampled.clone());
        true
    } else {
        false
    }
}

#[allow(clippy::too_many_lines)]
fn property_based_checking<T: Term + 'static>(
    property: &dyn Property<T>,
    decoy: Decoy,
) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let (term1, term2) = property.generate();
    let mut premise = Premise::default();
    let mut table = Table::<Success>::default();

    if property.requires_environment() && !property.apply(&mut table, &mut premise) {
        // failed to apply the environment
        return Err(TestCaseError::reject("failed to apply the environment"));
    }

    println!(
        "mapping count: {}",
        premise.equalities_mapping.mapping_count()
    );

    prop_assert!(equals(
        &term1,
        &term2,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));
    prop_assert!(equals(
        &term2,
        &term1,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    {
        let mut premise = premise.clone();

        if remove_sampled::<Type>(&mut premise.equalities_mapping)
            || remove_sampled::<Lifetime>(&mut premise.equalities_mapping)
            || remove_sampled::<Constant>(&mut premise.equalities_mapping)
        {
            prop_assert!(!equals(
                &term1,
                &term2,
                &premise,
                &table,
                &mut semantic::Default,
                &mut session::Default::default()
            ));
            prop_assert!(!equals(
                &term2,
                &term1,
                &premise,
                &table,
                &mut semantic::Default,
                &mut session::Default::default()
            ));
        }
    }

    // adding unrelated equalities should not affect the result.
    for decoy_lifetime_equalities in decoy.decoy_lifetime_equalities {
        premise
            .equalities_mapping
            .insert(decoy_lifetime_equalities.lhs, decoy_lifetime_equalities.rhs);
    }

    for decoy_type_equalities in decoy.decoy_type_equalities {
        premise
            .equalities_mapping
            .insert(decoy_type_equalities.lhs, decoy_type_equalities.rhs);
    }

    for decoy_constant_equalities in decoy.decoy_constant_equalities {
        premise
            .equalities_mapping
            .insert(decoy_constant_equalities.lhs, decoy_constant_equalities.rhs);
    }

    prop_assert!(equals(
        &term1,
        &term2,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));
    prop_assert!(equals(
        &term2,
        &term1,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    Ok(())
}

fn inequality_checking<T: Term>(lhs: &T, rhs: &T) -> TestCaseResult
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    let premise = Premise::default();
    let table = Table::<Success>::default();

    if semantic::Default.trivially_equals(lhs, rhs) {
        return Err(TestCaseError::reject("trivial equality"));
    }

    prop_assert!(!equals(
        lhs,
        rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut session::Default::default()
    ));

    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    decoy_lifetime_equalities: Vec<DecoyEquality<Lifetime>>,
    decoy_type_equalities: Vec<DecoyEquality<Type>>,
    decoy_constant_equalities: Vec<DecoyEquality<Constant>>,
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
            .prop_map(|(lts, tys, consts)| Self {
                decoy_lifetime_equalities: lts,
                decoy_type_equalities: tys,
                decoy_constant_equalities: consts,
            })
            .boxed()
    }
}

#[derive(Debug)]
pub struct DecoyEquality<T> {
    lhs: T,
    rhs: T,
}

impl<T: Arbitrary<Strategy = BoxedStrategy<T>> + 'static> Arbitrary for DecoyEquality<T> {
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
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_checking(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_checking(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_checking(&*property, decoy)?;
    }

    #[test]
    fn inequality_testing_constant(
        lhs in Constant::arbitrary(),
        rhs in Constant::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }

    #[test]
    fn inequality_testing_type(
        lhs in Type::arbitrary(),
        rhs in Type::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }

    #[test]
    fn inequality_testing_lifetime(
        lhs in Lifetime::arbitrary(),
        rhs in Lifetime::arbitrary(),
    ) {
        inequality_checking(&lhs, &rhs)?;
    }
}
