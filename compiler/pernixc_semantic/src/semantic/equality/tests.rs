use std::{collections::HashMap, fmt::Debug};

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
        session::{self, ExceedLimitError, Limit, Session},
        substitution::{Substitute, Substitution},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolKindID, Type},
            GenericArguments, Local, Symbol, Term,
        },
        visitor::{self, Visitor},
        Premise, Semantic,
    },
    symbol::{
        self, GenericDeclaration, GenericID, GenericParameters, TypeParameter,
        TypeParameterID,
    },
    table::{Success, Table},
};

#[test]
fn reflexive() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::default(),
        non_equality_predicates: Vec::new(),
    };
    let table = Table::<Success>::default();

    let term = Type::Primitive(Primitive::Bool);

    assert!(equals(
        &term,
        &term,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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
        non_equality_predicates: Vec::new(),
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
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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
        non_equality_predicates: Vec::new(),
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
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
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
        non_equality_predicates: Vec::new(),
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
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
}

#[test]
#[allow(missing_docs, clippy::too_many_lines)]
fn recursive() {
    let premise = Premise {
        equalities_mapping: mapping::Mapping::from_pairs(
            std::iter::empty(),
            [(
                Type::Primitive(Primitive::Int32),
                Type::Symbol(Symbol {
                    id: SymbolKindID::Enum(ID::new(0)),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Primitive(Primitive::Int32)],
                        constants: Vec::new(),
                    },
                }),
            )],
            std::iter::empty(),
        ),
        non_equality_predicates: Vec::new(),
    };
    let table = Table::<Success>::default();
    let lhs = Type::Primitive(Primitive::Int32);
    let rhs = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: Vec::new(),
                    types: vec![Type::Symbol(Symbol {
                        id: SymbolKindID::Enum(ID::new(0)),
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![Type::Symbol(Symbol {
                                id: SymbolKindID::Enum(ID::new(0)),
                                generic_arguments: GenericArguments {
                                    lifetimes: Vec::new(),
                                    types: vec![Type::Primitive(
                                        Primitive::Int32,
                                    )],
                                    constants: Vec::new(),
                                },
                            })],
                            constants: Vec::new(),
                        },
                    })],
                    constants: Vec::new(),
                },
            })],
            constants: Vec::new(),
        },
    });

    assert!(equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());

    let rhs = Type::Symbol(Symbol {
        id: SymbolKindID::Enum(ID::new(0)),
        generic_arguments: GenericArguments {
            lifetimes: Vec::new(),
            types: vec![Type::Symbol(Symbol {
                id: SymbolKindID::Enum(ID::new(0)),
                generic_arguments: GenericArguments {
                    lifetimes: Vec::new(),
                    types: vec![Type::Symbol(Symbol {
                        id: SymbolKindID::Enum(ID::new(0)),
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![Type::Symbol(Symbol {
                                id: SymbolKindID::Enum(ID::new(0)),
                                generic_arguments: GenericArguments {
                                    lifetimes: Vec::new(),
                                    types: vec![Type::Primitive(
                                        Primitive::Uint32,
                                    )],
                                    constants: Vec::new(),
                                },
                            })],
                            constants: Vec::new(),
                        },
                    })],
                    constants: Vec::new(),
                },
            })],
            constants: Vec::new(),
        },
    });

    assert!(!equals(
        &lhs,
        &rhs,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default()),
    )
    .unwrap());
    assert!(!equals(
        &rhs,
        &lhs,
        &premise,
        &table,
        &mut semantic::Default,
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
        table: &mut Table<Success>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError>;

    /// Generates the term for testing.
    #[must_use]
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

        leaf.prop_recursive(10, 60, 6, move |inner| {
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

        leaf.prop_recursive(10, 20, 2, |inner| {
            prop_oneof![Mapping::arbitrary_with(Some(inner))
                .prop_map(|x| Box::new(x) as _)]
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

        leaf.prop_recursive(10, 60, 6, move |inner| {
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
    fn apply(
        &self,
        _: &mut Table<Success>,
        _: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        Ok(())
    }

    fn generate(&self) -> (T, T) { (self.term.clone(), self.term.clone()) }
}

#[derive(Debug)]
pub struct Mapping<T> {
    property: Box<dyn Property<T>>,
    target: T,
    target_at_lhs: bool,
}

impl<T: 'static + Debug + Term + Arbitrary<Strategy = BoxedStrategy<T>>>
    Arbitrary for Mapping<T>
where
    Box<dyn Property<T>>:
        Arbitrary<Strategy = BoxedStrategy<Box<dyn Property<T>>>>,
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    type Parameters = Option<BoxedStrategy<Box<dyn Property<T>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<T>>::arbitrary);

        (strat, T::arbitrary(), proptest::bool::ANY)
            .prop_map(|(property, target, target_at_lhs)| Self {
                property,
                target,
                target_at_lhs,
            })
            .boxed()
    }
}

impl<T: Term + Debug + 'static> Property<T> for Mapping<T>
where
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
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
            premise,
            table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )? || equals(
            &to_be_mapped,
            &self.target,
            premise,
            table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        premise.equalities_mapping.insert(to_be_mapped, self.target.clone());

        Ok(())
    }

    fn generate(&self) -> (T, T) {
        if self.target_at_lhs {
            (self.target.clone(), self.property.generate().1)
        } else {
            (self.property.generate().0, self.target.clone())
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
    semantic::Default: Semantic<T>,
    session::Default: Session<T>,
{
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();
        if equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
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
    lifetime_properties: Vec<Box<dyn Property<Lifetime>>>,
    type_properties: Vec<Box<dyn Property<Type>>>,
    constant_properties: Vec<Box<dyn Property<Constant>>>,

    id: ID,
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
                0..=2,
            ),
            proptest::collection::vec(
                args.0.unwrap_or_else(Box::<dyn Property<Lifetime>>::arbitrary),
                0..=2,
            ),
            proptest::collection::vec(
                args.2.unwrap_or_else(Box::<dyn Property<Constant>>::arbitrary),
                0..=2,
            ),
            ID::arbitrary(),
        )
            .prop_map(|(tys, lts, consts, id)| Self {
                lifetime_properties: lts,
                type_properties: tys,
                constant_properties: consts,
                id,
            })
            .boxed()
    }
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
    terms: Vec<Type>,
}

impl Visitor for TermCollector {
    fn visit_type(&mut self, ty: &Type) -> bool {
        self.terms.push(ty.clone());
        true
    }

    fn visit_lifetime(&mut self, _: &Lifetime) -> bool { true }

    fn visit_constant(&mut self, _: &Constant) -> bool { true }
}

#[derive(Debug)]
pub struct TypeAlias {
    property: Box<dyn Property<Type>>,
    type_id: ID<symbol::Type>,
    argument: Type,
    aliased_at_lhs: bool,
    at_lhs: bool,
}

impl Arbitrary for TypeAlias {
    type Parameters = Option<BoxedStrategy<Box<dyn Property<Type>>>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strat = args.unwrap_or_else(Box::<dyn Property<Type>>::arbitrary);

        (
            (proptest::bool::ANY, strat).prop_ind_flat_map2(
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
                |(
                    ((aliased_at_lhs, ty_strat), argument),
                    type_id,
                    type_alias_at_lhs,
                )| {
                    Self {
                        property: ty_strat,
                        type_id,
                        argument,
                        aliased_at_lhs,
                        at_lhs: type_alias_at_lhs,
                    }
                },
            )
            .boxed()
    }
}

impl Property<Type> for TypeAlias {
    fn apply(
        &self,
        table: &mut Table<Success>,
        premise: &mut Premise,
    ) -> Result<(), ApplyPropertyError> {
        let (lhs, rhs) = self.generate();

        if equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )? {
            return Ok(());
        }

        self.property.apply(table, premise)?;

        if !equals(
            &lhs,
            &rhs,
            premise,
            table,
            &mut semantic::Default,
            &mut Limit::new(&mut session::Default::default()),
        )? {
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
                                    parent_generic_id: GenericID::Type(
                                        self.type_id,
                                    ),
                                    span: None,
                                    variance: symbol::Variance::Invariant,
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

                        let mut sampled =
                            if self.aliased_at_lhs { lhs } else { rhs };

                        sampled.apply(&Substitution {
                            types: {
                                let mut map = HashMap::new();

                                map.insert(
                                    self.argument.clone(),
                                    Type::Parameter(TypeParameterID {
                                        parent: GenericID::Type(self.type_id),
                                        id: ID::new(0),
                                    }),
                                );

                                map
                            },
                            constants: HashMap::new(),
                            lifetimes: HashMap::new(),
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
                return Err(ApplyPropertyError::TypeAliasIDCollision);
            }
        }

        Ok(())
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

        if self.at_lhs {
            (aliased, non_aliased)
        } else {
            (non_aliased, aliased)
        }
    }
}

fn remove<T: Term>(equalities: &mut mapping::Mapping, term: &T) {
    let removeds = equalities.remove_equality(term);

    for removed in removeds.into_iter().flatten() {
        remove(equalities, &removed);
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
fn property_based_testing<T: Term + 'static>(
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

    property.apply(&mut table, &mut premise).map_err(|x| match x {
        ApplyPropertyError::ExceedLimitError(_) => {
            TestCaseError::reject("too complex property")
        }
        ApplyPropertyError::TypeAliasIDCollision => {
            TestCaseError::reject("type alias id collision")
        }
    })?;

    prop_assert!(equals(
        &term1,
        &term2,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    prop_assert!(equals(
        &term2,
        &term1,
        &premise,
        &table,
        &mut semantic::Default,
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

        if remove_sampled::<Type>(&mut premise_removed.equalities_mapping)
            || remove_sampled::<Lifetime>(
                &mut premise_removed.equalities_mapping,
            )
            || remove_sampled::<Constant>(
                &mut premise_removed.equalities_mapping,
            )
        {
            prop_assert!(!equals(
                &term1,
                &term2,
                &premise_removed,
                &table,
                &mut semantic::Default,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);
            prop_assert!(!equals(
                &term2,
                &term1,
                &premise_removed,
                &table,
                &mut semantic::Default,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);
        }
    }

    // remove the type alias symbol should make the terms not equal.
    {
        let id = table.representation.types.ids().next().copied();
        if let Some(id) = id {
            let removed = table.representation.types.remove(id).unwrap();

            prop_assert!(!equals(
                &term1,
                &term2,
                &premise,
                &table,
                &mut semantic::Default,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);
            prop_assert!(!equals(
                &term2,
                &term1,
                &premise,
                &table,
                &mut semantic::Default,
                &mut Limit::new(&mut session::Default::default())
            )
            .map_err(|_| TestCaseError::reject("too complex property"))?);

            table.representation.types.insert_with_id(id, removed).unwrap();
        }
    }

    // adding unrelated equalities should not affect the result.
    for decoy_lifetime_equalities in decoy.lifetimes {
        premise.equalities_mapping.insert(
            decoy_lifetime_equalities.lhs,
            decoy_lifetime_equalities.rhs,
        );
    }

    for decoy_type_equalities in decoy.types {
        premise
            .equalities_mapping
            .insert(decoy_type_equalities.lhs, decoy_type_equalities.rhs);
    }

    for decoy_constant_equalities in decoy.constants {
        premise.equalities_mapping.insert(
            decoy_constant_equalities.lhs,
            decoy_constant_equalities.rhs,
        );
    }

    prop_assert!(equals(
        &term1,
        &term2,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);
    prop_assert!(equals(
        &term2,
        &term1,
        &premise,
        &table,
        &mut semantic::Default,
        &mut Limit::new(&mut session::Default::default())
    )
    .map_err(|_| TestCaseError::reject("too complex property"))?);

    Ok(())
}

#[derive(Debug, Default)]
pub struct Decoy {
    lifetimes: Vec<DecoyEquality<Lifetime>>,
    types: Vec<DecoyEquality<Type>>,
    constants: Vec<DecoyEquality<Constant>>,
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
                lifetimes: lts,
                types: tys,
                constants: consts,
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
        ..Default::default()
    })]

    #[test]
    fn property_based_testing_constant(
       property in Box::<dyn Property<Constant>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_type(
        property in Box::<dyn Property<Type>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }

    #[test]
    fn property_based_testing_lifetime(
        property in Box::<dyn Property<Lifetime>>::arbitrary(),
        decoy in Decoy::arbitrary()
    ) {
        property_based_testing(&*property, decoy)?;
    }
}
