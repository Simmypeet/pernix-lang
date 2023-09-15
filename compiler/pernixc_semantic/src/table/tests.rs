use std::collections::{HashMap, HashSet};

use crate::{
    constant,
    symbol::{
        GenericItemRef, GenericParameterRef, HigherRankedLifetime, HigherRankedableLifetime,
        LifetimeBoundOperand, LocalSubstitution, TraitBound, WhereClause,
    },
    ty::{self, Lifetime},
};

#[test]
#[allow(clippy::too_many_lines)]
fn trait_bound_is_subset_or_equal_test() {
    // regular lifetime equals case
    {
        let trivial = TraitBound {
            trait_index: 0,
            type_substituions: vec![ty::Type::Primitive(ty::Primitive::Bool)],
            lifetime_substituions: vec![
                HigherRankedableLifetime::Regular(Lifetime::Static),
                HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                })),
            ],
            constant_substituions: vec![constant::Constant::Int16(0)],
        };

        assert!(super::Table::trait_bound_is_subset_or_equal(
            &trivial, &trivial
        ));
    }
    // regular lifetime not equals case
    {
        let target = TraitBound {
            trait_index: 0,
            type_substituions: vec![ty::Type::Primitive(ty::Primitive::Bool)],
            lifetime_substituions: vec![
                HigherRankedableLifetime::Regular(Lifetime::Static),
                HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                })),
            ],
            constant_substituions: vec![constant::Constant::Int16(0)],
        };

        let mut source = target.clone();
        source.lifetime_substituions[1] = HigherRankedableLifetime::Regular(Lifetime::Static);

        assert!(!super::Table::trait_bound_is_subset_or_equal(
            &target, &source
        ));
    }
    // regular higher ranked subset case
    {
        let source = TraitBound {
            trait_index: 0,
            type_substituions: vec![ty::Type::Primitive(ty::Primitive::Bool)],
            lifetime_substituions: vec![
                HigherRankedableLifetime::Regular(Lifetime::Static),
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 }),
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 1 }),
            ],
            constant_substituions: vec![constant::Constant::Int16(0)],
        };

        let mut target = source.clone();
        target.lifetime_substituions[1] =
            HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                generic_item_ref: GenericItemRef::Struct(0),
                index: 0,
            }));
        target.lifetime_substituions[2] =
            HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                generic_item_ref: GenericItemRef::Struct(0),
                index: 1,
            }));

        assert!(super::Table::trait_bound_is_subset_or_equal(
            &target, &source
        ));
    }
    // regular higher ranked map to multiple case
    {
        let source = TraitBound {
            trait_index: 0,
            type_substituions: vec![ty::Type::Primitive(ty::Primitive::Bool)],
            lifetime_substituions: vec![
                HigherRankedableLifetime::Regular(Lifetime::Static),
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 }),
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 }),
            ],
            constant_substituions: vec![constant::Constant::Int16(0)],
        };

        let mut target = source.clone();
        target.lifetime_substituions[1] =
            HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                generic_item_ref: GenericItemRef::Struct(0),
                index: 0,
            }));
        target.lifetime_substituions[2] =
            HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                generic_item_ref: GenericItemRef::Struct(0),
                index: 1,
            }));

        assert!(!super::Table::trait_bound_is_subset_or_equal(
            &target, &source
        ));
    }

    // higher ranked trait bound on target
    {
        let source = TraitBound {
            trait_index: 0,
            type_substituions: vec![ty::Type::Primitive(ty::Primitive::Bool)],
            lifetime_substituions: vec![
                HigherRankedableLifetime::Regular(Lifetime::Static),
                HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                })),
            ],
            constant_substituions: vec![constant::Constant::Int16(0)],
        };

        let mut target = source.clone();
        target.lifetime_substituions[1] =
            HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 });

        assert!(!super::Table::trait_bound_is_subset_or_equal(
            &target, &source
        ));
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn where_clause_combine_normal_test() {
    /*
    '0: 'static,
    Trait0::Assoc0 = bool,
    Trait0::Assoc1 = {true},
    Trait0,
    */
    let mut first_where_clause = WhereClause {
        // single lifetime bound => '0: 'static
        lifetime_bounds: {
            let mut lifetime_bounds = HashMap::new();

            lifetime_bounds.insert(
                LifetimeBoundOperand::LifetimeParameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                }),
                {
                    let mut bounds = HashSet::new();
                    bounds.insert(Lifetime::Static);
                    bounds
                },
            );

            lifetime_bounds
        },
        associated_type_bounds: {
            let mut associated_type_bounds = HashMap::new();

            associated_type_bounds.insert(
                ty::TraitAssociated {
                    trait_index: 0,
                    associated_index: 0,
                    trait_substitution: LocalSubstitution::default(),
                    associated_substitution: LocalSubstitution::default(),
                },
                ty::Type::Primitive(ty::Primitive::Bool),
            );

            associated_type_bounds
        },
        associated_constant_bounds: {
            let mut associated_constant_bounds = HashMap::new();

            associated_constant_bounds.insert(
                constant::TraitAssociated {
                    trait_index: 0,
                    associated_index: 1,
                    trait_substitution: LocalSubstitution::default(),
                    associated_substitution: LocalSubstitution::default(),
                },
                constant::Constant::Bool(true),
            );

            associated_constant_bounds
        },
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 0,
                    type_substituions: Vec::new(),
                    lifetime_substituions: Vec::new(),
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    /*
    '0: '1,
    '2: 'static,
    Trait2::Assoc0 = bool,
    Trait2::Assoc1 = {true},
    Trait3,
    */
    let second_where_clause = WhereClause {
        // single lifetime bound => '0: '1
        lifetime_bounds: {
            let mut lifetime_bounds = HashMap::new();

            lifetime_bounds.insert(
                LifetimeBoundOperand::LifetimeParameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                }),
                {
                    let mut bounds = HashSet::new();
                    bounds.insert(Lifetime::Parameter(GenericParameterRef {
                        generic_item_ref: GenericItemRef::Struct(0),
                        index: 1,
                    }));
                    bounds
                },
            );

            lifetime_bounds.insert(
                LifetimeBoundOperand::LifetimeParameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 2,
                }),
                {
                    let mut bounds = HashSet::new();
                    bounds.insert(Lifetime::Static);
                    bounds
                },
            );

            lifetime_bounds
        },
        associated_type_bounds: {
            let mut associated_type_bounds = HashMap::new();

            associated_type_bounds.insert(
                ty::TraitAssociated {
                    trait_index: 2,
                    associated_index: 0,
                    trait_substitution: LocalSubstitution::default(),
                    associated_substitution: LocalSubstitution::default(),
                },
                ty::Type::Primitive(ty::Primitive::Bool),
            );

            associated_type_bounds
        },
        associated_constant_bounds: {
            let mut associated_constant_bounds = HashMap::new();

            associated_constant_bounds.insert(
                constant::TraitAssociated {
                    trait_index: 2,
                    associated_index: 1,
                    trait_substitution: LocalSubstitution::default(),
                    associated_substitution: LocalSubstitution::default(),
                },
                constant::Constant::Bool(true),
            );

            associated_constant_bounds
        },
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 3,
                    type_substituions: Vec::new(),
                    lifetime_substituions: Vec::new(),
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    super::Table::combine_where_clause(&mut first_where_clause, &second_where_clause);

    /*
    Expected:
    '0: '1 + 'static,
    '2: 'static,
    Trait0::Assoc0 = bool,
    Trait0::Assoc1 = {true},
    Trait2::Assoc0 = bool,
    Trait2::Assoc1 = {true},
    Trait3, Trait0
    */

    assert_eq!(first_where_clause.lifetime_bounds.len(), 2);
    assert_eq!(first_where_clause.associated_type_bounds.len(), 2);
    assert_eq!(first_where_clause.associated_constant_bounds.len(), 2);
    assert_eq!(first_where_clause.trait_bounds.len(), 2);

    // Trait3 and Trait0
    {
        let bounds = first_where_clause
            .trait_bounds
            .get(&TraitBound {
                trait_index: 3,
                type_substituions: Vec::new(),
                lifetime_substituions: Vec::new(),
                constant_substituions: Vec::new(),
            })
            .unwrap();

        assert_eq!(bounds, &true);

        let bounds = first_where_clause
            .trait_bounds
            .get(&TraitBound {
                trait_index: 0,
                type_substituions: Vec::new(),
                lifetime_substituions: Vec::new(),
                constant_substituions: Vec::new(),
            })
            .unwrap();

        assert_eq!(bounds, &true);
    }

    // Trait2::Assoc1 = {true}
    {
        let bounds = first_where_clause
            .associated_constant_bounds
            .get(&constant::TraitAssociated {
                trait_index: 2,
                associated_index: 1,
                trait_substitution: LocalSubstitution::default(),
                associated_substitution: LocalSubstitution::default(),
            })
            .unwrap();

        assert_eq!(bounds, &constant::Constant::Bool(true));
    }

    // Trait2::Assoc0 = bool
    {
        let bounds = first_where_clause
            .associated_type_bounds
            .get(&ty::TraitAssociated {
                trait_index: 2,
                associated_index: 0,
                trait_substitution: LocalSubstitution::default(),
                associated_substitution: LocalSubstitution::default(),
            })
            .unwrap();

        assert_eq!(bounds, &ty::Type::Primitive(ty::Primitive::Bool));
    }

    // Trait0::Assoc1 = {true}
    {
        let bounds = first_where_clause
            .associated_constant_bounds
            .get(&constant::TraitAssociated {
                trait_index: 0,
                associated_index: 1,
                trait_substitution: LocalSubstitution::default(),
                associated_substitution: LocalSubstitution::default(),
            })
            .unwrap();

        assert_eq!(bounds, &constant::Constant::Bool(true));
    }

    // Trait0::Assoc0 = bool
    {
        let bounds = first_where_clause
            .associated_type_bounds
            .get(&ty::TraitAssociated {
                trait_index: 0,
                associated_index: 0,
                trait_substitution: LocalSubstitution::default(),
                associated_substitution: LocalSubstitution::default(),
            })
            .unwrap();

        assert_eq!(bounds, &ty::Type::Primitive(ty::Primitive::Bool));
    }

    // '2: 'static
    {
        let bounds = first_where_clause
            .lifetime_bounds
            .get(&LifetimeBoundOperand::LifetimeParameter(
                GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 2,
                },
            ))
            .unwrap();

        assert_eq!(bounds.len(), 1);

        assert!(bounds.contains(&Lifetime::Static));
    }

    // '0: '1 + 'static
    {
        let bounds = first_where_clause
            .lifetime_bounds
            .get(&LifetimeBoundOperand::LifetimeParameter(
                GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                },
            ))
            .unwrap();

        assert_eq!(bounds.len(), 2);

        assert!(bounds.contains(&Lifetime::Parameter(GenericParameterRef {
            generic_item_ref: GenericItemRef::Struct(0),
            index: 1,
        })));
        assert!(bounds.contains(&Lifetime::Static));
    }
}

#[test]
fn where_clause_combine_with_superset_trait_bound_test() {
    let mut first_where_clause = WhereClause {
        lifetime_bounds: HashMap::new(),
        associated_type_bounds: HashMap::new(),
        associated_constant_bounds: HashMap::new(),
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 0,
                    type_substituions: Vec::new(),
                    lifetime_substituions: vec![
                        HigherRankedableLifetime::Regular(Lifetime::Static),
                        HigherRankedableLifetime::Regular(Lifetime::Parameter(
                            GenericParameterRef {
                                generic_item_ref: GenericItemRef::Struct(0),
                                index: 0,
                            },
                        )),
                        HigherRankedableLifetime::Regular(Lifetime::Static),
                    ],
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    let second_where_clause = WhereClause {
        lifetime_bounds: HashMap::new(),
        associated_type_bounds: HashMap::new(),
        associated_constant_bounds: HashMap::new(),
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 0,
                    type_substituions: Vec::new(),
                    lifetime_substituions: vec![
                        HigherRankedableLifetime::HigherRanked(HigherRankedLifetime {
                            unique_id: 0,
                        }),
                        HigherRankedableLifetime::Regular(Lifetime::Parameter(
                            GenericParameterRef {
                                generic_item_ref: GenericItemRef::Struct(0),
                                index: 0,
                            },
                        )),
                        HigherRankedableLifetime::HigherRanked(HigherRankedLifetime {
                            unique_id: 0,
                        }),
                    ],
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    super::Table::combine_where_clause(&mut first_where_clause, &second_where_clause);

    assert_eq!(first_where_clause.trait_bounds.len(), 1);

    let bounds = first_where_clause
        .trait_bounds
        .get(&TraitBound {
            trait_index: 0,
            type_substituions: Vec::new(),
            lifetime_substituions: vec![
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 }),
                HigherRankedableLifetime::Regular(Lifetime::Parameter(GenericParameterRef {
                    generic_item_ref: GenericItemRef::Struct(0),
                    index: 0,
                })),
                HigherRankedableLifetime::HigherRanked(HigherRankedLifetime { unique_id: 0 }),
            ],
            constant_substituions: Vec::new(),
        })
        .unwrap();

    assert_eq!(bounds, &true);
}

#[test]
fn where_clause_combine_with_subset_trait_bound_test() {
    let mut first_where_clause = WhereClause {
        lifetime_bounds: HashMap::new(),
        associated_type_bounds: HashMap::new(),
        associated_constant_bounds: HashMap::new(),
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 0,
                    type_substituions: Vec::new(),
                    lifetime_substituions: vec![HigherRankedableLifetime::HigherRanked(
                        HigherRankedLifetime { unique_id: 0 },
                    )],
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    let second_where_clause = WhereClause {
        lifetime_bounds: HashMap::new(),
        associated_type_bounds: HashMap::new(),
        associated_constant_bounds: HashMap::new(),
        trait_bounds: {
            let mut trait_bounds = HashMap::new();

            trait_bounds.insert(
                TraitBound {
                    trait_index: 0,
                    type_substituions: Vec::new(),
                    lifetime_substituions: vec![HigherRankedableLifetime::Regular(
                        Lifetime::Static,
                    )],
                    constant_substituions: Vec::new(),
                },
                true,
            );

            trait_bounds
        },
    };

    super::Table::combine_where_clause(&mut first_where_clause, &second_where_clause);

    assert_eq!(first_where_clause.trait_bounds.len(), 1);

    let bounds = first_where_clause
        .trait_bounds
        .get(&TraitBound {
            trait_index: 0,
            type_substituions: Vec::new(),
            lifetime_substituions: vec![HigherRankedableLifetime::HigherRanked(
                HigherRankedLifetime { unique_id: 0 },
            )],
            constant_substituions: Vec::new(),
        })
        .unwrap();
    assert_eq!(bounds, &true);
}
