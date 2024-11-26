use crate::{
    error::UnreachableMatchArm,
    ir::representation::binding::expression::{
        test::{setup_and_bind, Check},
        Config, Target,
    },
    symbol::{Accessibility, AdtTemplate, EnumDefinition, GenericDeclaration},
};

#[test]
fn unreachable_match_arm() {
    const BOOLEAN_UNREACHABLE_MATCH_ARM: &str = r#"
        match (true) {
            true: 1,
            true: 3,
            false: 2,
        }
    "#;

    const ENUM_UNREACHABLE_MATCH_ARM: &str = r#"
        match (Order::First) {
            case First: 1,
            case Second: 2,
            case First: 3,
            case Third: 4,
        }
    "#;

    const ENUM_UNREACHABLE_MATCH_ARM_BY_IRREFUTABLE_PATTERN: &str = r#"
        match (Order::First) {
            case First: 1,
            irrefutable:  2,
            case Third: 3,
        }
    "#;

    setup_and_bind(
        |test_template| {
            let order_enum_id = test_template
                .table
                .insert_member(
                    "Order".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    GenericDeclaration::default(),
                    AdtTemplate::<EnumDefinition>::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            let first_variant_id = test_template
                .table
                .insert_variant("First".to_string(), order_enum_id, None, None)
                .unwrap()
                .unwrap_no_duplication();

            let second_variant_id = test_template
                .table
                .insert_variant("Second".to_string(), order_enum_id, None, None)
                .unwrap()
                .unwrap_no_duplication();

            let third_variant_id = test_template
                .table
                .insert_variant("Third".to_string(), order_enum_id, None, None)
                .unwrap()
                .unwrap_no_duplication();

            let (binder, _) = test_template.create_binder();

            (
                binder,
                (
                    order_enum_id,
                    first_variant_id,
                    second_variant_id,
                    third_variant_id,
                ),
            )
        },
        [
            Check::new(
                BOOLEAN_UNREACHABLE_MATCH_ARM,
                Config { target: Target::RValue },
                |binder, exp, errors, _| {
                    let phi_node = binder
                        .intermediate_representation
                        .registers
                        .get(
                            exp.unwrap()
                                .into_r_value()
                                .unwrap()
                                .into_register()
                                .unwrap(),
                        )
                        .unwrap()
                        .assignment
                        .as_phi()
                        .unwrap();

                    assert_eq!(phi_node.incoming_values.len(), 2);

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "1"
                    }));

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "2"
                    }));

                    assert_eq!(errors.len(), 1);

                    assert!(errors.iter().any(|x| {
                        let Some(x) =
                            x.as_any().downcast_ref::<UnreachableMatchArm>()
                        else {
                            return false;
                        };

                        x.match_arm_span.str() == "3"
                    }));
                },
            ),
            Check::new(
                ENUM_UNREACHABLE_MATCH_ARM,
                Config { target: Target::RValue },
                |binder, exp, errors, _| {
                    let phi_node = binder
                        .intermediate_representation
                        .registers
                        .get(
                            exp.unwrap()
                                .into_r_value()
                                .unwrap()
                                .into_register()
                                .unwrap(),
                        )
                        .unwrap()
                        .assignment
                        .as_phi()
                        .unwrap();

                    assert_eq!(phi_node.incoming_values.len(), 3);

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "1"
                    }));

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "2"
                    }));

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "4"
                    }));

                    assert_eq!(errors.len(), 1);

                    assert!(errors.iter().any(|x| {
                        let Some(x) =
                            x.as_any().downcast_ref::<UnreachableMatchArm>()
                        else {
                            return false;
                        };

                        x.match_arm_span.str() == "3"
                    }));
                },
            ),
            Check::new(
                ENUM_UNREACHABLE_MATCH_ARM_BY_IRREFUTABLE_PATTERN,
                Config { target: Target::RValue },
                |binder, exp, errors, _| {
                    let phi_node = binder
                        .intermediate_representation
                        .registers
                        .get(
                            exp.unwrap()
                                .into_r_value()
                                .unwrap()
                                .into_register()
                                .unwrap(),
                        )
                        .unwrap()
                        .assignment
                        .as_phi()
                        .unwrap();

                    assert_eq!(phi_node.incoming_values.len(), 2);

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "1"
                    }));

                    assert!(phi_node.incoming_values.values().any(|x| {
                        let Some(numeric_lit) =
                            x.as_literal().and_then(|x| x.as_numeric())
                        else {
                            return false;
                        };

                        numeric_lit.decimal_stirng.is_none()
                            && numeric_lit.integer_string == "2"
                    }));

                    assert_eq!(errors.len(), 1);

                    assert!(errors.iter().any(|x| {
                        let Some(x) =
                            x.as_any().downcast_ref::<UnreachableMatchArm>()
                        else {
                            return false;
                        };

                        x.match_arm_span.str() == "3"
                    }));
                },
            ),
        ],
    );
}

#[test]
fn non_exhaustive_match_arm() {}

#[test]
fn exhaustive_match_arm() {
    const BOOLEAN_WITH_WILDCARD: &str = r#"
        match (true, false) {
            (true, false): 1,
            (false, false): 2,
            (.., true): 3,
        }
    "#;

    setup_and_bind(
        |test_template| {
            let (binder, _) = test_template.create_binder();

            (binder, ())
        },
        [Check::new(
            BOOLEAN_WITH_WILDCARD,
            Config { target: Target::RValue },
            |binder, exp, errors, _| {
                assert!(errors.is_empty());

                let phi_node = binder
                    .intermediate_representation
                    .registers
                    .get(
                        exp.unwrap()
                            .into_r_value()
                            .unwrap()
                            .into_register()
                            .unwrap(),
                    )
                    .unwrap()
                    .assignment
                    .as_phi()
                    .unwrap();

                assert_eq!(phi_node.incoming_values.len(), 3);
            },
        )],
    );
}

#[test]
fn inhabited_type_in_match_arm() {}

#[test]
fn zero_match_arms() {}

#[test]
fn option_enum() {}
