use pernixc_syntax::{syntax_tree, utility::parse};

use crate::binding::{
    diagnostic::UnreachableMatchArm,
    test::{build_table, BindExt, CreateBinderAtExt, Template},
};

#[test]
#[allow(clippy::too_many_lines)]
fn unreachable_match_arm() {
    const BOOLEAN_UNREACHABLE_MATCH_ARM: &str = r"
        match (true) {
            true: 1,
            true: 3,
            false: 2,
        }
    ";

    const ENUM_UNREACHABLE_MATCH_ARM: &str = r"
        match (Order::First) {
            case First: 1,
            case Second: 2,
            case First: 3,
            case Third: 4,
        }
    ";

    const ENUM_UNREACHABLE_MATCH_ARM_BY_IRREFUTABLE_PATTERN: &str = r"
        match (Order::First) {
            case First: 1,
            irrefutable:  2,
            case Third: 3,
        }
    ";

    const ORDER_ENUM: &str = r"
        public enum Order {
            First,
            Second,
            Third,
        }

        public function test() {}

    ";
    let table = build_table(ORDER_ENUM);
    let mut binder = table.create_binder_at(["test", "test"]);

    // boolean with unreachable match arm
    {
        let (value, errors) = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Match,
        >(
            BOOLEAN_UNREACHABLE_MATCH_ARM,
        ));

        let phi_node = binder
            .intermediate_representation
            .values
            .registers
            .get(value.into_register().unwrap())
            .unwrap()
            .assignment
            .as_phi()
            .unwrap();

        assert_eq!(phi_node.incoming_values.len(), 2);

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "1"
        }));

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "2"
        }));

        assert_eq!(errors.len(), 1);
        let error =
            errors[0].as_any().downcast_ref::<UnreachableMatchArm>().unwrap();

        assert!(error.match_arm_span.str() == "3");
    }

    // enum unerachable match arm
    {
        let (value, errors) = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Match,
        >(
            ENUM_UNREACHABLE_MATCH_ARM,
        ));

        let phi_node = binder
            .intermediate_representation
            .values
            .registers
            .get(value.into_register().unwrap())
            .unwrap()
            .assignment
            .as_phi()
            .unwrap();

        assert_eq!(phi_node.incoming_values.len(), 3);

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "1"
        }));

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "2"
        }));

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "4"
        }));

        assert_eq!(errors.len(), 1);

        let error =
            errors[0].as_any().downcast_ref::<UnreachableMatchArm>().unwrap();

        assert_eq!(error.match_arm_span.str(), "3");
    }

    // enum unreachable match arm by irrefutable pattern
    {
        let (value, errors) = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Match,
        >(
            ENUM_UNREACHABLE_MATCH_ARM_BY_IRREFUTABLE_PATTERN,
        ));

        let phi_node = binder
            .intermediate_representation
            .values
            .registers
            .get(value.into_register().unwrap())
            .unwrap()
            .assignment
            .as_phi()
            .unwrap();

        assert_eq!(phi_node.incoming_values.len(), 2);

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "1"
        }));

        assert!(phi_node.incoming_values.values().any(|x| {
            let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
            else {
                return false;
            };

            numeric_lit.decimal_stirng.is_none()
                && numeric_lit.integer_string == "2"
        }));

        assert_eq!(errors.len(), 1);
        let error =
            errors[0].as_any().downcast_ref::<UnreachableMatchArm>().unwrap();

        assert_eq!(error.match_arm_span.str(), "3");
    }
}

#[test]
fn exhaustive_match_arm() {
    const BOOLEAN_WITH_WILDCARD: &str = r"
        match (true, false) {
            (true, false): 1,
            (false, false): 2,
            (.., true): 3,
        }
    ";

    let tempalte = Template::new();
    let mut binder = tempalte.create_binder();

    let value = binder.bind_as_rvalue_success(&parse::<
        syntax_tree::expression::Match,
    >(BOOLEAN_WITH_WILDCARD));

    let phi_node = binder
        .intermediate_representation
        .values
        .registers
        .get(value.into_register().unwrap())
        .unwrap()
        .assignment
        .as_phi()
        .unwrap();

    assert_eq!(phi_node.incoming_values.len(), 3);

    assert!(phi_node.incoming_values.values().any(|x| {
        let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
        else {
            return false;
        };

        numeric_lit.decimal_stirng.is_none()
            && numeric_lit.integer_string == "1"
    }));

    assert!(phi_node.incoming_values.values().any(|x| {
        let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
        else {
            return false;
        };

        numeric_lit.decimal_stirng.is_none()
            && numeric_lit.integer_string == "2"
    }));

    assert!(phi_node.incoming_values.values().any(|x| {
        let Some(numeric_lit) = x.as_literal().and_then(|x| x.as_numeric())
        else {
            return false;
        };

        numeric_lit.decimal_stirng.is_none()
            && numeric_lit.integer_string == "3"
    }));
}
