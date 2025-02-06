use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    binding::{
        diagnostic::MismatchedType,
        test::{BindExt, Template},
    },
    model::{self, Constraint},
    value::register::PrefixOperator,
};

#[test]
fn prefix_operator() {
    const LOGICAL_NOT_SOURCE: &str = "!false";
    const NEGATE_SOURCE: &str = "-32";
    const BITWISE_NOT_SOURCE: &str = "~32";

    let template = Template::new();
    let mut binder = template.create_binder();

    // logical not
    {
        let register_id =
            binder
                .bind_as_rvalue_success(&parse::<
                    syntax_tree::expression::Prefixable,
                >(LOGICAL_NOT_SOURCE))
                .into_register()
                .unwrap();

        let prefix = binder.intermediate_representation.values.registers
            [register_id]
            .assignment
            .as_prefix()
            .unwrap();

        assert_eq!(prefix.operator, PrefixOperator::LogicalNot);
        assert!(
            !prefix.operand.as_literal().unwrap().as_boolean().unwrap().value
        );
    }

    // negate
    {
        let register_id = binder
            .bind_as_rvalue_success(
                &parse::<syntax_tree::expression::Prefixable>(NEGATE_SOURCE),
            )
            .into_register()
            .unwrap();

        let prefix = binder.intermediate_representation.values.registers
            [register_id]
            .assignment
            .as_prefix()
            .unwrap();

        assert_eq!(prefix.operator, PrefixOperator::Negate);
        assert_eq!(
            prefix
                .operand
                .as_literal()
                .unwrap()
                .as_numeric()
                .unwrap()
                .integer_string,
            "32"
        );
        assert!(prefix
            .operand
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap()
            .decimal_stirng
            .is_none());
    }

    // bitwise not
    {
        let register_id =
            binder
                .bind_as_rvalue_success(&parse::<
                    syntax_tree::expression::Prefixable,
                >(BITWISE_NOT_SOURCE))
                .into_register()
                .unwrap();

        let prefix = binder.intermediate_representation.values.registers
            [register_id]
            .assignment
            .as_prefix()
            .unwrap();

        assert_eq!(prefix.operator, PrefixOperator::BitwiseNot);
        assert_eq!(
            prefix
                .operand
                .as_literal()
                .unwrap()
                .as_numeric()
                .unwrap()
                .integer_string,
            "32"
        );
        assert!(prefix
            .operand
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap()
            .decimal_stirng
            .is_none());
    }
}

#[test]
fn prefix_type_mismatched_error() {
    const LOGICAL_NOT_SOURCE: &str = "!64";
    const NEGATE_SOURCE: &str = "-32u32";
    const BITWISE_NOT_SOURCE: &str = "~32.0";

    let template = Template::new();
    let mut binder = template.create_binder();

    // logical not
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Prefixable,
        >(
            LOGICAL_NOT_SOURCE
        ));

        assert_eq!(errors.len(), 1);

        let error = errors[0]
            .as_any()
            .downcast_ref::<MismatchedType<model::Constrained>>()
            .unwrap();

        assert_eq!(error.expected_type, Type::Primitive(Primitive::Bool));
        assert_eq!(error.found_type, Type::Inference(Constraint::Number));
    }

    // negate
    {
        let errors =
            binder.bind_as_rvalue_error_fatal(&parse::<
                syntax_tree::expression::Prefixable,
            >(NEGATE_SOURCE));

        assert_eq!(errors.len(), 1);

        let error = errors[0]
            .as_any()
            .downcast_ref::<MismatchedType<model::Constrained>>()
            .unwrap();

        assert_eq!(error.expected_type, Type::Inference(Constraint::Signed));
        assert_eq!(error.found_type, Type::Primitive(Primitive::Uint32));
    }

    // bitwise not
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Prefixable,
        >(
            BITWISE_NOT_SOURCE
        ));

        assert_eq!(errors.len(), 1);

        let error = errors[0]
            .as_any()
            .downcast_ref::<MismatchedType<model::Constrained>>()
            .unwrap();

        assert_eq!(error.expected_type, Type::Inference(Constraint::Integer));
        assert_eq!(error.found_type, Type::Inference(Constraint::Floating));
    }
}
