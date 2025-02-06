use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    binding::{
        diagnostic::{MismatchedType, NotAllFlowPathsExpressValue},
        test::{BindExt, Template},
    },
    model::{self, Constraint},
    value::{literal::Literal, Value},
};

#[test]
fn multiple_express_block() {
    const BLOCK: &str = r"
    'x: {
        if (true) {
            express 'x 2;
        } else if (false) {
            express 'x 4;
        } else {
            express 'x 8;
        }

        express 'x 16; // unreachable
    }
    ";

    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Block>(BLOCK))
        .into_register()
        .unwrap();

    let phi = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_phi()
        .unwrap();

    assert_eq!(phi.incoming_values.len(), 3);

    let check = |integer_string| {
        phi.incoming_values.iter().any(|(block_id, value)| {
            let Value::Literal(Literal::Numeric(numeric)) = value else {
                return false;
            };

            binder.current_block().predecessors().contains(block_id)
                && numeric.integer_string == integer_string
                && numeric.decimal_stirng.is_none()
        });
    };

    check("2");
    check("4");
    check("8");
}

#[test]
fn express_type_mismatched() {
    const BLOCK: &str = r"
    'x: {
        if (true) {
            express 'x 2;
        } else {
            express 'x false;
        }
    }
    ";

    let template = Template::new();
    let mut binder = template.create_binder();

    let (_, errors) = binder
        .bind_as_rvalue_error(&parse::<syntax_tree::expression::Block>(BLOCK));

    assert_eq!(errors.len(), 1);

    let error = &errors[0]
        .as_any()
        .downcast_ref::<MismatchedType<model::Constrained>>()
        .unwrap();

    assert_eq!(error.expected_type, Type::Inference(Constraint::Number));
    assert_eq!(error.found_type, Type::Primitive(Primitive::Bool));
    assert_eq!(error.span.str(), "express 'x false");
}

#[test]
fn not_all_flow_path_express_value_error() {
    const BLOCK: &str = r"
    'outer: {
        if (true) {
            express 'outer 32;
        }
    }
    ";

    let template = Template::new();
    let mut binder = template.create_binder();

    let (_, errors) = binder
        .bind_as_rvalue_error(&parse::<syntax_tree::expression::Block>(BLOCK));

    assert_eq!(errors.len(), 1);

    assert!(errors[0]
        .as_any()
        .downcast_ref::<NotAllFlowPathsExpressValue>()
        .is_some());
}

#[test]
fn unrechable_block() {
    const BLOCK_WITH_EXPRESS: &str = r"
    {
        return;
        express 32i32;
    }
    ";
    const BLOCK_NO_EXPRESS: &str = r"
    {
        return;
    }
    ";

    let template = Template::new();
    let mut binder = template.create_binder();

    // block with express
    {
        let unreachable = binder
            .bind_as_rvalue_success(&parse::<syntax_tree::expression::Block>(
                BLOCK_WITH_EXPRESS,
            ))
            .into_literal()
            .unwrap()
            .into_unreachable()
            .unwrap();

        assert_eq!(unreachable.r#type, Type::Primitive(Primitive::Int32));
    }

    // block with no express
    {
        let unreachable = binder
            .bind_as_rvalue_success(&parse::<syntax_tree::expression::Block>(
                BLOCK_NO_EXPRESS,
            ))
            .into_literal()
            .unwrap()
            .into_unreachable()
            .unwrap();

        let inference = unreachable.r#type.into_inference().unwrap();
        let constraint_id = binder
            .inference_context
            .get_inference(inference)
            .cloned()
            .unwrap()
            .into_inferring()
            .unwrap();

        assert_eq!(
            *binder
                .inference_context
                .get_constraint::<Type<_>>(constraint_id)
                .unwrap(),
            Constraint::All(true)
        );
    }
}
