use pernixc_syntax::{syntax_tree, utility::parse};

use crate::{
    binding::test::{BindExt, Template},
    value::{literal::Literal, Value},
};

#[test]
fn unreachable_loop() {
    const SOURCE: &str = r"
loop:   
    pass
";

    let template = Template::new();
    let mut binder = template.create_binder();

    let value = binder.bind_as_rvalue_success(&parse::<
        syntax_tree::expression::block::Loop,
    >(SOURCE));

    assert!(value.into_literal().unwrap().is_unreachable());
    assert!(binder.current_block().is_unreachable_or_terminated());
}

#[test]
fn single_break_loop() {
    let template = Template::new();
    {
        const SOURCE: &str = r"
loop:
    break 32
";
        let mut binder = template.create_binder();

        let numeric_literal = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::block::Loop,
            >(SOURCE))
            .into_literal()
            .unwrap()
            .into_numeric()
            .unwrap();

        assert_eq!(numeric_literal.integer_string, "32");
        assert!(numeric_literal.decimal_stirng.is_none());

        assert!(!binder.current_block().is_unreachable_or_terminated());
    }

    {
        const SOURCE: &str = r"
loop:
    break 64
    break 128
";

        let mut binder = template.create_binder();

        let numeric_literal = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::block::Loop,
            >(SOURCE))
            .into_literal()
            .unwrap()
            .into_numeric()
            .unwrap();

        assert_eq!(numeric_literal.integer_string, "64");
        assert!(numeric_literal.decimal_stirng.is_none());

        assert!(!binder.current_block().is_unreachable_or_terminated());
    }
}

#[test]
fn multiple_break_loop() {
    const BREAK_LOOP: &str = r"
loop:
    if true:
        break 32
    else if true:
        break 64
    else:
        break 128
    

    break 256 // unreachable
";

    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::block::Loop>(
            BREAK_LOOP,
        ))
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

    check("32");
    check("64");
    check("128");
}
