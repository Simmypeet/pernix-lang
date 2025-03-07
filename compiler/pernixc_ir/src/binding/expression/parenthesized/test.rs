use pernixc_syntax::{syntax_tree, utility::parse};

use crate::{
    binding::{
        diagnostic::MoreThanOneUnpackedInTupleExpression,
        test::{build_table, BindExt, CreateBinderAtExt, Template},
    },
    model,
};

#[test]
fn parenthesized_propagate() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let numeric = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::unit::Parenthesized,
        >("(32)"))
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric.integer_string, "32");
    assert!(numeric.decimal_stirng.is_none());
}

#[test]
fn parenthesized_as_unit() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let _ = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::unit::Parenthesized,
        >("()"))
        .into_literal()
        .unwrap()
        .into_unit()
        .unwrap();
}

#[test]
fn parenthesized_as_single_tuple() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let tuple_register_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::unit::Parenthesized,
        >("(32,)"))
        .into_register()
        .unwrap();

    let tuple_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(tuple_register_id)
        .unwrap()
        .assignment
        .as_tuple()
        .unwrap();

    assert_eq!(tuple_assignment.elements.len(), 1);

    let element = tuple_assignment.elements.first().unwrap();

    assert!(!element.is_unpacked);

    let numeric = element.value.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(numeric.integer_string, "32");
    assert!(numeric.decimal_stirng.is_none());
}

#[test]
fn parenthesized_as_tuple() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let tuple_register_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::unit::Parenthesized,
        >("(32, ...(true,), 64)"))
        .into_register()
        .unwrap();

    let tuple_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(tuple_register_id)
        .unwrap()
        .assignment
        .as_tuple()
        .unwrap();

    assert_eq!(tuple_assignment.elements.len(), 3);

    // 32
    {
        let element = tuple_assignment.elements.first().unwrap();

        assert!(!element.is_unpacked);

        let numeric = element.value.as_literal().unwrap().as_numeric().unwrap();

        assert_eq!(numeric.integer_string, "32");
        assert!(numeric.decimal_stirng.is_none());
    }

    // ...(true,)
    {
        let element = tuple_assignment.elements.get(1).unwrap();

        assert!(element.is_unpacked);

        let register_id = element.value.as_register().unwrap();

        let tuple_assignment = binder
            .intermediate_representation
            .values
            .registers
            .get(*register_id)
            .unwrap()
            .assignment
            .as_tuple()
            .unwrap();

        assert_eq!(tuple_assignment.elements.len(), 1);

        let element = tuple_assignment.elements.first().unwrap();

        assert!(!element.is_unpacked);

        let boolean = element.value.as_literal().unwrap().as_boolean().unwrap();

        assert!(boolean.value);
    }

    // 64
    {
        let element = tuple_assignment.elements.get(2).unwrap();

        assert!(!element.is_unpacked);

        let numeric = element.value.as_literal().unwrap().as_numeric().unwrap();

        assert_eq!(numeric.integer_string, "64");
        assert!(numeric.decimal_stirng.is_none());
    }
}

const MORE_THAN_ONE_UNPACKED_ERROR: &str = r"
public function test[T: tuple, U: tuple](t: T, u: U):
    pass
";

#[test]
fn more_than_one_unpacked_error() {
    let table = build_table(MORE_THAN_ONE_UNPACKED_ERROR);
    let mut binder = table.create_binder_at(["test", "test"]);

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::unit::Parenthesized,
    >("(...t, ...u)"));

    assert_eq!(errors.len(), 1);

    assert!(errors.iter().any(|x| {
        x.as_any()
            .downcast_ref::<MoreThanOneUnpackedInTupleExpression<model::Constrained>>(
            )
            .is_some()
    }));
}
