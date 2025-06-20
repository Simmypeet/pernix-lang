use pernixc_handler::Panic;
use pernixc_semantic::component::derived::ir::{
    instruction::Instruction,
    value::{
        register::{
            ArithmeticOperator, BinaryOperator, BitwiseOperator,
            RelationalOperator,
        },
        Value,
    },
};
use pernixc_syntax::{syntax_tree, utility::parse};

use crate::{
    diagnostic::ExpectedLValue,
    expression::LValue,
    test::{BindExt, Template},
};

#[test]
fn assignment() {
    const DECLARATION: &str = "let mut x = 32";
    const ASSIGNMENT: &str = "x = 64";

    let template = Template::new();
    let mut binder = template.create_binder();

    let (variable_address, _) =
        binder.bind_variable_declaration(&parse(DECLARATION), &Panic).unwrap();

    let LValue { address, .. } =
        binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::binary::Binary,
        >(ASSIGNMENT));

    assert_eq!(address, variable_address);

    assert!(binder.current_block().instructions().iter().any(|x| {
        let Instruction::Store(store) = x else {
            return false;
        };

        let correct_address = store.address == variable_address;

        let Some(numeric_literal) =
            store.value.as_literal().and_then(|x| x.as_numeric())
        else {
            return false;
        };

        correct_address
            && numeric_literal.integer_string == "64"
            && numeric_literal.decimal_stirng.is_none()
    }));

    // bind as value
    let register_id = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::binary::Binary>(ASSIGNMENT),
        )
        .into_register()
        .unwrap();

    let register = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_load()
        .unwrap();

    assert_eq!(register.address, variable_address);
}

#[test]
fn normal_operator() {
    const ARITHMETIC: &str = "1 + 2";
    const RELATIONAL: &str = "3 < 4";
    const SHIFT: &str = "5i32 << 6u64";
    const BITWISE: &str = "7 & 8";

    let template = Template::new();
    let mut binder = template.create_binder();

    let mut check = |source: &str, operator, lhs_str: &str, rhs_str: &str| {
        let register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::binary::Binary,
            >(source))
            .into_register()
            .unwrap();

        let register = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_binary()
            .unwrap();

        assert_eq!(register.operator, operator);

        let number_one =
            register.lhs.as_literal().unwrap().as_numeric().unwrap();

        assert_eq!(number_one.integer_string, lhs_str);
        assert_eq!(number_one.decimal_stirng, None);

        let number_two =
            register.rhs.as_literal().unwrap().as_numeric().unwrap();

        assert_eq!(number_two.integer_string, rhs_str);
        assert_eq!(number_two.decimal_stirng, None);
    };

    // arithmetic
    check(
        ARITHMETIC,
        BinaryOperator::Arithmetic(ArithmeticOperator::Add),
        "1",
        "2",
    );

    // relational
    check(
        RELATIONAL,
        BinaryOperator::Relational(RelationalOperator::LessThan),
        "3",
        "4",
    );

    // shift
    check(SHIFT, BinaryOperator::Bitwise(BitwiseOperator::LeftShift), "5", "6");

    // bitwise
    check(BITWISE, BinaryOperator::Bitwise(BitwiseOperator::And), "7", "8");
}

#[test]
fn compound_binary_operator() {
    const DECLARATION: &str = "let mut x = 32";
    const ASSIGNMENT: &str = "x += 64";

    let template = Template::new();
    let mut binder = template.create_binder();

    let (variable_address, _) =
        binder.bind_variable_declaration(&parse(DECLARATION), &Panic).unwrap();

    let LValue { address: found_address, .. } =
        binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::binary::Binary,
        >(ASSIGNMENT));

    assert_eq!(found_address, variable_address);

    assert!(binder.current_block().instructions().iter().any(|x| {
        let Instruction::Store(store) = x else {
            return false;
        };

        let Value::Register(binary_register_id) = &store.value else {
            return false;
        };

        if store.address != variable_address {
            return false;
        }

        let Some(binary) = binder
            .intermediate_representation
            .values
            .registers
            .get(*binary_register_id)
            .unwrap()
            .assignment
            .as_binary()
        else {
            return false;
        };

        assert_eq!(
            binary.operator,
            BinaryOperator::Arithmetic(ArithmeticOperator::Add)
        );

        let Value::Register(binary_lhs_register_id) = binary.lhs else {
            return false;
        };

        let Some(load) = binder
            .intermediate_representation
            .values
            .registers
            .get(binary_lhs_register_id)
            .unwrap()
            .assignment
            .as_load()
        else {
            return false;
        };

        if load.address != variable_address {
            return false;
        }

        binary.rhs.as_literal().and_then(|x| x.as_numeric()).is_some_and(|x| {
            x.integer_string == "64" && x.decimal_stirng.is_none()
        })
    }));
}

#[test]
fn expected_lvalue_error() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::binary::Binary,
    >("true = false"));

    assert_eq!(errors.len(), 1);
    assert!(errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpectedLValue>()
        .is_some());
}

#[test]
fn binary_operator_precedence() {
    const EXPRESSION: &str = "1 + 2 * 3";

    let template = Template::new();
    let mut binder = template.create_binder();

    let add_op = binder
        .bind_as_rvalue_success(
            &parse::<syntax_tree::expression::binary::Binary>(EXPRESSION),
        )
        .into_register()
        .unwrap();

    let add_register = binder
        .intermediate_representation
        .values
        .registers
        .get(add_op)
        .unwrap()
        .assignment
        .as_binary()
        .unwrap();

    assert_eq!(
        add_register.operator,
        BinaryOperator::Arithmetic(ArithmeticOperator::Add)
    );

    let num_one = add_register.lhs.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(num_one.integer_string, "1");
    assert!(num_one.decimal_stirng.is_none());

    let mul_register = binder
        .intermediate_representation
        .values
        .registers
        .get(*add_register.rhs.as_register().unwrap())
        .unwrap()
        .assignment
        .as_binary()
        .unwrap();

    assert_eq!(
        mul_register.operator,
        BinaryOperator::Arithmetic(ArithmeticOperator::Multiply)
    );

    let num_two = mul_register.lhs.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(num_two.integer_string, "2");
    assert!(num_two.decimal_stirng.is_none());

    let num_three =
        mul_register.rhs.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(num_three.integer_string, "3");
    assert!(num_three.decimal_stirng.is_none());
}
