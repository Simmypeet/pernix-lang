use pernixc_base::handler::Storage;

use super::Expression;
use crate::{
    arena::ID,
    error::{
        AmbiguousMethodCall, CannotDereference, CannotIndexPastUnpackedTuple,
        DuplicatedFieldInitialization, Error, ExpectArray,
        ExpectAssociatedValue, ExpectLValue, ExpectStructType,
        ExpressionIsNotCallable, FieldIsNotAccessible, FieldNotFound,
        FloatingPointLiteralHasIntegralSuffix, InvalidCastType,
        InvalidNumericSuffix, MismatchedArgumentCount,
        MismatchedQualifierForReferenceOf, MismatchedType,
        NotAllFlowPathsExpressValue, TooLargeTupleIndex, TupleExpected,
        TupleIndexOutOfBOunds, UninitializedFields,
    },
    ir::{
        address::{self, Address, Memory},
        instruction::Instruction,
        representation::binding::{
            expression::{Bind, Config, Target},
            infer,
            test::{parse_expression, parse_statement, TestTemplate},
            Binder,
        },
        value::{
            literal::Literal,
            register::{
                ArithmeticOperator, BinaryOperator, BitwiseOperator, Register,
                RelationalOperator,
            },
            Value,
        },
        ConstraintModel, Erased,
    },
    symbol::{
        table::{
            representation::{Index, IndexMut, Insertion, RwLockContainer},
            resolution, Building, Table,
        },
        Accessibility, AdtID, AdtImplementationDefinition,
        AdtImplementationFunction, AdtTemplate, CallableID, Enum,
        EnumDefinition, Field, Function, FunctionDefinition, FunctionTemplate,
        GenericDeclaration, GenericID, LifetimeParameter, LifetimeParameterID,
        Module, Parameter, Struct, StructDefinition, TraitDefinition,
        TraitFunction, TypeParameter, TypeParameterID, Variant,
    },
    type_system::{
        self,
        equality::Equality,
        term::{
            lifetime::Lifetime,
            r#type::{self, Constraint, Primitive, Qualifier, Reference, Type},
            GenericArguments, Symbol,
        },
        Compute,
    },
};

fn create_dummy_function(
) -> (Table<Building<RwLockContainer, ()>>, ID<Function>) {
    let mut table = Table::default();

    let Insertion { id: test_module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let Insertion { id: function_id, duplication } = table
        .insert_member(
            "test".to_string(),
            Accessibility::Public,
            test_module_id,
            None,
            GenericDeclaration::default(),
            FunctionTemplate::<FunctionDefinition>::default(),
        )
        .unwrap();

    assert!(duplication.is_none());

    (table, function_id)
}

#[test]
fn assignment() {
    const DECLARATION: &str = "let mutable x = 32;";
    const ASSIGNMENT: &str = "x = 64";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let declaration =
        parse_statement(DECLARATION).into_variable_declaration().unwrap();

    let (variable_address, _) =
        binder.bind_variable_declaration(&declaration, &storage).unwrap();

    assert!(storage.as_vec().is_empty());

    let assignment = parse_expression(ASSIGNMENT).into_binary().unwrap();

    let found_address = binder
        .bind(&assignment, Config { target: Target::LValue }, &storage)
        .unwrap()
        .into_l_value()
        .unwrap()
        .address;

    assert!(storage.as_vec().is_empty());

    assert_eq!(found_address, variable_address);

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
        .bind(&assignment, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

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
    const SHIFT: &str = "5i32 << 6i64";
    const BITWISE: &str = "7 & 8";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let mut check = |source: &str, operator, lhs_str: &str, rhs_str: &str| {
        let arithmetic = parse_expression(source).into_binary().unwrap();

        let register_id = binder
            .bind(&arithmetic, Config { target: Target::RValue }, &storage)
            .unwrap()
            .into_r_value()
            .unwrap()
            .into_register()
            .unwrap();

        assert!(storage.as_vec().is_empty());

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
    const DECLARATION: &str = "let mutable x = 32;";
    const ASSIGNMENT: &str = "x += 64";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let declaration =
        parse_statement(DECLARATION).into_variable_declaration().unwrap();

    let (variable_address, _) =
        binder.bind_variable_declaration(&declaration, &storage).unwrap();

    assert!(storage.as_vec().is_empty());

    let assignment = parse_expression(ASSIGNMENT).into_binary().unwrap();

    let found_address = binder
        .bind(&assignment, Config { target: Target::LValue }, &storage)
        .unwrap()
        .into_l_value()
        .unwrap()
        .address;

    assert!(storage.as_vec().is_empty());

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

        binary
            .rhs
            .as_literal()
            .and_then(|x| x.as_numeric())
            .map_or(false, |x| {
                x.integer_string == "64" && x.decimal_stirng.is_none()
            })
    }));
}

#[test]
fn binary_operator_precedence() {
    const EXPRESSION: &str = "1 + 2 * 3";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let expression = parse_expression(EXPRESSION).into_binary().unwrap();

    let add_op = binder
        .bind(&expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

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

#[test]
fn not_all_flow_path_express_value_error() {
    const BLOCK: &str = r"
    'outer: {
        if (true) {
            express 'outer 32;
        }
    }
    ";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let block = parse_expression(BLOCK)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_brace()
        .unwrap()
        .into_block()
        .unwrap();

    let _ = binder
        .bind(&block, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap();

    let errors = storage.into_vec();

    assert_eq!(errors.len(), 1);

    assert!(errors.iter().any(|x| {
        x.as_any().downcast_ref::<NotAllFlowPathsExpressValue>().is_some()
    }));
}

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

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let block = parse_expression(BLOCK)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_brace()
        .unwrap()
        .into_block()
        .unwrap();

    let register_id = binder
        .bind(&block, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

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

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let block = parse_expression(BLOCK_WITH_EXPRESS)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_brace()
        .unwrap()
        .into_block()
        .unwrap();

    let unreachable = binder
        .bind(&block, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_unreachable()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    assert_eq!(unreachable.r#type, Type::Primitive(Primitive::Int32));

    let block = parse_expression(BLOCK_NO_EXPRESS)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_brace()
        .unwrap()
        .into_block()
        .unwrap();

    let unreachable = binder
        .bind(&block, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_unreachable()
        .unwrap();

    assert!(storage.as_vec().is_empty());

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
    )
}

fn bind_as_value(
    source: impl std::fmt::Display,
    check: impl FnOnce(
        &Binder<Building, resolution::NoOp, type_system::observer::NoOp>,
        Value<infer::Model>,
    ),
) {
    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let expression = parse_expression(source);

    let value = binder
        .bind(&expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap();

    check(&binder, value);
}

fn bind_as_value_expect_error(
    source: impl std::fmt::Display,
    check: impl FnOnce(
        &Binder<Building, resolution::NoOp, type_system::observer::NoOp>,
        Result<super::Expression, super::Error>,
        Vec<Box<dyn Error>>,
    ),
) {
    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let expression = parse_expression(source);

    let result =
        binder.bind(&expression, Config { target: Target::RValue }, &storage);

    check(&binder, result, storage.into_vec());
}

/*
 */

#[test]
fn unreachable_loop() {
    bind_as_value("loop {}", |binder, value| {
        assert!(value.into_literal().unwrap().is_unreachable());
        assert!(binder.current_block().is_unreachable_or_terminated())
    })
}

#[test]
fn single_break_loop() {
    bind_as_value("loop { break 32; }", |binder, value| {
        let numeric_literal =
            value.into_literal().unwrap().into_numeric().unwrap();

        assert_eq!(numeric_literal.integer_string, "32");
        assert!(numeric_literal.decimal_stirng.is_none());

        assert!(!binder.current_block().is_unreachable_or_terminated());
    });

    bind_as_value("loop { break 64; break 32; }", |binder, value| {
        let numeric_literal =
            value.into_literal().unwrap().into_numeric().unwrap();

        assert_eq!(numeric_literal.integer_string, "64");
        assert!(numeric_literal.decimal_stirng.is_none());

        assert!(!binder.current_block().is_unreachable_or_terminated());
    });
}

#[test]
fn multiple_break_loop() {
    const BREAK_LOOP: &str = r"
    loop {
        if (true) {
            break 32;
        } else if (true) {
            break 64;
        } else {
            break 128;
        }

        break 256; // unreachable
    }";

    bind_as_value(BREAK_LOOP, |binder, value| {
        let register_id = value.into_register().unwrap();

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
    });
}

#[test]
fn expected_lvalue_error() {
    bind_as_value_expect_error("true = false", |_, result, errors| {
        assert!(result.is_err());

        assert!(errors
            .iter()
            .any(|x| { x.as_any().downcast_ref::<ExpectLValue>().is_some() }));
    });
}

#[test]
fn create_temporary_lvalue() {
    bind_as_value("&32", |binder, value| {
        let register_id = value.into_register().unwrap();

        let reference_of = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_borrow()
            .unwrap();

        // this should be an alloca that temporary stores 32 for the reference
        let alloca_id = reference_of
            .address
            .as_memory()
            .unwrap()
            .as_alloca()
            .copied()
            .unwrap();

        // the stored value should be 32
        assert!(binder.current_block().instructions().iter().any(|x| {
            let Instruction::Store(store) = x else {
                return false;
            };

            store.address == Address::Memory(Memory::Alloca(alloca_id))
                && store
                    .value
                    .as_literal()
                    .and_then(|x| x.as_numeric())
                    .map_or(false, |x| {
                        x.integer_string == "32" && x.decimal_stirng.is_none()
                    })
        }));
    })
}
