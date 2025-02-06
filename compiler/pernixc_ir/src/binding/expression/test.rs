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
