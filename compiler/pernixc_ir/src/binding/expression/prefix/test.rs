use pernixc_handler::Panic;
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Type},
};

use crate::{
    address::{Address, Memory, Reference},
    binding::{
        diagnostic::{
            CannotDereference, MismatchedQualifierForReferenceOf,
            MismatchedType,
        },
        test::{BindExt, Template},
    },
    instruction::Instruction,
    model::{self, Constraint, Erased},
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

#[test]
fn reference_of() {
    const VARIABLE_DECLARATION: &str = "let mutable x: int32 = 32;";
    const REFERENCE_OF_IMMUTABLE: &str = "&x";
    const REFERENCE_OF_MUTABLE: &str = "&mutable x";

    let template = Template::new();
    let mut binder = template.create_binder();

    let address = {
        let (variable_address, _) = binder
            .bind_variable_declaration(&parse(VARIABLE_DECLARATION), &Panic)
            .unwrap();

        variable_address
    };

    // reference of immutable
    {
        let register_id = binder
            .bind_as_rvalue_success(
                &parse::<syntax_tree::expression::Prefixable>(
                    REFERENCE_OF_IMMUTABLE,
                ),
            )
            .into_register()
            .unwrap();

        let borrow = binder.intermediate_representation.values.registers
            [register_id]
            .assignment
            .as_borrow()
            .unwrap();

        assert_eq!(borrow.qualifier, Qualifier::Immutable);
        assert_eq!(borrow.address, address);
        assert_eq!(borrow.lifetime, Lifetime::Inference(Erased));
    }

    // reference of mutable
    {
        let register_id =
            binder
                .bind_as_rvalue_success(&parse::<
                    syntax_tree::expression::Prefixable,
                >(REFERENCE_OF_MUTABLE))
                .into_register()
                .unwrap();

        let borrow = binder.intermediate_representation.values.registers
            [register_id]
            .assignment
            .as_borrow()
            .unwrap();

        assert_eq!(borrow.qualifier, Qualifier::Mutable);
        assert_eq!(borrow.address, address);
        assert_eq!(borrow.lifetime, Lifetime::Inference(Erased));
    }
}

#[test]
fn reference_of_mutability_error() {
    const VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const REFERENCE_OF_MUTABLE: &str = "&mutable x";

    let template = Template::new();
    let mut binder = template.create_binder();

    let address = {
        let (variable_address, _) = binder
            .bind_variable_declaration(&parse(VARIABLE_DECLARATION), &Panic)
            .unwrap();

        variable_address
    };

    let (value, errors) =
        binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Prefixable,
        >(REFERENCE_OF_MUTABLE));

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<MismatchedQualifierForReferenceOf>()
        .unwrap();

    assert_eq!(error.expected_qualifier, Qualifier::Mutable);
    assert_eq!(error.found_qualifier, Qualifier::Immutable);
    assert!(!error.is_behind_reference);

    let borrow = binder.intermediate_representation.values.registers
        [value.into_register().unwrap()]
    .assignment
    .as_borrow()
    .unwrap();

    assert_eq!(borrow.qualifier, Qualifier::Mutable);
    assert_eq!(borrow.address, address);
}

#[test]
fn dereference() {
    const VALUE_VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const REFERENCE_VARIABLE_DECLARATION: &str = "let y = &x;";
    const DEREFERENCE: &str = "*y";

    let template = Template::new();
    let mut binder = template.create_binder();

    let _ = binder
        .bind_variable_declaration(&parse(VALUE_VARIABLE_DECLARATION), &Panic)
        .unwrap();

    let y_address = {
        let (variable_address, _) = binder
            .bind_variable_declaration(
                &parse(REFERENCE_VARIABLE_DECLARATION),
                &Panic,
            )
            .unwrap();

        variable_address
    };

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Prefixable>(
            DEREFERENCE,
        ))
        .into_register()
        .unwrap();

    let dereference = binder.intermediate_representation.values.registers
        [register_id]
        .assignment
        .as_load()
        .unwrap();

    assert_eq!(
        dereference.address,
        Address::Reference(Reference {
            qualifier: Qualifier::Immutable,
            reference_address: Box::new(y_address)
        })
    );
}

#[test]
fn cannot_dereference_error() {
    const VALUE_VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const DEREFERENCE: &str = "*x";

    let template = Template::new();
    let mut binder = template.create_binder();

    let _ = binder
        .bind_variable_declaration(&parse(VALUE_VARIABLE_DECLARATION), &Panic)
        .unwrap();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Prefixable,
    >(DEREFERENCE));

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<CannotDereference<model::Constrained>>()
        .unwrap();

    assert_eq!(error.found_type, Type::Primitive(Primitive::Int32));
}

#[test]
fn create_temporary_lvalue() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Prefixable>(
            "&32",
        ))
        .into_register()
        .unwrap();

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
    let alloca_id =
        reference_of.address.as_memory().unwrap().as_alloca().copied().unwrap();

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
                .is_some_and(|x| {
                    x.integer_string == "32" && x.decimal_stirng.is_none()
                })
    }));
}
