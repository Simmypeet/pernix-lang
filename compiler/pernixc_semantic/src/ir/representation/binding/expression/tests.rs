use pernixc_base::diagnostic::Storage;

use crate::{
    arena::ID,
    error::{
        DuplicatedFieldInitialization, Error, ExpectedAssociatedValue,
        FieldIsNotAccessible, FieldNotFound,
        FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
        MismatchedArgumentCount, MismatchedMutability,
        MismatchedReferenceQualifier, MismatchedType, UninitializedFields,
    },
    ir::{
        address::{Address, Memory},
        instruction::Instruction,
        register::{
            ArithmeticOperator, BinaryOperator, BitwiseOperator, Register,
            RelationalOperator,
        },
        representation::binding::{
            expression::{Bind, Config, Target},
            infer::{self, ConstraintModel, Erased, NoConstraint},
            tests::{parse_expression, parse_statement, TestTemplate},
            Binder,
        },
    },
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, Index, IndexMut, Insertion,
                RwLockContainer,
            },
            resolution::NoOpObserver,
            Building, Table,
        },
        Accessibility, AdtTemplate, Enum, EnumDefinition, Field, Function,
        FunctionDefinition, FunctionTemplate, GenericDeclaration, GenericID,
        Module, Parameter, Struct, StructDefinition, TypeParameter,
        TypeParameterID, Variant,
    },
    type_system::{
        equality::Equality,
        term::{
            lifetime::Lifetime,
            r#type::{self, Constraint, Primitive, Qualifier, Reference, Type},
            Local,
        },
        Compute,
    },
};

fn create_dummy_function(
) -> (Table<Building<RwLockContainer, Finalizer>>, ID<Function>) {
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
fn numeric_literal_suffix() {
    const SOURCE: &str = r"432u64";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = parse_expression(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_register_id = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_register_id)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert!(numeric_value.decimal_stirng.is_none());
    assert_eq!(numeric_value.integer_string, "432");
    assert_eq!(
        binder.type_of_register(numeric_register_id).unwrap(),
        Type::Primitive(Primitive::Uint64)
    );
}

#[test]
fn numberic_literal_float_infer() {
    const SOURCE: &str = r"32.0";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = parse_expression(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_register_id = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_register_id)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert_eq!(numeric_value.integer_string, "32");
    assert_eq!(
        numeric_value.decimal_stirng.as_ref().map(AsRef::as_ref),
        Some("0")
    );
    let inference_variable = binder
        .type_of_register(numeric_register_id)
        .unwrap()
        .into_inference()
        .unwrap();

    let constraint_id = binder
        .inference_context
        .get_inference(inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&r#type::Constraint::Floating)
    );
}

#[test]
fn numeric_literal_number_infer() {
    const SOURCE: &str = r"32";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = parse_expression(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let numeric_register_id = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_register_id)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert_eq!(numeric_value.integer_string, "32");
    assert!(numeric_value.decimal_stirng.is_none());

    let inference_variable = binder
        .type_of_register(numeric_register_id)
        .unwrap()
        .into_inference()
        .unwrap();

    let constraint_id = binder
        .inference_context
        .get_inference(inference_variable)
        .cloned()
        .unwrap()
        .into_inferring()
        .unwrap();

    assert_eq!(
        binder.inference_context.get_constraint::<Type<_>>(constraint_id),
        Some(&r#type::Constraint::Number)
    );
}

#[test]
fn invalid_numeric_literal_suffix() {
    const SOURCE: &str = r"32goofy";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = parse_expression(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .is_err());

    let mut storage = storage.into_vec();
    assert_eq!(storage.len(), 1);
    let error = storage.pop().unwrap();

    let error = error.as_any().downcast_ref::<InvalidNumericSuffix>().unwrap();
    assert_eq!(error.suffix_span.str(), "goofy");
}

#[test]
fn floating_point_literal_has_integral_suffix() {
    const SOURCE: &str = r"32.0i64";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();
    let expression = parse_expression(SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_numeric()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .is_err());

    let mut storage = storage.into_vec();
    assert_eq!(storage.len(), 1);
    let error = storage.pop().unwrap();

    assert!(error
        .as_any()
        .downcast_ref::<FloatingPointLiteralHasIntegralSuffix>()
        .is_some());
}

#[test]
fn bind_boolean_literal() {
    const TRUE_SOURCE: &str = "true";
    const FALSE_SOURCE: &str = "false";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let true_expression = parse_expression(TRUE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_boolean()
        .unwrap();
    let false_expression = parse_expression(FALSE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_boolean()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let true_value = binder
        .bind(&true_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();
    let false_value = binder
        .bind(&false_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    assert!(
        binder
            .intermediate_representation
            .registers
            .get(true_value)
            .unwrap()
            .assignment
            .as_boolean()
            .unwrap()
            .value
    );

    assert!(
        !binder
            .intermediate_representation
            .registers
            .get(false_value)
            .unwrap()
            .assignment
            .as_boolean()
            .unwrap()
            .value
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn bind_prefix_operator() {
    const LOCAL_SOURCE: &str = "local 32";
    const LOGICAL_NOT_SOURCE: &str = "!false";
    const NEGATE_SOURCE: &str = "-32";
    const BITWISE_NOT_SOURCE: &str = "~32";
    const UNLOCAL_SOURCE: &str = "unlocal local 32";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let local_expression = parse_expression(LOCAL_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let logical_not_expression = parse_expression(LOGICAL_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let negate_expression = parse_expression(NEGATE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let bitwise_not_expression = parse_expression(BITWISE_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let unlocal_expression = parse_expression(UNLOCAL_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    let local = binder
        .bind(&local_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .registers
        .get(local)
        .unwrap()
        .assignment
        .is_prefix());

    let logical_not = binder
        .bind(
            &logical_not_expression,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .registers
        .get(logical_not)
        .unwrap()
        .assignment
        .is_prefix());

    let negate = binder
        .bind(&negate_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .registers
        .get(negate)
        .unwrap()
        .assignment
        .is_prefix());

    let bitwise_not = binder
        .bind(
            &bitwise_not_expression,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .registers
        .get(bitwise_not)
        .unwrap()
        .assignment
        .is_prefix());

    let unlocal = binder
        .bind(&unlocal_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .registers
        .get(unlocal)
        .unwrap()
        .assignment
        .is_prefix());

    assert!(storage.as_vec().is_empty());
}

#[test]
#[allow(clippy::too_many_lines)]
fn prefix_type_mismatched_error() {
    const LOGICAL_NOT_SOURCE: &str = "!64";
    const NEGATE_SOURCE: &str = "-32u32";
    const BITWISE_NOT_SOURCE: &str = "~32.0";
    const UNLOCAL_SOURCE: &str = "unlocal 64";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let logical_not_expression = parse_expression(LOGICAL_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let negate_expression = parse_expression(NEGATE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let bitwise_not_expression = parse_expression(BITWISE_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let unlocal_expression = parse_expression(UNLOCAL_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        NoOpObserver,
        function_id,
        std::iter::empty(),
        false,
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind(
            &logical_not_expression,
            Config { target: Target::Value },
            &storage,
        )
        .is_err());
    assert!(binder
        .bind(&negate_expression, Config { target: Target::Value }, &storage,)
        .is_err());
    assert!(binder
        .bind(
            &bitwise_not_expression,
            Config { target: Target::Value },
            &storage,
        )
        .is_err());
    assert!(binder
        .bind(&unlocal_expression, Config { target: Target::Value }, &storage,)
        .is_err());

    let storage = storage.into_vec();

    // "{{number}} != bool" for !64
    assert!(storage.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<MismatchedType<ConstraintModel>>()
        else {
            return false;
        };

        error.expected_type == Type::Primitive(Primitive::Bool)
            && error
                .found_type
                .as_inference()
                .map_or(false, |constraint| *constraint == Constraint::Number)
    }));

    // "uint32 != {{signed}}" for -32u32
    assert!(storage.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<MismatchedType<ConstraintModel>>()
        else {
            return false;
        };

        error.found_type == Type::Primitive(Primitive::Uint32)
            && error
                .expected_type
                .as_inference()
                .map_or(false, |constraint| *constraint == Constraint::Signed)
    }));

    // "{{floating}} != {{integer}}" for ~32.0
    assert!(storage.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<MismatchedType<ConstraintModel>>()
        else {
            return false;
        };

        error
            .expected_type
            .as_inference()
            .map_or(false, |constraint| *constraint == Constraint::Integer)
            && error
                .found_type
                .as_inference()
                .map_or(false, |constraint| *constraint == Constraint::Floating)
    }));

    // "unlocal 64" for unlocal 64
    assert!(storage.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<MismatchedType<ConstraintModel>>()
        else {
            return false;
        };

        error.expected_type
            == Type::Local(Local(Box::new(Type::Inference(Constraint::All))))
            && error
                .found_type
                .as_inference()
                .map_or(false, |constraint| *constraint == Constraint::Number)
    }));
}

#[test]
fn named_load() {
    const VARIABLE_DECLARATION: &str = "let mutable x: int32 = 32;";
    const VARIABLE_LOAD: &str = "x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let variable_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let alloca_id = binder
        .bind_variable_declaration(&variable_declaration, &storage)
        .unwrap();

    let named_load = parse_expression(VARIABLE_LOAD)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_qualified_identifier()
        .unwrap();

    let register_id = binder
        .bind(&named_load, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let load_alloca_id = *binder
        .intermediate_representation
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_load()
        .unwrap()
        .address
        .as_base()
        .unwrap()
        .as_alloca()
        .unwrap();

    assert_eq!(load_alloca_id, alloca_id);

    let (as_address, _) = binder
        .bind(
            &named_load,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Mutable,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    assert_eq!(*as_address.as_base().unwrap().as_alloca().unwrap(), alloca_id);
}

#[test]
fn named_load_mutability_error() {
    const VARIABLE_DECLARATION: &str = "let x: int32 = 32;";
    const VARIABLE_LOAD: &str = "x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let variable_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let alloca_id = binder
        .bind_variable_declaration(&variable_declaration, &storage)
        .unwrap();

    let named_load = parse_expression(VARIABLE_LOAD)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_qualified_identifier()
        .unwrap();

    let (as_address, _) = binder
        .bind(
            &named_load,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Mutable,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap();

    assert_eq!(
        alloca_id,
        as_address.into_base().unwrap().into_alloca().unwrap()
    );

    let errors = storage.into_vec();
    assert!(errors
        .iter()
        .find_map(|x| x.as_any().downcast_ref::<MismatchedMutability>())
        .is_some());
}

#[test]
fn reference_of() {
    const VARIABLE_DECLARATION: &str = "let mutable x: int32 = 32;";
    const REFERENCE_OF_IMMUTABLE: &str = "&x";
    const REFERENCE_OF_MUTABLE: &str = "&mutable x";
    const REFERENCE_OF_UNIQUE: &str = "&unique x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let variable_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let alloca_id = binder
        .bind_variable_declaration(&variable_declaration, &storage)
        .unwrap();

    let reference_of_immutable = parse_expression(REFERENCE_OF_IMMUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let reference_of_mutable = parse_expression(REFERENCE_OF_MUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let reference_of_unique = parse_expression(REFERENCE_OF_UNIQUE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let reference_of_immutable_id = binder
        .bind(
            &reference_of_immutable,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();
    let reference_of_mutable_id = binder
        .bind(&reference_of_mutable, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();
    let reference_of_unique_id = binder
        .bind(&reference_of_unique, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let assert_reference_of =
        |id: ID<Register<infer::Model>>, qualifier: Qualifier| {
            let reference_of = binder
                .intermediate_representation
                .registers
                .get(id)
                .unwrap()
                .assignment
                .as_reference_of()
                .unwrap();

            assert_eq!(
                *reference_of.address.as_base().unwrap().as_alloca().unwrap(),
                alloca_id
            );
            assert!(!reference_of.is_local);
            assert_eq!(reference_of.qualifier, qualifier);
        };

    assert_reference_of(reference_of_immutable_id, Qualifier::Immutable);
    assert_reference_of(reference_of_mutable_id, Qualifier::Mutable);
    assert_reference_of(reference_of_unique_id, Qualifier::Unique);
}

#[test]
fn reference_of_local() {
    const LOCAL_DECLARATION: &str = "let mutable x = local 6420i32;";
    const REFERENCE_OF_IMMUTABLE: &str = "@x";
    const REFERENCE_OF_MUTABLE: &str = "@mutable x";
    const REFERENCE_OF_UNIQUE: &str = "@unique x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let local_declaration =
        parse_statement(LOCAL_DECLARATION).into_variable_declaration().unwrap();

    let alloca_id =
        binder.bind_variable_declaration(&local_declaration, &storage).unwrap();

    let reference_of_immutable = parse_expression(REFERENCE_OF_IMMUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let reference_of_mutable = parse_expression(REFERENCE_OF_MUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();
    let reference_of_unique = parse_expression(REFERENCE_OF_UNIQUE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let reference_of_immutable_id = binder
        .bind(
            &reference_of_immutable,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();
    let reference_of_mutable_id = binder
        .bind(&reference_of_mutable, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();
    let reference_of_unique_id = binder
        .bind(&reference_of_unique, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let assert_reference_of =
        |id: ID<Register<infer::Model>>, qualifier: Qualifier| {
            let register =
                binder.intermediate_representation.registers.get(id).unwrap();

            assert!(Equality::new(
                binder.type_of_register(id).unwrap(),
                Type::Reference(Reference {
                    qualifier,
                    lifetime: Lifetime::Inference(Erased),
                    pointee: Box::new(Type::Primitive(Primitive::Int32)),
                }),
            )
            .query(&binder.create_environment())
            .unwrap()
            .is_some());

            let reference_of = register.assignment.as_reference_of().unwrap();

            assert_eq!(
                *reference_of.address.as_base().unwrap().as_alloca().unwrap(),
                alloca_id
            );
            assert!(reference_of.is_local);
            assert_eq!(reference_of.qualifier, qualifier);
        };

    assert_reference_of(reference_of_immutable_id, Qualifier::Immutable);
    assert_reference_of(reference_of_mutable_id, Qualifier::Mutable);
    assert_reference_of(reference_of_unique_id, Qualifier::Unique);
}

#[test]
fn reference_of_mutability_error() {
    const VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const REFERENCE_OF_MUTABLE: &str = "&mutable x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let variable_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let _ = binder.bind_variable_declaration(&variable_declaration, &storage);

    let reference_of_mutable = parse_expression(REFERENCE_OF_MUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    assert!(binder
        .bind(&reference_of_mutable, Config { target: Target::Value }, &storage)
        .is_ok());

    let errors = storage.into_vec();

    assert!(errors
        .iter()
        .find_map(|x| x.as_any().downcast_ref::<MismatchedMutability>())
        .is_some());
}

#[test]
fn reference_of_local_error() {
    const VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const REFERENCE_OF_MUTABLE: &str = "@x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let local_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let _ = binder.bind_variable_declaration(&local_declaration, &storage);

    let reference_of_mutable = parse_expression(REFERENCE_OF_MUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    assert!(binder
        .bind(&reference_of_mutable, Config { target: Target::Value }, &storage)
        .is_err());

    let errors = storage.into_vec();

    assert!(errors.iter().any(|x| {
        let Some(error) =
            x.as_any().downcast_ref::<MismatchedType<infer::ConstraintModel>>()
        else {
            return false;
        };

        error.expected_type
            == Type::Local(Local(Box::new(Type::Inference(Constraint::All))))
            && error.found_type == Type::Primitive(Primitive::Int32)
    }));
}

#[test]
fn dereference_as_value() {
    const VALUE_VARIABLE_DECLARATION: &str = "let x = 6420i32;";
    const REFERENCE_VARIABLE_DECLARATION: &str = "let y = &x;";
    const DEREFERENCE: &str = "*y";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let value_variable_declaration =
        parse_statement(VALUE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let _ =
        binder.bind_variable_declaration(&value_variable_declaration, &storage);

    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let reference_alloca_id = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage)
        .unwrap();

    let dereference = parse_expression(DEREFERENCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let register_id = binder
        .bind(&dereference, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let dereference_register =
        binder.intermediate_representation.registers.get(register_id).unwrap();

    let dereference = dereference_register.assignment.as_load().unwrap();

    let reference_value =
        dereference.address.as_base().unwrap().into_reference_value().unwrap();

    let name_load_register = binder
        .intermediate_representation
        .registers
        .get(reference_value)
        .unwrap();

    assert_eq!(
        *name_load_register
            .assignment
            .as_load()
            .unwrap()
            .address
            .as_base()
            .unwrap()
            .as_alloca()
            .unwrap(),
        reference_alloca_id
    );
}

#[test]
fn dereference_as_address() {
    const VALUE_VARIABLE_DECLARATION: &str = "let mutable x = 6420i32;";
    const REFERENCE_VARIABLE_DECLARATION: &str = "let y = &unique x;";
    const DEREFERENCE: &str = "*y";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let value_variable_declaration =
        parse_statement(VALUE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let _ =
        binder.bind_variable_declaration(&value_variable_declaration, &storage);

    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let reference_alloca_id = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage)
        .unwrap();

    let dereference = parse_expression(DEREFERENCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let (address, _) = binder
        .bind(
            &dereference,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Unique,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let reference_id =
        *address.as_base().unwrap().as_reference_value().unwrap();

    assert_eq!(
        *binder
            .intermediate_representation
            .registers
            .get(reference_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap()
            .address
            .as_base()
            .unwrap()
            .as_alloca()
            .unwrap(),
        reference_alloca_id
    );
}

#[test]
fn dereference_mismatched_qualifier_error() {
    const VALUE_VARIABLE_DECLARATION: &str = "let mutable x = 6420i32;";
    const REFERENCE_VARIABLE_DECLARATION: &str = "let y = &mutable x;";
    const DEREFERENCE: &str = "*y";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let value_variable_declaration =
        parse_statement(VALUE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let _ =
        binder.bind_variable_declaration(&value_variable_declaration, &storage);
    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let _ = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage);

    let dereference = parse_expression(DEREFERENCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefix()
        .unwrap();

    let _ = binder
        .bind(
            &dereference,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Unique,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap();

    let errors = storage.into_vec();

    assert!(errors.iter().any(|predicate| {
        let Some(error) = predicate
                    .as_any()
                    .downcast_ref::<MismatchedReferenceQualifier<
                    infer::ConstraintModel,
                >>() else {
                    return false;
                };

        error.expected_qualifier == Qualifier::Unique
            && error.found_reference_type
                == Type::Reference(Reference {
                    qualifier: Qualifier::Mutable,
                    lifetime: Lifetime::Inference(NoConstraint),
                    pointee: Box::new(Type::Primitive(Primitive::Int32)),
                })
    }));
}

impl TestTemplate {
    fn create_struct_sample(
        &mut self,
        parent_module_id: ID<Module>,
        x_accessibility: Accessibility,
        y_accessibility: Accessibility,
    ) -> (ID<Struct>, ID<Field> /* x field */, ID<Field> /* y field */) {
        let Insertion { id: struct_id, duplication } = self
            .table
            .insert_member(
                "Vector2".to_string(),
                Accessibility::Public,
                parent_module_id,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let (x_field_id, y_field_id) = {
            let struct_sym = self.table.get_mut(struct_id).unwrap();

            let x_field_id = struct_sym
                .insert_field(Field {
                    accessibility: x_accessibility,
                    name: "x".to_string(),
                    r#type: Type::Primitive(Primitive::Float32),
                    span: None,
                })
                .unwrap();

            let y_field_id = struct_sym
                .insert_field(Field {
                    accessibility: y_accessibility,
                    name: "y".to_string(),
                    r#type: Type::Primitive(Primitive::Float32),
                    span: None,
                })
                .unwrap();

            (x_field_id, y_field_id)
        };

        (struct_id, x_field_id, y_field_id)
    }
}

#[test]
fn struct_expression() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, y: 64 }";

    let mut test_template = TestTemplate::new();

    let (_, x_field_id, y_field_id) = test_template.create_struct_sample(
        test_template.test_module_id,
        Accessibility::Public,
        Accessibility::Public,
    );
    let (mut binder, storage) = test_template.create_binder();

    let struct_expression = parse_expression(STRUCT_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_struct()
        .unwrap();

    let register_id = binder
        .bind(&struct_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let assert_struct_initializer =
        |field_id: ID<Field>, expected_string: &str| {
            let register = binder
                .intermediate_representation
                .registers
                .get(register_id)
                .unwrap();
            let struct_initializer = register.assignment.as_struct().unwrap();

            let initializer = *struct_initializer
                .initializers_by_field_id
                .get(&field_id)
                .unwrap();

            let numeric_register = binder
                .intermediate_representation
                .registers
                .get(initializer)
                .unwrap();

            let numeric_assignment =
                numeric_register.assignment.as_numeric().unwrap();

            assert_eq!(numeric_assignment.integer_string, expected_string);
        };

    assert_struct_initializer(x_field_id, "32");
    assert_struct_initializer(y_field_id, "64");
}

#[test]
fn struct_uninitialized_field_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32 }";

    let mut test_template = TestTemplate::new();
    let (_, _, y_field_id) = test_template.create_struct_sample(
        test_template.test_module_id,
        Accessibility::Public,
        Accessibility::Public,
    );

    let (mut binder, storage) = test_template.create_binder();

    let struct_expression = parse_expression(STRUCT_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_struct()
        .unwrap();

    let _ = binder
        .bind(&struct_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let errors = storage.into_vec();

    assert!(errors.iter().any(|predicate| {
        let Some(error) =
            predicate.as_any().downcast_ref::<UninitializedFields>()
        else {
            return false;
        };

        error.uninitialized_fields.len() == 1
            && error.uninitialized_fields.contains(&y_field_id)
    }));
}

#[test]
fn struct_duplicated_initialization_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, x: 64, y: 24 }";

    let mut test_template = TestTemplate::new();
    let (struct_id, x_field_id, _) = test_template.create_struct_sample(
        test_template.test_module_id,
        Accessibility::Public,
        Accessibility::Public,
    );

    let (mut binder, storage) = test_template.create_binder();

    let struct_expression = parse_expression(STRUCT_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_struct()
        .unwrap();

    let _ = binder
        .bind(&struct_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let errors = storage.into_vec();

    assert!(errors.iter().any(|predicate| {
        let Some(error) =
            predicate.as_any().downcast_ref::<DuplicatedFieldInitialization>()
        else {
            return false;
        };

        error.struct_id == struct_id && error.field_id == x_field_id
    }));
}

#[test]
fn struct_unknown_field_error() {
    const STRUCT_EXPRESSION: &str = "Vector2 { x: 32, y: 24, z: 64 }";

    let mut test_template = TestTemplate::new();
    let (struct_id, _, _) = test_template.create_struct_sample(
        test_template.test_module_id,
        Accessibility::Public,
        Accessibility::Public,
    );

    let (mut binder, storage) = test_template.create_binder();

    let struct_expression = parse_expression(STRUCT_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_struct()
        .unwrap();

    let _ = binder
        .bind(&struct_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let errors = storage.into_vec();

    assert!(errors.iter().any(|predicate| {
        let Some(error) = predicate.as_any().downcast_ref::<FieldNotFound>()
        else {
            return false;
        };

        error.struct_id == struct_id && error.identifier_span.str() == "z"
    }));
}

#[test]
fn struct_field_is_not_accessible_error() {
    const STRUCT_EXPRESSION: &str = "inner::Vector2 { x: 32, y: 24 }";

    let mut test_template = TestTemplate::new();
    let Insertion { id: inner_module_id, duplication } = test_template
        .table
        .insert_module(
            "inner".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
        )
        .unwrap();

    let (struct_id, x_field_id, _) = test_template.create_struct_sample(
        inner_module_id,
        Accessibility::Scoped(inner_module_id),
        Accessibility::Public,
    );

    assert!(duplication.is_none());

    let (mut binder, storage) = test_template.create_binder();

    let struct_expression = parse_expression(STRUCT_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_struct()
        .unwrap();

    let _ = binder
        .bind(&struct_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let errors = storage.into_vec();

    assert!(errors.iter().any(|predicate| {
        let Some(error) =
            predicate.as_any().downcast_ref::<FieldIsNotAccessible>()
        else {
            return false;
        };

        error.struct_id == struct_id && error.field_id == x_field_id
    }));
}

impl TestTemplate {
    /*
    public enum Sample[F, S] {
        First(F),
        Second(S),
        Third(bool),
        Fourth
    }
     */
    fn crate_enum_template(
        &mut self,
        parent_module_id: ID<Module>,
    ) -> (
        ID<Enum>,
        ID<Variant>, /* First */
        ID<Variant>, /* Second */
        ID<Variant>, /* Third */
        ID<Variant>, /* Fourth */
    ) {
        let Insertion { id: enum_id, duplication } = self
            .table
            .insert_member(
                "Sample".to_string(),
                Accessibility::Public,
                parent_module_id,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<EnumDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let f_ty_param = self
            .table
            .get_mut(enum_id)
            .unwrap()
            .generic_declaration
            .parameters
            .add_type_parameter(TypeParameter {
                name: Some("F".to_string()),
                span: None,
            })
            .unwrap();

        let s_ty_param = self
            .table
            .get_mut(enum_id)
            .unwrap()
            .generic_declaration
            .parameters
            .add_type_parameter(TypeParameter {
                name: Some("S".to_string()),
                span: None,
            })
            .unwrap();

        let (first_id, second_id, third_id, fourth_id) = {
            let first_id = self
                .table
                .insert_variant(
                    "First".to_string(),
                    enum_id,
                    Some(Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(enum_id),
                        id: f_ty_param,
                    })),
                    None,
                )
                .unwrap()
                .unwrap_no_duplication();

            let second_id = self
                .table
                .insert_variant(
                    "Second".to_string(),
                    enum_id,
                    Some(Type::Parameter(TypeParameterID {
                        parent: GenericID::Enum(enum_id),
                        id: s_ty_param,
                    })),
                    None,
                )
                .unwrap()
                .unwrap_no_duplication();

            let third_id = self
                .table
                .insert_variant(
                    "Third".to_string(),
                    enum_id,
                    Some(Type::Primitive(Primitive::Bool)),
                    None,
                )
                .unwrap()
                .unwrap_no_duplication();

            let fourth_id = self
                .table
                .insert_variant("Fourth".to_string(), enum_id, None, None)
                .unwrap()
                .unwrap_no_duplication();

            (first_id, second_id, third_id, fourth_id)
        };

        (enum_id, first_id, second_id, third_id, fourth_id)
    }
}

#[test]
fn variant_call() {
    const FIRST_ENUM_EXPRESSION: &str = "inner::Sample::First(32)";
    const FOURTH_ENUM_EXPRESSION: &str = "inner::Sample::Fourth()";

    let mut test_template = TestTemplate::new();
    let module_id = test_template
        .table
        .insert_module(
            "inner".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
        )
        .unwrap()
        .unwrap_no_duplication();

    let (_, first_id, _, _, fourth_id) =
        test_template.crate_enum_template(module_id);

    let (mut binder, storage) = test_template.create_binder();

    let first_variant_call_expression = parse_expression(FIRST_ENUM_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();
    let fourth_variant_call_expression =
        parse_expression(FOURTH_ENUM_EXPRESSION)
            .into_binary()
            .unwrap()
            .destruct()
            .0
            .into_postfixable()
            .unwrap()
            .into_postfix()
            .unwrap();

    let first_register_id = binder
        .bind(
            &first_variant_call_expression,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let first_assignment = binder
        .intermediate_representation
        .registers
        .get(first_register_id)
        .unwrap()
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(first_assignment.variant_id, first_id);

    let associated_value = *first_assignment.associated_value.as_ref().unwrap();

    let numeric_register = binder
        .intermediate_representation
        .registers
        .get(associated_value)
        .unwrap();

    let numeric_assignment = numeric_register.assignment.as_numeric().unwrap();

    assert_eq!(numeric_assignment.integer_string, "32");
    assert_eq!(numeric_assignment.decimal_stirng, None);

    let fourth_register_id = binder
        .bind(
            &fourth_variant_call_expression,
            Config { target: Target::Value },
            &storage,
        )
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let fourth_assignment = binder
        .intermediate_representation
        .registers
        .get(fourth_register_id)
        .unwrap()
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(fourth_assignment.variant_id, fourth_id);
    assert!(fourth_assignment.associated_value.is_none());
}

#[test]
fn variant_call_mismatched_argument_count_error() {
    const FIRST_EXPRESSION: &str = "inner::Sample::First(32, 64)";
    const SECOND_EXPRESSION: &str = "inner::Sample::Second()";
    const FOURTH_EXPRESSION: &str = "inner::Sample::Fourth(32)";

    let mut test_template = TestTemplate::new();
    let module_id = test_template
        .table
        .insert_module(
            "inner".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
        )
        .unwrap()
        .unwrap_no_duplication();

    let (_, first_id, second_id, _, fourth_id) =
        test_template.crate_enum_template(module_id);

    let (mut binder, storage) = test_template.create_binder();

    let first_expression = parse_expression(FIRST_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    let _ = binder
        .bind(&first_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert_eq!(storage.as_vec().len(), 1);

    assert!(storage.as_vec().iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<MismatchedArgumentCount>()
        else {
            return false;
        };

        error.expected_count == 1
            && error.found_count == 2
            && error.called_id == first_id.into()
    }));

    let second_expression = parse_expression(SECOND_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    storage.clear();

    let _ = binder
        .bind(&second_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert_eq!(storage.as_vec().len(), 1);

    assert!(storage.as_vec().iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<MismatchedArgumentCount>()
        else {
            return false;
        };

        error.expected_count == 1
            && error.found_count == 0
            && error.called_id == second_id.into()
    }));

    let fourth_expression = parse_expression(FOURTH_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    storage.clear();

    let _ = binder
        .bind(&fourth_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert_eq!(storage.as_vec().len(), 1);

    assert!(storage.as_vec().iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<MismatchedArgumentCount>()
        else {
            return false;
        };

        error.expected_count == 0
            && error.found_count == 1
            && error.called_id == fourth_id.into()
    }));
}

#[test]
fn qualified_identifier_variant() {
    const FOURTH_EXPRESSION: &str = "inner::Sample::Fourth";

    let mut test_template = TestTemplate::new();
    let module_id = test_template
        .table
        .insert_module(
            "inner".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
        )
        .unwrap()
        .unwrap_no_duplication();

    let (_, _, _, _, fourth_id) = test_template.crate_enum_template(module_id);

    let (mut binder, storage) = test_template.create_binder();

    let fourth_expression = parse_expression(FOURTH_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_qualified_identifier()
        .unwrap();

    // no check
    let fourth_register = binder
        .bind(&fourth_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let fourth_assignment = binder
        .intermediate_representation
        .registers
        .get(fourth_register)
        .unwrap()
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(fourth_assignment.variant_id, fourth_id);
    assert!(fourth_assignment.associated_value.is_none());
}

#[test]
fn qualified_identifier_variant_expected_associated_value_error() {
    const FIRST_EXPRESSION: &str = "inner::Sample::First";

    let mut test_template = TestTemplate::new();
    let module_id = test_template
        .table
        .insert_module(
            "inner".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
        )
        .unwrap()
        .unwrap_no_duplication();

    let (_, first_id, _, _, _) = test_template.crate_enum_template(module_id);

    let (mut binder, storage) = test_template.create_binder();

    let first_expression = parse_expression(FIRST_EXPRESSION)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_qualified_identifier()
        .unwrap();

    // no check
    let _ = binder
        .bind(&first_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    let errors = storage.into_vec();

    assert_eq!(errors.len(), 1);

    assert!(errors.iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<ExpectedAssociatedValue>()
        else {
            return false;
        };

        error.variant_id == first_id
    }));
}

impl TestTemplate {
    /*
    public function test[T](a: T, b: T, c: int32): T {}
     */
    fn create_function_template(
        &mut self,
        parent_module_id: ID<Module>,
    ) -> ID<Function> {
        let function_id = self
            .table
            .insert_member(
                "test".to_string(),
                Accessibility::Public,
                parent_module_id,
                None,
                GenericDeclaration::default(),
                FunctionTemplate::<FunctionDefinition>::default(),
            )
            .unwrap()
            .unwrap_no_duplication();

        let t_ty_param = self
            .table
            .get_mut(function_id)
            .unwrap()
            .generic_declaration
            .parameters
            .add_type_parameter(TypeParameter {
                name: Some("T".to_string()),
                span: None,
            })
            .unwrap();

        // a
        self.table.get_mut(function_id).unwrap().insert_parameter(Parameter {
            r#type: Type::Parameter(TypeParameterID {
                parent: GenericID::Function(function_id),
                id: t_ty_param,
            }),
            span: None,
        });

        // b
        self.table.get_mut(function_id).unwrap().insert_parameter(Parameter {
            r#type: Type::Parameter(TypeParameterID {
                parent: GenericID::Function(function_id),
                id: t_ty_param,
            }),
            span: None,
        });

        // c
        self.table.get_mut(function_id).unwrap().insert_parameter(Parameter {
            r#type: Type::Primitive(Primitive::Int32),
            span: None,
        });

        function_id
    }
}

#[test]
fn function_call() {
    const FUNCTION_CALL: &str = "sample::test(true, false, 32)";

    let mut test_template = TestTemplate::new();
    let module_id = test_template
        .table
        .create_root_module("sample".to_string())
        .unwrap_no_duplication();

    let function_id = test_template.create_function_template(module_id);
    let (mut binder, storage) = test_template.create_binder();

    let call_expression = parse_expression(FUNCTION_CALL)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    let call_register = binder
        .bind(&call_expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let function_call_assignment = binder
        .intermediate_representation
        .registers
        .get(call_register)
        .unwrap()
        .assignment
        .as_function_call()
        .unwrap();

    let function_t_ty_param = binder
        .table
        .get(function_id)
        .unwrap()
        .generic_declaration
        .parameters
        .type_parameter_ids_by_name()
        .get("T")
        .copied()
        .unwrap();

    assert_eq!(function_call_assignment.callable_id, function_id.into());
    assert_eq!(function_call_assignment.arguments.len(), 3);

    // check if the generic arguments instantiated correctly
    {
        assert!(function_call_assignment.instantiation.lifetimes.is_empty());
        assert_eq!(function_call_assignment.instantiation.types.len(), 1);
        assert!(function_call_assignment.instantiation.constants.is_empty());

        let ty_param = Type::Parameter(TypeParameterID {
            parent: function_id.into(),
            id: function_t_ty_param,
        });
        let instantiated = function_call_assignment
            .instantiation
            .types
            .get(&ty_param)
            .unwrap()
            .clone();

        let environment = binder.create_environment();

        assert!(Equality::new(instantiated, Type::Primitive(Primitive::Bool))
            .query(&environment)
            .unwrap()
            .is_some());
    }

    // the first arguments is a boolean true
    {
        let first_argument =
            *function_call_assignment.arguments.get(0).unwrap();

        let first_register = binder
            .intermediate_representation
            .registers
            .get(first_argument)
            .unwrap();

        let first_assignment = first_register.assignment.as_boolean().unwrap();

        assert_eq!(first_assignment.value, true);
    }

    // the second arguments is a boolean false
    {
        let second_argument =
            *function_call_assignment.arguments.get(1).unwrap();

        let second_register = binder
            .intermediate_representation
            .registers
            .get(second_argument)
            .unwrap();

        let second_assignment =
            second_register.assignment.as_boolean().unwrap();

        assert_eq!(second_assignment.value, false);
    }

    // the third arguments is an integer 32
    {
        let third_argument =
            *function_call_assignment.arguments.get(2).unwrap();

        let third_register = binder
            .intermediate_representation
            .registers
            .get(third_argument)
            .unwrap();

        let third_assignment = third_register.assignment.as_numeric().unwrap();

        assert_eq!(third_assignment.integer_string, "32");
    }
}

#[test]
fn function_call_mismatched_argument_count_error() {
    const TOO_MANY_ARGUMENTS: &str = "sample::test(true, false, 32, 64)";
    const TOO_FEW_ARGUMENTS: &str = "sample::test(true, false)";

    let mut test_template = TestTemplate::new();

    let module_id = test_template
        .table
        .create_root_module("sample".to_string())
        .unwrap_no_duplication();

    let function_id = test_template.create_function_template(module_id);
    let (mut binder, storage) = test_template.create_binder();

    let too_many_arguments = parse_expression(TOO_MANY_ARGUMENTS)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    let too_few_arguments = parse_expression(TOO_FEW_ARGUMENTS)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    let _ = binder
        .bind(&too_many_arguments, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert_eq!(storage.as_vec().len(), 1);

    assert!(storage.as_vec().iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<MismatchedArgumentCount>()
        else {
            return false;
        };

        error.expected_count == 3
            && error.found_count == 4
            && error.called_id == function_id.into()
    }));

    storage.clear();

    let _ = binder
        .bind(&too_few_arguments, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert_eq!(storage.as_vec().len(), 1);

    assert!(storage.as_vec().iter().any(|x| {
        let Some(error) = x.as_any().downcast_ref::<MismatchedArgumentCount>()
        else {
            return false;
        };

        error.expected_count == 3
            && error.found_count == 2
            && error.called_id == function_id.into()
    }));
}

#[test]
fn assignment() {
    const DECLARATION: &str = "let mutable x = 32;";
    const ASSIGNMENT: &str = "x = 64";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let declaration =
        parse_statement(DECLARATION).into_variable_declaration().unwrap();

    let alloca_id =
        binder.bind_variable_declaration(&declaration, &storage).unwrap();

    assert!(storage.as_vec().is_empty());

    let assignment = parse_expression(ASSIGNMENT).into_binary().unwrap();

    let found_address = binder
        .bind(
            &assignment,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Immutable,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap()
        .0;

    assert!(storage.as_vec().is_empty());

    let expected_address = Address::Base(Memory::Alloca(alloca_id));

    assert_eq!(found_address, expected_address);

    assert!(binder.current_block().instructions().iter().any(|x| {
        let Instruction::Store(store) = x else {
            return false;
        };

        let correct_address = store.address == expected_address;
        let correct_kind = !store.is_initializattion;

        let register = binder
            .intermediate_representation
            .registers
            .get(store.value)
            .unwrap()
            .assignment
            .as_numeric()
            .unwrap();

        correct_address
            && correct_kind
            && register.integer_string == "64"
            && register.decimal_stirng.is_none()
    }));

    // bind as value
    let register_id = binder
        .bind(&assignment, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_load()
        .unwrap();

    assert_eq!(register.address, expected_address);
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
            .bind(&arithmetic, Config { target: Target::Value }, &storage)
            .unwrap()
            .into_value()
            .unwrap();

        assert!(storage.as_vec().is_empty());

        let register = binder
            .intermediate_representation
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_binary()
            .unwrap();

        assert_eq!(register.operator, operator);

        let number_one = binder
            .intermediate_representation
            .registers
            .get(register.lhs)
            .unwrap()
            .assignment
            .as_numeric()
            .unwrap();

        assert_eq!(number_one.integer_string, lhs_str);
        assert_eq!(number_one.decimal_stirng, None);

        let number_two = binder
            .intermediate_representation
            .registers
            .get(register.rhs)
            .unwrap()
            .assignment
            .as_numeric()
            .unwrap();

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

    let alloca_id =
        binder.bind_variable_declaration(&declaration, &storage).unwrap();

    assert!(storage.as_vec().is_empty());

    let assignment = parse_expression(ASSIGNMENT).into_binary().unwrap();

    let found_address = binder
        .bind(
            &assignment,
            Config {
                target: Target::Address {
                    expected_qualifier: Qualifier::Immutable,
                },
            },
            &storage,
        )
        .unwrap()
        .into_address()
        .unwrap()
        .0;

    assert!(storage.as_vec().is_empty());

    let expected_address = Address::Base(Memory::Alloca(alloca_id));

    assert_eq!(found_address, expected_address);

    dbg!(&binder.intermediate_representation);

    assert!(binder.current_block().instructions().iter().any(|x| {
        let Instruction::Store(store) = x else {
            return false;
        };

        if store.address != expected_address || store.is_initializattion {
            return false;
        }

        let Some(binary) = binder
            .intermediate_representation
            .registers
            .get(store.value)
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

        let Some(load) = binder
            .intermediate_representation
            .registers
            .get(binary.lhs)
            .unwrap()
            .assignment
            .as_load()
        else {
            return false;
        };

        if load.address != expected_address {
            return false;
        }

        let Some(numeric) = binder
            .intermediate_representation
            .registers
            .get(binary.rhs)
            .unwrap()
            .assignment
            .as_numeric()
        else {
            return false;
        };

        dbg!(numeric.integer_string == "64" && numeric.decimal_stirng.is_none())
    }));
}

#[test]
fn binary_operator_precedence() {
    const EXPRESSION: &str = "1 + 2 * 3";

    let test_template = TestTemplate::new();

    let (mut binder, storage) = test_template.create_binder();

    let expression = parse_expression(EXPRESSION).into_binary().unwrap();

    let mul_op = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let add_register = binder
        .intermediate_representation
        .registers
        .get(mul_op)
        .unwrap()
        .assignment
        .as_binary()
        .unwrap();

    assert_eq!(
        add_register.operator,
        BinaryOperator::Arithmetic(ArithmeticOperator::Add)
    );

    let num_one = binder
        .intermediate_representation
        .registers
        .get(add_register.lhs)
        .unwrap()
        .assignment
        .as_numeric()
        .unwrap();

    assert_eq!(num_one.integer_string, "1");
    assert!(num_one.decimal_stirng.is_none());

    let mul_register = binder
        .intermediate_representation
        .registers
        .get(add_register.rhs)
        .unwrap()
        .assignment
        .as_binary()
        .unwrap();

    assert_eq!(
        mul_register.operator,
        BinaryOperator::Arithmetic(ArithmeticOperator::Multiply)
    );

    let num_two = binder
        .intermediate_representation
        .registers
        .get(mul_register.lhs)
        .unwrap()
        .assignment
        .as_numeric()
        .unwrap();

    assert_eq!(num_two.integer_string, "2");
    assert!(num_two.decimal_stirng.is_none());

    let num_three = binder
        .intermediate_representation
        .registers
        .get(mul_register.rhs)
        .unwrap()
        .assignment
        .as_numeric()
        .unwrap();

    assert_eq!(num_three.integer_string, "3");
    assert!(num_three.decimal_stirng.is_none());
}
