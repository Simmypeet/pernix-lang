use pernixc_base::diagnostic::Storage;

use crate::{
    arena::ID,
    error::{
        Error, FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
        MismatchedMutability, MismatchedReferenceQualifier, MismatchedType,
    },
    ir::{
        register::Register,
        representation::binding::{
            expression::{Bind, Config, Target},
            infer::{self, ConstraintModel, Erased, NoConstraint},
            tests::{parse_expression, parse_statement, TestTemplate},
            Binder,
        },
    },
    semantic::{
        equality::equals,
        term::{
            lifetime::Lifetime,
            r#type::{self, Constraint, Primitive, Qualifier, Reference, Type},
            Local,
        },
    },
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, Insertion, RwLockContainer,
            },
            resolution::NoOpObserver,
            Building, Table,
        },
        Accessibility, Function, FunctionDefinition, FunctionTemplate,
        GenericDeclaration,
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
fn bind_numeric_literal_suffix() {
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

    let numeric_value = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_value)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert!(numeric_value.decimal_stirng.is_none());
    assert_eq!(numeric_value.integer_string, "432");
    assert_eq!(register.r#type, Type::Primitive(Primitive::Uint64));
}

#[test]
fn bind_numeric_literal_float_infer() {
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

    let numeric_value = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_value)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert_eq!(numeric_value.integer_string, "32");
    assert_eq!(
        numeric_value.decimal_stirng.as_ref().map(AsRef::as_ref),
        Some("0")
    );

    let inference_variable = register.r#type.as_inference().unwrap();
    let constraint_id = binder
        .inference_context
        .get_inference(*inference_variable)
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
fn bind_numeric_literal_number_infer() {
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

    let numeric_value = binder
        .bind(&expression, Config { target: Target::Value }, &storage)
        .unwrap()
        .into_value()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let register = binder
        .intermediate_representation
        .registers
        .get(numeric_value)
        .unwrap();
    let numeric_value = register.assignment.as_numeric().unwrap();

    assert_eq!(numeric_value.integer_string, "32");
    assert!(numeric_value.decimal_stirng.is_none());

    let inference_variable = register.r#type.as_inference().unwrap();
    let constraint_id = binder
        .inference_context
        .get_inference(*inference_variable)
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
fn bind_prefix_type_mismatched() {
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

    let alloca_id =
        binder.bind_variable_declaration(&variable_declaration, &storage);

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

    let alloca_id =
        binder.bind_variable_declaration(&variable_declaration, &storage);

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

    let alloca_id =
        binder.bind_variable_declaration(&variable_declaration, &storage);

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
        binder.bind_variable_declaration(&local_declaration, &storage);

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

            assert!(equals(
                &register.r#type,
                &Type::Reference(Reference {
                    qualifier,
                    lifetime: Lifetime::Inference(Erased),
                    pointee: Box::new(Type::Primitive(Primitive::Int32))
                }),
                &binder.create_environment()
            )
            .unwrap());

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

    binder.bind_variable_declaration(&value_variable_declaration, &storage);

    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let reference_alloca_id = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage);

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

    binder.bind_variable_declaration(&value_variable_declaration, &storage);

    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    let reference_alloca_id = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage);

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
fn dereference_mismatched_qualifier() {
    const VALUE_VARIABLE_DECLARATION: &str = "let mutable x = 6420i32;";
    const REFERENCE_VARIABLE_DECLARATION: &str = "let y = &mutable x;";
    const DEREFERENCE: &str = "*y";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let value_variable_declaration =
        parse_statement(VALUE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    binder.bind_variable_declaration(&value_variable_declaration, &storage);
    let reference_variable_declaration =
        parse_statement(REFERENCE_VARIABLE_DECLARATION)
            .into_variable_declaration()
            .unwrap();

    binder.bind_variable_declaration(&reference_variable_declaration, &storage);

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
