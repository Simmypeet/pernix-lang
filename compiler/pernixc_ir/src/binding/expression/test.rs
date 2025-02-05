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
#[allow(clippy::too_many_lines)]
fn bind_prefix_operator() {
    const LOGICAL_NOT_SOURCE: &str = "!false";
    const NEGATE_SOURCE: &str = "-32";
    const BITWISE_NOT_SOURCE: &str = "~32";

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let logical_not_expression = parse_expression(LOGICAL_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();
    let negate_expression = parse_expression(NEGATE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();
    let bitwise_not_expression = parse_expression(BITWISE_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let logical_not = binder
        .bind(
            &logical_not_expression,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .values
        .registers
        .get(logical_not)
        .unwrap()
        .assignment
        .is_prefix());

    let negate = binder
        .bind(&negate_expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .values
        .registers
        .get(negate)
        .unwrap()
        .assignment
        .is_prefix());

    let bitwise_not = binder
        .bind(
            &bitwise_not_expression,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(binder
        .intermediate_representation
        .values
        .registers
        .get(bitwise_not)
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

    let (table, function_id) = create_dummy_function();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let logical_not_expression = parse_expression(LOGICAL_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();
    let negate_expression = parse_expression(NEGATE_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();
    let bitwise_not_expression = parse_expression(BITWISE_NOT_SOURCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    assert!(binder
        .bind(
            &logical_not_expression,
            Config { target: Target::RValue },
            &storage,
        )
        .is_err());
    assert!(binder
        .bind(&negate_expression, Config { target: Target::RValue }, &storage,)
        .is_err());
    assert!(binder
        .bind(
            &bitwise_not_expression,
            Config { target: Target::RValue },
            &storage,
        )
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

    let (variable_address, _) = binder
        .bind_variable_declaration(&variable_declaration, &storage)
        .unwrap();

    let named_load = parse_expression(VARIABLE_LOAD)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_postfixable()
        .unwrap()
        .into_unit()
        .unwrap()
        .into_qualified_identifier()
        .unwrap();

    let register_id = binder
        .bind(&named_load, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert_eq!(
        binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap()
            .address,
        variable_address
    );

    let as_address = binder
        .bind(&named_load, Config { target: Target::LValue }, &storage)
        .unwrap()
        .into_l_value()
        .unwrap()
        .address;

    assert!(storage.as_vec().is_empty());

    assert_eq!(as_address, variable_address);
}

#[test]
fn reference_of() {
    const VARIABLE_DECLARATION: &str = "let mutable x: int32 = 32;";
    const REFERENCE_OF_IMMUTABLE: &str = "&x";
    const REFERENCE_OF_MUTABLE: &str = "&mutable x";

    let test_template = TestTemplate::new();
    let (mut binder, storage) = test_template.create_binder();

    let variable_declaration = parse_statement(VARIABLE_DECLARATION)
        .into_variable_declaration()
        .unwrap();

    let (variable_address, _) = binder
        .bind_variable_declaration(&variable_declaration, &storage)
        .unwrap();

    let reference_of_immutable = parse_expression(REFERENCE_OF_IMMUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();
    let reference_of_mutable = parse_expression(REFERENCE_OF_MUTABLE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    let reference_of_immutable_id = binder
        .bind(
            &reference_of_immutable,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();
    let reference_of_mutable_id = binder
        .bind(
            &reference_of_mutable,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let assert_reference_of =
        |id: ID<Register<infer::Model>>, qualifier: Qualifier| {
            let reference_of = binder
                .intermediate_representation
                .values
                .registers
                .get(id)
                .unwrap()
                .assignment
                .as_borrow()
                .unwrap();

            assert_eq!(reference_of.address, variable_address);
            assert_eq!(reference_of.qualifier, qualifier);
        };

    assert_reference_of(reference_of_immutable_id, Qualifier::Immutable);
    assert_reference_of(reference_of_mutable_id, Qualifier::Mutable);
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
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    assert!(binder
        .bind(
            &reference_of_mutable,
            Config { target: Target::RValue },
            &storage
        )
        .is_ok());

    let errors = storage.into_vec();

    assert!(errors.iter().any(|x| {
        let Some(error) =
            x.as_any().downcast_ref::<MismatchedQualifierForReferenceOf>()
        else {
            return false;
        };

        error.expected_qualifier == Qualifier::Mutable
            && error.found_qualifier == Qualifier::Immutable
            && !error.is_behind_reference
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

    let (reference_variable_address, _) = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage)
        .unwrap();

    let dereference = parse_expression(DEREFERENCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    let register_id = binder
        .bind(&dereference, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let dereference_register = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap();

    let dereference = dereference_register.assignment.as_load().unwrap();

    assert_eq!(
        *dereference.address.as_reference().unwrap().reference_address,
        reference_variable_address
    );
}

#[test]
fn dereference_as_address() {
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

    let (reference_variable_address, _) = binder
        .bind_variable_declaration(&reference_variable_declaration, &storage)
        .unwrap();

    let dereference = parse_expression(DEREFERENCE)
        .into_binary()
        .unwrap()
        .destruct()
        .0
        .into_prefixable()
        .unwrap()
        .into_prefix()
        .unwrap();

    let address = binder
        .bind(&dereference, Config { target: Target::LValue }, &storage)
        .unwrap()
        .into_l_value()
        .unwrap()
        .address;

    assert!(storage.as_vec().is_empty());

    assert_eq!(
        *address.into_reference().unwrap().reference_address,
        reference_variable_address
    );
}

impl TestTemplate {
    fn create_struct_template(
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
        .into_prefixable()
        .unwrap()
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
            .into_prefixable()
            .unwrap()
            .into_postfixable()
            .unwrap()
            .into_postfix()
            .unwrap();

    let first_register_id = binder
        .bind(
            &first_variant_call_expression,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let first_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(first_register_id)
        .unwrap()
        .assignment
        .as_variant()
        .unwrap();

    assert_eq!(first_assignment.variant_id, first_id);

    let numeric_literal = first_assignment
        .associated_value
        .as_ref()
        .unwrap()
        .as_literal()
        .unwrap()
        .as_numeric()
        .unwrap();

    assert_eq!(numeric_literal.integer_string, "32");
    assert_eq!(numeric_literal.decimal_stirng, None);

    let fourth_register_id = binder
        .bind(
            &fourth_variant_call_expression,
            Config { target: Target::RValue },
            &storage,
        )
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_register()
        .unwrap();

    assert!(storage.as_vec().is_empty());

    let fourth_assignment = binder
        .intermediate_representation
        .values
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
        .into_prefixable()
        .unwrap()
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    let _ = binder
        .bind(&first_expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
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
        .into_prefixable()
        .unwrap()
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    // no check
    storage.clear();

    let _ = binder
        .bind(&second_expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
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
        .into_prefixable()
        .unwrap()
        .into_postfixable()
        .unwrap()
        .into_postfix()
        .unwrap();

    storage.clear();

    let _ = binder
        .bind(&fourth_expression, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
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
fn single_express_block() {
    const BLOCK: &str = r"{
        express 32;
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

    let numeric_literal = binder
        .bind(&block, Config { target: Target::RValue }, &storage)
        .unwrap()
        .into_r_value()
        .unwrap()
        .into_literal()
        .unwrap()
        .into_numeric()
        .unwrap();

    assert_eq!(numeric_literal.integer_string, "32");
    assert!(numeric_literal.decimal_stirng.is_none());
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

#[test]
fn cannot_dereference_error() {
    bind_as_value_expect_error("*32", |_, result, errors| {
        assert!(result.is_err());

        assert!(errors.iter().any(|x| {
            x.as_any()
                .downcast_ref::<CannotDereference<ConstraintModel>>()
                .is_some()
        }));
    });
}

pub struct Check<T> {
    source: String,
    config: Config,
    check: Box<
        dyn FnOnce(
            &Binder<Building, resolution::NoOp, type_system::observer::NoOp>,
            Result<Expression, super::Error>,
            Vec<Box<dyn crate::error::Error>>,
            &T,
        ),
    >,
}

impl<T> Check<T> {
    pub fn new(
        source: impl std::fmt::Display,
        config: Config,
        check: impl FnOnce(
                &Binder<Building, resolution::NoOp, type_system::observer::NoOp>,
                Result<Expression, super::Error>,
                Vec<Box<dyn crate::error::Error>>,
                &T,
            ) + 'static,
    ) -> Self {
        Self { source: source.to_string(), config, check: Box::new(check) }
    }
}

pub fn setup_and_bind<T: 'static>(
    setup: impl for<'a> FnOnce(
        &'a mut TestTemplate,
    ) -> (
        Binder<'a, Building, resolution::NoOp, type_system::observer::NoOp>,
        T,
    ),
    checks: impl IntoIterator<Item = Check<T>>,
) {
    let mut test_template = TestTemplate::new();
    let (mut binder, data) = setup(&mut test_template);

    for check in checks {
        let expression = parse_expression(check.source);

        let storage = Storage::<Box<dyn crate::error::Error>>::new();
        let expression = binder.bind(&expression, check.config, &storage);

        (check.check)(&binder, expression, storage.into_vec(), &data);
    }
}

#[test]
fn array_access() {
    let normal = Check::new(
        "myArray.[3]",
        Config { target: Target::LValue },
        |_binder, exp, errors, variable_address| {
            assert!(errors.is_empty());

            let index_address = exp
                .unwrap()
                .into_l_value()
                .unwrap()
                .address
                .into_index()
                .unwrap();

            assert_eq!(*index_address.array_address, *variable_address);

            let numeric_value = index_address
                .indexing_value
                .into_literal()
                .unwrap()
                .into_numeric()
                .unwrap();

            assert_eq!(numeric_value.integer_string, "3");
            assert!(numeric_value.decimal_stirng.is_none());
        },
    );

    let usize_expected = Check::new(
        "myArray.[3f64]",
        Config { target: Target::RValue },
        |_binder, _exp, errors, _variable_address| {
            assert!(errors.iter().any(|x| {
                let Some(error) = x
                    .as_any()
                    .downcast_ref::<MismatchedType<ConstraintModel>>()
                else {
                    return false;
                };

                error.span.str() == "[3f64]"
                    && error.expected_type == Type::Primitive(Primitive::Usize)
                    && error.found_type == Type::Primitive(Primitive::Float64)
            }));
        },
    );
    let array_expected = Check::new(
        "32i32.[3]",
        Config { target: Target::RValue },
        |_binder, _exp, errors, _variable_address| {
            assert!(errors.iter().any(|x| {
                let Some(error) =
                    x.as_any().downcast_ref::<ExpectArray<ConstraintModel>>()
                else {
                    return false;
                };

                error.r#type == Type::Primitive(Primitive::Int32)
            }));
        },
    );

    setup_and_bind(
        |test_template| {
            let (mut binder, storage) = test_template.create_binder();

            let statement = parse_statement(
                "let myArray = [1i32, 2, 4, 8, 16, 32, 64, 128];",
            );

            let (variable_address, _) = binder
                .bind_variable_declaration(
                    &statement.into_variable_declaration().unwrap(),
                    &storage,
                )
                .unwrap();

            (binder, variable_address)
        },
        [normal, usize_expected, array_expected],
    );
}

#[test]
fn arrow_access() {
    let norml_check = Check::new(
        "(&mutable myTuple)->0",
        Config { target: Target::LValue },
        |binder, exp, errors, variable_address| {
            assert!(errors.is_empty());

            let address = exp
                .unwrap()
                .into_l_value()
                .unwrap()
                .address
                .into_tuple()
                .unwrap();

            let temp_mutable_tuple_variable = address
                .tuple_address
                .into_reference()
                .unwrap()
                .reference_address
                .into_memory()
                .unwrap()
                .into_alloca()
                .unwrap();

            let store = binder
                .current_block()
                .instructions()
                .iter()
                .find_map(|x| {
                    x.as_store().take_if(|x| {
                        x.address
                            .as_memory()
                            .and_then(|x| x.as_alloca().copied())
                            .map_or(false, |x| x == temp_mutable_tuple_variable)
                    })
                })
                .unwrap();

            let reference_of = binder
                .intermediate_representation
                .values
                .registers
                .get(store.value.as_register().copied().unwrap())
                .unwrap()
                .assignment
                .as_borrow()
                .unwrap();

            assert_eq!(reference_of.address, *variable_address);

            assert_eq!(address.offset, address::Offset::FromStart(0));
        },
    );
    let reference_type_expected = Check::new(
        "myTuple->0",
        Config { target: Target::RValue },
        |_binder, _exp, errors, _variable_address| {
            assert!(errors.iter().any(|x| {
                x.as_any()
                    .downcast_ref::<CannotDereference<ConstraintModel>>()
                    .is_some()
            }));
        },
    );

    setup_and_bind(
        |test_template| {
            let (mut binder, storage) = test_template.create_binder();

            let statement = parse_statement(
                "let mutable myTuple = (1i8, 2i16, 3i32, 4i64);",
            );

            let (variable_address, _) = binder
                .bind_variable_declaration(
                    &statement.into_variable_declaration().unwrap(),
                    &storage,
                )
                .unwrap();

            (binder, variable_address)
        },
        [norml_check, reference_type_expected],
    );
}

#[test]
fn struct_method() {
    let get_x_check = Check::new(
        "vector2.getX()",
        Config { target: Target::RValue },
        |binder, exp, errors, (_, get_x_id, _, _, variable_address, _)| {
            assert!(errors.is_empty(), "{:?}", errors);

            let call_register_id =
                exp.unwrap().into_r_value().unwrap().into_register().unwrap();

            let call = binder
                .intermediate_representation
                .values
                .registers
                .get(call_register_id)
                .unwrap()
                .assignment
                .as_function_call()
                .unwrap();

            assert_eq!(call.arguments.len(), 1);
            assert_eq!(
                call.callable_id,
                CallableID::AdtImplementationFunction(*get_x_id)
            );

            let load_register_id =
                *call.arguments.get(0).unwrap().as_register().unwrap();

            let load = binder
                .intermediate_representation
                .values
                .registers
                .get(load_register_id)
                .unwrap()
                .assignment
                .as_load()
                .unwrap();

            assert_eq!(load.address, *variable_address);

            assert!(Equality::new(
                Type::Primitive(Primitive::Int32),
                binder.type_of_register(call_register_id).unwrap()
            )
            .query(&binder.create_environment())
            .unwrap()
            .is_some());
        },
    );

    let get_ref_x_check = Check::new(
        "vector2.getRefX()",
        Config { target: Target::RValue },
        |binder, exp, errors, (_, _, get_ref_x_id, _, variable_address, _)| {
            assert!(errors.is_empty(), "{:?}", errors);

            let call_register_id =
                exp.unwrap().into_r_value().unwrap().into_register().unwrap();

            let call = binder
                .intermediate_representation
                .values
                .registers
                .get(call_register_id)
                .unwrap()
                .assignment
                .as_function_call()
                .unwrap();

            assert_eq!(call.arguments.len(), 1);
            assert_eq!(
                call.callable_id,
                CallableID::AdtImplementationFunction(*get_ref_x_id)
            );

            let reference_of_register_id =
                *call.arguments.get(0).unwrap().as_register().unwrap();

            let reference_of = binder
                .intermediate_representation
                .values
                .registers
                .get(reference_of_register_id)
                .unwrap()
                .assignment
                .as_borrow()
                .unwrap();

            assert_eq!(reference_of.address, *variable_address);
            assert_eq!(reference_of.qualifier, Qualifier::Immutable);
            assert_eq!(reference_of.lifetime, Lifetime::Inference(Erased));

            assert!(Equality::new(
                Type::Reference(Reference {
                    lifetime: Lifetime::Inference(Erased),
                    qualifier: Qualifier::Immutable,
                    pointee: Box::new(Type::Primitive(Primitive::Int32)),
                }),
                binder.type_of_register(call_register_id).unwrap()
            )
            .query(&binder.create_environment())
            .unwrap()
            .is_some());
        },
    );

    let set_x_check = Check::new(
        "vector2Mutable.setX(0)",
        Config { target: Target::RValue },
        |binder,
         exp,
         errors,
         (_, _, _, set_x_id, _, mutable_variable_address)| {
            assert!(errors.is_empty(), "{:?}", errors);

            let call_register_id =
                exp.unwrap().into_r_value().unwrap().into_register().unwrap();

            let call = binder
                .intermediate_representation
                .values
                .registers
                .get(call_register_id)
                .unwrap()
                .assignment
                .as_function_call()
                .unwrap();

            assert_eq!(call.arguments.len(), 2);
            assert_eq!(
                call.callable_id,
                CallableID::AdtImplementationFunction(*set_x_id)
            );

            let reference_of_register_id =
                *call.arguments.get(0).unwrap().as_register().unwrap();

            let reference_of = binder
                .intermediate_representation
                .values
                .registers
                .get(reference_of_register_id)
                .unwrap()
                .assignment
                .as_borrow()
                .unwrap();

            assert_eq!(reference_of.address, *mutable_variable_address);
            assert_eq!(reference_of.qualifier, Qualifier::Mutable);
            assert_eq!(reference_of.lifetime, Lifetime::Inference(Erased));

            let numeric_literal = call
                .arguments
                .get(1)
                .unwrap()
                .as_literal()
                .unwrap()
                .as_numeric()
                .unwrap();

            assert_eq!(numeric_literal.decimal_stirng, None);
            assert_eq!(numeric_literal.integer_string, "0");
        },
    );

    setup_and_bind(
        |test_template| {
            // create struct Vector2[T] { public x: T, public y: T }
            let struct_id = test_template
                .table
                .insert_member(
                    "Vector2".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    {
                        let mut generic = GenericDeclaration::default();
                        let _ = generic.parameters.add_type_parameter(
                            TypeParameter { name: None, span: None },
                        );
                        generic
                    },
                    AdtTemplate::<StructDefinition>::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            let t_parameter_id;
            let implementation_id = test_template
                .table
                .insert_implementation(
                    struct_id,
                    test_template.test_module_id,
                    {
                        let mut generic = GenericDeclaration::default();
                        t_parameter_id = generic
                            .parameters
                            .add_type_parameter(TypeParameter {
                                name: Some("T".to_string()),
                                span: None,
                            })
                            .unwrap();

                        generic
                    },
                    None,
                    GenericArguments::default(),
                    AdtImplementationDefinition::default(),
                )
                .unwrap();

            test_template.table.get_mut(implementation_id).unwrap().arguments =
                GenericArguments {
                    lifetimes: Vec::new(),
                    types: vec![Type::Parameter(TypeParameterID {
                        parent: implementation_id.into(),
                        id: t_parameter_id,
                    })],
                    constants: Vec::new(),
                };

            // create getX(this): T
            let get_x_id: ID<AdtImplementationFunction> = test_template
                .table
                .insert_member(
                    "getX".to_string(),
                    Accessibility::Public,
                    implementation_id,
                    None,
                    GenericDeclaration::default(),
                    FunctionTemplate::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            let get_x_func = test_template.table.get_mut(get_x_id).unwrap();
            get_x_func.insert_parameter(Parameter {
                r#type: Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(struct_id.into()),
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Parameter(TypeParameterID {
                            parent: implementation_id.into(),
                            id: t_parameter_id,
                        })],
                        constants: Vec::new(),
                    },
                }),

                span: None,
            });
            get_x_func.return_type = Type::Parameter(TypeParameterID {
                parent: implementation_id.into(),
                id: t_parameter_id,
            });

            // create getRefX['a](vector2: &'a Vector2[T]): &'a T
            let a_lifetime_id;
            let get_ref_x_id = test_template
                .table
                .insert_member(
                    "getRefX".to_string(),
                    Accessibility::Public,
                    implementation_id,
                    None,
                    {
                        let mut generic = GenericDeclaration::default();
                        a_lifetime_id = generic
                            .parameters
                            .add_lifetime_parameter(LifetimeParameter {
                                name: Some("a".to_string()),
                                span: None,
                            })
                            .unwrap();

                        generic
                    },
                    FunctionTemplate::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            let get_ref_x_func =
                test_template.table.get_mut(get_ref_x_id).unwrap();
            get_ref_x_func.insert_parameter(Parameter {
                r#type: Type::Reference(Reference {
                    lifetime: Lifetime::Parameter(LifetimeParameterID {
                        parent: get_ref_x_id.into(),
                        id: a_lifetime_id,
                    }),
                    qualifier: Qualifier::Immutable,
                    pointee: Box::new(Type::Symbol(Symbol {
                        id: r#type::SymbolID::Adt(struct_id.into()),
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![Type::Parameter(TypeParameterID {
                                parent: implementation_id.into(),
                                id: t_parameter_id,
                            })],
                            constants: Vec::new(),
                        },
                    })),
                }),

                span: None,
            });
            get_ref_x_func.return_type = Type::Reference(Reference {
                lifetime: Lifetime::Parameter(LifetimeParameterID {
                    parent: get_ref_x_id.into(),
                    id: a_lifetime_id,
                }),
                qualifier: Qualifier::Immutable,
                pointee: Box::new(Type::Parameter(TypeParameterID {
                    parent: implementation_id.into(),
                    id: t_parameter_id,
                })),
            });

            // create setX['a](vector2: &'a mutable Vector2[T], value: T)
            let a_lifetime_id;
            let set_x_id = test_template
                .table
                .insert_member(
                    "setX".to_string(),
                    Accessibility::Public,
                    implementation_id,
                    None,
                    {
                        let mut generic = GenericDeclaration::default();
                        a_lifetime_id = generic
                            .parameters
                            .add_lifetime_parameter(LifetimeParameter {
                                name: Some("a".to_string()),
                                span: None,
                            })
                            .unwrap();

                        generic
                    },
                    FunctionTemplate::default(),
                )
                .unwrap()
                .unwrap_no_duplication();
            let set_x_func = test_template.table.get_mut(set_x_id).unwrap();
            set_x_func.insert_parameter(Parameter {
                r#type: Type::Reference(Reference {
                    lifetime: Lifetime::Parameter(LifetimeParameterID {
                        parent: set_x_id.into(),
                        id: a_lifetime_id,
                    }),
                    qualifier: Qualifier::Mutable,
                    pointee: Box::new(Type::Symbol(Symbol {
                        id: r#type::SymbolID::Adt(struct_id.into()),
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![Type::Parameter(TypeParameterID {
                                parent: implementation_id.into(),
                                id: t_parameter_id,
                            })],
                            constants: Vec::new(),
                        },
                    })),
                }),

                span: None,
            });
            set_x_func.insert_parameter(Parameter {
                r#type: Type::Parameter(TypeParameterID {
                    parent: implementation_id.into(),
                    id: t_parameter_id,
                }),
                span: None,
            });

            let (mut binder, storage) = test_template.create_binder();
            let statement = parse_statement(
                "let vector2 = Vector2[int32] { x: 32, y: 64 };",
            );

            let (variable_address, _) = binder
                .bind_variable_declaration(
                    &statement.into_variable_declaration().unwrap(),
                    &storage,
                )
                .unwrap();

            let statement = parse_statement(
                "let mutable vector2Mutable = Vector2[int32] { x: 32, y: 64 };",
            );

            let (mutable_variable_address, _) = binder
                .bind_variable_declaration(
                    &statement.into_variable_declaration().unwrap(),
                    &storage,
                )
                .unwrap();

            (
                binder,
                (
                    struct_id,
                    get_x_id,
                    get_ref_x_id,
                    set_x_id,
                    variable_address,
                    mutable_variable_address,
                ),
            )
        },
        [get_x_check, get_ref_x_check, set_x_check],
    )
}

/*
public struct MyStruct {}

public trait NoMethod {
    public function method(self: MyStruct, myBoolean: bool);
}

public trait MethodWithBoolean[T] {
    public function method(self: T, myBoolean: bool);
}

public trait MethodWithInt32[T] {
    public function method(self: T, myInt32: int32);
}

public trait FirstAmbiguousMethod[T] {
    public function ambiguous(self: T, first: int32);
}

public trait SecondAmbiguousMethod[T] {
    public function ambiguous(self: T, second: int64);
}

public function main() {
    let mutable myStruct = MyStruct {};

    myStruct.method(true); // MethodWithBoolean[int32]::method
    myStruct.method(0i32); // MethodWithInt32[int32]::method

    myStruct.ambiguous(32); // ??? (ambiguous)
}
 */

#[test]
fn trait_method() {
    let method_with_boolean = Check::new(
        "myStruct.method(true)",
        Config { target: Target::RValue },
        |binder, exp, errors, (_, method_with_boolean_id, _, _, _, _)| {
            assert!(errors.is_empty(), "{:?}", errors);

            let call_register_id =
                exp.unwrap().into_r_value().unwrap().into_register().unwrap();

            let call = binder
                .intermediate_representation
                .values
                .registers
                .get(call_register_id)
                .unwrap()
                .assignment
                .as_function_call()
                .unwrap();

            assert_eq!(
                call.callable_id,
                CallableID::TraitFunction(*method_with_boolean_id)
            );
        },
    );

    let method_with_int32 = Check::new(
        "myStruct.method(0i32)",
        Config { target: Target::RValue },
        |binder, exp, errors, (_, _, method_with_int32_id, _, _, _)| {
            assert!(errors.is_empty(), "{:?}", errors);

            let call_register_id =
                exp.unwrap().into_r_value().unwrap().into_register().unwrap();

            let call = binder
                .intermediate_representation
                .values
                .registers
                .get(call_register_id)
                .unwrap()
                .assignment
                .as_function_call()
                .unwrap();

            assert_eq!(
                call.callable_id,
                CallableID::TraitFunction(*method_with_int32_id)
            );
        },
    );

    let ambiguous_method = Check::new(
        "myStruct.ambiguous(32)",
        Config { target: Target::RValue },
        |_, exp, errors, (_, _, _, first_amb, second_amb, _)| {
            assert!(exp.is_err());
            assert!(errors.len() == 1);

            let error = errors[0]
                .as_any()
                .downcast_ref::<AmbiguousMethodCall>()
                .unwrap();

            assert_eq!(error.callable_candidates.len(), 2);

            assert!(error
                .callable_candidates
                .contains(&CallableID::TraitFunction(*first_amb)));
            assert!(error
                .callable_candidates
                .contains(&(CallableID::TraitFunction(*second_amb))));
        },
    );

    setup_and_bind(
        |test_template| {
            // create struct MyStruct {}
            let my_struct_id = test_template
                .table
                .insert_member(
                    "MyStruct".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    GenericDeclaration::default(),
                    AdtTemplate::<StructDefinition>::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            // create trait NoMethod { ... }
            let no_method_trait_id = test_template
                .table
                .insert_member(
                    "NoMethod".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    GenericDeclaration::default(),
                    TraitDefinition::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            // create function method(self: MyStruct, myBoolean: bool);
            let no_method_trait_method_id: ID<TraitFunction> = test_template
                .table
                .insert_member(
                    "method".to_string(),
                    Accessibility::Public,
                    no_method_trait_id,
                    None,
                    GenericDeclaration::default(),
                    FunctionTemplate::default(),
                )
                .unwrap()
                .unwrap_no_duplication();

            let no_method_trait_method =
                test_template.table.get_mut(no_method_trait_method_id).unwrap();

            no_method_trait_method.insert_parameter(Parameter {
                r#type: Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Struct(my_struct_id)),
                    generic_arguments: GenericArguments::default(),
                }),
                span: None,
            });
            no_method_trait_method.insert_parameter(Parameter {
                r#type: Type::Primitive(Primitive::Bool),
                span: None,
            });

            // create trait MethodWithBoolean[T] { ... }
            let t_parameter_id;
            let method_with_boolean_trait_id = test_template
                .table
                .insert_member(
                    "MethodWithBoolean".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    {
                        let mut generic = GenericDeclaration::default();
                        t_parameter_id = generic
                            .parameters
                            .add_type_parameter(TypeParameter {
                                name: None,
                                span: None,
                            })
                            .unwrap();
                        generic
                    },
                    TraitDefinition::default(),
                )
                .unwrap()
                .unwrap_no_duplication();
            let t_parameter = Type::Parameter(TypeParameterID {
                parent: method_with_boolean_trait_id.into(),
                id: t_parameter_id,
            });

            // create function method(self: T, myBoolean: bool);
            let method_with_boolean_trait_method_id: ID<TraitFunction> =
                test_template
                    .table
                    .insert_member(
                        "method".to_string(),
                        Accessibility::Public,
                        method_with_boolean_trait_id,
                        None,
                        GenericDeclaration::default(),
                        FunctionTemplate::default(),
                    )
                    .unwrap()
                    .unwrap_no_duplication();

            let method_with_boolean_trait_method = test_template
                .table
                .get_mut(method_with_boolean_trait_method_id)
                .unwrap();

            method_with_boolean_trait_method.insert_parameter(Parameter {
                r#type: t_parameter.clone(),
                span: None,
            });
            method_with_boolean_trait_method.insert_parameter(Parameter {
                r#type: Type::Primitive(Primitive::Bool),
                span: None,
            });

            // create trait MethodWithInt32[T] { ... }
            let t_parameter_id;
            let method_with_int32_trait_id = test_template
                .table
                .insert_member(
                    "MethodWithInt32".to_string(),
                    Accessibility::Public,
                    test_template.test_module_id,
                    None,
                    {
                        let mut generic = GenericDeclaration::default();
                        t_parameter_id = generic
                            .parameters
                            .add_type_parameter(TypeParameter {
                                name: None,
                                span: None,
                            })
                            .unwrap();
                        generic
                    },
                    TraitDefinition::default(),
                )
                .unwrap()
                .unwrap_no_duplication();
            let t_parameter = Type::Parameter(TypeParameterID {
                parent: method_with_int32_trait_id.into(),
                id: t_parameter_id,
            });

            // create function method(self: T, myBoolean: bool);
            let method_with_int32_trait_method_id: ID<TraitFunction> =
                test_template
                    .table
                    .insert_member(
                        "method".to_string(),
                        Accessibility::Public,
                        method_with_int32_trait_id,
                        None,
                        GenericDeclaration::default(),
                        FunctionTemplate::default(),
                    )
                    .unwrap()
                    .unwrap_no_duplication();

            let method_with_boolean_trait_method = test_template
                .table
                .get_mut(method_with_int32_trait_method_id)
                .unwrap();

            method_with_boolean_trait_method.insert_parameter(Parameter {
                r#type: t_parameter.clone(),
                span: None,
            });
            method_with_boolean_trait_method.insert_parameter(Parameter {
                r#type: Type::Primitive(Primitive::Int32),
                span: None,
            });

            // create trait FirstAmbiguousMethod[T] { ... }
            let mut ambiguous_methods = [
                Type::Primitive(Primitive::Int32),
                Type::Primitive(Primitive::Int64),
            ]
            .into_iter()
            .enumerate()
            .map(|(i, x)| {
                let t_parameter_id;
                let trait_id = test_template
                    .table
                    .insert_member(
                        format!("AmbiguousMethod{}", i),
                        Accessibility::Public,
                        test_template.test_module_id,
                        None,
                        {
                            let mut generic = GenericDeclaration::default();
                            t_parameter_id = generic
                                .parameters
                                .add_type_parameter(TypeParameter {
                                    name: None,
                                    span: None,
                                })
                                .unwrap();
                            generic
                        },
                        TraitDefinition::default(),
                    )
                    .unwrap()
                    .unwrap_no_duplication();

                let t_parameter = Type::Parameter(TypeParameterID {
                    parent: trait_id.into(),
                    id: t_parameter_id,
                });

                let method_id = test_template
                    .table
                    .insert_member(
                        "ambiguous".to_string(),
                        Accessibility::Public,
                        trait_id,
                        None,
                        GenericDeclaration::default(),
                        FunctionTemplate::default(),
                    )
                    .unwrap()
                    .unwrap_no_duplication();

                let method = test_template.table.get_mut(method_id).unwrap();

                method.insert_parameter(Parameter {
                    r#type: t_parameter.clone(),
                    span: None,
                });

                method.insert_parameter(Parameter {
                    r#type: x.clone(),
                    span: None,
                });

                method_id
            });

            let first_ambiguous_method_id = ambiguous_methods.next().unwrap();
            let second_ambiguous_method_id = ambiguous_methods.next().unwrap();

            let (mut binder, storage) = test_template.create_binder();
            let statement = parse_statement("let myStruct = MyStruct {};");

            let alloca_id = binder
                .bind_variable_declaration(
                    &statement.into_variable_declaration().unwrap(),
                    &storage,
                )
                .unwrap();

            (
                binder,
                (
                    my_struct_id,
                    method_with_boolean_trait_method_id,
                    method_with_int32_trait_method_id,
                    first_ambiguous_method_id,
                    second_ambiguous_method_id,
                    alloca_id,
                ),
            )
        },
        [method_with_boolean, method_with_int32, ambiguous_method],
    )
}
