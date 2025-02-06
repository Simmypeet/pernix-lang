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
