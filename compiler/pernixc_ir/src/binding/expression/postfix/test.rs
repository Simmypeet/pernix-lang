use pernixc_handler::{Panic, Storage};
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::{
    generic_parameter::{GenericParameters, TypeParameterID},
    r#type::{Primitive, Qualifier, Type},
};
use pernixc_type_system::equality::Equality;

use crate::{
    address::{self, Address, Offset},
    binding::{
        diagnostic::{
            CannotIndexPastUnpackedTuple, ExpectedStructType, ExpectedTuple,
            ExpressionIsNotCallable, FieldIsNotAccessible, FieldNotFound,
            InvalidCastType, MismatchedArgumentCount, MismatchedType,
            TooLargeTupleIndex, TupleIndexOutOfBOunds,
            UnexpectedGenericArgumentsInField,
        },
        test::{build_table, BindExt, CreateBinderAtExt, Template},
    },
    model::{self, Constraint},
};

#[test]
fn cast() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Postfixable>(
            "32i32 as float64",
        ))
        .into_register()
        .unwrap();

    let cast = binder
        .intermediate_representation
        .values
        .registers
        .get(register_id)
        .unwrap()
        .assignment
        .as_cast()
        .unwrap();

    let numeric = cast.value.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(numeric.integer_string, "32");
    assert!(numeric.decimal_stirng.is_none());

    assert_eq!(cast.r#type, Type::Primitive(Primitive::Float64));
}

#[test]
fn invalid_cast_type() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("32i32 as ()"));

    assert_eq!(errors.len(), 1);

    let error = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<InvalidCastType<model::Constrained>>()
        .unwrap();

    assert_eq!(
        error.r#type,
        Type::Tuple(pernixc_term::Tuple { elements: Vec::new() })
    );
}

#[test]
fn expression_is_not_callable_error() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("32()"));

    assert_eq!(errors.len(), 1);

    let _ = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpressionIsNotCallable>()
        .unwrap();
}

const FUNCTION_DECLARATION: &str = r"
public function fizz[T](a: T, b: T, c: int32) {}

public function test() {}
";

#[test]
fn function_call() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    // no check
    let call_register = binder
        .bind_as_rvalue_success(&parse::<syntax_tree::expression::Postfixable>(
            "fizz(true, false, 32)",
        ))
        .into_register()
        .unwrap();

    let function_call_assignment = binder
        .intermediate_representation
        .values
        .registers
        .get(call_register)
        .unwrap()
        .assignment
        .as_function_call()
        .unwrap();

    let function_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();
    let generic_params = table.query::<GenericParameters>(function_id).unwrap();

    let function_t_ty_param = generic_params.type_parameter_ids_by_name()["T"];

    assert_eq!(function_call_assignment.callable_id, function_id);
    assert_eq!(function_call_assignment.arguments.len(), 3);

    // check if the generic arguments instantiated correctly
    {
        assert!(function_call_assignment.instantiation.lifetimes.is_empty());
        assert_eq!(function_call_assignment.instantiation.types.len(), 1);
        assert!(function_call_assignment.instantiation.constants.is_empty());

        let ty_param = Type::Parameter(TypeParameterID {
            parent: function_id,
            id: function_t_ty_param,
        });
        let instantiated = function_call_assignment
            .instantiation
            .types
            .get(&ty_param)
            .unwrap()
            .clone();

        let environment = binder.create_environment();

        assert!(environment
            .query(&Equality::new(
                instantiated,
                Type::Primitive(Primitive::Bool)
            ))
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());
    }

    // the first arguments is a boolean true
    {
        let first_value = function_call_assignment
            .arguments
            .first()
            .unwrap()
            .as_literal()
            .unwrap()
            .as_boolean()
            .unwrap();

        assert!(first_value.value);
    }

    // the second arguments is a boolean false
    {
        let second_value = function_call_assignment
            .arguments
            .get(1)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_boolean()
            .unwrap();

        assert!(!second_value.value);
    }

    // the third arguments is an integer 32
    {
        let third_value = function_call_assignment
            .arguments
            .get(2)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap();

        assert_eq!(third_value.integer_string, "32");
        assert!(third_value.decimal_stirng.is_none());
    }
}

#[test]
fn function_call_mismatched_argument_count_error() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);
    let fizz_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();

    // too many arguments
    {
        let (value, errors) = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Postfixable,
        >(
            "fizz(true, false, 32, 32)",
        ));

        let register_id = value.into_register().unwrap();
        let function_assignment = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_function_call()
            .unwrap();

        // should be trimmed to 3
        assert_eq!(function_assignment.arguments.len(), 3);

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 3);
        assert_eq!(error.found_count, 4);
        assert_eq!(error.called_id, fizz_id);
    }

    // to few
    {
        let (value, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::Postfixable,
            >("fizz(true, false)"));

        let register_id = value.into_register().unwrap();
        let function_assignment = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_function_call()
            .unwrap();

        // should be trimmed to 3
        assert_eq!(function_assignment.arguments.len(), 3);

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 3);
        assert_eq!(error.found_count, 2);
        assert_eq!(error.called_id, fizz_id);
    }
}

const STRUCT_DECLARATION: &str = r"
public module math {
    public struct Vector2 {
        public x: float32,
        public y: float32,
        
        private secret: float32,
    }
}

public function test() {}
";

#[test]
fn struct_field_access() {
    let table = build_table(STRUCT_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let address = {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        let (address, _) = binder
            .bind_variable_declaration(
                &parse(
                    "let mutable vector = math::Vector2 { x: 32, y: 64, \
                     secret: 128 };",
                ),
                &storage,
            )
            .unwrap();

        assert_eq!(storage.as_vec().len(), 1);

        address
    };
    let vector_id =
        table.get_by_qualified_name(["test", "math", "Vector2"]).unwrap();
    let fields =
        table.query::<pernixc_component::fields::Fields>(vector_id).unwrap();

    // x field
    {
        let register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::Postfixable,
            >("vector.x"))
            .into_register()
            .unwrap();

        let load = binder
            .intermediate_representation
            .values
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(
            load.address,
            Address::Field(address::Field {
                struct_address: Box::new(address.clone()),
                id: fields.field_ids_by_name["x"]
            })
        );
    }

    // y field
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::Postfixable,
        >("vector.y"));

        assert_eq!(
            lvalue.address,
            Address::Field(address::Field {
                struct_address: Box::new(address),
                id: fields.field_ids_by_name["y"]
            })
        );

        // check if the field is mutable
        assert_eq!(lvalue.qualifier, Qualifier::Mutable);
    }

    // not accessible
    {
        let (_, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::Postfixable,
            >("vector.secret"));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<FieldIsNotAccessible>()
            .unwrap();

        assert_eq!(error.field_id, fields.field_ids_by_name["secret"]);
    }

    // field with generic arguments
    {
        let (_, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::Postfixable,
            >("vector.x[int32]"));

        assert_eq!(errors.len(), 1);

        let _ = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<UnexpectedGenericArgumentsInField>()
            .unwrap();
    }

    // field not found
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("vector.z"));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<FieldNotFound>()
            .unwrap();

        assert_eq!(error.struct_id, vector_id);
        assert_eq!(error.identifier_span.str(), "z");
    }
}

#[test]
fn struct_field_access_on_non_struct() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("true.x"));

    assert_eq!(errors.len(), 1);

    let error = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpectedStructType<model::Constrained>>()
        .unwrap();

    assert_eq!(error.r#type, Type::Primitive(Primitive::Bool));
}

const TUPLE_FUNCTION_DECLARATION: &str = r"
public function test[T](t: T) where tuple T {}
";

#[test]
#[allow(clippy::too_many_lines)]
fn tuple_access() {
    let table = build_table(TUPLE_FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let address = {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        let (address, _) = binder
            .bind_variable_declaration(
                &parse(
                    "let mutable myTuple = (32i64, true, ...t, false, 64i32);",
                ),
                &storage,
            )
            .unwrap();

        assert!(storage.as_vec().is_empty());

        address
    };

    // from start 0
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.0"));

        assert_eq!(lvalue.qualifier, Qualifier::Mutable);
        assert_eq!(
            lvalue.address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(address.clone()),
                offset: address::Offset::FromStart(0)
            })
        );
    }

    // from end 0
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.-0"));

        let tuple_address = Address::Tuple(address::Tuple {
            tuple_address: Box::new(address),
            offset: address::Offset::FromEnd(0),
        });

        assert_eq!(lvalue.qualifier, Qualifier::Mutable);
        assert_eq!(lvalue.address, tuple_address);

        assert_eq!(
            binder.type_of_address(&tuple_address).unwrap(),
            Type::Primitive(Primitive::Int32)
        );
    }

    // index past unpacked from start
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.3"));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<CannotIndexPastUnpackedTuple<model::Constrained>>()
            .is_some());
    }

    // index past unpacked from end
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.-3"));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<CannotIndexPastUnpackedTuple<model::Constrained>>()
            .is_some());
    }

    // index past unpacked from start
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.3"));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<CannotIndexPastUnpackedTuple<model::Constrained>>()
            .is_some());
    }

    // index too large
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >(
            "myTuple.10000000000000000000000000000000000000000",
        ));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<TooLargeTupleIndex>()
            .is_some());
    }

    // index to unpack
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.2"));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<CannotIndexPastUnpackedTuple<model::Constrained>>()
            .is_some());
    }
}

#[test]
fn tuple_index_out_of_bounds() {
    let table = build_table(TUPLE_FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage = Storage::<Box<dyn Diagnostic>>::new();
    let _ = binder
        .bind_variable_declaration(
            &parse("let mutable myTuple = (32i64, true, false, 64i32);"),
            &storage,
        )
        .unwrap();

    assert!(storage.as_vec().is_empty());

    // index out of bounds
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::Postfixable,
        >("myTuple.5"));

        assert_eq!(errors.len(), 1);

        assert!(errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<TupleIndexOutOfBOunds<model::Constrained>>()
            .is_some());
    }
}

#[test]
fn tuple_index_on_non_tuple() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let errors = binder.bind_as_rvalue_error_fatal(&parse::<
        syntax_tree::expression::Postfixable,
    >("true.0"));

    assert_eq!(errors.len(), 1);

    assert!(errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpectedTuple<model::Constrained>>()
        .is_some());
}

#[test]
fn array_access() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let storage = Storage::<Box<dyn Diagnostic>>::new();
    let (address, _) = binder
        .bind_variable_declaration(
            &parse("let mutable array = [32, 64, 128];"),
            &storage,
        )
        .unwrap();

    assert!(storage.as_vec().is_empty());

    // from start 0
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::Postfixable,
        >("array.[0]"));

        assert_eq!(lvalue.qualifier, Qualifier::Mutable);

        let index = lvalue.address.as_index().unwrap();

        assert_eq!(*index.array_address, address);
        assert_eq!(
            index
                .indexing_value
                .as_literal()
                .unwrap()
                .as_numeric()
                .unwrap()
                .integer_string,
            "0"
        );
        assert!(index
            .indexing_value
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap()
            .decimal_stirng
            .is_none());
    }

    // usize expected
    {
        let error = binder.bind_as_rvalue_error(&parse::<
            syntax_tree::expression::Postfixable,
        >("array.[1.0]"));

        assert_eq!(error.1.len(), 1);

        let error = error
            .1
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedType<model::Constrained>>()
            .unwrap();

        assert_eq!(error.expected_type, Type::Primitive(Primitive::Usize));
        assert_eq!(error.found_type, Type::Inference(Constraint::Floating));
    }
}
