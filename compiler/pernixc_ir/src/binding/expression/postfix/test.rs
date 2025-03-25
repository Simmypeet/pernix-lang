use pernixc_handler::{Panic, Storage};
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_table::{
    component::{Implemented, Member},
    diagnostic::Diagnostic,
    GlobalID,
};
use pernixc_term::{
    generic_parameter::{GenericParameters, TypeParameterID},
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Reference, Type},
    Model,
};
use pernixc_type_system::equality::Equality;

use crate::{
    address::{self, Address},
    binding::{
        diagnostic::{
            AmbiguousMethodCall, CannotIndexPastUnpackedTuple,
            ExpectedStructType, ExpectedTuple, ExpressionIsNotCallable,
            FieldIsNotAccessible, FieldNotFound, InvalidCastType,
            MismatchedArgumentCount, MismatchedType, TooLargeTupleIndex,
            TupleIndexOutOfBOunds, UnexpectedGenericArgumentsInField,
        },
        test::{build_table, BindExt, CreateBinderAtExt, Template},
    },
    model::{self, Constraint, Erased},
    value::Value,
};

#[test]
fn cast() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let register_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >("32i32 as float64"))
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
        syntax_tree::expression::postfix::Postfixable,
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
        syntax_tree::expression::postfix::Postfixable,
    >("32()"));

    assert_eq!(errors.len(), 1);

    let _ = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<ExpressionIsNotCallable>()
        .unwrap();
}

#[test]
fn numeric_to_pointer_cast() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let reg_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >("4 as *int32"))
        .into_register()
        .unwrap();

    let cast = binder
        .intermediate_representation
        .values
        .registers
        .get(reg_id)
        .unwrap()
        .assignment
        .as_cast()
        .unwrap();

    let numeric = cast.value.as_literal().unwrap().as_numeric().unwrap();
    assert_eq!(numeric.integer_string, "4");

    assert_eq!(
        binder.type_of_value(&cast.value, &Panic).unwrap(),
        Type::Primitive(Primitive::Usize)
    );

    let pointer = cast.r#type.as_pointer().unwrap();
    assert_eq!(pointer.pointee, Box::new(Type::Primitive(Primitive::Int32)));
    assert!(!pointer.mutable);
}

const FUNCTION_DECLARATION: &str = r"
public function fizz[T](a: T, b: T, c: int32):
    pass

public function test():
    pass
";

#[test]
fn function_call() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    // no check
    let call_register = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >("fizz(true, false, 32)"))
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
            syntax_tree::expression::postfix::Postfixable,
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
                syntax_tree::expression::postfix::Postfixable,
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
public module math:
    public struct Vector2:
        public x: float32
        public y: float32

        private secret: float32


public function test():
    pass
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
                    "let mut vector = math::Vector2 { x: 32, y: 64, secret: \
                     128 }",
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
                syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
                syntax_tree::expression::postfix::Postfixable,
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
                syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
        syntax_tree::expression::postfix::Postfixable,
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
public function test[T: tuple](t: T):
    pass
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
                &parse("let mut myTuple = (32i64, true, ...t, false, 64i32);"),
                &storage,
            )
            .unwrap();

        assert!(storage.as_vec().is_empty());

        address
    };

    // from start 0
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
        >("myTuple.-0"));

        let tuple_address = Address::Tuple(address::Tuple {
            tuple_address: Box::new(address),
            offset: address::Offset::FromEnd(0),
        });

        assert_eq!(lvalue.qualifier, Qualifier::Mutable);
        assert_eq!(lvalue.address, tuple_address);

        assert_eq!(
            binder.type_of_address(&tuple_address, &Panic).unwrap(),
            Type::Primitive(Primitive::Int32)
        );
    }

    // index past unpacked from start
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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
            &parse("let mut myTuple = (32i64, true, false, 64i32);"),
            &storage,
        )
        .unwrap();

    assert!(storage.as_vec().is_empty());

    // index out of bounds
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::postfix::Postfixable,
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
        syntax_tree::expression::postfix::Postfixable,
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
            &parse("let mut array = [32, 64, 128]"),
            &storage,
        )
        .unwrap();

    assert!(storage.as_vec().is_empty());

    // from start 0
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
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
            syntax_tree::expression::postfix::Postfixable,
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

const STRUCT_WITH_ADT_METHODS_DECLARATION: &str = r"
public struct Vector2[T]:
    public x: T
    public y: T


implements[T] Vector2[T]:
    public function getX(self: this) -> T:
        panic

    public function getRefX(self: &this) -> &T:
        panic

    public function setX(self: &mut this, x: T):
        panic


public function test():
    panic
";

#[test]
#[allow(clippy::too_many_lines)]
fn adt_method() {
    let table = build_table(STRUCT_WITH_ADT_METHODS_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let (vector_address, _) = binder
        .bind_variable_declaration(
            &parse("let vector = Vector2 { x: 32i32, y: 64 };"),
            &Storage::<Box<dyn Diagnostic>>::new(),
        )
        .unwrap();

    let (mutable_vector_address, _) = binder
        .bind_variable_declaration(
            &parse("let mut mutableVector = Vector2 { x: 32i32, y: 64 };"),
            &Storage::<Box<dyn Diagnostic>>::new(),
        )
        .unwrap();

    let vector_id = table.get_by_qualified_name(["test", "Vector2"]).unwrap();
    let impl_id =
        table.get::<Implemented>(vector_id).iter().next().copied().unwrap();

    let get_x =
        GlobalID::new(impl_id.target_id, table.get::<Member>(impl_id)["getX"]);
    let get_ref_x = GlobalID::new(
        impl_id.target_id,
        table.get::<Member>(impl_id)["getRefX"],
    );
    let set_x =
        GlobalID::new(impl_id.target_id, table.get::<Member>(impl_id)["setX"]);

    // getX
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("vector.getX()"))
            .into_register()
            .unwrap();

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
        assert_eq!(call.callable_id, get_x);

        let load_register_id =
            *call.arguments.first().unwrap().as_register().unwrap();

        let load = binder
            .intermediate_representation
            .values
            .registers
            .get(load_register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(load.address, vector_address);

        assert!(binder
            .create_environment()
            .query(&Equality::new(
                Type::Primitive(Primitive::Int32),
                binder.type_of_register(call_register_id, &Panic).unwrap()
            ))
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());
    }

    // getRefX
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("vector.getRefX()"))
            .into_register()
            .unwrap();

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
        assert_eq!(call.callable_id, get_ref_x);

        let reference_of_register_id =
            *call.arguments.first().unwrap().as_register().unwrap();

        let reference_of = binder
            .intermediate_representation
            .values
            .registers
            .get(reference_of_register_id)
            .unwrap()
            .assignment
            .as_borrow()
            .unwrap();

        assert_eq!(reference_of.address, vector_address);
        assert_eq!(reference_of.qualifier, Qualifier::Immutable);
        assert_eq!(reference_of.lifetime, Lifetime::Inference(Erased));

        assert!(binder
            .create_environment()
            .query(&Equality::new(
                Type::Reference(Reference {
                    lifetime: Lifetime::Inference(Erased),
                    qualifier: Qualifier::Immutable,
                    pointee: Box::new(Type::Primitive(Primitive::Int32)),
                }),
                binder.type_of_register(call_register_id, &Panic).unwrap()
            ))
            .unwrap()
            .unwrap()
            .constraints
            .is_empty());
    }

    // set x
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("mutableVector.setX(0)"))
            .into_register()
            .unwrap();

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
        assert_eq!(call.callable_id, set_x);

        let reference_of_register_id =
            *call.arguments.first().unwrap().as_register().unwrap();

        let reference_of = binder
            .intermediate_representation
            .values
            .registers
            .get(reference_of_register_id)
            .unwrap()
            .assignment
            .as_borrow()
            .unwrap();

        assert_eq!(reference_of.address, mutable_vector_address);
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
    }
}

const TRAIT_METHOD_DECLARATION: &str = r"
public struct MyStruct:
    pass


public trait NoMethod:
    public function method(self: MyStruct, myBoolean: bool)


public trait MethodWithBoolean[T]:
    public function method(self: T, myBoolean: bool)


public trait MethodWithInt32[T]:
    public function method(self: T, myInt32: int32)


public trait FirstAmbiguousMethod[T] :
    public function ambiguous(self: T, first: int32)


public trait SecondAmbiguousMethod[T]:
    public function ambiguous(self: T, second: int64)


public function main():
    /*
    let mut myStruct = MyStruct {}

    myStruct.method(true) // MethodWithBoolean[int32]::method
    myStruct.method(0i32) // MethodWithInt32[int32]::method

    myStruct.ambiguous(32) // ??? (ambiguous)
    */
    pass

";

#[test]
#[allow(clippy::too_many_lines)]
fn trait_method() {
    let table = build_table(TRAIT_METHOD_DECLARATION);
    let mut binder = table.create_binder_at(["test", "main"]);

    let method_with_boolean_id = table
        .get_by_qualified_name(["test", "MethodWithBoolean", "method"])
        .unwrap();
    let method_with_int32_id = table
        .get_by_qualified_name(["test", "MethodWithInt32", "method"])
        .unwrap();
    let method_first_ambiguous_id = table
        .get_by_qualified_name(["test", "FirstAmbiguousMethod", "ambiguous"])
        .unwrap();
    let method_second_ambiguous_id = table
        .get_by_qualified_name(["test", "SecondAmbiguousMethod", "ambiguous"])
        .unwrap();

    let my_struct_address = {
        let (address, _) = binder
            .bind_variable_declaration(
                &parse("let mut myStruct = MyStruct {};"),
                &Panic,
            )
            .unwrap();

        address
    };

    // method with boolean
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("myStruct.method(true)"))
            .into_register()
            .unwrap();

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
        assert_eq!(call.callable_id, method_with_boolean_id);

        let load = binder
            .intermediate_representation
            .values
            .registers
            .get(*call.arguments.first().unwrap().as_register().unwrap())
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(load.address, my_struct_address);

        let boolean_literal = call
            .arguments
            .get(1)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_boolean()
            .unwrap();

        assert!(boolean_literal.value);
    }

    // method with i32
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("myStruct.method(0i32)"))
            .into_register()
            .unwrap();

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
        assert_eq!(call.callable_id, method_with_int32_id);

        let load = binder
            .intermediate_representation
            .values
            .registers
            .get(*call.arguments.first().unwrap().as_register().unwrap())
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(load.address, my_struct_address);

        let numeric_literal = call
            .arguments
            .get(1)
            .unwrap()
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap();

        assert_eq!(numeric_literal.integer_string, "0");
        assert!(numeric_literal.decimal_stirng.is_none());
    }

    // ambiguous
    {
        let errors = binder.bind_as_rvalue_error_fatal(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >(
            "myStruct.ambiguous(32)",
        ));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<AmbiguousMethodCall>()
            .unwrap();

        assert_eq!(error.callable_candidates.len(), 2);
        assert!(error.callable_candidates.contains(&method_first_ambiguous_id));
        assert!(error
            .callable_candidates
            .contains(&method_second_ambiguous_id));
    }
}

#[test]
fn arrow_access() {
    let template = Template::new();
    let mut binder = template.create_binder();

    let _ = binder
        .bind_variable_declaration(
            &parse("let mut myTuple = (0i8, 1i16, 2i32, 3i64);"),
            &Panic,
        )
        .unwrap();

    let my_tuple_mut_ref_address = {
        let (address, _) = binder
            .bind_variable_declaration(
                &parse("let myTupleMutRef = &mut myTuple;"),
                &Panic,
            )
            .unwrap();

        address
    };

    let my_tuple_ref_address = {
        let (address, _) = binder
            .bind_variable_declaration(
                &parse("let myTupleRef = &myTuple;"),
                &Panic,
            )
            .unwrap();

        address
    };

    // myTupleRef
    {
        let lvalue = binder.bind_as_lvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >("myTupleRef->1"));

        assert_eq!(
            lvalue.address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Reference(
                    address::Reference {
                        qualifier: Qualifier::Immutable,
                        reference_address: Box::new(my_tuple_ref_address)
                    }
                )),
                offset: address::Offset::FromStart(1)
            })
        );
        assert_eq!(lvalue.qualifier, Qualifier::Immutable);
    }

    // myTupleRefMut
    {
        let lvalue =
            binder.bind_as_lvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("myTupleMutRef->2"));

        assert_eq!(
            lvalue.address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Reference(
                    address::Reference {
                        qualifier: Qualifier::Mutable,
                        reference_address: Box::new(my_tuple_mut_ref_address)
                    }
                )),
                offset: address::Offset::FromStart(2)
            })
        );
        assert_eq!(lvalue.qualifier, Qualifier::Mutable);
    }
}

const ENUM_DECLARATION: &str = r"
public enum Sample[F, S]:
    First(F)
    Second(S)
    Third(bool)
    Fourth


public function test():
    pass
";

#[test]
fn variant_call() {
    let table = build_table(ENUM_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let first_variant_id =
        table.get_by_qualified_name(["test", "Sample", "First"]).unwrap();
    let fourth_variant_id =
        table.get_by_qualified_name(["test", "Sample", "Fourth"]).unwrap();

    // first variant
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("Sample::First(32)"))
            .into_register()
            .unwrap();

        let call = binder
            .intermediate_representation
            .values
            .registers
            .get(call_register_id)
            .unwrap()
            .assignment
            .as_variant()
            .unwrap();

        assert_eq!(call.variant_id, first_variant_id);

        let numeric_literal = call
            .associated_value
            .as_ref()
            .unwrap()
            .as_literal()
            .unwrap()
            .as_numeric()
            .unwrap();

        assert_eq!(numeric_literal.integer_string, "32");
        assert!(numeric_literal.decimal_stirng.is_none());
    }

    // fourth variant
    {
        let call_register_id = binder
            .bind_as_rvalue_success(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("Sample::Fourth()"))
            .into_register()
            .unwrap();

        let call = binder
            .intermediate_representation
            .values
            .registers
            .get(call_register_id)
            .unwrap()
            .assignment
            .as_variant()
            .unwrap();

        assert_eq!(call.variant_id, fourth_variant_id);
        assert!(call.associated_value.is_none());
    }
}

#[test]
fn variant_call_mismatched_argument_count_error() {
    let table = build_table(ENUM_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    // too many
    {
        let (_, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("Sample::First(32, 64)"));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 1);
        assert_eq!(error.found_count, 2);
    }

    // too few
    {
        let (_, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("Sample::Second()"));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 1);
        assert_eq!(error.found_count, 0);
    }

    // too many
    {
        let (_, errors) =
            binder.bind_as_rvalue_error(&parse::<
                syntax_tree::expression::postfix::Postfixable,
            >("Sample::Fourth(32)"));

        assert_eq!(errors.len(), 1);

        let error = errors
            .first()
            .unwrap()
            .as_any()
            .downcast_ref::<MismatchedArgumentCount>()
            .unwrap();

        assert_eq!(error.expected_count, 0);
        assert_eq!(error.found_count, 1);
    }
}

const VAR_ARGS: &str = r#"
extern "C":
    public function fizz(a: int32, ...)


public function test():
    pass
"#;

fn test_integer(value: &Value<impl Model>, int: &str) {
    let num = value.as_literal().unwrap().as_numeric().unwrap();

    assert_eq!(num.integer_string, int);
    assert_eq!(num.decimal_stirng, None);
}

#[test]
fn var_args_eqals() {
    let table = build_table(VAR_ARGS);
    let mut binder = table.create_binder_at(["test", "test"]);

    let fizz_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();

    let call_register_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >("fizz(0)"))
        .into_register()
        .unwrap();

    let call = binder.intermediate_representation.values.registers
        [call_register_id]
        .assignment
        .as_function_call()
        .unwrap();

    assert_eq!(call.callable_id, fizz_id);
    assert_eq!(call.arguments.len(), 1);

    test_integer(&call.arguments[0], "0");
}

#[test]
fn var_args_greater() {
    let table = build_table(VAR_ARGS);
    let mut binder = table.create_binder_at(["test", "test"]);

    let fizz_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();

    let call_register_id = binder
        .bind_as_rvalue_success(&parse::<
            syntax_tree::expression::postfix::Postfixable,
        >(
            "fizz(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
        ))
        .into_register()
        .unwrap();

    let call = binder.intermediate_representation.values.registers
        [call_register_id]
        .assignment
        .as_function_call()
        .unwrap();

    assert_eq!(call.callable_id, fizz_id);
    assert_eq!(call.arguments.len(), 11);

    for i in 0..11 {
        test_integer(&call.arguments[i], &i.to_string());
    }
}

#[test]
fn var_args_lesser() {
    let table = build_table(VAR_ARGS);
    let mut binder = table.create_binder_at(["test", "test"]);

    let fizz_id = table.get_by_qualified_name(["test", "fizz"]).unwrap();

    let (value, error) = binder.bind_as_rvalue_error(&parse::<
        syntax_tree::expression::postfix::Postfixable,
    >("fizz()"));

    let call = binder
        .intermediate_representation
        .values
        .registers
        .get(value.into_register().unwrap())
        .unwrap()
        .assignment
        .as_function_call()
        .unwrap();

    assert_eq!(call.callable_id, fizz_id);

    assert_eq!(error.len(), 1);

    let err =
        error[0].as_any().downcast_ref::<MismatchedArgumentCount>().unwrap();

    assert_eq!(err.expected_count, 1);
    assert_eq!(err.found_count, 0);
    assert_eq!(err.called_id, fizz_id);
}
