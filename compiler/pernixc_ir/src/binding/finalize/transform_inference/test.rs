use pernixc_handler::{Panic, Storage};
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_syntax::utility::parse;
use pernixc_term::{
    generic_arguments::GenericArguments,
    r#type::{Primitive, Type},
    Symbol,
};

use crate::{
    binding::{
        diagnostic::TypeAnnotationRequired,
        test::{build_table, CreateBinderAtExt},
    },
    model::Constraint,
};

const OPTION_DECLARATION: &str = r"
public enum Option[T]:
    Some(T)
    None


public function test():
    pass
";

#[test]
fn none_option() {
    let table = build_table(OPTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let storage = Storage::<Box<dyn Diagnostic>>::new();

    assert!(binder
        .bind_statement(&parse("let x = Option::None;"), &Panic)
        .is_ok());
    assert!(binder.finalize(&storage).is_ok());

    let vec = storage.as_vec();

    assert_eq!(vec.len(), 1);

    let enum_id = table.get_by_qualified_name(["test", "Option"]).unwrap();

    let error =
        vec[0].as_any().downcast_ref::<TypeAnnotationRequired>().unwrap();

    assert_eq!(error.span.str(), "Option::None");
    assert_eq!(
        error.r#type,
        Type::Symbol(Symbol {
            id: enum_id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Inference(Constraint::All(false))],
                constants: Vec::new()
            }
        })
    );
}

const FUNCTION_DECLARATION: &str = r"
public function foo[T, U](t: T):
    pass


public function test():
    pass
";

#[test]
fn function_with_no_generic_parameter() {
    let table = build_table(FUNCTION_DECLARATION);
    let mut binder = table.create_binder_at(["test", "test"]);

    let foo_id = table.get_by_qualified_name(["test", "foo"]).unwrap();

    let storage = Storage::<Box<dyn Diagnostic>>::new();

    assert!(binder.bind_statement(&parse("let x = foo(32);"), &Panic).is_ok());
    assert!(binder.finalize(&storage).is_ok());

    let vec = storage.as_vec();

    assert_eq!(vec.len(), 1);

    let error =
        vec[0].as_any().downcast_ref::<TypeAnnotationRequired>().unwrap();

    assert_eq!(error.span.str(), "foo(32)");
    assert_eq!(
        error.r#type,
        Type::Symbol(Symbol {
            id: foo_id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![
                    Type::Primitive(Primitive::Int32),
                    Type::Inference(Constraint::All(false))
                ],
                constants: Vec::new()
            }
        })
    );
}

const FUNCTION_DECLARATION_WITH_LIFETIME: &str = r"
public function foo['a, T](t: & 'a T):
    pass


public function test():
    pass
";

#[test]
fn lifetime_inference_is_not_error() {
    let table = build_table(FUNCTION_DECLARATION_WITH_LIFETIME);
    let mut binder = table.create_binder_at(["test", "test"]);

    assert!(binder.bind_statement(&parse("let x = foo(&32);"), &Panic).is_ok());
    assert!(binder.finalize(&Panic).is_ok());
}
