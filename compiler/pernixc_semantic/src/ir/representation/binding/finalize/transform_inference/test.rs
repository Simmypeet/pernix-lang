use pernixc_base::handler::Panic;

use crate::{
    error,
    ir::representation::binding::test::{parse_statement, TestTemplate},
    symbol::{
        table::representation::IndexMut, Accessibility, AdtID, AdtTemplate,
        EnumDefinition, FunctionDefinition, FunctionTemplate,
        GenericDeclaration, LifetimeParameter, LifetimeParameterID, Parameter,
        TypeParameter, TypeParameterID,
    },
    type_system::term::{
        lifetime::Lifetime,
        r#type::{Constraint, Primitive, Qualifier, Reference, SymbolID, Type},
        GenericArguments, Symbol,
    },
};

#[test]
fn none_option() {
    let mut test_template = TestTemplate::new();
    let enum_id = test_template
        .table
        .insert_member(
            "Option".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
            GenericDeclaration::default(),
            AdtTemplate::<EnumDefinition>::default(),
        )
        .unwrap()
        .unwrap_no_duplication();

    let enum_sym = test_template.table.get_mut(enum_id).unwrap();
    let ty_id = enum_sym
        .generic_declaration
        .parameters
        .add_type_parameter(TypeParameter {
            name: Some("T".to_string()),
            span: None,
        })
        .unwrap();

    test_template.table.insert_variant("None".to_string(), enum_id, None, None);

    test_template.table.insert_variant(
        "Some".to_string(),
        enum_id,
        Some(Type::Parameter(TypeParameterID {
            parent: enum_id.into(),
            id: ty_id,
        })),
        None,
    );

    let (mut binder, storage) = test_template.create_binder();

    assert!(binder
        .bind_statement(&parse_statement("let x = Option::None;"), &Panic)
        .is_ok());
    assert!(binder.finalize(&storage).is_err());

    let vec = storage.as_vec();

    assert_eq!(vec.len(), 1);

    let error = vec[0]
        .as_any()
        .downcast_ref::<error::TypeAnnotationRequired>()
        .unwrap();

    assert_eq!(error.span.str(), "Option::None");
    assert_eq!(
        error.r#type,
        Type::Symbol(Symbol {
            id: SymbolID::Adt(AdtID::Enum(enum_id)),
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Inference(Constraint::All(false))],
                constants: Vec::new()
            }
        })
    );
}

#[test]
fn function_with_no_generic_parameter() {
    let mut test_template = TestTemplate::new();

    let function_id = test_template
        .table
        .insert_member(
            "foo".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
            GenericDeclaration::default(),
            FunctionTemplate::<FunctionDefinition>::default(),
        )
        .unwrap()
        .unwrap_no_duplication();

    let function_sym = test_template.table.get_mut(function_id).unwrap();

    let t_id = function_sym
        .generic_declaration
        .parameters
        .add_type_parameter(TypeParameter {
            name: Some("T".to_string()),
            span: None,
        })
        .unwrap();

    function_sym
        .generic_declaration
        .parameters
        .add_type_parameter(TypeParameter {
            name: Some("U".to_string()),
            span: None,
        })
        .unwrap();

    function_sym.insert_parameter(Parameter {
        r#type: Type::Parameter(TypeParameterID {
            parent: function_id.into(),
            id: t_id,
        }),
        span: None,
    });

    let (mut binder, storage) = test_template.create_binder();

    assert!(binder
        .bind_statement(&parse_statement("let x = foo(32);"), &Panic)
        .is_ok());
    assert!(binder.finalize(&storage).is_err());

    let vec = storage.as_vec();

    assert_eq!(vec.len(), 1);

    let error = vec[0]
        .as_any()
        .downcast_ref::<error::TypeAnnotationRequired>()
        .unwrap();

    assert_eq!(error.span.str(), "foo(32)");
    assert_eq!(
        error.r#type,
        Type::Symbol(Symbol {
            id: SymbolID::Function(function_id),
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

#[test]
fn lifetime_inference_is_not_error() {
    let mut test_template = TestTemplate::new();

    let function_id = test_template
        .table
        .insert_member(
            "foo".to_string(),
            Accessibility::Public,
            test_template.test_module_id,
            None,
            GenericDeclaration::default(),
            FunctionTemplate::<FunctionDefinition>::default(),
        )
        .unwrap()
        .unwrap_no_duplication();

    let function_sym = test_template.table.get_mut(function_id).unwrap();

    let lt_id = function_sym
        .generic_declaration
        .parameters
        .add_lifetime_parameter(LifetimeParameter {
            name: Some("a".to_string()),
            span: None,
        })
        .unwrap();

    let t_id = function_sym
        .generic_declaration
        .parameters
        .add_type_parameter(TypeParameter {
            name: Some("T".to_string()),
            span: None,
        })
        .unwrap();

    function_sym.insert_parameter(Parameter {
        r#type: Type::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime: Lifetime::Parameter(LifetimeParameterID {
                parent: function_id.into(),
                id: lt_id,
            }),
            pointee: Box::new(Type::Parameter(TypeParameterID {
                parent: function_id.into(),
                id: t_id,
            })),
        }),
        span: None,
    });

    let (mut binder, storage) = test_template.create_binder();

    assert!(binder
        .bind_statement(&parse_statement("let x = foo(&32);"), &Panic)
        .is_ok());

    assert!(binder.finalize(&storage).is_ok(), "{:?}", storage.as_vec());
}
