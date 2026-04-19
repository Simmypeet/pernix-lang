use pernixc_symbol::SymbolID;
use pernixc_target::TargetID;

use super::Instantiation;
use crate::{
    constant::{self, Constant},
    generic_arguments::GenericArguments,
    generic_parameters::{
        ConstantParameter, GenericParameters, InstanceParameter,
        LifetimeParameter, TypeParameter,
    },
    instance::Instance,
    lifetime::Lifetime,
    test_support::create_test_engine,
    r#type::{Primitive, Qualifier, Reference, Type},
};

#[tokio::test]
async fn round_trips_generic_arguments() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let mut parameters = GenericParameters::default();
    let lifetime_id = parameters
        .add_lifetime_parameter(LifetimeParameter::new(
            tracked.intern_unsized("a".to_owned()),
            None,
        ))
        .unwrap();
    let type_id = parameters
        .add_type_parameter(TypeParameter::new(
            tracked.intern_unsized("T".to_owned()),
            None,
        ))
        .unwrap();
    let constant_id = parameters
        .add_constant_parameter(ConstantParameter::new(
            tracked.intern_unsized("N".to_owned()),
            tracked.intern(Type::Primitive(Primitive::Uint32)),
            None,
        ))
        .unwrap();
    let instance_id = parameters
        .add_instance_parameter(InstanceParameter::new(
            tracked.intern_unsized("I".to_owned()),
            None,
            None,
        ))
        .unwrap();

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(300));
    let arguments = GenericArguments::new(
        vec![tracked.intern(Lifetime::Static)],
        vec![tracked.intern(Type::Primitive(Primitive::Bool))],
        vec![
            tracked
                .intern(Constant::Primitive(constant::Primitive::Bool(true))),
        ],
        vec![tracked.intern(Instance::new_anonymous_trait(
            TargetID::TEST.make_global(SymbolID::from_u128(301)),
        ))],
    );

    let mut instantiation = Instantiation::default();
    instantiation.append_from_generic_arguments(
        &arguments,
        symbol_id,
        &parameters,
        &tracked,
    );

    let lifetime_parameter =
        tracked.intern(Lifetime::new_parameter(symbol_id, lifetime_id));
    let type_parameter =
        tracked.intern(Type::new_parameter(symbol_id, type_id));
    let constant_parameter =
        tracked.intern(Constant::new_parameter(symbol_id, constant_id));
    let instance_parameter =
        tracked.intern(Instance::new_parameter(symbol_id, instance_id));

    assert_eq!(
        instantiation.get_lifetime_mapping(&lifetime_parameter),
        Some(&arguments.lifetimes()[0]),
    );
    assert_eq!(
        instantiation.get_type_mapping(&type_parameter),
        Some(&arguments.types()[0]),
    );
    assert_eq!(
        instantiation.get_constant_mapping(&constant_parameter),
        Some(&arguments.constants()[0]),
    );
    assert_eq!(
        instantiation.get_instance_mapping(&instance_parameter),
        Some(&arguments.instances()[0]),
    );

    let recreated = instantiation.create_generic_arguments(
        symbol_id,
        &parameters,
        &tracked,
    );

    assert_eq!(recreated.as_ref(), &arguments);
}

#[tokio::test]
async fn rewrites_nested_terms() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let static_lifetime = tracked.intern(Lifetime::Static);
    let erased_lifetime = tracked.intern(Lifetime::Erased);
    let bool_type = tracked.intern(Type::Primitive(Primitive::Bool));
    let uint_type = tracked.intern(Type::Primitive(Primitive::Uint32));

    let mut instantiation = Instantiation::default();
    instantiation
        .insert_lifetime_mapping(static_lifetime.clone(), erased_lifetime);
    instantiation.insert_type_mapping(bool_type.clone(), uint_type);

    let mut term = tracked.intern(Type::Reference(Reference::new(
        Qualifier::Immutable,
        static_lifetime,
        tracked.intern(Type::Reference(Reference::new(
            Qualifier::Immutable,
            tracked.intern(Lifetime::Static),
            bool_type,
        ))),
    )));

    instantiation.instantiate(&mut term, &tracked).unwrap();

    let Type::Reference(outer_reference) = term.as_ref() else {
        panic!("expected outer reference");
    };

    assert!(matches!(outer_reference.lifetime().as_ref(), Lifetime::Erased,));

    let Type::Reference(inner_reference) = outer_reference.pointee().as_ref()
    else {
        panic!("expected inner reference");
    };

    assert!(matches!(inner_reference.lifetime().as_ref(), Lifetime::Erased,));
    assert!(matches!(
        inner_reference.pointee().as_ref(),
        Type::Primitive(Primitive::Uint32),
    ));
}

#[tokio::test]
async fn rewrites_instantiation_values() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let bool_type = tracked.intern(Type::Primitive(Primitive::Bool));
    let static_lifetime = tracked.intern(Lifetime::Static);

    let mut mapped_values = Instantiation::default();
    mapped_values.insert_type_mapping(
        bool_type,
        tracked.intern(Type::Reference(Reference::new(
            Qualifier::Immutable,
            static_lifetime,
            tracked.intern(Type::Primitive(Primitive::Bool)),
        ))),
    );

    let mut replacement = Instantiation::default();
    replacement.insert_lifetime_mapping(
        tracked.intern(Lifetime::Static),
        tracked.intern(Lifetime::Erased),
    );
    replacement.insert_type_mapping(
        tracked.intern(Type::Primitive(Primitive::Bool)),
        tracked.intern(Type::Primitive(Primitive::Uint32)),
    );

    mapped_values.instantiate_values(&replacement, &tracked).unwrap();

    let updated = mapped_values
        .get_type_mapping(&tracked.intern(Type::Primitive(Primitive::Bool)))
        .unwrap();

    let Type::Reference(reference) = updated.as_ref() else {
        panic!("expected rewritten reference type");
    };

    assert!(matches!(reference.lifetime().as_ref(), Lifetime::Erased));
    assert!(matches!(
        reference.pointee().as_ref(),
        Type::Primitive(Primitive::Uint32),
    ));
}
