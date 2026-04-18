use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory};
use pernixc_symbol::SymbolID;
use pernixc_target::TargetID;
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};

use crate::{
    constant,
    generic_arguments::{
        GenericArguments, SubGenericArgumentsLocation, Symbol,
    },
    generic_parameters::{
        GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    instance::{AnoymousTrait, Instance, InstanceAssociated},
    lifetime::Lifetime,
    sub_term::Location,
    r#type::{self, Primitive, Qualifier, Reference, Type},
};

async fn create_test_engine() -> Arc<Engine> {
    Arc::new(
        Engine::new_with(
            Plugin::default(),
            InMemoryFactory,
            SeededStableHasherBuilder::new(0),
        )
        .await
        .unwrap(),
    )
}

#[tokio::test]
async fn reference_type_stores_interned_children() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let lifetime = tracked.intern(Lifetime::Static);
    let pointee = tracked.intern(Type::Primitive(Primitive::Bool));
    let reference = tracked.intern(Type::Reference(Reference::new(
        Qualifier::Immutable,
        lifetime.clone(),
        pointee.clone(),
    )));

    let Type::Reference(reference) = reference.as_ref() else {
        panic!("expected reference type");
    };

    assert_eq!(reference.lifetime(), &lifetime);
    assert_eq!(reference.pointee(), &pointee);
}

#[tokio::test]
async fn identity_generic_arguments_are_interned() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let mut generic_parameters = GenericParameters::default();
    let lifetime_id = generic_parameters
        .add_lifetime_parameter(LifetimeParameter::new(
            tracked.intern_unsized("a".to_owned()),
            None,
        ))
        .unwrap();
    let type_id = generic_parameters
        .add_type_parameter(TypeParameter::new(
            tracked.intern_unsized("T".to_owned()),
            None,
        ))
        .unwrap();

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(7));
    let generic_arguments = generic_parameters
        .create_identity_generic_arguments(symbol_id, &tracked);

    assert_eq!(
        generic_arguments.lifetimes()[0].as_ref(),
        &Lifetime::Parameter(LifetimeParameterID::new(symbol_id, lifetime_id)),
    );
    assert_eq!(
        generic_arguments.types()[0].as_ref(),
        &Type::Parameter(TypeParameterID::new(symbol_id, type_id)),
    );

    let symbol = Symbol::new(symbol_id, generic_arguments.clone());
    assert_eq!(symbol.generic_arguments(), &generic_arguments);
}

#[tokio::test]
async fn single_type_generic_arguments_keep_interned_type() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
    let generic_arguments = GenericArguments::new_single_type(r#type.clone());

    assert_eq!(generic_arguments.types(), &[r#type]);
    assert!(generic_arguments.lifetimes().is_empty());
    assert!(generic_arguments.constants().is_empty());
    assert!(generic_arguments.instances().is_empty());
}

#[tokio::test]
async fn type_sub_term_locations_return_interned_children() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let lifetime = tracked.intern(Lifetime::Static);
    let pointee = tracked.intern(Type::Primitive(Primitive::Bool));
    let reference = tracked.intern(Type::Reference(Reference::new(
        Qualifier::Immutable,
        lifetime.clone(),
        pointee.clone(),
    )));

    assert_eq!(
        r#type::SubLifetimeLocation::Reference.get_sub_term(reference.as_ref()),
        lifetime,
    );
    assert_eq!(
        r#type::SubTypeLocation::Reference.get_sub_term(reference.as_ref()),
        pointee,
    );
    assert!(
        r#type::SubTypeLocation::Pointer
            .try_get_sub_term(reference.as_ref())
            .is_none()
    );
}

#[tokio::test]
async fn constant_sub_term_locations_return_interned_children() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let element = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Uint8(1)));
    let array =
        tracked.intern(constant::Constant::Array(constant::Array::new(vec![
            element.clone(),
        ])));

    assert_eq!(
        constant::SubConstantLocation::Array(0).get_sub_term(array.as_ref()),
        element,
    );
}

#[tokio::test]
async fn instance_sub_term_locations_return_interned_children() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(11));
    let nested_symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(12));

    let lifetime = tracked.intern(Lifetime::Static);
    let r#type = tracked.intern(Type::Primitive(Primitive::Uint32));
    let constant = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Bool(true)));
    let parent_instance =
        tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
    let nested_instance = tracked
        .intern(Instance::AnonymousTrait(AnoymousTrait::new(nested_symbol_id)));
    let generic_arguments = tracked.intern(GenericArguments::new(
        vec![lifetime.clone()],
        vec![r#type.clone()],
        vec![constant.clone()],
        vec![nested_instance.clone()],
    ));
    let instance =
        tracked.intern(Instance::InstanceAssociated(InstanceAssociated::new(
            parent_instance.clone(),
            symbol_id,
            generic_arguments,
        )));

    assert_eq!(
        crate::instance::SubLifetimeLocation::InstanceAssociatedGenericArguments(
            SubGenericArgumentsLocation::new(0),
        )
        .get_sub_term(instance.as_ref()),
        lifetime,
    );
    assert_eq!(
        crate::instance::SubTypeLocation::InstanceAssociatedGenericArguments(
            SubGenericArgumentsLocation::new(0),
        )
        .get_sub_term(instance.as_ref()),
        r#type,
    );
    assert_eq!(
        crate::instance::SubConstantLocation::InstanceAssociatedGenericArguments(
            SubGenericArgumentsLocation::new(0),
        )
        .get_sub_term(instance.as_ref()),
        constant,
    );
    assert_eq!(
        crate::instance::SubInstanceLocation::InstanceAssociated(
            crate::instance::SubInstanceAssociatedLocation::Instance,
        )
        .get_sub_term(instance.as_ref()),
        parent_instance,
    );
    assert_eq!(
        crate::instance::SubInstanceLocation::InstanceAssociated(
            crate::instance::SubInstanceAssociatedLocation::GenericArguments(
                SubGenericArgumentsLocation::new(0),
            ),
        )
        .get_sub_term(instance.as_ref()),
        nested_instance,
    );
}
