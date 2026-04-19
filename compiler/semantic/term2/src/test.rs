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
    predicate::{Compatible, Outlives, Predicate},
    sub_term::{IterSubTerms, Location, RecursivelyIterSubTerms},
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
        r#type::SubLifetimeLocation::Reference
            .get_sub_term(reference.as_ref(), &tracked),
        lifetime,
    );
    assert_eq!(
        r#type::SubTypeLocation::Reference
            .get_sub_term(reference.as_ref(), &tracked),
        pointee,
    );
    assert!(
        r#type::SubTypeLocation::Pointer
            .try_get_sub_term(reference.as_ref(), &tracked)
            .is_none()
    );
}

#[tokio::test]
async fn tuple_range_sub_type_location_returns_interned_tuple() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let first = tracked.intern(Type::Primitive(Primitive::Bool));
    let second = tracked.intern(Type::Primitive(Primitive::Uint32));

    let tuple_type = tracked.intern(Type::Tuple(r#type::Tuple::new(vec![
        crate::tuple::Element::new_regular(first.clone()),
        crate::tuple::Element::new_regular(second),
    ])));

    let sub_term =
        r#type::SubTypeLocation::Tuple(crate::tuple::SubTupleLocation::Range(
            crate::tuple::TupleRange::new(0, 1),
        ))
        .get_sub_term(tuple_type.as_ref(), &tracked);

    let Type::Tuple(tuple) = sub_term.as_ref() else {
        panic!("expected tuple type")
    };

    assert_eq!(tuple.elements().len(), 1);
    assert_eq!(tuple.elements()[0].term(), &first);
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
        constant::SubConstantLocation::Array(0)
            .get_sub_term(array.as_ref(), &tracked),
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
        .get_sub_term(instance.as_ref(), &tracked),
        lifetime,
    );
    assert_eq!(
        crate::instance::SubTypeLocation::InstanceAssociatedGenericArguments(
            SubGenericArgumentsLocation::new(0),
        )
        .get_sub_term(instance.as_ref(), &tracked),
        r#type,
    );
    assert_eq!(
        crate::instance::SubConstantLocation::InstanceAssociatedGenericArguments(
            SubGenericArgumentsLocation::new(0),
        )
        .get_sub_term(instance.as_ref(), &tracked),
        constant,
    );
    assert_eq!(
        crate::instance::SubInstanceLocation::InstanceAssociated(
            crate::instance::SubInstanceAssociatedLocation::Instance,
        )
        .get_sub_term(instance.as_ref(), &tracked),
        parent_instance,
    );
    assert_eq!(
        crate::instance::SubInstanceLocation::InstanceAssociated(
            crate::instance::SubInstanceAssociatedLocation::GenericArguments(
                SubGenericArgumentsLocation::new(0),
            ),
        )
        .get_sub_term(instance.as_ref(), &tracked),
        nested_instance,
    );
}

#[tokio::test]
async fn lifetime_iter_sub_terms_is_empty() {
    let engine = create_test_engine().await;
    let _tracked = engine.tracked().await;

    assert!(Lifetime::Static.iter_sub_terms().next().is_none());
}

#[tokio::test]
async fn type_iter_sub_terms_reference_emits_lifetime_then_type() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let lifetime = tracked.intern(Lifetime::Static);
    let pointee = tracked.intern(Type::Primitive(Primitive::Bool));
    let reference = Type::Reference(Reference::new(
        Qualifier::Immutable,
        lifetime.clone(),
        pointee.clone(),
    ));

    let sub_terms: Vec<_> = reference.iter_sub_terms().collect();
    assert_eq!(sub_terms.len(), 2);

    assert!(matches!(
        sub_terms[0].0,
        crate::TermRef::Lifetime(term) if term == &lifetime
    ));
    assert_eq!(
        sub_terms[0].1,
        r#type::IterSubTermLocation::Lifetime(
            r#type::SubLifetimeLocation::Reference,
        ),
    );

    assert!(matches!(
        sub_terms[1].0,
        crate::TermRef::Type(term) if term == &pointee
    ));
    assert_eq!(
        sub_terms[1].1,
        r#type::IterSubTermLocation::Type(r#type::SubTypeLocation::Reference),
    );
}

#[tokio::test]
async fn constant_iter_sub_terms_tuple_uses_single_locations() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let first = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Bool(true)));
    let second = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Uint32(2)));
    let tuple = constant::Constant::Tuple(constant::Tuple::new(vec![
        crate::tuple::Element::new_regular(first.clone()),
        crate::tuple::Element::new_unpacked(second.clone()),
    ]));

    let sub_terms: Vec<_> = tuple.iter_sub_terms().collect();
    assert_eq!(sub_terms.len(), 2);

    assert!(matches!(
        sub_terms[0].0,
        crate::TermRef::Constant(term) if term == &first
    ));
    assert_eq!(
        sub_terms[0].1,
        constant::SubConstantLocation::Tuple(
            crate::tuple::SubTupleLocation::Single(0,)
        ),
    );

    assert!(matches!(
        sub_terms[1].0,
        crate::TermRef::Constant(term) if term == &second
    ));
    assert_eq!(
        sub_terms[1].1,
        constant::SubConstantLocation::Tuple(
            crate::tuple::SubTupleLocation::Single(1,)
        ),
    );
}

#[tokio::test]
async fn instance_iter_sub_terms_instance_associated_order_is_stable() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(101));
    let nested_symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(102));

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
    let instance = Instance::InstanceAssociated(InstanceAssociated::new(
        parent_instance.clone(),
        symbol_id,
        generic_arguments,
    ));

    let sub_terms: Vec<_> = instance.iter_sub_terms().collect();
    assert_eq!(sub_terms.len(), 5);

    assert!(matches!(
        sub_terms[0].0,
        crate::TermRef::Lifetime(term) if term == &lifetime
    ));
    assert_eq!(
        sub_terms[0].1,
        crate::instance::IterSubTermLocation::Lifetime(
            crate::instance::SubLifetimeLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            ),
        )
        ,
    );

    assert!(matches!(
        sub_terms[1].0,
        crate::TermRef::Type(term) if term == &r#type
    ));
    assert_eq!(
        sub_terms[1].1,
        crate::instance::IterSubTermLocation::Type(
            crate::instance::SubTypeLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            ),
        )
        ,
    );

    assert!(matches!(
        sub_terms[2].0,
        crate::TermRef::Constant(term) if term == &constant
    ));
    assert_eq!(
        sub_terms[2].1,
        crate::instance::IterSubTermLocation::Constant(
            crate::instance::SubConstantLocation::InstanceAssociatedGenericArguments(
                SubGenericArgumentsLocation::new(0),
            ),
        )
        ,
    );

    assert!(matches!(
        sub_terms[3].0,
        crate::TermRef::Instance(term) if term == &nested_instance
    ));
    assert_eq!(
        sub_terms[3].1,
        crate::instance::IterSubTermLocation::Instance(
            crate::instance::SubInstanceLocation::InstanceAssociated(
                crate::instance::SubInstanceAssociatedLocation::GenericArguments(
                    SubGenericArgumentsLocation::new(0),
                ),
            ),
        )
        ,
    );

    assert!(matches!(
        sub_terms[4].0,
        crate::TermRef::Instance(term) if term == &parent_instance
    ));
    assert_eq!(
        sub_terms[4].1,
        crate::instance::IterSubTermLocation::Instance(
            crate::instance::SubInstanceLocation::InstanceAssociated(
                crate::instance::SubInstanceAssociatedLocation::Instance,
            ),
        ),
    );
}

#[tokio::test]
async fn type_recursive_iteration_includes_root_in_depth_first_order() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let element_type = tracked.intern(Type::Primitive(Primitive::Bool));
    let length = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Uint8(3)));
    let array = tracked.intern(Type::Array(r#type::Array::new(
        length.clone(),
        element_type.clone(),
    )));
    let lifetime = tracked.intern(Lifetime::Static);
    let root = tracked.intern(Type::Reference(Reference::new(
        Qualifier::Immutable,
        lifetime.clone(),
        array.clone(),
    )));

    let terms: Vec<_> = root.iter_sub_terms_recursive().collect();
    assert_eq!(terms.len(), 5);

    assert!(matches!(
        terms[0],
        crate::TermRef::Type(term) if term == &root
    ));
    assert!(matches!(
        terms[1],
        crate::TermRef::Lifetime(term) if term == &lifetime
    ));
    assert!(matches!(
        terms[2],
        crate::TermRef::Type(term) if term == &array
    ));
    assert!(matches!(
        terms[3],
        crate::TermRef::Type(term) if term == &element_type
    ));
    assert!(matches!(
        terms[4],
        crate::TermRef::Constant(term) if term == &length
    ));
}

#[tokio::test]
async fn constant_recursive_iteration_includes_root_in_depth_first_order() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let inner = tracked
        .intern(constant::Constant::Primitive(constant::Primitive::Bool(true)));
    let tuple =
        tracked.intern(constant::Constant::Tuple(constant::Tuple::new(vec![
            crate::tuple::Element::new_regular(inner.clone()),
        ])));
    let root =
        tracked.intern(constant::Constant::Array(constant::Array::new(vec![
            tuple.clone(),
        ])));

    let terms: Vec<_> = root.iter_sub_terms_recursive().collect();
    assert_eq!(terms.len(), 3);

    assert!(matches!(
        terms[0],
        crate::TermRef::Constant(term) if term == &root
    ));
    assert!(matches!(
        terms[1],
        crate::TermRef::Constant(term) if term == &tuple
    ));
    assert!(matches!(
        terms[2],
        crate::TermRef::Constant(term) if term == &inner
    ));
}

#[tokio::test]
async fn predicate_contains_error_detects_nested_error_terms() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let lifetime = tracked.intern(Lifetime::Static);
    let pointee = tracked.intern(Type::Error(crate::error::Error));
    let operand = tracked.intern(Type::Reference(Reference::new(
        Qualifier::Immutable,
        lifetime.clone(),
        pointee,
    )));

    let predicate = Predicate::TypeOutlives(Outlives::new(operand, lifetime));

    assert!(predicate.contains_error());
}

#[tokio::test]
async fn predicate_contains_error_detects_instance_associated_type_equality_errors()
 {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(201));
    let parent_instance =
        tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(symbol_id)));
    let error_type = tracked.intern(Type::Error(crate::error::Error));
    let generic_arguments = tracked.intern(GenericArguments::new(
        vec![],
        vec![error_type],
        vec![],
        vec![],
    ));
    let lhs =
        InstanceAssociated::new(parent_instance, symbol_id, generic_arguments);
    let rhs = tracked.intern(Type::Primitive(Primitive::Bool));
    let predicate =
        Predicate::InstanceAssociatedTypeEquality(Compatible::new(lhs, rhs));

    assert!(predicate.contains_error());
}

#[tokio::test]
async fn instance_recursive_iteration_includes_root_in_depth_first_order() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(201));
    let nested_symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(202));

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
    let root =
        tracked.intern(Instance::InstanceAssociated(InstanceAssociated::new(
            parent_instance.clone(),
            symbol_id,
            generic_arguments,
        )));

    let terms: Vec<_> = root.iter_sub_terms_recursive().collect();
    assert_eq!(terms.len(), 6);

    assert!(matches!(
        terms[0],
        crate::TermRef::Instance(term) if term == &root
    ));
    assert!(matches!(
        terms[1],
        crate::TermRef::Lifetime(term) if term == &lifetime
    ));
    assert!(matches!(
        terms[2],
        crate::TermRef::Type(term) if term == &r#type
    ));
    assert!(matches!(
        terms[3],
        crate::TermRef::Constant(term) if term == &constant
    ));
    assert!(matches!(
        terms[4],
        crate::TermRef::Instance(term) if term == &nested_instance
    ));
    assert!(matches!(
        terms[5],
        crate::TermRef::Instance(term) if term == &parent_instance
    ));
}

#[tokio::test]
async fn lifetime_recursive_iteration_includes_root_only() {
    let engine = create_test_engine().await;
    let tracked = engine.tracked().await;

    let root = tracked.intern(Lifetime::Static);
    let terms: Vec<_> = root.iter_sub_terms_recursive().collect();

    assert_eq!(terms.len(), 1);
    assert!(matches!(
        terms[0],
        crate::TermRef::Lifetime(term) if term == &root
    ));
}
