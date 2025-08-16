use std::{borrow::Cow, sync::Arc};

use pernixc_query::Engine;
use pernixc_symbol::kind::Kind;
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{
        GenericParameters, LifetimeParameterID, TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::PositiveTrait,
    r#type::{Primitive, Qualifier, Reference, Type},
    tuple::{self, Tuple},
};

use crate::{
    environment::{Environment, Premise},
    normalizer,
};

#[tokio::test]
async fn single_implementation() {
    // given a trait predicate `Trait[&'static (int32,)]` with a single
    // implements `implements['a, T] Trait[&'a T]`

    let tuple_with_i32 = Type::Tuple(Tuple {
        elements: vec![tuple::Element {
            term: Type::Primitive(Primitive::Int32),
            is_unpacked: false,
        }],
    });

    let ref_tuple_with_i32 = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(tuple_with_i32.clone()),
    });

    let implements_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(0));
    let trait_id = Global::new(TargetID::Extern(1), pernixc_symbol::ID(0));

    let mut engine = Arc::new(Engine::default());

    Arc::get_mut(&mut engine).unwrap().input_session(|x| {
        x.set_input(
            pernixc_symbol::kind::Key(implements_id),
            Kind::PositiveTraitImplementation,
        );
        x.set_input(pernixc_symbol::kind::Key(trait_id), Kind::Trait);
        x.set_input(pernixc_symbol::implements::Key(implements_id), trait_id);
        x.set_input(
            pernixc_symbol::final_implements::Key(implements_id),
            false,
        );
        x.set_input(
            pernixc_term::where_clause::Key(implements_id),
            Arc::default(),
        );
        x.set_input(
            pernixc_symbol::implemented::Key(trait_id),
            Arc::new(std::iter::once(implements_id).collect()),
        );

        let mut trait_generic_param = GenericParameters::default();
        trait_generic_param
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();

        x.set_input(
            pernixc_term::generic_parameters::Key(trait_id),
            Arc::new(trait_generic_param),
        );

        let mut impl_generic_param = GenericParameters::default();
        let impl_a = impl_generic_param
            .add_lifetime_parameter(
                pernixc_term::generic_parameters::LifetimeParameter {
                    name: "a".to_string(),
                    span: None,
                },
            )
            .unwrap();
        let impl_t = impl_generic_param
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();

        x.set_input(
            pernixc_term::generic_parameters::Key(implements_id),
            Arc::new(impl_generic_param),
        );

        x.set_input(
            pernixc_term::implements_argument::Key(implements_id),
            GenericArguments {
                lifetimes: vec![],
                types: vec![Type::Reference(Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: Lifetime::Parameter(LifetimeParameterID::new(
                        implements_id,
                        impl_a,
                    )),
                    pointee: Box::new(Type::Parameter(TypeParameterID::new(
                        trait_id, impl_t,
                    ))),
                })],
                constants: vec![],
            },
        );
    });

    let predicate = PositiveTrait::new(trait_id, false, GenericArguments {
        lifetimes: vec![],
        types: vec![ref_tuple_with_i32],
        constants: vec![],
    });

    let environment = Environment::new(
        Cow::Owned(Premise::default()),
        Cow::Owned(engine.tracked()),
        normalizer::NO_OP,
    );

    let result = environment.query(&predicate).await.unwrap().unwrap();

    assert!(result.constraints.is_empty());

    let implementation = result.result.as_implementation().unwrap();

    assert!(!implementation.is_not_general_enough);
    assert_eq!(implementation.id, implements_id);
}
