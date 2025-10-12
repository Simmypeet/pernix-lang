//! Defines and implements the `Copy` marker in the core library.

use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_query::Engine;
use pernixc_semantic_element::{
    implemented, implements, implements_arguments,
    where_clause::{self, Predicate},
};
use pernixc_symbol::{
    accessibility, calculate_implements_id_by_unique_name,
    calculate_qualified_name_id, get_target_root_module_id, kind, member, name,
    parent,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{
        self, GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate,
    r#type::{Primitive, Type},
};

#[allow(missing_docs)]
pub const MARKER_NAME: &str = "Copy";
#[allow(missing_docs)]
pub const MARKER_SEQUENCE: [&str; 2] = ["core", "Copy"];

/// Creates a `Copy` marker in the core library and implements it for all
/// primitive types, raw pointers, and immutable references.
///
/// ```txt
/// public marker Copy[T]
///
/// implements Copy[{primitive types}]
///
/// implements Copy[*T]
/// implements Copy[*mut T]
///
/// implements Copy[&'a T] delete:
///     where:
///         T: 'a    
/// ```
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
pub async fn initialize_copy_marker(
    engine: &mut Arc<Engine>,
) -> pernixc_symbol::ID {
    let (root_target_module_id, copy_marker_id) = {
        let tracked_engine = engine.tracked();

        let root_target_module_id =
            tracked_engine.get_target_root_module_id(TargetID::CORE).await;

        let copy_marker_id = TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    MARKER_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        );

        (root_target_module_id, copy_marker_id)
    };

    Arc::get_mut(engine)
        .unwrap()
        .input_session(async move |x| {
            x.set_input(kind::Key(copy_marker_id), kind::Kind::Marker).await;
            x.set_input(name::Key(copy_marker_id), MARKER_NAME.into()).await;
            x.set_input(
                parent::Key(copy_marker_id),
                Some(root_target_module_id),
            )
            .await;
            x.set_input(
                accessibility::Key(copy_marker_id),
                accessibility::Accessibility::Public,
            )
            .await;
            x.set_input(where_clause::Key(copy_marker_id), Arc::default())
                .await;

            let mut generic_params = GenericParameters::default();

            generic_params
                .add_type_parameter(
                    pernixc_term::generic_parameters::TypeParameter {
                        name: "T".into(),
                        span: None,
                    },
                )
                .unwrap();

            x.set_input(
                generic_parameters::Key(copy_marker_id),
                Arc::new(generic_params),
            )
            .await;
        })
        .await;

    let mut implemented = HashSet::default();

    for primitive in [
        Primitive::Int8,
        Primitive::Int16,
        Primitive::Int32,
        Primitive::Int64,
        Primitive::Uint8,
        Primitive::Uint16,
        Primitive::Uint32,
        Primitive::Uint64,
        Primitive::Float32,
        Primitive::Float64,
        Primitive::Bool,
        Primitive::Isize,
        Primitive::Usize,
    ] {
        let impl_id = get_impl_id(engine, &primitive.to_string()).await;
        implements_copy_marker(
            engine,
            kind::Kind::PositiveImplementation,
            Type::Primitive(primitive),
            impl_id,
            copy_marker_id,
            Arc::new(GenericParameters::default()),
            Arc::default(),
        )
        .await;

        implemented.insert(impl_id);
    }

    // immutable reference of any type can be copied
    {
        let impl_id = get_impl_id(engine, "&T").await;
        let mut generic_parameters = GenericParameters::default();
        let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
            impl_id,
            generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: "a".into(),
                    span: None,
                })
                .unwrap(),
        ));
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".into(),
                    span: None,
                })
                .unwrap(),
        ));
        let where_clause = Arc::new([Predicate {
            predicate: predicate::Predicate::type_outlives(
                t_ty.clone(),
                a_lt.clone(),
            ),
            span: None,
        }]);

        implements_copy_marker(
            engine,
            kind::Kind::PositiveImplementation,
            Type::Reference(pernixc_term::r#type::Reference {
                qualifier: pernixc_term::r#type::Qualifier::Immutable,
                lifetime: a_lt,
                pointee: Box::new(t_ty),
            }),
            impl_id,
            copy_marker_id,
            Arc::new(generic_parameters),
            where_clause,
        )
        .await;

        implemented.insert(impl_id);
    }

    // mutable reference can't be copied
    {
        let impl_id = get_impl_id(engine, "&mut T").await;
        let mut generic_parameters = GenericParameters::default();
        let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
            impl_id,
            generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: "a".into(),
                    span: None,
                })
                .unwrap(),
        ));
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".into(),
                    span: None,
                })
                .unwrap(),
        ));
        let where_clause = Arc::new([Predicate {
            predicate: predicate::Predicate::type_outlives(
                t_ty.clone(),
                a_lt.clone(),
            ),
            span: None,
        }]);

        implements_copy_marker(
            engine,
            kind::Kind::NegativeImplementation,
            Type::Reference(pernixc_term::r#type::Reference {
                qualifier: pernixc_term::r#type::Qualifier::Mutable,
                lifetime: a_lt,
                pointee: Box::new(t_ty),
            }),
            impl_id,
            copy_marker_id,
            Arc::new(generic_parameters),
            where_clause,
        )
        .await;

        implemented.insert(impl_id);
    }

    // mutable/immutable pointer of any type can be copied
    let mut impl_pointer = async |mutable| {
        let impl_id =
            get_impl_id(engine, if mutable { "*mut T" } else { "*const T" })
                .await;
        let mut generic_parameters = GenericParameters::default();
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".into(),
                    span: None,
                })
                .unwrap(),
        ));
        let where_clause = Arc::new([Predicate {
            predicate: predicate::Predicate::type_outlives(
                t_ty.clone(),
                Lifetime::Static,
            ),
            span: None,
        }]);

        implements_copy_marker(
            engine,
            kind::Kind::PositiveImplementation,
            Type::Pointer(pernixc_term::r#type::Pointer {
                mutable,
                pointee: Box::new(t_ty),
            }),
            impl_id,
            copy_marker_id,
            Arc::new(generic_parameters),
            where_clause,
        )
        .await;

        implemented.insert(impl_id);
    };

    impl_pointer(false).await;
    impl_pointer(true).await;

    Arc::get_mut(engine)
        .unwrap()
        .input_session(async move |x| {
            x.set_input(
                implemented::InTargetKey {
                    implementable_id: copy_marker_id,
                    target_id: TargetID::CORE,
                },
                Arc::new(implemented),
            )
            .await;
        })
        .await;

    copy_marker_id.id
}

async fn get_impl_id(
    engine: &mut Arc<Engine>,
    ty_name: &str,
) -> Global<pernixc_symbol::ID> {
    TargetID::CORE.make_global(
        engine
            .tracked()
            .calculate_implements_id_by_unique_name(
                &format!("core::Copy[{ty_name}]"),
                TargetID::CORE,
            )
            .await,
    )
}

async fn implements_copy_marker(
    engine: &mut Arc<Engine>,
    kind: kind::Kind,
    rt: Type,
    impl_id: Global<pernixc_symbol::ID>,
    copy_marker_id: Global<pernixc_symbol::ID>,
    generic_parameters: Arc<GenericParameters>,
    where_clause: Arc<[Predicate]>,
) {
    Arc::get_mut(engine)
        .unwrap()
        .input_session(async move |x| {
            x.set_input(kind::Key(impl_id), kind).await;
            x.set_input(generic_parameters::Key(impl_id), generic_parameters)
                .await;
            x.set_input(implements::Key(impl_id), Some(copy_marker_id)).await;
            x.set_input(where_clause::Key(impl_id), where_clause).await;
            x.set_input(implements_arguments::Key(impl_id), {
                let mut args = GenericArguments::default();
                args.types.push(rt);
                Some(Arc::new(args))
            })
            .await;
            x.set_input(member::Key(impl_id), Arc::default()).await;
        })
        .await;
}
