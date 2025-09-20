use std::sync::Arc;

use pernixc_query::Engine;
use pernixc_symbol::{calculate_qualified_name_id, get_target_root_module_id};
use pernixc_target::TargetID;

#[allow(missing_docs)]
pub const MARKER_NAME: &str = "Copy";
#[allow(missing_docs)]
pub const MARKER_SEQUENCE: [&str; 2] = ["core", "Copy"];

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

    assert!(table.add_component(copy_marker_id, SymbolKind::Marker));
    assert!(table.add_component(copy_marker_id, Name("Copy".to_string())));
    assert!(table.add_component(copy_marker_id, Parent {
        parent: Some(pernixc_table::ID::ROOT_MODULE)
    }));
    assert!(table.add_component(copy_marker_id, {
        let mut generic_params = GenericParameters::default();
        assert!(generic_params
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .is_ok());
        generic_params
    }));
    assert!(table.add_component(copy_marker_id, WhereClause::default()));
    assert!(table.add_component(copy_marker_id, ForallLifetimes::default()));
    assert!(table.add_component(copy_marker_id, Accessibility::Public));

    // add to the core module
    assert!(core_member
        .insert("Copy".to_string(), copy_marker_id.id)
        .is_none());

    let mut implemented = Implemented::default();

    for primitive in Primitive::iter() {
        let impl_id = GlobalID::new(
            TargetID::CORE,
            pernixc_table::ID(id_gen.next().unwrap()),
        );

        assert!(table
            .add_component(impl_id, SymbolKind::PositiveMarkerImplementation));
        assert!(table.add_component(impl_id, GenericParameters::default()));
        assert!(table.add_component(impl_id, Implements(copy_marker_id)));
        assert!(table.add_component(impl_id, WhereClause::default()));
        assert!(table.add_component(impl_id, {
            let mut implementation = Implementation::default();
            implementation
                .generic_arguments
                .types
                .push(Type::Primitive(primitive));

            implementation
        }));
        assert!(table.add_component(impl_id, ForallLifetimes::default()));

        implemented.insert(impl_id);
    }

    // mutable reference can't be copied
    {
        let impl_id = GlobalID::new(
            TargetID::CORE,
            pernixc_table::ID(id_gen.next().unwrap()),
        );
        let mut generic_parameters = GenericParameters::default();
        let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
            impl_id,
            generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: "a".to_string(),
                    span: None,
                })
                .unwrap(),
        ));
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".to_string(),
                    span: None,
                })
                .unwrap(),
        ));

        assert!(table
            .add_component(impl_id, SymbolKind::NegativeMarkerImplementation));
        assert!(table.add_component(impl_id, generic_parameters));
        assert!(table.add_component(impl_id, Implements(copy_marker_id)));
        assert!(table.add_component(impl_id, ForallLifetimes::default()));
        assert!(table.add_component(impl_id, {
            let mut where_clause = WhereClause::default();
            where_clause.predicates.push(Predicate {
                predicate: predicate::Predicate::TypeOutlives(Outlives::new(
                    t_ty.clone(),
                    a_lt,
                )),
                span: None,
            });
            where_clause
        }));
        assert!(table.add_component(impl_id, {
            let mut implementation = Implementation::default();
            implementation.generic_arguments.types.push(Type::Reference(
                Reference {
                    qualifier: Qualifier::Mutable,
                    lifetime: a_lt,
                    pointee: Box::new(t_ty),
                },
            ));
            implementation
        }));

        implemented.insert(impl_id);
    }

    // immutable reference of any type can be copied
    {
        let impl_id = GlobalID::new(
            TargetID::CORE,
            pernixc_table::ID(id_gen.next().unwrap()),
        );
        let mut generic_parameters = GenericParameters::default();
        let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
            impl_id,
            generic_parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: "a".to_string(),
                    span: None,
                })
                .unwrap(),
        ));
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".to_string(),
                    span: None,
                })
                .unwrap(),
        ));

        assert!(table
            .add_component(impl_id, SymbolKind::PositiveMarkerImplementation));
        assert!(table.add_component(impl_id, generic_parameters));
        assert!(table.add_component(impl_id, Implements(copy_marker_id)));
        assert!(table.add_component(impl_id, ForallLifetimes::default()));
        assert!(table.add_component(impl_id, {
            let mut where_clause = WhereClause::default();
            where_clause.predicates.push(Predicate {
                predicate: predicate::Predicate::TypeOutlives(Outlives::new(
                    t_ty.clone(),
                    a_lt,
                )),
                span: None,
            });
            where_clause
        }));
        assert!(table.add_component(impl_id, {
            let mut implementation = Implementation::default();
            implementation.generic_arguments.types.push(Type::Reference(
                Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: a_lt,
                    pointee: Box::new(t_ty),
                },
            ));
            implementation
        }));

        implemented.insert(impl_id);
    }

    // mutable/immutable pointer of any type can be copied
    let mut impl_pointer = |mutable| {
        let impl_id = GlobalID::new(
            TargetID::CORE,
            pernixc_table::ID(id_gen.next().unwrap()),
        );
        let mut generic_parameters = GenericParameters::default();
        let t_ty = Type::Parameter(TypeParameterID::new(
            impl_id,
            generic_parameters
                .add_type_parameter(TypeParameter {
                    name: "T".to_string(),
                    span: None,
                })
                .unwrap(),
        ));

        assert!(table
            .add_component(impl_id, SymbolKind::PositiveMarkerImplementation));
        assert!(table.add_component(impl_id, generic_parameters));
        assert!(table.add_component(impl_id, Implements(copy_marker_id)));
        assert!(table.add_component(impl_id, ForallLifetimes::default()));
        assert!(table.add_component(impl_id, WhereClause::default()));
        assert!(table.add_component(impl_id, {
            let mut implementation = Implementation::default();
            implementation.generic_arguments.types.push(Type::Pointer(
                Pointer { mutable, pointee: Box::new(t_ty) },
            ));
            implementation
        }));

        implemented.insert(impl_id);
    };

    impl_pointer(false);
    impl_pointer(true);

    assert!(table.add_component(copy_marker_id, implemented));

    todo!()
}
