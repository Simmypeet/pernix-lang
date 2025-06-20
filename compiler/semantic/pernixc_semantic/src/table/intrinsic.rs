//! The crate initializes the `core` target for the [`Table`].

use pernixc_arena::Arena;
use strum::IntoEnumIterator;

use crate::{
    component::{
        derived::{
            elided_lifetimes::{
                ElidedLifetime, ElidedLifetimeID, ElidedLifetimes,
            },
            fields::{Field, Fields},
            forall_lifetimes::ForallLifetimes,
            function_signature::{FunctionSignature, Parameter},
            generic_parameters::{
                GenericParameters, LifetimeParameter, LifetimeParameterID,
                TypeParameter, TypeParameterID,
            },
            implementation::Implementation,
            implied_predicates::{ImpliedPredicate, ImpliedPredicates},
            where_clause::{Predicate, WhereClause},
        },
        input::{
            Accessibility, FunctionConstness, FunctionUnsafeness, Implemented,
            Implements, Import, Member, Name, Parent, SymbolKind,
        },
    },
    table::{self, GlobalID, Table, TargetID},
    term::{
        self,
        lifetime::Lifetime,
        predicate::{self, Outlives},
        r#type::{Pointer, Primitive, Qualifier, Reference, Type},
        Default, Tuple,
    },
};

/*
// Copy marker synopsis:

public marker Copy[T]

// implements copy for all primitive types
implements Copy[int8]
implements Copy[int16]
implements Copy[int32]
implements Copy[int64]
implements Copy[uint8]
implements Copy[uint16]
implements Copy[uint43]
implements Copy[uint64]
implements Copy[float32]
implements Copy[float64]
implements Copy[isize]
implements Copy[usize]

// mutable reference can't be copied
implements['a, T] Copy[&'a mut T] delete:
    where:
        T: 'a

implements['a, T] Copy[&'a T]:
    where:
        T: 'a

implements[T] Copy[*T]:
    pass

implements[T] Copy[*mut T]:
    pass
 */

/*
// Drop trait synopsis:
public trait Drop[T]:
    public function drop(value: &mutable T)
 */

/*
// size and alignof synopsis:
public function sizeof[T]() -> usize
public function alignof[T]() -> usize

public function dropAt[T](pointer: *mut T)
  */

impl Table {
    /// Initializes the core module with intrinsic types and functions.
    pub(super) fn initialize_core(&self) {
        let root_core_module_id =
            GlobalID::new(TargetID::CORE, table::ID::ROOT_MODULE);

        assert!(self.add_component(root_core_module_id, SymbolKind::Module));
        assert!(self.add_component(root_core_module_id, Accessibility::Public));
        assert!(self.add_component(root_core_module_id, Parent(None)));
        assert!(self.add_component(root_core_module_id, Import::default()));
        assert!(
            self.add_component(root_core_module_id, Name("core".to_string()))
        );

        let mut id_gen = 1..;
        let mut core_member = Member::default();

        initialize_copy_marker(self, &mut id_gen, &mut core_member);
        initialize_drop_trait(self, &mut id_gen, &mut core_member);
        initialize_sizeof(self, &mut id_gen, &mut core_member);
        initialize_alignof(self, &mut id_gen, &mut core_member);
        initialize_no_drop(self, &mut id_gen, &mut core_member);
        initialize_drop_at(self, &mut id_gen, &mut core_member);

        assert!(self.add_component(root_core_module_id, core_member));
    }
}

fn initialize_no_drop(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let no_drop_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

    assert!(table.add_component(no_drop_id, SymbolKind::Struct));
    assert!(table.add_component(no_drop_id, Name("NoDrop".to_string())));
    assert!(
        table.add_component(no_drop_id, Parent(Some(table::ID::ROOT_MODULE)))
    );
    assert!(table.add_component(no_drop_id, Accessibility::Public));
    let t_id;
    assert!(table.add_component(no_drop_id, {
        let mut generic_params = GenericParameters::default();
        t_id = generic_params
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap();
        generic_params
    }));

    assert!(table.add_component(no_drop_id, WhereClause::default()));
    assert!(table.add_component(no_drop_id, ForallLifetimes::default()));

    let mut fields = Arena::default();
    let value_field_id = fields.insert(Field {
        accessibility: Accessibility::Public,
        name: "value".to_string(),
        r#type: Type::Parameter(TypeParameterID::new(no_drop_id, t_id)),
        span: None,
    });

    assert!(table.add_component(no_drop_id, Fields {
        fields,
        field_ids_by_name: {
            let mut map = std::collections::HashMap::new();
            map.insert("value".to_string(), value_field_id);
            map
        },
        field_declaration_order: vec![value_field_id],
    }));

    // add to the core module
    assert!(core_member.insert("NoDrop".to_string(), no_drop_id.id).is_none());
}

fn initialize_drop_at(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let drop_at_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

    assert!(table.add_component(drop_at_id, SymbolKind::Function));
    assert!(table.add_component(drop_at_id, Name("dropAt".to_string())));
    assert!(
        table.add_component(drop_at_id, Parent(Some(table::ID::ROOT_MODULE)))
    );
    let mut generic_params = GenericParameters::default();
    let t_id = generic_params
        .add_type_parameter(TypeParameter { name: "T".to_string(), span: None })
        .unwrap();

    assert!(table.add_component(drop_at_id, generic_params));
    assert!(table.add_component(drop_at_id, WhereClause::default()));
    assert!(table.add_component(drop_at_id, ForallLifetimes::default()));
    assert!(table.add_component(drop_at_id, Accessibility::Public));
    assert!(table.add_component(drop_at_id, ElidedLifetimes::default()));
    assert!(table.add_component(drop_at_id, ImpliedPredicates::default()));
    assert!(table.add_component(drop_at_id, FunctionConstness(true)));
    assert!(table.add_component(drop_at_id, FunctionUnsafeness(true)));

    // add to the core module
    assert!(core_member.insert("dropAt".to_string(), drop_at_id.id).is_none());

    let mut parameters = Arena::default();

    let param_id = parameters.insert(Parameter {
        r#type: Type::Pointer(Pointer {
            mutable: true,
            pointee: Box::new(Type::Parameter(TypeParameterID {
                parent: drop_at_id,
                id: t_id,
            })),
        }),
        span: None,
    });

    assert!(table.add_component(drop_at_id, FunctionSignature {
        parameters,
        parameter_order: vec![param_id],
        return_type: Type::Tuple(Tuple { elements: Vec::new() })
    }));
}

fn initialize_sizeof(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let sizeof_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

    assert!(table.add_component(sizeof_id, SymbolKind::Function));
    assert!(table.add_component(sizeof_id, Name("sizeof".to_string())));
    assert!(
        table.add_component(sizeof_id, Parent(Some(table::ID::ROOT_MODULE)))
    );
    assert!(table.add_component(sizeof_id, {
        let mut generic_params = GenericParameters::default();
        assert!(generic_params
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .is_ok());
        generic_params
    }));
    assert!(table.add_component(sizeof_id, WhereClause::default()));
    assert!(table.add_component(sizeof_id, ForallLifetimes::default()));
    assert!(table.add_component(sizeof_id, Accessibility::Public));
    assert!(table.add_component(sizeof_id, ElidedLifetimes::default()));
    assert!(table.add_component(sizeof_id, ImpliedPredicates::default()));
    assert!(table.add_component(sizeof_id, FunctionConstness(true)));
    assert!(table.add_component(sizeof_id, FunctionUnsafeness(false)));

    // add to the core module
    assert!(core_member.insert("sizeof".to_string(), sizeof_id.id).is_none());

    assert!(table.add_component(sizeof_id, FunctionSignature {
        parameters: Arena::default(),
        parameter_order: Vec::new(),
        return_type: Type::Primitive(Primitive::Usize)
    }));
}

fn initialize_alignof(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let alignof_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

    assert!(table.add_component(alignof_id, SymbolKind::Function));
    assert!(table.add_component(alignof_id, Name("alignof".to_string())));
    assert!(
        table.add_component(alignof_id, Parent(Some(table::ID::ROOT_MODULE)))
    );
    assert!(table.add_component(alignof_id, {
        let mut generic_params = GenericParameters::default();
        assert!(generic_params
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .is_ok());
        generic_params
    }));
    assert!(table.add_component(alignof_id, WhereClause::default()));
    assert!(table.add_component(alignof_id, ForallLifetimes::default()));
    assert!(table.add_component(alignof_id, Accessibility::Public));
    assert!(table.add_component(alignof_id, ElidedLifetimes::default()));
    assert!(table.add_component(alignof_id, ImpliedPredicates::default()));
    assert!(table.add_component(alignof_id, FunctionConstness(true)));
    assert!(table.add_component(alignof_id, FunctionUnsafeness(false)));

    // add to the core module
    assert!(core_member.insert("alignof".to_string(), alignof_id.id).is_none());

    assert!(table.add_component(alignof_id, FunctionSignature {
        parameters: Arena::default(),
        parameter_order: Vec::new(),
        return_type: Type::Primitive(Primitive::Usize)
    }));
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn initialize_copy_marker(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let id = id_gen.next().unwrap();

    let copy_marker_id = GlobalID::new(TargetID::CORE, table::ID(id));

    assert!(table.add_component(copy_marker_id, SymbolKind::Marker));
    assert!(table.add_component(copy_marker_id, Name("Copy".to_string())));
    assert!(table
        .add_component(copy_marker_id, Parent(Some(table::ID::ROOT_MODULE))));
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
        let impl_id =
            GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

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
        let impl_id =
            GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));
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
        let impl_id =
            GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));
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
        let impl_id =
            GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));
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
}

fn initialize_drop_trait(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    member: &mut Member,
) {
    let drop_trait_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));
    let drop_function_id =
        GlobalID::new(TargetID::CORE, table::ID(id_gen.next().unwrap()));

    let mut trait_generic_params = GenericParameters::default();
    let t_ty = Type::Parameter(TypeParameterID::new(
        drop_trait_id,
        trait_generic_params
            .add_type_parameter(TypeParameter {
                name: "T".to_string(),
                span: None,
            })
            .unwrap(),
    ));

    assert!(table.add_component(drop_trait_id, SymbolKind::Trait));
    assert!(table.add_component(drop_trait_id, Name("Drop".to_string())));
    assert!(table.add_component(drop_trait_id, ForallLifetimes::default()));
    assert!(table
        .add_component(drop_trait_id, Parent(Some(table::ID::ROOT_MODULE))));
    assert!(table.add_component(drop_trait_id, trait_generic_params));
    assert!(table.add_component(drop_trait_id, WhereClause::default()));
    assert!(table.add_component(
        drop_trait_id,
        Member(
            std::iter::once(("drop".to_string(), drop_function_id.id))
                .collect()
        )
    ));
    assert!(table.add_component(drop_trait_id, Accessibility::Public));
    assert!(table.add_component(drop_trait_id, Implemented::default()));

    assert!(member.insert("Drop".to_string(), drop_trait_id.id).is_none());

    // add drop method
    {
        assert!(
            table.add_component(drop_function_id, SymbolKind::TraitFunction)
        );
        assert!(table.add_component(drop_function_id, Name("drop".to_string())));
        assert!(table
            .add_component(drop_function_id, Parent(Some(drop_trait_id.id))));

        let mut elided_lifetimes = ElidedLifetimes::default();
        let elided_lt = Lifetime::<Default>::Elided(ElidedLifetimeID::new(
            drop_function_id,
            elided_lifetimes.elided_lifetimes.insert(ElidedLifetime {}),
        ));

        let mut implied_predicates = ImpliedPredicates::default();
        implied_predicates.implied_predicates.insert(
            ImpliedPredicate::TypeOutlives(Outlives::new(
                t_ty.clone(),
                elided_lt,
            )),
        );

        assert!(
            table.add_component(drop_function_id, GenericParameters::default())
        );
        assert!(table.add_component(drop_function_id, WhereClause::default()));
        assert!(table.add_component(drop_function_id, elided_lifetimes));
        assert!(table.add_component(drop_function_id, implied_predicates));
        assert!(table.add_component(drop_function_id, Accessibility::Public));
        assert!(
            table.add_component(drop_function_id, ForallLifetimes::default())
        );

        let mut parameters = Arena::default();
        let param_id = parameters.insert(Parameter {
            r#type: Type::Reference(Reference {
                qualifier: Qualifier::Mutable,
                lifetime: elided_lt,
                pointee: Box::new(t_ty),
            }),
            span: None,
        });

        assert!(table.add_component(drop_function_id, FunctionSignature {
            parameters,
            parameter_order: vec![param_id],
            return_type: Type::Tuple(term::Tuple { elements: Vec::new() })
        }));
    }
}
