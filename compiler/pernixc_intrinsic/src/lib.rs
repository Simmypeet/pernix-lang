//! The crate initializes the `core` target for the [`Table`].

use pernixc_arena::Arena;
use pernixc_component::{
    function_signature::{FunctionSignature, Parameter},
    implementation::Implementation,
    implied_predicates::{ImpliedPredicate, ImpliedPredicates},
};
use pernixc_table::{
    component::{
        Accessibility, Implemented, Implements, Import, Member, Name, Parent,
        SymbolKind,
    },
    GlobalID, Table, TargetID,
};
use pernixc_term::{
    elided_lifetimes::{ElidedLifetime, ElidedLifetimeID, ElidedLifetimes},
    forall_lifetime,
    generic_parameter::{
        GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::{self, Outlives},
    r#type::{Primitive, Qualifier, Reference, Type},
    where_clause::{Predicate, WhereClause},
    Default,
};
use strum::IntoEnumIterator;

/// An extension trait on the [`Table`] to initialize the `core` target.
pub trait IntrinsicExt {
    /// Initializes the `core` target for the [`Table`].
    ///
    /// # Panics
    ///
    /// This assumes that the `core` target has not been initialized yet.
    fn initialize_core(&self);
}

/*
// Copy marker synopsis:

public marker Copy[T];

// implements copy for all primitive types
implements Copy[int8];
implements Copy[int16];
implements Copy[int32];
implements Copy[int64];
implements Copy[uint8];
implements Copy[uint16];
implements Copy[uint43];
implements Copy[uint64];
implements Copy[float32];
implements Copy[float64];
implements Copy[isize];
implements Copy[usize];

// mutable reference can't be copied
implements['a, T] Copy[&'a mutable T]
where
    T: 'a
delete;
*/

/*
// Drop trait synopsis:
public trait Drop[T] {
    public function drop(value: &mutable T);
}
 */

impl IntrinsicExt for Table {
    fn initialize_core(&self) {
        let root_core_module_id =
            GlobalID::new(TargetID::CORE, pernixc_table::ID::ROOT_MODULE);

        assert!(self.add_component(root_core_module_id, SymbolKind::Module));
        assert!(self.add_component(root_core_module_id, Accessibility::Public));
        assert!(
            self.add_component(root_core_module_id, Parent { parent: None })
        );
        assert!(self.add_component(root_core_module_id, Import::default()));
        assert!(
            self.add_component(root_core_module_id, Name("core".to_string()))
        );

        let mut id_gen = 1..;
        let mut core_member = Member::default();

        initialize_copy_marker(self, &mut id_gen, &mut core_member);
        initialize_drop_trait(self, &mut id_gen, &mut core_member);

        assert!(self.add_component(root_core_module_id, core_member));
    }
}

#[allow(clippy::too_many_lines)]
fn initialize_copy_marker(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    core_member: &mut Member,
) {
    let id = id_gen.next().unwrap();

    let copy_marker_id = GlobalID::new(TargetID::CORE, pernixc_table::ID(id));

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
    assert!(
        table.add_component(copy_marker_id, forall_lifetime::Map::default())
    );
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
        assert!(table.add_component(impl_id, forall_lifetime::Map::default()));

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
        assert!(table.add_component(impl_id, forall_lifetime::Map::default()));
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

    assert!(table.add_component(copy_marker_id, implemented));
}

fn initialize_drop_trait(
    table: &Table,
    id_gen: &mut impl Iterator<Item = usize>,
    member: &mut Member,
) {
    let drop_trait_id = GlobalID::new(
        TargetID::CORE,
        pernixc_table::ID(id_gen.next().unwrap()),
    );
    let drop_function_id = GlobalID::new(
        TargetID::CORE,
        pernixc_table::ID(id_gen.next().unwrap()),
    );

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
    assert!(table.add_component(drop_trait_id, forall_lifetime::Map::default()));
    assert!(table.add_component(drop_trait_id, Parent {
        parent: Some(pernixc_table::ID::ROOT_MODULE)
    }));
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

    assert!(member.insert("Drop".to_string(), drop_trait_id.id).is_none());

    // add drop method
    {
        assert!(
            table.add_component(drop_function_id, SymbolKind::TraitFunction)
        );
        assert!(table.add_component(drop_function_id, Name("drop".to_string())));
        assert!(table.add_component(drop_function_id, Parent {
            parent: Some(drop_trait_id.id)
        }));

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
        assert!(table
            .add_component(drop_function_id, forall_lifetime::Map::default()));

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
            return_type: Type::Tuple(pernixc_term::Tuple {
                elements: Vec::new()
            })
        }));
    }
}
