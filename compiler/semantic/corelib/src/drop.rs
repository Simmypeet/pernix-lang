//! Contains the definition for the `Drop` trait in the core library.

use std::sync::Arc;

use pernixc_arena::Arena;
use pernixc_hash::HashSet;
use pernixc_query::Engine;
use pernixc_semantic_element::{
    elided_lifetime, implemented, implied_predicate,
    parameter::{self, Parameter, Parameters},
    return_type, where_clause,
};
use pernixc_symbol::{
    accessibility::{self, Accessibility},
    calculate_qualified_name_id, get_target_root_module_id, kind,
    member::{self, Member},
    name, parent,
};
use pernixc_target::TargetID;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{
        self, GenericParameters, LifetimeParameter, LifetimeParameterID,
        TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::{self, NegativeMarker},
    r#type::{Qualifier, Reference, Type},
};

#[allow(missing_docs)]
pub const TRAIT_NAME: &str = "Drop";
#[allow(missing_docs)]
pub const DROP_FUNCTION_NAME: &str = "drop";
#[allow(missing_docs)]
pub const DROP_TRAIT_SEQUENCE: [&str; 2] = ["core", "Drop"];
#[allow(missing_docs)]
pub const DROP_FUNCTION_SEQUENCE: [&str; 3] = ["core", "Drop", "drop"];

/// Creates a `Drop` trait in the core library.
///
/// ```txt
/// public trait Drop[T]:
///     public function drop['a](self: &'a mut T):
///         where:
///             marker not Copy[T]
///             T: 'a
/// ```
#[allow(clippy::too_many_lines)]
pub async fn initialize_drop_trait(
    engine: &mut Arc<Engine>,
    copy_marker_id: pernixc_symbol::ID,
) -> pernixc_symbol::ID {
    let (root_target_module_id, drop_trait_id, drop_function_id) = {
        let tracked_engine = engine.tracked();

        let root_target_module_id =
            tracked_engine.get_target_root_module_id(TargetID::CORE).await;

        let drop_trait_id = TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    DROP_TRAIT_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        );

        let drop_function_id = TargetID::CORE.make_global(
            tracked_engine
                .calculate_qualified_name_id(
                    DROP_FUNCTION_SEQUENCE,
                    TargetID::CORE,
                    Some(root_target_module_id),
                    0,
                )
                .await,
        );

        (root_target_module_id, drop_trait_id, drop_function_id)
    };

    let mut trait_generic_params = GenericParameters::default();
    let t_ty = Type::Parameter(TypeParameterID::new(
        drop_trait_id,
        trait_generic_params
            .add_type_parameter(TypeParameter { name: "T".into(), span: None })
            .unwrap(),
    ));

    let input_lock = Arc::get_mut(engine).unwrap().input_lock();

    input_lock.set_input(kind::Key(drop_trait_id), kind::Kind::Trait).await;
    input_lock.set_input(name::Key(drop_trait_id), "Drop".into()).await;
    input_lock
        .set_input(parent::Key(drop_trait_id), Some(root_target_module_id))
        .await;
    input_lock
        .set_input(
            generic_parameters::Key(drop_trait_id),
            Arc::new(trait_generic_params),
        )
        .await;
    input_lock
        .set_input(
            where_clause::Key(drop_trait_id),
            Arc::from([where_clause::Predicate {
                predicate: predicate::Predicate::NegativeMarker(
                    NegativeMarker {
                        marker_id: TargetID::CORE.make_global(copy_marker_id),
                        generic_arguments: GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![t_ty.clone()],
                            constants: Vec::new(),
                        },
                    },
                ),
                span: None,
            }]),
        )
        .await;
    input_lock
        .set_input(
            member::Key(drop_trait_id),
            Arc::new(Member {
                member_ids_by_name: std::iter::once((
                    "drop".into(),
                    drop_function_id.id,
                ))
                .collect(),
                unnameds: HashSet::default(),
            }),
        )
        .await;
    input_lock
        .set_input(accessibility::Key(drop_trait_id), Accessibility::Public)
        .await;
    input_lock
        .set_input(implemented::Key(drop_function_id), Arc::default())
        .await;

    // add drop method
    {
        input_lock
            .set_input(kind::Key(drop_function_id), kind::Kind::TraitFunction)
            .await;
        input_lock.set_input(name::Key(drop_function_id), "drop".into()).await;
        input_lock
            .set_input(parent::Key(drop_function_id), Some(drop_trait_id.id))
            .await;
        input_lock
            .set_input(elided_lifetime::Key(drop_function_id), Arc::default())
            .await;
        input_lock
            .set_input(implied_predicate::Key(drop_function_id), Arc::default())
            .await;

        let mut inner_generic_params = GenericParameters::default();
        let a_lt = inner_generic_params
            .add_lifetime_parameter(LifetimeParameter {
                name: "a".into(),
                span: None,
            })
            .unwrap();
        let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
            drop_function_id,
            a_lt,
        ));

        input_lock
            .set_input(
                generic_parameters::Key(drop_function_id),
                Arc::new(inner_generic_params),
            )
            .await;

        input_lock
            .set_input(
                where_clause::Key(drop_function_id),
                Arc::from([where_clause::Predicate {
                    predicate: predicate::Predicate::type_outlives(
                        t_ty.clone(),
                        a_lt,
                    ),
                    span: None,
                }]),
            )
            .await;

        input_lock
            .set_input(
                accessibility::Key(drop_function_id),
                Accessibility::Public,
            )
            .await;

        let mut parameters = Arena::default();
        let param_id = parameters.insert(Parameter {
            r#type: Type::Reference(Reference {
                qualifier: Qualifier::Mutable,
                lifetime: a_lt,
                pointee: Box::new(t_ty),
            }),
            span: None,
        });

        input_lock
            .set_input(
                parameter::Key(drop_function_id),
                Arc::new(Parameters {
                    parameters,
                    parameter_order: vec![param_id],
                }),
            )
            .await;

        input_lock
            .set_input(
                return_type::Key(drop_function_id),
                Arc::new(Type::unit()),
            )
            .await;
    }

    input_lock
        .set_input(
            implemented::InTargetKey {
                implementable_id: drop_trait_id,
                target_id: TargetID::CORE,
            },
            Arc::new(HashSet::default()),
        )
        .await;

    drop_trait_id.id
}
