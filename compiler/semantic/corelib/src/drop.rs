//! Contains the definition for the `Drop` trait in the core library.

use pernixc_arena::{Arena, OrderedArena};
use pernixc_hash::HashSet;
use pernixc_semantic_element::{
    effect_annotation, elided_lifetime, implemented, implied_predicate,
    parameter::{self, Parameter, Parameters},
    return_type, where_clause,
};
use pernixc_symbol::{
    accessibility::{self, Accessibility},
    calculate_qualified_name_id_with_given_seed, kind,
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

use crate::CoreLibInitializer;

#[allow(missing_docs)]
pub const TRAIT_NAME: &str = "Drop";
#[allow(missing_docs)]
pub const DROP_FUNCTION_NAME: &str = "drop";
#[allow(missing_docs)]
pub const DROP_TRAIT_SEQUENCE: [&str; 2] = ["core", "Drop"];
#[allow(missing_docs)]
pub const DROP_FUNCTION_SEQUENCE: [&str; 3] = ["core", "Drop", "drop"];

impl CoreLibInitializer<'_> {
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
        &mut self,
        copy_marker_id: pernixc_symbol::ID,
    ) -> pernixc_symbol::ID {
        let (drop_trait_id, drop_function_id) = {
            let drop_trait_id = TargetID::CORE.make_global(
                calculate_qualified_name_id_with_given_seed(
                    DROP_TRAIT_SEQUENCE,
                    Some(self.root_target_module_id.id),
                    0,
                    self.target_seed,
                ),
            );

            let drop_function_id = TargetID::CORE.make_global(
                calculate_qualified_name_id_with_given_seed(
                    DROP_FUNCTION_SEQUENCE,
                    Some(self.root_target_module_id.id),
                    0,
                    self.target_seed,
                ),
            );

            (drop_trait_id, drop_function_id)
        };

        let mut trait_generic_params = GenericParameters::default();
        let t_ty = Type::Parameter(TypeParameterID::new(
            drop_trait_id,
            trait_generic_params
                .add_type_parameter(TypeParameter {
                    name: self.input_session.intern_unsized("T".to_owned()),
                    span: None,
                })
                .unwrap(),
        ));

        self.input_session
            .set_input(
                kind::Key { symbol_id: drop_trait_id },
                kind::Kind::Trait,
            )
            .await;
        self.input_session
            .set_input(
                name::Key { symbol_id: drop_trait_id },
                self.input_session.intern_unsized("Drop"),
            )
            .await;
        self.input_session
            .set_input(
                parent::Key { symbol_id: drop_trait_id },
                Some(self.root_target_module_id.id),
            )
            .await;
        self.input_session
            .set_input(
                generic_parameters::Key { symbol_id: drop_trait_id },
                self.input_session.intern(trait_generic_params),
            )
            .await;

        self.input_session
            .set_input(
                where_clause::Key { symbol_id: drop_trait_id },
                self.input_session.intern_unsized([where_clause::Predicate {
                    predicate: predicate::Predicate::NegativeMarker(
                        NegativeMarker {
                            marker_id: TargetID::CORE
                                .make_global(copy_marker_id),
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
        self.input_session
            .set_input(
                member::Key { symbol_id: drop_trait_id },
                self.input_session.intern(Member {
                    member_ids_by_name: std::iter::once((
                        self.input_session.intern_unsized("drop"),
                        drop_function_id.id,
                    ))
                    .collect(),
                    unnameds: HashSet::default(),
                }),
            )
            .await;
        self.input_session
            .set_input(
                accessibility::Key { symbol_id: drop_trait_id },
                Accessibility::Public,
            )
            .await;
        self.input_session
            .set_input(
                implemented::Key { symbol_id: drop_function_id },
                self.input_session.intern(HashSet::default()),
            )
            .await;

        // add drop method
        {
            self.input_session
                .set_input(
                    kind::Key { symbol_id: drop_function_id },
                    kind::Kind::TraitFunction,
                )
                .await;
            self.input_session
                .set_input(
                    name::Key { symbol_id: drop_function_id },
                    self.input_session.intern_unsized("drop".to_owned()),
                )
                .await;
            self.input_session
                .set_input(
                    parent::Key { symbol_id: drop_function_id },
                    Some(drop_trait_id.id),
                )
                .await;
            self.input_session
                .set_input(
                    elided_lifetime::Key { symbol_id: drop_function_id },
                    self.input_session.intern(Arena::default()),
                )
                .await;
            self.input_session
                .set_input(
                    implied_predicate::Key { symbol_id: drop_function_id },
                    self.input_session.intern(HashSet::default()),
                )
                .await;
            self.input_session
                .set_input(
                    pernixc_symbol::r#unsafe::Key {
                        symbol_id: drop_function_id,
                    },
                    false,
                )
                .await;

            let mut inner_generic_params = GenericParameters::default();

            let a_lt = inner_generic_params
                .add_lifetime_parameter(LifetimeParameter {
                    name: self.input_session.intern_unsized("a".to_owned()),
                    span: None,
                })
                .unwrap();
            let a_lt = Lifetime::Parameter(LifetimeParameterID::new(
                drop_function_id,
                a_lt,
            ));

            self.input_session
                .set_input(
                    generic_parameters::Key { symbol_id: drop_function_id },
                    self.input_session.intern(inner_generic_params),
                )
                .await;

            self.input_session
                .set_input(
                    where_clause::Key { symbol_id: drop_function_id },
                    self.input_session.intern_unsized([
                        where_clause::Predicate {
                            predicate: predicate::Predicate::type_outlives(
                                t_ty.clone(),
                                a_lt.clone(),
                            ),
                            span: None,
                        },
                    ]),
                )
                .await;

            self.input_session
                .set_input(
                    accessibility::Key { symbol_id: drop_function_id },
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

            self.input_session
                .set_input(
                    parameter::Key { symbol_id: drop_function_id },
                    self.input_session.intern(Parameters {
                        parameters,
                        parameter_order: vec![param_id],
                    }),
                )
                .await;

            self.input_session
                .set_input(
                    return_type::Key { symbol_id: drop_function_id },
                    self.input_session.intern(Type::unit()),
                )
                .await;

            self.input_session
                .set_input(
                    effect_annotation::Key { symbol_id: drop_function_id },
                    self.input_session.intern(OrderedArena::default()),
                )
                .await;
        }

        self.input_session
            .set_input(
                implemented::InTargetKey {
                    implementable_id: drop_trait_id,
                    target_id: TargetID::CORE,
                },
                self.input_session.intern(HashSet::default()),
            )
            .await;

        drop_trait_id.id
    }
}
