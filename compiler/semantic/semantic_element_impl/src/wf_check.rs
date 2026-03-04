//! Performs the well-formedness checking for every symbol
//! resolutions/occurrences in a symbol.

use std::borrow::Cow;

use diagnostic::Diagnostic;
use linkme::distributed_slice;
use pernixc_handler::{Handler, Storage};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::trait_ref;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent_global,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    instantiation::get_instantiation,
    predicate::{self, Outlives, Predicate},
    r#type::Type,
};
use pernixc_type_system::{
    diagnostic::UnsatisfiedPredicate,
    environment::{Environment, get_active_premise},
    normalizer,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    build, function_signature, implements_qualified_identifier,
    occurrences::Occurrences,
};

pub mod diagnostic;

/// Checker for the well-formedness of the instantiations.
pub(super) struct Checker<'a> {
    environment: &'a Environment<'a, normalizer::NoOp>,
    handler: &'a Storage<Diagnostic>,
}

impl Checker<'_> {
    /// Checks if the given `resolution` is well-formed. The errors are reported
    /// to the `handler`.
    #[allow(clippy::too_many_lines, unused)]
    pub(super) async fn check_resolution_occurrence(
        &self,
        resolution: &Resolution,
        resolution_span: &RelativeSpan,
    ) {
        match resolution {
            Resolution::Type(_)
            | Resolution::Instance(_)
            | Resolution::InstanceAssociatedFunction(_)
            | Resolution::Module(_)
            | Resolution::Variant(_) => {}

            Resolution::Generic(generic) => {
                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id,
                    generic.generic_arguments.clone(),
                    resolution_span,
                )
                .await;
            }
            Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let symbol_kind = self
                    .environment
                    .tracked_engine()
                    .get_kind(member_generic.id)
                    .await;

                let mut base_instantiation = match symbol_kind {
                    Kind::ImplementationAssociatedConstant
                    | Kind::ImplementationAssociatedFunction
                    | Kind::ImplementationAssociatedType => {
                        let impl_id = self
                            .environment
                            .tracked_engine()
                            .get_parent_global(member_generic.id)
                            .await
                            .unwrap();

                        let Ok(Some(implementation_check)) = self
                            .environment
                            .wf_check_implementation(
                                impl_id,
                                resolution_span,
                                &member_generic.parent_generic_arguments,
                                self.handler,
                            )
                            .await
                        else {
                            return;
                        };

                        implementation_check.into_instantiation()
                    }

                    _ => {
                        let parent = self
                            .environment
                            .tracked_engine()
                            .get_parent_global(member_generic.id)
                            .await
                            .unwrap();

                        self.environment
                            .tracked_engine()
                            .get_instantiation(
                                parent,
                                member_generic.parent_generic_arguments.clone(),
                            )
                            .await
                            .unwrap()
                    }
                };

                let generic_parameters = self
                    .environment
                    .tracked_engine()
                    .get_generic_parameters(member_generic.id)
                    .await;

                base_instantiation
                    .append_from_generic_arguments(
                        member_generic.member_generic_arguments.clone(),
                        member_generic.id,
                        &generic_parameters,
                    )
                    .unwrap();

                let instances = member_generic
                    .member_generic_arguments
                    .instances()
                    .to_vec();

                self.environment
                    .wf_check_instantiation(
                        member_generic.id,
                        resolution_span,
                        &base_instantiation,
                        self.handler,
                    )
                    .await;

                self.environment
                    .check_instance_trait_ref(
                        member_generic.id,
                        &instances,
                        resolution_span,
                        &base_instantiation,
                        self.handler,
                    )
                    .await;
            }
        }
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    pub(super) async fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
        instantiation_span: &RelativeSpan,
    ) {
        let instances = generic_arguments.instances().to_vec();

        let inst = self
            .environment
            .tracked_engine()
            .get_instantiation(instantiated, generic_arguments)
            .await
            .unwrap();

        let _ = self
            .environment
            .wf_check_instantiation(
                instantiated,
                instantiation_span,
                &inst,
                self.handler,
            )
            .await;

        let _ = self
            .environment
            .check_instance_trait_ref(
                instantiated,
                &instances,
                instantiation_span,
                &inst,
                self.handler,
            )
            .await;
    }

    /// Do predicates check for the given type occurrences.
    #[allow(clippy::too_many_lines)]
    pub(super) async fn check_type_ocurrence(
        &self,
        ty: &Type,
        instantiation_span: &RelativeSpan,
    ) {
        match ty {
            Type::Error(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::Pointer(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Symbol(_)
            | Type::AssociatedSymbol(_)
            | Type::FunctionSignature(_)
            | Type::InstanceAssociated(_)
            | Type::Inference(_) => {
                // no additional check
            }
            Type::Reference(reference) => {
                let outlives = Outlives::new(
                    (*reference.pointee).clone(),
                    reference.lifetime.clone(),
                );

                match self.environment.query(&outlives).await {
                    Ok(true) => {}

                    Ok(false) => {
                        self.handler.receive(
                            UnsatisfiedPredicate::builder()
                                .predicate(Predicate::TypeOutlives(outlives))
                                .instantiation_span(*instantiation_span)
                                .build(),
                        );
                    }

                    Err(overflow_error) => {
                        overflow_error.report_as_undecidable_predicate(
                            Predicate::TypeOutlives(outlives),
                            None,
                            *instantiation_span,
                            self.handler,
                        );
                    }
                }
            }
            Type::Array(_) => {
                todo!("implements type check of the array length with usize")
                /*
                let expected_type = Type::Primitive(Primitive::Usize);
                let type_check =
                    TypeCheck::new(array.length.clone(), expected_type.clone());

                match self.environment.query(&type_check) {
                    Ok(Some(result)) => {
                        for LifetimeConstraint::LifetimeOutlives(outlives) in
                            &result.constraints
                        {
                            match self.environment.query(outlives) {
                                Ok(None) => {
                                    self.handler.receive(Box::new(
                                        UnsatisfiedPredicate {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    outlives.clone(),
                                                ),
                                            predicate_declaration_span: None,
                                            instantiation_span:
                                                instantiation_span.clone(),
                                        },
                                    ));
                                }

                                Err(Error::Abort(Abort)) | Ok(Some(_)) => {}

                                Err(Error::Overflow(overflow_error)) => {
                                    overflow_error
                                        .report_as_undecidable_predicate(
                                            Predicate::LifetimeOutlives(
                                                outlives.clone(),
                                            ),
                                            None,
                                            instantiation_span.clone(),
                                            self.handler,
                                        );
                                }
                            }
                        }
                    }

                    Ok(None) => {
                        self.handler.receive(Box::new(
                            ConstantArgumentTypeMismatched {
                                span: instantiation_span.clone(),
                                expected_type,
                                constant_argument: array.length.clone(),
                            },
                        ));
                    }

                    Err(Error::Abort(Abort)) => {}

                    Err(Error::Overflow(overflow_error)) => {
                        overflow_error.report_as_type_check_overflow(
                            instantiation_span.clone(),
                            self.handler,
                        );
                    }
                }
                */
            }
        }
    }

    #[allow(unused)]
    async fn check_unpacked_ocurrences(
        &self,
        unpacked_term: Type,
        instantiation_span: &RelativeSpan,
    ) {
        let tuple_predicate = predicate::Tuple(unpacked_term);

        self.environment
            .predicate_satisfied(
                Predicate::TupleType(tuple_predicate),
                instantiation_span,
                None,
                self.handler,
            )
            .await;
    }
}

pub(super) async fn check_occurrences(
    occurrences: &Occurrences,
    environment: &Environment<'_, normalizer::NoOp>,
    storage: &Storage<Diagnostic>,
) {
    let checker = Checker { environment, handler: storage };

    // check resolution occurrences
    for (resolution, span) in &occurrences.resolutions {
        checker.check_resolution_occurrence(resolution, span).await;
    }

    // check type occurrences
    for (ty, syn) in &occurrences.types {
        checker.check_type_ocurrence(ty, &syn.span()).await;
    }

    // check unpacked type occurrences
    for (unpacked, syn) in &occurrences.unpacked_types {
        checker.check_unpacked_ocurrences(unpacked.clone(), &syn.span()).await;
    }

    // check unpacked constant occurrences
    for _ in &occurrences.unpacked_constants {
        // TODO: check that the type of constant must be a tuple
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static WF_CHECK_EXECUTOR: Registration<Config> =
    Registration::new::<Key, WfCheckExecutor>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Interned<[Diagnostic]>)]
pub struct Key {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config)]
pub async fn wf_check_executor(
    &Key { symbol_id }: &Key,
    tracked_engine: &TrackedEngine,
) -> Interned<[Diagnostic]> {
    let mut occurrences = Vec::new();
    let kind = tracked_engine.get_kind(symbol_id).await;

    if kind.has_generic_parameters() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_term::generic_parameters::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind.has_where_clause() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::where_clause::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind.has_type_alias() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::type_alias::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind == Kind::Variant {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::variant::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind == Kind::Struct {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    pernixc_semantic_element::fields::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind.is_implementation() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(
                    implements_qualified_identifier::Key { symbol_id },
                ))
                .await,
        );
    }

    if kind.has_function_signature() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(function_signature::Key {
                    symbol_id,
                }))
                .await,
        );
    }

    if kind.has_trait_ref() {
        occurrences.push(
            tracked_engine
                .query(&build::OccurrencesKey::new(trait_ref::Key {
                    symbol_id,
                }))
                .await,
        );
    }

    let active_premise = tracked_engine.get_active_premise(symbol_id).await;
    let environment = Environment::new_do_outlives_check(
        Cow::Borrowed(&active_premise),
        Cow::Borrowed(tracked_engine),
        normalizer::NO_OP,
    );

    let storage = Storage::<Diagnostic>::default();
    for occurrence in occurrences {
        check_occurrences(&occurrence, &environment, &storage).await;
    }

    tracked_engine.intern_unsized(storage.into_vec())
}
