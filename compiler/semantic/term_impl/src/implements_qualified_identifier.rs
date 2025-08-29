use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::runtime::executor::{self, CyclicError};
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    term::{resolution_to_type, ResolutionToTypeError},
    Config,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceElement;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    final_implements::is_implements_final,
    kind::{get_kind, Kind},
    member::get_members,
    name::get_name,
    syntax::get_implements_qualified_identifier,
};
use pernixc_target::Global;
use pernixc_term::{generic_arguments::Symbol, r#type::Type};

use crate::{
    build::Output,
    implements_qualified_identifier::diagnostic::{
        InvalidSymbolForImplements, MarkerImplementsNotFinal,
        TraitMemberNotImplemented, TraitMemberImplementedMultipleTimes,
        TraitMemberKindMismatch, ExtraneousImplementationMember,
    },
    occurrences,
};

pub mod diagnostic;

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
    Serialize,
    Deserialize,
    pernixc_query::Key,
)]
#[value(Option<Arc<Resolution>>)]
pub struct Key(pub Global<pernixc_symbol::ID>);

impl crate::build::Build for Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, CyclicError> {
        let qualified_identifier =
            engine.get_implements_qualified_identifier(key.0).await;

        let mut occurrences = occurrences::Occurrences::default();
        let storage = Storage::<diagnostic::Diagnostic>::new();
        let generic_parameter_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;

        let resolution = match engine
            .resolve_qualified_identifier(
                &qualified_identifier,
                Config::builder()
                    .consider_adt_implements(false)
                    .observer(&mut occurrences)
                    .referring_site(key.0)
                    .extra_namespace(&generic_parameter_namespace)
                    .build(),
                &storage,
            )
            .await
        {
            Ok(resolution) => resolution,

            // couldn't resolve, but we still want to return diagnostics
            Err(pernixc_resolution::Error::Abort) => {
                return Ok(Output {
                    item: None,
                    diagnostics: storage.into_vec().into(),
                    occurrences: Arc::new(occurrences),
                });
            }

            Err(pernixc_resolution::Error::Cyclic(cyclic)) => {
                return Err(cyclic);
            }
        };

        check_valid_resolution(
            engine,
            resolution.clone(),
            qualified_identifier.span(),
            &storage,
        )
        .await?;

        // performs extra necessary check
        if let Resolution::Generic(generic) = &resolution {
            let kind = engine.get_kind(generic.id).await;

            #[allow(clippy::match_same_arms)]
            match kind {
                Kind::Marker => {
                    check_marker(
                        engine,
                        key.0,
                        qualified_identifier.span(),
                        &storage,
                    )
                    .await?;
                }

                Kind::Trait => {
                    check_trait(
                        engine,
                        key.0,
                        qualified_identifier.span(),
                        generic.id,
                        &storage,
                    )
                    .await?;
                }

                _ => {}
            }
        }

        Ok(Output {
            item: Some(Arc::new(resolution)),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

async fn check_valid_resolution(
    tracked_engine: &pernixc_query::TrackedEngine,
    resolution: Resolution,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    match resolution {
        Resolution::Module(global) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: global,
                    },
                ),
            );

            Ok(())
        }

        Resolution::Variant(variant) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: variant.variant_id,
                    },
                ),
            );

            Ok(())
        }

        Resolution::Generic(generic) => {
            let kind = tracked_engine.get_kind(generic.id).await;

            match kind {
                Kind::Trait | Kind::Marker | Kind::Struct | Kind::Enum => {
                    // valid implements symbol
                }

                Kind::Type => {
                    // if it's a type, it must be a struct/enum
                    let ty = match tracked_engine
                        .resolution_to_type(Resolution::Generic(generic))
                        .await
                    {
                        Ok(ty) => ty,
                        Err(ResolutionToTypeError::Failed(_)) => unreachable!(),
                        Err(ResolutionToTypeError::Cyclic(cyclic)) => {
                            return Err(cyclic)
                        }
                    };

                    if !matches!(
                        &ty,
                        Type::Symbol(sym)
                            if is_adt_type(tracked_engine, sym).await
                    ) {
                        storage.receive(
                            diagnostic::Diagnostic::InvalidTypeForImplements(
                                diagnostic::InvalidTypeForImplements {
                                    qualified_identifier_span:
                                        qualified_identifier,
                                    r#type: ty,
                                },
                            ),
                        );
                    }
                }

                _ => {
                    storage.receive(
                        diagnostic::Diagnostic::InvalidSymbolForImplements(
                            InvalidSymbolForImplements {
                                qualified_identifier_span: qualified_identifier,
                                symbol_id: generic.id,
                            },
                        ),
                    );
                }
            }

            Ok(())
        }

        Resolution::MemberGeneric(member_generic) => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        symbol_id: member_generic.id,
                    },
                ),
            );

            Ok(())
        }
    }
}

async fn is_adt_type(
    engine: &pernixc_query::TrackedEngine,
    sym_ty: &Symbol,
) -> bool {
    let kind = engine.get_kind(sym_ty.id).await;
    matches!(kind, Kind::Struct | Kind::Enum)
}

async fn check_marker(
    engine: &pernixc_query::TrackedEngine,
    implements: Global<pernixc_symbol::ID>,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let is_final = engine.is_implements_final(implements).await;

    if !is_final {
        storage.receive(diagnostic::Diagnostic::MarkerImplementsNotFinal(
            MarkerImplementsNotFinal {
                qualified_identifier_span: qualified_identifier,
            },
        ));
    }

    // we'll check if he marker implementation has any members (it shouldn't).
    if engine.get_kind(implements).await != Kind::PositiveImplementation {
        return Ok(());
    }

    let members = engine.get_members(implements).await;

    for member_id in members
        .member_ids_by_name
        .values()
        .copied()
        .chain(members.unnameds.iter().copied())
    {
        storage.receive(
            diagnostic::Diagnostic::MemberInMarkerImplementationIsNotAllowed(
                diagnostic::MemberInMarkerImplementationIsNotAllowed {
                    implements_member_id: implements
                        .target_id
                        .make_global(member_id),
                },
            ),
        );
    }

    Ok(())
}

async fn check_trait(
    engine: &pernixc_query::TrackedEngine,
    implements: Global<pernixc_symbol::ID>,
    _qualified_identifier: RelativeSpan,
    trait_id: Global<pernixc_symbol::ID>,
    storage: &Storage<diagnostic::Diagnostic>,
) -> Result<(), executor::CyclicError> {
    // no need to check the member
    if engine.get_kind(implements).await != Kind::PositiveImplementation {
        return Ok(());
    }

    let implements_members = engine.get_members(implements).await;
    let trait_members = engine.get_members(trait_id).await;

    // Maps member name to (implementation_member_id, trait_member_id)
    let mut implemented_member_by_name: HashMap<
        SharedStr,
        (Global<pernixc_symbol::ID>, Option<Global<pernixc_symbol::ID>>),
    > = HashMap::default();

    // Check each implementation member
    for implements_member_id in implements_members
        .member_ids_by_name
        .values()
        .copied()
        .map(|x| implements.target_id.make_global(x))
    {
        let member_name = engine.get_name(implements_member_id).await;
        let trait_equivalent_id = trait_members
            .member_ids_by_name
            .get(&member_name)
            .map(|&id| trait_id.target_id.make_global(id));

        // Check if this member is implemented multiple times
        if let Some((first_impl_id, _)) =
            implemented_member_by_name.get(&member_name)
        {
            storage.receive(
                diagnostic::Diagnostic::TraitMemberImplementedMultipleTimes(
                    diagnostic::TraitMemberImplementedMultipleTimes {
                        first_implementation_member_id: *first_impl_id,
                        second_implementation_member_id: implements_member_id,
                    },
                ),
            );
            continue;
        }

        if let Some(trait_member_id) = trait_equivalent_id {
            // Check if the kinds match
            let impl_kind = engine.get_kind(implements_member_id).await;
            let trait_kind = engine.get_kind(trait_member_id).await;

            let kinds_match = matches!(
                (trait_kind, impl_kind),
                (Kind::TraitFunction, Kind::ImplementationFunction)
                    | (Kind::TraitType, Kind::ImplementationType)
                    | (Kind::TraitConstant, Kind::ImplementationConstant)
            );

            if !kinds_match {
                storage.receive(
                    diagnostic::Diagnostic::TraitMemberKindMismatch(
                        diagnostic::TraitMemberKindMismatch {
                            trait_member_id,
                            implementation_member_id: implements_member_id,
                        },
                    ),
                );
            }

            implemented_member_by_name.insert(
                member_name.clone(),
                (implements_member_id, Some(trait_member_id)),
            );
        } else {
            // Implementation member doesn't correspond to any trait member
            storage.receive(
                diagnostic::Diagnostic::ExtraneousImplementationMember(
                    diagnostic::ExtraneousImplementationMember {
                        implementation_member_id: implements_member_id,
                    },
                ),
            );

            implemented_member_by_name
                .insert(member_name.clone(), (implements_member_id, None));
        }
    }

    // Check for missing trait members
    let mut unimplemented_trait_members = Vec::new();
    
    for (trait_member_name, &trait_member_id) in
        &trait_members.member_ids_by_name
    {
        let trait_member_global_id =
            trait_id.target_id.make_global(trait_member_id);

        if !implemented_member_by_name.contains_key(trait_member_name) {
            unimplemented_trait_members.push(trait_member_global_id);
        }
    }

    // Emit a single diagnostic for all unimplemented members if any
    if !unimplemented_trait_members.is_empty() {
        storage.receive(diagnostic::Diagnostic::TraitMemberNotImplemented(
            diagnostic::TraitMemberNotImplemented {
                unimplemented_trait_member_ids: unimplemented_trait_members,
                implementation_id: implements,
            },
        ));
    }

    Ok(())
}
