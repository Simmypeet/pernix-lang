use std::sync::Arc;

use pernixc_handler::{Handler, Storage};
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
    kind::{get_kind, Kind},
    syntax::get_implements_qualified_identifier,
};
use pernixc_target::Global;
use pernixc_term::{generic_arguments::Symbol, r#type::Type};

use crate::{
    build::Output,
    implements_qualified_identifier::diagnostic::InvalidSymbolForImplements,
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
