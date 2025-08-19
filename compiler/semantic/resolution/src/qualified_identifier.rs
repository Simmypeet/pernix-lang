//! Contains the logic for resolving the qualified identifier.

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_extend::extend;
use pernixc_query::TrackedEngine;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    get_target_root_module_id,
    import::get_imports,
    kind::{get_kind, Kind},
    member::get_members,
    name::{diagnostic::SymbolNotFound, get_member_of},
    parent::{get_closest_module_id, scope_walker},
};
use pernixc_syntax::{QualifiedIdentifier, QualifiedIdentifierRoot};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters, implements::get_implements,
    implements_argument::get_implements_argument,
};

use crate::{
    diagnostic::{NoGenericArgumentsRequired, ThisNotFound},
    term::resolve_generic_arguments_for,
    Config, Diagnostic, Error, Handler,
};

/// Repersents a resolution to a symbol that can be supplied with generic
/// arguments such as `SYMBOl['a, T, U, V]`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic {
    /// The resolved symbbol.
    ///
    /// This includes: `struct`, `enum`, `function`, `trait`, `const`,
    /// `type`, `marker` and trait implementation's `type`, `function`, and
    /// `const`.
    ///
    /// Trait implementation members are included here instead of in the
    /// `MemberGeneric` because you can never refer to a trait implementation
    /// member while specifying the trait implementation itself.
    pub id: Global<pernixc_symbol::ID>,

    /// The generic arguments that are supplied to the resolved symbol.
    pub generic_arguments: GenericArguments,
}

/// Represents a resolution to an enum-varaint symbol such as
/// `Option[int32]::None`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The resolved variant symbol.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The generic arguments that are supplied to the parent enum.
    pub generic_arguments: GenericArguments,
}

/// Represents a resolution to a symbol that is a member of an another symbol
/// such as `Clone[T]::clone['a]`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberGeneric {
    /// The resolved member symbol.
    ///
    /// This includes: trait member's `type`, `function`, and `const`, and
    /// adt implementation's `function`.
    pub id: Global<pernixc_symbol::ID>,

    /// The generic arguments that are supplied to the parent symbol. Suppose
    /// `Clone[T]::clone['a]`, then `[T]` is the parent generic arguments.
    pub parent_generic_arguments: GenericArguments,

    /// The generic arguments that are supplied to the member symbol. Suppose
    /// `Clone[T]::clone['a]`, then `['a]` is the member generic arguments.
    pub member_generic_arguments: GenericArguments,
}

/// Represents a resolution of a qualified identifier syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Resolution {
    /// Resolved to a module symbol.
    Module(Global<pernixc_symbol::ID>),

    /// Resolved to an enum-variant symbol.
    Variant(Variant),

    /// Resolved to a symbol with generic arguments such as `Symbol['a, T, U]`.
    Generic(Generic),

    /// Resolved to a member symbol with generic arguments such as
    /// `Symbol[V]::member['a, T, U]`.
    MemberGeneric(MemberGeneric),
}

impl Resolution {
    /// Returns the ID of the resolved symbol.
    #[must_use]
    pub const fn global_id(&self) -> Global<pernixc_symbol::ID> {
        match self {
            Self::Module(id) => *id,
            Self::Variant(variant) => variant.variant_id,
            Self::Generic(generic) => generic.id,
            Self::MemberGeneric(member) => member.id,
        }
    }
}

fn to_resolution(
    resolved_id: Global<pernixc_symbol::ID>,
    symbol_kind: Kind,
    generic_arguments: Option<GenericArguments>,
    latest_resolution: Resolution,
) -> Resolution {
    match symbol_kind {
        Kind::Module => Resolution::Module(resolved_id),
        Kind::Struct
        | Kind::Enum
        | Kind::Function
        | Kind::ExternFunction
        | Kind::Trait
        | Kind::Constant
        | Kind::Type
        | Kind::Marker => {
            assert!(latest_resolution.is_module());

            Resolution::Generic(Generic {
                id: resolved_id,
                generic_arguments: generic_arguments.unwrap(),
            })
        }
        Kind::Variant => Resolution::Variant(Variant {
            variant_id: resolved_id,
            generic_arguments: latest_resolution
                .into_generic()
                .unwrap()
                .generic_arguments,
        }),
        Kind::TraitType
        | Kind::TraitFunction
        | Kind::TraitConstant
        | Kind::ImplementationConstant
        | Kind::ImplementationFunction
        | Kind::ImplementationType => {
            Resolution::MemberGeneric(MemberGeneric {
                id: resolved_id,
                parent_generic_arguments: latest_resolution
                    .into_generic()
                    .unwrap()
                    .generic_arguments,
                member_generic_arguments: generic_arguments.unwrap(),
            })
        }

        Kind::PositiveImplementation | Kind::NegativeImplementation => {
            unreachable!()
        }
    }
}

pub(super) async fn search_this(
    tracked_engine: &TrackedEngine,
    global: Global<pernixc_symbol::ID>,
) -> Option<(Global<pernixc_symbol::ID>, Kind)> {
    let mut scope_walker = tracked_engine.scope_walker(global);

    while let Some(current) = scope_walker.next().await {
        let current = global.target_id.make_global(current);

        let kind = tracked_engine.get_kind(current).await;
        if matches!(
            kind,
            Kind::Trait
                | Kind::Struct
                | Kind::Enum
                | Kind::PositiveImplementation
        ) {
            return Some((current, kind));
        }
    }

    None
}

#[allow(clippy::too_many_lines)]
pub(super) async fn resolve_root(
    tracked_engine: &TrackedEngine,
    root: &QualifiedIdentifierRoot,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Resolution, Error> {
    let resolution = match root {
        QualifiedIdentifierRoot::Target(_) => Resolution::Module(Global::new(
            config.referring_site.target_id,
            tracked_engine
                .get_target_root_module_id(config.referring_site.target_id)
                .await,
        )),
        QualifiedIdentifierRoot::This(this) => {
            let found_this =
                search_this(tracked_engine, config.referring_site).await;

            let Some((this_symbol, kind)) = found_this else {
                handler.receive(Diagnostic::ThisNotFound(ThisNotFound {
                    this_span: this.span,
                }));
                return Err(Error::Abort);
            };

            match kind {
                Kind::Trait | Kind::Enum | Kind::Struct => {
                    Resolution::Generic(Generic {
                        id: this_symbol,
                        generic_arguments: tracked_engine
                            .get_generic_parameters(this_symbol)
                            .await?
                            .create_identity_generic_arguments(this_symbol),
                    })
                }

                Kind::PositiveImplementation => {
                    let Some(implemented_id) =
                        tracked_engine.get_implements(this_symbol).await?
                    else {
                        return Err(Error::Abort);
                    };

                    let Some(implemented_generic_arguments) = tracked_engine
                        .get_implements_argument(implemented_id)
                        .await?
                    else {
                        return Err(Error::Abort);
                    };

                    Resolution::Generic(Generic {
                        id: implemented_id,
                        generic_arguments: implemented_generic_arguments
                            .deref()
                            .clone(),
                    })
                }

                _ => unreachable!(),
            }
        }
        QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
            let current_module_id = tracked_engine
                .get_closest_module_id(config.referring_site)
                .await;

            let current_module_id =
                Global::new(config.referring_site.target_id, current_module_id);

            let identifier =
                generic_identifier.identifier().ok_or(Error::Abort)?;

            let id = match tracked_engine
                .get_members(current_module_id)
                .await
                .member_ids_by_name
                .get(&identifier.kind.0)
                .copied()
                .map(|x| Global::new(current_module_id.target_id, x))
            {
                Some(id) => Some(id),
                None => tracked_engine
                    .get_imports(current_module_id)
                    .await
                    .get(&identifier.kind.0)
                    .copied()
                    .map(|x| x.id),
            };

            let Some(id) = id else {
                handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
                    searched_item_id: Some(current_module_id),
                    name: identifier.kind.0,
                    resolution_span: identifier.span,
                }));

                return Err(Error::Abort);
            };

            let symbol_kind = tracked_engine.get_kind(id).await;

            let generic_arguments = if symbol_kind.has_generic_parameters() {
                Some(
                    tracked_engine
                        .resolve_generic_arguments_for(
                            id,
                            generic_identifier,
                            config.reborrow(),
                            handler,
                        )
                        .await?,
                )
            } else {
                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(Diagnostic::NoGenericArgumentsRequired(
                        NoGenericArgumentsRequired {
                            global_id: id,
                            generic_argument_span: gen_args.span(),
                        },
                    ));
                }

                None
            };

            match (symbol_kind, generic_arguments) {
                (Kind::Module, None) => Resolution::Module(id),

                (_, Some(generic_arguments)) => {
                    Resolution::Generic(Generic { id, generic_arguments })
                }

                _ => unreachable!(),
            }
        }
    };

    if let Some(observer) = config.observer.as_mut() {
        observer.on_resolution_resolved(
            tracked_engine,
            config.referring_site,
            &resolution,
            &root.span(),
            handler,
        );
    }

    Ok(resolution)
}

/// Resolves a [`QualifiedIdentifier`] to a [`Resolution`].
#[extend]
pub async fn resolve_qualified_identifier(
    self: &TrackedEngine,
    qualified_identifier: &QualifiedIdentifier,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Resolution, Error> {
    // create the current root
    let mut latest_resolution = resolve_root(
        self,
        &qualified_identifier.root().ok_or(Error::Abort)?,
        config.reborrow(),
        handler,
    )
    .await?;

    for generic_identifier in qualified_identifier
        .subsequences()
        .filter_map(|x| x.generic_identifier())
    {
        let Some(identifier) = generic_identifier.identifier() else {
            continue;
        };

        let Some(resolved_id) = self
            .get_member_of(latest_resolution.global_id(), &identifier.kind)
            .await
        else {
            handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
                searched_item_id: Some(latest_resolution.global_id()),
                resolution_span: identifier.span,
                name: identifier.kind.0.clone(),
            }));

            return Err(Error::Abort);
        };

        let symbol_kind = self.get_kind(resolved_id).await;

        let generic_arguments = if symbol_kind.has_generic_parameters() {
            Some(
                self.resolve_generic_arguments_for(
                    resolved_id,
                    &generic_identifier,
                    config.reborrow(),
                    handler,
                )
                .await?,
            )
        } else {
            if let Some(gen_args) =
                generic_identifier.generic_arguments().as_ref()
            {
                handler.receive(Diagnostic::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        global_id: resolved_id,
                        generic_argument_span: gen_args.span(),
                    },
                ));
            }

            None
        };

        let next_resolution = to_resolution(
            resolved_id,
            symbol_kind,
            generic_arguments,
            latest_resolution,
        );

        if let Some(observer) = config.observer.as_mut() {
            observer.on_resolution_resolved(
                self,
                config.referring_site,
                &next_resolution,
                &generic_identifier.span(),
                handler,
            );
        }

        latest_resolution = next_resolution;
    }

    Ok(latest_resolution)
}
