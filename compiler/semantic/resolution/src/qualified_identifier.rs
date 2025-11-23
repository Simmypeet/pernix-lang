//! Contains the logic for resolving the qualified identifier.

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::{
    implemented::get_implemented, implements::get_implements,
    implements_arguments::get_implements_argument, import::get_import_map,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceElement;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    accessibility::symbol_accessible,
    get_target_root_module_id,
    kind::{Kind, get_kind},
    member::get_members,
    parent::{get_closest_module_id, scope_walker},
};
use pernixc_syntax::{
    Identifier, QualifiedIdentifier, QualifiedIdentifierRoot, SimplePath,
    SimplePathRoot,
};
use pernixc_target::{Global, get_linked_targets, get_target_map};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters, r#type::Type,
};

use crate::{
    Config, Diagnostic, Error, Handler,
    diagnostic::{
        NoGenericArgumentsRequired, SymbolIsNotAccessible, SymbolNotFound,
        ThisNotFound,
    },
    term::resolve_generic_arguments_for_internal,
};

/// Repersents a resolution to a symbol that can be supplied with generic
/// arguments such as `SYMBOl['a, T, U, V]`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Variant {
    /// The resolved variant symbol.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The generic arguments that are supplied to the parent enum.
    pub generic_arguments: GenericArguments,
}

/// Represents a resolution to a symbol that is a member of an another symbol
/// such as `Clone[T]::clone['a]`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    EnumAsInner,
)]
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
        | Kind::Effect
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
        | Kind::EffectOperation
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

/// Simply resolves the [`QualifiedIdentifierRoot`] to a [`GlobalID`].
///
/// No diagnostics are omitted and no generic arguments are considered.
#[extend]
pub async fn resolve_simple_qualified_identifier_root(
    self: &TrackedEngine,
    current_site: Global<pernixc_symbol::ID>,
    root: &QualifiedIdentifierRoot,
) -> Option<Global<pernixc_symbol::ID>> {
    match root {
        QualifiedIdentifierRoot::Target(_) => Some(Global::new(
            current_site.target_id,
            self.get_target_root_module_id(current_site.target_id).await,
        )),

        QualifiedIdentifierRoot::This(_) => {
            let Some((this_symbol, _)) = search_this(self, current_site).await
            else {
                return Some(current_site);
            };

            Some(this_symbol)
        }

        QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
            let current_module_id =
                self.get_closest_module_id(current_site).await;

            let current_module_id =
                Global::new(current_site.target_id, current_module_id);

            let identifier =
                generic_identifier.identifier().ok_or(Error::Abort).unwrap();

            match self
                .get_members(current_module_id)
                .await
                .member_ids_by_name
                .get(&identifier.kind.0)
                .copied()
                .map(|x| Global::new(current_module_id.target_id, x))
            {
                Some(id) => Some(id),
                None => self
                    .get_import_map(current_module_id)
                    .await
                    .get(&identifier.kind.0)
                    .copied()
                    .map(|x| x.id),
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
async fn resolve_root(
    tracked_engine: &TrackedEngine,
    root: &QualifiedIdentifierRoot,
    type_bound: Option<&Type>,
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
                        .get_implements_argument(this_symbol)
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
                    .get_import_map(current_module_id)
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
                        .resolve_generic_arguments_for_internal(
                            id,
                            (symbol_kind == Kind::Marker
                                || symbol_kind == Kind::Trait)
                                .then_some(type_bound)
                                .flatten(),
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

/// Resolves a [`QualifiedIdentifier`] to a [`Resolution`] where the last
/// identifier is expected to be a `marker` or `trait`. The resolution will
/// insert the given `type_bound` as the first generic argument to the last
/// identifier if it is a `marker` or `trait`.
#[allow(clippy::too_many_lines)]
#[extend]
pub async fn resolve_type_bound(
    self: &TrackedEngine,
    qualified_identifier: &QualifiedIdentifier,
    type_bound: &Type,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Resolution, Error> {
    self.resolve_qualified_identifier_internal(
        qualified_identifier,
        Some(type_bound),
        config.reborrow(),
        handler,
    )
    .await
}

/// Resolves a member of given name in the given symbol ID.
///
/// This analogous to doing `Symbol::member` resolution. where `Symbol` is
/// the `symbol_id` and `member` is the `identifier`.
#[extend]
pub async fn resolve_in(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    identifier: &str,
    consider_adt_implements: bool,
) -> Result<Option<Global<pernixc_symbol::ID>>, CyclicError> {
    if let Some(resolved_id) = self.get_member_of(symbol_id, identifier).await {
        return Ok(Some(resolved_id));
    }

    // try to search in the implements if the latest resolution is an
    // ADT

    let kind = self.get_kind(symbol_id).await;

    if !(kind.is_adt() && consider_adt_implements) {
        return Ok(None);
    }

    let implemented = self.get_implemented(symbol_id).await?;

    for impl_id in implemented.iter().copied() {
        let impl_members = self.get_members(impl_id).await;

        if let Some(resolved_id) =
            impl_members.member_ids_by_name.get(identifier)
        {
            return Ok(Some(Global::new(impl_id.target_id, *resolved_id)));
        }
    }

    Ok(None)
}

#[extend]
async fn resolve_step(
    self: &TrackedEngine,
    latest_resolution: Global<pernixc_symbol::ID>,
    identifier: &pernixc_syntax::Identifier,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Global<pernixc_symbol::ID>, Error> {
    let resolved_id = self
        .resolve_in(
            latest_resolution,
            &identifier.kind.0,
            config.consider_adt_implements,
        )
        .await?;

    let Some(resolved_id) = resolved_id else {
        handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
            searched_item_id: Some(latest_resolution),
            resolution_span: identifier.span,
            name: identifier.kind.0.clone(),
        }));

        return Err(Error::Abort);
    };

    // check if the symbol is accessible
    if !self.symbol_accessible(config.referring_site, resolved_id).await {
        handler.receive(Diagnostic::SymbolIsNotAccessible(
            SymbolIsNotAccessible {
                referring_site: config.referring_site,
                referred: resolved_id,
                referred_span: identifier.span,
            },
        ));
    }

    Ok(resolved_id)
}

#[extend]
async fn resolve_qualified_identifier_internal(
    self: &TrackedEngine,
    qualified_identifier: &QualifiedIdentifier,
    bound_type: Option<&Type>,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Resolution, Error> {
    // create the current root
    let mut latest_resolution = resolve_root(
        self,
        &qualified_identifier.root().ok_or(Error::Abort)?,
        bound_type,
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

        let resolved_id = self
            .resolve_step(
                latest_resolution.global_id(),
                &identifier,
                config.reborrow(),
                handler,
            )
            .await?;

        let symbol_kind = self.get_kind(resolved_id).await;

        let generic_arguments = if symbol_kind.has_generic_parameters() {
            Some(
                self.resolve_generic_arguments_for_internal(
                    resolved_id,
                    (symbol_kind == Kind::Marker || symbol_kind == Kind::Trait)
                        .then_some(bound_type)
                        .flatten(),
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

/// Resolves a [`QualifiedIdentifier`] to a [`Resolution`].
#[allow(clippy::too_many_lines)]
#[extend]
pub async fn resolve_qualified_identifier(
    self: &TrackedEngine,
    qualified_identifier: &QualifiedIdentifier,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Resolution, Error> {
    self.resolve_qualified_identifier_internal(
        qualified_identifier,
        None,
        config.reborrow(),
        handler,
    )
    .await
}

/// Resolves a [`SimplePath`] as a [`GlobalID`].
#[extend]
pub async fn resolve_simple_path(
    self: &TrackedEngine,
    simple_path: &SimplePath,
    referring_site: Global<pernixc_symbol::ID>,
    start_from_root: bool,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<pernixc_symbol::ID>> {
    // simple path should always have root tough
    let root: Global<pernixc_symbol::ID> = match simple_path.root()? {
        SimplePathRoot::Target(_) => Global::new(
            referring_site.target_id,
            self.get_target_root_module_id(referring_site.target_id).await,
        ),

        SimplePathRoot::Identifier(ident) => {
            if start_from_root {
                let target_map = self.get_target_map().await;
                let target =
                    self.get_linked_targets(referring_site.target_id).await;

                let Some(target_id) = target_map
                    .get(ident.kind.0.as_str())
                    .copied()
                    .filter(|x| {
                        x == &referring_site.target_id || { target.contains(x) }
                    })
                else {
                    handler.receive(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        },
                    ));

                    return None;
                };

                Global::new(
                    target_id,
                    self.get_target_root_module_id(target_id).await,
                )
            } else {
                let closet_module_id =
                    self.get_closest_module_id(referring_site).await;

                let global_closest_module_id =
                    Global::new(referring_site.target_id, closet_module_id);

                let id = match self
                    .get_members(global_closest_module_id)
                    .await
                    .member_ids_by_name
                    .get(ident.kind.0.as_str())
                    .map(|x| Global::new(referring_site.target_id, *x))
                {
                    Some(id) => Some(id),
                    None => self
                        .get_import_map(global_closest_module_id)
                        .await
                        .get(ident.kind.0.as_str())
                        .map(|x| x.id),
                };

                let Some(id) = id else {
                    handler.receive(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: Some(global_closest_module_id),
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        },
                    ));

                    return None;
                };

                id
            }
        }
    };

    self.resolve_sequence(
        simple_path.subsequences().flat_map(|x| x.identifier().into_iter()),
        referring_site,
        root,
        handler,
    )
    .await
}

/// Resolves a sequence of identifier starting of from the given `root`.
#[extend]
pub async fn resolve_sequence<'a>(
    self: &TrackedEngine,
    simple_path: impl Iterator<Item = Identifier>,
    referring_site: Global<pernixc_symbol::ID>,
    root: Global<pernixc_symbol::ID>,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<pernixc_symbol::ID>> {
    let mut lastest_resolution = root;

    for identifier in simple_path {
        let Some(new_id) = self
            .get_member_of(lastest_resolution, identifier.kind.0.as_str())
            .await
        else {
            handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
                searched_item_id: Some(lastest_resolution),
                resolution_span: identifier.span,
                name: identifier.kind.0,
            }));

            return None;
        };

        // non-fatal error, no need to return early
        if !self.symbol_accessible(referring_site, new_id).await {
            handler.receive(Diagnostic::SymbolIsNotAccessible(
                SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span,
                },
            ));
        }

        lastest_resolution = new_id;
    }

    Some(lastest_resolution)
}

/// Searches for a member in the given symbol scope and returns its ID if it
/// exists.
#[extend]
pub async fn get_member_of(
    self: &TrackedEngine,
    id: Global<pernixc_symbol::ID>,
    member_name: &str,
) -> Option<Global<pernixc_symbol::ID>> {
    let symbol_kind = self.get_kind(id).await;

    if symbol_kind.has_member() {
        self.get_members(id)
            .await
            .member_ids_by_name
            .get(member_name)
            .copied()
            .map(|x| id.target_id.make_global(x))
    } else {
        None
    }
}
