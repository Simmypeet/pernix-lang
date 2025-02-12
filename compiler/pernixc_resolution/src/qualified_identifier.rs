//! Contains the logic for resolving the qualified identifier.

use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_component::implementation::Implementation;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    QualifiedIdentifier, QualifiedIdentifierRoot,
};
use pernixc_table::{
    component::{Implements, Import, Member, SymbolKind},
    resolution::diagnostic::{
        NoGenericArgumentsRequired, SymbolNotFound, ThisNotFound,
    },
    GlobalID, Table,
};
use pernixc_term::{
    generic_arguments::GenericArguments, generic_parameter::GenericParameters,
    Model,
};

use crate::{Config, Diagnostic, Ext, Handler};

/// Repersents a resolution to a symbol that can be supplied with generic
/// arguments such as `SYMBOl['a, T, U, V]`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic<M: Model> {
    /// The resolved symbbol.
    ///
    /// This includes: `struct`, `enum`, `function`, `trait`, `const`,
    /// `type`, `marker` and trait implementation's `type`, `function`, and
    /// `const`.
    ///
    /// Trait implementation members are included here instead of in the
    /// `MemberGeneric` because you can never refer to a trait implementation
    /// member while specifying the trait implementation itself.
    ///
    /// # Possible Kind
    /// - [`pernixc_table::SymbolKind::Struct`]
    /// - [`pernixc_table::SymbolKind::Enum`]
    /// - [`pernixc_table::SymbolKind::ExternFunction`]
    /// - [`pernixc_table::SymbolKind::Function`]
    /// - [`pernixc_table::SymbolKind::Trait`]
    /// - [`pernixc_table::SymbolKind::Constant`]
    /// - [`pernixc_table::SymbolKind::Type`]
    /// - [`pernixc_table::SymbolKind::Marker`]
    /// - [`pernixc_table::SymbolKind::TraitImplementationType`]
    /// - [`pernixc_table::SymbolKind::TraitImplementationFunction`]
    /// - [`pernixc_table::SymbolKind::TraitImplementationConstant`]
    pub id: GlobalID,

    /// The generic arguments that are supplied to the resolved symbol.
    pub generic_arguments: GenericArguments<M>,
}

/// Represents a resolution to an enum-varaint symbol such as
/// `Option[int32]::None`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The resolved variant symbol.
    pub variant_id: GlobalID,

    /// The generic arguments that are supplied to the parent enum.
    pub generic_arguments: GenericArguments<M>,
}

/// Represents a resolution to a symbol that is a member of an another symbol
/// such as `Clone[T]::clone['a]`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberGeneric<M: Model> {
    /// The resolved member symbol.
    ///
    /// This includes: trait member's `type`, `function`, and `const`, and
    /// adt implementation's `function`.
    ///
    /// # Possible Kind
    ///
    /// - [`pernixc_table::SymbolKind::TraitType`]
    /// - [`pernixc_table::SymbolKind::TraitFunction`]
    /// - [`pernixc_table::SymbolKind::TraitConstant`]
    /// - [`pernixc_table::SymbolKind::AdtImplementationFunction`]
    pub id: GlobalID,

    /// The generic arguments that are supplied to the parent symbol. Suppose
    /// `Clone[T]::clone['a]`, then `[T]` is the parent generic arguments.
    pub parent_generic_arguments: GenericArguments<M>,

    /// The generic arguments that are supplied to the member symbol. Suppose
    /// `Clone[T]::clone['a]`, then `['a]` is the member generic arguments.
    pub member_generic_arguments: GenericArguments<M>,
}

/// Represents a resolution of a qualified identifier syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Resolution<M: Model> {
    /// Resolved to a module symbol.
    Module(GlobalID),

    /// Resolved to an enum-variant symbol.
    Variant(Variant<M>),

    /// Resolved to a symbol with generic arguments such as `Symbol['a, T, U]`.
    Generic(Generic<M>),

    /// Resolved to a member symbol with generic arguments such as
    /// `Symbol[V]::member['a, T, U]`.
    MemberGeneric(MemberGeneric<M>),

    /// Refers to a positive trait implementation. This is usually happens when
    /// using keyword `this` in the trait implementation.
    PositiveTraitImplementation(GlobalID),
}

impl<M: Model> Resolution<M> {
    /// Returns the ID of the resolved symbol.
    #[must_use]
    pub const fn global_id(&self) -> GlobalID {
        match self {
            Self::PositiveTraitImplementation(id) | Self::Module(id) => *id,

            Self::Variant(variant) => variant.variant_id,
            Self::Generic(generic) => generic.id,
            Self::MemberGeneric(member) => member.id,
        }
    }
}

fn to_resolution<M: Model>(
    resolved_id: GlobalID,
    symbol_kind: SymbolKind,
    generic_arguments: Option<GenericArguments<M>>,
    latest_resolution: Resolution<M>,
) -> Resolution<M> {
    match symbol_kind {
        SymbolKind::Module => Resolution::Module(resolved_id),

        SymbolKind::Struct
        | SymbolKind::Enum
        | SymbolKind::Function
        | SymbolKind::ExternFunction
        | SymbolKind::Trait
        | SymbolKind::Constant
        | SymbolKind::Type
        | SymbolKind::Marker => {
            assert!(latest_resolution.is_module());

            Resolution::Generic(Generic {
                id: resolved_id,
                generic_arguments: generic_arguments.unwrap(),
            })
        }

        SymbolKind::TraitImplementationFunction
        | SymbolKind::TraitImplementationType
        | SymbolKind::TraitImplementationConstant => {
            assert!(latest_resolution.is_positive_trait_implementation());

            Resolution::Generic(Generic {
                id: resolved_id,
                generic_arguments: generic_arguments.unwrap(),
            })
        }

        SymbolKind::Variant => Resolution::Variant(Variant {
            variant_id: resolved_id,
            generic_arguments: latest_resolution
                .into_generic()
                .unwrap()
                .generic_arguments,
        }),

        SymbolKind::TraitType
        | SymbolKind::TraitFunction
        | SymbolKind::TraitConstant
        | SymbolKind::AdtImplementationFunction => {
            Resolution::MemberGeneric(MemberGeneric {
                id: resolved_id,
                parent_generic_arguments: latest_resolution
                    .into_generic()
                    .unwrap()
                    .generic_arguments,
                member_generic_arguments: generic_arguments.unwrap(),
            })
        }

        SymbolKind::PositiveMarkerImplementation
        | SymbolKind::NegativeMarkerImplementation
        | SymbolKind::PositiveTraitImplementation
        | SymbolKind::NegativeTraitImplementation
        | SymbolKind::AdtImplementation => unreachable!(),
    }
}

#[allow(clippy::too_many_lines)]
pub(super) fn resolve_root<M: Model>(
    table: &Table,
    root: &QualifiedIdentifierRoot,
    referring_site: GlobalID,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Resolution<M>, Abort> {
    let resolution = match root {
        QualifiedIdentifierRoot::Target(_) => {
            Resolution::Module(GlobalID::new(
                referring_site.target_id,
                pernixc_table::ID::ROOT_MODULE,
            ))
        }
        QualifiedIdentifierRoot::This(this) => {
            let found_this = table.scope_walker(referring_site).find_map(|x| {
                match *table.get::<SymbolKind>(GlobalID::new(
                    referring_site.target_id,
                    x,
                )) {
                    kind @ (SymbolKind::Trait
                    | SymbolKind::PositiveTraitImplementation
                    | SymbolKind::AdtImplementation) => {
                        Some((GlobalID::new(referring_site.target_id, x), kind))
                    }
                    _ => None,
                }
            });

            let Some((this_symbol, kind)) = found_this else {
                handler.receive(Box::new(ThisNotFound {
                    span: this.span.clone(),
                }));
                return Err(Abort);
            };

            match kind {
                SymbolKind::Trait => Resolution::Generic(Generic {
                    id: this_symbol,
                    generic_arguments: table
                        .query::<GenericParameters>(this_symbol)?
                        .create_identity_generic_arguments(this_symbol),
                }),

                SymbolKind::PositiveTraitImplementation => {
                    Resolution::PositiveTraitImplementation(this_symbol)
                }

                SymbolKind::AdtImplementation => {
                    let implemented_id = // should be enum or struct
                        table.get::<Implements>(this_symbol).0;
                    let implementation_generic_arguments =
                        GenericArguments::from_default_model(
                            table
                                .query::<Implementation>(this_symbol)?
                                .generic_arguments
                                .clone(),
                        );

                    Resolution::Generic(Generic {
                        id: implemented_id,
                        generic_arguments: implementation_generic_arguments,
                    })
                }

                _ => unreachable!(),
            }
        }
        QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
            let current_module_id = table.get_closet_module_id(referring_site);

            let current_module_id =
                GlobalID::new(referring_site.target_id, current_module_id);

            let id = table
                .get::<Member>(current_module_id)
                .get(generic_identifier.identifier().span.str())
                .copied()
                .map(|x| GlobalID::new(current_module_id.target_id, x))
                .or_else(|| {
                    table
                        .get::<Import>(current_module_id)
                        .get(generic_identifier.identifier().span.str())
                        .map(|x| x.id)
                });

            let Some(id) = id else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(current_module_id),
                    resolution_span: generic_identifier
                        .identifier()
                        .span
                        .clone(),
                }));

                return Err(Abort);
            };

            let symbol_kind = *table.get::<SymbolKind>(id);

            let generic_arguments = if symbol_kind.has_generic_parameters() {
                Some(table.resolve_generic_arguments_for(
                    id,
                    generic_identifier,
                    referring_site,
                    config.reborrow(),
                    handler,
                )?)
            } else {
                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id: id,
                        generic_argument_span: gen_args.span(),
                    }));
                }

                None
            };

            match (symbol_kind, generic_arguments) {
                (SymbolKind::Module, None) => Resolution::Module(id),

                (_, Some(generic_arguments)) => {
                    Resolution::Generic(Generic { id, generic_arguments })
                }

                _ => unreachable!(),
            }
        }
    };

    if let Some(observer) = config.observer.as_mut() {
        observer.on_resolution_resolved(
            table,
            referring_site,
            &resolution,
            &root.span(),
            handler,
        );
    }

    Ok(resolution)
}

pub(super) fn resolve<M: Model>(
    table: &Table,
    qualified_identifier: &QualifiedIdentifier,
    referring_site: GlobalID,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Resolution<M>, Abort> {
    // create the current root
    let mut latest_resolution = table.resolve_qualified_identifier_root(
        qualified_identifier.root(),
        referring_site,
        config.reborrow(),
        handler,
    )?;

    for (_, generic_identifier) in qualified_identifier.rest() {
        let Some(resolved_id) = table.get_member_of(
            latest_resolution.global_id(),
            generic_identifier.identifier().span.str(),
        ) else {
            handler.receive(Box::new(SymbolNotFound {
                searched_item_id: Some(latest_resolution.global_id()),
                resolution_span: generic_identifier.identifier().span.clone(),
            }));

            return Err(Abort);
        };

        let symbol_kind = *table.get::<SymbolKind>(resolved_id);

        let generic_arguments = if symbol_kind.has_generic_parameters() {
            Some(table.resolve_generic_arguments_for(
                resolved_id,
                generic_identifier,
                referring_site,
                config.reborrow(),
                handler,
            )?)
        } else {
            if let Some(gen_args) =
                generic_identifier.generic_arguments().as_ref()
            {
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: resolved_id,
                    generic_argument_span: gen_args.span(),
                }));
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
                table,
                referring_site,
                &next_resolution,
                &generic_identifier.span(),
                handler,
            );
        }

        latest_resolution = next_resolution;
    }

    Ok(latest_resolution)
}
