//! Contains the logic for resolving the qualified identifier.

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    implemented::get_implemented, implements::get_implements,
    implements_arguments::get_implements_argument, import::get_import_map,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::symbol_accessible,
    get_target_root_module_id,
    kind::{Kind, get_kind},
    member::{get_member_by_name, get_members},
    name::get_qualified_name,
    parent::{get_closest_module_id, get_parent_global, scope_walker},
};
use pernixc_syntax::{
    Identifier, QualifiedIdentifier, QualifiedIdentifierRoot, SimplePath,
    SimplePathRoot,
};
use pernixc_target::{Global, TargetID, get_linked_targets, get_target_map};
use pernixc_term::{
    TermMut, TermRef,
    display::{self, Display},
    generic_arguments::{
        AssociatedSymbol, DisplaySymbolWithGenericArguments, GenericArguments,
        Symbol, create_identity_generic_arguments,
    },
    generic_parameters::get_generic_parameters,
    instance::{Instance, InstanceAssociated},
    instantiation::Instantiation,
    r#type::Type,
};
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    Error, Resolver,
    diagnostic::{
        Diagnostic, NoGenericArgumentsRequired, NoMemberInSymbol,
        NoMemberInType, SymbolIsNotAccessible, SymbolNotFound, ThisNotFound,
    },
};

/// Represents a resolution to the variant.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Variant(Symbol);

impl display::Display for Variant {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let parent_enum_id = self.parent_enum_id(engine).await;

        DisplaySymbolWithGenericArguments::new(
            parent_enum_id,
            self.0.generic_arguments(),
        )
        .fmt(engine, formatter)
        .await
    }
}

impl Variant {
    /// Creates a new [`Variant`] from the given [`Symbol`].
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Creates a new [`Variant`]
    #[must_use]
    pub const fn new(
        variant_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: GenericArguments,
    ) -> Self {
        Self(Symbol::new(variant_id, generic_arguments))
    }

    /// Returns the ID of the resolved variant symbol.
    #[must_use]
    pub const fn variant_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.0.id()
    }

    /// Returns the generic arguments supplied to the parent enum of the variant
    /// symbol.
    #[must_use]
    pub const fn enum_generic_arguments(&self) -> &GenericArguments {
        self.0.generic_arguments()
    }

    /// Returns the type of the variant.
    pub async fn create_enum_type(&self, engine: &TrackedEngine) -> Type {
        let enum_id =
            engine.get_parent_global(self.variant_id()).await.unwrap();

        Type::new_symbol(enum_id, self.0.generic_arguments().clone())
    }

    /// Creates [`Instantiation`] for this variant.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.0.create_instantiation_parent(engine).await
    }

    /// Returns an iterator yielding mutable references to all terms appeared in
    /// the generic arguments
    pub fn iter_all_term_mut(
        &mut self,
    ) -> impl Iterator<Item = TermMut<'_>> + '_ {
        self.0.iter_all_term_mut()
    }

    /// Returns an iterator yielding immutable references to all terms appeared
    /// in the generic arguments.
    pub fn iter_all_term(&self) -> impl Iterator<Item = TermRef<'_>> + '_ {
        self.0.iter_all_term()
    }

    #[must_use]
    pub async fn parent_enum_id(
        &self,
        engine: &TrackedEngine,
    ) -> Global<pernixc_symbol::SymbolID> {
        engine.get_parent_global(self.variant_id()).await.unwrap()
    }
}

/// Represents a resolution to an ADT implements member.
///
/// This is the intermediate resolution, which can't be directly used to create
/// an [`Instantiation`] to the implements associated symbol.
///
/// For example,
///
/// ```pnx
/// public struct Test[T, U]:
///     pass
///
///
/// implements[T] Test[T]:
///     public function inner()
/// ```
///
/// When will resolve for `Test[int32, bool]::inner`, the resolution would be
/// valid and produced this struct where [`Self::adt_generic_arguments`] is
/// `[int32, bool]` and [`Self::impl_associated_generic_arguments`] is `[int32]`
/// .
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct IntermediateAdtImplSymbol {
    adt_generic_arguments: GenericArguments,
    impl_associated_generic_arguments: GenericArguments,
    impl_associated_symbol_id: Global<pernixc_symbol::SymbolID>,
}

impl IntermediateAdtImplSymbol {
    /// Creates a new [`IntermediateAdtImplSymbol`] with the given generic
    /// arguments and the symbol ID of the implements associated symbol.
    #[must_use]
    pub const fn new(
        adt_generic_arguments: GenericArguments,
        impl_associated_generic_arguments: GenericArguments,
        impl_associated_symbol_id: Global<pernixc_symbol::SymbolID>,
    ) -> Self {
        Self {
            adt_generic_arguments,
            impl_associated_generic_arguments,
            impl_associated_symbol_id,
        }
    }

    /// Returns the impl associated symbol ID.
    #[must_use]
    pub const fn impl_associated_symbol_id(
        &self,
    ) -> Global<pernixc_symbol::SymbolID> {
        self.impl_associated_symbol_id
    }

    #[must_use]
    pub async fn get_parent_impl_id(
        &self,
        engine: &TrackedEngine,
    ) -> Global<pernixc_symbol::SymbolID> {
        engine.get_parent_global(self.impl_associated_symbol_id).await.unwrap()
    }

    #[must_use]
    pub const fn adt_generic_arguments(&self) -> &GenericArguments {
        &self.adt_generic_arguments
    }

    #[must_use]
    pub fn into_impl_associated_generic_arguments(self) -> GenericArguments {
        self.impl_associated_generic_arguments
    }

    #[must_use]
    pub const fn impl_associated_generic_arguments(&self) -> &GenericArguments {
        &self.impl_associated_generic_arguments
    }
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
    Encode,
    Decode,
    Identifiable,
    EnumAsInner,
)]
pub enum Resolution {
    /// Resolved to a module symbol.
    Module(Global<pernixc_symbol::SymbolID>),

    /// Resolved to an enum-variant symbol.
    Variant(Variant),

    /// Resolved to a symbol with generic arguments such as `Symbol['a, T, U]`.
    GenericSymbol(Symbol),

    /// See [`IntermediateAdtImplSymbol`] for more details.
    IntermediateAdtImplSymbol(IntermediateAdtImplSymbol),

    /// Resolved to a member symbol with generic arguments such as
    /// `Symbol[V]::member['a, T, U]`.
    GenericAssociatedSymbol(AssociatedSymbol),

    /// Resolved to a type, supplied from [`ExtraNamespace`].
    Type(Type),

    /// Resolved to an instance, supplied from [`ExtraNamespace`].
    Instance(Instance),

    /// Resolved to an instance associated symbol such as `I::function['a, T]`
    /// where `I` is an instance.
    InstanceAssociatedSymbol(InstanceAssociated),
}

impl Resolution {
    /// Returns the ID of the resolved symbol.
    #[must_use]
    pub const fn global_id(&self) -> Option<Global<pernixc_symbol::SymbolID>> {
        match self {
            Self::Module(id) => Some(*id),
            Self::Variant(variant) => Some(variant.variant_id()),
            Self::GenericSymbol(generic) => Some(generic.id()),
            Self::GenericAssociatedSymbol(member) => Some(member.id()),
            Self::IntermediateAdtImplSymbol(impl_symbol) => {
                Some(impl_symbol.impl_associated_symbol_id)
            }
            Self::InstanceAssociatedSymbol(func) => {
                Some(func.trait_associated_symbol_id())
            }

            Self::Instance(_) | Self::Type(_) => None,
        }
    }

    /// Renders the resolved [`Resolution`] into a user-friendly string for
    /// diagnostics.
    #[must_use]
    pub async fn found_string(&self, engine: &TrackedEngine) -> String {
        if let Some(global_id) = self.global_id() {
            let qualified_name = engine.get_qualified_name(global_id).await;
            let kind = engine.get_kind(global_id).await;

            format!("{} {qualified_name}", kind.kind_str())
        } else {
            match self {
                Self::Module(_)
                | Self::Variant(_)
                | Self::GenericSymbol(_)
                | Self::IntermediateAdtImplSymbol(_)
                | Self::GenericAssociatedSymbol(_)
                | Self::InstanceAssociatedSymbol(_) => {
                    unreachable!("should've gotten a global_id()")
                }

                Self::Type(ty) => {
                    let mut string = "type ".to_string();
                    ty.write_async(engine, &mut string).await.unwrap();
                    string
                }

                Self::Instance(instance) => {
                    let mut string = "instance ".to_string();
                    instance.write_async(engine, &mut string).await.unwrap();
                    string
                }
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn to_resolution(
    resolved_id: Global<pernixc_symbol::SymbolID>,
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
        | Kind::Instance
        | Kind::Marker => {
            assert!(latest_resolution.is_module());

            Resolution::GenericSymbol(Symbol::new(
                resolved_id,
                generic_arguments.unwrap(),
            ))
        }

        Kind::Variant => Resolution::Variant(Variant::new(
            resolved_id,
            latest_resolution
                .into_generic_symbol()
                .unwrap()
                .into_generic_arguments(),
        )),

        trait_associated @ (Kind::TraitAssociatedType
        | Kind::TraitAssociatedFunction
        | Kind::TraitAssociatedConstant
        | Kind::TraitAssociatedInstance) => match latest_resolution {
            Resolution::GenericSymbol(generic) => {
                Resolution::GenericAssociatedSymbol(AssociatedSymbol::new(
                    resolved_id,
                    generic.into_generic_arguments(),
                    generic_arguments.unwrap(),
                ))
            }

            Resolution::Instance(instance) => match trait_associated {
                Kind::TraitAssociatedInstance
                | Kind::TraitAssociatedConstant
                | Kind::TraitAssociatedType
                | Kind::TraitAssociatedFunction => {
                    Resolution::InstanceAssociatedSymbol(
                        InstanceAssociated::new(
                            Box::new(instance),
                            resolved_id,
                            generic_arguments.unwrap(),
                        ),
                    )
                }

                _ => {
                    unreachable!()
                }
            },

            Resolution::Type(_)
            | Resolution::GenericAssociatedSymbol(_)
            | Resolution::Module(_)
            | Resolution::InstanceAssociatedSymbol(_)
            | Resolution::IntermediateAdtImplSymbol(_)
            | Resolution::Variant(_) => unreachable!(),
        },

        Kind::EffectOperation => {
            Resolution::GenericAssociatedSymbol(AssociatedSymbol::new(
                resolved_id,
                latest_resolution
                    .into_generic_symbol()
                    .unwrap()
                    .into_generic_arguments(),
                generic_arguments.unwrap(),
            ))
        }

        Kind::ImplementationAssociatedConstant
        | Kind::ImplementationAssociatedType
        | Kind::ImplementationAssociatedFunction => {
            Resolution::IntermediateAdtImplSymbol(IntermediateAdtImplSymbol {
                adt_generic_arguments: latest_resolution
                    .into_generic_symbol()
                    .unwrap()
                    .into_generic_arguments(),
                impl_associated_generic_arguments: generic_arguments.unwrap(),
                impl_associated_symbol_id: resolved_id,
            })
        }

        Kind::PositiveImplementation | Kind::NegativeImplementation => {
            unreachable!()
        }

        Kind::InstanceAssociatedType
        | Kind::InstanceAssociatedFunction
        | Kind::InstanceAssociatedConstant
        | Kind::InstanceAssociatedInstance => {
            Resolution::GenericAssociatedSymbol(AssociatedSymbol::new(
                resolved_id,
                match latest_resolution {
                    Resolution::GenericSymbol(generic) => {
                        generic.into_generic_arguments()
                    }
                    Resolution::Instance(instance) => {
                        instance.into_symbol().unwrap().into_generic_arguments()
                    }

                    _ => unreachable!(),
                },
                generic_arguments.unwrap(),
            ))
        }
    }
}

pub(super) async fn search_this(
    tracked_engine: &TrackedEngine,
    global: Global<pernixc_symbol::SymbolID>,
) -> Option<(Global<pernixc_symbol::SymbolID>, Kind)> {
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
                | Kind::Instance
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
    current_site: Global<pernixc_symbol::SymbolID>,
    root: &QualifiedIdentifierRoot,
) -> Option<Global<pernixc_symbol::SymbolID>> {
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

impl Resolver<'_, '_> {
    #[allow(clippy::too_many_lines)]
    async fn resolve_root(
        &mut self,
        root: &QualifiedIdentifierRoot,
        type_bound: Option<&Type>,
    ) -> Result<Resolution, Error> {
        let resolution = match root {
            QualifiedIdentifierRoot::Target(_) => {
                Resolution::Module(Global::new(
                    self.referring_site().target_id,
                    self.tracked_engine()
                        .get_target_root_module_id(
                            self.referring_site().target_id,
                        )
                        .await,
                ))
            }

            QualifiedIdentifierRoot::This(this) => {
                let found_this =
                    search_this(self.tracked_engine(), self.referring_site())
                        .await;

                let Some((this_symbol, kind)) = found_this else {
                    self.receive_diagnostic(Diagnostic::ThisNotFound(
                        ThisNotFound { this_span: this.span },
                    ));
                    return Err(Error::Abort);
                };

                match kind {
                    Kind::Trait
                    | Kind::Enum
                    | Kind::Struct
                    | Kind::Instance => Resolution::GenericSymbol(Symbol::new(
                        this_symbol,
                        this_symbol
                            .create_identity_generic_arguments(
                                self.tracked_engine(),
                            )
                            .await,
                    )),

                    Kind::PositiveImplementation => {
                        let Some(implemented_id) = self
                            .tracked_engine()
                            .get_implements(this_symbol)
                            .await
                        else {
                            return Err(Error::Abort);
                        };

                        let Some(implemented_generic_arguments) = self
                            .tracked_engine()
                            .get_implements_argument(this_symbol)
                            .await
                        else {
                            return Err(Error::Abort);
                        };

                        Resolution::GenericSymbol(Symbol::new(
                            implemented_id,
                            implemented_generic_arguments.deref().clone(),
                        ))
                    }

                    _ => unreachable!(),
                }
            }

            QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
                let current_module_id = self
                    .tracked_engine()
                    .get_closest_module_id(self.referring_site())
                    .await;

                let current_module_id = Global::new(
                    self.referring_site().target_id,
                    current_module_id,
                );

                let identifier =
                    generic_identifier.identifier().ok_or(Error::Abort)?;

                // try resolve from the extra namespace first
                // search from type then instance
                if let Some(ty) =
                    self.lookup_extra_type(identifier.kind.0.as_ref())
                {
                    return Ok(Resolution::Type(ty));
                }

                if let Some(instance) =
                    self.lookup_extra_instance(identifier.kind.0.as_ref())
                {
                    return Ok(Resolution::Instance(instance));
                }

                let id = match self
                    .tracked_engine()
                    .get_members(current_module_id)
                    .await
                    .member_ids_by_name
                    .get(&identifier.kind.0)
                    .copied()
                    .map(|x| Global::new(current_module_id.target_id, x))
                {
                    Some(id) => Some(id),
                    None => self
                        .tracked_engine()
                        .get_import_map(current_module_id)
                        .await
                        .get(&identifier.kind.0)
                        .copied()
                        .map(|x| x.id),
                };

                let Some(id) = id else {
                    self.receive_diagnostic(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: Some(current_module_id),
                            name: identifier.kind.0,
                            resolution_span: identifier.span,
                        },
                    ));

                    return Err(Error::Abort);
                };

                let symbol_kind = self.tracked_engine().get_kind(id).await;

                let generic_arguments = if symbol_kind.has_generic_parameters()
                {
                    Some(
                        self.resolve_generic_arguments_for_internal(
                            id,
                            (symbol_kind == Kind::Marker
                                || symbol_kind == Kind::Trait)
                                .then_some(type_bound)
                                .flatten(),
                            generic_identifier,
                        )
                        .await,
                    )
                } else {
                    if let Some(gen_args) =
                        generic_identifier.generic_arguments().as_ref()
                    {
                        self.receive_diagnostic(
                            Diagnostic::NoGenericArgumentsRequired(
                                NoGenericArgumentsRequired {
                                    global_id: id,
                                    generic_argument_span: gen_args.span(),
                                },
                            ),
                        );
                    }

                    None
                };

                match (symbol_kind, generic_arguments) {
                    (Kind::Module, None) => Resolution::Module(id),

                    (_, Some(generic_arguments)) => Resolution::GenericSymbol(
                        Symbol::new(id, generic_arguments),
                    ),

                    _ => unreachable!(),
                }
            }
        };

        self.notify_resolution_resolved(&resolution, &root.span());

        Ok(resolution)
    }

    /// Resolves a [`QualifiedIdentifier`] to a [`Resolution`] where the last
    /// identifier is expected to be a `marker` or `trait`. The resolution will
    /// insert the given `type_bound` as the first generic argument to the last
    /// identifier if it is a `marker` or `trait`.
    #[allow(clippy::too_many_lines)]
    pub async fn resolve_type_bound(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        type_bound: &Type,
    ) -> Result<Resolution, Error> {
        self.resolve_qualified_identifier_internal(
            qualified_identifier,
            Some(type_bound),
        )
        .await
    }

    #[allow(clippy::too_many_lines)]
    async fn resolve_step(
        &mut self,
        latest_resolution: &Resolution,
        identifier: &pernixc_syntax::Identifier,
    ) -> Result<Global<pernixc_symbol::SymbolID>, Error> {
        let resolved = match latest_resolution {
            normal @ (Resolution::Module(_)
            | Resolution::Variant(_)
            | Resolution::GenericSymbol(_)
            | Resolution::GenericAssociatedSymbol(_)) => {
                let global_id = normal
                    .global_id()
                    .expect("these four variants should have global_id");

                let resolved_id = self
                    .tracked_engine()
                    .resolve_in(
                        global_id,
                        self.referring_site().target_id,
                        &identifier.kind.0,
                        self.consider_adt_implements(),
                    )
                    .await;

                let Some(resolved_id) = resolved_id else {
                    self.receive_diagnostic(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: Some(global_id),
                            resolution_span: identifier.span,
                            name: identifier.kind.0.clone(),
                        },
                    ));

                    return Err(Error::Abort);
                };

                resolved_id
            }

            Resolution::Type(ty) => {
                self.receive_diagnostic(Diagnostic::NoMemberInType(
                    NoMemberInType::builder()
                        .resolution_span(identifier.span)
                        .r#type(ty.clone())
                        .build(),
                ));

                return Err(Error::Abort);
            }

            Resolution::Instance(instance) => match instance {
                Instance::AnonymousTrait(tr) => {
                    let member_id = self
                        .tracked_engine()
                        .get_member_by_name(tr.trait_id(), &identifier.kind)
                        .await;

                    let Some(member_id) = member_id else {
                        self.receive_diagnostic(Diagnostic::SymbolNotFound(
                            SymbolNotFound {
                                searched_item_id: Some(tr.trait_id()),
                                resolution_span: identifier.span,
                                name: identifier.kind.0.clone(),
                            },
                        ));

                        return Err(Error::Abort);
                    };

                    member_id
                }

                Instance::Symbol(symbol) => {
                    let member_id = self
                        .tracked_engine()
                        .get_member_by_name(symbol.id(), &identifier.kind)
                        .await;

                    let Some(member_id) = member_id else {
                        self.receive_diagnostic(Diagnostic::SymbolNotFound(
                            SymbolNotFound {
                                searched_item_id: Some(symbol.id()),
                                resolution_span: identifier.span,
                                name: identifier.kind.0.clone(),
                            },
                        ));

                        return Err(Error::Abort);
                    };

                    member_id
                }

                Instance::Parameter(member_id) => {
                    let trait_id = if let Some(fut) =
                        self.resolve_instance_parameter_trait_ref(member_id)
                    {
                        fut.await.ok_or(Error::Abort)?
                    } else {
                        self.tracked_engine()
                            .get_generic_parameters(member_id.parent_id())
                            .await[member_id.id()]
                        .trait_ref()
                        .map(|x| x.trait_id())
                        .ok_or(Error::Abort)?
                    };

                    let Some(member_id) = self
                        .tracked_engine()
                        .get_member_by_name(trait_id, &identifier.kind)
                        .await
                    else {
                        self.receive_diagnostic(Diagnostic::SymbolNotFound(
                            SymbolNotFound {
                                searched_item_id: Some(trait_id),
                                resolution_span: identifier.span,
                                name: identifier.kind.0.clone(),
                            },
                        ));

                        return Err(Error::Abort);
                    };

                    member_id
                }

                Instance::InstanceAssociated(instance_associated) => {
                    let trait_id =
                        instance_associated.trait_associated_symbol_id();

                    let Some(member_id) = self
                        .tracked_engine()
                        .get_member_by_name(trait_id, &identifier.kind)
                        .await
                    else {
                        self.receive_diagnostic(Diagnostic::SymbolNotFound(
                            SymbolNotFound {
                                searched_item_id: Some(trait_id),
                                resolution_span: identifier.span,
                                name: identifier.kind.0.clone(),
                            },
                        ));

                        return Err(Error::Abort);
                    };

                    member_id
                }

                Instance::Inference(_) | Instance::Error(_) => {
                    return Err(Error::Abort);
                }
            },

            Resolution::InstanceAssociatedSymbol(function_id) => {
                self.receive_diagnostic(Diagnostic::NoMemberInFunction(
                    NoMemberInSymbol::builder()
                        .resolution_span(identifier.span)
                        .symbol_id(function_id.trait_associated_symbol_id())
                        .build(),
                ));

                return Err(Error::Abort);
            }

            Resolution::IntermediateAdtImplSymbol(symbol) => {
                self.receive_diagnostic(Diagnostic::NoMemberInFunction(
                    NoMemberInSymbol::builder()
                        .resolution_span(identifier.span)
                        .symbol_id(symbol.impl_associated_symbol_id())
                        .build(),
                ));

                return Err(Error::Abort);
            }
        };

        // check if the symbol is accessible
        if !self
            .tracked_engine()
            .symbol_accessible(self.referring_site(), resolved)
            .await
        {
            self.receive_diagnostic(Diagnostic::SymbolIsNotAccessible(
                SymbolIsNotAccessible {
                    referring_site: self.referring_site(),
                    referred: resolved,
                    referred_span: identifier.span,
                },
            ));
        }

        Ok(resolved)
    }

    async fn resolve_qualified_identifier_internal(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        bound_type: Option<&Type>,
    ) -> Result<Resolution, Error> {
        // create the current root
        let mut latest_resolution = self
            .resolve_root(
                &qualified_identifier.root().ok_or(Error::Abort)?,
                bound_type,
            )
            .await?;

        for generic_identifier in qualified_identifier
            .subsequences()
            .filter_map(|x| x.generic_identifier())
        {
            let Some(identifier) = generic_identifier.identifier() else {
                continue;
            };

            let resolved_id =
                self.resolve_step(&latest_resolution, &identifier).await?;

            let symbol_kind = self.tracked_engine().get_kind(resolved_id).await;

            let generic_arguments = if symbol_kind.has_generic_parameters() {
                Some(
                    self.resolve_generic_arguments_for_internal(
                        resolved_id,
                        (symbol_kind == Kind::Marker
                            || symbol_kind == Kind::Trait)
                            .then_some(bound_type)
                            .flatten(),
                        &generic_identifier,
                    )
                    .await,
                )
            } else {
                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
                {
                    self.receive_diagnostic(
                        Diagnostic::NoGenericArgumentsRequired(
                            NoGenericArgumentsRequired {
                                global_id: resolved_id,
                                generic_argument_span: gen_args.span(),
                            },
                        ),
                    );
                }

                None
            };

            let next_resolution = to_resolution(
                resolved_id,
                symbol_kind,
                generic_arguments,
                latest_resolution,
            );

            self.notify_resolution_resolved(
                &next_resolution,
                &generic_identifier.span(),
            );

            latest_resolution = next_resolution;
        }

        Ok(latest_resolution)
    }

    /// Resolves a [`QualifiedIdentifier`] to a [`Resolution`].
    #[allow(clippy::too_many_lines)]
    pub async fn resolve_qualified_identifier(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
    ) -> Result<Resolution, Error> {
        self.resolve_qualified_identifier_internal(qualified_identifier, None)
            .await
    }
}

/// Resolves a [`SimplePath`] as a [`GlobalID`].
#[extend]
pub async fn resolve_simple_path(
    self: &TrackedEngine,
    simple_path: &SimplePath,
    referring_site: Global<pernixc_symbol::SymbolID>,
    start_from_root: bool,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<pernixc_symbol::SymbolID>> {
    // simple path should always have root tough
    let root: Global<pernixc_symbol::SymbolID> = match simple_path.root()? {
        SimplePathRoot::Target(_) => Global::new(
            referring_site.target_id,
            self.get_target_root_module_id(referring_site.target_id).await,
        ),

        SimplePathRoot::Identifier(ident) => {
            if start_from_root {
                let target_map = self.get_target_map().await;
                let target =
                    self.get_linked_targets(referring_site.target_id).await;

                let Some(target_id) =
                    target_map.get(&*ident.kind.0).copied().filter(|x| {
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
                    .get(&*ident.kind.0)
                    .map(|x| Global::new(referring_site.target_id, *x))
                {
                    Some(id) => Some(id),
                    None => self
                        .get_import_map(global_closest_module_id)
                        .await
                        .get(&*ident.kind.0)
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
    referring_site: Global<pernixc_symbol::SymbolID>,
    root: Global<pernixc_symbol::SymbolID>,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<pernixc_symbol::SymbolID>> {
    let mut lastest_resolution = root;

    for identifier in simple_path {
        let Some(new_id) =
            self.get_member_of(lastest_resolution, &identifier.kind.0).await
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
    id: Global<pernixc_symbol::SymbolID>,
    member_name: &str,
) -> Option<Global<pernixc_symbol::SymbolID>> {
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

/// Resolves a member of given name in the given symbol ID.
///
/// This analogous to doing `Symbol::member` resolution. where `Symbol` is
/// the `symbol_id` and `member` is the `identifier`.
#[extend]
pub async fn resolve_in(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::SymbolID>,
    site_target: TargetID,
    identifier: &str,
    consider_adt_implements: bool,
) -> Option<Global<pernixc_symbol::SymbolID>> {
    if let Some(resolved_id) = self.get_member_of(symbol_id, identifier).await {
        return Some(resolved_id);
    }

    // try to search in the implements if the latest resolution is an ADT
    let kind = self.get_kind(symbol_id).await;

    if !(kind.is_adt() && consider_adt_implements) {
        return None;
    }

    let implemented = self.get_implemented(symbol_id, site_target).await;

    for impl_id in implemented.iter().copied() {
        let impl_members = self.get_members(impl_id).await;

        if let Some(resolved_id) =
            impl_members.member_ids_by_name.get(identifier)
        {
            return Some(Global::new(impl_id.target_id, *resolved_id));
        }
    }

    None
}
