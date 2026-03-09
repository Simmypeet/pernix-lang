//! Contains the logic for solving terms (types, lifetimes, and constants) in
//! the resolution process.

use std::{fmt::Debug, ops::Deref};

use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    instance_associated_value::get_instance_associated_value,
    type_alias::get_type_alias,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_syntax::{
    GenericArgument as GenericArgumentSyn, GenericIdentifier,
    LifetimeIdentifier,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    effect,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        ConstantParameter, GenericKind, InstanceParameter, InstanceParameterID,
        TypeParameter, TypeParameterID, get_generic_parameters,
    },
    instance::{Instance, TraitRef},
    instantiation::{Instantiation, get_instantiation_for_associated_symbol},
    lifetime::Lifetime,
    tuple,
    r#type::{Array, Phantom, Pointer, Primitive, Qualifier, Reference, Type},
};

use crate::{
    Config, ElidedTermProvider, Error,
    diagnostic::{
        Diagnostic, ExpectEffect, ExpectInstance, ExpectTrait, ExpectType,
        LifetimeParameterNotFound, MismatchedGenericArgumentCount,
        MismatchedKindInArgument, MisorderedGenericArgument,
        MoreThanOneUnpackedInTupleType, UnexpectedInference,
    },
    qualified_identifier::{Resolution, resolve_qualified_identifier},
};

/// Resolves the generic arguments resides within the given
/// [`generic_identifier`] for the given symbol.
///
/// This function ensure that the generic arguments returned has a matching
/// generic arguments count with the generic parameters of the given symbol.
#[extend]
pub async fn resolve_generic_arguments_for(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    generic_identifier: &GenericIdentifier,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> GenericArguments {
    self.resolve_generic_arguments_for_internal(
        symbol_id,
        None,
        generic_identifier,
        config.reborrow(),
        handler,
    )
    .await
}

macro_rules! resolve_generic_arguments_kind {
    (
        $generic_parameters:ident,
        $param:ident,
        $generic_arguments_syn:ident,
        $terms:ident,
        $handler:ident,
        $kind:expr,
        $span:expr,
        $error_term:expr,
        $ellided_provider:expr,
        $done:expr,
        |$syn:ident, $param_id:ident| $resolve:expr,
    ) => {{
        let mut parameter_order =
            $generic_parameters.parameter_order::<$param>();

        if let Some(generic_arguments_syn) = $generic_arguments_syn.as_mut() {
            while let Some($param_id) = parameter_order.next()
                && let Some($syn) = (!generic_arguments_syn.is_empty())
                    .then(|| generic_arguments_syn.remove(0))
            {
                let term = $terms.push($resolve);
            }

            if $terms.len() != $generic_parameters.parameter_len::<$param>() {
                $handler.receive(Diagnostic::MismatchedGenericArgumentCount(
                    MismatchedGenericArgumentCount {
                        generic_kind: $kind,
                        generic_identifier_span: $span,
                        expected_count: $generic_parameters
                            .parameter_len::<$param>(),
                        supplied_count: $terms.len(),
                    },
                ));
            }

            $terms.resize(
                $generic_parameters.parameter_len::<$param>(),
                $error_term,
            );

            $terms
        } else {
            if $generic_parameters.parameter_len::<$param>() > 0 {
                if let Some(provider) = $ellided_provider {
                    $terms.resize_with(
                        $generic_parameters.parameter_len::<$param>(),
                        || provider.create(),
                    );

                    $done
                }
            }

            if $generic_parameters.parameter_len::<$param>() != $terms.len() {
                $handler.receive(Diagnostic::MismatchedGenericArgumentCount(
                    MismatchedGenericArgumentCount {
                        generic_kind: $kind,
                        generic_identifier_span: $span,
                        expected_count: $generic_parameters
                            .parameter_len::<$param>(),
                        supplied_count: $terms.len(),
                    },
                ));
            }

            $terms.resize(
                $generic_parameters.parameter_len::<$param>(),
                $error_term,
            );

            $terms
        }
    }};
}

#[extend]
async fn resolve_type_argument(
    self: &TrackedEngine,
    syn: &pernixc_syntax::GenericArgument,
    symbol_id: Global<pernixc_symbol::ID>,
    param_id: pernixc_arena::ID<TypeParameter>,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Type {
    match syn {
        GenericArgumentSyn::Lifetime(lifetime) => {
            handler.receive(Diagnostic::MismatchedKindInArgument(
                MismatchedKindInArgument::builder()
                    .argument_span(lifetime.span())
                    .found_kind(GenericKind::Lifetime)
                    .found_parameter(
                        TypeParameterID::new(symbol_id, param_id).into(),
                    )
                    .build(),
            ));

            Type::Error(pernixc_term::error::Error)
        }

        GenericArgumentSyn::QualifiedIdentifierWithHigherRankedLifetimes(
            qualified_identifier,
        ) => {
            let Some(q) = qualified_identifier.qualified_identifier() else {
                return Type::Error(pernixc_term::error::Error);
            };

            Box::pin(async move {
                self.resolve_qualified_identifier_type(&q, config, handler)
                    .await
            })
            .await
        }

        GenericArgumentSyn::Type(ty) => {
            Box::pin(
                async move { self.resolve_type(ty, config, handler).await },
            )
            .await
        }

        GenericArgumentSyn::Constant(constant_argument) => {
            handler.receive(Diagnostic::MismatchedKindInArgument(
                MismatchedKindInArgument::builder()
                    .argument_span(constant_argument.span())
                    .found_kind(GenericKind::Constant)
                    .found_parameter(
                        TypeParameterID::new(symbol_id, param_id).into(),
                    )
                    .build(),
            ));

            Type::Error(pernixc_term::error::Error)
        }
    }
}

#[extend]
async fn resolv_instance_argument(
    self: &TrackedEngine,
    syn: &pernixc_syntax::GenericArgument,
    symbol_id: Global<pernixc_symbol::ID>,
    param_id: pernixc_arena::ID<InstanceParameter>,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Instance {
    match syn {
        GenericArgumentSyn::QualifiedIdentifierWithHigherRankedLifetimes(
            qualified_identifier,
        ) => {
            let Some(q) = qualified_identifier.qualified_identifier() else {
                return Instance::Error(pernixc_term::error::Error);
            };

            Box::pin(async move {
                self.resolve_qualified_identifier_instance(&q, config, handler)
                    .await
            })
            .await
        }

        syn => {
            handler.receive(Diagnostic::MismatchedKindInArgument(
                MismatchedKindInArgument::builder()
                    .argument_span(syn.span())
                    .found_parameter(
                        InstanceParameterID::new(symbol_id, param_id).into(),
                    )
                    .found_kind(match syn {
                        GenericArgumentSyn::Lifetime(_) => {
                            GenericKind::Lifetime
                        }
                        GenericArgumentSyn::Type(_) => GenericKind::Type,
                        GenericArgumentSyn::Constant(_) => {
                            GenericKind::Constant
                        }

                        GenericArgumentSyn::QualifiedIdentifierWithHigherRankedLifetimes(
                            qualified_identifier,
                        ) => unreachable!(),
                    })
                    .build(),
            ));

            Instance::Error(pernixc_term::error::Error)
        }
    }
}

#[extend]
#[allow(
    clippy::too_many_lines,
    clippy::cognitive_complexity,
    clippy::diverging_sub_expression,
    unreachable_code
)]
pub(crate) async fn resolve_generic_arguments_for_internal(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    bound_type: Option<&Type>,
    generic_identifier: &GenericIdentifier,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> GenericArguments {
    let mut generic_arguments_syn = generic_identifier
        .generic_arguments()
        .map(|x| x.arguments().collect::<Vec<_>>());

    let generic_parameters = self.get_generic_parameters(symbol_id).await;

    let lifetimes = 'lifetime: {
        let lifetime_syn = {
            generic_arguments_syn.as_mut().map_or_else(
                Vec::new,
                |generic_arguments_syn| {
                    let last_lifetime_pos = generic_arguments_syn
                        .iter()
                        .position(|x| !x.is_lifetime())
                        .unwrap_or(generic_arguments_syn.len());

                    let lifetime_syn = generic_arguments_syn
                        .drain(..last_lifetime_pos)
                        .collect();

                    // scan for lifetimes appearing after non-lifetime generic
                    // arguments
                    for syn in generic_arguments_syn
                        .extract_if(.., |x| x.is_lifetime())
                        .map(|x| x.into_lifetime().unwrap())
                    {
                        handler.receive(Diagnostic::MisorderedGenericArgument(
                            MisorderedGenericArgument {
                                generic_kind: GenericKind::Lifetime,
                                generic_argument: syn.span(),
                            },
                        ));
                    }

                    lifetime_syn
                },
            )
        };

        // if there is no lifetime found but they are required, use the elided
        // lifetime provider (if any) to fill the lifetime arguments
        if lifetime_syn.is_empty()
            && generic_parameters.lifetime_parameters_len() > 0
            && let Some(provider) = config.elided_lifetime_provider.as_mut()
        {
            break 'lifetime vec![
                provider.create();
                generic_parameters.lifetime_parameters_len()
            ];
        }

        let mut lifetimes = Vec::new();
        for lifetime_syn in lifetime_syn {
            lifetimes.push(
                self.resolve_lifetime(
                    &lifetime_syn.into_lifetime().unwrap(),
                    config.reborrow(),
                    handler,
                )
                .await,
            );
        }

        if lifetimes.len()
            != generic_parameters.lifetime_parameter_order().len()
        {
            handler.receive(Diagnostic::MismatchedGenericArgumentCount(
                MismatchedGenericArgumentCount {
                    generic_kind: GenericKind::Lifetime,
                    generic_identifier_span: generic_identifier.span(),
                    expected_count: generic_parameters
                        .lifetime_parameter_order()
                        .len(),
                    supplied_count: lifetimes.len(),
                },
            ));
        }

        lifetimes.resize(
            generic_parameters.lifetime_parameters_len(),
            Lifetime::Error(pernixc_term::error::Error),
        );

        lifetimes
    };

    let types = 'ty: {
        let mut types = Vec::new();

        if let Some(bound_type) = bound_type {
            types.push(bound_type.clone());
        }

        resolve_generic_arguments_kind! {
            generic_parameters,
            TypeParameter,
            generic_arguments_syn,
            types,
            handler,
            GenericKind::Type,
            generic_identifier.span(),
            Type::Error(pernixc_term::error::Error),
            config.elided_type_provider.as_mut(),
            break 'ty types,
            |syn, param_id| self.resolve_type_argument(
                &syn,
                symbol_id,
                param_id,
                config.reborrow(),
                handler,
            ).await,
        }
    };

    let constants = 'consts: {
        let mut constants = Vec::new();

        resolve_generic_arguments_kind! {
            generic_parameters,
            ConstantParameter,
            generic_arguments_syn,
            constants,
            handler,
            GenericKind::Constant,
            generic_identifier.span(),
            Constant::Error(pernixc_term::error::Error),
            config.elided_constant_provider.as_mut(),
            break 'consts constants,
            |syn, param_id| {
                todo!("constant evaluation is not implemented yet")
            },
        }
    };

    let instances = 'instances: {
        let mut instances = Vec::new();

        resolve_generic_arguments_kind! {
            generic_parameters,
            InstanceParameter,
            generic_arguments_syn,
            instances,
            handler,
            GenericKind::Instance,
            generic_identifier.span(),
            Instance::Error(pernixc_term::error::Error),
            config.elided_instance_provider.as_mut(),
            break 'instances instances,
            |syn, param_id| {
                self.resolv_instance_argument(
                    &syn,
                    symbol_id,
                    param_id,
                    config.reborrow(),
                    handler,
                ).await
            },
        }
    };

    GenericArguments::new(lifetimes, types, constants, instances)
}

#[allow(clippy::too_many_arguments)]
fn resolve_generic_arguments_kinds<
    'a,
    T: From<pernixc_term::error::Error> + Clone + Debug + 'a,
    P: 'a + Debug,
>(
    generic_arguments: impl ExactSizeIterator<Item = T>,
    parameters: impl ExactSizeIterator<Item = &'a P>,
    defaults: Option<impl ExactSizeIterator<Item = &'a T>>,
    generic_identifier_span: RelativeSpan,
    mut config: Config<'_, '_, '_, '_, '_>,
    generic_kind: GenericKind,
    get_provider: impl for<'x> Fn(
        &'x mut Config<'_, '_, '_, '_, '_>,
    ) -> Option<&'x mut dyn ElidedTermProvider<T>>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<T> {
    if generic_arguments.len() == 0 {
        if parameters.len() == 0 {
            return Vec::new();
        }

        let Some(provider) = get_provider(&mut config) else {
            diagnostics.push(Diagnostic::MismatchedGenericArgumentCount(
                MismatchedGenericArgumentCount {
                    generic_kind,
                    generic_identifier_span,
                    expected_count: parameters.len(),
                    supplied_count: generic_arguments.len(),
                },
            ));

            // return the error terms
            return parameters
                .map(|_| pernixc_term::error::Error.into())
                .collect();
        };

        parameters.map(|_| provider.create()).collect()
    } else {
        let valid_count = defaults.as_ref().map_or_else(
            || parameters.len() == generic_arguments.len(),
            |defaults| {
                let expected_range =
                    (parameters.len() - defaults.len())..=parameters.len();

                expected_range.contains(&generic_arguments.len())
            },
        );

        // check if the number of supplied generic arugmnets is valid
        if !valid_count {
            diagnostics.push(Diagnostic::MismatchedGenericArgumentCount(
                MismatchedGenericArgumentCount {
                    generic_identifier_span,
                    generic_kind,
                    expected_count: parameters.len(),
                    supplied_count: generic_arguments.len(),
                },
            ));
        }

        let mut arguments =
            generic_arguments.take(parameters.len()).collect::<Vec<_>>();

        if valid_count {
            if let Some(defaults) = defaults {
                let leftovers = parameters.len() - arguments.len();
                let default_fill_count = leftovers.min(defaults.len());
                let default_len = defaults.len();

                arguments.extend(
                    defaults.skip(default_len - default_fill_count).cloned(),
                );
            }
        } else {
            // extend the arguments with error term
            let extra_term_count = parameters.len() - arguments.len();

            arguments.extend(std::iter::repeat_n(
                pernixc_term::error::Error.into(),
                extra_term_count,
            ));
        }

        assert!(
            arguments.len() == parameters.len(),
            "{:#?} {:#?} {:#?}",
            arguments,
            parameters.into_iter().collect::<Vec<_>>(),
            valid_count
        );

        arguments
    }
}

/// Verifies whether the given [`generic_arguments`] are a valid generic
/// arguments for the generic parameters of the given [`global_id`] symbol. The
/// function will attempt to fill the missing generic arguments with default
/// value (if any) or error term.
#[allow(clippy::option_if_let_else, clippy::type_complexity)]
#[extend]
pub async fn verify_generic_arguments_for(
    self: &TrackedEngine,
    generic_arguments: GenericArguments,
    generic_id: Global<pernixc_symbol::ID>,
    generic_identifier_span: RelativeSpan,
    mut config: Config<'_, '_, '_, '_, '_>,
) -> (GenericArguments, Vec<Diagnostic>) {
    let generic_parameters = self.get_generic_parameters(generic_id).await;

    let (
        lifetime_parameter_orders,
        type_parameter_orders,
        constant_parameter_ords,
        instance_parameter_ords,
        default_type_arguments,
        default_constant_arguments,
    ) = {
        (
            generic_parameters
                .lifetime_parameters_as_order()
                .map(|(_, x)| x)
                .collect::<Vec<_>>(),
            generic_parameters
                .type_parameters_as_order()
                .map(|(_, x)| x)
                .collect::<Vec<_>>(),
            generic_parameters
                .constant_parameters_as_order()
                .map(|(_, x)| x)
                .collect::<Vec<_>>(),
            generic_parameters
                .instance_parameters_as_order()
                .map(|(_, x)| x)
                .collect::<Vec<_>>(),
            generic_arguments
                .constants()
                .is_empty()
                .then(|| generic_parameters.default_type_parameters()),
            generic_parameters.default_constant_parameters(),
        )
    };

    let mut diagnostics = Vec::new();

    let (lifetime_args, type_args, constant_args, instance_args) =
        generic_arguments.into_arguments();

    let generic_args = GenericArguments::new(
        resolve_generic_arguments_kinds(
            lifetime_args.into_iter(),
            lifetime_parameter_orders.iter(),
            Option::<std::iter::Empty<_>>::None,
            generic_identifier_span,
            config.reborrow(),
            GenericKind::Lifetime,
            |x| match &mut x.elided_lifetime_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            &mut diagnostics,
        ),
        resolve_generic_arguments_kinds(
            type_args.into_iter(),
            type_parameter_orders.iter(),
            default_type_arguments.as_ref().map(|x| x.iter()),
            generic_identifier_span,
            config.reborrow(),
            GenericKind::Type,
            |x| match &mut x.elided_type_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            &mut diagnostics,
        ),
        resolve_generic_arguments_kinds(
            constant_args.into_iter(),
            constant_parameter_ords.iter(),
            Some(default_constant_arguments.iter()),
            generic_identifier_span,
            config.reborrow(),
            GenericKind::Constant,
            |x| match &mut x.elided_constant_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            &mut diagnostics,
        ),
        resolve_generic_arguments_kinds(
            instance_args.into_iter(),
            instance_parameter_ords.iter(),
            Option::<std::iter::Empty<_>>::None,
            generic_identifier_span,
            config,
            GenericKind::Instance,
            |x| match &mut x.elided_instance_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            &mut diagnostics,
        ),
    );

    (generic_args, diagnostics)
}

/// Resolves a [`pernixc_syntax::Lifetime`] as a [`Lifetime`] term.
#[extend]
pub async fn resolve_lifetime(
    self: &TrackedEngine,
    lifetime_argument: &pernixc_syntax::Lifetime,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Lifetime {
    let lifetime = match lifetime_argument.identifier() {
        Some(LifetimeIdentifier::Static(..)) => Lifetime::Static,
        Some(LifetimeIdentifier::Identifier(ident)) => {
            resolve_lifetime_parameter(&ident, &config, handler)
        }
        Some(LifetimeIdentifier::Elided(elided)) => {
            config.elided_lifetime_provider.map_or_else(
                || {
                    handler.receive(Diagnostic::UnexpectedInference(
                        UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Lifetime,
                        },
                    ));
                    Lifetime::Error(pernixc_term::error::Error)
                },
                ElidedTermProvider::create,
            )
        }

        None => Lifetime::Error(pernixc_term::error::Error),
    };

    if let Some(observer) = config.observer {
        observer.on_lifetime_resolved(
            self,
            config.referring_site,
            &lifetime,
            lifetime_argument,
            handler,
        );
    }

    lifetime
}

/// Resolves a [`Lifetime`] from an identifier.
pub fn resolve_lifetime_parameter(
    identifier: &pernixc_syntax::Identifier,
    config: &Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Lifetime {
    // reach to the extra namespace first
    if let Some(extra_lifeime) = config
        .extra_namespace
        .and_then(|x| x.lifetimes.get(&*identifier.kind.0).cloned())
    {
        return extra_lifeime;
    }

    handler.receive(Diagnostic::LifetimeParameterNotFound(
        LifetimeParameterNotFound {
            referred_span: identifier.span(),
            referring_site: config.referring_site,
            name: identifier.kind.0.clone(),
        },
    ));
    Lifetime::Error(pernixc_term::error::Error)
}

/// Enumeration of trying to interpreting [`Resolution`] as a type.
#[derive(Debug, derive_more::From)]
#[allow(missing_docs)]
pub enum ResolutionToTermError {
    Failed(Resolution),
}

/// Interprets the [`Resolution`] as a [`Type`].
#[extend]
pub async fn resolution_to_instance(
    self: &TrackedEngine,
    resolution: Resolution,
) -> Result<Instance, ResolutionToTermError> {
    match resolution {
        Resolution::Generic(generic)
            if {
                let symbol_kind = self.get_kind(generic.id).await;

                matches!(symbol_kind, Kind::Instance)
            } =>
        {
            Ok(Instance::Symbol(Symbol::new(
                generic.id,
                generic.generic_arguments,
            )))
        }

        Resolution::MemberGeneric(member_generic)
            if {
                let symbol_kind = self.get_kind(member_generic.id).await;

                matches!(symbol_kind, Kind::InstanceAssociatedInstance)
            } =>
        {
            let inst = self
                .get_instantiation_for_associated_symbol(
                    member_generic.id,
                    member_generic.parent_generic_arguments,
                    member_generic.member_generic_arguments,
                )
                .await
                .unwrap();

            let mut instance_value = self
                .get_instance_associated_value(member_generic.id)
                .await
                .deref()
                .clone();

            inst.instantiate(&mut instance_value);

            Ok(instance_value)
        }

        Resolution::Instance(instance) => Ok(instance),

        resolution => Err(ResolutionToTermError::Failed(resolution)),
    }
}

/// Interprets the [`Resolution`] as a [`Type`].
#[extend]
pub async fn resolution_to_type(
    self: &TrackedEngine,
    resolution: Resolution,
) -> Result<Type, ResolutionToTermError> {
    match resolution {
        Resolution::Generic(symbol) => {
            let symbol_kind = self.get_kind(symbol.id).await;

            match symbol_kind {
                Kind::Struct | Kind::Enum => Ok(Type::Symbol(Symbol::new(
                    symbol.id,
                    symbol.generic_arguments,
                ))),

                Kind::ImplementationAssociatedType | Kind::Type => {
                    let generic_parameters =
                        self.get_generic_parameters(symbol.id).await;

                    let instantiation = Instantiation::from_generic_arguments(
                        symbol.generic_arguments,
                        symbol.id,
                        &generic_parameters,
                    )
                    .unwrap();

                    let mut result_ty =
                        self.get_type_alias(symbol.id).await.deref().clone();

                    instantiation.instantiate(&mut result_ty);

                    Ok(result_ty)
                }

                _ => Err(ResolutionToTermError::Failed(Resolution::Generic(
                    symbol,
                ))),
            }
        }

        Resolution::MemberGeneric(member_generic) => {
            let symbol_kind = self.get_kind(member_generic.id).await;

            match symbol_kind {
                Kind::InstanceAssociatedType => {
                    let inst = self
                        .get_instantiation_for_associated_symbol(
                            member_generic.id,
                            member_generic.parent_generic_arguments,
                            member_generic.member_generic_arguments,
                        )
                        .await
                        .unwrap();

                    let mut type_alias = self
                        .get_type_alias(member_generic.id)
                        .await
                        .deref()
                        .clone();

                    inst.instantiate(&mut type_alias);

                    Ok(type_alias)
                }

                _ => Err(ResolutionToTermError::Failed(
                    Resolution::MemberGeneric(member_generic),
                )),
            }
        }

        Resolution::Type(ty) => Ok(ty),

        resolution => Err(ResolutionToTermError::Failed(resolution)),
    }
}

/// Interprets the [`Resolution`] as a [`TraitRef`].
#[extend]
pub async fn resolution_to_trait_ref(
    self: &TrackedEngine,
    resolution: Resolution,
) -> Result<TraitRef, ResolutionToTermError> {
    match resolution {
        Resolution::Generic(symbol) => {
            let symbol_kind = self.get_kind(symbol.id).await;

            match symbol_kind {
                Kind::Trait => {
                    Ok(TraitRef::new(symbol.id, symbol.generic_arguments))
                }

                _ => Err(ResolutionToTermError::Failed(Resolution::Generic(
                    symbol,
                ))),
            }
        }

        resolution => Err(ResolutionToTermError::Failed(resolution)),
    }
}

/// Interprets the [`Resolution`] as a [`effect::Unit`].
#[extend]
pub async fn resolution_to_effect_unit(
    self: &TrackedEngine,
    resolution: Resolution,
) -> Result<effect::Unit, ResolutionToTermError> {
    match resolution {
        Resolution::Generic(symbol) => {
            let symbol_kind = self.get_kind(symbol.id).await;

            match symbol_kind {
                Kind::Effect => {
                    Ok(effect::Unit::new(symbol.id, symbol.generic_arguments))
                }

                _ => Err(ResolutionToTermError::Failed(Resolution::Generic(
                    symbol,
                ))),
            }
        }

        resolution => Err(ResolutionToTermError::Failed(resolution)),
    }
}

/// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as a [`Type`] term.
#[extend]
pub async fn resolve_qualified_identifier_instance(
    self: &TrackedEngine,
    syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Instance {
    let resolution = match self
        .resolve_qualified_identifier(syntax_tree, config, handler)
        .await
    {
        Ok(resolution) => resolution,

        Err(Error::Abort) => {
            return Instance::Error(pernixc_term::error::Error);
        }
    };

    match self.resolution_to_instance(resolution).await {
        Ok(ty) => ty,
        Err(ResolutionToTermError::Failed(resolution)) => {
            handler.receive(Diagnostic::ExpectInstance(
                ExpectInstance::builder()
                    .non_instance_symbol_span(syntax_tree.span())
                    .resolved_resolution(resolution)
                    .build(),
            ));

            Instance::Error(pernixc_term::error::Error)
        }
    }
}

/// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as a [`Type`] term.
#[extend]
pub async fn resolve_qualified_identifier_type(
    self: &TrackedEngine,
    syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Type {
    let resolution = match self
        .resolve_qualified_identifier(syntax_tree, config, handler)
        .await
    {
        Ok(resolution) => resolution,

        Err(Error::Abort) => {
            return Type::Error(pernixc_term::error::Error);
        }
    };

    match self.resolution_to_type(resolution).await {
        Ok(ty) => ty,
        Err(ResolutionToTermError::Failed(resolution)) => {
            handler.receive(Diagnostic::ExpectType(
                ExpectType::builder()
                    .non_type_symbol_span(syntax_tree.span())
                    .resolved_resolution(resolution)
                    .build(),
            ));

            Type::Error(pernixc_term::error::Error)
        }
    }
}

/// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as a [`TraitRef`].
#[extend]
pub async fn resolve_qualified_identifier_trait_ref(
    self: &TrackedEngine,
    syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Option<TraitRef> {
    let resolution = match self
        .resolve_qualified_identifier(syntax_tree, config, handler)
        .await
    {
        Ok(resolution) => resolution,

        Err(Error::Abort) => {
            return None;
        }
    };

    match self.resolution_to_trait_ref(resolution).await {
        Ok(trait_ref) => Some(trait_ref),
        Err(ResolutionToTermError::Failed(resolution)) => {
            handler.receive(Diagnostic::ExpectTrait(
                ExpectTrait::builder()
                    .non_trait_symbol_span(syntax_tree.span())
                    .resolved_resolution(resolution)
                    .build(),
            ));

            None
        }
    }
}

/// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as a [`TraitRef`].
#[extend]
pub async fn resolve_qualified_identifier_effect_unit(
    self: &TrackedEngine,
    syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Option<effect::Unit> {
    let resolution = match self
        .resolve_qualified_identifier(syntax_tree, config, handler)
        .await
    {
        Ok(resolution) => resolution,

        Err(Error::Abort) => {
            return None;
        }
    };

    match self.resolution_to_effect_unit(resolution).await {
        Ok(effect_unit) => Some(effect_unit),
        Err(ResolutionToTermError::Failed(resolution)) => {
            handler.receive(Diagnostic::ExpectEffect(
                ExpectEffect::builder()
                    .non_effect_symbol_span(syntax_tree.span())
                    .resolved_resolution(resolution)
                    .build(),
            ));

            None
        }
    }
}

/// Resolves the type syntax tree to a [`Type`] term.
#[extend]
#[allow(clippy::too_many_lines, clippy::diverging_sub_expression)]
pub async fn resolve_type(
    self: &TrackedEngine,
    syntax_tree: &pernixc_syntax::r#type::Type,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> Type {
    let ty = match syntax_tree {
        pernixc_syntax::r#type::Type::Primitive(primitive) => {
            Type::Primitive(match primitive {
                pernixc_syntax::r#type::Primitive::Bool(_) => Primitive::Bool,
                pernixc_syntax::r#type::Primitive::Float32(_) => {
                    Primitive::Float32
                }
                pernixc_syntax::r#type::Primitive::Float64(_) => {
                    Primitive::Float64
                }
                pernixc_syntax::r#type::Primitive::Int8(_) => Primitive::Int8,
                pernixc_syntax::r#type::Primitive::Int16(_) => Primitive::Int16,
                pernixc_syntax::r#type::Primitive::Int32(_) => Primitive::Int32,
                pernixc_syntax::r#type::Primitive::Int64(_) => Primitive::Int64,
                pernixc_syntax::r#type::Primitive::Uint8(_) => Primitive::Uint8,
                pernixc_syntax::r#type::Primitive::Uint16(_) => {
                    Primitive::Uint16
                }
                pernixc_syntax::r#type::Primitive::Uint32(_) => {
                    Primitive::Uint32
                }
                pernixc_syntax::r#type::Primitive::Uint64(_) => {
                    Primitive::Uint64
                }
                pernixc_syntax::r#type::Primitive::Usize(_) => Primitive::Usize,
                pernixc_syntax::r#type::Primitive::Isize(_) => Primitive::Isize,
            })
        }

        pernixc_syntax::r#type::Type::QualifiedIdentifier(
            qualified_identifier,
        ) => {
            Box::pin(self.resolve_qualified_identifier_type(
                qualified_identifier,
                config.reborrow(),
                handler,
            ))
            .await
        }

        pernixc_syntax::r#type::Type::Reference(reference) => {
            let lifetime = if let Some(lifetime) = reference.lifetime().as_ref()
            {
                Some(
                    self.resolve_lifetime(lifetime, config.reborrow(), handler)
                        .await,
                )
            } else {
                None
            };

            let lifetime = if let Some(lifetime) = lifetime {
                lifetime
            } else if let Some(provider) =
                config.elided_lifetime_provider.as_mut()
            {
                provider.create()
            } else {
                handler.receive(Diagnostic::UnexpectedInference(
                    UnexpectedInference {
                        unexpected_span: reference
                            .ampersand()
                            .map_or_else(|| reference.span(), |x| x.span),
                        generic_kind: GenericKind::Lifetime,
                    },
                ));
                Lifetime::Error(pernixc_term::error::Error)
            };

            let qualifier = if reference.mut_keyword().is_some() {
                Qualifier::Mutable
            } else {
                Qualifier::Immutable
            };

            let pointee = Box::new(if let Some(pointee) = reference.r#type() {
                Box::pin(self.resolve_type(
                    &pointee,
                    config.reborrow(),
                    handler,
                ))
                .await
            } else {
                Type::Error(pernixc_term::error::Error)
            });

            Type::Reference(Reference { qualifier, lifetime, pointee })
        }
        pernixc_syntax::r#type::Type::Pointer(pointer_ty) => {
            let pointee =
                Box::new(if let Some(pointee) = pointer_ty.r#type() {
                    Box::pin(self.resolve_type(
                        &pointee,
                        config.reborrow(),
                        handler,
                    ))
                    .await
                } else {
                    Type::Error(pernixc_term::error::Error)
                });

            Type::Pointer(Pointer {
                mutable: pointer_ty.mut_keyword().is_some(),
                pointee,
            })
        }
        pernixc_syntax::r#type::Type::Tuple(syntax_tree) => {
            let mut elements = Vec::new();

            for element in syntax_tree.types() {
                let ty = if let Some(ty) = element.r#type() {
                    Box::pin(self.resolve_type(&ty, config.reborrow(), handler))
                        .await
                } else {
                    Type::Error(pernixc_term::error::Error)
                };

                if element.ellipsis().is_some() {
                    match ty {
                        Type::Tuple(tuple) => {
                            elements.extend(tuple.into_elements());
                        }
                        ty => {
                            if let Some(observer) = config.observer.as_mut() {
                                observer.on_unpacked_type_resolved(
                                    self,
                                    config.referring_site,
                                    &ty,
                                    &element,
                                    handler,
                                );
                            }

                            elements.push(tuple::Element::new(ty, true));
                        }
                    }
                } else {
                    elements.push(tuple::Element::new(ty, false));
                }
            }

            // check if there is more than one unpacked type
            if elements.iter().filter(|x| x.is_unpacked()).count() > 1 {
                handler.receive(Diagnostic::MoreThanOneUnpackedInTupleType(
                    MoreThanOneUnpackedInTupleType {
                        illegal_tuple_type_span: syntax_tree.span(),
                    },
                ));

                Type::Error(pernixc_term::error::Error)
            } else {
                Type::Tuple(tuple::Tuple::new(elements))
            }
        }

        #[allow(unreachable_code, unused_variables)]
        pernixc_syntax::r#type::Type::Array(array) => Type::Array(Array {
            length: todo!("implements a constant eval"),
            r#type: Box::new(if let Some(ty) = array.r#type() {
                self.resolve_type(&ty, config.reborrow(), handler).await
            } else {
                Type::Error(pernixc_term::error::Error)
            }),
        }),

        pernixc_syntax::r#type::Type::Phantom(phantom) => {
            let ty = if let Some(ty) = phantom.r#type() {
                Box::pin(self.resolve_type(&ty, config.reborrow(), handler))
                    .await
            } else {
                Type::Error(pernixc_term::error::Error)
            };

            Type::Phantom(Phantom(Box::new(ty)))
        }
        pernixc_syntax::r#type::Type::Elided(elided) => {
            config.elided_type_provider.as_mut().map_or_else(
                || {
                    handler.receive(Diagnostic::UnexpectedInference(
                        UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Type,
                        },
                    ));
                    Type::Error(pernixc_term::error::Error)
                },
                |provider| provider.create(),
            )
        }
    };

    if let Some(observer) = config.observer.as_mut() {
        observer.on_type_resolved(
            self,
            config.referring_site,
            &ty,
            syntax_tree,
            handler,
        );
    }

    ty
}
