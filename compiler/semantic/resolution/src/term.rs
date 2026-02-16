//! Contains the logic for solving terms (types, lifetimes, and constants) in
//! the resolution process.

use std::{fmt::Debug, ops::Deref};

use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::type_alias::get_type_alias;
use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_syntax::{GenericIdentifier, LifetimeIdentifier};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{GenericArguments, MemberSymbol, Symbol, TraitMember},
    generic_parameters::{GenericKind, get_generic_parameters},
    instantiation::Instantiation,
    lifetime::Lifetime,
    tuple,
    r#type::{Array, Phantom, Pointer, Primitive, Qualifier, Reference, Type},
};

use crate::{
    Config, ElidedTermProvider, Error,
    diagnostic::{
        Diagnostic, ExpectType, LifetimeParameterNotFound,
        MismatchedGenericArgumentCount, MisorderedGenericArgument,
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

#[extend]
pub(crate) async fn resolve_generic_arguments_for_internal(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    bound_type: Option<&Type>,
    generic_identifier: &GenericIdentifier,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> GenericArguments {
    let mut generic_arguments = if let Some(generic_arguments) =
        generic_identifier.generic_arguments()
    {
        self.resolve_generic_arguments(
            &generic_arguments,
            config.reborrow(),
            handler,
        )
        .await
    } else {
        GenericArguments::default()
    };

    // add the bound type as the first type argument
    if let Some(bound_type) = bound_type {
        generic_arguments.types.insert(0, bound_type.clone());
    }

    let (generic_arguments, diagnostics) = self
        .verify_generic_arguments_for(
            generic_arguments,
            symbol_id,
            generic_identifier.span(),
            config,
        )
        .await;

    for diagnostic in diagnostics {
        handler.receive(diagnostic);
    }

    generic_arguments
}

/// Resolves the [`pernixc_syntax::GenericArguments`] as a [`GenericArguments`]
/// term.
#[extend]
pub async fn resolve_generic_arguments(
    self: &TrackedEngine,
    generic_arguments: &pernixc_syntax::GenericArguments,
    mut config: Config<'_, '_, '_, '_, '_>,
    handler: &dyn Handler<Diagnostic>,
) -> GenericArguments {
    let mut lifetime_argument_syns = Vec::new();
    let mut type_argument_syns = Vec::new();
    let mut constant_argument_syns = Vec::new();

    // extracts the generic arguments from the syntax tree to the list of
    // syntax trees
    for generic_argument in generic_arguments.arguments() {
        let misordered = match generic_argument.clone() {
            pernixc_syntax::GenericArgument::Constant(arg) => {
                constant_argument_syns.push(arg);

                false
            }
            pernixc_syntax::GenericArgument::Type(arg) => {
                type_argument_syns.push(arg);

                !constant_argument_syns.is_empty()
            }
            pernixc_syntax::GenericArgument::Lifetime(arg) => {
                lifetime_argument_syns.push(arg);

                !constant_argument_syns.is_empty()
                    || !type_argument_syns.is_empty()
            }
        };

        if misordered {
            handler.receive(Diagnostic::MisorderedGenericArgument(
                MisorderedGenericArgument {
                    generic_kind: match generic_argument {
                        pernixc_syntax::GenericArgument::Type(_) => {
                            GenericKind::Type
                        }
                        pernixc_syntax::GenericArgument::Constant(_) => {
                            GenericKind::Constant
                        }
                        pernixc_syntax::GenericArgument::Lifetime(_) => {
                            GenericKind::Lifetime
                        }
                    },
                    generic_argument: generic_argument.span(),
                },
            ));
        }
    }

    let mut lifetimes = Vec::with_capacity(lifetime_argument_syns.len());
    let mut types = Vec::with_capacity(type_argument_syns.len());
    let constants = Vec::with_capacity(constant_argument_syns.len());

    for lt in lifetime_argument_syns {
        lifetimes
            .push(self.resolve_lifetime(&lt, config.reborrow(), handler).await);
    }

    for ty in type_argument_syns {
        types.push(self.resolve_type(&ty, config.reborrow(), handler).await);
    }

    for _con in constant_argument_syns {
        todo!("implements const eval")
    }

    GenericArguments { lifetimes, types, constants }
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
            generic_arguments
                .constants
                .is_empty()
                .then(|| generic_parameters.default_type_parameters()),
            generic_parameters.default_constant_parameters(),
        )
    };

    let mut diagnostics = Vec::new();
    let generic_args = GenericArguments {
        lifetimes: resolve_generic_arguments_kinds(
            generic_arguments.lifetimes.into_iter(),
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
        types: resolve_generic_arguments_kinds(
            generic_arguments.types.into_iter(),
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
        constants: resolve_generic_arguments_kinds(
            generic_arguments.constants.into_iter(),
            constant_parameter_ords.iter(),
            Some(default_constant_arguments.iter()),
            generic_identifier_span,
            config,
            GenericKind::Constant,
            |x| match &mut x.elided_constant_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            &mut diagnostics,
        ),
    };

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
pub enum ResolutionToTypeError {
    Failed(Resolution),
}

/// Interprets the [`Resolution`] as a [`Type`].
#[extend]
pub async fn resolution_to_type(
    self: &TrackedEngine,
    resolution: Resolution,
) -> Result<Type, ResolutionToTypeError> {
    match resolution {
        Resolution::Generic(symbol) => {
            let symbol_kind = self.get_kind(symbol.id).await;

            match symbol_kind {
                Kind::Struct | Kind::Enum => Ok(Type::Symbol(Symbol {
                    id: symbol.id,
                    generic_arguments: symbol.generic_arguments,
                })),

                Kind::ImplementationType | Kind::Type => {
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

                _ => Err(ResolutionToTypeError::Failed(Resolution::Generic(
                    symbol,
                ))),
            }
        }

        Resolution::MemberGeneric(symbol) => {
            let symbol_kind = self.get_kind(symbol.id).await;

            match symbol_kind {
                Kind::TraitType => {
                    Ok(Type::TraitMember(TraitMember(MemberSymbol {
                        id: symbol.id,
                        member_generic_arguments: symbol
                            .member_generic_arguments,
                        parent_generic_arguments: symbol
                            .parent_generic_arguments,
                    })))
                }

                _ => Err(ResolutionToTypeError::Failed(
                    Resolution::MemberGeneric(symbol),
                )),
            }
        }

        resolution => Err(ResolutionToTypeError::Failed(resolution)),
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
    let rest_count = syntax_tree.subsequences().count();
    let is_simple_identifier = rest_count == 0
        && syntax_tree
            .root()
            .and_then(|x| x.into_generic_identifier().ok())
            .is_some_and(|x| {
                x.identifier().is_some() && x.generic_arguments().is_none()
            });

    // try to resolve the simple identifier in the extra namespace
    if is_simple_identifier
        && let Some(extra_type) = config.extra_namespace.and_then(|x| {
            x.types
                .get(
                    &*syntax_tree
                        .root()
                        .unwrap()
                        .as_generic_identifier()
                        .unwrap()
                        .identifier()
                        .unwrap()
                        .kind
                        .0,
                )
                .cloned()
        })
    {
        return extra_type;
    }

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
        Err(ResolutionToTypeError::Failed(resolution)) => {
            handler.receive(Diagnostic::ExpectType(ExpectType {
                non_type_symbol_span: syntax_tree.span(),
                resolved_global_id: resolution.global_id(),
            }));

            Type::Error(pernixc_term::error::Error)
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
                            elements.extend(tuple.elements.into_iter());
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
            if elements.iter().filter(|x| x.is_unpacked).count() > 1 {
                handler.receive(Diagnostic::MoreThanOneUnpackedInTupleType(
                    MoreThanOneUnpackedInTupleType {
                        illegal_tuple_type_span: syntax_tree.span(),
                    },
                ));

                Type::Error(pernixc_term::error::Error)
            } else {
                Type::Tuple(tuple::Tuple { elements })
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
