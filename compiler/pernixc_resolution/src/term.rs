//! Contains the logic for solving terms (types, lifetimes, and constants) in
//! the resolution process.

use std::fmt::Debug;

use pernixc_component::type_alias::TypeAlias;
use pernixc_handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, ConnectedList, GenericIdentifier, LifetimeIdentifier,
};
use pernixc_table::{
    component::SymbolKind, diagnostic::Diagnostic, query, GlobalID, Table,
};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::{GenericKind, GenericParameters},
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{
        Array, Phantom, Pointer, Primitive, Qualifier, Reference, TraitMember,
        Type,
    },
    Default, MemberSymbol, Model, ModelOf, Symbol, Tuple, TupleElement,
};

use crate::{
    diagnostic::{
        ExpectType, LifetimeParameterNotFound, MismatchedGenericArgumentCount,
        MisorderedGenericArgument, MoreThanOneUnpackedInTupleType,
        UnexpectedInference,
    },
    qualified_identifier::{self, Resolution},
    Config, ElidedTermProvider, Ext,
};

/// An error returned when resolving a term.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Query(#[from] query::Error),

    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,
}

pub(super) fn resolve_generic_arguments_for<M: Model>(
    table: &Table,
    symbol_id: GlobalID,
    generic_identifier: &GenericIdentifier,
    referring_site: GlobalID,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<GenericArguments<M>, Error> {
    let generic_arguments = if let Some(generic_arguments) =
        generic_identifier.generic_arguments().as_ref()
    {
        table.resolve_generic_arguments(
            generic_arguments,
            referring_site,
            config.reborrow(),
            handler,
        )?
    } else {
        GenericArguments::default()
    };

    table.verify_generic_arguments_for(
        generic_arguments,
        symbol_id,
        generic_identifier.span(),
        config,
        handler,
    )
}

pub(super) fn resolve_generic_arguments<M: Model>(
    table: &Table,
    generic_arguments: &syntax_tree::GenericArguments,
    referring_site: GlobalID,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<GenericArguments<M>, Error> {
    let mut lifetime_argument_syns = Vec::new();
    let mut type_argument_syns = Vec::new();
    let mut constant_argument_syns = Vec::new();

    // extracts the generic arguments from the syntax tree to the list of
    // syntax trees
    for generic_argument in generic_arguments
        .connected_list()
        .iter()
        .flat_map(ConnectedList::elements)
    {
        let misordered = match generic_argument {
            syntax_tree::GenericArgument::Constant(arg) => {
                constant_argument_syns.push(arg.tree());

                false
            }
            syntax_tree::GenericArgument::Type(arg) => {
                type_argument_syns.push(&**arg);

                !constant_argument_syns.is_empty()
            }
            syntax_tree::GenericArgument::Lifetime(arg) => {
                lifetime_argument_syns.push(arg);

                !constant_argument_syns.is_empty()
                    || !type_argument_syns.is_empty()
            }
        };

        if misordered {
            handler.receive(Box::new(MisorderedGenericArgument {
                generic_kind: match generic_argument {
                    syntax_tree::GenericArgument::Type(_) => GenericKind::Type,
                    syntax_tree::GenericArgument::Constant(_) => {
                        GenericKind::Constant
                    }
                    syntax_tree::GenericArgument::Lifetime(_) => {
                        GenericKind::Lifetime
                    }
                },
                generic_argument: generic_argument.span(),
            }));
        }
    }

    Ok(GenericArguments {
        lifetimes: lifetime_argument_syns
            .into_iter()
            .map(|x| {
                table.resolve_lifetime(
                    x,
                    referring_site,
                    config.reborrow(),
                    handler,
                )
            })
            .collect(),
        types: type_argument_syns
            .into_iter()
            .map(|x| {
                table.resolve_type(
                    x,
                    referring_site,
                    config.reborrow(),
                    handler,
                )
            })
            .collect::<Result<_, _>>()?,
        constants: constant_argument_syns
            .into_iter()
            .map(|_| todo!())
            .collect(),
    })
}

#[allow(clippy::too_many_arguments)]
fn resolve_generic_arguments_kinds<
    'a,
    T: ModelOf + From<pernixc_term::Error> + Clone + Debug,
    P: 'a + Debug,
>(
    generic_arguments: impl ExactSizeIterator<Item = T>,
    parameters: impl ExactSizeIterator<Item = &'a P>,
    defaults: Option<impl ExactSizeIterator<Item = &'a T::Rebind<Default>>>,
    generic_identifier_span: Span,
    mut config: Config<T::Model>,
    generic_kind: GenericKind,
    get_provider: impl for<'x> Fn(
        &'x mut Config<T::Model>,
    ) -> Option<&'x mut dyn ElidedTermProvider<T>>,
    from_default_model: impl Fn(T::Rebind<Default>) -> T,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Vec<T>
where
    T::Rebind<Default>: 'a + Clone,
{
    if generic_arguments.len() == 0 {
        if parameters.len() == 0 {
            return Vec::new();
        }

        let Some(provider) = get_provider(&mut config) else {
            handler.receive(Box::new(MismatchedGenericArgumentCount {
                generic_kind,
                generic_identifier_span,
                expected_count: parameters.len(),
                supplied_count: generic_arguments.len(),
            }));

            // return the error terms
            return parameters.map(|_| pernixc_term::Error.into()).collect();
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
            handler.receive(Box::new(MismatchedGenericArgumentCount {
                generic_identifier_span,
                generic_kind,
                expected_count: parameters.len(),
                supplied_count: generic_arguments.len(),
            }));
        }

        let mut arguments =
            generic_arguments.take(parameters.len()).collect::<Vec<_>>();

        if valid_count {
            if let Some(defaults) = defaults {
                let leftovers = parameters.len() - arguments.len();
                let default_fill_count = leftovers.min(defaults.len());
                let default_len = defaults.len();

                arguments.extend(
                    defaults
                        .skip(default_len - default_fill_count)
                        .cloned()
                        .map(from_default_model),
                );
            }
        } else {
            // extend the arguments with error term
            let extra_term_count = parameters.len() - arguments.len();

            arguments.extend(
                std::iter::repeat(pernixc_term::Error.into())
                    .take(extra_term_count),
            );
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

#[allow(clippy::option_if_let_else)]
pub(super) fn verify_generic_arguments_for<M: Model>(
    table: &Table,
    generic_arguments: GenericArguments<M>,
    generic_id: GlobalID,
    generic_identifier_span: Span,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<GenericArguments<M>, Error> {
    let generic_parameters = table.query::<GenericParameters>(generic_id)?;

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

    Ok(GenericArguments {
        lifetimes: resolve_generic_arguments_kinds(
            generic_arguments.lifetimes.into_iter(),
            lifetime_parameter_orders.iter(),
            Option::<std::iter::Empty<_>>::None,
            generic_identifier_span.clone(),
            config.reborrow(),
            GenericKind::Lifetime,
            |x| match &mut x.elided_lifetime_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            M::from_default_lifetime,
            handler,
        ),
        types: resolve_generic_arguments_kinds(
            generic_arguments.types.into_iter(),
            type_parameter_orders.iter(),
            default_type_arguments.as_ref().map(|x| x.iter()),
            generic_identifier_span.clone(),
            config.reborrow(),
            GenericKind::Type,
            |x| match &mut x.elided_type_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            M::from_default_type,
            handler,
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
            M::from_default_constant,
            handler,
        ),
    })
}

pub(super) fn resolve_lifetime<M: Model>(
    table: &Table,
    lifetime_argument: &syntax_tree::Lifetime,
    referring_site: GlobalID,
    config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Lifetime<M> {
    let lifetime = match lifetime_argument.identifier() {
        LifetimeIdentifier::Static(..) => Lifetime::Static,
        LifetimeIdentifier::Identifier(ident) => {
            resolve_lifetime_parameter(ident, referring_site, &config, handler)
        }
        LifetimeIdentifier::Elided(elided) => {
            config.elided_lifetime_provider.map_or_else(
                || {
                    handler.receive(Box::new(UnexpectedInference {
                        unexpected_span: elided.span(),
                        generic_kind: GenericKind::Lifetime,
                    }));
                    Lifetime::Error(pernixc_term::Error)
                },
                ElidedTermProvider::create,
            )
        }
    };

    if let Some(observer) = config.observer {
        observer.on_lifetime_resolved(
            table,
            referring_site,
            &lifetime,
            lifetime_argument,
            handler,
        );
    }

    lifetime
}

pub(super) fn resolve_lifetime_parameter<M: Model>(
    identifier: &Identifier,
    referring_site: GlobalID,
    config: &Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Lifetime<M> {
    // reach to the extra namespace first
    if let Some(extra_lifeime) = config
        .extra_namespace
        .and_then(|x| x.lifetimes.get(identifier.span.str()).cloned())
    {
        return extra_lifeime;
    }

    handler.receive(Box::new(LifetimeParameterNotFound {
        referred_span: identifier.span(),
        referring_site,
    }));
    Lifetime::Error(pernixc_term::Error)
}

enum ResolutionToTypeError<M: Model> {
    InvalidKind(Resolution<M>),
    Query(pernixc_table::query::Error),
}

#[allow(clippy::result_large_err)]
fn resolution_to_type<M: Model>(
    table: &Table,
    resolution: Resolution<M>,
) -> Result<Type<M>, ResolutionToTypeError<M>> {
    match resolution {
        Resolution::Generic(symbol) => {
            let symbol_kind = *table.get::<SymbolKind>(symbol.id).unwrap();

            match symbol_kind {
                SymbolKind::Struct | SymbolKind::Enum => {
                    Ok(Type::Symbol(Symbol {
                        id: symbol.id,
                        generic_arguments: symbol.generic_arguments,
                    }))
                }

                SymbolKind::TraitImplementationType | SymbolKind::Type => {
                    let generic_parameters = table
                        .query::<GenericParameters>(symbol.id)
                        .map_err(ResolutionToTypeError::Query)?;

                    let instantiation = Instantiation::from_generic_arguments(
                        symbol.generic_arguments,
                        symbol.id,
                        &generic_parameters,
                    )
                    .unwrap();

                    let mut result_ty = M::from_default_type(
                        table
                            .query::<TypeAlias>(symbol.id)
                            .map_err(ResolutionToTypeError::Query)?
                            .0
                            .clone(),
                    );

                    instantiation::instantiate(&mut result_ty, &instantiation);

                    Ok(result_ty)
                }

                _ => Err(ResolutionToTypeError::InvalidKind(
                    Resolution::Generic(symbol),
                )),
            }
        }

        Resolution::MemberGeneric(symbol) => {
            let symbol_kind = *table.get::<SymbolKind>(symbol.id).unwrap();

            match symbol_kind {
                SymbolKind::TraitType => {
                    Ok(Type::TraitMember(TraitMember(MemberSymbol {
                        id: symbol.id,
                        member_generic_arguments: symbol
                            .member_generic_arguments,
                        parent_generic_arguments: symbol
                            .parent_generic_arguments,
                    })))
                }

                _ => Err(ResolutionToTypeError::InvalidKind(
                    Resolution::MemberGeneric(symbol),
                )),
            }
        }

        resolution => Err(ResolutionToTypeError::InvalidKind(resolution)),
    }
}

pub(super) fn resolve_qualified_identifier_type<M: Model>(
    table: &Table,
    syntax_tree: &syntax_tree::QualifiedIdentifier,
    referring_site: GlobalID,
    config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Type<M>, Error> {
    let is_simple_identifier = syntax_tree.rest().is_empty()
        && syntax_tree
            .root()
            .as_generic_identifier()
            .map_or(false, |x| x.generic_arguments().is_none());

    // try to resolve the simple identifier in the extra namespace
    if let (true, Some(extra_type)) = (
        is_simple_identifier,
        config.extra_namespace.and_then(|x| {
            x.types.get(syntax_tree.root().span().str()).cloned()
        }),
    ) {
        return Ok(extra_type);
    }

    let resolution = match table.resolve_qualified_identifier(
        syntax_tree,
        referring_site,
        config,
        handler,
    ) {
        Ok(resolution) => resolution,

        Err(qualified_identifier::Error::InvalidReferringSiteID) => {
            return Err(Error::InvalidReferringSiteID);
        }

        Err(qualified_identifier::Error::Fatal) => {
            return Ok(Type::Error(pernixc_term::Error));
        }

        Err(qualified_identifier::Error::Query(error)) => {
            return Err(Error::Query(error));
        }
    };

    match resolution_to_type(table, resolution) {
        Ok(ty) => Ok(ty),
        Err(ResolutionToTypeError::InvalidKind(resolution)) => {
            handler.receive(Box::new(ExpectType {
                non_type_symbol_span: syntax_tree.span(),
                resolved_global_id: resolution.global_id(),
            }));

            Ok(Type::Error(pernixc_term::Error))
        }
        Err(ResolutionToTypeError::Query(error)) => Err(Error::Query(error)),
    }
}

#[allow(clippy::too_many_lines, clippy::diverging_sub_expression)]
pub(super) fn resolve_type<M: Model>(
    table: &Table,
    syntax_tree: &syntax_tree::r#type::Type,
    referring_site: GlobalID,
    mut config: Config<M>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Type<M>, Error> {
    let ty = match syntax_tree {
        syntax_tree::r#type::Type::Primitive(primitive) => {
            Type::Primitive(match primitive {
                syntax_tree::r#type::Primitive::Bool(_) => Primitive::Bool,
                syntax_tree::r#type::Primitive::Float32(_) => {
                    Primitive::Float32
                }
                syntax_tree::r#type::Primitive::Float64(_) => {
                    Primitive::Float64
                }
                syntax_tree::r#type::Primitive::Int8(_) => Primitive::Int8,
                syntax_tree::r#type::Primitive::Int16(_) => Primitive::Int16,
                syntax_tree::r#type::Primitive::Int32(_) => Primitive::Int32,
                syntax_tree::r#type::Primitive::Int64(_) => Primitive::Int64,
                syntax_tree::r#type::Primitive::Uint8(_) => Primitive::Uint8,
                syntax_tree::r#type::Primitive::Uint16(_) => Primitive::Uint16,
                syntax_tree::r#type::Primitive::Uint32(_) => Primitive::Uint32,
                syntax_tree::r#type::Primitive::Uint64(_) => Primitive::Uint64,
                syntax_tree::r#type::Primitive::Usize(_) => Primitive::Usize,
                syntax_tree::r#type::Primitive::Isize(_) => Primitive::Isize,
            })
        }
        syntax_tree::r#type::Type::QualifiedIdentifier(
            qualified_identifier,
        ) => resolve_qualified_identifier_type(
            table,
            qualified_identifier,
            referring_site,
            config.reborrow(),
            handler,
        )?,
        syntax_tree::r#type::Type::Reference(reference) => {
            let lifetime = reference.lifetime().as_ref().map(|x| {
                table.resolve_lifetime(
                    x,
                    referring_site,
                    config.reborrow(),
                    handler,
                )
            });

            let lifetime = if let Some(lifetime) = lifetime {
                lifetime
            } else if let Some(provider) =
                config.elided_lifetime_provider.as_mut()
            {
                provider.create()
            } else {
                handler.receive(Box::new(UnexpectedInference {
                    unexpected_span: reference.ampersand().span(),
                    generic_kind: GenericKind::Lifetime,
                }));
                Lifetime::Error(pernixc_term::Error)
            };

            let qualifier = if reference.mutable_keyword().is_some() {
                Qualifier::Mutable
            } else {
                Qualifier::Immutable
            };

            let pointee = Box::new(table.resolve_type(
                reference.operand(),
                referring_site,
                config.reborrow(),
                handler,
            )?);

            Type::Reference(Reference { qualifier, lifetime, pointee })
        }
        syntax_tree::r#type::Type::Pointer(pointer_ty) => {
            let pointee = Box::new(table.resolve_type(
                pointer_ty.operand(),
                referring_site,
                config.reborrow(),
                handler,
            )?);

            Type::Pointer(Pointer {
                mutable: pointer_ty.mutable_keyword().is_some(),
                pointee,
            })
        }
        syntax_tree::r#type::Type::Tuple(syntax_tree) => {
            let mut elements = Vec::new();

            for element in syntax_tree
                .connected_list()
                .iter()
                .flat_map(ConnectedList::elements)
            {
                let ty = table.resolve_type(
                    element.r#type(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?;

                if element.ellipsis().is_some() {
                    match ty {
                        Type::Tuple(tuple) => {
                            elements.extend(tuple.elements.into_iter());
                        }
                        ty => {
                            if let Some(observer) = config.observer.as_mut() {
                                observer.on_unpacked_type_resolved(
                                    table,
                                    referring_site,
                                    &ty,
                                    element.r#type(),
                                    handler,
                                );
                            }

                            elements.push(TupleElement {
                                term: ty,
                                is_unpacked: true,
                            });
                        }
                    }
                } else {
                    elements
                        .push(TupleElement { term: ty, is_unpacked: false });
                }
            }

            // check if there is more than one unpacked type
            if elements.iter().filter(|x| x.is_unpacked).count() > 1 {
                handler.receive(Box::new(MoreThanOneUnpackedInTupleType {
                    illegal_tuple_type_span: syntax_tree.span(),
                }));
                return Ok(Type::Error(pernixc_term::Error));
            }

            Type::Tuple(Tuple { elements })
        }

        #[allow(unreachable_code, unused_variables)]
        syntax_tree::r#type::Type::Array(array) => Type::Array(Array {
            length: todo!("implements a constant eval"),
            r#type: Box::new(table.resolve_type(
                array.operand(),
                referring_site,
                config.reborrow(),
                handler,
            )?),
        }),

        syntax_tree::r#type::Type::Phantom(phantom) => {
            let ty = table.resolve_type(
                phantom.r#type(),
                referring_site,
                config.reborrow(),
                handler,
            )?;

            Type::Phantom(Phantom(Box::new(ty)))
        }
        syntax_tree::r#type::Type::Elided(elided) => {
            config.elided_type_provider.as_mut().map_or_else(
                || {
                    handler.receive(Box::new(UnexpectedInference {
                        unexpected_span: elided.span(),
                        generic_kind: GenericKind::Type,
                    }));
                    Type::Error(pernixc_term::Error)
                },
                |provider| provider.create(),
            )
        }
    };

    if let Some(observer) = config.observer.as_mut() {
        observer.on_type_resolved(
            table,
            referring_site,
            &ty,
            syntax_tree,
            handler,
        );
    }

    Ok(ty)
}
