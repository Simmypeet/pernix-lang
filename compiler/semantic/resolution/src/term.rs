//! Contains the logic for solving terms (types, lifetimes, and constants) in
//! the resolution process.

use std::{fmt::Debug, ops::Deref};

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    instance_associated_value::get_instance_associated_value,
    type_alias::get_type_alias,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent_global,
};
use pernixc_syntax::{
    GenericArgument as GenericArgumentSyn, GenericIdentifier,
    LifetimeIdentifier,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    effect,
    generic_arguments::{
        GenericArguments, Symbol, is_generic_arguments_identity_to,
    },
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
    Error, Resolver,
    diagnostic::{
        Diagnostic, ExpectEffect, ExpectInstance, ExpectTrait, ExpectType,
        LifetimeParameterNotFound, MismatchedGenericArgumentCount,
        MismatchedKindInArgument, MisorderedGenericArgument,
        MoreThanOneUnpackedInTupleType, UnexpectedInference,
    },
    qualified_identifier::Resolution,
};

macro_rules! resolve_generic_arguments_kind {
    (
        $self:ident,
        $generic_parameters:ident,
        $param:ident,
        $generic_arguments_syn:ident,
        $terms:ident,
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
                $terms.push($resolve);
            }

            if $terms.len() != $generic_parameters.parameter_len::<$param>() {
                $self.receive_diagnostic(
                    Diagnostic::MismatchedGenericArgumentCount(
                        MismatchedGenericArgumentCount {
                            generic_kind: $kind,
                            generic_identifier_span: $span,
                            expected_count: $generic_parameters
                                .parameter_len::<$param>(),
                            supplied_count: $terms.len(),
                        },
                    ),
                );
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
                $self.receive_diagnostic(
                    Diagnostic::MismatchedGenericArgumentCount(
                        MismatchedGenericArgumentCount {
                            generic_kind: $kind,
                            generic_identifier_span: $span,
                            expected_count: $generic_parameters
                                .parameter_len::<$param>(),
                            supplied_count: $terms.len(),
                        },
                    ),
                );
            }

            $terms.resize(
                $generic_parameters.parameter_len::<$param>(),
                $error_term,
            );

            $terms
        }
    }};
}

impl Resolver<'_, '_> {
    async fn resolve_type_argument(
        &mut self,
        syn: &pernixc_syntax::GenericArgument,
        symbol_id: Global<pernixc_symbol::ID>,
        param_id: pernixc_arena::ID<TypeParameter>,
    ) -> Type {
        match syn {
            GenericArgumentSyn::Lifetime(lifetime) => {
                self.receive_diagnostic(Diagnostic::MismatchedKindInArgument(
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

            GenericArgumentSyn::InstanceValue(qualified_identifier) => {
                Box::pin(async move {
                    let Some(q) = qualified_identifier.qualified_identifier()
                    else {
                        return Type::Error(pernixc_term::error::Error);
                    };

                    self.push_higher_ranked_lifetimes(
                        qualified_identifier.higher_ranked_lifetimes().as_ref(),
                    );

                    let res = self.resolve_qualified_identifier_type(&q).await;

                    self.pop_higher_ranked_lifetimes();

                    res
                })
                .await
            }

            GenericArgumentSyn::Type(ty) => {
                Box::pin(async move { self.resolve_type(ty).await }).await
            }

            GenericArgumentSyn::Constant(constant_argument) => {
                self.receive_diagnostic(Diagnostic::MismatchedKindInArgument(
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

    async fn resolve_instance_argument(
        &mut self,
        syn: &pernixc_syntax::GenericArgument,
        symbol_id: Global<pernixc_symbol::ID>,
        param_id: pernixc_arena::ID<InstanceParameter>,
    ) -> Instance {
        use GenericArgumentSyn::InstanceValue;

        match syn {
            GenericArgumentSyn::InstanceValue(qualified_identifier) => {
                Box::pin(async move {
                    let Some(q) = qualified_identifier.qualified_identifier()
                    else {
                        return Instance::Error(pernixc_term::error::Error);
                    };

                    self.push_higher_ranked_lifetimes(
                        qualified_identifier.higher_ranked_lifetimes().as_ref(),
                    );

                    let res =
                        self.resolve_qualified_identifier_instance(&q).await;

                    self.pop_higher_ranked_lifetimes();

                    res
                })
                .await
            }

            syn => {
                self.receive_diagnostic(Diagnostic::MismatchedKindInArgument(
                    MismatchedKindInArgument::builder()
                        .argument_span(syn.span())
                        .found_parameter(
                            InstanceParameterID::new(symbol_id, param_id)
                                .into(),
                        )
                        .found_kind(match syn {
                            GenericArgumentSyn::Lifetime(_) => {
                                GenericKind::Lifetime
                            }
                            GenericArgumentSyn::Type(_) => GenericKind::Type,
                            GenericArgumentSyn::Constant(_) => {
                                GenericKind::Constant
                            }

                            InstanceValue(_) => unreachable!(),
                        })
                        .build(),
                ));

                Instance::Error(pernixc_term::error::Error)
            }
        }
    }

    /// Resolves the generic arguments resides within the given
    /// [`generic_identifier`] for the given symbol.
    ///
    /// This function ensure that the generic arguments returned has a matching
    /// generic arguments count with the generic parameters of the given symbol.
    pub async fn resolve_generic_arguments_for(
        &mut self,
        symbol_id: Global<pernixc_symbol::ID>,
        generic_identifier: &GenericIdentifier,
    ) -> GenericArguments {
        self.resolve_generic_arguments_for_internal(
            symbol_id,
            None,
            generic_identifier,
        )
        .await
    }

    #[allow(
        clippy::too_many_lines,
        clippy::cognitive_complexity,
        clippy::diverging_sub_expression,
        unreachable_code
    )]
    pub(crate) async fn resolve_generic_arguments_for_internal(
        &mut self,
        symbol_id: Global<pernixc_symbol::ID>,
        bound_type: Option<&Type>,
        generic_identifier: &GenericIdentifier,
    ) -> GenericArguments {
        let mut generic_arguments_syn = generic_identifier
            .generic_arguments()
            .map(|x| x.arguments().collect::<Vec<_>>());

        let generic_parameters =
            self.tracked_engine().get_generic_parameters(symbol_id).await;

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

                        // scan for lifetimes appearing after non-lifetime
                        // generic arguments
                        for syn in generic_arguments_syn
                            .extract_if(.., |x| x.is_lifetime())
                            .map(|x| x.into_lifetime().unwrap())
                        {
                            self.receive_diagnostic(
                                Diagnostic::MisorderedGenericArgument(
                                    MisorderedGenericArgument {
                                        generic_kind: GenericKind::Lifetime,
                                        generic_argument: syn.span(),
                                    },
                                ),
                            );
                        }

                        lifetime_syn
                    },
                )
            };

            // if there is no lifetime found but they are required, use the
            // elided lifetime provider (if any) to fill the lifetime arguments
            if lifetime_syn.is_empty()
                && generic_parameters.lifetime_parameters_len() > 0
                && let Some(lt) = self.create_elided_lifetime()
            {
                break 'lifetime vec![
                    lt;
                    generic_parameters
                        .lifetime_parameters_len()
                ];
            }

            let mut lifetimes = Vec::new();
            for lifetime_syn in lifetime_syn {
                lifetimes.push(
                    self.resolve_lifetime(
                        &lifetime_syn.into_lifetime().unwrap(),
                    ),
                );
            }

            if lifetimes.len()
                != generic_parameters.lifetime_parameter_order().len()
            {
                self.receive_diagnostic(
                    Diagnostic::MismatchedGenericArgumentCount(
                        MismatchedGenericArgumentCount {
                            generic_kind: GenericKind::Lifetime,
                            generic_identifier_span: generic_identifier.span(),
                            expected_count: generic_parameters
                                .lifetime_parameter_order()
                                .len(),
                            supplied_count: lifetimes.len(),
                        },
                    ),
                );
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
                self,
                generic_parameters,
                TypeParameter,
                generic_arguments_syn,
                types,
                GenericKind::Type,
                generic_identifier.span(),
                Type::Error(pernixc_term::error::Error),
                self.elided_type_provider_mut(),
                break 'ty types,
                |syn, param_id| self.resolve_type_argument(
                    &syn,
                    symbol_id,
                    param_id,
                ).await,
            }
        };

        #[allow(unused_variables)]
        let constants = 'consts: {
            let mut constants = Vec::new();

            resolve_generic_arguments_kind! {
                self,
                generic_parameters,
                ConstantParameter,
                generic_arguments_syn,
                constants,
                GenericKind::Constant,
                generic_identifier.span(),
                Constant::Error(pernixc_term::error::Error),
                self.elided_constant_provider_mut(),
                break 'consts constants,
                |syn, param_id| {
                    todo!("constant evaluation is not implemented yet")
                },
            }
        };

        let instances = 'instances: {
            let mut instances = Vec::new();

            resolve_generic_arguments_kind! {
                self,
                generic_parameters,
                InstanceParameter,
                generic_arguments_syn,
                instances,
                GenericKind::Instance,
                generic_identifier.span(),
                Instance::Error(pernixc_term::error::Error),
                self.elided_instance_provider_mut(),
                break 'instances instances,
                |syn, param_id| {
                    self.resolve_instance_argument(
                        &syn,
                        symbol_id,
                        param_id,
                    ).await
                },
            }
        };

        GenericArguments::new(lifetimes, types, constants, instances)
    }

    /// Resolves a [`pernixc_syntax::Lifetime`] as a [`Lifetime`] term.
    pub fn resolve_lifetime(
        &mut self,
        lifetime_argument: &pernixc_syntax::Lifetime,
    ) -> Lifetime {
        let lifetime = match lifetime_argument.identifier() {
            Some(LifetimeIdentifier::Static(..)) => Lifetime::Static,
            Some(LifetimeIdentifier::Identifier(ident)) => {
                self.resolve_lifetime_parameter(&ident)
            }
            Some(LifetimeIdentifier::Elided(elided)) => {
                self.create_elided_lifetime().unwrap_or_else(|| {
                    self.receive_diagnostic(Diagnostic::UnexpectedInference(
                        UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Lifetime,
                        },
                    ));
                    Lifetime::Error(pernixc_term::error::Error)
                })
            }

            None => Lifetime::Error(pernixc_term::error::Error),
        };

        self.notify_lifetime_resolved(&lifetime, lifetime_argument);

        lifetime
    }

    /// Resolves a [`Lifetime`] from an identifier.
    #[must_use]
    pub fn resolve_lifetime_parameter(
        &self,
        identifier: &pernixc_syntax::Identifier,
    ) -> Lifetime {
        // reach to the extra namespace first
        if let Some(extra_lifetime) =
            self.lookup_extra_lifetime(&identifier.kind.0)
        {
            return extra_lifetime;
        }

        self.receive_diagnostic(Diagnostic::LifetimeParameterNotFound(
            LifetimeParameterNotFound {
                referred_span: identifier.span(),
                referring_site: self.referring_site(),
                name: identifier.kind.0.clone(),
            },
        ));
        Lifetime::Error(pernixc_term::error::Error)
    }

    /// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as an [`Instance`].
    pub async fn resolve_instance_value(
        &mut self,
        syntax_tree: &pernixc_syntax::InstanceValue,
    ) -> Instance {
        let Some(qual) = syntax_tree.qualified_identifier() else {
            return Instance::Error(pernixc_term::error::Error);
        };

        self.push_higher_ranked_lifetimes(
            syntax_tree.higher_ranked_lifetimes().as_ref(),
        );

        let instance = self.resolve_qualified_identifier_instance(&qual).await;

        self.pop_higher_ranked_lifetimes();

        instance
    }

    /// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as an [`Instance`].
    pub async fn resolve_qualified_identifier_instance(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    ) -> Instance {
        let resolution =
            match self.resolve_qualified_identifier(syntax_tree).await {
                Ok(resolution) => resolution,

                Err(Error::Abort) => {
                    return Instance::Error(pernixc_term::error::Error);
                }
            };

        match self.tracked_engine().resolution_to_instance(resolution).await {
            Ok(instance) => instance,
            Err(ResolutionToTermError::Failed(resolution)) => {
                self.receive_diagnostic(Diagnostic::ExpectInstance(
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
    pub async fn resolve_qualified_identifier_type(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    ) -> Type {
        let resolution =
            match self.resolve_qualified_identifier(syntax_tree).await {
                Ok(resolution) => resolution,

                Err(Error::Abort) => {
                    return Type::Error(pernixc_term::error::Error);
                }
            };

        match self.tracked_engine().resolution_to_type(resolution).await {
            Ok(ty) => ty,
            Err(ResolutionToTermError::Failed(resolution)) => {
                self.receive_diagnostic(Diagnostic::ExpectType(
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
    pub async fn resolve_qualified_identifier_trait_ref(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    ) -> Option<TraitRef> {
        let resolution =
            match self.resolve_qualified_identifier(syntax_tree).await {
                Ok(resolution) => resolution,

                Err(Error::Abort) => {
                    return None;
                }
            };

        match self.tracked_engine().resolution_to_trait_ref(resolution).await {
            Ok(trait_ref) => Some(trait_ref),
            Err(ResolutionToTermError::Failed(resolution)) => {
                self.receive_diagnostic(Diagnostic::ExpectTrait(
                    ExpectTrait::builder()
                        .non_trait_symbol_span(syntax_tree.span())
                        .resolved_resolution(resolution)
                        .build(),
                ));

                None
            }
        }
    }

    /// Resolves a [`pernixc_syntax::QualifiedIdentifier`] as an effect unit.
    pub async fn resolve_qualified_identifier_effect_unit(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
    ) -> Option<effect::Unit> {
        let resolution =
            match self.resolve_qualified_identifier(syntax_tree).await {
                Ok(resolution) => resolution,

                Err(Error::Abort) => {
                    return None;
                }
            };

        match self.tracked_engine().resolution_to_effect_unit(resolution).await
        {
            Ok(effect_unit) => Some(effect_unit),
            Err(ResolutionToTermError::Failed(resolution)) => {
                self.receive_diagnostic(Diagnostic::ExpectEffect(
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
    #[allow(clippy::too_many_lines, clippy::diverging_sub_expression)]
    pub async fn resolve_type(
        &mut self,
        syntax_tree: &pernixc_syntax::r#type::Type,
    ) -> Type {
        let ty = match syntax_tree {
            pernixc_syntax::r#type::Type::Primitive(primitive) => {
                Type::Primitive(match primitive {
                    pernixc_syntax::r#type::Primitive::Bool(_) => {
                        Primitive::Bool
                    }
                    pernixc_syntax::r#type::Primitive::Float32(_) => {
                        Primitive::Float32
                    }
                    pernixc_syntax::r#type::Primitive::Float64(_) => {
                        Primitive::Float64
                    }
                    pernixc_syntax::r#type::Primitive::Int8(_) => {
                        Primitive::Int8
                    }
                    pernixc_syntax::r#type::Primitive::Int16(_) => {
                        Primitive::Int16
                    }
                    pernixc_syntax::r#type::Primitive::Int32(_) => {
                        Primitive::Int32
                    }
                    pernixc_syntax::r#type::Primitive::Int64(_) => {
                        Primitive::Int64
                    }
                    pernixc_syntax::r#type::Primitive::Uint8(_) => {
                        Primitive::Uint8
                    }
                    pernixc_syntax::r#type::Primitive::Uint16(_) => {
                        Primitive::Uint16
                    }
                    pernixc_syntax::r#type::Primitive::Uint32(_) => {
                        Primitive::Uint32
                    }
                    pernixc_syntax::r#type::Primitive::Uint64(_) => {
                        Primitive::Uint64
                    }
                    pernixc_syntax::r#type::Primitive::Usize(_) => {
                        Primitive::Usize
                    }
                    pernixc_syntax::r#type::Primitive::Isize(_) => {
                        Primitive::Isize
                    }
                })
            }

            pernixc_syntax::r#type::Type::QualifiedIdentifier(
                qualified_identifier,
            ) => {
                Box::pin(
                    self.resolve_qualified_identifier_type(
                        qualified_identifier,
                    ),
                )
                .await
            }

            pernixc_syntax::r#type::Type::Reference(reference) => {
                let lifetime = reference
                    .lifetime()
                    .as_ref()
                    .map(|lifetime| self.resolve_lifetime(lifetime));

                let lifetime = lifetime.unwrap_or_else(|| {
                    self.create_elided_lifetime().unwrap_or_else(|| {
                        self.receive_diagnostic(
                            Diagnostic::UnexpectedInference(
                                UnexpectedInference {
                                    unexpected_span: reference
                                        .ampersand()
                                        .map_or_else(
                                            || reference.span(),
                                            |x| x.span,
                                        ),
                                    generic_kind: GenericKind::Lifetime,
                                },
                            ),
                        );
                        Lifetime::Error(pernixc_term::error::Error)
                    })
                });

                let qualifier = if reference.mut_keyword().is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
                };

                let pointee =
                    Box::new(if let Some(pointee) = reference.r#type() {
                        Box::pin(self.resolve_type(&pointee)).await
                    } else {
                        Type::Error(pernixc_term::error::Error)
                    });

                Type::Reference(Reference { qualifier, lifetime, pointee })
            }
            pernixc_syntax::r#type::Type::Pointer(pointer_ty) => {
                let pointee =
                    Box::new(if let Some(pointee) = pointer_ty.r#type() {
                        Box::pin(self.resolve_type(&pointee)).await
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
                        Box::pin(self.resolve_type(&ty)).await
                    } else {
                        Type::Error(pernixc_term::error::Error)
                    };

                    if element.ellipsis().is_some() {
                        match ty {
                            Type::Tuple(tuple) => {
                                elements.extend(tuple.into_elements());
                            }
                            ty => {
                                self.notify_unpacked_type_resolved(
                                    &ty, &element,
                                );

                                elements.push(tuple::Element::new(ty, true));
                            }
                        }
                    } else {
                        elements.push(tuple::Element::new(ty, false));
                    }
                }

                // check if there is more than one unpacked type
                if elements.iter().filter(|x| x.is_unpacked()).count() > 1 {
                    self.receive_diagnostic(
                        Diagnostic::MoreThanOneUnpackedInTupleType(
                            MoreThanOneUnpackedInTupleType {
                                illegal_tuple_type_span: syntax_tree.span(),
                            },
                        ),
                    );

                    Type::Error(pernixc_term::error::Error)
                } else {
                    Type::Tuple(tuple::Tuple::new(elements))
                }
            }

            #[allow(unreachable_code, unused_variables)]
            pernixc_syntax::r#type::Type::Array(array) => Type::Array(Array {
                length: todo!("implements a constant eval"),
                r#type: Box::new(if let Some(ty) = array.r#type() {
                    self.resolve_type(&ty).await
                } else {
                    Type::Error(pernixc_term::error::Error)
                }),
            }),

            pernixc_syntax::r#type::Type::Phantom(phantom) => {
                let ty = if let Some(ty) = phantom.r#type() {
                    Box::pin(self.resolve_type(&ty)).await
                } else {
                    Type::Error(pernixc_term::error::Error)
                };

                Type::Phantom(Phantom(Box::new(ty)))
            }
            pernixc_syntax::r#type::Type::Elided(elided) => {
                self.create_elided_type().unwrap_or_else(|| {
                    self.receive_diagnostic(Diagnostic::UnexpectedInference(
                        UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Type,
                        },
                    ));
                    Type::Error(pernixc_term::error::Error)
                })
            }
        };

        self.notify_type_resolved(&ty, syntax_tree);

        ty
    }
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

        Resolution::MemberGeneric(member_generic) => {
            let kind = self.get_kind(member_generic.id).await;

            match kind {
                Kind::TraitAssociatedInstance => {
                    let parent_trait_id = self
                        .get_parent_global(member_generic.id)
                        .await
                        .unwrap();

                    if !self
                        .is_generic_arguments_identity_to(
                            &member_generic.parent_generic_arguments,
                            parent_trait_id,
                        )
                        .await
                    {
                        return Err(ResolutionToTermError::Failed(
                            Resolution::MemberGeneric(member_generic),
                        ));
                    }

                    Ok(Instance::new_instance_associated(
                        Box::new(Instance::new_anonymous_trait(
                            parent_trait_id,
                        )),
                        member_generic.id,
                        member_generic.member_generic_arguments,
                    ))
                }

                Kind::InstanceAssociatedInstance => {
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

                _ => Err(ResolutionToTermError::Failed(
                    Resolution::MemberGeneric(member_generic),
                )),
            }
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

                Kind::TraitAssociatedType => {
                    // if the parent generic arugments is identical to the
                    // trait, we can make it an instance
                    // associated type and make the instance be anonymous trait
                    let parent_trait = self
                        .get_parent_global(member_generic.id)
                        .await
                        .unwrap();

                    if !self
                        .is_generic_arguments_identity_to(
                            &member_generic.parent_generic_arguments,
                            parent_trait,
                        )
                        .await
                    {
                        return Err(ResolutionToTermError::Failed(
                            Resolution::MemberGeneric(member_generic),
                        ));
                    }

                    Ok(Type::new_instance_associated(
                        Box::new(Instance::new_anonymous_trait(parent_trait)),
                        member_generic.id,
                        member_generic.member_generic_arguments,
                    ))
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
