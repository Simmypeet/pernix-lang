use std::{ops::Deref, pin::Pin};

use pernixc_handler::{Handler, Storage};
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    ExtraNamespaceWithForallLifetimes, Resolver,
    generic_parameter_namespace::get_generic_parameter_namespace,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    MemberID,
    kind::{Kind, get_kind},
    parent::get_parent,
    syntax::get_generic_parameters_syntax,
};
use pernixc_syntax::item::generic_parameters::GenericParameter as GenericParameterSyn;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_parameters::{
        ConstantParameter, GenericKind, GenericParameters, InstanceParameter,
        InstanceParameterID, LifetimeParameter, TypeParameter,
        get_generic_parameters,
    },
    instance::Instance,
    lifetime::Lifetime,
    r#type::Type,
};

use crate::{
    build::{self, Output},
    generic_parameters::diagnostic::{
        DefaultGenericParameterMustBeTrailing, Diagnostic,
        EffectOperationCanNotHaveTypeOrConstantParameters,
        GenericParameterRedefinition, MisorderedGenericParameter,
    },
    occurrences::Occurrences,
};

pub mod diagnostic;

struct ResolveInstanceParameterTraitRef<'a> {
    engine: &'a TrackedEngine,
    generic_parameters: &'a GenericParameters,
    symbol_id: Global<pernixc_symbol::ID>,
}

impl pernixc_resolution::ResolveInstanceParameterTraitRef
    for ResolveInstanceParameterTraitRef<'_>
{
    fn resolve_instance_parameter_trait_ref<'a>(
        &'a self,
        instance_parameter: &'a InstanceParameterID,
    ) -> Pin<
        Box<
            dyn Future<Output = Option<Global<pernixc_symbol::ID>>> + Send + 'a,
        >,
    > {
        Box::pin(async move {
            if instance_parameter.parent_id() == self.symbol_id {
                self.generic_parameters[instance_parameter.id()]
                    .trait_ref()
                    .map(|x| x.trait_id())
            } else {
                self.engine
                    .get_generic_parameters(instance_parameter.parent_id())
                    .await[instance_parameter.id()]
                .trait_ref()
                .map(|x| x.trait_id())
            }
        })
    }
}

impl build::Build for pernixc_term::generic_parameters::Key {
    type Diagnostic = diagnostic::Diagnostic;

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    async fn execute(
        engine: &TrackedEngine,
        key: &Self,
    ) -> build::Output<Self> {
        let syntax_tree =
            engine.get_generic_parameters_syntax(key.symbol_id).await;

        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();
        let mut instance_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        let storage = Storage::<Diagnostic>::new();

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree
            .into_iter()
            .flat_map(|x| x.parameters().collect::<Vec<_>>())
        {
            match parameter {
                GenericParameterSyn::Constant(constant)
                    if instance_parameter_syns.is_empty() =>
                {
                    let (Some(identifier), Some(ty)) =
                        (constant.identifier(), constant.r#type())
                    else {
                        continue;
                    };
                    // extract out the default constant syntax tree
                    constant_parameter_syns.push((identifier, ty));

                    if let Some(default) = constant.default() {
                        default_constant_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }

                GenericParameterSyn::Type(type_syn)
                    if instance_parameter_syns.is_empty()
                        && constant_parameter_syns.is_empty() =>
                {
                    // extract out the default type syntax tree

                    let Some(identifier) = type_syn.identifier() else {
                        continue;
                    };

                    type_parameter_syns.push(identifier);

                    if let Some(default) = type_syn.default() {
                        default_type_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }

                GenericParameterSyn::Instance(instance_syn) => {
                    let Some(name) = instance_syn.identifier() else {
                        continue;
                    };

                    instance_parameter_syns
                        .push((name, instance_syn.trait_ref()));
                }

                GenericParameterSyn::Lifetime(lifetiem)
                    if instance_parameter_syns.is_empty()
                        && constant_parameter_syns.is_empty()
                        && type_parameter_syns.is_empty() =>
                {
                    let Some(identifier) = lifetiem.identifier() else {
                        continue;
                    };

                    lifetime_parameter_syns.push(identifier);
                }

                arg => {
                    storage.receive(Diagnostic::MisorderedGenericParameter(
                        MisorderedGenericParameter {
                            generic_kind: match arg {
                                GenericParameterSyn::Lifetime(_) => {
                                    GenericKind::Lifetime
                                }
                                GenericParameterSyn::Type(_) => {
                                    GenericKind::Type
                                }
                                GenericParameterSyn::Constant(_) => {
                                    GenericKind::Constant
                                }
                                GenericParameterSyn::Instance(_) => {
                                    GenericKind::Instance
                                }
                            },
                            generic_parameter_span: arg.span(),
                        },
                    ));
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            for span in default_constant_syns
                .iter()
                .map(SourceElement::span)
                .chain(default_type_syns.iter().map(SourceElement::span))
            {
                storage.receive(
                    Diagnostic::DefaultGenericParameterMustBeTrailing(
                        DefaultGenericParameterMustBeTrailing {
                            invalid_generic_default_parameter_span: span,
                        },
                    ),
                );
            }
        }

        let mut generic_parameters = GenericParameters::default();
        let mut extra_name_space = engine
            .get_generic_parameter_namespace(Global::new(
                key.symbol_id.target_id,
                engine.get_parent(key.symbol_id).await.unwrap(),
            ))
            .await
            .deref()
            .clone();

        for lifetime_parameter_syn in lifetime_parameter_syns {
            match generic_parameters.add_lifetime_parameter(
                LifetimeParameter::new(
                    lifetime_parameter_syn.kind.0.clone(),
                    Some(lifetime_parameter_syn.span),
                ),
            ) {
                Ok(id) => {
                    extra_name_space.insert_lifetime(
                        lifetime_parameter_syn.kind.0,
                        Lifetime::Parameter(MemberID::new(key.symbol_id, id)),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::LifetimeParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID::new(
                                key.symbol_id,
                                id,
                            ),
                            duplicating_generic_parameter_span:
                                lifetime_parameter_syn.span,
                        },
                    ));
                }
            }
        }

        let kind = engine.get_kind(key.symbol_id).await;

        // check if the generic parameters are for an effect operation
        if kind == Kind::EffectOperation
            && (!type_parameter_syns.is_empty()
                || !constant_parameter_syns.is_empty())
        {
            storage.receive(
                Diagnostic::EffectOperationCanNotHaveTypeOrConstantParameters(
                    EffectOperationCanNotHaveTypeOrConstantParameters {
                        type_or_constant_parameter_span: type_parameter_syns
                            .iter()
                            .map(|x| x.span)
                            .chain(
                                constant_parameter_syns
                                    .iter()
                                    .map(|x| x.0.span),
                            )
                            .collect(),
                        effect_operation_id: key.symbol_id,
                    },
                ),
            );

            type_parameter_syns.clear();
            constant_parameter_syns.clear();
        }

        for type_parameter_syn in type_parameter_syns {
            match generic_parameters.add_type_parameter(TypeParameter::new(
                type_parameter_syn.kind.0.clone(),
                Some(type_parameter_syn.span),
            )) {
                Ok(id) => {
                    extra_name_space.insert_type(
                        type_parameter_syn.kind.0,
                        Type::Parameter(MemberID::new(key.symbol_id, id)),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::TypeParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID::new(
                                key.symbol_id,
                                id,
                            ),
                            duplicating_generic_parameter_span:
                                type_parameter_syn.span,
                        },
                    ));
                }
            }
        }

        let mut occurrences = Occurrences::default();
        for constant_parameter_syn in constant_parameter_syns {
            let constant_parameter = {
                // the type used for the constant parameter
                let constant_type = Resolver::builder()
                    .tracked_engine(engine)
                    .handler(&storage)
                    .observer(&mut occurrences)
                    .extra_namespace(&extra_name_space)
                    .referring_site(Global::new(
                        key.symbol_id.target_id,
                        engine.get_parent(key.symbol_id).await.unwrap(),
                    ))
                    .build()
                    .resolve_type(&constant_parameter_syn.1)
                    .await;

                // add the constant type to the occurrences
                occurrences.constant_types.push((
                    constant_type.clone(),
                    constant_parameter_syn.1.clone(),
                ));

                ConstantParameter::new(
                    constant_parameter_syn.0.kind.0.clone(),
                    constant_type,
                    Some(constant_parameter_syn.0.span),
                )
            };

            match generic_parameters.add_constant_parameter(constant_parameter)
            {
                Ok(id) => {
                    extra_name_space.insert_constant(
                        constant_parameter_syn.0.kind.0.clone(),
                        Constant::Parameter(MemberID::new(key.symbol_id, id)),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::ConstantParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID::new(
                                key.symbol_id,
                                id,
                            ),
                            duplicating_generic_parameter_span:
                                constant_parameter_syn.0.span,
                        },
                    ));
                }
            }
        }

        for (identifier, trait_ref_syn) in instance_parameter_syns {
            let trait_ref = 'trait_ref: {
                let Some(trait_ref_syn) = trait_ref_syn else {
                    break 'trait_ref None;
                };

                let Some(qualified_identifier) =
                    trait_ref_syn.qualified_identifier()
                else {
                    break 'trait_ref None;
                };

                let resolver = ResolveInstanceParameterTraitRef {
                    engine,
                    generic_parameters: &generic_parameters,
                    symbol_id: key.symbol_id,
                };

                let extra_namespace_wrapper =
                    ExtraNamespaceWithForallLifetimes::new(
                        &mut extra_name_space,
                        trait_ref_syn.higher_ranked_lifetimes().as_ref(),
                        &storage,
                    );

                Resolver::builder()
                    .tracked_engine(engine)
                    .handler(&storage)
                    .observer(&mut occurrences)
                    .extra_namespace(
                        extra_namespace_wrapper.extra_namespace(),
                    )
                    .referring_site(Global::new(
                        key.symbol_id.target_id,
                        engine.get_parent(key.symbol_id).await.unwrap(),
                    ))
                    .resolve_instance_parameter_trait_ref(&resolver)
                    .build()
                    .resolve_qualified_identifier_trait_ref(&qualified_identifier)
                    .await
            };

            match generic_parameters.add_instance_parameter(
                InstanceParameter::new(
                    identifier.kind.0.clone(),
                    trait_ref.map(|x| engine.intern(x)),
                    Some(identifier.span),
                ),
            ) {
                Ok(id) => {
                    extra_name_space.insert_instance(
                        identifier.kind.0.clone(),
                        Instance::Parameter(MemberID::new(key.symbol_id, id)),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::InstanceParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID::new(
                                key.symbol_id,
                                id,
                            ),
                            duplicating_generic_parameter_span: identifier.span,
                        },
                    ));
                }
            }
        }

        Output {
            item: engine.intern(generic_parameters),
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(occurrences),
        }
    }
}

build::register_build!(pernixc_term::generic_parameters::Key);
