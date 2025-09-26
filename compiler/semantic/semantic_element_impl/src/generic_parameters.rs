use std::{ops::Deref, sync::Arc};

use pernixc_handler::{Handler, Storage};
use pernixc_query::runtime::executor;
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::get_parent,
    syntax::get_generic_parameters_syntax,
    MemberID,
};
use pernixc_syntax::item::generic_parameters::GenericParameter as GenericParameterSyn;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_parameters::{
        ConstantParameter, GenericKind, GenericParameters, LifetimeParameter,
        TypeParameter,
    },
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

impl build::Build for pernixc_term::generic_parameters::Key {
    type Diagnostic = diagnostic::Diagnostic;

    #[allow(clippy::too_many_lines)]
    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<build::Output<Self>, executor::CyclicError> {
        let syntax_tree = engine.get_generic_parameters_syntax(key.0).await;

        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

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
                GenericParameterSyn::Constant(constant) => {
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
                    if constant_parameter_syns.is_empty() =>
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
                GenericParameterSyn::Lifetime(lifetiem)
                    if constant_parameter_syns.is_empty()
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
                key.0.target_id,
                engine.get_parent(key.0).await.unwrap(),
            ))
            .await?
            .deref()
            .clone();

        for lifetime_parameter_syn in lifetime_parameter_syns {
            match generic_parameters.add_lifetime_parameter(LifetimeParameter {
                name: lifetime_parameter_syn.kind.0.clone(),
                span: Some(lifetime_parameter_syn.span),
            }) {
                Ok(id) => {
                    extra_name_space.lifetimes.insert(
                        lifetime_parameter_syn.kind.0,
                        Lifetime::Parameter(MemberID { parent_id: key.0, id }),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::LifetimeParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID {
                                parent_id: key.0,
                                id,
                            },
                            duplicating_generic_parameter_span:
                                lifetime_parameter_syn.span,
                        },
                    ));
                }
            }
        }

        let kind = engine.get_kind(key.0).await;

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
                        effect_operation_id: key.0,
                    },
                ),
            );

            type_parameter_syns.clear();
            constant_parameter_syns.clear();
        }

        for type_parameter_syn in type_parameter_syns {
            match generic_parameters.add_type_parameter(TypeParameter {
                name: type_parameter_syn.kind.0.clone(),
                span: Some(type_parameter_syn.span),
            }) {
                Ok(id) => {
                    extra_name_space.types.insert(
                        type_parameter_syn.kind.0,
                        Type::Parameter(MemberID { parent_id: key.0, id }),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::TypeParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID {
                                parent_id: key.0,
                                id,
                            },
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
                let constant_type = engine
                    .resolve_type(
                        &constant_parameter_syn.1,
                        Config {
                            elided_lifetime_provider: None,
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(&mut occurrences),
                            extra_namespace: Some(&extra_name_space),
                            consider_adt_implements: true,
                            referring_site: Global::new(
                                key.0.target_id,
                                engine.get_parent(key.0).await.unwrap(),
                            ),
                        },
                        &storage,
                    )
                    .await?;

                // add the constant type to the occurrences
                occurrences.constant_types.push((
                    constant_type.clone(),
                    constant_parameter_syn.1.clone(),
                ));

                ConstantParameter {
                    name: constant_parameter_syn.0.kind.0.clone(),
                    r#type: constant_type,
                    span: Some(constant_parameter_syn.0.span),
                }
            };

            match generic_parameters.add_constant_parameter(constant_parameter)
            {
                Ok(id) => {
                    extra_name_space.constants.insert(
                        constant_parameter_syn.0.kind.0.clone(),
                        Constant::Parameter(MemberID { parent_id: key.0, id }),
                    );
                }
                Err(id) => {
                    storage.receive(Diagnostic::ConstantParameterRedefinition(
                        GenericParameterRedefinition {
                            existing_generic_parameter_id: MemberID {
                                parent_id: key.0,
                                id,
                            },
                            duplicating_generic_parameter_span:
                                constant_parameter_syn.0.span,
                        },
                    ));
                }
            }
        }

        Ok(Output {
            item: Arc::new(generic_parameters),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

build::register_build!(pernixc_term::generic_parameters::Key);
