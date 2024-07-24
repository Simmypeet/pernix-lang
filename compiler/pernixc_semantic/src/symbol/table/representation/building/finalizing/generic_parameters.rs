//! Contains the code for creating generic parameters for a generic symbol.

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{occurrences::Occurrences, Finalizer};
use crate::{
    arena::ID,
    error::{self, DuplicatedGenericParameter, MisOrderedGenericParameter},
    symbol::{
        table::{
            representation::{Element, RwLockContainer},
            resolution, Building, Table,
        },
        ConstantParameter, Generic, GenericID, GenericKind, GlobalID,
        LifetimeParameter, MemberID, TypeParameter,
    },
};

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Creates a generic parameter for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    pub(in crate::symbol::table) fn create_generic_parameters<
        T: Generic + Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: Option<&syntax_tree::item::GenericParameters>,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree.iter().flat_map(|x| {
            x.parameter_list().iter().flat_map(ConnectedList::elements)
        }) {
            match parameter {
                syntax_tree::item::GenericParameter::Constant(constant) => {
                    // extract out the default constant syntax tree
                    constant_parameter_syns
                        .push((constant.identifier(), constant.r#type()));

                    if let Some(default) = constant.default() {
                        default_constant_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Type(type_syn)
                    if constant_parameter_syns.is_empty() =>
                {
                    // extract out the default type syntax tree

                    type_parameter_syns.push(type_syn.identifier());

                    if let Some(default) = type_syn.default() {
                        default_type_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Lifetime(lifetiem)
                    if constant_parameter_syns.is_empty()
                        && type_parameter_syns.is_empty() =>
                {
                    lifetime_parameter_syns.push(lifetiem);
                }
                arg => {
                    handler.receive(Box::new(MisOrderedGenericParameter {
                        generic_kind: match arg {
                            syntax_tree::item::GenericParameter::Lifetime(
                                _,
                            ) => GenericKind::Lifetime,
                            syntax_tree::item::GenericParameter::Type(_) => {
                                GenericKind::Type
                            }
                            syntax_tree::item::GenericParameter::Constant(
                                _,
                            ) => GenericKind::Constant,
                        },
                        generic_parameter_span: arg.span(),
                    }));
                    return;
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            handler.receive(Box::new(
                error::DefaultGenericParameterMustBeTrailing {
                    invalid_generic_default_parameter_spans: default_type_syns
                        .iter()
                        .map(|x| x.span())
                        .chain(default_constant_syns.iter().map(|x| x.span()))
                        .collect(),
                },
            ));
        }

        for lifetime_parameter_syn in lifetime_parameter_syns {
            if let Err(existing) = T::get_arena(self)
                .get(generic_id)
                .unwrap()
                .write()
                .generic_declaration_mut()
                .parameters
                .add_lifetime_parameter(LifetimeParameter {
                    name: Some(
                        lifetime_parameter_syn
                            .identifier()
                            .span
                            .str()
                            .to_owned(),
                    ),
                    span: Some(
                        lifetime_parameter_syn.identifier().span.clone(),
                    ),
                })
            {
                handler.receive(Box::new(DuplicatedGenericParameter {
                    existing_generic_parameter_id: MemberID {
                        parent: generic_id.into(),
                        id: existing,
                    },
                    duplicating_generic_parameter_span: lifetime_parameter_syn
                        .identifier()
                        .span
                        .clone(),
                }));
            }
        }

        for type_parameter_syn in type_parameter_syns {
            if let Err(existing) = T::get_arena(self)
                .get(generic_id)
                .unwrap()
                .write()
                .generic_declaration_mut()
                .parameters
                .add_type_parameter(TypeParameter {
                    name: Some(type_parameter_syn.span.str().to_owned()),
                    span: Some(type_parameter_syn.span.clone()),
                })
            {
                handler.receive(Box::new(DuplicatedGenericParameter {
                    existing_generic_parameter_id: MemberID {
                        parent: generic_id.into(),
                        id: existing,
                    },
                    duplicating_generic_parameter_span: type_parameter_syn
                        .span
                        .clone(),
                }));
            }
        }

        for constant_parameter_syn in constant_parameter_syns {
            let constant_parameter = {
                let constant_type = self
                    .resolve_type(
                        constant_parameter_syn.1,
                        generic_id.into(),
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(occurrences),
                            higher_ranked_lifetimes: None,
                        },
                        handler,
                    )
                    .unwrap_or_default();

                let ty_accessibility = self
                    .get_type_accessibility(&constant_type)
                    .expect("should be valid");

                // no private type in public interface
                if self
                    .get_accessibility(generic_id.into())
                    .expect("should be a valid id")
                    > ty_accessibility
                {
                    handler.receive(Box::new(
                        error::PrivateEntityLeakedToPublicInterface {
                            entity: constant_type.clone(),
                            leaked_span: constant_parameter_syn.1.span(),
                            public_interface_id: generic_id.into(),
                            entity_overall_accessibility: ty_accessibility,
                        },
                    ));
                }

                // add the constant type to the occurrences
                occurrences.add_constant_type(
                    constant_type.clone(),
                    constant_parameter_syn.1.clone(),
                );

                ConstantParameter {
                    name: Some(constant_parameter_syn.0.span.str().to_string()),
                    r#type: constant_type,
                    span: Some(constant_parameter_syn.0.span.clone()),
                }
            };

            if let Err(existing) = T::get_arena(self)
                .get(generic_id)
                .unwrap()
                .write()
                .generic_declaration_mut()
                .parameters
                .add_constant_parameter(constant_parameter)
            {
                handler.receive(Box::new(DuplicatedGenericParameter {
                    existing_generic_parameter_id: MemberID {
                        parent: generic_id.into(),
                        id: existing,
                    },
                    duplicating_generic_parameter_span: constant_parameter_syn
                        .0
                        .span
                        .clone(),
                }));
            }
        }

        // TODO: create default parameters
    }
}
