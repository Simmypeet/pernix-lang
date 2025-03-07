//! Contains the builder for the generic parameters.

use std::sync::Arc;

use diagnostic::{
    DefaultGenericParameterMustBeTrailing, DuplicatedGenericParameter,
    MisOrderedGenericParameter,
};
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext, GetGenericParameterNamespaceExt as _};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    item::generic_parameter::GenericParameter as GenericParameterSyn,
    ConnectedList,
};
use pernixc_table::{
    component::{
        syntax_tree as syntax_tree_component, Derived, Parent, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalID, MemberID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_parameter::{
        ConstantParameter, GenericKind, GenericParameters, LifetimeParameter,
        TypeParameter,
    },
    lifetime::Lifetime,
    r#type::Type,
};

use crate::{
    builder::Builder,
    occurrences::{self, Occurrences},
};

pub mod diagnostic;

impl query::Builder<GenericParameters> for Builder {
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<GenericParameters>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_generic_parameters() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            GenericParameters::component_name(),
        );

        let syntax_tree =
            table.get::<syntax_tree_component::GenericParameters>(global_id);

        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree.iter().flat_map(|x| {
            x.connected_list.iter().flat_map(ConnectedList::elements)
        }) {
            match parameter {
                GenericParameterSyn::Constant(constant) => {
                    // extract out the default constant syntax tree
                    constant_parameter_syns
                        .push((&constant.identifier, &constant.r#type));

                    if let Some(default) = &constant.default {
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

                    type_parameter_syns.push(&type_syn.identifier);

                    if let Some(default) = &type_syn.default {
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
                    lifetime_parameter_syns.push(lifetiem);
                }
                arg => {
                    handler.receive(Box::new(MisOrderedGenericParameter {
                        generic_kind: match arg {
                            GenericParameterSyn::Lifetime(_) => {
                                GenericKind::Lifetime
                            }
                            GenericParameterSyn::Type(_) => GenericKind::Type,
                            GenericParameterSyn::Constant(_) => {
                                GenericKind::Constant
                            }
                        },
                        generic_parameter_span: arg.span(),
                    }));
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            for span in default_constant_syns
                .iter()
                .map(|x| x.span())
                .chain(default_type_syns.iter().map(|x| x.span()))
            {
                handler.receive(Box::new(
                    DefaultGenericParameterMustBeTrailing {
                        invalid_generic_default_parameter_span: span,
                    },
                ));
            }
        }

        let mut generic_parameters = GenericParameters::default();
        let mut extra_name_space =
            table.get_generic_parameter_namepsace(GlobalID::new(
                global_id.target_id,
                table.get::<Parent>(global_id).parent.unwrap(),
            ));

        for lifetime_parameter_syn in lifetime_parameter_syns {
            match generic_parameters.add_lifetime_parameter(LifetimeParameter {
                name: lifetime_parameter_syn.identifier.span.str().to_owned(),
                span: Some(lifetime_parameter_syn.identifier.span.clone()),
            }) {
                Ok(id) => {
                    extra_name_space.lifetimes.insert(
                        lifetime_parameter_syn.identifier.span.str().to_owned(),
                        Lifetime::Parameter(MemberID { parent: global_id, id }),
                    );
                }
                Err(id) => {
                    handler.receive(Box::new(DuplicatedGenericParameter {
                        existing_generic_parameter_id: MemberID {
                            parent: global_id,
                            id,
                        },
                        duplicating_generic_parameter_span:
                            lifetime_parameter_syn.identifier.span.clone(),
                    }));
                }
            }
        }

        for type_parameter_syn in type_parameter_syns {
            match generic_parameters.add_type_parameter(TypeParameter {
                name: type_parameter_syn.span.str().to_owned(),
                span: Some(type_parameter_syn.span.clone()),
            }) {
                Ok(id) => {
                    extra_name_space.types.insert(
                        type_parameter_syn.span.str().to_owned(),
                        Type::Parameter(MemberID { parent: global_id, id }),
                    );
                }
                Err(id) => {
                    handler.receive(Box::new(DuplicatedGenericParameter {
                        existing_generic_parameter_id: MemberID {
                            parent: global_id,
                            id,
                        },
                        duplicating_generic_parameter_span: type_parameter_syn
                            .span
                            .clone(),
                    }));
                }
            }
        }

        for constant_parameter_syn in constant_parameter_syns {
            let constant_parameter = {
                // the type used for the constant parameter
                let constant_type = table.resolve_type(
                    constant_parameter_syn.1,
                    global_id,
                    Config {
                        elided_lifetime_provider: None,
                        elided_type_provider: None,
                        elided_constant_provider: None,
                        observer: Some(&mut occurrences::Observer),
                        extra_namespace: Some(&extra_name_space),
                    },
                    handler,
                );

                // add the constant type to the occurrences
                table
                    .get::<Occurrences>(global_id)
                    .write()
                    .constant_types
                    .push((
                        constant_type.clone(),
                        constant_parameter_syn.1.clone(),
                    ));

                ConstantParameter {
                    name: constant_parameter_syn.0.span.str().to_string(),
                    r#type: constant_type,
                    span: Some(constant_parameter_syn.0.span.clone()),
                }
            };

            match generic_parameters.add_constant_parameter(constant_parameter)
            {
                Ok(id) => {
                    extra_name_space.constants.insert(
                        constant_parameter_syn.0.span.str().to_owned(),
                        Constant::Parameter(MemberID { parent: global_id, id }),
                    );
                }
                Err(id) => {
                    handler.receive(Box::new(DuplicatedGenericParameter {
                        existing_generic_parameter_id: MemberID {
                            parent: global_id,
                            id,
                        },
                        duplicating_generic_parameter_span:
                            constant_parameter_syn.0.span.clone(),
                    }));
                }
            }
        }

        Some(Arc::new(generic_parameters))
    }
}

#[cfg(test)]
mod test;
