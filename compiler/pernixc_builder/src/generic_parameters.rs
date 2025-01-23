//! Contains the builder for the generic parameters.

use std::{collections::hash_map::Entry, sync::Arc};

use diagnostic::{
    DefaultGenericParameterMustBeTrailing, DuplicatedGenericParameter,
    MisOrderedGenericParameter,
};
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext as _, ExtraNamespace};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{self, ConnectedList};
use pernixc_table::{
    component::{
        syntax_tree as syntax_tree_component, Derived, Parent, SymbolKind,
    },
    diagnostic::Diagnostic,
    query::{self, Handle},
    GlobalID, MemberID, Table,
};
use pernixc_term::{
    accessibility::Ext as _,
    constant::Constant,
    generic_parameter::{
        ConstantParameter, ConstantParameterID, GenericKind, GenericParameters,
        LifetimeParameter, LifetimeParameterID, TypeParameter, TypeParameterID,
    },
    lifetime::Lifetime,
    r#type::Type,
    Model,
};

use crate::{
    accessibility,
    builder::Builder,
    diagnostic::PrivateEntityLeakedToPublicInterface,
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
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_generic_parameters() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            GenericParameters::component_name(),
        );

        let syntax_tree = table
            .get::<syntax_tree_component::GenericParameters>(global_id)
            .unwrap();

        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree.iter().flat_map(|x| {
            x.connected_list().iter().flat_map(ConnectedList::elements)
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
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            for span in default_constant_syns
                .iter()
                .map(|x| x.span())
                .chain(default_constant_syns.iter().map(|x| x.span()))
            {
                handler.receive(Box::new(
                    DefaultGenericParameterMustBeTrailing {
                        invalid_generic_default_parameter_span: span,
                    },
                ));
            }
        }

        let mut generic_parameters = GenericParameters::default();
        let mut extra_name_space = table.get_generic_parameter_namepsace(
            GlobalID::new(
                global_id.target_id,
                table.get::<Parent>(global_id).unwrap().0,
            ),
            handler,
        );

        for lifetime_parameter_syn in lifetime_parameter_syns {
            match generic_parameters.add_lifetime_parameter(LifetimeParameter {
                name: Some(
                    lifetime_parameter_syn.identifier().span.str().to_owned(),
                ),
                span: Some(lifetime_parameter_syn.identifier().span.clone()),
            }) {
                Ok(id) => {
                    extra_name_space.lifetimes.insert(
                        lifetime_parameter_syn
                            .identifier()
                            .span
                            .str()
                            .to_owned(),
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
                            lifetime_parameter_syn.identifier().span.clone(),
                    }));
                }
            }
        }

        for type_parameter_syn in type_parameter_syns {
            match generic_parameters.add_type_parameter(TypeParameter {
                name: Some(type_parameter_syn.span.str().to_owned()),
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

        let symbol_accessibility = table
            .get_accessibility(global_id)
            .unwrap()
            .into_global(global_id.target_id);

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

                let ty_accessibility = table
                    .get_type_accessibility(&constant_type)
                    .expect("should be valid");

                if accessibility::check_private_entity_leakage(
                    table,
                    ty_accessibility,
                    symbol_accessibility,
                ) {
                    handler.receive(Box::new(
                        PrivateEntityLeakedToPublicInterface {
                            entity: constant_type.clone(),
                            leaked_span: constant_parameter_syn.1.span(),
                            entity_overall_accessibility: ty_accessibility,
                            public_accessibility: symbol_accessibility,
                        },
                    ));
                }

                // add the constant type to the occurrences
                if !table.has::<Occurrences>(global_id) {
                    let _ =
                        table.add_component(global_id, Occurrences::default());
                }

                table
                    .get_mut::<Occurrences>(global_id)
                    .unwrap()
                    .constant_types
                    .push((
                        constant_type.clone(),
                        constant_parameter_syn.1.clone(),
                    ));

                ConstantParameter {
                    name: Some(constant_parameter_syn.0.span.str().to_string()),
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

/// An extension for the [`Table`] to get the generic parameters namespace.
pub trait Ext {
    /// Creates the [`ExtraNamespace`] that includes the generic parameters.
    ///
    /// Includes this [`ExtraNamespace`] to the [`Config`] to enable the
    /// resolution of generic parameters.
    fn get_generic_parameter_namepsace<M: Model>(
        &self,
        global_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> ExtraNamespace<M>;
}

impl Ext for Table {
    fn get_generic_parameter_namepsace<M: Model>(
        &self,
        global_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> ExtraNamespace<M> {
        let mut extra_namespace = ExtraNamespace::default();

        for scope in self.scope_walker(global_id) {
            let scope = GlobalID::new(global_id.target_id, scope);
            let symbol_kind = *self.get::<SymbolKind>(scope).unwrap();

            if !symbol_kind.has_generic_parameters() {
                continue;
            }

            let Some(generic_parameter) =
                self.query::<GenericParameters>(scope).handle(handler)
            else {
                continue;
            };

            for (name, lt) in generic_parameter.lifetime_parameter_ids_by_name()
            {
                if let Entry::Vacant(entry) =
                    extra_namespace.lifetimes.entry(name.clone())
                {
                    entry.insert(Lifetime::Parameter(LifetimeParameterID {
                        parent: scope,
                        id: *lt,
                    }));
                }
            }

            for (name, ty) in generic_parameter.type_parameter_ids_by_name() {
                if let Entry::Vacant(entry) =
                    extra_namespace.types.entry(name.clone())
                {
                    entry.insert(Type::Parameter(TypeParameterID {
                        parent: scope,
                        id: *ty,
                    }));
                }
            }

            for (name, constant) in
                generic_parameter.constant_parameter_ids_by_name()
            {
                if let Entry::Vacant(entry) =
                    extra_namespace.constants.entry(name.clone())
                {
                    entry.insert(Constant::Parameter(ConstantParameterID {
                        parent: scope,
                        id: *constant,
                    }));
                }
            }
        }

        extra_namespace
    }
}
