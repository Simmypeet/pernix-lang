//! Contains the builder for the generic parameters.

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
        syntax_tree as syntax_tree_component, Accessibility, Derived,
        HierarchyRelationship, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalAccessibility, GlobalID, MemberID, Table,
};
use pernixc_term::{
    accessibility::Ext as _,
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
    diagnostic::PrivateEntityLeakedToPublicInterface,
    occurrences::{self, Occurrences},
};

pub mod diagnostic;

impl query::Builder<GenericParameters> for Builder {
    #[allow(clippy::too_many_lines)]
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<GenericParameters> {
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
        let mut extra_name_space = ExtraNamespace::default();

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

        for constant_parameter_syn in constant_parameter_syns {
            let constant_parameter = {
                // the type used for the constant parameter
                let constant_type = match table.resolve_type(
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
                ) {
                    Ok(ty) => ty,

                    Err(
                        pernixc_resolution::term::Error::InvalidReferringSiteID
                        | pernixc_resolution::term::Error::Query(
                            query::Error::NoBuilderFound
                            | query::Error::SymbolNotFoundOrInvalidComponent,
                        ),
                    ) => unreachable!(),

                    Err(pernixc_resolution::term::Error::Query(
                        query::Error::CyclicDependency(error),
                    )) => {
                        handler.receive(Box::new(error));
                        Type::Error(pernixc_term::Error)
                    }
                };

                let ty_accessibility = table
                    .get_type_accessibility(&constant_type)
                    .expect("should be valid");
                let symbol_accessibility = match table
                    .get_accessibility(global_id)
                    .unwrap()
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(global_id.target_id, id),
                    ),
                };

                // no private type in public interface
                let private_entity_leaked =
                    match (ty_accessibility, symbol_accessibility) {
                        (
                            GlobalAccessibility::Public,
                            GlobalAccessibility::Public
                            | GlobalAccessibility::Scoped(_),
                        ) => false,

                        (
                            GlobalAccessibility::Scoped(_),
                            GlobalAccessibility::Public,
                        ) => true,

                        (
                            GlobalAccessibility::Scoped(ty),
                            GlobalAccessibility::Scoped(sym),
                        ) => {
                            assert_eq!(ty.target_id, sym.target_id);

                            table.symbol_hierarchy_relationship(
                                ty.target_id,
                                ty.id,
                                sym.id,
                            ) == HierarchyRelationship::Child
                        }
                    };

                if private_entity_leaked {
                    handler.receive(Box::new(
                        PrivateEntityLeakedToPublicInterface {
                            entity: constant_type.clone(),
                            leaked_span: constant_parameter_syn.1.span(),
                            public_interface_id: global_id,
                            entity_overall_accessibility: ty_accessibility,
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

        Some(generic_parameters)
    }
}
