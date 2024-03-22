use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
};

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{finalize::Occurrences, Finalizer};
use crate::{
    arena::{Arena, ID},
    error::{self, DuplicatedGenericParameter, MisOrderedGenericParameter},
    symbol::{
        ConstantParameter, Generic, GenericID, GenericKind, GenericParameter,
        GlobalID, LifetimeParameter, MemberID, TypeParameter, Variance,
    },
    table::{resolution, Element, Table},
};

impl Table<Finalizer> {
    /// Creates a generic parameter for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    pub(in crate::table::building) fn create_generic_parameters<
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

        let (lifetimes, lifetime_order, lifetime_parameter_ids_by_name) = self
            .create_generic_parameters_kind(
                generic_id.into(),
                lifetime_parameter_syns.into_iter(),
                occurrences,
                |x| x.identifier(),
                |_, syntax_tree, generic_id, _, _| LifetimeParameter {
                    name: Some(syntax_tree.identifier().span.str().to_string()),
                    parent_generic_id: generic_id,
                    span: Some(syntax_tree.identifier().span.clone()),
                    variance: Variance::Covariant,
                },
                handler,
            );
        let (types, type_order, type_parameter_ids_by_name) = self
            .create_generic_parameters_kind(
                generic_id.into(),
                type_parameter_syns.into_iter(),
                occurrences,
                |x| x,
                |_, syntax_tree, generic_id, _, _| TypeParameter {
                    name: syntax_tree.span.str().to_string(),
                    parent_generic_id: generic_id,
                    span: Some(syntax_tree.span.clone()),
                    variance: Variance::Covariant,
                },
                handler,
            );

        let mut generic_symbol_write =
            T::get_arena(self).get(generic_id).unwrap().write();
        let generic_parameters =
            &mut generic_symbol_write.generic_declaration_mut().parameters;

        // assign the generic parameters to the generic symbol
        generic_parameters.lifetimes = lifetimes;
        generic_parameters.types = types;
        generic_parameters.lifetime_order = lifetime_order;
        generic_parameters.type_order = type_order;
        generic_parameters.lifetime_parameter_ids_by_name =
            lifetime_parameter_ids_by_name;
        generic_parameters.type_parameter_ids_by_name =
            type_parameter_ids_by_name;

        drop(generic_symbol_write);

        let (constants, constant_order, constant_parameter_ids_by_name) = self
            .create_generic_parameters_kind(
                generic_id.into(),
                constant_parameter_syns.into_iter(),
                occurrences,
                |x| x.0,
                |this, (name, ty), generic_id, occurrences, handler| {
                    let constant_type = this
                        .resolve_type(
                            ty,
                            generic_id.into(),
                            resolution::Config {
                                ellided_lifetime_provider: None,
                                ellided_type_provider: None,
                                ellided_constant_provider: None,
                                observer: Some(occurrences),
                                higher_ranked_liftimes: None,
                            },
                            handler,
                        )
                        .unwrap_or_default();

                    let ty_accessibility = this
                        .get_type_overall_accessibility(&constant_type)
                        .expect("should be valid");

                    // no private type in public interface
                    if this
                        .get_accessibility(generic_id.into())
                        .expect("should be a valid id")
                        > ty_accessibility
                    {
                        handler.receive(Box::new(
                            error::PrivateEntityLeakedToPublicInterface {
                                entity: constant_type.clone(),
                                leaked_span: ty.span(),
                                public_interface_id: generic_id.into(),
                                entity_overall_accessibility: ty_accessibility,
                            },
                        ));
                    }

                    // add the constant type to the occurrences
                    occurrences.add_constant_type(
                        constant_type.clone(),
                        (*ty).clone(),
                    );

                    ConstantParameter {
                        name: name.span.str().to_string(),
                        parent_generic_id: generic_id,
                        r#type: constant_type,
                        span: Some(name.span.clone()),
                    }
                },
                handler,
            );

        let mut generic_symbol_write =
            T::get_arena(self).get(generic_id).unwrap().write();
        let generic_parameters =
            &mut generic_symbol_write.generic_declaration_mut().parameters;

        generic_parameters.constants = constants;
        generic_parameters.constant_order = constant_order;
        generic_parameters.constant_parameter_ids_by_name =
            constant_parameter_ids_by_name;

        // TODO: create default parameters
    }

    #[allow(clippy::type_complexity)]
    fn create_generic_parameters_kind<
        'a,
        T: GenericParameter + Debug,
        S: 'a,
    >(
        &self,
        referring_site: GenericID,
        syntax_trees: impl Iterator<Item = S>,
        occurrences: &mut Occurrences,
        identifier_func: impl Fn(&S) -> &Identifier,
        create_func: impl Fn(
            &Self,
            &S,
            GenericID,
            &mut Occurrences,
            &dyn Handler<Box<dyn error::Error>>,
        ) -> T,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> (Arena<T>, Vec<ID<T>>, HashMap<String, ID<T>>) {
        let mut arena = Arena::default();
        let mut order = Vec::new();
        let mut map = HashMap::new();

        for syntax_tree in syntax_trees {
            match map
                .entry(identifier_func(&syntax_tree).span.str().to_string())
            {
                Entry::Occupied(entry) => {
                    handler.receive(Box::new(DuplicatedGenericParameter {
                        existing_generic_parameter_id: MemberID {
                            parent: referring_site,
                            id: *entry.get(),
                        },
                        duplicating_generic_parameter_span: identifier_func(
                            &syntax_tree,
                        )
                        .span
                        .clone(),
                    }));
                }
                Entry::Vacant(entry) => {
                    let id = arena.insert(create_func(
                        self,
                        &syntax_tree,
                        referring_site,
                        occurrences,
                        handler,
                    ));

                    entry.insert(id);
                    order.push(id);
                }
            }
        }

        (arena, order, map)
    }
}
