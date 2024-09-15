//! Contains the definition of [`Occurrences`].

use getset::Getters;
use pernixc_base::{handler::Handler, source_file::Span};
use pernixc_syntax::syntax_tree;

use crate::{
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, RwLockContainer,
            },
            resolution::{Observer, Resolution},
            Building, State, Table,
        },
        GlobalID,
    },
    type_system::{model::Default, term},
};

/// A structure containing the list of all resolution resolved so far in the
/// finalizing process.
///
/// This is primarily used for well-formedness checking of all instantiations
/// made in the program.
#[derive(Debug, Default, Getters)]
pub struct Occurrences {
    #[get = "pub"]
    types: Vec<(term::r#type::Type<Default>, syntax_tree::r#type::Type)>,
    #[get = "pub"]
    lifetimes: Vec<(term::lifetime::Lifetime<Default>, syntax_tree::Lifetime)>,
    #[get = "pub"]
    constants: Vec<(term::constant::Constant<Default>, syntax_tree::Constant)>,

    #[get = "pub"]
    resolutions: Vec<(Resolution<Default>, Span)>,

    #[get = "pub"]
    unpacked_types:
        Vec<(term::r#type::Type<Default>, syntax_tree::r#type::Type)>,
    #[get = "pub"]
    unpacked_constants: Vec<(
        term::constant::Constant<Default>,
        syntax_tree::expression::Expression,
    )>,

    #[get = "pub"]
    constant_types:
        Vec<(term::r#type::Type<Default>, syntax_tree::r#type::Type)>,
}

impl Occurrences {
    // pub fn build_occurrences_to_definition(
    //     &self,
    //     table: &Table<impl State>,
    //     referring_site: GlobalID,
    //     handler: &dyn Handler<Box<dyn error::Error>>,
    // ) {
    //     let environment = Environment::new(
    //         table.get_active_premise(referring_site).unwrap(),
    //         table,
    //         &NO_OP,
    //     );

    //     for (resolution, _) in &self.resolutions {
    //         let _ = builder::build_for_definition_internal(
    //             &environment,
    //             resolution,
    //             Some(referring_site),
    //             false,
    //             handler,
    //         );
    //     }
    // }

    /// Get all the global IDs that has occurred so far.
    pub fn get_all_global_id_occurrences(
        &self,
        table: &Table<impl State>,
    ) -> Option<Vec<GlobalID>> {
        let mut global_ids = Vec::new();

        for (ty, _) in &self.types {
            global_ids.extend(ty.get_global_id_dependencies(table)?);
        }

        for (constant, _) in &self.constants {
            global_ids.extend(constant.get_global_id_dependencies(table)?);
        }

        for (ty, _) in &self.unpacked_types {
            global_ids.extend(ty.get_global_id_dependencies(table)?);
        }

        for (constant, _) in &self.unpacked_constants {
            global_ids.extend(constant.get_global_id_dependencies(table)?);
        }

        for (ty, _) in &self.constant_types {
            global_ids.extend(ty.get_global_id_dependencies(table)?);
        }

        for (resolution, _) in &self.resolutions {
            global_ids.push(resolution.global_id());
        }

        global_ids.sort_unstable();
        global_ids.dedup();

        Some(global_ids)
    }

    /// Add a type which appears as a type of constant generic parameter.
    pub fn add_constant_type(
        &mut self,
        ty: term::r#type::Type<Default>,
        syntax_tree: syntax_tree::r#type::Type,
    ) {
        self.constant_types.push((ty, syntax_tree));
    }
}

impl Observer<Building<RwLockContainer, Finalizer>, Default> for Occurrences {
    fn on_global_id_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: GlobalID,
        _: &Span,
    ) -> bool {
        true
    }

    fn on_resolution_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        resolution: &Resolution<Default>,
        generic_identifier: &Span,
    ) -> bool {
        self.resolutions.push((resolution.clone(), generic_identifier.clone()));
        true
    }

    fn on_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        ty: term::r#type::Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) -> Option<term::r#type::Type<Default>> {
        self.types.push((ty.clone(), syntax_tree.clone()));

        Some(ty)
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        lifetime: term::lifetime::Lifetime<Default>,
        syntax_tree: &syntax_tree::Lifetime,
    ) -> Option<term::lifetime::Lifetime<Default>> {
        self.lifetimes.push((lifetime.clone(), syntax_tree.clone()));

        Some(lifetime)
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        constant: &term::constant::Constant<Default>,
        syntax_tree: &syntax_tree::Constant,
    ) {
        self.constants.push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        ty: &term::r#type::Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) {
        self.unpacked_types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        constant: &term::constant::Constant<Default>,
        syntax_tree: &syntax_tree::expression::Expression,
    ) {
        self.unpacked_constants.push((constant.clone(), syntax_tree.clone()));
    }
}
