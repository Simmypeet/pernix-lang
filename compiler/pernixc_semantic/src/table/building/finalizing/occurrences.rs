//! Contains the definition of [`Occurrences`].

use getset::Getters;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::finalizer::build_preset::BuildPreset;
use crate::{
    error,
    semantic::{model::Default, predicate, term},
    symbol::GlobalID,
    table::{
        building::finalizing::Finalizer, resolution::Observer, State, Table,
    },
};

/// A structure containing the list of all resolution resolved so far in the
/// building process.
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
    constants: Vec<(
        term::constant::Constant<Default>,
        syntax_tree::expression::Expression,
    )>,

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

    #[get = "pub"]
    trait_predicates:
        Vec<(predicate::Trait<Default>, syntax_tree::QualifiedIdentifier)>,
}

impl Occurrences {
    /// Builds all the occurrences (dependencies) to completion.
    pub fn build_all_occurrences_to<B: BuildPreset>(
        &self,
        table: &Table<Finalizer>,
        dependant: GlobalID,
        cyclic_dependency_as_error: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for id in
            self.get_all_global_id_occurrences(table).into_iter().flatten()
        {
            let _ = table.build_preset::<B>(
                id,
                Some(dependant),
                cyclic_dependency_as_error,
                handler,
            );
        }
    }

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

        for (predicate, _) in &self.trait_predicates {
            global_ids.push(predicate.id.into());
            global_ids.extend(
                predicate
                    .generic_arguments
                    .get_global_id_dependencies(table)?,
            );
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

    /// Add a trait predicate.
    pub fn add_trait_predicate(
        &mut self,
        predicate: predicate::Trait<Default>,
        syntax_tree: syntax_tree::QualifiedIdentifier,
    ) {
        self.trait_predicates.push((predicate, syntax_tree));
    }
}

impl Observer<Default> for Occurrences {
    fn on_type_resolved(
        &mut self,
        ty: &term::r#type::Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) {
        self.types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_lifetime_resolved(
        &mut self,
        lifetime: &term::lifetime::Lifetime<Default>,
        syntax_tree: &syntax_tree::Lifetime,
    ) {
        self.lifetimes.push((*lifetime, syntax_tree.clone()));
    }

    fn on_constant_arguments_resolved(
        &mut self,
        constant: &term::constant::Constant<Default>,
        syntax_tree: &syntax_tree::expression::Expression,
    ) {
        self.constants.push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        ty: &term::r#type::Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) {
        self.unpacked_types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        constant: &term::constant::Constant<Default>,
        syntax_tree: &syntax_tree::expression::Expression,
    ) {
        self.unpacked_constants.push((constant.clone(), syntax_tree.clone()));
    }
}
