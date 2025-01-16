//! Contains the definition of [`Occurrences`].

use std::ops::DerefMut;

use pernixc_handler::Handler;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_source_file::Span;
use pernixc_syntax::syntax_tree;
use pernixc_table::{
    component::{Input, InputMut},
    diagnostic::Diagnostic,
    GlobalID, Table,
};
use pernixc_term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, Default,
};

/// A structure containing the list of all resolution resolved so far in the
/// finalizing process.
///
/// This is primarily used for well-formedness checking of all instantiations
/// made in the program.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub(crate) struct Occurrences {
    pub types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
    pub lifetimes: Vec<(Lifetime<Default>, syntax_tree::Lifetime)>,
    pub constants: Vec<(Constant<Default>, syntax_tree::Constant)>,

    pub resolutions: Vec<(Resolution<Default>, Span)>,

    pub unpacked_types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
    pub unpacked_constants:
        Vec<(Constant<Default>, syntax_tree::expression::Expression)>,

    pub constant_types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
}

impl Input for Occurrences {}
impl InputMut for Occurrences {}

/// An observer object for the [`pernixc_resolution::Observer`] that will
/// add the resolution to the [`Occurrences`] component to the referring site
/// symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Observer;

impl Observer {
    fn ensure_occurrences(
        table: &Table,
        referring_site: GlobalID,
    ) -> Option<impl DerefMut<Target = Occurrences> + '_> {
        if !table.has::<Occurrences>(referring_site) {
            let _ = table.add_component(referring_site, Occurrences::default());
        }

        table.get_mut::<Occurrences>(referring_site).ok()
    }
}

impl pernixc_resolution::Observer<Default> for Observer {
    fn on_resolution_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        resolution: &Resolution<Default>,
        span: &Span,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences.resolutions.push((resolution.clone(), span.clone()));
    }

    fn on_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences.types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_lifetime_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        lifetime: &Lifetime<Default>,
        syntax_tree: &syntax_tree::Lifetime,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences.lifetimes.push((*lifetime, syntax_tree.clone()));
    }

    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<Default>,
        syntax_tree: &syntax_tree::Constant,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences.constants.push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences.unpacked_types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<Default>,
        syntax_tree: &syntax_tree::expression::Expression,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let Some(mut occurrences) =
            Self::ensure_occurrences(table, referring_site)
        else {
            return;
        };

        occurrences
            .unpacked_constants
            .push((constant.clone(), syntax_tree.clone()));
    }
}
