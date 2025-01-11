//! Contains the definition of [`Occurrences`].

use getset::Getters;
use pernixc_syntax::syntax_tree;

use super::{Input, InputMut};
use crate::type_system::{model::Default, term};

/// A **local-input** component containing the list of all resolution resolved
/// so far in the target compilation process.
///
/// This is primarily used for well-formedness checking of all instantiations
/// made in the program.
#[derive(Debug, Default, Getters)]
pub struct Occurrences {
    /// The list of all types that are resolved so far.
    #[get = "pub"]
    types: Vec<(term::r#type::Type<Default>, syntax_tree::r#type::Type)>,
    /// The list of all lifetimes that are resolved so far.
    #[get = "pub"]
    lifetimes: Vec<(term::lifetime::Lifetime<Default>, syntax_tree::Lifetime)>,
    /// The list of all constants that are resolved so far.
    #[get = "pub"]
    constants: Vec<(term::constant::Constant<Default>, syntax_tree::Constant)>,
}

impl Input for Occurrences {}
impl InputMut for Occurrences {}
