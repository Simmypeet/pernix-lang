//! Contains code related to well-formedness checking of each instantiation of
//! symbols and types.

use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_table::{diagnostic::Diagnostic, GlobalID};
use pernixc_term::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    predicate::Predicate, Default, Model,
};
use pernixc_type_system::{
    environment::Environment, normalizer, resolution::Implementation,
    OverflowError,
};

use crate::type_system::{
    diagnostic::{UndecidablePredicate, UnsatisfiedPredicate},
    ImplementationIsNotGeneralEnough,
};
