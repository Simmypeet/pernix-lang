//! Contains the finalization logic for each kinds of symbols.

use std::fmt::Debug;

use pernixc_base::diagnostic::Handler;

use super::finalizer::Finalizer;
use crate::{
    arena::ID,
    error,
    symbol::table::{representation::RwLockContainer, Building, Table},
};

pub(in crate::symbol::table::representation::building) mod adt_implementation;
pub(in crate::symbol::table::representation::building) mod adt_implementation_function;
pub(in crate::symbol::table::representation::building) mod constant;
pub(in crate::symbol::table::representation::building) mod r#enum;
pub(in crate::symbol::table::representation::building) mod function;
pub(in crate::symbol::table::representation::building) mod negative_trait_implementation;
pub(in crate::symbol::table::representation::building) mod positive_trait_implementation;
pub(in crate::symbol::table::representation::building) mod r#struct;
pub(in crate::symbol::table::representation::building) mod r#trait;
pub(in crate::symbol::table::representation::building) mod trait_constant;
pub(in crate::symbol::table::representation::building) mod trait_function;
pub(in crate::symbol::table::representation::building) mod trait_implementation_constant;
pub(in crate::symbol::table::representation::building) mod trait_implementation_function;
pub(in crate::symbol::table::representation::building) mod trait_implementation_type;
pub(in crate::symbol::table::representation::building) mod trait_type;
pub(in crate::symbol::table::representation::building) mod r#type;

/// The type used to determine the state of the finalization.
pub type StateFlag = usize;

/// A trait for finalizing a symbol.
pub trait Finalize {
    type SyntaxTree: Debug;
    const FINAL_STATE: usize;
    type Data: Debug + Send + Sync + Default;

    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    );
}
