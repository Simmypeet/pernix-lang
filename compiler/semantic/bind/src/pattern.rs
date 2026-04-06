//! Contains the logic for processing patterns during the binding phase.

pub mod bind;
pub mod insert_name_binding;
pub mod matching;
pub mod path;

pub use matching::{
    MatchScrutineeBindingResult, replace_refutable_in_tuple_pack,
};
