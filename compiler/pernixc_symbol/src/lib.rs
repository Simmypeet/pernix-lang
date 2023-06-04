//! This crate implements the symbol resolution / analysis logic of the compiler.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    missing_docs,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

use std::{collections::HashMap, sync::Arc};

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use pernixc_system::create_symbol;

pub mod ty;

create_symbol! {
    /// Reprsents a type parameter symbol.
    #[derive(Debug, Clone)]
    pub struct TypeParameter {
        /// The name of the type parameter.
        pub name: String,
    }
}

create_symbol! {
    /// Represents a lifetime parameter symbol.
    #[derive(Debug, Clone)]
    pub struct LifetimeParameter {
        /// The name of the lifetime parameter.
        pub name: String,
    }
}

/// Is an enumeration of all possible parameter IDs that can be used in a generic symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericParameterID {
    Type(TypeParameterID),
    Lifetime(LifetimeParameterID),
}

/// Is a list of generic parameters defined in a generic symbol.
#[derive(Debug, Clone)]
pub struct GenericParameters {
    /// The list of declared generic parameter IDs (in order of declaration).
    pub generic_parameter_declaration_ids: Vec<GenericParameterID>,

    /// The type parameters of the generic symbol.
    pub type_parameter_ids_by_name: HashMap<String, TypeParameterID>,

    /// The lifetime parameters of the generic symbol.
    pub lifetime_parameter_ids_by_name: HashMap<String, LifetimeParameterID>,

    /// The generic symbol ID of the generic symbol.
    pub generic_symbol_id: GenericSymbolID,

    /// The generic parameters of the parent generic symbol.
    pub parent: Option<Arc<GenericParameters>>,
}

create_symbol! {
    /// Represents a struct symbol.
    #[derive(Debug, Clone)]
    pub struct Struct {
        /// Contains the list of accessible generic parameters.
        pub generic_parameters: Arc<GenericParameters>,
    }
}

create_symbol! {
    /// Represents a struct symbol with generic parameters.
    #[derive(Debug, Clone, Deref, DerefMut)]
    pub struct GenericStruct(pub Struct);
}

/// Is an enumeration of all possible symbol IDs that have generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum GenericSymbolID {
    Struct(GenericStructID),
}
