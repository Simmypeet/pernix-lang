//! Contains all the definition of errors that can be emitted by the semantic analyzer.

use crate::{
    arena::ID,
    symbol::{Module, ModuleMemberID},
};

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemDuplication {
    /// The ID of the existing symbol.
    pub existing_symbol: ModuleMemberID,

    /// The ID of the new symbol.
    pub new_symbol: ModuleMemberID,

    /// The scope in which the duplication occurred.
    pub scope: ID<Module>,
}

/// An enumeration containing all kinds of errors that can be emitted by the semantic analyzer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    GlobalSymbolDuplication(ItemDuplication),
}
