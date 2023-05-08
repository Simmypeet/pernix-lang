//! Contains the definition of [`Scope`] and [`Stack`]

use std::{borrow::Borrow, collections::HashMap};

use derive_more::Deref;
use getset::{CopyGetters, Getters};
use pernixc_system::create_symbol;

use super::IntermediateTypeID;
use crate::{
    cfg::BasicBlockID,
    hir::{value::Value, AllocaID},
};

/// Is a data struct representing the stack of scopes in the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack<T> {
    scopes: Vec<T>,
}

impl<T> Stack<T> {
    /// Creates a new empty stack of scopes.
    #[must_use]
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    /// Pushes a new [`Scope`] to the top of the stack.
    pub fn push(&mut self, scope: T) {
        self.scopes.push(scope);
    }

    /// Pops the top [`Scope`] from the stack and returns it.
    pub fn pop(&mut self) -> Option<T> {
        self.scopes.pop()
    }

    /// Returns a reference to the topmost [`Scope`] in the stack.
    #[must_use]
    pub fn top(&self) -> Option<&T> {
        self.scopes.last()
    }

    /// Returns a mutable reference to the topmost [`Scope`] in the stack.
    pub fn top_mut(&mut self) -> Option<&mut T> {
        self.scopes.last_mut()
    }

    /// Returns the number of scopes currently in the stack.
    #[must_use]
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    /// Returns `true` if the stack is empty.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    /// Searches for a value in the stack and returns a reference to it if it exists.
    ///
    /// The search is done from the top of the stack to the bottom.
    pub fn serach<'a, Q>(&'a self, key: &Q) -> Option<T::V>
    where
        T: Scope<'a>,
        Q: ?Sized + Eq + std::hash::Hash,
        T::K: std::borrow::Borrow<Q>,
    {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.search(key) {
                return Some(value);
            }
        }
        None
    }
}

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Is a trait the represents a scope that contains the mapping of keys to values.
pub trait Scope<'a> {
    /// The type of the keys in the scope.
    type K: Eq + std::hash::Hash;

    /// The type of the values in the scope.
    type V: 'a;

    /// Searches for a value in the scope and returns a reference to it if it exists.
    fn search<Q>(&'a self, key: &Q) -> Option<Self::V>
    where
        Q: ?Sized + Eq + std::hash::Hash,
        Self::K: std::borrow::Borrow<Q>;
}

/// Represents a [`Scope`] that contains mappings of variable name and its [`AllocaID`].
///
/// This struct is used for variable name resolution.
#[derive(Debug, Clone, PartialEq, Eq, Default, Deref, Getters)]
pub struct Locals {
    #[deref]
    values: HashMap<String, AllocaID>,

    /// The list of declared variables in the scope.
    #[get = "pub"]
    variable_declarations: Vec<AllocaID>,
}

impl Locals {
    /// Inserts a new variable name and its [`AllocaID`] into the scope.
    pub fn insert(&mut self, name: String, id: AllocaID) {
        self.values.insert(name, id);
        self.variable_declarations.push(id);
    }
}

impl<'a> Scope<'a> for Locals {
    type K = String;
    type V = AllocaID;

    fn search<Q>(&'a self, key: &Q) -> Option<Self::V>
    where
        Q: ?Sized + Eq + std::hash::Hash,
        Self::K: std::borrow::Borrow<Q>,
    {
        self.values.get(key).copied()
    }
}

/// Is an enum used to search a particular stack's scope by its name or ID.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NameOrIDKey<ID> {
    /// Search by its name.
    Name(String),

    /// Search by its ID.
    ID(ID),
}

create_symbol! {
    /// Contains the information about the [`Block`](pernixc_syntax::syntax_tree::expression::Block)
    /// expression that being pushed into the [`Stack`].
    #[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
    pub struct Block {
        /// The label name of the block if it has one.
        #[get = "pub"]
        pub(super) label: Option<String>,

        /// The [`BasicBlockID`] where the block starts.
        #[get_copy = "pub"]
        pub(super) start_basic_block_id: BasicBlockID,

        /// The [`BasicBlockID`] where the block ends.
        #[get_copy = "pub"]
        pub(super) end_basic_block_id: BasicBlockID,

        /// The [`IntermediateTypeID`] of the expression if it has one.
        #[get_copy = "pub"]
        pub(super) express_ty: Option<IntermediateTypeID>,

        /// A map of the incoming values of the block.
        #[get = "pub"]
        pub(super) incoming_values: HashMap<BasicBlockID, Value<IntermediateTypeID>>,

        /// The depth of the scope where the block is declared (starts from 0 for the block
        /// declared in the function's body level)
        #[get_copy = "pub"]
        pub(super) scope_depth: usize,
    }

}

/// Represents a pointer to a [`Block`] that will be pushed into the [`Stack`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters, CopyGetters)]
pub struct BlockPointer {
    /// The label name of the block if it has one.
    #[get = "pub"]
    pub(super) label: Option<String>,

    /// The block ID that this pointer points to.
    #[get_copy = "pub"]
    pub(super) block_id: BlockID,
}

impl<'a> Scope<'a> for BlockPointer {
    type K = String;
    type V = BlockID;

    fn search<Q>(&'a self, key: &Q) -> Option<Self::V>
    where
        Q: ?Sized + Eq + std::hash::Hash,
        Self::K: std::borrow::Borrow<Q>,
    {
        if let Some(label) = &self.label {
            if key == label.borrow() {
                return Some(self.block_id);
            }
        }
        None
    }
}
