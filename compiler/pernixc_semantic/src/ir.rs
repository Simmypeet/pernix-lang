//! Contains the definition of the intermediate representation of the program.
//!
//! ## Steps of Creating IR
//!
//! ### Binding
//!
//! The syntax tree is traversed and the Control Flow Graph (CFG) is built.
//! During this step, the type inference and type checking are performed.
//!
//! The model used for the type system in this step is
//! [`representation::binding::infer::Model`].
//!
//! ### Auto Move Analysis
//!
//! In the prior step, every load instruction is performed as copy. In this
//! step, the redundant copy instructions are removed and replaced with move
//! instructions.
//!
//! ### Alternate Drop Analysis
//!
//! The required drop instructions are inserted in the CFG. These drop
//! instructions are inserted in the alternative path when the value is moved
//! out.
//!
//! ### Well-formedness
//!
//! After the Binding step, when the full type information is available, the
//! IR is checked for well-formedness. This step includes checking for
//! where clauses predicates, type bounds, move/copy,and other
//! type-related checks.
//!
//! ### Borrow Checking
//!
//! The copy of IR is created and all the "erased" lifetimes are replaced with
//! the inference variables. The borrow checking is performed on this copy.

use std::{fmt::Debug, hash::Hash};

use self::representation::Representation;
use crate::semantic::model::{self, Model};

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod pattern;
pub mod register;
pub mod representation;
pub mod scope;

/// The model to used to generate the IR.
pub trait State {
    /// The model to use for the type system.
    type Model: Model;
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = model::Default;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = model::Default;
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, derive_more::Deref)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}
