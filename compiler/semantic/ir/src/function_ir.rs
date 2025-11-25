//! Defines the intermediate representation of a function.

use std::sync::Arc;

use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    IRWithContext,
    capture::{Captures, CapturesMap},
    closure_parameters::{ClosureParameters, ClosureParametersMap},
    handling_scope::HandlingScopes,
    ir::{IR, IRMap},
};

/// An intermediate representation of a function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Value,
)]
#[id(Global<pernixc_symbol::ID>)]
#[value(Arc<FunctionIR>)]
#[extend(method(get_ir))]
pub struct FunctionIR {
    /// The collection of all handler groups defined in the function body.
    handler_groups: HandlingScopes,

    /// The collection of all IRs defined in the function.
    ir_map: IRMap,

    /// The collection of all closure parameters defined in the function.
    closure_parameters_map: ClosureParametersMap,

    /// The collection of all captures defined in the function.
    captures_map: CapturesMap,

    /// The ID of the root IR representing the main body of the function.
    ///
    /// Used to index into [`Self::ir_map`].
    root_ir_id: ID<IRWithContext>,
}

impl FunctionIR {
    /// Returns an iterator over all IRs with their IDs.
    #[must_use]
    pub fn ir_with_contexts(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<IRWithContext>, &'_ IRWithContext)> + '_
    {
        self.ir_map.ir_with_contexts()
    }

    /// Gets the captures with the given ID.
    #[must_use]
    pub fn get_capture(&self, id: ID<Captures>) -> &Captures {
        &self.captures_map[id]
    }

    /// Gets the closure parameters with the given ID.
    #[must_use]
    pub fn get_closure_parameters(
        &self,
        id: ID<ClosureParameters>,
    ) -> &ClosureParameters {
        &self.closure_parameters_map[id]
    }
}

/// The context specifying that the IR is used as an operation handler.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    CopyGetters,
)]
pub struct OperationHandlerContext {
    /// The ID of the closure parameters that the operation handler uses.
    #[get_copy = "pub"]
    closure_parameters_id: ID<ClosureParameters>,

    /// The ID of the captures that the operation handler uses.
    #[get_copy = "pub"]
    captures_id: ID<Captures>,
}

impl OperationHandlerContext {
    /// Creates a new operation handler context.
    #[must_use]
    pub const fn new(
        closure_parameters_id: ID<ClosureParameters>,
        captures_id: ID<Captures>,
    ) -> Self {
        Self { closure_parameters_id, captures_id }
    }
}

/// The context specifying that the IR is used in a `do` block.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    CopyGetters,
)]
pub struct DoContext {
    /// The ID of the captures that the `do` block uses.
    #[get_copy = "pub"]
    captures_id: ID<Captures>,
}

impl DoContext {
    /// Creates a new `do` context.
    #[must_use]
    pub const fn new(captures_id: ID<Captures>) -> Self { Self { captures_id } }
}

/// The context in which an IR is used.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
#[allow(missing_docs)]
pub enum IRContext {
    Root,
    OperationHandler(OperationHandlerContext),
    Do(DoContext),
}

impl IRContext {
    /// Creates a do IR context.
    #[must_use]
    pub const fn new_do_context(captures_id: ID<Captures>) -> Self {
        Self::Do(DoContext::new(captures_id))
    }

    /// Creates an operation handler IR context.
    #[must_use]
    pub const fn new_operation_handler_context(
        closure_parameters_id: ID<ClosureParameters>,
        captures_id: ID<Captures>,
    ) -> Self {
        Self::OperationHandler(OperationHandlerContext::new(
            closure_parameters_id,
            captures_id,
        ))
    }
}

impl FunctionIR {
    /// Creates a new function IR.
    #[must_use]
    pub const fn new(
        handler_groups: HandlingScopes,
        ir_map: IRMap,
        closure_parameters_map: ClosureParametersMap,
        captures_map: CapturesMap,
        root_ir_id: ID<IRWithContext>,
    ) -> Self {
        Self {
            handler_groups,
            ir_map,
            closure_parameters_map,
            captures_map,
            root_ir_id,
        }
    }

    /// Gets the root IR representing the main body of the function.
    #[must_use]
    pub fn root_ir(&self) -> &IR { self.ir_map[self.root_ir_id].ir() }

    /// Gets a mutable reference to the IR with the given ID.
    #[must_use]
    pub fn get_ir_mut(&mut self, id: ID<IRWithContext>) -> &mut IRWithContext {
        &mut self.ir_map[id]
    }
}
