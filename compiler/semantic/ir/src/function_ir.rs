//! Defines the intermediate representation of a function.

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_target::Global;
use pernixc_type_system::normalizer::Normalizer;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    IRWithContext,
    capture::{Captures, CapturesMap},
    closure_parameters::{ClosureParameters, ClosureParametersMap},
    handling_scope::HandlingScopes,
    ir::{IR, IRMap},
    value::Environment as ValueEnvironment,
};

/// An intermediate representation of a function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Encode,
    Decode,
    StableHash,
    Getters,
    Identifiable,
)]
pub struct FunctionIR {
    /// The collection of all handler groups defined in the function body.
    #[get = "pub"]
    handling_scopes: HandlingScopes,

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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
    Encode,
    Decode,
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
            handling_scopes: handler_groups,
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

impl FunctionIR {
    /// Iterates over all IRs along with their [`ValueEnvironment`]
    /// corresponding to each IR's context.
    pub fn ir_with_value_environments<'s, N: Normalizer>(
        &'s self,
        ty_environment: &'s pernixc_type_system::environment::Environment<
            's,
            N,
        >,
        current_site: Global<pernixc_symbol::ID>,
    ) -> impl Iterator<
        Item = (
            ID<IRWithContext>,
            &'s IRWithContext,
            crate::value::Environment<'s, N>,
        ),
    > + 's {
        self.ir_map.ir_with_contexts().map(move |(ir_id, ir_with_context)| {
            let environment = match ir_with_context.context() {
                IRContext::Root => ValueEnvironment::builder()
                    .type_environment(ty_environment)
                    .current_site(current_site)
                    .handling_scopes(&self.handling_scopes)
                    .build(),

                IRContext::OperationHandler(operation_handler_context) => {
                    let captures = self
                        .get_capture(operation_handler_context.captures_id());
                    let closure_parameters = self.get_closure_parameters(
                        operation_handler_context.closure_parameters_id(),
                    );

                    ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .current_site(current_site)
                        .captures(captures)
                        .closure_parameters(closure_parameters)
                        .handling_scopes(&self.handling_scopes)
                        .build()
                }

                IRContext::Do(do_context) => {
                    let captures = self.get_capture(do_context.captures_id());

                    ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .current_site(current_site)
                        .captures(captures)
                        .handling_scopes(&self.handling_scopes)
                        .build()
                }
            };

            (ir_id, ir_with_context, environment)
        })
    }

    /// Iterates over all IRs along with their [`ValueEnvironment`]
    /// corresponding to each IR's context, mutably.
    pub fn ir_with_value_environments_mut<'s, N: Normalizer>(
        &'s mut self,
        ty_environment: &'s pernixc_type_system::environment::Environment<
            's,
            N,
        >,
        current_site: Global<pernixc_symbol::ID>,
    ) -> impl Iterator<
        Item = (
            ID<IRWithContext>,
            &'s mut IRWithContext,
            crate::value::Environment<'s, N>,
        ),
    > + 's {
        let captures_map = &self.captures_map;
        let closure_parameters_map = &self.closure_parameters_map;
        let handling_scopes = &self.handling_scopes;

        self.ir_map.ir_with_contexts_mut().map(
            move |(ir_id, ir_with_context)| {
                let environment = match ir_with_context.context() {
                    IRContext::Root => ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .current_site(current_site)
                        .handling_scopes(handling_scopes)
                        .build(),

                    IRContext::OperationHandler(operation_handler_context) => {
                        let captures = &captures_map
                            [operation_handler_context.captures_id()];

                        let closure_parameters = &closure_parameters_map
                            [operation_handler_context.closure_parameters_id()];

                        ValueEnvironment::builder()
                            .type_environment(ty_environment)
                            .current_site(current_site)
                            .captures(captures)
                            .closure_parameters(closure_parameters)
                            .handling_scopes(handling_scopes)
                            .build()
                    }

                    IRContext::Do(do_context) => {
                        let captures = &captures_map[do_context.captures_id()];

                        ValueEnvironment::builder()
                            .type_environment(ty_environment)
                            .current_site(current_site)
                            .captures(captures)
                            .handling_scopes(handling_scopes)
                            .build()
                    }
                };

                (ir_id, ir_with_context, environment)
            },
        )
    }
}

/// Query key for retrieving [`FunctionIR`].
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
    Encode,
    Decode,
    qbice::Query,
)]
#[value(Interned<FunctionIR>)]
#[extend(name = get_function_ir, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub function_id: Global<pernixc_symbol::ID>,
}
