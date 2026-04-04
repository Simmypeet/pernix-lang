//! Defines the intermediate representation of a function.

use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_target::Global;
use pernixc_type_system::{
    environment::Environment as TyEnvironment, normalizer::Normalizer,
};
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    IRWithContext,
    capture::{Captures, CapturesMap},
    handling_scope::{HandlingScope, HandlingScopes, OperationHandlerID},
    ir::{IR, IRMap},
    resolution_visitor::{
        Abort, MutableResolutionVisitable, MutableResolutionVisitor,
    },
    value::{SimpleIRContext, ValueEnvironment},
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
    pub fn get_captures(&self, id: ID<Captures>) -> &Captures {
        &self.captures_map[id]
    }

    /// Accepts a visitor and invoke it on the closure parameters and captures
    /// of all IRs in the function.
    pub async fn accept_visitor_for_captures_and_handling_scopes<
        T: MutableResolutionVisitor,
    >(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.captures_map.accept_mut(visitor).await?;
        self.handling_scopes.accept_mut(visitor).await
    }

    pub async fn accept_visitor_for_ir<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.ir_map.accept_mut(visitor).await
    }
}

impl std::ops::Index<ID<IRWithContext>> for FunctionIR {
    type Output = IRWithContext;

    fn index(&self, index: ID<IRWithContext>) -> &Self::Output {
        &self.ir_map[index]
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
    operation_handler_id: OperationHandlerID,
    captures_id: ID<Captures>,
}

impl OperationHandlerContext {
    /// Creates a new operation handler context.
    #[must_use]
    pub const fn new(
        operation_handler_id: OperationHandlerID,
        captures_id: ID<Captures>,
    ) -> Self {
        Self { operation_handler_id, captures_id }
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

    /// The handling scope ID of the `do` block.
    #[get_copy = "pub"]
    handling_scope_id: ID<HandlingScope>,
}

impl DoContext {
    /// Creates a new `do` context.
    #[must_use]
    pub const fn new(
        captures_id: ID<Captures>,
        handling_scope_id: ID<HandlingScope>,
    ) -> Self {
        Self { captures_id, handling_scope_id }
    }
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
    pub const fn new_do_context(
        captures_id: ID<Captures>,
        handling_scope_id: ID<HandlingScope>,
    ) -> Self {
        Self::Do(DoContext::new(captures_id, handling_scope_id))
    }

    /// Creates an operation handler IR context.
    #[must_use]
    pub const fn new_operation_handler_context(
        operation_handler_id: OperationHandlerID,
        captures_id: ID<Captures>,
    ) -> Self {
        Self::OperationHandler(OperationHandlerContext::new(
            operation_handler_id,
            captures_id,
        ))
    }

    #[must_use]
    pub const fn to_simple_ir_context(&self) -> SimpleIRContext {
        match self {
            Self::Root => SimpleIRContext::Root,
            Self::OperationHandler(operation_handler_context) => {
                SimpleIRContext::OperationHandler(
                    operation_handler_context.operation_handler_id,
                )
            }
            Self::Do(do_context) => {
                SimpleIRContext::Do(do_context.handling_scope_id)
            }
        }
    }
}

impl FunctionIR {
    /// Creates a new function IR.
    #[must_use]
    pub const fn new(
        handling_scopes: HandlingScopes,
        ir_map: IRMap,
        captures_map: CapturesMap,
        root_ir_id: ID<IRWithContext>,
    ) -> Self {
        Self { handling_scopes, ir_map, captures_map, root_ir_id }
    }

    /// Gets the root IR representing the main body of the function.
    #[must_use]
    pub fn root_ir(&self) -> &IR { self.ir_map[self.root_ir_id].ir() }

    /// Retrieves the ID of the root IR representing the main body of the
    /// function.
    #[must_use]
    pub const fn root_ir_id(&self) -> ID<IRWithContext> { self.root_ir_id }

    /// Gets a mutable reference to the IR with the given ID.
    #[must_use]
    pub fn get_ir_mut(&mut self, id: ID<IRWithContext>) -> &mut IRWithContext {
        &mut self.ir_map[id]
    }

    #[must_use]
    pub fn create_value_environment_from_ir_id<'s, N: Normalizer>(
        &'s self,
        ir_id: ID<IRWithContext>,
        ty_environment: &'s TyEnvironment<'s, N>,
    ) -> ValueEnvironment<'s, N> {
        let context = self.ir_map[ir_id].context();
        let simple_ir_context = context.to_simple_ir_context();

        match context {
            IRContext::Root => ValueEnvironment::builder()
                .type_environment(ty_environment)
                .handling_scopes(&self.handling_scopes)
                .ir_context(simple_ir_context)
                .build(),

            IRContext::OperationHandler(operation_handler_context) => {
                let captures =
                    self.get_captures(operation_handler_context.captures_id);

                ValueEnvironment::builder()
                    .type_environment(ty_environment)
                    .captures(captures)
                    .ir_context(simple_ir_context)
                    .handling_scopes(&self.handling_scopes)
                    .build()
            }

            IRContext::Do(do_context) => {
                let captures = self.get_captures(do_context.captures_id());

                ValueEnvironment::builder()
                    .type_environment(ty_environment)
                    .captures(captures)
                    .handling_scopes(&self.handling_scopes)
                    .ir_context(simple_ir_context)
                    .build()
            }
        }
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
    ) -> impl Iterator<
        Item = (
            ID<IRWithContext>,
            &'s IRWithContext,
            crate::value::ValueEnvironment<'s, N>,
        ),
    > + 's {
        self.ir_map.ir_with_contexts().map(move |(ir_id, ir_with_context)| {
            let simple_ir_context =
                ir_with_context.context().to_simple_ir_context();

            let environment = match ir_with_context.context() {
                IRContext::Root => ValueEnvironment::builder()
                    .type_environment(ty_environment)
                    .handling_scopes(&self.handling_scopes)
                    .ir_context(simple_ir_context)
                    .build(),

                IRContext::OperationHandler(operation_handler_context) => {
                    let captures = self
                        .get_captures(operation_handler_context.captures_id);

                    ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .captures(captures)
                        .ir_context(simple_ir_context)
                        .handling_scopes(&self.handling_scopes)
                        .build()
                }

                IRContext::Do(do_context) => {
                    let captures = self.get_captures(do_context.captures_id());

                    ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .captures(captures)
                        .handling_scopes(&self.handling_scopes)
                        .ir_context(simple_ir_context)
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
    ) -> impl Iterator<
        Item = (
            ID<IRWithContext>,
            &'s mut IRWithContext,
            crate::value::ValueEnvironment<'s, N>,
        ),
    > + 's {
        let captures_map = &self.captures_map;
        let handling_scopes = &self.handling_scopes;

        self.ir_map.ir_with_contexts_mut().map(
            move |(ir_id, ir_with_context)| {
                let simple_ir_context =
                    ir_with_context.context().to_simple_ir_context();

                let environment = match ir_with_context.context() {
                    IRContext::Root => ValueEnvironment::builder()
                        .type_environment(ty_environment)
                        .handling_scopes(handling_scopes)
                        .ir_context(simple_ir_context)
                        .build(),

                    IRContext::OperationHandler(operation_handler_context) => {
                        let captures = &captures_map
                            [operation_handler_context.captures_id];

                        ValueEnvironment::builder()
                            .type_environment(ty_environment)
                            .captures(captures)
                            .ir_context(simple_ir_context)
                            .handling_scopes(handling_scopes)
                            .build()
                    }

                    IRContext::Do(do_context) => {
                        let captures = &captures_map[do_context.captures_id()];

                        ValueEnvironment::builder()
                            .type_environment(ty_environment)
                            .captures(captures)
                            .ir_context(simple_ir_context)
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
    pub function_id: Global<pernixc_symbol::SymbolID>,
}
