//! Contains all the definitions related to the intermediate representation of
//! the function body.

use std::{fmt::Debug, hash::Hash};

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod model;
pub mod pattern;
pub mod scope;
pub mod value;

/// The model to used to generate the IR.
pub trait State {
    /// The model to use for the type system.
    type Model: pernixc_term::Model + Serialize + for<'de> Deserialize<'de>;
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = model::Model;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = model::Model;
}

/// An intermediate representation of the program.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}

pub use representation::Representation;
use serde::{Deserialize, Serialize};
pub use value::Value;

mod representation {
    use getset::Getters;
    use pernixc_arena::{Arena, Key, ID};
    use pernixc_table::{query::CyclicDependencyError, Table};
    use pernixc_term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, Model,
    };
    use serde::{Deserialize, Serialize};

    use crate::{
        alloca::Alloca, control_flow_graph::ControlFlowGraph, model::Transform,
        scope, value::register::Register,
    };

    /// Contains all the registers and allocas used in the program.
    #[derive(
        Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, Getters,
    )]
    pub struct Values<M: Model> {
        /// Contains all the registers used in the program.
        pub registers: Arena<Register<M>>,

        /// Contains all the allocas used in the program.
        pub allocas: Arena<Alloca<M>>,
    }

    /// An intermediate representation of the program.
    #[derive(
        Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, Getters,
    )]
    pub struct Representation<M: Model> {
        /// Contains the registers and allocas used in the program.
        pub values: Values<M>,

        /// The control flow graph of the program.
        pub control_flow_graph: ControlFlowGraph<M>,

        /// The tree of scopes in the program.
        pub scope_tree: scope::Tree,
    }

    impl<M: Model> Representation<M> {
        /// Transforms the IR to another model using the given transformer.
        #[allow(clippy::missing_errors_doc)]
        pub fn transform_model<
            U: Model,
            E: From<CyclicDependencyError>,
            T: Transform<Lifetime<M>, Target = U, Error = E>
                + Transform<Type<M>, Target = U, Error = E>
                + Transform<Constant<M>, Target = U, Error = E>,
        >(
            self,
            transformer: &mut T,
            table: &Table,
        ) -> Result<Representation<U>, E> {
            Ok(Representation {
                values: Values {
                    registers: self
                        .values
                        .registers
                        .into_iter()
                        .map(|(key, register)| {
                            Ok((
                                ID::from_index(key.into_index()),
                                register.transform_model(transformer, table)?,
                            ))
                        })
                        .collect::<Result<_, E>>()?,
                    allocas: self
                        .values
                        .allocas
                        .into_iter()
                        .map(|(key, alloca)| {
                            Ok((
                                ID::from_index(key.into_index()),
                                alloca.transform_model(transformer)?,
                            ))
                        })
                        .collect::<Result<_, E>>()?,
                },
                control_flow_graph: {
                    self.control_flow_graph.transform_model(transformer)?
                },
                scope_tree: self.scope_tree,
            })
        }
    }
}
