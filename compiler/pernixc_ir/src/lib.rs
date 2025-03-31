//! Contains all the definitions related to the intermediate representation of
//! the function body.

use alloca::Alloca;
use control_flow_graph::ControlFlowGraph;
use getset::Getters;
use model::Transform;
use pernixc_abort::Abort;
use pernixc_arena::{Arena, Key, ID};
use pernixc_semantic::{component::Derived, Table};
use pernixc_term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, Model,
};
use serde::{Deserialize, Serialize};
use value::register::Register;

pub mod address;
pub mod alloca;
pub mod binding;
pub mod control_flow_graph;
pub mod instruction;
pub mod model;
pub mod pattern;
pub mod scope;
pub mod value;

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

/// An intermediate representation of the program.
pub type IR = Representation<model::Model>;

impl Derived for IR {
    fn component_name() -> &'static str { "intermediate representation" }
}

impl<M: Model> Representation<M> {
    /// Transforms the IR to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<
        U: Model,
        E: From<Abort>,
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
