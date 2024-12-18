//! Contains the definition of [`Representation`].

use getset::Getters;

use super::{
    control_flow_graph::ControlFlowGraph, scope, value::register::Register,
    Transform,
};
use crate::{
    arena::{Arena, Key, ID},
    ir::alloca::Alloca,
    symbol::table::{self, Table},
    type_system::{
        model::Model,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type},
    },
};

pub mod binding;
pub mod borrow;

/// Contains all the registers and allocas used in the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Values<M: Model> {
    /// Contains all the registers used in the program.
    #[get = "pub"]
    registers: Arena<Register<M>>,
    /// Contains all the allocas used in the program.
    #[get = "pub"]
    allocas: Arena<Alloca<M>>,
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Representation<M: Model> {
    /// Contains the registers and allocas used in the program.
    #[get = "pub"]
    values: Values<M>,

    /// The control flow graph of the program.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<M>,

    /// The tree of scopes in the program.
    #[get = "pub"]
    scope_tree: scope::Tree,
}

impl<M: Model> Representation<M> {
    /// Transforms the IR to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<
        U: Model,
        E,
        T: Transform<Lifetime<M>, Target = U, Error = E>
            + Transform<Type<M>, Target = U, Error = E>
            + Transform<Constant<M>, Target = U, Error = E>,
    >(
        self,
        transformer: &mut T,
        table: &Table<impl table::State>,
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
