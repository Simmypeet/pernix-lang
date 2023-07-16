use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::States, Table};
use crate::{error, Struct, ID};

impl Table {
    pub(super) fn finalize_symbol(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        match id {
            ID::Module(..) | ID::Enum(..) | ID::EnumVariant(_) => {
                // currently, there is nothing to finalize for this symbol
            }
            ID::Struct(struct_id) => {
                self.finalize_struct(struct_id, states, handler);
            }
            ID::Function(_) => todo!(),
            ID::Type(_) => todo!(),
            ID::Field(_) => todo!(),
            ID::FunctionParameter(_) => todo!(),
            ID::TraitFunctionParameter(_) => todo!(),
            ID::ImplementsFunctionParameter(_) => todo!(),
            ID::Trait(_) => todo!(),
            ID::TraitType(_) => todo!(),
            ID::TypeParameter(_) => todo!(),
            ID::LifetimeParameter(_) => todo!(),
            ID::TraitFunction(_) => todo!(),
            ID::Implements(_) => todo!(),
            ID::ImplementsFunction(_) => todo!(),
            ID::ImplementsType(_) => todo!(),
        }
    }

    pub(super) fn finalize_struct(
        &mut self,
        struct_id: arena::ID<Struct>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        if let Err(err) = states.mark_as_constructing(struct_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        };

        let struct_symbol = &self.structs[struct_id];

        // span out the fields
        for field_id in struct_symbol.field_order.iter().copied() {
            states.add_drafted_symbol(field_id.into());
        }

        states.remove_constructing_symbol(struct_id.into());
    }
}
