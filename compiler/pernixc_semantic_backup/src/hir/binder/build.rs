use crate::{
    hir::{
        value::{Address, FieldAddress, VariableID},
        Container, ScopeID,
    },
    symbol::{ty::Type, FieldID},
};

/// Checks if the `sub_address` is a subaddress of the `parent_address` (e.g., `parent_address` is
/// an address to a struct and `sub_address` is an address to a field of that struct)
fn is_subaddress_or_equal(parent_address: &Address, mut sub_address: &Address) -> bool {
    loop {
        match (parent_address, sub_address) {
            (Address::VariableID(lhs), Address::VariableID(rhs)) => return lhs == rhs,
            (Address::VariableID(..), Address::FieldAddress(rhs)) => {
                sub_address = &rhs.operand_address;
            }
            (Address::FieldAddress(..), Address::VariableID(..)) => return false,
            (Address::FieldAddress(lhs), Address::FieldAddress(rhs)) => {
                if lhs == rhs {
                    return true;
                }
                sub_address = &rhs.operand_address;
            }
        }
    }
}

/// Gets the root variable id of an address.
fn get_address_root_variable_id(mut address: &Address) -> VariableID {
    loop {
        match address {
            Address::VariableID(id) => return *id,
            Address::FieldAddress(field_address) => {
                address = &field_address.operand_address;
            }
        }
    }
}

/// Gets the declaration order (scope local order) and the scope depth of its declaration of a
/// variable.
///
/// # Returns
/// A tuple of scope depth and local declaration order.
fn get_variable_id_declaration_order(
    variable_id: VariableID,
    container: &Container<Type>,
) -> (usize, usize) {
    match variable_id {
        VariableID::AllocaID(alloca_id) => {
            let scope_depth = container
                .scope_tree
                .scopes
                .get(container.allocas.get(alloca_id).unwrap().scope_id)
                .unwrap()
                .depth;

            let declaration_order = container.allocas.get(alloca_id).unwrap().declaration_order;

            (scope_depth, declaration_order)
        }
        VariableID::ParameterID(parameter_id) => (
            0,
            container
                .table
                .get_parameter(parameter_id)
                .unwrap()
                .declaration_order(),
        ),
    }
}

impl Container<Type> {
    /// Gets the [`ScopeID`] in which given `variable_id` is declared.
    fn get_variable_id_scope_declaration_id(&self, variable_id: VariableID) -> ScopeID {
        match variable_id {
            VariableID::AllocaID(alloca_id) => self.allocas.get(alloca_id).unwrap().scope_id,
            VariableID::ParameterID(..) => self.scope_tree.root_scope,
        }
    }

    /// Gets the order in which given `variable_id` is declared in its scope (scope local, starting
    /// at 0).
    fn get_variable_id_local_declaration_order(&self, variable_id: VariableID) -> usize {
        match variable_id {
            VariableID::AllocaID(alloca_id) => {
                self.allocas.get(alloca_id).unwrap().declaration_order()
            }
            VariableID::ParameterID(parameter_id) => self
                .table
                .get_parameter(parameter_id)
                .unwrap()
                .declaration_order(),
        }
    }

    /// Gets the type of the variable with given `variable_id`.
    fn get_variable_id_type(&self, variable_id: VariableID) -> Type {
        match variable_id {
            VariableID::AllocaID(alloca_id) => self.allocas.get(alloca_id).unwrap().ty(),
            VariableID::ParameterID(parameter_id) => {
                self.table
                    .get_parameter(parameter_id)
                    .unwrap()
                    .type_binding()
                    .ty
            }
        }
    }
}

fn get_field_heirarchy(address: &FieldAddress) -> Vec<FieldID> {
    let mut heirarchy = vec![address.field_id];
    let mut address: &Address = &address.operand_address;

    while let Address::FieldAddress(field_address) = address {
        heirarchy.push(field_address.field_id);
        address = &field_address.operand_address;
    }

    heirarchy.reverse();
    heirarchy
}

mod alternate_move;
mod auto_move;
mod destruct;
mod transform;

pub(super) use alternate_move::alternate_move;
pub(super) use auto_move::auto_move;
pub(super) use destruct::insert_destruct;
pub(super) use transform::transform_type_system;
