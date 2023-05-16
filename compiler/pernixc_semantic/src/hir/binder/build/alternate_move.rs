use std::collections::{HashMap, HashSet};

use crate::{
    cfg::{BasicBlockID, Instruction},
    hir::{
        instruction::{Basic, Destruct, ScopePush},
        value::{Address, FieldAddress, VariableID},
        BranchID, Container, ScopeChildID, ScopeID,
    },
    symbol::{ty::Type, FieldID},
};

pub(in crate::hir::binder) fn alternate_move(
    container: &mut Container<Type>,
    moved_addresses_by_scope_id: &HashMap<ScopeID, Vec<Address>>,
) {
    insert_move_on_alternate_branches(
        container,
        container.control_flow_graph.entry_block(),
        moved_addresses_by_scope_id,
        HashSet::new(),
    );
}

fn insert_move_on_alternate_branches(
    container: &mut Container<Type>,
    basic_block_id: BasicBlockID,
    moved_addresses_by_scope_id: &HashMap<ScopeID, Vec<Address>>,
    mut visited: HashSet<BasicBlockID>,
) {
    if visited.contains(&basic_block_id) {
        return;
    }

    visited.insert(basic_block_id);

    let mut basic_block = container
        .control_flow_graph
        .get_mut(basic_block_id)
        .unwrap();

    let mut i = 0;
    while i < basic_block.instructions().len() {
        let instruction = &basic_block.instructions()[i];
        match instruction {
            Instruction::Jump(inst) => {
                let jump_target = inst.jump_target;
                insert_move_on_alternate_branches(
                    container,
                    jump_target,
                    moved_addresses_by_scope_id,
                    visited,
                );
                return;
            }
            Instruction::Return(..) => return,
            Instruction::ConditionalJump(inst) => {
                let true_jump_target = inst.true_jump_target;
                let false_jump_target = inst.false_jump_target;

                insert_move_on_alternate_branches(
                    container,
                    true_jump_target,
                    moved_addresses_by_scope_id,
                    visited.clone(),
                );
                insert_move_on_alternate_branches(
                    container,
                    false_jump_target,
                    moved_addresses_by_scope_id,
                    visited,
                );

                return;
            }
            Instruction::Basic(inst) => {
                let Basic::ScopePush(ScopePush { scope_id }) = inst else {
                    i += 1;
                    continue;
                };
                let scope_id = *scope_id;

                let Some(branch_id) = container.scope_tree.scopes.get(
                    scope_id
                ).unwrap().branch else {
                    i += 1;
                    continue;
                };

                let mut alternate_destruct_list =
                    get_destruct_list(container, scope_id, branch_id, moved_addresses_by_scope_id);

                let mut this_scope_destruct_list = Vec::new();
                populate_destruct_list(
                    container,
                    scope_id,
                    moved_addresses_by_scope_id,
                    container.scope_tree.scopes.get(scope_id).unwrap().depth,
                    &mut this_scope_destruct_list,
                );

                for super_address in this_scope_destruct_list {
                    filter_address(&super_address, &mut alternate_destruct_list);
                }

                let destruct_len = alternate_destruct_list.len();
                for address in alternate_destruct_list {
                    container
                        .control_flow_graph
                        .get_mut(basic_block_id)
                        .unwrap()
                        .insert_basic_instruction(i + 1, Basic::Destruct(Destruct { address }));
                }

                i += destruct_len;

                basic_block = container
                    .control_flow_graph
                    .get_mut(basic_block_id)
                    .unwrap();
            }
        }
        i += 1;
    }
}

fn get_destruct_list(
    container: &Container<Type>,
    scope_id: ScopeID,
    branch_id: BranchID,
    moved_addresses_by_scope_id: &HashMap<ScopeID, Vec<Address>>,
) -> Vec<Address> {
    let mut destruct_list = Vec::new();
    let maximum_scope_depth = container.scope_tree.scopes.get(scope_id).unwrap().depth;

    // get the list of addresses that need to be destructped in order to be in sync
    // with another scopes in the same branch
    for branch_scope_id in container
        .scope_tree
        .branches
        .get(branch_id)
        .unwrap()
        .scopes
        .iter()
        .copied()
    {
        if branch_scope_id == scope_id {
            continue;
        }

        populate_destruct_list(
            container,
            branch_scope_id,
            moved_addresses_by_scope_id,
            maximum_scope_depth,
            &mut destruct_list,
        );
    }

    // sort the list of destruct addresses in order of LIFO (last to be declared, first to be
    // destructped)
    destruct_list.sort_unstable_by(|lhs, rhs| {
        println!("sorting");
        match (lhs, rhs) {
            (Address::VariableID(lhs), Address::VariableID(rhs)) => {
                destruct_ordering(*lhs, *rhs, container)
            }
            (Address::VariableID(lhs), Address::FieldAddress(rhs)) => {
                let rhs_root = super::get_address_root_variable_id(&rhs.operand_address);
                assert_ne!(*lhs, rhs_root);

                destruct_ordering(*lhs, rhs_root, container)
            }
            (Address::FieldAddress(lhs), Address::VariableID(rhs)) => {
                let lhs_root = super::get_address_root_variable_id(&lhs.operand_address);

                assert_ne!(lhs_root, *rhs);

                destruct_ordering(lhs_root, *rhs, container)
            }
            (Address::FieldAddress(lhs), Address::FieldAddress(rhs)) => {
                let root_lhs = super::get_address_root_variable_id(&lhs.operand_address);
                let root_rhs = super::get_address_root_variable_id(&rhs.operand_address);

                if root_lhs == root_rhs {
                    destruct_ordering(root_lhs, root_rhs, container)
                } else {
                    // we nned to check for the odering of the fields
                    let lhs_field_heirarchy = get_field_heirarchy(lhs);
                    let rhs_field_heirarchy = get_field_heirarchy(rhs);

                    for (lhs_field, rhs_field) in lhs_field_heirarchy
                        .iter()
                        .copied()
                        .zip(rhs_field_heirarchy.iter().copied())
                    {
                        if lhs_field == rhs_field {
                            continue;
                        }

                        let lhs_field_ordering = container
                            .table
                            .get_field(lhs_field)
                            .unwrap()
                            .declaration_order();
                        let rhs_field_ordering = container
                            .table
                            .get_field(rhs_field)
                            .unwrap()
                            .declaration_order();

                        return lhs_field_ordering.cmp(&rhs_field_ordering);
                    }

                    unreachable!("there should be a field ordering here")
                }
            }
        }
    });

    destruct_list
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

fn destruct_ordering(
    lhs: VariableID,
    rhs: VariableID,
    container: &Container<Type>,
) -> std::cmp::Ordering {
    let (lhs_scope_depth, lhs_declaration_order) =
        super::get_variable_id_declaration_order(lhs, container);
    let (rhs_scope_depth, rhs_declaration_order) =
        super::get_variable_id_declaration_order(rhs, container);

    match lhs_scope_depth.cmp(&rhs_scope_depth) {
        std::cmp::Ordering::Less => std::cmp::Ordering::Less,
        std::cmp::Ordering::Equal => {
            // sort by its declaration order
            lhs_declaration_order.cmp(&rhs_declaration_order)
        }
        std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
    }
}

fn filter_address(super_address: &Address, addresses: &mut Vec<Address>) {
    let mut index = 0;
    while index < addresses.len() {
        if super::is_subaddress_or_equal(super_address, &addresses[index]) {
            addresses.remove(index);
        } else {
            index += 1;
        }
    }
}

fn add_to_destruct_list(destruct_list: &mut Vec<Address>, insert_address: Address) {
    for address in destruct_list.iter_mut() {
        // the address is already in the destruct list
        if super::is_subaddress_or_equal(address, &insert_address) {
            return;
        }
    }

    // look for any address that is a subaddress of the insert address and remove it
    filter_address(&insert_address, destruct_list);

    destruct_list.push(insert_address);
}

fn populate_destruct_list(
    container: &Container<Type>,
    scope_id: ScopeID,
    moved_addresses_by_scope_id: &HashMap<ScopeID, Vec<Address>>,
    maximum_scope_depth: usize,
    destruct_list: &mut Vec<Address>,
) {
    if let Some(addresses) = moved_addresses_by_scope_id.get(&scope_id) {
        for address in addresses {
            let root_variable_id = super::get_address_root_variable_id(address);
            let root_scope_depth = match root_variable_id {
                VariableID::AllocaID(alloca_id) => {
                    container
                        .scope_tree
                        .scopes
                        .get(container.allocas.get(alloca_id).unwrap().scope_id)
                        .unwrap()
                        .depth
                }
                VariableID::ParameterID(_) => 0,
            };

            if root_scope_depth < maximum_scope_depth {
                add_to_destruct_list(destruct_list, address.clone());
            }
        }
    }

    for child in &container.scope_tree.scopes.get(scope_id).unwrap().children {
        match child {
            ScopeChildID::ScopeID(scope_id) => populate_destruct_list(
                container,
                *scope_id,
                moved_addresses_by_scope_id,
                maximum_scope_depth,
                destruct_list,
            ),
            ScopeChildID::BranchID(branch_id) => {
                for scope_id in &container
                    .scope_tree
                    .branches
                    .get(*branch_id)
                    .unwrap()
                    .scopes
                {
                    populate_destruct_list(
                        container,
                        *scope_id,
                        moved_addresses_by_scope_id,
                        maximum_scope_depth,
                        destruct_list,
                    );
                }
            }
        }
    }
}
