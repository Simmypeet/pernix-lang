use std::collections::{HashMap, HashSet};

use crate::{
    cfg::{BasicBlockID, Instruction},
    hir::{
        instruction::Basic,
        value::{
            binding::{Binding, LoadType},
            Address,
        },
        Container, ScopeID,
    },
    symbol::ty::Type,
};

/// Performs *auto-move* analysis on the given [`Container`].
///
/// # Returns
/// Returns a mapping of [`ScopeID`]s to a list of addresses that are moved out. This will be used
/// in *alternate-move* analysis.
pub(in crate::hir::binder) fn auto_move(
    container: &mut Container<Type>,
) -> HashMap<ScopeID, Vec<Address>> {
    let mut moved_addresses_by_scope_id = HashMap::new();
    auto_move_internal(
        container,
        container.control_flow_graph.entry_block(),
        HashSet::new(),
        Vec::new(),
        &mut moved_addresses_by_scope_id,
    );
    moved_addresses_by_scope_id
}

fn auto_move_internal(
    container: &mut Container<Type>,
    basic_block_id: BasicBlockID,
    mut visited: HashSet<BasicBlockID>,
    mut scope_stack: Vec<ScopeID>,
    moved_addresses_by_scope_id: &mut HashMap<ScopeID, Vec<Address>>,
) {
    // prevent infinite recursion
    if visited.contains(&basic_block_id) {
        return;
    }

    visited.insert(basic_block_id);

    for (index, instruction) in container
        .control_flow_graph
        .get(basic_block_id)
        .unwrap()
        .instructions()
        .iter()
        .enumerate()
    {
        match instruction {
            Instruction::Jump(inst) => {
                auto_move_internal(
                    container,
                    inst.jump_target,
                    visited,
                    scope_stack,
                    moved_addresses_by_scope_id,
                );
                return;
            }
            Instruction::Return(..) => return,
            Instruction::ConditionalJump(inst) => {
                let true_jump_target = inst.true_jump_target;
                let false_jump_target = inst.false_jump_target;
                auto_move_internal(
                    container,
                    true_jump_target,
                    visited.clone(),
                    scope_stack.clone(),
                    moved_addresses_by_scope_id,
                );
                auto_move_internal(
                    container,
                    false_jump_target,
                    visited,
                    scope_stack,
                    moved_addresses_by_scope_id,
                );
                return;
            }
            Instruction::Basic(inst) => {
                match inst {
                    Basic::RegisterAssignment(register_assignment) => {
                        // expect load binding
                        let Binding::Load(load) = &container.registers.get(
                            register_assignment.register_id).unwrap().binding else {
                            continue;
                        };
                        let address = load.address.clone();

                        if is_movable(container, basic_block_id, index + 1, &address) {
                            container
                                .registers
                                .get_mut(register_assignment.register_id)
                                .unwrap()
                                .binding
                                .as_load_mut()
                                .unwrap()
                                .load_type = LoadType::Move;

                            moved_addresses_by_scope_id
                                .entry(*scope_stack.last().unwrap())
                                .or_default()
                                .push(address);
                        }
                    }
                    Basic::ScopePush(new_scope_id) => scope_stack.push(new_scope_id.scope_id),
                    Basic::ScopePop(pop_scope_id) => {
                        assert_eq!(scope_stack.pop(), Some(pop_scope_id.scope_id));
                    }

                    Basic::VariableDeclaration(..) | Basic::Store(..) | Basic::Destruct(..) => {}
                }
            }
        }
    }
}

fn is_movable(
    container: &Container<Type>,
    basic_block_id: BasicBlockID,
    instruction_offset: usize,
    target_address: &Address,
) -> bool {
    for instruction in container
        .control_flow_graph
        .get(basic_block_id)
        .unwrap()
        .instructions()
        .iter()
        .skip(instruction_offset)
    {
        match instruction {
            Instruction::Jump(inst) => {
                return is_movable(container, inst.jump_target, 0, target_address);
            }
            Instruction::Return(..) => {
                return true;
            }
            Instruction::ConditionalJump(inst) => {
                return is_movable(container, inst.true_jump_target, 0, target_address)
                    && is_movable(container, inst.false_jump_target, 0, target_address);
            }
            Instruction::Basic(basic) => match basic {
                Basic::ScopePop(..) | Basic::ScopePush(..) | Basic::Destruct(..) => {}
                Basic::VariableDeclaration(inst) => {
                    if super::is_subaddress_or_equal(
                        &Address::VariableID(inst.variable_id),
                        target_address,
                    ) {
                        return true;
                    }
                }
                Basic::Store(inst) => {
                    if super::is_subaddress_or_equal(&inst.address, target_address) {
                        return true;
                    }
                }
                Basic::RegisterAssignment(register_assignment) => {
                    let Binding::Load(load) = &container.registers.get(
                        register_assignment.register_id
                    ).unwrap().binding else {
                        continue;
                    };

                    if super::is_subaddress_or_equal(&load.address, target_address)
                        || super::is_subaddress_or_equal(target_address, &load.address)
                    {
                        return false;
                    }
                }
            },
        }
    }

    true
}
