use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    cfg::{BasicBlockID, Instruction},
    hir::{
        instruction::{Basic, Destruct},
        value::{
            binding::{Binding, LoadType},
            Address, FieldAddress, VariableID,
        },
        Container, ScopeID,
    },
    symbol::{ty::Type, FieldID},
};

/// Represents a state of variable and field layout.
#[derive(Debug, Clone, EnumAsInner)]
enum State {
    /// The variable/field is fully intact.
    Constructed,

    /// Part of the variable/field is moved/destroyed
    Dissolved(Vec<FieldLayout>),

    /// The variable is destructed
    Destructed,
}

impl State {
    fn try_to_construct(&mut self) {
        match self {
            Self::Constructed | Self::Destructed => {}
            Self::Dissolved(field_layout) => {
                // every field must be constructed
                for field in field_layout.iter_mut() {
                    field.state.try_to_construct();
                }

                for field in field_layout {
                    if !matches!(field.state, Self::Constructed) {
                        return;
                    }
                }

                *self = Self::Constructed;
            }
        }
    }
}

/// Represents a state of a field,
#[derive(Debug, Clone)]
struct FieldLayout {
    /// The field ID.
    field_id: FieldID,

    /// The state of the field.
    state: State,
}

#[derive(Debug, Clone)]
struct VariableStates {
    /// Maps the variable id to its state
    states_by_variable_id: HashMap<VariableID, State>,

    /// Maps scope ID to its variable ids that are declared in the scope.
    declared_variable_ids_by_scope_id: HashMap<ScopeID, Vec<VariableID>>,
}

impl VariableStates {
    /// Registers a new `variable_id` into the container with [`State::Constructed`] as a default
    /// state.
    fn fresh(&mut self, variable_id: VariableID, container: &Container<Type>) {
        let scope_id = container.get_variable_id_scope_declaration_id(variable_id);

        self.states_by_variable_id
            .insert(variable_id, State::Constructed);
        self.declared_variable_ids_by_scope_id
            .entry(scope_id)
            .or_insert_with(Vec::new)
            .push(variable_id);
    }

    /// Pops out the scope and returns that addresses that need to be destructed.
    fn pop(&mut self, scope_id: ScopeID, container: &Container<Type>) -> Vec<Address> {
        fn populate_destruct_instruction(
            base_address: Address,
            field_layout: FieldLayout,
            destruct_addresses: &mut Vec<Address>,
        ) {
            let address = Address::FieldAddress(FieldAddress {
                operand_address: Box::new(base_address),
                field_id: field_layout.field_id,
            });

            match field_layout.state {
                State::Constructed => {
                    destruct_addresses.push(address);
                }
                State::Dissolved(dissolve) => {
                    for field_layout in dissolve {
                        populate_destruct_instruction(
                            address.clone(),
                            field_layout,
                            destruct_addresses,
                        );
                    }
                }
                State::Destructed => {}
            }
        }

        let mut variables = self
            .declared_variable_ids_by_scope_id
            .remove(&scope_id)
            .unwrap_or_default();

        // sort by declaration order
        variables.sort_unstable_by(|lhs, rhs| {
            container
                .get_variable_id_local_declaration_order(*lhs)
                .cmp(&container.get_variable_id_local_declaration_order(*rhs))
        });

        let mut destruct_addresses = Vec::new();
        for variable_id in variables {
            let mut state = self.states_by_variable_id.remove(&variable_id).unwrap();

            // try convert itself back to `Constructed`
            state.try_to_construct();

            let address = Address::VariableID(variable_id);

            match state {
                // fully destruct the whole variable
                State::Constructed => destruct_addresses.push(address),
                State::Dissolved(field_layout) => {
                    for field_layout in field_layout {
                        populate_destruct_instruction(
                            address.clone(),
                            field_layout,
                            &mut destruct_addresses,
                        );
                    }
                }
                State::Destructed => {}
            }
        }

        destruct_addresses
    }

    fn restore_variable_state(&mut self, address: &Address) {
        match address {
            Address::VariableID(variable_id) => {
                *self.states_by_variable_id.get_mut(variable_id).unwrap() = State::Constructed;
            }
            Address::FieldAddress(field_address) => {
                let root_variable_id =
                    super::get_address_root_variable_id(&field_address.operand_address);
                let field_heirarchy = super::get_field_heirarchy(field_address);

                let mut state = {
                    let state = self
                        .states_by_variable_id
                        .get_mut(&root_variable_id)
                        .unwrap();

                    if state.as_dissolved().is_none() {
                        return;
                    }

                    state
                };

                for field_id in field_heirarchy {
                    let field_layout_index = state
                        .as_dissolved_mut()
                        .unwrap()
                        .iter()
                        .position(|x| x.field_id == field_id)
                        .unwrap();

                    let next_state =
                        &mut state.as_dissolved_mut().unwrap()[field_layout_index].state;

                    state = match next_state {
                        State::Constructed => return, // already intact
                        State::Dissolved(..) => next_state,
                        State::Destructed => {
                            unreachable!("auto move won't allow this in the first place")
                        }
                    }
                }

                // restore the state to intact
                *state = State::Constructed;

                state.try_to_construct();
            }
        }
    }

    fn destruct_variable_state(&mut self, address: &Address, container: &Container<Type>) {
        match address {
            Address::VariableID(variable_id) => {
                // remove the variable from the state
                *self.states_by_variable_id.get_mut(variable_id).unwrap() = State::Destructed;
            }
            Address::FieldAddress(address) => {
                let root_variable_id =
                    super::get_address_root_variable_id(&address.operand_address);

                let mut state = self
                    .states_by_variable_id
                    .get_mut(&root_variable_id)
                    .unwrap();

                match state {
                    State::Constructed => {
                        let struct_id = container
                            .get_variable_id_type(root_variable_id)
                            .into_typed_id()
                            .unwrap()
                            .into_struct()
                            .unwrap();

                        *state = State::Dissolved(
                            container
                                .table
                                .get_struct(struct_id)
                                .unwrap()
                                .field_order()
                                .iter()
                                .copied()
                                .map(|x| FieldLayout {
                                    field_id: x,
                                    state: State::Constructed,
                                })
                                .collect(),
                        );
                    }
                    State::Dissolved(..) => {}
                    State::Destructed => panic!("cannot destruct a destructed variable"),
                }

                let mut field_heirarchy = super::get_field_heirarchy(address);
                assert!(!field_heirarchy.is_empty());

                for field_id in &field_heirarchy[..field_heirarchy.len() - 1] {
                    state = state
                        .as_dissolved_mut()
                        .unwrap()
                        .iter_mut()
                        .find_map(|x| {
                            if x.field_id == *field_id {
                                Some(&mut x.state)
                            } else {
                                None
                            }
                        })
                        .unwrap();

                    let struct_id = container
                        .table
                        .get_field(*field_id)
                        .unwrap()
                        .ty()
                        .into_typed_id()
                        .unwrap()
                        .into_struct()
                        .unwrap();

                    match state {
                        State::Constructed => {
                            *state = State::Dissolved(
                                container
                                    .table
                                    .get_struct(struct_id)
                                    .unwrap()
                                    .field_order()
                                    .iter()
                                    .copied()
                                    .map(|x| FieldLayout {
                                        field_id: x,
                                        state: State::Constructed,
                                    })
                                    .collect(),
                            );
                        }
                        State::Dissolved(..) => {}
                        State::Destructed => panic!("cannot destruct a destructed variable"),
                    }
                }

                // get the last field id
                let field_id = field_heirarchy.pop().unwrap();

                state
                    .as_dissolved_mut()
                    .unwrap()
                    .iter_mut()
                    .find_map(|x| {
                        if x.field_id == field_id {
                            Some(x)
                        } else {
                            None
                        }
                    })
                    .unwrap()
                    .state = State::Destructed;
            }
        }
    }
}

pub(in crate::hir::binder) fn insert_destruct(container: &mut Container<Type>) {
    insert_destruct_internal(
        container,
        container.control_flow_graph.entry_block(),
        VariableStates {
            states_by_variable_id: HashMap::new(),
            declared_variable_ids_by_scope_id: HashMap::new(),
        },
        HashSet::new(),
    );
}

#[allow(clippy::too_many_lines)]
fn insert_destruct_internal(
    container: &mut Container<Type>,
    basic_block_id: BasicBlockID,
    mut variable_states: VariableStates,
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
        match &basic_block.instructions()[i] {
            Instruction::Jump(inst) => {
                let jump_target = inst.jump_target;
                insert_destruct_internal(container, jump_target, variable_states.clone(), visited);
                return;
            }
            Instruction::Return(..) => return,
            Instruction::ConditionalJump(inst) => {
                let true_target = inst.true_jump_target;
                let false_target = inst.false_jump_target;

                insert_destruct_internal(
                    container,
                    true_target,
                    variable_states.clone(),
                    visited.clone(),
                );
                insert_destruct_internal(container, false_target, variable_states, visited);

                return;
            }
            Instruction::Basic(inst) => match inst {
                Basic::RegisterAssignment(register) => {
                    let Binding::Load(load) = &container.registers.get(
                        register.register_id
                    ).unwrap().binding else {
                        i += 1;
                        continue;
                    };

                    if load.load_type != LoadType::Move {
                        i += 1;
                        continue;
                    }

                    variable_states.destruct_variable_state(load.address(), container);

                    // reset the pointer
                    basic_block = container
                        .control_flow_graph
                        .get_mut(basic_block_id)
                        .unwrap();
                }
                Basic::VariableDeclaration(declaration) => {
                    variable_states.fresh(declaration.variable_id, container);

                    // reset the pointer
                    basic_block = container
                        .control_flow_graph
                        .get_mut(basic_block_id)
                        .unwrap();
                }
                Basic::Store(store) => {
                    let address = store.address.clone();
                    variable_states.restore_variable_state(&address);
                }
                Basic::ScopePush(..) => {}
                Basic::ScopePop(scope_pop) => {
                    let destruct_addresses = variable_states.pop(scope_pop.scope_id, container);
                    let destruct_len = destruct_addresses.len();

                    // reset the pointer
                    basic_block = container
                        .control_flow_graph
                        .get_mut(basic_block_id)
                        .unwrap();

                    // adds the destruct instructions to the basic block
                    for address in destruct_addresses {
                        basic_block
                            .insert_basic_instruction(i, Basic::Destruct(Destruct { address }));
                    }

                    i += destruct_len;
                }
                Basic::Destruct(destruct) => {
                    variable_states.destruct_variable_state(&destruct.address.clone(), container);

                    // reset the pointer
                    basic_block = container
                        .control_flow_graph
                        .get_mut(basic_block_id)
                        .unwrap();
                }
            },
        }

        i += 1;
    }
}
