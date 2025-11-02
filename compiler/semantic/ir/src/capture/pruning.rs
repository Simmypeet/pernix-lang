//! A module for pruning closure captures based on their usage.

use std::collections::hash_map::Entry;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_term::{lifetime::Lifetime, r#type::Qualifier};

use crate::{
    address::{Address, Memory, Reference},
    capture::{Capture, CaptureMode, Captures, ReferenceCaptureMode},
    instruction::{self, Instruction},
    value::register::Assignment,
    Values, IR,
};

/// A structure used for pruning the closure captures.
#[derive(Debug, Default)]
struct PruningContext {
    unsages: HashMap<ID<Capture>, CaptureMode>,
}

impl PruningContext {
    fn new() -> Self { Self { unsages: HashMap::default() } }

    fn visit_usage(&mut self, capture: ID<Capture>, mode: CaptureMode) {
        let order_capture_mode = |x: &CaptureMode| match x {
            CaptureMode::ByValue => 2,
            CaptureMode::ByReference(by_ref) => match by_ref.qualifier {
                Qualifier::Immutable => 0,
                Qualifier::Mutable => 1,
            },
        };

        match self.unsages.entry(capture) {
            Entry::Occupied(mut occupied_entry) => {
                let existing_rank = order_capture_mode(occupied_entry.get());
                let new_rank = order_capture_mode(&mode);

                // update to the more general capture mode
                if new_rank > existing_rank {
                    occupied_entry.insert(mode);
                }
            }

            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(mode);
            }
        }
    }

    fn visit_instruction(
        &mut self,
        instruction: &Instruction,
        values: &Values,
        prune_mode: PruneMode,
    ) {
        for (address, access) in instruction.get_access_address(values) {
            // we're interested in the capture memory only
            let Memory::Capture(root_address) = address.get_root_memory()
            else {
                continue;
            };

            self.visit_usage(*root_address, match access {
                instruction::AccessKind::Normal(access_mode) => {
                    match access_mode {
                        instruction::AccessMode::Read(read) => {
                            CaptureMode::ByReference(ReferenceCaptureMode {
                                lifetime: Lifetime::Erased,
                                qualifier: read.qualifier,
                            })
                        }
                        instruction::AccessMode::Load(_) => {
                            match prune_mode {
                                PruneMode::Once => CaptureMode::ByValue,
                                PruneMode::Multiple => CaptureMode::ByReference(
                                    ReferenceCaptureMode {
                                        lifetime: Lifetime::Erased, /* defaults to
                                                                     * erased */
                                        qualifier: Qualifier::Immutable,
                                    },
                                ),
                            }
                        }
                        instruction::AccessMode::Write(_) => {
                            CaptureMode::ByReference(ReferenceCaptureMode {
                                lifetime: Lifetime::Erased, /* defaults to
                                                             * erased */
                                qualifier: Qualifier::Mutable,
                            })
                        }
                    }
                }
                instruction::AccessKind::Drop => unreachable!(),
            });
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum AccessMode {
    Borrow(Qualifier),
    Move,
}

/// The mode used for pruning captures.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PruneMode {
    /// The capture is used under the closure that can be called only once.
    Once,

    /// The capture is used under the closure that can be called multiple
    /// times.
    ///
    /// This implies that some operation like loading/moving on the capture
    /// value needs to be `Copy`.
    Multiple,
}

impl Captures {
    /// Initially, the closure captures all the variables with by-value capture
    /// mode. This is the most general settings. After binding, we look through
    /// all the usage of the captures and figure out the most restrictive
    /// capture mode for each capture (by-value, by-mut-ref, by-imm-ref),
    /// and adjust all the instructions related to the captures accordingly.
    ///
    /// # Parameters
    ///
    /// - captures: The captures which initially contains all captures with
    ///   by-value. After pruning, it will contain only the used captures with
    ///   the most restrictive capture mode.
    pub fn prune_capture_ir<'x, I: Iterator<Item = &'x mut IR>>(
        &mut self,
        irs: I,
        mode: PruneMode,
    ) {
        let mut irs = irs.collect::<Vec<_>>();

        let mut pruning_context = PruningContext::new();

        // visit all instructions in all IRs to collect usage information
        for ir in &mut irs {
            ir.control_flow_graph.traverse().for_each(|(_, block)| {
                for inst in block.instructions() {
                    pruning_context.visit_instruction(inst, &ir.values, mode);
                }
            });
        }

        let all_capture_ids = self.ids().collect::<Vec<_>>();

        // remove unused captures
        for id in all_capture_ids {
            if !pruning_context.unsages.contains_key(&id) {
                let _ = self.captures.remove(id);
            }
        }

        for (capture_id, new_capture) in pruning_context.unsages {
            self.captures[capture_id].capture_mode = new_capture;
        }

        // adjust all IRs to use the new capture modes
        for ir in &mut irs {
            for inst in ir.control_flow_graph.traverse_mut_instructions() {
                match inst {
                    Instruction::Store(store) => {
                        self.adjust_memory_usage(
                            &mut store.address,
                            AccessMode::Borrow(Qualifier::Mutable),
                        );
                    }

                    Instruction::RegisterAssignment(register_assignment) => {
                        let register =
                            &mut ir.values.registers[register_assignment.id];

                        match &mut register.assignment {
                            Assignment::Load(load) => {
                                self.adjust_memory_usage(
                                    &mut load.address,
                                    AccessMode::Move,
                                );
                            }

                            Assignment::Borrow(borrow) => {
                                self.adjust_memory_usage(
                                    &mut borrow.address,
                                    AccessMode::Borrow(borrow.qualifier),
                                );
                            }

                            Assignment::VariantNumber(variant_number) => {
                                self.adjust_memory_usage(
                                    &mut variant_number.address,
                                    AccessMode::Borrow(Qualifier::Immutable),
                                );
                            }

                            Assignment::Tuple(_)
                            | Assignment::Prefix(_)
                            | Assignment::Struct(_)
                            | Assignment::Variant(_)
                            | Assignment::FunctionCall(_)
                            | Assignment::Binary(_)
                            | Assignment::Array(_)
                            | Assignment::Phi(_)
                            | Assignment::Cast(_) => {}
                        }
                    }

                    Instruction::TuplePack(tuple_pack) => {
                        self.adjust_memory_usage(
                            &mut tuple_pack.tuple_address,
                            AccessMode::Move,
                        );
                        self.adjust_memory_usage(
                            &mut tuple_pack.store_address,
                            AccessMode::Borrow(Qualifier::Mutable),
                        );
                    }

                    Instruction::RegisterDiscard(_)
                    | Instruction::ScopePush(_)
                    | Instruction::ScopePop(_) => {}

                    Instruction::DropUnpackTuple(_) | Instruction::Drop(_) => {
                        unreachable!()
                    }
                }
            }
        }

        // readjust the drop order of the captures since some might have been
        // removed.

        // TODO: maybe there's a  better way than collecting all ids and sorting
        let mut captures_ids_with_order = self
            .captures
            .iter()
            .map(|(id, c)| (id, c.drop_order))
            .collect::<Vec<_>>();

        captures_ids_with_order.sort_by_key(|x| x.1);

        for (new_order, (capture_id, _)) in
            captures_ids_with_order.into_iter().enumerate()
        {
            self.captures[capture_id].drop_order = new_order;
        }
    }

    fn adjust_memory_usage(
        &self,
        address: &mut Address,
        access_mode: AccessMode,
    ) {
        let Memory::Capture(capture_id) = address.get_root_memory() else {
            return;
        };

        let capture_memory_address =
            Address::Memory(Memory::Capture(*capture_id));

        let capture = &self[*capture_id];

        match (&capture.capture_mode, access_mode) {
            (
                CaptureMode::ByValue,
                AccessMode::Borrow(_) | AccessMode::Move,
            ) => {
                // no-adjustment needed
            }

            (
                CaptureMode::ByReference(reference_capture_mode),
                AccessMode::Borrow(qualifier),
            ) => {
                if qualifier == Qualifier::Mutable {
                    assert_eq!(
                        reference_capture_mode.qualifier,
                        Qualifier::Mutable,
                        "cannot borrow immutable reference as mutable"
                    );
                }

                address.replace_with(
                    &capture_memory_address.clone(),
                    Address::Reference(Reference {
                        qualifier: reference_capture_mode.qualifier,
                        reference_address: Box::new(capture_memory_address),
                    }),
                );
            }
            (
                CaptureMode::ByReference(reference_capture_mode),
                AccessMode::Move,
            ) => {
                address.replace_with(
                    &capture_memory_address.clone(),
                    Address::Reference(Reference {
                        qualifier: reference_capture_mode.qualifier,
                        reference_address: Box::new(capture_memory_address),
                    }),
                );
            }
        }
    }
}
