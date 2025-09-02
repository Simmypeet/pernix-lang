use pernixc_arena::ID;
use pernixc_term::inference;

use crate::inference_context::table::{Constraint, Table};

#[derive(Debug)]
enum Log<C: Constraint> {
    Register(RegisterLog<C>),
    Combine(CombineLog<C>),
    AssignToKnown(AssignToKnownLog<C>),
}

#[derive(Debug)]
struct RegisterLog<C: Constraint> {
    inference_variable: inference::Variable<C::Term>,
    constraint_id: ID<C>,
    constraint: C,
}

#[derive(Debug)]
struct CombineLog<C: Constraint> {
    constraint_id: ID<C>,
    old_constraint: C,
}

#[derive(Debug)]
struct AssignToKnownLog<C: Constraint> {
    old_constraint: C,
    constraint_id: ID<C>,
    changed_variables: Vec<inference::Variable<C::Term>>,
}

/// Representing the checkpoint state that the inference table can be rewinded
/// back.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_copy_implementations)] // intentionally non-cloneable
pub struct Checkpoint {
    log_len: usize,
}

/// A struct implementing the checkpoint/replay functionality for the inference
/// table.
#[derive(Debug)]
pub struct History<C: Constraint> {
    replay_log: Option<Vec<Log<C>>>,
}

impl<C: Constraint> Default for History<C> {
    fn default() -> Self { Self { replay_log: None } }
}

impl<C: Constraint> Table<C> {
    /// Start a new checkpoint for the inference table. The checkpoint can be
    /// replayed later via [`Self::restore`].
    #[must_use]
    pub fn start_checkpoint(&mut self) -> Checkpoint {
        if let Some(log) = &self.history.replay_log {
            Checkpoint { log_len: log.len() }
        } else {
            self.history.replay_log = Some(Vec::new());
            Checkpoint { log_len: 0 }
        }
    }

    /// Restores the inference table to the state at the checkpoint.
    #[allow(clippy::needless_pass_by_value)] // intentionally pass by value
    pub fn restore(&mut self, checkpoint: Checkpoint) {
        assert!(
            self.history.replay_log.is_some(),
            "possibly incorrect use of checkpoint, the checkpoint hasn't been \
             started or the checkpoint from another table is used"
        );

        assert!(
            self.history.replay_log.as_ref().unwrap().len()
                >= checkpoint.log_len,
            "replay log is shorter than checkpoint, possibly incorrect use of \
             checkpoint"
        );

        while self.history.replay_log.as_ref().unwrap().len()
            > checkpoint.log_len
        {
            let log = self.history.replay_log.as_mut().unwrap().pop().unwrap();

            self.undo_log(log);
        }
    }

    pub(super) fn add_register_log(
        &mut self,
        inference_variable: inference::Variable<C::Term>,
        constraint_id: ID<C>,
        constraint: C,
    ) {
        if let Some(replay_log) = &mut self.history.replay_log {
            replay_log.push(Log::Register(RegisterLog {
                inference_variable,
                constraint_id,
                constraint,
            }));
        }
    }

    pub(super) fn add_constraint_combine_log(
        &mut self,
        constraint_id: ID<C>,
        old_constraint: C,
    ) {
        if let Some(replay_log) = &mut self.history.replay_log {
            replay_log.push(Log::Combine(CombineLog {
                constraint_id,
                old_constraint,
            }));
        }
    }

    pub(super) fn add_assign_to_known_log(
        &mut self,
        old_constraint: C,
        constraint_id: ID<C>,
        changed_variables: Vec<inference::Variable<C::Term>>,
    ) {
        if let Some(replay_log) = &mut self.history.replay_log {
            replay_log.push(Log::AssignToKnown(AssignToKnownLog {
                old_constraint,
                constraint_id,
                changed_variables,
            }));
        }
    }

    fn undo_log(&mut self, log: Log<C>) {
        match log {
            Log::Register(register_log) => {
                // remove the inference variable
                let removed_constraint_id = self
                    .inference_by_ids
                    .remove(&register_log.inference_variable)
                    .unwrap();

                let constraint_id =
                    removed_constraint_id.into_inferring().unwrap();

                // should be the same as it was registered
                assert_eq!(constraint_id, register_log.constraint_id);

                let constraint =
                    self.constraints.remove(constraint_id).unwrap();

                assert_eq!(constraint, register_log.constraint);
            }

            Log::Combine(combine) => {
                let current_constraint =
                    self.constraints.get_mut(combine.constraint_id).unwrap();

                *current_constraint = combine.old_constraint;
            }

            Log::AssignToKnown(assign_to_known_log) => {
                assert!(self
                    .constraints
                    .insert_with_id(
                        assign_to_known_log.constraint_id,
                        assign_to_known_log.old_constraint,
                    )
                    .is_ok());

                for inference_variable in assign_to_known_log.changed_variables
                {
                    *self
                        .inference_by_ids
                        .get_mut(&inference_variable)
                        .unwrap() = super::Inference::Inferring(
                        assign_to_known_log.constraint_id,
                    );
                }
            }
        }
    }
}
