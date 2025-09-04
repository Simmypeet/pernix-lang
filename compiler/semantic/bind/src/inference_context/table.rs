//! Contains the definition of the inference table.

use std::{
    clone::Clone,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
};

use enum_as_inner::EnumAsInner;
use pernixc_arena::{Arena, ID};
use pernixc_term::inference;

use crate::inference_context::{
    constraint::Constraint, table::history::History,
};

pub mod history;

pub use history::Checkpoint;

/// Either the inference variable is known or is inferring.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Inference<C: Constraint> {
    Known(C::Term),
    Inferring(ID<C>),
}

/// The table that stores the inference variable and its state. Offering basic
/// operations to manage and query the inference variables.
#[derive(Debug)]
pub struct Table<C: Constraint> {
    inference_by_ids: HashMap<inference::Variable<C::Term>, Inference<C>>,
    constraints: Arena<C>,
    history: History<C>,
}

impl<C: Constraint> Default for Table<C> {
    fn default() -> Self {
        Self {
            inference_by_ids: HashMap::new(),
            constraints: Arena::new(),
            history: History::default(),
        }
    }
}

/// A view over the inference variable at a particular point in time whether or
/// not it's: inferred to a known value or is still being inferred.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum View<'x, C: Constraint> {
    /// Inferred to a known value.
    Known(&'x C::Term),

    /// Is still being inferred with the current constraint.
    Constraint(&'x C),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "failed to unify the inference variable with the constraint due to \
     unsatisfiable constraint"
)]
#[allow(missing_docs)]
pub struct UnsatisfiedConstraintError<C: Constraint> {
    /// The term that failed to satisfy the constraint.
    pub term: C::Term,

    /// The constraint that the term failed to satisfy.
    pub constraint: C,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("the two constraints cannot be combined")]
#[allow(missing_docs)]
pub struct CombineConstraintError<C> {
    /// The left-hand side constraint.
    pub lhs: C,

    /// The right-hand side constraint.
    pub rhs: C,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum AssignConstraintError<C: Constraint> {
    #[error(transparent)]
    UnsatisfiedConstraint(#[from] UnsatisfiedConstraintError<C>),

    #[error(transparent)]
    CombineConstraint(#[from] CombineConstraintError<C>),
}

impl<C: Constraint> Table<C> {
    /// Registers the inference variable to the table and assign the constraint
    /// to it.
    ///
    /// If this inference variable has already been registered, this function
    /// does nothing.
    pub fn register(
        &mut self,
        inference_variable: inference::Variable<C::Term>,
        constraint: C,
    ) -> bool {
        match self.inference_by_ids.entry(inference_variable) {
            Entry::Occupied(_) => false,
            Entry::Vacant(vacant_entry) => {
                let constraint_id = self.constraints.insert(constraint.clone());
                vacant_entry.insert(Inference::Inferring(constraint_id));

                self.add_register_log(
                    inference_variable,
                    constraint_id,
                    constraint,
                );

                true
            }
        }
    }

    /// Assigns a new constraint to an existing inference variable.
    ///
    /// Attempts to combine an existing constraint if the inference variable is
    /// still inferring.
    pub fn assign_constraint(
        &mut self,
        inference_variable: inference::Variable<C::Term>,
        constraint: C,
    ) -> Result<(), AssignConstraintError<C>> {
        let inference =
            self.inference_by_ids.get_mut(&inference_variable).expect(
                "inference variable must be registered before assigning a \
                 constraint",
            );

        match inference {
            Inference::Known(inferred) => {
                if !constraint.satisfies(inferred) {
                    return Err(AssignConstraintError::UnsatisfiedConstraint(
                        UnsatisfiedConstraintError {
                            term: inferred.clone(),
                            constraint,
                        },
                    ));
                }
                Ok(())
            }

            Inference::Inferring(id) => {
                let current_constraint = self.constraints.get_mut(*id).unwrap();

                let Some(combined) = current_constraint.combine(&constraint)
                else {
                    return Err(AssignConstraintError::CombineConstraint(
                        CombineConstraintError {
                            lhs: current_constraint.clone(),
                            rhs: constraint,
                        },
                    ));
                };

                let old_constraint = current_constraint.clone();
                *current_constraint = combined;

                let current_id = *id;

                self.add_constraint_combine_log(current_id, old_constraint);

                Ok(())
            }
        }
    }

    /// Assigns a known value to the constraint.
    pub fn assign_known(
        &mut self,
        constraint_id: ID<C>,
        known: C::Term,
    ) -> Result<(), UnsatisfiedConstraintError<C>> {
        // check if the known value satisfies the constraint
        let constraint =
            self.constraints.get(constraint_id).expect("invalid constraint ID");

        // check if the known value satisfies the constraint
        if !constraint.satisfies(&known) {
            return Err(UnsatisfiedConstraintError {
                term: known,
                constraint: constraint.clone(),
            });
        }

        let current_constraint = constraint.clone();
        let mut changed_variables = Vec::new();

        // replace every occurrence of the constraint with the known value
        for (id, infer) in &mut self.inference_by_ids {
            let Inference::Inferring(infer_constraint_id) = infer else {
                continue;
            };
            let infer_constraint_id = *infer_constraint_id;

            if infer_constraint_id != constraint_id {
                continue;
            }

            changed_variables.push(*id);
            *infer = Inference::Known(known.clone());
        }

        self.add_assign_to_known_log(
            current_constraint,
            constraint_id,
            changed_variables,
        );

        // remove the constraint
        assert!(self.constraints.remove(constraint_id).is_some());

        Ok(())
    }

    /// Attemps to unify two constraint variables.
    pub fn unify_infers(
        &mut self,
        left: ID<C>,
        right: ID<C>,
    ) -> Result<(), CombineConstraintError<C>> {
        if left == right {
            return Ok(());
        }

        let left_constraint = self.constraints.get(left).unwrap();
        let right_constraint = self.constraints.get(right).unwrap();

        let Some(combined) = left_constraint.combine(right_constraint) else {
            return Err(CombineConstraintError {
                lhs: left_constraint.clone(),
                rhs: right_constraint.clone(),
            });
        };

        let old_left_constraint = left_constraint.clone();
        let old_right_constraint = right_constraint.clone();

        *self.constraints.get_mut(left).unwrap() = combined;

        // remove the rhs id and then reassign all inference variables that
        // pointing to rhs constraint to lhs constraint
        self.constraints.remove(right).unwrap();
        let mut redirected_variables = Vec::new();

        for (id, infer) in &mut self.inference_by_ids {
            let Inference::Inferring(constraint_id) = infer else {
                continue;
            };
            let constraint_id = *constraint_id;

            if constraint_id != right {
                continue;
            }

            redirected_variables.push(*id);
            *infer = Inference::Inferring(left);
        }

        self.add_unify_constraint_log(
            left,
            right,
            old_left_constraint,
            old_right_constraint,
            redirected_variables,
        );

        Ok(())
    }

    /// Retrieves the current state of the inference variable. It can be either
    /// has been inferred to a known value or is still being inferred.
    #[must_use]
    pub fn get_view(
        &self,
        inference_variable: inference::Variable<C::Term>,
    ) -> Option<View<'_, C>> {
        match self.inference_by_ids.get(&inference_variable)? {
            Inference::Known(term) => Some(View::Known(term)),
            Inference::Inferring(constraint_id) => Some(View::Constraint(
                self.constraints.get(*constraint_id).unwrap(),
            )),
        }
    }

    /// Retrieves the current inference state of the variable.
    #[must_use]
    pub fn get_inference(
        &self,
        inference_variable: inference::Variable<C::Term>,
    ) -> Option<&Inference<C>> {
        self.inference_by_ids.get(&inference_variable)
    }
}

#[cfg(test)]
mod test;
