use std::{
    clone::Clone,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
};

use enum_as_inner::EnumAsInner;
use pernixc_arena::{Arena, ID};
use pernixc_term::inference;

use crate::inference_context::table::history::History;

mod history;

pub use history::Checkpoint;

/// Implements by a constraint type. Representing a restrict domain of what
/// terms can be inferred.
pub trait Constraint: Debug + Clone + Eq {
    /// The type of terms that can be inferred.
    type Term: Debug + Clone + Eq + Hash;

    /// Checks if the given term satisfies the constraint.
    fn satisfies(&self, term: &Self::Term) -> bool;

    /// Tries to combine this constraint with another constraint.
    fn combine(&self, another: &Self) -> Option<Self>
    where
        Self: Sized;
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum AssignKnownValueError<C: Constraint> {
    #[error(transparent)]
    UnsatisfiedConstraintError(#[from] UnsatisfiedConstraintError<C>),
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
    pub fn assign_infer_to_known(
        &mut self,
        constraint_id: ID<C>,
        known: C::Term,
    ) -> Result<(), AssignKnownValueError<C>> {
        // check if the known value satisfies the constraint
        let constraint =
            self.constraints.get(constraint_id).expect("invalid constraint ID");

        // check if the known value satisfies the constraint
        if !constraint.satisfies(&known) {
            return Err(AssignKnownValueError::UnsatisfiedConstraintError(
                UnsatisfiedConstraintError {
                    term: known,
                    constraint: constraint.clone(),
                },
            ));
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
}
