//! Implements the type inference algorithm for the Pernix programming language.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_system::{
    arena::{Arena, InvalidIDError},
    create_symbol,
};
use thiserror::Error;

use crate::symbol::ty::{PrimitiveType, Type};

/// Represents a constraint that is used in the type variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// The type can be any type.
    All = 0,

    /// The type can be any number type. (`i32-64`, `u32-64`, `f32-64`)
    Number = 1,

    /// The type can be only signed number types. (`i32-64`, `f32-64`)
    Signed = 2,

    /// The type can be only floating point number types. (`f32-64`)
    Float = 3,
}

impl Constraint {
    /// Returns true if the constraint is more constrainted than the other constraint.
    #[must_use]
    pub fn is_more_constrainted_than(self, other: Self) -> bool { self as usize > other as usize }

    /// Returns true if the constraint is satisfied by the concrete type.
    #[must_use]
    pub fn satisfies(self, concrete_type: Type) -> bool {
        match self {
            Self::All => true,
            Self::Number => matches!(
                concrete_type,
                Type::PrimitiveType(
                    PrimitiveType::Float32
                        | PrimitiveType::Float64
                        | PrimitiveType::Int8
                        | PrimitiveType::Int16
                        | PrimitiveType::Int32
                        | PrimitiveType::Int64
                        | PrimitiveType::Uint8
                        | PrimitiveType::Uint16
                        | PrimitiveType::Uint32
                        | PrimitiveType::Uint64
                )
            ),
            Self::Signed => matches!(
                concrete_type,
                Type::PrimitiveType(
                    PrimitiveType::Float32
                        | PrimitiveType::Float64
                        | PrimitiveType::Int8
                        | PrimitiveType::Int16
                        | PrimitiveType::Int32
                        | PrimitiveType::Int64
                )
            ),
            Self::Float => matches!(
                concrete_type,
                Type::PrimitiveType(PrimitiveType::Float32 | PrimitiveType::Float64)
            ),
        }
    }
}

create_symbol! {
    /// Represents an information about a type variable.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
    pub struct TypeVariable {
        /// Gets the constraint that is associated with the type variable.
        #[get = "pub"]
        constraint: Constraint,
    }
}

create_symbol! {
    /// Represnts a new type assignment in the inference context.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, CopyGetters)]
    pub struct Inference {
        /// Gets the type variable that is assigned.
        #[get_copy = "pub"]
        mono_type: MonoType
    }
}

/// Represents a type that is used in the type inference algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum MonoType {
    TypeVariable(TypeVariableID),
    Type(Type),
}

/// Represents a result of retrieving a type with [`InferenceID`] from the [`InferenceContext`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum InferableType {
    /// The type is successfully inferred.
    Type(Type),

    /// The type is not yet inferred and is represented by a type variable.
    Constraint(Constraint),
}

/// Is a struct used in type inference algorithm
#[derive(Debug)]
pub struct InferenceContext {
    inferences: Arena<Inference>,
    type_variables: Arena<TypeVariable>,
}

impl InferenceContext {
    /// Creates a new type inference context with default values.
    #[must_use]
    pub fn new() -> Self {
        Self {
            inferences: Arena::new(),
            type_variables: Arena::new(),
        }
    }

    /// Creates a new inference variable with initial constraint.
    #[must_use]
    pub fn new_inference(&mut self, constraint: Constraint) -> InferenceID {
        let type_variable = self.new_type_variable(constraint);
        self.inferences.insert(Inference {
            mono_type: MonoType::TypeVariable(type_variable),
        })
    }

    /// Creates a new type variable.
    #[must_use]
    fn new_type_variable(&mut self, constraint: Constraint) -> TypeVariableID {
        self.type_variables.insert(TypeVariable { constraint })
    }

    /// Returns the inference state of the given [`InferenceID`] at the time of the call.
    ///
    /// # Errors
    /// Returns [`InvalidIDError`] if the given [`InferenceID`] is invalid.
    pub fn get_inferable_type(
        &self,
        inference_id: InferenceID,
    ) -> Result<InferableType, InvalidIDError> {
        Ok(match self.inferences.get(inference_id)?.mono_type {
            MonoType::TypeVariable(type_var) => {
                InferableType::Constraint(*self.type_variables.get(type_var)?.constraint())
            }
            MonoType::Type(concrete) => InferableType::Type(concrete),
        })
    }

    /// Adds a constraint to the given inference id.
    ///
    /// # Panics
    /// Panics if the `inference_id` was not from this context or invalid.
    ///
    /// # Errors
    /// - [`ConstraintAddError`]: if the inference id is already assigned to a concrete type and the
    ///   constraint is not satisfied by the concrete type.
    pub fn unify_with_constraint(
        &mut self,
        inference_id: InferenceID,
        constraint: Constraint,
    ) -> Result<(), UnificationError> {
        match self.inferences.get(inference_id)?.mono_type {
            MonoType::TypeVariable(type_variable) => {
                let type_variable = self.type_variables.get_mut(type_variable)?;
                if constraint.is_more_constrainted_than(type_variable.constraint) {
                    type_variable.constraint = constraint;
                }
                Ok(())
            }
            MonoType::Type(concrete_type) => {
                if constraint.satisfies(concrete_type) {
                    Ok(())
                } else {
                    Err(ConstraintNotSatisfiedError {
                        constraint,
                        concrete_type,
                    }
                    .into())
                }
            }
        }
    }

    fn map_id_to_concrete(
        &mut self,
        variable_id: TypeVariableID,
        concrete: Type,
    ) -> Result<(), UnificationError> {
        for infer in self.inferences.values_mut() {
            let MonoType::TypeVariable(ty_var) = infer.mono_type else {
                continue;
            };

            if ty_var != variable_id {
                continue;
            }

            if !self
                .type_variables
                .get(ty_var)?
                .constraint
                .satisfies(concrete)
            {
                return Err(ConstraintNotSatisfiedError {
                    constraint: self.type_variables.get(ty_var)?.constraint,
                    concrete_type: concrete,
                }
                .into());
            }

            infer.mono_type = MonoType::Type(concrete);
        }

        // remove variable_id type variable
        self.type_variables.remove(variable_id)?;

        Ok(())
    }

    /// Unifies the inference variable with the given concrete type.
    ///
    /// # Errors
    /// - [`UnificationError::TypeMismatch`]: The concrete type of the two inference variables do
    ///   not match.
    /// - [`UnificationError::ConstraintNotSatisfied`]: One of the inference variables got assigned
    ///  a concrete type that does not satisfy the constraint.
    pub fn unify_with_concrete(
        &mut self,
        inference_id: InferenceID,
        concrete: Type,
    ) -> Result<(), UnificationError> {
        match self.inferences.get(inference_id)?.mono_type {
            MonoType::TypeVariable(type_variable) => {
                self.map_id_to_concrete(type_variable, concrete)
            }
            MonoType::Type(inferred) => {
                if inferred == concrete {
                    Ok(())
                } else {
                    Err(TypeMismatchError {
                        left: inferred,
                        right: concrete,
                    }
                    .into())
                }
            }
        }
    }

    /// Unifies two inference variables together.
    ///
    /// # Errors
    /// - [`UnificationError::TypeMismatch`]: The concrete type of the two inference variables do
    ///   not match.
    /// - [`UnificationError::ConstraintNotSatisfied`]: One of the inference variables got assigned
    ///  a concrete type that does not satisfy the constraint.
    #[allow(clippy::too_many_lines)]
    pub fn unify(&mut self, left: InferenceID, right: InferenceID) -> Result<(), UnificationError> {
        if let (Some(left), Some(right)) = (
            self.inferences
                .get(left)?
                .mono_type
                .as_type_variable()
                .copied(),
            self.inferences
                .get(right)?
                .mono_type
                .as_type_variable()
                .copied(),
        ) {
            if left == right {
                return Ok(());
            }

            // Check if the left type variable is more constrainted than the right type
            // variable.
            let left_more_constraint = self
                .type_variables
                .get(left)?
                .constraint
                .is_more_constrainted_than(self.type_variables.get(right)?.constraint);

            let map_fn = |this: &mut Self, from: TypeVariableID, to: TypeVariableID| {
                // map from left to right in assignments
                for infer in this.inferences.values_mut() {
                    if let MonoType::TypeVariable(ty) = &mut infer.mono_type {
                        if *ty == from {
                            *ty = to;
                        }
                    }
                }
            };

            if left_more_constraint {
                map_fn(self, right, left);
                // remove right type variable
                self.type_variables.remove(right)?;
            } else {
                map_fn(self, left, right);
                // remove left type variable
                self.type_variables.remove(left)?;
            }

            Ok(())
        } else if let (Some(left), Some(right)) = (
            self.inferences
                .get(left)?
                .mono_type
                .as_type_variable()
                .copied(),
            self.inferences.get(right)?.mono_type.as_type().copied(),
        ) {
            self.map_id_to_concrete(left, right)
        } else if let (Some(left), Some(right)) = (
            self.inferences.get(left)?.mono_type.as_type().copied(),
            self.inferences
                .get(right)?
                .mono_type
                .as_type_variable()
                .copied(),
        ) {
            self.map_id_to_concrete(right, left)
        } else {
            let left = self.inferences.get(left)?.mono_type.as_type().unwrap();
            let right = self.inferences.get(right)?.mono_type.as_type().unwrap();
            if left == right {
                Ok(())
            } else {
                Err(TypeMismatchError {
                    left: *left,
                    right: *right,
                }
                .into())
            }
        }
    }
}

/// Is an error ocurred when trying to unify two concrete types that are not the same.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("Attempted to unify two concrete types that are not the same.")]
pub struct TypeMismatchError {
    /// The left type.
    pub left: Type,

    /// The right type.
    pub right: Type,
}

/// Is an error ocurred when trying to unify a constraint with a conrete type that doesn't satisfy
/// it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The concrete type doesn't satisfy particular constraint")]
pub struct ConstraintNotSatisfiedError {
    /// The constraint.
    pub constraint: Constraint,

    /// The concrete type.
    pub concrete_type: Type,
}

/// Represents an error that can occur during unification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum UnificationError {
    #[error("{0}")]
    TypeMismatchError(TypeMismatchError),
    #[error("{0}")]
    ConstraintNotSatisfiedError(ConstraintNotSatisfiedError),
    #[error("{0}")]
    InvalidIDError(InvalidIDError),
}

impl Default for InferenceContext {
    fn default() -> Self { Self::new() }
}

#[cfg(test)]
mod tests;
