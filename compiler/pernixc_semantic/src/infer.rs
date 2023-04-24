//! Implements the type inference algorithm for the Pernix programming language.

use std::{collections::HashMap, sync::atomic::AtomicUsize};

use enum_as_inner::EnumAsInner;
use getset::Getters;
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

/// Represents an information about a type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeVariableInfo {
    /// Gets the constraint that is associated with the type variable.
    #[get = "pub"]
    constraint: Constraint,
}

/// Represents a type variable that is used in the type inference algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariableID(usize);

/// Represents a type that is used in the type inference algorithm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum MonoType {
    TypeVariable(TypeVariableID),
    Type(Type),
}

/// Represents a result of retrieving a type with [`InferenceID`] from the [`InferenceContext`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Infererence<'a> {
    /// The type is successfully inferred.
    Inferred(&'a Type),

    /// The type is not inferred yet and the type variable info is returned.
    TypeVariable(&'a TypeVariableInfo),
}

/// Represents an identifier used to retrieve a type assignment from the [`InferenceContext`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferenceID(usize);

/// Is a struct used in type inference algorithm
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceContext {
    assignments: HashMap<InferenceID, MonoType>,
    type_variables: HashMap<TypeVariableID, TypeVariableInfo>,
}

impl InferenceContext {
    /// Creates a new type inference context with default values.
    #[must_use]
    pub fn new() -> Self {
        Self {
            assignments: HashMap::new(),
            type_variables: HashMap::new(),
        }
    }

    /// Creates a new inference variable with initial constraint.
    #[must_use]
    pub fn add_inference(&mut self, constraint: Constraint) -> InferenceID {
        // use atomic counter to get the next id
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let id = InferenceID(id);

        let type_variable = self.add_type_variable(constraint);

        self.assignments
            .insert(id, MonoType::TypeVariable(type_variable));

        id
    }

    /// Creates a new type variable.
    #[must_use]
    fn add_type_variable(&mut self, constraint: Constraint) -> TypeVariableID {
        // use atomic counter to get the next id
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let id = TypeVariableID(id);

        self.type_variables
            .insert(id, TypeVariableInfo { constraint });

        id
    }

    /// Gets the type of the given inference id.
    ///
    /// # Panics
    /// Panics if the `inference_id` was not from this context or invalid.
    #[must_use]
    pub fn get_inference(&self, inference_id: InferenceID) -> Infererence<'_> {
        match self.assignments.get(&inference_id).unwrap() {
            MonoType::TypeVariable(type_var) => {
                Infererence::TypeVariable(self.type_variables.get(type_var).unwrap())
            }
            MonoType::Type(concrete) => Infererence::Inferred(concrete),
        }
    }

    /// Adds a constraint to the given inference id.
    ///
    /// # Panics
    /// Panics if the `inference_id` was not from this context or invalid.
    ///
    /// # Errors
    /// - [`ConstraintAddError`]: if the inference id is already assigned to a concrete type and the
    ///   constraint is not satisfied by the concrete type.
    pub fn add_constraint(
        &mut self,
        inference_id: InferenceID,
        constraint: Constraint,
    ) -> Result<(), ConstraintAddError> {
        match *self.assignments.get(&inference_id).unwrap() {
            MonoType::TypeVariable(type_variable) => {
                let type_variable = self.type_variables.get_mut(&type_variable).unwrap();
                if constraint.is_more_constrainted_than(type_variable.constraint) {
                    type_variable.constraint = constraint;
                }
                Ok(())
            }
            MonoType::Type(concrete_type) => {
                if constraint.satisfies(concrete_type) {
                    Ok(())
                } else {
                    Err(ConstraintAddError(concrete_type))
                }
            }
        }
    }

    fn map_id_to_concrete(
        &mut self,
        variable_id: TypeVariableID,
        concrete: Type,
    ) -> Result<(), UnificationError> {
        for ty in self.assignments.values_mut() {
            let MonoType::TypeVariable(ty_var) = ty else {
                        continue;
                    };
            let ty_var = *ty_var;
            if ty_var != variable_id {
                continue;
            }

            if !self
                .type_variables
                .get(&ty_var)
                .unwrap()
                .constraint
                .satisfies(concrete)
            {
                return Err(UnificationError::ConstraintNotSatisfied {
                    constraint: self.type_variables.get(&ty_var).unwrap().constraint,
                    concrete_type: concrete,
                });
            }

            *ty = MonoType::Type(concrete);
        }

        // remove variable_id type variable
        self.type_variables.remove(&variable_id);

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
        match self.assignments.get(&inference_id).unwrap() {
            MonoType::TypeVariable(type_variable) => {
                self.map_id_to_concrete(*type_variable, concrete)
            }
            MonoType::Type(inferred) => {
                if inferred == &concrete {
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch {
                        left: *inferred,
                        right: concrete,
                    })
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
            self.assignments
                .get(&left)
                .unwrap()
                .as_type_variable()
                .copied(),
            self.assignments
                .get(&right)
                .unwrap()
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
                .get(&left)
                .unwrap()
                .constraint
                .is_more_constrainted_than(self.type_variables.get(&right).unwrap().constraint);

            let map_fn = |this: &mut Self, from: TypeVariableID, to: TypeVariableID| {
                // map from left to right in assignments
                for ty in this.assignments.values_mut() {
                    if let MonoType::TypeVariable(ty) = ty {
                        if *ty == from {
                            *ty = to;
                        }
                    }
                }
            };

            if left_more_constraint {
                map_fn(self, right, left);
                // remove right type variable
                self.type_variables.remove(&right);
            } else {
                map_fn(self, left, right);
                // remove left type variable
                self.type_variables.remove(&left);
            }

            Ok(())
        } else if let (Some(left), Some(right)) = (
            self.assignments
                .get(&left)
                .unwrap()
                .as_type_variable()
                .copied(),
            self.assignments.get(&right).unwrap().as_type().copied(),
        ) {
            self.map_id_to_concrete(left, right)
        } else if let (Some(left), Some(right)) = (
            self.assignments.get(&left).unwrap().as_type().copied(),
            self.assignments
                .get(&right)
                .unwrap()
                .as_type_variable()
                .copied(),
        ) {
            self.map_id_to_concrete(right, left)
        } else {
            let left = self.assignments.get(&left).unwrap().as_type().unwrap();
            let right = self.assignments.get(&right).unwrap().as_type().unwrap();
            if left == right {
                Ok(())
            } else {
                Err(UnificationError::TypeMismatch {
                    left: *left,
                    right: *right,
                })
            }
        }
    }
}

/// Represents an error that can occur during unification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error, EnumAsInner)]
#[allow(missing_docs)]
pub enum UnificationError {
    #[error("Attempted to unify two concrete types that are not the same.")]
    TypeMismatch { left: Type, right: Type },

    #[error(
        "Attempted to unify a type variable with a concrete type that does not satisfy the \
         constraint"
    )]
    ConstraintNotSatisfied {
        constraint: Constraint,
        concrete_type: Type,
    },
}

/// Represents an error that can occur when adding a constraint to an inference variable via
/// [`InferenceContext::add_constraint()`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error(
    "The inference variable is already assigned a concrete type but the concrete type does not \
     satisfy the constraint."
)]
pub struct ConstraintAddError(pub Type);

impl Default for InferenceContext {
    fn default() -> Self { Self::new() }
}

#[cfg(test)]
mod tests;
