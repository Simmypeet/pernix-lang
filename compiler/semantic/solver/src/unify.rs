use pernixc_qbice::Interner;
use pernixc_type::{
    substitution::{Substitutable, Substitution},
    r#type::Type2,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
    type_relation::TypeRelation,
};

#[cfg(test)]
mod test;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unify {
    left: Interned<Type2>,
    right: Interned<Type2>,
}

impl Unify {
    #[must_use]
    pub const fn new(left: Interned<Type2>, right: Interned<Type2>) -> Self {
        Self { left, right }
    }

    #[must_use]
    pub const fn left(&self) -> &Interned<Type2> { &self.left }

    #[must_use]
    pub const fn right(&self) -> &Interned<Type2> { &self.right }

    #[must_use]
    pub fn into_type_relation(self) -> TypeRelation {
        TypeRelation::invariant(self.left, self.right)
    }
}

impl From<TypeRelation> for Unify {
    fn from(relation: TypeRelation) -> Self {
        Self::new(relation.left().clone(), relation.right().clone())
    }
}

impl Substitutable for Unify {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl Interner,
    ) -> Option<Self> {
        match (
            self.left.apply(subst, interner),
            self.right.apply(subst, interner),
        ) {
            (Some(left), Some(right)) => Some(Self { left, right }),
            (Some(left), None) => {
                Some(Self { left, right: self.right.clone() })
            }
            (None, Some(right)) => {
                Some(Self { left: self.left.clone(), right })
            }
            (None, None) => None,
        }
    }
}

impl Solver<'_> {
    pub async fn unify(
        &mut self,
        left: Interned<Type2>,
        right: Interned<Type2>,
    ) -> Result<Option<(Substitution, Constraints)>, OverflowError> {
        let (substitution, residual_relations, constraints) = self
            .resolve_type_relations(vec![TypeRelation::invariant(left, right)])
            .await?;

        if residual_relations.is_empty() {
            Ok(Some((substitution, constraints)))
        } else {
            Ok(None)
        }
    }

    pub async fn resolve_unification_constraints(
        &mut self,
        unifications: Vec<Unify>,
    ) -> Result<(Substitution, Vec<Unify>, Constraints), OverflowError> {
        let (substitution, residual_relations, constraints) = self
            .resolve_type_relations(
                unifications
                    .into_iter()
                    .map(Unify::into_type_relation)
                    .collect(),
            )
            .await?;

        Ok((
            substitution,
            residual_relations.iter().cloned().map(Unify::from).collect(),
            constraints,
        ))
    }
}
