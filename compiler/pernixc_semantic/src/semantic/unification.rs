//! Contains the code related to unification of entities.

use std::collections::hash_map::Entry;

use super::{
    map::Map,
    model::Model,
    predicate::Premises,
    session::Session,
    substitution::{Substitute, Substitution},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments},
    Semantic, Term,
};
use crate::table::{State, Table};

/// Describes which types of terms can be unified.
pub trait Config<T: ?Sized> {
    /// Returns `true` if the given terms can be unified.
    fn unifiable(&mut self, lhs: &T, rhs: &T) -> bool;
}

/// Represents a record of unifying two terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Record<'a, T> {
    pub lhs: &'a T,
    pub rhs: &'a T,
}

/// An error that occurs during unification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, thiserror::Error)]
#[error("could not unify the given terms")]
pub struct Error;

/// A type alias for the result of unification.
pub type Result = std::result::Result<(), Error>;

/// A unifier for unification algorithm.
///
/// Normally, this is used internally by the unification algorithm. To use the unfication algorithm,
/// use [`unify()`] function.
#[derive(Debug, PartialEq, Eq)]
pub struct Unifier<'a, M: Model> {
    exisitng: &'a mut Substitution<M>,
}

impl<'a, M: Model> Unifier<'a, M> {
    /// Unifies the given terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the given terms cannot be unified.
    #[allow(clippy::too_many_arguments)]
    pub fn unify<
        T: Term<Model = M>,
        C: Config<T>
            + Config<Type<<T as Term>::Model>>
            + Config<Lifetime<<T as Term>::Model>>
            + Config<Constant<<T as Term>::Model>>,
        S: Semantic<T> + Semantic<Type<M>> + Semantic<Lifetime<M>> + Semantic<Constant<M>>,
        R: Session<T> + Session<Type<M>> + Session<Lifetime<M>> + Session<Constant<M>>,
    >(
        &mut self,
        lhs: &T,
        rhs: &T,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut R,
        config: &mut C,
    ) -> Result {
        unify_internal(
            lhs,
            rhs,
            premises,
            table,
            semantic,
            session,
            config,
            self.exisitng,
        )
    }
}

impl<M: Model> GenericArguments<M> {
    /// Unifies the given generic arguments.
    #[must_use]
    pub fn unify<
        C: Config<Type<M>> + Config<Lifetime<M>> + Config<Constant<M>>,
        S: Semantic<Type<M>> + Semantic<Lifetime<M>> + Semantic<Constant<M>>,
        R: Session<Type<M>> + Session<Lifetime<M>> + Session<Constant<M>>,
    >(
        &self,
        other: &Self,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut R,
        config: &mut C,
    ) -> Option<Substitution<M>> {
        if self.types.len() != other.types.len()
            || self.lifetimes.len() != other.lifetimes.len()
            || self.constants.len() != other.constants.len()
        {
            return None;
        }

        let mut existing = Substitution::default();

        for (lhs, rhs) in self.types.iter().zip(other.types.iter()) {
            unify_internal(
                lhs,
                rhs,
                premises,
                table,
                semantic,
                session,
                config,
                &mut existing,
            )
            .ok()?;
        }

        for (lhs, rhs) in self.lifetimes.iter().zip(other.lifetimes.iter()) {
            unify_internal(
                lhs,
                rhs,
                premises,
                table,
                semantic,
                session,
                config,
                &mut existing,
            )
            .ok()?;
        }

        for (lhs, rhs) in self.constants.iter().zip(other.constants.iter()) {
            unify_internal(
                lhs,
                rhs,
                premises,
                table,
                semantic,
                session,
                config,
                &mut existing,
            )
            .ok()?;
        }

        Some(existing)
    }
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn unify_internal<
    T: Term,
    C: Config<T>
        + Config<Type<<T as Term>::Model>>
        + Config<Lifetime<<T as Term>::Model>>
        + Config<Constant<<T as Term>::Model>>,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
    config: &mut C,
    existing: &mut Substitution<<T as Term>::Model>,
) -> Result {
    if !session.mark_as_working_on(Record { lhs, rhs }) {
        return Err(Error);
    }

    if config.unifiable(lhs, rhs) {
        match <T as Substitute>::get_mut(existing).entry(lhs.clone()) {
            // already mapped, check if the rhs is equal to the mapped value
            Entry::Occupied(entry) => {
                if !entry.get().equals(rhs, premises, table, semantic, session) {
                    session.mark_as_done(Record { lhs, rhs });
                    return Err(Error);
                }
            }

            // not mapped, map the lhs to the rhs
            Entry::Vacant(entry) => {
                entry.insert(rhs.clone());
            }
        }

        session.mark_as_done(Record { lhs, rhs });
        return Ok(());
    }

    let existing_clone = existing.clone();
    let mut unifier = Unifier { exisitng: existing };

    // sub structurally unify
    if semantic
        .sub_structural_unify(lhs, rhs, premises, table, session, config, &mut unifier)
        .is_ok()
    {
        session.mark_as_done(Record { lhs, rhs });
        return Ok(());
    }

    // restore the existing substitution
    *unifier.exisitng = existing_clone.clone();

    // try to unify by looking for equivalent terms
    for (lhs_eq, rhs_eqs) in <T as Map>::get(&premises.mapping) {
        if !rhs.equals(lhs_eq, premises, table, semantic, session) {
            continue;
        }

        for rhs_eq in rhs_eqs {
            match unify_internal(
                lhs,
                rhs_eq,
                premises,
                table,
                semantic,
                session,
                config,
                unifier.exisitng,
            ) {
                Ok(()) => {
                    session.mark_as_done(Record { lhs, rhs });
                    return Ok(());
                }
                Err(Error) => {
                    // restore the existing substitution
                    *unifier.exisitng = existing_clone.clone();
                }
            }
        }
    }

    if let Some(lhs_normalized) = lhs.normalize(premises, table, semantic, session) {
        match unify_internal(
            &lhs_normalized,
            rhs,
            premises,
            table,
            semantic,
            session,
            config,
            unifier.exisitng,
        ) {
            Ok(()) => {
                session.mark_as_done(Record { lhs, rhs });
                Ok(())
            }
            Err(Error) => {
                //restore the existing substitution
                session.mark_as_done(Record { lhs, rhs });

                *unifier.exisitng = existing_clone;
                Err(Error)
            }
        }
    } else if let Some(rhs_normalized) = rhs.normalize(premises, table, semantic, session) {
        match unify_internal(
            lhs,
            &rhs_normalized,
            premises,
            table,
            semantic,
            session,
            config,
            unifier.exisitng,
        ) {
            Ok(()) => {
                session.mark_as_done(Record { lhs, rhs });
                Ok(())
            }
            Err(Error) => {
                //restore the existing substitution
                session.mark_as_done(Record { lhs, rhs });
                *unifier.exisitng = existing_clone;
                Err(Error)
            }
        }
    } else {
        Err(Error)
    }
}

pub(super) fn unify<
    T: Term,
    C: Config<T>
        + Config<Type<<T as Term>::Model>>
        + Config<Lifetime<<T as Term>::Model>>
        + Config<Constant<<T as Term>::Model>>,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
    config: &mut C,
) -> std::result::Result<Substitution<<T as Term>::Model>, Error> {
    let mut existing = Substitution::default();

    unify_internal(
        lhs,
        rhs,
        premises,
        table,
        semantic,
        session,
        config,
        &mut existing,
    )
    .map(|()| existing)
}

pub(super) fn sub_structural_unify<
    T: Term,
    C: Config<T>
        + Config<Type<<T as Term>::Model>>
        + Config<Lifetime<<T as Term>::Model>>
        + Config<Constant<<T as Term>::Model>>,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
>(
    lhs: &T,
    rhs: &T,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut R,
    config: &mut C,
) -> std::result::Result<Substitution<<T as Term>::Model>, Error> {
    let mut existing = Substitution::default();
    let mut unifier = Unifier {
        exisitng: &mut existing,
    };

    semantic
        .sub_structural_unify(lhs, rhs, premises, table, session, config, &mut unifier)
        .map(|()| existing)
}

#[cfg(test)]
mod tests;
