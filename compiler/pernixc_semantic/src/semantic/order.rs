//! Contains the implementation of the order of the generic arguments.
//!
//! This is primarily used to determine the specialization of the
//! implementation of the trait.

use super::{
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::{self, Constant},
        lifetime::Lifetime,
        r#type::{self, Type},
        GenericArguments, MemberSymbol, Term,
    },
    unification::{self, Unification},
    Premise, Semantic,
};
use crate::table::{State, Table};

/// The order in terms of specificity of the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

const fn lifetime_predicate(_: &Lifetime) -> bool { true }

fn type_predicate(x: &Type) -> bool {
    x.is_parameter()
        || matches!(
            x,
            Type::MemberSymbol(MemberSymbol {
                id: r#type::MemberSymbolKindID::Trait(_),
                ..
            })
        )
}

fn constant_predicate(x: &Constant) -> bool {
    x.is_parameter()
        || matches!(
            x,
            Constant::MemberSymbol(MemberSymbol {
                id: constant::MemberSymbolKindID::Trait(_),
                ..
            })
        )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OrderUnifyingConfig;

impl unification::Config for OrderUnifyingConfig {
    fn lifetime_unifiable(&mut self, _: &Lifetime, _: &Lifetime) -> bool {
        true
    }

    fn type_unifiable(&mut self, from: &Type, to: &Type) -> bool {
        type_predicate(from) || type_predicate(to)
    }

    fn constant_unifiable(&mut self, from: &Constant, to: &Constant) -> bool {
        constant_predicate(from) || constant_predicate(to)
    }
}

fn get_arguments_matching_count<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    this: &[T],
    other: &[T],
    predicate: &impl Fn(&T) -> bool,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Option<usize>, ExceedLimitError> {
    let mut count = 0;

    for (lifetime, other_lifetime) in this.iter().zip(other.iter()) {
        let Some(unification) = unification::unify(
            lifetime,
            other_lifetime,
            premise,
            table,
            &mut OrderUnifyingConfig,
            semantic,
            session,
        )?
        else {
            return Ok(None);
        };

        count += get_unification_matching_count(&unification, predicate);
    }

    Ok(Some(count))
}

fn get_generic_arguments_matching_count<
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    this: &GenericArguments,
    other: &GenericArguments,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Option<usize>, ExceedLimitError> {
    let (Some(lifetime_count), Some(ty_count), Some(constant_count)) = (
        get_arguments_matching_count(
            &this.lifetimes,
            &other.lifetimes,
            &lifetime_predicate,
            premise,
            table,
            semantic,
            session,
        )?,
        get_arguments_matching_count(
            &this.types,
            &other.types,
            &type_predicate,
            premise,
            table,
            semantic,
            session,
        )?,
        get_arguments_matching_count(
            &this.constants,
            &other.constants,
            &constant_predicate,
            premise,
            table,
            semantic,
            session,
        )?,
    ) else {
        return Ok(None);
    };

    Ok(Some(lifetime_count + ty_count + constant_count))
}

fn get_unification_matching_count<T: Term>(
    unification: &Unification<T>,
    predicate: impl Fn(&T) -> bool,
) -> usize {
    match &unification.r#match {
        unification::Match::Unifiable(from, _) => usize::from(predicate(from)),
        unification::Match::Substructural(substructural) => {
            substructural
                .lifetimes
                .values()
                .map(|x| get_unification_matching_count(x, lifetime_predicate))
                .sum::<usize>()
                + substructural
                    .types
                    .values()
                    .map(|x| get_unification_matching_count(x, type_predicate))
                    .sum::<usize>()
                + substructural
                    .constants
                    .values()
                    .map(|x| {
                        get_unification_matching_count(x, constant_predicate)
                    })
                    .sum::<usize>()
        }
        unification::Match::Equality => 0,
    }
}

impl GenericArguments {
    /// Determines the order of the generic arguments.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn order<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        other: &Self,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Order, ExceedLimitError> {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(Order::Incompatible);
        }

        let self_to_other = get_generic_arguments_matching_count(
            self, other, premise, table, semantic, session,
        )?;
        let other_to_self = get_generic_arguments_matching_count(
            other, self, premise, table, semantic, session,
        )?;

        match (self_to_other, other_to_self) {
            (None, None) => Ok(Order::Incompatible),
            (None, Some(_)) => Ok(Order::MoreSpecific),
            (Some(_), None) => Ok(Order::MoreGeneral),
            (Some(self_to_other), Some(other_to_self)) => {
                match self_to_other.cmp(&other_to_self) {
                    std::cmp::Ordering::Less => Ok(Order::MoreSpecific),
                    std::cmp::Ordering::Equal => Ok(Order::Ambiguous),
                    std::cmp::Ordering::Greater => Ok(Order::MoreGeneral),
                }
            }
        }
    }
}
