//! Contains the semantic logic of the compiler (i.e. type checking/system).

use self::{
    mapping::Mapping,
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{SymbolKindID, Type},
        Symbol, Term,
    },
};
use crate::{
    semantic::substitution::{Substitute, Substitution},
    table::{Index, State, Table},
};

pub mod equality;
pub mod mapping;
pub mod predicate;
pub mod session;
pub mod substitution;
pub mod term;
pub mod unification;
pub mod visitor;

/// The foundation truth used to derive further arguments.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premise {
    /// The mapping of predefined equalities.
    pub equalities_mapping: Mapping,
}

/// A customization point for the semantic logic.
pub trait Semantic<T: Term> {
    /// Checks if the two given terms are trivially equal.
    fn trivially_equals(&mut self, lhs: &T, rhs: &T) -> bool;

    /// Normalizes the given term.
    ///
    /// The normalization allows the term to transform into a new equivalent term. Sometimes, it's
    /// not trivial to list all the possible equivalent terms into the premise.
    ///
    /// It's primarily used to resolve the trait-member terms or obtain the inferred term from an
    /// inference variable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    fn normalize<R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>>(
        &mut self,
        term: &T,
        premise: &Premise,
        table: &Table<impl State>,
        session: &mut Limit<R>,
    ) -> Result<Option<T>, ExceedLimitError>;
}

/// The basic implementation of the semantic logic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Default;

impl Semantic<Lifetime> for Default {
    fn trivially_equals(&mut self, lhs: &Lifetime, rhs: &Lifetime) -> bool { lhs == rhs }

    fn normalize<R: Session<Lifetime> + Session<Type> + Session<Constant>>(
        &mut self,
        _: &Lifetime,
        _: &Premise,
        _: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Option<Lifetime>, ExceedLimitError> {
        Ok(None)
    }
}

impl Semantic<Type> for Default {
    fn trivially_equals(&mut self, lhs: &Type, rhs: &Type) -> bool { lhs == rhs }

    fn normalize<R: Session<Lifetime> + Session<Type> + Session<Constant>>(
        &mut self,
        term: &Type,
        _: &Premise,
        table: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Option<Type>, ExceedLimitError> {
        match term {
            // transform type alias into the aliased type equivalent
            Type::Symbol(Symbol {
                id: SymbolKindID::Type(id),
                generic_arguments,
            }) => {
                let Some(type_sym) = table.get(*id) else {
                    return Ok(None);
                };

                let mut type_aliased = type_sym.r#type.clone();
                type_aliased.apply(&Substitution::from_generic_arguments(
                    generic_arguments.clone(),
                    (*id).into(),
                ));

                Ok(Some(type_aliased))
            }

            // TODO: Transform trait-type into the trait-implementation type equivalent.
            // TODO: Transform ADT-member-type into the aliased type equivalent.
            // TODO: Normalize the unpacked tuple type.
            _ => Ok(None),
        }
    }
}

impl Semantic<Constant> for Default {
    fn trivially_equals(&mut self, lhs: &Constant, rhs: &Constant) -> bool { lhs == rhs }

    fn normalize<R: Session<Lifetime> + Session<Type> + Session<Constant>>(
        &mut self,
        _: &Constant,
        _: &Premise,
        _: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Option<Constant>, ExceedLimitError> {
        // TODO: Implement this.
        Ok(None)
    }
}
