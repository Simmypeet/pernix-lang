//! Contains code related to logic applied to the entities.

use self::{
    definite::Definitiveness,
    model::{Entity, Model},
    predicate::Premises,
    session::Session,
    substitution::Substitute,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Term, Tuple,
        TupleElement, Unpacked,
    },
    unification::{Config, Unifier},
};
use crate::table::{Index, Table};

pub mod definite;
pub mod equality;
pub mod map;
pub mod model;
pub mod pattern;
pub mod predicate;
pub mod session;
pub mod substitution;
pub mod term;
pub mod r#trait;
pub mod unification;
pub mod visitor;

/// Implements the basic semantic logic used to reason about the entities.
pub trait Semantic<T: Term> {
    /// Gets the [`Definitiveness`] of the given term.
    fn definitetiveness(&mut self, term: &T) -> Definitiveness;

    /// Checks if the two given terms are trivally equal.
    ///
    /// This is used to check if the two terms are equal without any further reasoning.
    fn trivially_equals(&mut self, lhs: &T, rhs: &T) -> bool;

    /// Normalizes the given term.
    fn normalize<
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    >(
        &mut self,
        term: &T,
        premises: &Premises<<T as Term>::Model>,
        table: &Table,
        session: &mut R,
    ) -> Option<T>;

    /// Sub-structurally unifies the two given terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms cannot be unified.
    #[allow(clippy::too_many_arguments)]
    fn sub_structural_unify_internal<
        C: Config<T>
            + Config<Type<<T as Term>::Model>>
            + Config<Constant<<T as Term>::Model>>
            + Config<Lifetime<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>,
    >(
        &mut self,
        lhs: &T,
        rhs: &T,
        premises: &Premises<<T as Term>::Model>,
        table: &Table,
        session: &mut R,
        config: &mut C,
        unifier: &mut Unifier<'_, <T as Term>::Model>,
    ) -> unification::Result;
}

/// The logic object providing fundemental logic for reasoning about the system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Default;

impl<S: Model> Semantic<Type<S>> for Default {
    fn definitetiveness(&mut self, term: &Type<S>) -> Definitiveness {
        match term {
            Type::Primitive(_) => Definitiveness::Definite,
            Type::Parameter(_) | Type::Inference(_) => Definitiveness::Indefinite,
            Type::Algebraic(_)
            | Type::Local(_)
            | Type::Tuple(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::TraitMember(_) => Definitiveness::Applicative,
        }
    }

    fn trivially_equals(&mut self, lhs: &Type<S>, rhs: &Type<S>) -> bool { lhs == rhs }

    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &mut self,
        term: &Type<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table,
        session: &mut R,
    ) -> Option<Type<S>> {
        let Type::TraitMember(trait_member) = term else {
            return None;
        };

        let resolved_implementation = table
            .resolve_implementation(
                table.get(trait_member.trait_type_id)?.parent_trait_id,
                &trait_member.trait_generic_arguments,
                premises,
                self,
                session,
            )
            .ok()?;

        let implementation_type_id = table
            .get(resolved_implementation.implementation_id)?
            .implementation_type_ids_by_trait_type_id
            .get(&trait_member.trait_type_id)
            .copied()?;

        let mut equivalent = table
            .get(implementation_type_id)
            .map(|x| x.r#type.clone())?
            .into_other_model();

        let substitution = {
            resolved_implementation
                .deduced_unification
                .append_from_generic_arguments(
                    trait_member.member_generic_arguments.clone(),
                    implementation_type_id.into(),
                )?
        };

        equivalent.apply(&substitution);

        Some(equivalent)
    }

    #[allow(clippy::too_many_lines)]
    fn sub_structural_unify_internal<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &mut self,
        lhs: &Type<S>,
        rhs: &Type<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table,
        session: &mut R,
        config: &mut C,
        unifier: &mut Unifier<'_, <Type<S> as Term>::Model>,
    ) -> unification::Result {
        match (lhs, rhs) {
            (Type::Local(lhs), Type::Local(rhs)) => {
                unifier.unify(&*lhs.0, &*rhs.0, premises, table, self, session, config)
            }

            (Type::Pointer(lhs), Type::Pointer(rhs)) if lhs.qualifier == rhs.qualifier => unifier
                .unify(
                    &*lhs.pointee,
                    &*rhs.pointee,
                    premises,
                    table,
                    self,
                    session,
                    config,
                ),

            (Type::Algebraic(lhs), Type::Algebraic(rhs)) if lhs.kind == rhs.kind => {
                sub_structural_unify_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )
            }

            (Type::Reference(lhs), Type::Reference(rhs)) if lhs.qualifier == rhs.qualifier => {
                unifier.unify(
                    &lhs.lifetime,
                    &rhs.lifetime,
                    premises,
                    table,
                    self,
                    session,
                    config,
                )?;
                unifier.unify(
                    &*lhs.pointee,
                    &*rhs.pointee,
                    premises,
                    table,
                    self,
                    session,
                    config,
                )
            }

            (Type::Array(lhs), Type::Array(rhs)) => {
                unifier.unify(
                    &lhs.length,
                    &rhs.length,
                    premises,
                    table,
                    self,
                    session,
                    config,
                )?;

                unifier.unify(
                    &*lhs.element,
                    &*rhs.element,
                    premises,
                    table,
                    self,
                    session,
                    config,
                )
            }

            (Type::TraitMember(lhs), Type::TraitMember(rhs))
                if lhs.trait_type_id == rhs.trait_type_id =>
            {
                sub_structural_unify_generic_arguments(
                    &lhs.trait_generic_arguments,
                    &rhs.trait_generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )?;

                sub_structural_unify_generic_arguments(
                    &lhs.member_generic_arguments,
                    &rhs.member_generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )
            }

            (Type::Tuple(lhs), Type::Tuple(rhs)) if tuple_unifiable(lhs, rhs) => {
                sub_structural_unify_tuple(
                    lhs, rhs, premises, table, self, session, config, unifier,
                )
            }

            (lhs, rhs) => {
                if lhs.equals(rhs, premises, table, self, session) {
                    Ok(())
                } else {
                    Err(unification::Error)
                }
            }
        }
    }
}

impl<S: Model> Semantic<Constant<S>> for Default {
    fn definitetiveness(&mut self, term: &Constant<S>) -> Definitiveness {
        match term {
            Constant::Primitive(_) => Definitiveness::Definite,
            Constant::Parameter(_) | Constant::Inference(_) => Definitiveness::Indefinite,
            Constant::Struct(_)
            | Constant::Enum(_)
            | Constant::Array(_)
            | Constant::TraitMember(_)
            | Constant::Local(_)
            | Constant::Tuple(_) => Definitiveness::Applicative,
        }
    }

    fn trivially_equals(&mut self, lhs: &Constant<S>, rhs: &Constant<S>) -> bool { lhs == rhs }

    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &mut self,
        term: &Constant<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table,
        session: &mut R,
    ) -> Option<Constant<S>> {
        let Constant::TraitMember(trait_member) = term else {
            return None;
        };

        let resolved_implementation = table
            .resolve_implementation(
                table.get(trait_member.trait_constant_id)?.parent_trait_id,
                &trait_member.trait_arguments,
                premises,
                self,
                session,
            )
            .ok()?;

        let implementation_constant_id = table
            .get(resolved_implementation.implementation_id)?
            .implementation_constant_ids_by_trait_constant_id
            .get(&trait_member.trait_constant_id)
            .copied()?;

        let mut equivalent = table
            .get(implementation_constant_id)
            .map(|x| x.constant.clone())?
            .into_other_model();

        let substitution = resolved_implementation.deduced_unification;

        equivalent.apply(&substitution);

        Some(equivalent)
    }

    fn sub_structural_unify_internal<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &mut self,
        lhs: &Constant<S>,
        rhs: &Constant<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table,
        session: &mut R,
        config: &mut C,
        unifier: &mut Unifier<'_, <Type<S> as Term>::Model>,
    ) -> unification::Result {
        match (lhs, rhs) {
            (Constant::Struct(lhs), Constant::Struct(rhs))
                if lhs.struct_id == rhs.struct_id && lhs.fields.len() == rhs.fields.len() =>
            {
                sub_structural_unify_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )?;

                for (lhs, rhs) in lhs.fields.iter().zip(rhs.fields.iter()) {
                    unifier.unify(lhs, rhs, premises, table, self, session, config)?;
                }

                Ok(())
            }

            (Constant::Enum(lhs), Constant::Enum(rhs))
                if lhs.variant_id == rhs.variant_id
                    && lhs.associated_value.is_some() == rhs.associated_value.is_some() =>
            {
                sub_structural_unify_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )?;

                match (&lhs.associated_value, &rhs.associated_value) {
                    (Some(lhs), Some(rhs)) => unifier.unify(
                        lhs.as_ref(),
                        rhs.as_ref(),
                        premises,
                        table,
                        self,
                        session,
                        config,
                    ),

                    (None, None) => Ok(()),

                    _ => unreachable!(),
                }
            }

            (Constant::Array(lhs), Constant::Array(rhs))
                if lhs.elements.len() == rhs.elements.len() =>
            {
                unifier.unify(
                    &*lhs.element_ty,
                    &*rhs.element_ty,
                    premises,
                    table,
                    self,
                    session,
                    config,
                )?;

                for (lhs, rhs) in lhs.elements.iter().zip(rhs.elements.iter()) {
                    unifier.unify(lhs, rhs, premises, table, self, session, config)?;
                }

                Ok(())
            }

            (Constant::TraitMember(lhs), Constant::TraitMember(rhs))
                if lhs.trait_constant_id == rhs.trait_constant_id =>
            {
                sub_structural_unify_generic_arguments(
                    &lhs.trait_arguments,
                    &rhs.trait_arguments,
                    premises,
                    table,
                    self,
                    session,
                    config,
                    unifier,
                )?;

                Ok(())
            }

            (Constant::Tuple(lhs), Constant::Tuple(rhs)) if tuple_unifiable(lhs, rhs) => {
                sub_structural_unify_tuple(
                    lhs, rhs, premises, table, self, session, config, unifier,
                )
            }

            (lhs, rhs) => {
                if lhs.equals(rhs, premises, table, self, session) {
                    Ok(())
                } else {
                    Err(unification::Error)
                }
            }
        }
    }
}

impl<S: Model> Semantic<Lifetime<S>> for Default {
    fn definitetiveness(&mut self, _: &Lifetime<S>) -> Definitiveness { Definitiveness::Definite }

    fn trivially_equals(&mut self, lhs: &Lifetime<S>, rhs: &Lifetime<S>) -> bool { lhs == rhs }

    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &mut self,
        _: &Lifetime<S>,
        _: &Premises<<Type<S> as Term>::Model>,
        _: &Table,
        _: &mut R,
    ) -> Option<Lifetime<S>> {
        None
    }

    fn sub_structural_unify_internal<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &mut self,
        lhs: &Lifetime<S>,
        rhs: &Lifetime<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table,
        session: &mut R,
        _: &mut C,
        _: &mut Unifier<'_, <Type<S> as Term>::Model>,
    ) -> unification::Result {
        if lhs.equals(rhs, premises, table, self, session) {
            Ok(())
        } else {
            Err(unification::Error)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn sub_structural_unify_generic_arguments<
    M: Model,
    S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
    C: Config<Type<M>> + Config<Constant<M>> + Config<Lifetime<M>>,
    R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
>(
    lhs: &GenericArguments<M>,
    rhs: &GenericArguments<M>,
    premises: &Premises<M>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
    config: &mut C,
    unifier: &mut Unifier<'_, M>,
) -> unification::Result {
    if lhs.lifetimes.len() != rhs.lifetimes.len()
        || lhs.types.len() != rhs.types.len()
        || lhs.constants.len() != rhs.constants.len()
    {
        return Err(unification::Error);
    }

    for (lhs, rhs) in lhs.lifetimes.iter().zip(rhs.lifetimes.iter()) {
        unifier.unify(lhs, rhs, premises, table, semantic, session, config)?;
    }

    for (lhs, rhs) in lhs.types.iter().zip(rhs.types.iter()) {
        unifier.unify(lhs, rhs, premises, table, semantic, session, config)?;
    }

    for (lhs, rhs) in lhs.constants.iter().zip(rhs.constants.iter()) {
        unifier.unify(lhs, rhs, premises, table, semantic, session, config)?;
    }

    Ok(())
}

fn tuple_unifiable<S, T, Parameter, TraitMember>(
    lhs: &Tuple<T, Parameter, TraitMember>,
    rhs: &Tuple<T, Parameter, TraitMember>,
) -> bool
where
    S: Model,
    Tuple<T, Parameter, TraitMember>: Into<T>,
    T: Term<Model = S> + From<Parameter> + From<TraitMember>,
    Parameter: Clone + TryFrom<T, Error = T>,
    TraitMember: Clone + TryFrom<T, Error = T>,
{
    match lhs.elements.iter().filter(|x| x.is_unpacked()).count() {
        0 => {
            lhs.elements.len() == rhs.elements.len()
                && !rhs.elements.iter().any(TupleElement::is_unpacked)
        }
        1 => {
            if lhs.elements.len() > rhs.elements.len() + 1 {
                return false;
            }

            let unpacked_position = lhs
                .elements
                .iter()
                .position(TupleElement::is_unpacked)
                .unwrap();

            let tail_to_unpack_count = lhs.elements.len() - unpacked_position - 1;

            if rhs.elements[..unpacked_position]
                .iter()
                .any(TupleElement::is_unpacked)
            {
                return false;
            }

            if rhs.elements[rhs.elements.len() - tail_to_unpack_count..]
                .iter()
                .any(TupleElement::is_unpacked)
            {
                return false;
            }

            true
        }
        _ => false,
    }
}

#[allow(clippy::too_many_arguments)]
fn sub_structural_unify_tuple<M, T, Parameter, TraitMember, C, S, R>(
    lhs: &Tuple<T, Parameter, TraitMember>,
    rhs: &Tuple<T, Parameter, TraitMember>,
    premises: &Premises<M>,
    table: &Table,
    semantic: &mut S,
    session: &mut R,
    config: &mut C,
    unifier: &mut Unifier<'_, M>,
) -> unification::Result
where
    M: Model,
    T: Term<Model = M> + From<Parameter> + From<TraitMember>,
    Tuple<T, Parameter, TraitMember>: Into<T>,
    Parameter: Clone + TryFrom<T, Error = T>,
    TraitMember: Clone + TryFrom<T, Error = T>,
    C: Config<Type<M>> + Config<Constant<M>> + Config<Lifetime<M>> + Config<T>,
    S: Semantic<T> + Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
    R: Session<T> + Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
{
    let unpacked_count = lhs.elements.iter().filter(|x| x.is_unpacked()).count();

    match unpacked_count {
        // no unpacked elements case
        0 => {
            for (lhs_element, rhs_element) in lhs.elements.iter().zip(rhs.elements.iter()) {
                let lhs_element = lhs_element.as_regular().unwrap();
                let rhs_element = rhs_element.as_regular().unwrap();

                unifier.unify(
                    lhs_element,
                    rhs_element,
                    premises,
                    table,
                    semantic,
                    session,
                    config,
                )?;
            }

            Ok(())
        }

        // one unpacked element case
        1 => {
            let unpacked_position = lhs
                .elements
                .iter()
                .position(TupleElement::is_unpacked)
                .unwrap();

            let head_range = 0..unpacked_position;
            let lhs_tail_range = (unpacked_position + 1)..lhs.elements.len();
            let rhs_tail_range =
                (rhs.elements.len() - lhs_tail_range.clone().count())..rhs.elements.len();
            let rhs_unpack_range = unpacked_position..rhs_tail_range.start;

            // unify head
            for (lhs_element, rhs_element) in lhs.elements[head_range.clone()]
                .iter()
                .zip(&rhs.elements[head_range])
            {
                let lhs_element = lhs_element.as_regular().unwrap();
                let rhs_element = rhs_element.as_regular().unwrap();

                unifier.unify(
                    lhs_element,
                    rhs_element,
                    premises,
                    table,
                    semantic,
                    session,
                    config,
                )?;
            }

            // unify tail
            for (lhs_element, rhs_element) in lhs.elements[lhs_tail_range]
                .iter()
                .zip(&rhs.elements[rhs_tail_range])
            {
                let lhs_element = lhs_element.as_regular().unwrap();
                let rhs_element = rhs_element.as_regular().unwrap();

                unifier.unify(
                    lhs_element,
                    rhs_element,
                    premises,
                    table,
                    semantic,
                    session,
                    config,
                )?;
            }

            let rhs_unpack = Tuple {
                elements: rhs.elements[rhs_unpack_range].to_vec(),
            }
            .into();

            let unpacked = lhs.elements[unpacked_position].as_unpacked().unwrap();
            match unpacked {
                Unpacked::Parameter(parameter) => {
                    let parameter = T::from(parameter.clone());

                    unifier.unify(
                        &parameter,
                        &rhs_unpack,
                        premises,
                        table,
                        semantic,
                        session,
                        config,
                    )?;
                }
                Unpacked::TraitMember(trait_member) => {
                    let trait_member = T::from(trait_member.clone());

                    unifier.unify(
                        &trait_member,
                        &rhs_unpack,
                        premises,
                        table,
                        semantic,
                        session,
                        config,
                    )?;
                }
            }

            Ok(())
        }

        _ => unreachable!(),
    }
}
