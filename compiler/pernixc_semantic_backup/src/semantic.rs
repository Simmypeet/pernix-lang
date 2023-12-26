//! Contains code related to logic applied to the entities.

use self::{
    model::Model,
    predicate::{ConstantTypeProperty, OutlivesProperty, Premises},
    session::Session,
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{ImplementationKindID, SymbolKindID, Type},
        GenericArguments, MemberSymbol, Symbol, Term, Tuple, TupleElement, Unpacked,
    },
    unification::{Config, Substructural},
};
use crate::{
    semantic::{
        model::Entity,
        substitution::{Substitute, Substitution},
    },
    table::{Index, State, Table},
};

pub mod deduction;
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
pub trait Semantic<T: Term>: 'static + Send + Sync {
    /// Gets the definitiveness property of the given term.
    fn definitive_property(&self, term: &T) -> definite::Property;

    /// Checks if the two given terms are trivally equal.
    ///
    /// This is used to check if the two terms are equal without any further reasoning.
    fn trivially_equals(&self, lhs: &T, rhs: &T) -> bool;

    /// Determines if the given term outlibes the given lifetime bound.
    fn outlives_property(
        &self,
        term: &T,
        lifetime_bound: &Lifetime<<T as Term>::Model>,
    ) -> OutlivesProperty;

    /// Gets the constant type property of the given term.
    fn constant_type_property(&self, term: &T) -> ConstantTypeProperty;

    /// Normalizes the given term.
    ///
    /// Some terms can be normalized to a simpler form, for example, a type alias can be normalized
    /// to the aliased type.
    fn normalize<
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    >(
        &self,
        term: &T,
        premises: &Premises<<T as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Vec<T>;

    /// Sub-structurally unifies the two given terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms cannot be unified.
    #[allow(clippy::too_many_arguments)]
    fn sub_structural_unify<
        C: Config<T>
            + Config<Type<<T as Term>::Model>>
            + Config<Constant<<T as Term>::Model>>
            + Config<Lifetime<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>,
    >(
        &self,
        lhs: &T,
        rhs: &T,
        premises: &Premises<<T as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Result<Substructural<<T as Term>::Model>, unification::Error>;
}

/// The logic object providing fundemental logic for reasoning about the system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Default;

impl<S: Model> Semantic<Type<S>> for Default {
    fn definitive_property(&self, term: &Type<S>) -> definite::Property {
        match term {
            Type::Primitive(_) => definite::Property::Positive,
            Type::Parameter(_) | Type::Inference(_) => definite::Property::Negative,

            Type::Implementation(_)
            | Type::Symbol(_)
            | Type::Local(_)
            | Type::Tuple(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::TraitMember(_) => definite::Property::Applicative,
        }
    }

    fn trivially_equals(&self, lhs: &Type<S>, rhs: &Type<S>) -> bool { lhs == rhs }

    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &self,
        term: &Type<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Vec<Type<S>> {
        match term {
            // transform type alias into the aliased type equivalent
            Type::Symbol(Symbol {
                id: SymbolKindID::Type(id),
                generic_arguments,
            }) => {
                let Some(type_sym) = table.get(*id) else {
                    return Vec::new();
                };

                let mut type_aliased = type_sym.r#type.clone().into_other_model();
                type_aliased.apply(&Substitution::from_generic_arguments(
                    generic_arguments.clone(),
                    (*id).into(),
                ));

                vec![type_aliased]
            }

            // transform trait type into its implementation member
            Type::TraitMember(trait_member) => {
                let trait_id = table.get(trait_member.id).unwrap().parent_id;
                let mut results = Vec::new();

                for implementation in table
                    .resolve_implementation(
                        trait_id,
                        &trait_member.parent_generic_arguments,
                        premises,
                        self,
                        session,
                    )
                    .into_iter()
                    .flatten()
                {
                    let Some(implementation_sym) = table.get(implementation.implementation_id)
                    else {
                        continue;
                    };

                    let Some(implementation_type_id) = implementation_sym
                        .implementation_type_ids_by_trait_type_id
                        .get(&trait_member.id)
                        .copied()
                    else {
                        continue;
                    };

                    results.push(Type::Implementation(term::MemberSymbol {
                        id: implementation_type_id.into(),
                        member_generic_arguments: trait_member.member_generic_arguments.clone(),
                        parent_generic_arguments: implementation.deduced_generic_arguments,
                    }));
                }

                results
            }

            // transform the adt's implementation type alias
            Type::Implementation(MemberSymbol {
                id: ImplementationKindID::Adt(adt_implementation_id),
                member_generic_arguments,
                parent_generic_arguments, // not-deduced
            }) => {
                let Some(type_sym) = table.get(*adt_implementation_id) else {
                    return Vec::new();
                };

                let Some(implementation_sym) = table.get(type_sym.parent_id) else {
                    return Vec::new();
                };

                let Some(deduction) = implementation_sym
                    .signature
                    .arguments
                    .clone()
                    .into_other_model()
                    .deduce(parent_generic_arguments, premises, table, self, session)
                else {
                    return Vec::new();
                };

                let mut type_aliased = type_sym.r#type.clone().into_other_model();

                type_aliased.apply(&deduction);
                type_aliased.apply(&Substitution::from_generic_arguments(
                    member_generic_arguments.clone(),
                    (*adt_implementation_id).into(),
                ));

                vec![type_aliased]
            }

            _ => Vec::new(),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn sub_structural_unify<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &self,
        lhs: &Type<S>,
        rhs: &Type<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Result<Substructural<S>, unification::Error> {
        match (lhs, rhs) {
            (Type::Local(lhs), Type::Local(rhs)) => Ok(Substructural {
                lifetimes: Vec::new(),
                types: vec![((*lhs.0).clone(), (*rhs.0).clone())],
                constants: Vec::new(),
            }),

            (Type::Pointer(lhs), Type::Pointer(rhs)) if lhs.qualifier == rhs.qualifier => {
                Ok(Substructural {
                    lifetimes: Vec::new(),
                    types: vec![((*lhs.pointee).clone(), (*rhs.pointee).clone())],
                    constants: Vec::new(),
                })
            }

            (Type::Symbol(lhs), Type::Symbol(rhs)) if lhs.id == rhs.id => {
                sub_structural_unify_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premises,
                    table,
                    self,
                    session,
                    Substructural::default(),
                )
            }

            (Type::Reference(lhs), Type::Reference(rhs)) if lhs.qualifier == rhs.qualifier => {
                Ok(Substructural {
                    lifetimes: vec![(lhs.lifetime.clone(), rhs.lifetime.clone())],
                    types: vec![((*lhs.pointee).clone(), (*rhs.pointee).clone())],
                    constants: Vec::new(),
                })
            }

            (Type::Array(lhs), Type::Array(rhs)) => Ok(Substructural {
                lifetimes: Vec::new(),
                types: vec![((*lhs.element).clone(), (*rhs.element).clone())],
                constants: vec![(lhs.length.clone(), rhs.length.clone())],
            }),

            (Type::Implementation(lhs), Type::Implementation(rhs)) if lhs.id == rhs.id => {
                sub_structural_unify_member_symbol(
                    lhs,
                    rhs,
                    premises,
                    table,
                    self,
                    session,
                    Substructural::default(),
                )
            }

            (Type::TraitMember(lhs), Type::TraitMember(rhs)) if lhs.id == rhs.id => {
                sub_structural_unify_member_symbol(
                    lhs,
                    rhs,
                    premises,
                    table,
                    self,
                    session,
                    Substructural::default(),
                )
            }

            (Type::Tuple(lhs), Type::Tuple(rhs)) if tuple_unifiable(lhs, rhs) => {
                sub_structural_unify_tuple(
                    lhs,
                    rhs,
                    premises,
                    table,
                    self,
                    session,
                    Substructural::default(),
                )
            }

            (_, _) => Err(unification::Error),
        }
    }

    fn outlives_property(
        &self,
        term: &Type<S>,
        _: &Lifetime<<Type<S> as Term>::Model>,
    ) -> OutlivesProperty {
        match term {
            Type::Primitive(_) => OutlivesProperty::Positive,
            Type::Inference(_) | Type::Parameter(_) => OutlivesProperty::Negative,

            Type::Symbol(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::TraitMember(_)
            | Type::Tuple(_)
            | Type::Local(_)
            | Type::Implementation(_) => OutlivesProperty::Applicative,
        }
    }

    fn constant_type_property(&self, term: &Type<S>) -> ConstantTypeProperty {
        match term {
            Type::Primitive(_) => ConstantTypeProperty::Positive,
            Type::Implementation(_)
            | Type::TraitMember(_)
            | Type::Parameter(_)
            | Type::Pointer(_)
            | Type::Inference(_)
            | Type::Symbol(Symbol {
                id: SymbolKindID::Type(_),
                ..
            }) => ConstantTypeProperty::Negative,

            Type::Symbol(Symbol {
                id: SymbolKindID::Enum(_) | SymbolKindID::Struct(_),
                ..
            })
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Local(_) => ConstantTypeProperty::Applicative,
        }
    }
}

impl<S: Model> Semantic<Constant<S>> for Default {
    fn definitive_property(&self, term: &Constant<S>) -> definite::Property {
        match term {
            Constant::Primitive(_) => definite::Property::Positive,

            Constant::Parameter(_) | Constant::Inference(_) => definite::Property::Negative,

            Constant::Struct(_)
            | Constant::Enum(_)
            | Constant::Array(_)
            | Constant::TraitMember(_)
            | Constant::Local(_)
            | Constant::Symbol(_)
            | Constant::Implementation(_)
            | Constant::Tuple(_) => definite::Property::Applicative,
        }
    }

    fn trivially_equals(&self, lhs: &Constant<S>, rhs: &Constant<S>) -> bool { lhs == rhs }

    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &self,
        _: &Constant<S>,
        _: &Premises<<Type<S> as Term>::Model>,
        _: &Table<impl State>,
        _: &mut R,
    ) -> Vec<Constant<S>> {
        todo!("We must normalize for trait members, symbol, and implementation symbol")
    }

    #[allow(clippy::too_many_lines)]
    fn sub_structural_unify<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &self,
        lhs: &Constant<S>,
        rhs: &Constant<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Result<Substructural<S>, unification::Error> {
        match (lhs, rhs) {
            (Constant::Struct(lhs), Constant::Struct(rhs))
                if lhs.struct_id == rhs.struct_id && lhs.fields.len() == rhs.fields.len() =>
            {
                let mut result = sub_structural_unify_generic_arguments(&lhs.generic_arguments, &rhs.generic_arguments, premises, table, self, session, Substructural { lifetimes: (), types: (), constants: () })

                for (lhs, rhs) in lhs.fields.iter().zip(rhs.fields.iter()) {
                    unifier.unify(lhs, rhs, premises, table, self, session, config)?;
                }

                Ok(())
            }

            (Constant::Symbol(lhs), Constant::Symbol(rhs)) if lhs.id == rhs.id => {
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

            (Constant::Implementation(lhs), Constant::Implementation(rhs)) if lhs.id == rhs.id => {
                sub_structural_unify_generic_arguments(
                    &lhs.parent_generic_arguments,
                    &rhs.parent_generic_arguments,
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

            (Constant::TraitMember(lhs), Constant::TraitMember(rhs)) if lhs.id == rhs.id => {
                sub_structural_unify_generic_arguments(
                    &lhs.parent_generic_arguments,
                    &rhs.parent_generic_arguments,
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

    fn outlives_property(
        &self,
        _: &Constant<S>,
        _: &Lifetime<<Constant<S> as Term>::Model>,
    ) -> OutlivesProperty {
        OutlivesProperty::Positive
    }

    fn constant_type_property(&self, _: &Constant<S>) -> ConstantTypeProperty {
        ConstantTypeProperty::Positive
    }
}

impl<S: Model> Semantic<Lifetime<S>> for Default {
    fn definitive_property(&self, _: &Lifetime<S>) -> definite::Property {
        definite::Property::Positive
    }

    fn trivially_equals(&self, lhs: &Lifetime<S>, rhs: &Lifetime<S>) -> bool { lhs == rhs }

    // no normalization for lifetimes
    fn normalize<R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>>(
        &self,
        _: &Lifetime<S>,
        _: &Premises<<Type<S> as Term>::Model>,
        _: &Table<impl State>,
        _: &mut R,
    ) -> Vec<Lifetime<S>> {
        Vec::new()
    }

    #[allow(clippy::too_many_lines)]
    fn sub_structural_unify<
        C: Config<Type<S>> + Config<Constant<S>> + Config<Lifetime<S>>,
        R: Session<Type<S>> + Session<Lifetime<S>> + Session<Constant<S>>,
    >(
        &self,
        lhs: &Lifetime<S>,
        rhs: &Lifetime<S>,
        premises: &Premises<<Type<S> as Term>::Model>,
        table: &Table<impl State>,
        session: &mut R,
    ) -> Result<Substructural<S>, unification::Error> {
        Ok(Substructural::default())
    }

    fn outlives_property(
        &self,
        term: &Lifetime<S>,
        _: &Lifetime<<Lifetime<S> as Term>::Model>,
    ) -> OutlivesProperty {
        if term.is_static() {
            OutlivesProperty::Positive
        } else {
            OutlivesProperty::Negative
        }
    }

    fn constant_type_property(&self, term: &Lifetime<S>) -> ConstantTypeProperty {
        if term.is_static() {
            ConstantTypeProperty::Positive
        } else {
            ConstantTypeProperty::Negative
        }
    }
}

fn sub_structural_unify_member_symbol<
    M: Model,
    S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
    R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
    I,
>(
    lhs: &MemberSymbol<I, M>,
    rhs: &MemberSymbol<I, M>,
    premises: &Premises<M>,
    table: &Table<impl State>,
    semantic: &S,
    session: &mut R,
    mut existing: Substructural<M>,
) -> Result<Substructural<M>, unification::Error> {
    let result = sub_structural_unify_generic_arguments(
        &lhs.member_generic_arguments,
        &rhs.member_generic_arguments,
        premises,
        table,
        semantic,
        session,
        existing,
    )?;

    sub_structural_unify_generic_arguments(
        &lhs.parent_generic_arguments,
        &rhs.parent_generic_arguments,
        premises,
        table,
        semantic,
        session,
        result,
    )
}

#[allow(clippy::too_many_arguments)]
fn sub_structural_unify_generic_arguments<
    M: Model,
    S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
    R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
>(
    lhs: &GenericArguments<M>,
    rhs: &GenericArguments<M>,
    premises: &Premises<M>,
    table: &Table<impl State>,
    semantic: &S,
    session: &mut R,
    mut existing: Substructural<M>,
) -> Result<Substructural<M>, unification::Error> {
    if lhs.lifetimes.len() != rhs.lifetimes.len()
        || lhs.types.len() != rhs.types.len()
        || lhs.constants.len() != rhs.constants.len()
    {
        return Err(unification::Error);
    }

    for (lhs, rhs) in lhs.lifetimes.iter().zip(rhs.lifetimes.iter()) {
        existing.lifetimes.push((lhs.clone(), rhs.clone()));
    }

    for (lhs, rhs) in lhs.types.iter().zip(rhs.types.iter()) {
        existing.types.push((lhs.clone(), rhs.clone()));
    }

    for (lhs, rhs) in lhs.constants.iter().zip(rhs.constants.iter()) {
        existing.constants.push((lhs.clone(), rhs.clone()));
    }

    Ok(existing)
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
fn sub_structural_unify_tuple<M, T, Parameter, TraitMember, S, R>(
    lhs: &Tuple<T, Parameter, TraitMember>,
    rhs: &Tuple<T, Parameter, TraitMember>,
    premises: &Premises<M>,
    table: &Table<impl State>,
    semantic: &S,
    session: &mut R,
    mut existing: Substructural<M>,
) -> Result<Substructural<M>, unification::Error>
where
    M: Model,
    T: Term<Model = M> + From<Parameter> + From<TraitMember>,
    Tuple<T, Parameter, TraitMember>: Into<T>,
    Parameter: Clone + TryFrom<T, Error = T>,
    TraitMember: Clone + TryFrom<T, Error = T>,
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

                T::get_substructural(&mut existing)
                    .push((lhs_element.clone(), rhs_element.clone()));
            }

            Ok(existing)
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

                T::get_substructural(&mut existing)
                    .push((lhs_element.clone(), rhs_element.clone()));
            }

            // unify tail
            for (lhs_element, rhs_element) in lhs.elements[lhs_tail_range]
                .iter()
                .zip(&rhs.elements[rhs_tail_range])
            {
                let lhs_element = lhs_element.as_regular().unwrap();
                let rhs_element = rhs_element.as_regular().unwrap();

                T::get_substructural(&mut existing)
                    .push((lhs_element.clone(), rhs_element.clone()));
            }

            let rhs_unpack = Tuple {
                elements: rhs.elements[rhs_unpack_range].to_vec(),
            }
            .into();

            let unpacked = lhs.elements[unpacked_position].as_unpacked().unwrap();
            match unpacked {
                Unpacked::Parameter(parameter) => {
                    let parameter = T::from(parameter.clone());

                    T::get_substructural(&mut existing).push((parameter, rhs_unpack));
                }
                Unpacked::TraitMember(trait_member) => {
                    let trait_member = T::from(trait_member.clone());

                    T::get_substructural(&mut existing).push((trait_member, rhs_unpack));
                }
            }

            Ok(existing)
        }

        _ => unreachable!(),
    }
}
