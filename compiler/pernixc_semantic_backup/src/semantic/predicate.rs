//! Contains the definition of predicates that are used in the trait system.

use std::marker::PhantomData;

use enum_as_inner::EnumAsInner;

use super::{
    map::{Map, Mapping},
    model::{Entity, Model},
    r#trait::LifetimeConstraint,
    session::Session,
    substitution::{Substitute, Substitution},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification,
    visitor::{Source, VisitMode},
    Semantic,
};
use crate::{
    arena::ID,
    semantic::visitor,
    symbol,
    table::{Index, State, Success, Table},
};

/// Represents a subset of [`Predicate`] that does not contain equality
/// predicates.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum NonEquality<S: Model> {
    LifetimeOutlives(LifetimeOutlives<S>),
    TypeOutlives(TypeOutlives<S>),
    Trait(Trait<S>),
    ConstantType(ConstantType<S>),
}

impl<S: Model> NonEquality<S> {
    /// Applies the given substitution to this non-equality predicate.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        match self {
            Self::LifetimeOutlives(outlives) => outlives.apply(substitution),
            Self::TypeOutlives(outlives) => outlives.apply(substitution),
            Self::Trait(trait_bound) => trait_bound.apply(substitution),
            Self::ConstantType(constant_type) => {
                constant_type.apply(substitution)
            }
        }
    }
}

/// Containing a list of non-equality predicates and a [`Mapping`] representing
/// equality of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premises<S: Model> {
    /// A list of non-equality predicates.
    pub non_equality_predicates: Vec<NonEquality<S>>,

    /// A mapping that represents equality of terms.
    pub mapping: Mapping<S>,
}

impl<S: Model> Premises<S> {
    /// Creates a new instance of [`Premises`] from a list of predicates.
    ///
    /// Equality predicates are separated from non-equality predicates and
    /// stored in a [`Mapping`].
    pub fn from_predicates(
        predicates: impl Iterator<Item = Predicate<S>>,
    ) -> Self {
        let mut mapping = Mapping::default();
        let mut non_equality_predicates = Vec::new();

        for predicate in predicates {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    non_equality_predicates
                        .push(NonEquality::LifetimeOutlives(outlives));
                }
                Predicate::TypeOutlives(outlives) => {
                    non_equality_predicates
                        .push(NonEquality::TypeOutlives(outlives));
                }
                Predicate::TypeEquals(type_equals) => {
                    mapping.insert_type(type_equals.lhs, type_equals.rhs);
                }
                Predicate::ConstantEquals(constant_equals) => {
                    mapping.insert_constant(
                        constant_equals.lhs,
                        constant_equals.rhs,
                    );
                }
                Predicate::Trait(trait_bound) => {
                    non_equality_predicates
                        .push(NonEquality::Trait(trait_bound));
                }
                Predicate::ConstantType(constant_type) => {
                    non_equality_predicates
                        .push(NonEquality::ConstantType(constant_type));
                }
            }
        }

        Self { non_equality_predicates, mapping }
    }
}

/// Represents a predicate in that will be used in the generic engine.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Predicate<S: Model> {
    LifetimeOutlives(LifetimeOutlives<S>),
    TypeOutlives(TypeOutlives<S>),
    TypeEquals(TypeEquals<S>),
    ConstantEquals(ConstantEquals<S>),
    Trait(Trait<S>),
    ConstantType(ConstantType<S>),
}

impl<S: Model> Predicate<S> {
    /// Applies the given substitution to this predicate.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        match self {
            Self::LifetimeOutlives(outlives) => outlives.apply(substitution),
            Self::TypeOutlives(outlives) => outlives.apply(substitution),
            Self::TypeEquals(type_equals) => type_equals.apply(substitution),
            Self::ConstantEquals(constant_equals) => {
                constant_equals.apply(substitution)
            }
            Self::Trait(trait_bound) => trait_bound.apply(substitution),
            Self::ConstantType(constant_type) => {
                constant_type.apply(substitution)
            }
        }
    }
}

impl<S: Model> Entity for Predicate<S> {
    type Model = S;
    type Rebind<A: Model> = Predicate<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        match self {
            Self::LifetimeOutlives(outlives) => {
                Predicate::LifetimeOutlives(outlives.into_other_model())
            }
            Self::TypeOutlives(outlives) => {
                Predicate::TypeOutlives(outlives.into_other_model())
            }
            Self::TypeEquals(type_equals) => {
                Predicate::TypeEquals(type_equals.into_other_model())
            }
            Self::ConstantEquals(constant_equals) => {
                Predicate::ConstantEquals(constant_equals.into_other_model())
            }
            Self::Trait(trait_bound) => {
                Predicate::Trait(trait_bound.into_other_model())
            }
            Self::ConstantType(constant_type) => {
                Predicate::ConstantType(constant_type.into_other_model())
            }
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(match self {
            Self::LifetimeOutlives(outlives) => {
                Predicate::LifetimeOutlives(outlives.try_into_other_model()?)
            }
            Self::TypeOutlives(outlives) => {
                Predicate::TypeOutlives(outlives.try_into_other_model()?)
            }
            Self::TypeEquals(type_equals) => {
                Predicate::TypeEquals(type_equals.try_into_other_model()?)
            }
            Self::ConstantEquals(constant_equals) => Predicate::ConstantEquals(
                constant_equals.try_into_other_model()?,
            ),
            Self::Trait(trait_bound) => {
                Predicate::Trait(trait_bound.try_into_other_model()?)
            }
            Self::ConstantType(constant_type) => {
                Predicate::ConstantType(constant_type.try_into_other_model()?)
            }
        })
    }
}

/// Denotes a constant type predicate, denoted by `const TYPE` syntax.
///
/// This predicate allows the type to be used as a type of constant parameters.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType<S: Model> {
    /// The type that is used as a constant type.
    pub r#type: Type<S>,
}

/// The property of a constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ConstantTypeProperty {
    Positive,
    Negative,
    Applicative,
}

/// Records object for the [`ConstantType::satisfies()`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantTypeRecord<'a, T: Term>(pub &'a T);

impl<S: Model> Entity for ConstantType<S> {
    type Model = S;
    type Rebind<A: Model> = ConstantType<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        ConstantType { r#type: self.r#type.into_other_model() }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(ConstantType { r#type: self.r#type.try_into_other_model()? })
    }
}

#[derive(Debug)]
struct ConstantTypeVisitor<
    'p,
    's,
    'r,
    Ts: State,
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
> {
    premises: &'p Premises<<T as Term>::Model>,
    table: &'p Table<Ts>,
    semantic: &'s S,
    session: &'r mut R,

    first: bool,
    is_constant_type: bool,
}

impl<
        Ts: State,
        T: Term,
        S: Semantic<T>
            + Semantic<Type<<T as Term>::Model>>
            + Semantic<Lifetime<<T as Term>::Model>>
            + Semantic<Constant<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    > visitor::Visitor for ConstantTypeVisitor<'_, '_, '_, Ts, T, S, R>
{
    type Model = <T as Term>::Model;

    fn visit_type(&mut self, ty: &Type<Self::Model>, _: Source) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = ConstantType::satisfies(
            ty,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.is_constant_type = false;
        }

        result
    }

    fn visit_lifetime(
        &mut self,
        ty: &Lifetime<Self::Model>,
        _: Source,
    ) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = ConstantType::satisfies(
            ty,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.is_constant_type = false;
        }

        result
    }

    fn visit_constant(
        &mut self,
        ty: &Constant<Self::Model>,
        _: Source,
    ) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = ConstantType::satisfies(
            ty,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.is_constant_type = false;
        }

        result
    }
}

impl<M: Model> ConstantType<M> {
    /// Applies the given substitution to this constant type predicate.
    pub fn apply(&mut self, substitution: &Substitution<M>) {
        self.r#type.apply(substitution);
    }

    /// Determines whether the `term` can be used as a type of the constant
    /// parameter.
    pub fn satisfies<
        T: Term<Model = M>,
        S: Semantic<T>
            + Semantic<Type<<T as Term>::Model>>
            + Semantic<Lifetime<<T as Term>::Model>>
            + Semantic<Constant<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    >(
        term: &T,
        premises: &Premises<<T as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        let is_applicative = match semantic.constant_type_property(term) {
            ConstantTypeProperty::Positive => return true,

            ConstantTypeProperty::Negative => false,
            ConstantTypeProperty::Applicative => true,
        };

        if !session.mark_as_working_on(ConstantTypeRecord(term)) {
            // coinductive reasoning for ADT type
            if term.is_adt_type_term() {
                session.mark_as_done(ConstantTypeRecord(term));
                return true;
            }

            return false;
        }

        // try to look in the premises first
        for operand in T::constant_type_premises(premises) {
            if operand.equals(term, premises, table, semantic, session) {
                session.mark_as_done(ConstantTypeRecord(term));
                return true;
            }
        }

        if is_applicative {
            let mut visitor = ConstantTypeVisitor::<'_, '_, '_, _, T, S, R> {
                premises,
                table,
                semantic,
                session,
                first: true,
                is_constant_type: true,
            };

            let _ =
                term.accept(&mut visitor, VisitMode::<Success>::OnlySubTerms);

            if visitor.is_constant_type {
                session.mark_as_done(ConstantTypeRecord(term));
                return true;
            }
        }

        // try to normalize
        for normalized in term.normalize(premises, table, semantic, session) {
            if Self::satisfies(&normalized, premises, table, semantic, session)
            {
                session.mark_as_done(ConstantTypeRecord(term));
                return true;
            }
        }
        // try to look for equivalences
        for (lhs_eq, rhs_eqs) in <T as Map>::get(&premises.mapping) {
            if !lhs_eq.equals(term, premises, table, semantic, session) {
                continue;
            }

            for rhs_eq in rhs_eqs {
                if Self::satisfies(rhs_eq, premises, table, semantic, session) {
                    session.mark_as_done(ConstantTypeRecord(term));
                    return true;
                }
            }
        }

        session.mark_as_done(ConstantTypeRecord(term));
        false
    }
}

/// Represents an outlive predicate, defnoted by `'OPERAND 'argument` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Outlives<T: Entity> {
    /// The entity that lives as long or longer than the [`Self::bound`]
    /// lifetime.
    pub operand: T,

    /// The lifetime that the [`Self::operand`] lives as long or longer than.
    pub bound: Lifetime<T::Model>,
}

impl<S: Model, T: Term<Model = S> + Entity<Model = S>> Outlives<T> {
    /// Applies the given substitution to this outlive predicate.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        self.operand.apply(substitution);
        self.bound.apply(substitution);
    }
}

/// Represents a lifetime outlive predicate, denoted by `'OPERAND: 'argument`
/// syntax.
pub type LifetimeOutlives<S> = Outlives<Lifetime<S>>;

impl<S: Model> Entity for LifetimeOutlives<S> {
    type Model = S;
    type Rebind<A: Model> = LifetimeOutlives<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        LifetimeOutlives {
            operand: self.operand.into_other_model(),
            bound: self.bound.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(LifetimeOutlives {
            operand: self.operand.try_into_other_model()?,
            bound: self.bound.try_into_other_model()?,
        })
    }
}

/// Represents a type outlive predicate, denoted by `TYPE: 'argument` syntax.
pub type TypeOutlives<S> = Outlives<Type<S>>;

impl<S: Model> Entity for TypeOutlives<S> {
    type Model = S;
    type Rebind<A: Model> = TypeOutlives<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        TypeOutlives {
            operand: self.operand.into_other_model(),
            bound: self.bound.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(TypeOutlives {
            operand: self.operand.try_into_other_model()?,
            bound: self.bound.try_into_other_model()?,
        })
    }
}

/// The outlives property of a lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum OutlivesProperty {
    Positive,
    Negative,
    Applicative,
}

/// Records object for the [`Outlives::outlives()`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OutlivesRecord<'a, T: Term> {
    /// The operand of the outlives predicate.
    pub operand: &'a T,

    /// The bound of the outlives predicate.
    pub bound: &'a Lifetime<<T as Term>::Model>,
}

#[derive(Debug)]
struct OutlivesVisitor<
    'p,
    's,
    'r,
    Ts: State,
    T: Term,
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>,
> {
    premises: &'p Premises<<T as Term>::Model>,
    table: &'p Table<Ts>,
    lifetime_bound: &'p Lifetime<<T as Term>::Model>,
    semantic: &'s S,
    session: &'r mut R,

    first: bool,
    outlives: bool,
}

impl<
        Ts: State,
        T: Term,
        S: Semantic<T>
            + Semantic<Type<<T as Term>::Model>>
            + Semantic<Lifetime<<T as Term>::Model>>
            + Semantic<Constant<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    > visitor::Visitor for OutlivesVisitor<'_, '_, '_, Ts, T, S, R>
{
    type Model = <T as Term>::Model;

    fn visit_type(&mut self, ty: &Type<Self::Model>, _: Source) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = Outlives::satisfies(
            ty,
            self.lifetime_bound,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.outlives = false;
        }

        result
    }

    fn visit_lifetime(
        &mut self,
        ty: &Lifetime<Self::Model>,
        _: Source,
    ) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = Outlives::satisfies(
            ty,
            self.lifetime_bound,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.outlives = false;
        }

        result
    }

    fn visit_constant(
        &mut self,
        ty: &Constant<Self::Model>,
        _: Source,
    ) -> bool {
        if !self.first {
            self.first = false;
            return true;
        }

        let result = Outlives::satisfies(
            ty,
            self.lifetime_bound,
            self.premises,
            self.table,
            self.semantic,
            self.session,
        );

        if !result {
            self.outlives = false;
        }

        result
    }
}

impl<T: Term + Entity> Outlives<T> {
    /// Determines whether the `term` outlives the given `lifetime`.
    pub fn satisfies<
        S: Semantic<T>
            + Semantic<Type<<T as Term>::Model>>
            + Semantic<Lifetime<<T as Term>::Model>>
            + Semantic<Constant<<T as Term>::Model>>,
        R: Session<T>
            + Session<Type<<T as Term>::Model>>
            + Session<Lifetime<<T as Term>::Model>>
            + Session<Constant<<T as Term>::Model>>,
    >(
        term: &T,
        lifetime_bound: &Lifetime<<T as Term>::Model>,
        premises: &Premises<<T as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        let is_applicative =
            match semantic.outlives_property(term, lifetime_bound) {
                OutlivesProperty::Positive => return true,

                OutlivesProperty::Negative => false,
                OutlivesProperty::Applicative => true,
            };

        if !session.mark_as_working_on(OutlivesRecord {
            operand: term,
            bound: lifetime_bound,
        }) {
            return false;
        }

        // try to look in the premises first
        for (operand, bound) in T::outlives_premises(premises) {
            if operand.equals(term, premises, table, semantic, session)
                && bound.equals(
                    lifetime_bound,
                    premises,
                    table,
                    semantic,
                    session,
                )
            {
                session.mark_as_done(OutlivesRecord {
                    operand: term,
                    bound: lifetime_bound,
                });
                return true;
            }
        }

        if is_applicative {
            let mut visitor = OutlivesVisitor::<'_, '_, '_, _, T, S, R> {
                premises,
                table,
                lifetime_bound,
                semantic,
                session,
                first: true,
                outlives: true,
            };

            let _ =
                term.accept(&mut visitor, VisitMode::<Success>::OnlySubTerms);

            if visitor.outlives {
                session.mark_as_done(OutlivesRecord {
                    operand: term,
                    bound: lifetime_bound,
                });
                return true;
            }
        }

        // four different loops
        // time complexity will go wild

        // try to look for equivalences
        for (lhs_eq, rhs_eqs) in <T as Map>::get(&premises.mapping) {
            if !lhs_eq.equals(term, premises, table, semantic, session) {
                continue;
            }

            for rhs_eq in rhs_eqs {
                if Self::satisfies(
                    rhs_eq,
                    lifetime_bound,
                    premises,
                    table,
                    semantic,
                    session,
                ) {
                    session.mark_as_done(OutlivesRecord {
                        operand: term,
                        bound: lifetime_bound,
                    });
                    return true;
                }
            }
        }
        // try to look for equivalences for lifetime bound
        for (lhs_eq, rhs_eqs) in
            <Lifetime<<T as Term>::Model> as Map>::get(&premises.mapping)
        {
            if !lhs_eq.equals(
                lifetime_bound,
                premises,
                table,
                semantic,
                session,
            ) {
                continue;
            }

            for rhs_eq in rhs_eqs {
                if Self::satisfies(
                    term, rhs_eq, premises, table, semantic, session,
                ) {
                    session.mark_as_done(OutlivesRecord {
                        operand: term,
                        bound: lifetime_bound,
                    });
                    return true;
                }
            }
        }
        // try to normalize
        for normalized in term.normalize(premises, table, semantic, session) {
            if Self::satisfies(
                &normalized,
                lifetime_bound,
                premises,
                table,
                semantic,
                session,
            ) {
                session.mark_as_done(OutlivesRecord {
                    operand: term,
                    bound: lifetime_bound,
                });
                return true;
            }
        }
        // try to normalize for lifetime bound
        for normalized in
            lifetime_bound.normalize(premises, table, semantic, session)
        {
            if Self::satisfies(
                term,
                &normalized,
                premises,
                table,
                semantic,
                session,
            ) {
                session.mark_as_done(OutlivesRecord {
                    operand: term,
                    bound: lifetime_bound,
                });
                return true;
            }
        }

        session.mark_as_done(OutlivesRecord {
            operand: term,
            bound: lifetime_bound,
        });
        false
    }
}

/// Represents an entity equality predicate, denoted by `ENTITY = ENTITY`
/// syntax.
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equals<Entity> {
    /// The left hand side of the predicate.
    pub lhs: Entity,

    /// The right hand side of the predicate.
    pub rhs: Entity,
}

/// Represents a type equality predicate, denoted by `TYPE = TYPE` syntax.
pub type TypeEquals<S> = Equals<Type<S>>;

/// Represents a constant equality predicate, denoted by `CONSTANT = CONSTANT`
/// syntax.
pub type ConstantEquals<S> = Equals<Constant<S>>;

impl<S: Model, T: Term<Model = S> + Entity<Model = S>> Equals<T> {
    /// Applies the given substitution to this equality predicate.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        self.lhs.apply(substitution);
        self.rhs.apply(substitution);
    }
}

impl<S: Entity> Entity for Equals<S> {
    type Model = S::Model;
    type Rebind<A: Model> = Equals<S::Rebind<A>>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        <Self::Model as Model>::ConstantInference: Into<T::ConstantInference>,
        <Self::Model as Model>::TypeInference: Into<T::TypeInference>,
        <Self::Model as Model>::LifetimeInference: Into<T::LifetimeInference>,
        <Self::Model as Model>::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        Equals {
            lhs: self.lhs.into_other_model(),
            rhs: self.rhs.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        <Self::Model as Model>::ConstantInference:
            TryInto<T::ConstantInference>,
        <Self::Model as Model>::TypeInference: TryInto<T::TypeInference>,
        <Self::Model as Model>::LifetimeInference:
            TryInto<T::LifetimeInference>,
        <Self::Model as Model>::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(Equals {
            lhs: self.lhs.try_into_other_model()?,
            rhs: self.rhs.try_into_other_model()?,
        })
    }
}

/// Represents a trait implementation predicate, denoted by
/// `TRAIT<GENERIC_ARGUMENTS>` syntax.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait<S: Model> {
    /// The trait that is implemented.
    pub trait_id: ID<symbol::Trait>,

    /// Determines whether the trait must be implemented in `const` context or
    /// not.
    pub const_trait: bool,

    /// The generic arguments supplied to the trait.
    pub generic_arguments: GenericArguments<S>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct HigherRankedLifetimeUnificationConfig<S: Model>(PhantomData<S>);

impl<S: Model> unification::Config<Type<S>>
    for HigherRankedLifetimeUnificationConfig<S>
{
    fn unifiable(&mut self, _: &Type<S>, _: &Type<S>) -> bool { false }
}

impl<S: Model> unification::Config<Lifetime<S>>
    for HigherRankedLifetimeUnificationConfig<S>
{
    fn unifiable(&mut self, _: &Lifetime<S>, rhs: &Lifetime<S>) -> bool {
        rhs.is_forall()
    }
}

impl<S: Model> unification::Config<Constant<S>>
    for HigherRankedLifetimeUnificationConfig<S>
{
    fn unifiable(&mut self, _: &Constant<S>, _: &Constant<S>) -> bool { false }
}

impl<S: Model> Trait<S> {
    /// Applies the given substitution to this trait predicate.
    pub fn apply(&mut self, substitution: &Substitution<S>) {
        self.generic_arguments.apply(&substitution.clone().into_other_model());
    }
}

impl<S: Model> Entity for Trait<S> {
    type Model = S;
    type Rebind<A: Model> = Trait<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        Trait {
            trait_id: self.trait_id,
            const_trait: self.const_trait,
            generic_arguments: self.generic_arguments.into_other_model(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(Trait {
            trait_id: self.trait_id,
            const_trait: self.const_trait,
            generic_arguments: self.generic_arguments.try_into_other_model()?,
        })
    }
}

/// Records to query of the [`Trait::satisfies()`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitRecord<'a, S: Model> {
    pub trait_id: ID<symbol::Trait>,
    pub const_trait: bool,
    pub generic_arguments: &'a GenericArguments<S>,
}

/// Results of checking whether a trait bound is satifiable or not.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitSatisfiability<S: Model> {
    /// The additional lifetime constraints that must be satisfied in order to
    /// satisfy the trait predicate.
    pub lifetime_constraints: Vec<LifetimeConstraint<S>>,
}

impl<M: Model> Trait<M> {
    /// Determines whether the given predicate is satisfiable or not.
    pub fn satisfies<
        S: Semantic<Type<M>> + Semantic<Lifetime<M>> + Semantic<Constant<M>>,
        R: Session<Type<M>> + Session<Lifetime<M>> + Session<Constant<M>>,
    >(
        trait_id: ID<symbol::Trait>,
        const_trait: bool,
        generic_arguments: &GenericArguments<M>,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> Vec<TraitSatisfiability<M>> {
        // coinductive reasoning
        if session.mark_as_working_on(TraitRecord {
            trait_id,
            const_trait,
            generic_arguments,
        }) {
            session.mark_as_done(TraitRecord {
                trait_id,
                const_trait,
                generic_arguments,
            });
            return vec![TraitSatisfiability {
                lifetime_constraints: Vec::new(),
            }];
        }

        for premise in &premises.non_equality_predicates {
            let NonEquality::Trait(trait_predicate) = premise else {
                continue;
            };

            // not-applicable
            if (const_trait && !trait_predicate.const_trait)
                || (trait_id != trait_predicate.trait_id)
            {
                continue;
            }

            let mut config = HigherRankedLifetimeUnificationConfig(PhantomData);
            if generic_arguments
                .unify(
                    &trait_predicate.generic_arguments,
                    premises,
                    table,
                    semantic,
                    session,
                    &mut config,
                )
                .is_some()
            {
                session.mark_as_done(TraitRecord {
                    trait_id,
                    const_trait,
                    generic_arguments,
                });
                // no additional lifetime constraints
                return vec![TraitSatisfiability {
                    lifetime_constraints: Vec::new(),
                }];
            }
        }

        // manually search for the trait implementation
        let result = table.resolve_implementation(
            trait_id,
            generic_arguments,
            premises,
            semantic,
            session,
        );
        session.mark_as_done(TraitRecord {
            trait_id,
            const_trait,
            generic_arguments,
        });

        let mut implementations = Vec::new();
        for implementation in result.into_iter().flatten() {
            let Some(implementation_sym) =
                table.get(implementation.implementation_id)
            else {
                continue;
            };

            if const_trait && !implementation_sym.is_const {
                continue;
            }

            implementations.push(TraitSatisfiability {
                lifetime_constraints: implementation.lifetime_constraints,
            });
        }

        implementations
    }
}

impl<M: Model, T: Term<Model = M> + Entity<Model = M>> Equals<T> {
    /// Determines whether the given predicate is satisfiable or not.
    pub fn satisfies<
        S: Semantic<T>
            + Semantic<Type<M>>
            + Semantic<Lifetime<M>>
            + Semantic<Constant<M>>,
        R: Session<T>
            + Session<Type<M>>
            + Session<Lifetime<M>>
            + Session<Constant<M>>,
    >(
        &self,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        self.lhs.equals(&self.rhs, premises, table, semantic, session)
    }
}
