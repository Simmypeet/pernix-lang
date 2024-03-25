//! Contains the semantic logic of the compiler (i.e. type checking/system).

use self::{
    equality::equals,
    mapping::Mapping,
    predicate::{NonEquality, Predicate, Satisfiability},
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, SymbolID, Type},
        GenericArguments, MemberSymbol, Symbol, Term,
    },
};
use crate::{
    arena::ID,
    semantic::instantiation::Instantiation,
    symbol::{MemberID, TraitImplementation},
    table::{Index, State, Table},
};

pub mod deduction;
pub mod equality;
pub mod instantiation;
pub mod mapping;
pub mod matching;
pub mod order;
pub mod predicate;
pub mod session;
pub mod subterm;
pub mod term;
pub mod unification;
pub mod visitor;

/// The foundation truth used to derive further arguments.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premise {
    /// The mapping of predefined equalities.
    pub equalities_mapping: Mapping,

    /// The list of non-equality predicates.
    pub non_equality_predicates: Vec<NonEquality>,

    /// Specifies the trait implementation that the semantic logic is currently
    /// taking place in.
    pub active_trait_implementation: Option<ID<TraitImplementation>>,
}

impl Premise {
    /// Appends the given predicates to the premise.
    pub fn append_from_predicates(
        &mut self,
        predicates: impl Iterator<Item = Predicate>,
    ) {
        for predicate in predicates {
            let non_equality = match predicate {
                Predicate::TypeEquality(ty_equality) => {
                    self.equalities_mapping
                        .insert(ty_equality.lhs, ty_equality.rhs);
                    continue;
                }
                Predicate::ConstantEquality(constant_equality) => {
                    self.equalities_mapping
                        .insert(constant_equality.lhs, constant_equality.rhs);
                    continue;
                }
                Predicate::ConstantType(constant_type) => {
                    NonEquality::ConstantType(constant_type)
                }
                Predicate::LifetimeOutlives(lifetime_outlives) => {
                    NonEquality::LifetimeOutlives(lifetime_outlives)
                }
                Predicate::TypeOutlives(type_outlives) => {
                    NonEquality::TypeOutlives(type_outlives)
                }
                Predicate::TupleType(tuple_type) => {
                    NonEquality::TupleType(tuple_type)
                }
                Predicate::TupleConstant(tuple_constant) => {
                    NonEquality::TupleConstant(tuple_constant)
                }
                Predicate::Trait(tr) => NonEquality::Trait(tr),
            };

            self.non_equality_predicates.push(non_equality);
        }
    }

    /// Creates a new [`Premise`] with the given predicates.
    pub fn from_predicates(
        predicates: impl Iterator<Item = Predicate>,
    ) -> Self {
        let mut premise = Self::default();
        premise.append_from_predicates(predicates);
        premise
    }
}

/// A customization point for the semantic logic.
pub trait Semantic<T: Term> {
    /// Checks if the two given terms are trivially equal.
    fn trivially_equals(&mut self, lhs: &T, rhs: &T) -> bool;

    /// Normalizes the given term.
    ///
    /// The normalization allows the term to transform into a new equivalent
    /// term. Sometimes, it's not trivial to list all the possible
    /// equivalent terms into the premise.
    ///
    /// It's primarily used to resolve the trait-member terms or obtain the
    /// inferred term from an inference variable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    fn normalize<
        R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &mut self,
        term: &T,
        premise: &Premise,
        table: &Table<impl State>,
        session: &mut Limit<R>,
    ) -> Result<Option<T>, ExceedLimitError>;

    /// Checks if the given term outlives the given lifetime.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    fn outlives_satisfiability<
        R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &mut self,
        term: &T,
        lifetime: &Lifetime,
        premise: &Premise,
        table: &Table<impl State>,
        session: &mut Limit<R>,
    ) -> Result<Satisfiability, ExceedLimitError>;
}

/// The basic implementation of the semantic logic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Default;

impl Semantic<Lifetime> for Default {
    fn trivially_equals(&mut self, lhs: &Lifetime, rhs: &Lifetime) -> bool {
        lhs == rhs
    }

    fn normalize<R: Session<Lifetime> + Session<Type> + Session<Constant>>(
        &mut self,
        _: &Lifetime,
        _: &Premise,
        _: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Option<Lifetime>, ExceedLimitError> {
        Ok(None)
    }

    fn outlives_satisfiability<
        R: Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &mut self,
        term: &Lifetime,
        lifetime: &Lifetime,
        premise: &Premise,
        table: &Table<impl State>,
        limit: &mut Limit<R>,
    ) -> Result<Satisfiability, ExceedLimitError> {
        if term.is_static() {
            Ok(Satisfiability::Satisfied)
        } else {
            // reflexivity
            if equals(term, lifetime, premise, table, self, limit)? {
                Ok(Satisfiability::Satisfied)
            } else {
                Ok(Satisfiability::Unsatisfied)
            }
        }
    }
}

impl Semantic<Type> for Default {
    fn trivially_equals(&mut self, lhs: &Type, rhs: &Type) -> bool {
        lhs == rhs
    }

    #[allow(clippy::too_many_lines)]
    fn normalize<R: Session<Lifetime> + Session<Type> + Session<Constant>>(
        &mut self,
        term: &Type,
        premise: &Premise,
        table: &Table<impl State>,
        session: &mut Limit<R>,
    ) -> Result<Option<Type>, ExceedLimitError> {
        match term {
            // transform type alias into the aliased type equivalent
            Type::Symbol(Symbol {
                id: SymbolID::Type(id),
                generic_arguments,
            }) => {
                let Some(type_sym) = table.get(*id) else {
                    return Ok(None);
                };

                let mut type_aliased = type_sym.r#type.clone();
                let Ok(inst) = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    (*id).into(),
                    &type_sym.generic_declaration.parameters,
                ) else {
                    return Ok(None);
                };
                instantiation::instantiate(&mut type_aliased, &inst);

                Ok(Some(type_aliased))
            }

            // transform the trait-member into trait-implementation-type
            // equivalent
            Type::TraitMember(trait_member) => {
                let Some(trait_id) =
                    table.get(trait_member.id).map(|x| x.parent_id)
                else {
                    // invalid id
                    return Ok(None);
                };

                // resolve for the appropriate trait-implementation
                let Ok(mut result) = table.resolve_implementation(
                    trait_id,
                    &trait_member.parent_generic_arguments,
                    premise,
                    self,
                    session,
                ) else {
                    return Ok(None);
                };

                let Some(implementation_type_symbol) = table
                    .get(result.id)
                    .and_then(|x| {
                        x.implementation_type_ids_by_trait_type_id
                            .get(&trait_member.id)
                            .copied()
                    })
                    .and_then(|x| table.get(x))
                else {
                    return Ok(None);
                };

                let Some(implementation_symbol) = table.get(result.id) else {
                    return Ok(None);
                };

                let parent_lifetimes = implementation_symbol
                    .signature
                    .generic_declaration
                    .parameters
                    .lifetime_order()
                    .iter()
                    .map(|x| {
                        result.deduced_substitution.lifetimes.remove(
                            &MemberID { id: *x, parent: result.id.into() }
                                .into(),
                        )
                    })
                    .collect::<Option<Vec<_>>>();
                let parent_types = implementation_symbol
                    .signature
                    .generic_declaration
                    .parameters
                    .type_order()
                    .iter()
                    .map(|x| {
                        result.deduced_substitution.types.remove(
                            &MemberID { id: *x, parent: result.id.into() }
                                .into(),
                        )
                    })
                    .collect::<Option<Vec<_>>>();
                let parent_constants = implementation_symbol
                    .signature
                    .generic_declaration
                    .parameters
                    .constant_order()
                    .iter()
                    .map(|x| {
                        result.deduced_substitution.constants.remove(
                            &MemberID { id: *x, parent: result.id.into() }
                                .into(),
                        )
                    })
                    .collect::<Option<Vec<_>>>();

                let (
                    Some(parent_lifetimes),
                    Some(parent_types),
                    Some(parent_constants),
                ) = (parent_lifetimes, parent_types, parent_constants)
                else {
                    return Ok(None);
                };

                // append the deduced generic arguments
                Ok(Some(Type::MemberSymbol(MemberSymbol {
                    id: implementation_type_symbol.id.into(),
                    member_generic_arguments: trait_member
                        .member_generic_arguments
                        .clone(),
                    parent_generic_arguments: GenericArguments {
                        lifetimes: parent_lifetimes,
                        types: parent_types,
                        constants: parent_constants,
                    },
                })))
            }

            // transform trait-implementation-type into the aliased type
            Type::MemberSymbol(MemberSymbol {
                id: r#type::MemberSymbolID::TraitImplementation(id),
                member_generic_arguments,
                parent_generic_arguments,
            }) => {
                let Some(implementation_type_symbol) = table.get(*id) else {
                    return Ok(None);
                };
                let Some(implementation_symbol) =
                    table.get(implementation_type_symbol.parent_id)
                else {
                    return Ok(None);
                };

                let mut aliased = implementation_type_symbol.r#type.clone();

                let Ok(mut instantiation) =
                    Instantiation::from_generic_arguments(
                        parent_generic_arguments.clone(),
                        implementation_type_symbol.parent_id.into(),
                        &implementation_symbol
                            .signature
                            .generic_declaration
                            .parameters,
                    )
                else {
                    return Ok(None);
                };

                if instantiation
                    .append_from_generic_arguments(
                        member_generic_arguments.clone(),
                        implementation_type_symbol.id.into(),
                        &implementation_type_symbol
                            .generic_declaration
                            .parameters,
                    )
                    .is_err()
                {
                    return Ok(None);
                }

                instantiation::instantiate(&mut aliased, &instantiation);

                Ok(Some(aliased))
            }

            // transform into its aliased equivalent
            Type::MemberSymbol(MemberSymbol {
                id: r#type::MemberSymbolID::AdtImplementation(id),
                member_generic_arguments,
                parent_generic_arguments,
            }) => {
                let Some(implementation_type_symbol) = table.get(*id) else {
                    return Ok(None);
                };

                let Some(adt_implementation_symbol) =
                    table.get(implementation_type_symbol.parent_id)
                else {
                    return Ok(None);
                };

                // gets the decution for the parent generic arguments
                let Some(mut deduction) =
                    adt_implementation_symbol.signature.arguments.deduce(
                        parent_generic_arguments,
                        premise,
                        table,
                        self,
                        session,
                    )?
                else {
                    return Ok(None);
                };

                if deduction
                    .append_from_generic_arguments(
                        member_generic_arguments.clone(),
                        implementation_type_symbol.id.into(),
                        &implementation_type_symbol
                            .generic_declaration
                            .parameters,
                    )
                    .is_err()
                {
                    return Ok(None);
                }

                let mut aliased = implementation_type_symbol.r#type.clone();

                instantiation::instantiate(&mut aliased, &deduction);

                Ok(Some(aliased))
            }

            // unpack the tuple
            Type::Tuple(tuple) => {
                let contain_upacked =
                    tuple.elements.iter().any(term::TupleElement::is_unpacked);

                if !contain_upacked {
                    return Ok(None);
                }

                let mut result = Vec::new();

                for element in tuple.elements.iter().cloned() {
                    match element {
                        regular @ term::TupleElement::Regular(_) => {
                            result.push(regular);
                        }
                        term::TupleElement::Unpacked(term) => match term {
                            Type::Tuple(inner) => {
                                result.extend(inner.elements.iter().cloned());
                            }
                            term => {
                                result.push(term::TupleElement::Unpacked(term));
                            }
                        },
                    }
                }

                Ok(Some(Type::Tuple(r#type::Tuple { elements: result })))
            }

            _ => Ok(None),
        }
    }

    fn outlives_satisfiability<
        R: Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &mut self,
        term: &Type,
        _: &Lifetime,
        _: &Premise,
        _: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Satisfiability, ExceedLimitError> {
        match term {
            Type::Primitive(_) => Ok(Satisfiability::Satisfied),

            Type::Inference(_) | Type::Parameter(_) => {
                Ok(Satisfiability::Unsatisfied)
            }

            Type::TraitMember(_)
            | Type::Local(_)
            | Type::Symbol(_)
            | Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::MemberSymbol(_) => Ok(Satisfiability::Congruent),
        }
    }
}

impl Semantic<Constant> for Default {
    fn trivially_equals(&mut self, lhs: &Constant, rhs: &Constant) -> bool {
        lhs == rhs
    }

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

    fn outlives_satisfiability<
        R: Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &mut self,
        _: &Constant,
        _: &Lifetime,
        _: &Premise,
        _: &Table<impl State>,
        _: &mut Limit<R>,
    ) -> Result<Satisfiability, ExceedLimitError> {
        // constants value do not have lifetimes
        Ok(Satisfiability::Satisfied)
    }
}

#[cfg(test)]
pub(super) mod tests;
