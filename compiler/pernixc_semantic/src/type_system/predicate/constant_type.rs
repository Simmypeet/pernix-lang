use std::sync::Arc;

use pernixc_table::{DisplayObject, Table};
use serde::{Deserialize, Serialize};

use super::{contains_error, Satisfiability};
use crate::type_system::{
    self,
    compatible::{Compatibility, Compatible},
    equivalence::get_equivalences,
    instantiation::{self, Instantiation},
    model::{Default, Model},
    normalizer::Normalizer,
    query::{Call, DynArc, Query},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{Primitive, Type},
        Term,
    },
    variance::Variance,
    visitor::{self, Element},
    AbruptError, Environment, Satisfied, Succeeded,
};

#[derive(Debug)]
struct Visitor<'t, N: Normalizer<M>, M: Model> {
    constant_type: Result<Option<Succeeded<Satisfied, M>>, AbruptError>,
    environment: &'t Environment<'t, M, N>,
}

impl<'t, 'v, M: Model, N: Normalizer<M>> visitor::Visitor<'v, Lifetime<M>>
    for Visitor<'t, N, M>
{
    fn visit(
        &mut self,
        _: &Lifetime<M>,
        _: <Lifetime<M> as Element>::Location,
    ) -> bool {
        if self.constant_type.is_ok() {
            self.constant_type = Ok(None);
        }

        false
    }
}

impl<'a, 'v, M: Model, N: Normalizer<M>> visitor::Visitor<'v, Type<M>>
    for Visitor<'a, N, M>
{
    fn visit(
        &mut self,
        term: &Type<M>,
        _: <Type<M> as Element>::Location,
    ) -> bool {
        match &mut self.constant_type {
            Ok(Some(satisified)) => {
                match self.environment.query_with(
                    &ConstantType(term.clone()),
                    (),
                    QuerySource::Normal,
                ) {
                    Ok(Some(new_satisfied)) => {
                        satisified
                            .constraints
                            .extend(new_satisfied.constraints.iter().cloned());
                        true
                    }

                    Ok(None) => {
                        self.constant_type = Ok(None);
                        false
                    }

                    Err(err) => {
                        self.constant_type = Err(err);
                        false
                    }
                }
            }
            Ok(None) | Err(_) => false,
        }
    }
}

impl<'a, 'v, M: Model, N: Normalizer<M>> visitor::Visitor<'v, Constant<M>>
    for Visitor<'a, N, M>
{
    fn visit(
        &mut self,
        _: &Constant<M>,
        _: <Constant<M> as Element>::Location,
    ) -> bool {
        true
    }
}

/// Describes the source of the query for constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum QuerySource {
    /// Used to reason the satisfiability by querying it by an another
    /// equivalent type.
    FromEquivalence,

    /// Normal
    #[default]
    Normal,
}

/// Represents a type can be used as a type of a compile-time constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ConstantType<M: Model>(pub Type<M>);

impl<M: Model> Query for ConstantType<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = QuerySource;
    type Result = Succeeded<Satisfied, M>;
    type Error = type_system::AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        _: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        let satisfiability = match self.0 {
            Type::Primitive(primitive_type) => match primitive_type {
                Primitive::Int8
                | Primitive::Int16
                | Primitive::Int32
                | Primitive::Int64
                | Primitive::Uint8
                | Primitive::Uint16
                | Primitive::Uint32
                | Primitive::Uint64
                | Primitive::Bool
                | Primitive::Usize
                | Primitive::Isize => Satisfiability::Satisfied,

                Primitive::Float32 | Primitive::Float64 => {
                    Satisfiability::Unsatisfied
                }
            },

            Type::Error(_)
            | Type::TraitMember(_)
            | Type::Parameter(_)
            | Type::Inference(_) => Satisfiability::Unsatisfied,

            Type::Pointer(_)
            | Type::Symbol(_)
            | Type::MemberSymbol(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_) => Satisfiability::Congruent,
        };

        // trivially satisfiable
        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Arc::new(Succeeded::satisfied())));
        }

        // if the term is congruent, then we need to check the sub-terms
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                constant_type: Ok(Some(Succeeded::satisfied())),
                environment,
            };

            assert!(self.0.accept_one_level(&mut visitor).is_ok());

            if let Some(mut result) = visitor.constant_type? {
                // look for the fields of the term as well (if it's an ADT)
                let mut found_error = false;
                if let Some(fields) = self.0.get_adt_fields(environment.table) {
                    for field in fields {
                        let Some(new_result) = environment.query_with(
                            &Self(field.clone()),
                            (),
                            QuerySource::Normal,
                        )?
                        else {
                            found_error = true;
                            break;
                        };

                        result
                            .constraints
                            .extend(new_result.constraints.iter().cloned());
                    }
                }

                if !found_error {
                    return Ok(Some(Arc::new(result)));
                }
            }
        }

        // satisfiable with premises
        for premise_term in environment
            .premise
            .predicates
            .iter()
            .filter_map(|x| x.as_constant_type())
        {
            if let Some(Succeeded {
                result: Compatibility { forall_lifetime_errors, .. },
                constraints,
            }) = self.0.compatible(
                &premise_term.0,
                Variance::Covariant,
                environment,
            )? {
                if !forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    constraints,
                ))));
            }
        }

        // satisfiable with equivalence
        for Succeeded { result: eq, mut constraints } in
            get_equivalences(&self.0, environment)?
        {
            if let Some(result) = environment.query_with(
                &Self(eq.clone()),
                (),
                QuerySource::FromEquivalence,
            )? {
                constraints.extend(result.constraints.iter().cloned());
                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    constraints,
                ))));
            }
        }

        Ok(None)
    }

    fn on_cyclic(
        &self,
        (): Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        call_stacks: &[Call<DynArc, DynArc>],
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        for call in call_stacks.iter().skip(1) {
            let (Some(_), Some(in_progress)) = (
                call.query.downcast_ref::<Self>(),
                call.in_progress.downcast_ref::<QuerySource>(),
            ) else {
                continue;
            };

            if *in_progress == QuerySource::Normal {
                return Ok(Some(Arc::new(Succeeded::satisfied())));
            }
        }

        Ok(None)
    }
}

impl<M: Model> pernixc_table::Display for ConstantType<M>
where
    Type<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "const type {}", DisplayObject { display: &self.0, table })
    }
}

impl<M: Model> ConstantType<M> {
    /// Checks if the type contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool { contains_error(&self.0) }

    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        instantiation::instantiate(&mut self.0, instantiation);
    }

    /// Converts a constant type with the model `U` into the model `M`.
    pub fn from_other_model<U: Model>(term: ConstantType<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self(Type::from_other_model(term.0))
    }

    /// Tries to convert a negative marker with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        term: ConstantType<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self(Type::try_from_other_model(term.0)?))
    }

    /// Converts the [`ConstantType`] with [`Default`] model to the model `M`.
    #[must_use]
    pub fn from_default_model(predicate: ConstantType<Default>) -> Self {
        Self(Type::from_default_model(predicate.0))
    }
}
