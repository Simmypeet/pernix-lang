//! Implements the [`Query`] for the [`ConstantType`]

use std::sync::Arc;

use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_symbol::kind::{get_kind, Kind};
use pernixc_term::{
    constant::Constant,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    lifetime::Lifetime,
    predicate::ConstantType,
    r#type::{Primitive, Type},
    variance::Variance,
    visitor::{self, Element},
};

use crate::{
    adt_fields::get_adt_fields,
    environment::{Call, DynArc, Environment, Query},
    normalizer::Normalizer,
    Error, Satisfiability, Satisfied, Succeeded,
};

#[derive(Debug)]
struct Visitor<'t, N: Normalizer> {
    constant_type: Result<Option<Succeeded<Satisfied>>, Error>,
    environment: &'t Environment<'t, N>,
}

impl<N: Normalizer> visitor::AsyncVisitor<Lifetime> for Visitor<'_, N> {
    async fn visit(
        &mut self,
        _: &Lifetime,
        _: <Lifetime as visitor::Element>::Location,
    ) -> bool {
        if self.constant_type.is_ok() {
            self.constant_type = Ok(None);
        }

        false
    }
}

impl<N: Normalizer> visitor::AsyncVisitor<Type> for Visitor<'_, N> {
    async fn visit(
        &mut self,
        term: &Type,
        _: <Type as visitor::Element>::Location,
    ) -> bool {
        match &mut self.constant_type {
            Ok(Some(satisified)) => {
                match Box::pin(self.environment.query_with(
                    &ConstantType(term.clone()),
                    (),
                    QuerySource::Normal,
                ))
                .await
                {
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

impl<N: Normalizer> visitor::AsyncVisitor<Constant> for Visitor<'_, N> {
    async fn visit(
        &mut self,
        _: &Constant,
        _: <Constant as visitor::Element>::Location,
    ) -> bool {
        true
    }
}

async fn try_get_adt_fields(
    ty: &Type,
    engine: &TrackedEngine,
) -> Result<Option<Vec<Type>>, executor::CyclicError> {
    let Type::Symbol(symbol) = ty else {
        return Ok(None);
    };

    if !matches!(engine.get_kind(symbol.id).await, Kind::Enum | Kind::Struct) {
        return Ok(None);
    }

    let mut results = Vec::new();

    let generic_parameters = engine.get_generic_parameters(symbol.id).await?;

    let instantiation = Instantiation::from_generic_arguments(
        symbol.generic_arguments.clone(),
        symbol.id,
        &generic_parameters,
    )
    .unwrap();

    for mut result in engine.get_adt_fields(symbol.id).await?.iter().cloned() {
        instantiation.instantiate(&mut result);

        results.push(result);
    }

    Ok(Some(results))
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

impl Query for ConstantType {
    type Parameter = ();
    type InProgress = QuerySource;
    type Result = Succeeded<Satisfied>;
    type Error = Error;

    #[allow(clippy::too_many_lines)]
    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
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
            | Type::FunctionSignature(_)
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

            assert!(self.0.accept_one_level_async(&mut visitor).await.is_ok());

            if let Some(mut result) = visitor.constant_type? {
                // look for the fields of the term as well (if it's an ADT)
                let mut found_error = false;

                if let Some(fields) =
                    try_get_adt_fields(&self.0, environment.tracked_engine())
                        .await?
                {
                    for field in fields {
                        let Some(new_result) =
                            Box::pin(environment.query_with(
                                &Self(field.clone()),
                                (),
                                QuerySource::Normal,
                            ))
                            .await?
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
            .premise()
            .predicates
            .iter()
            .filter_map(|x| x.as_constant_type())
        {
            if let Some(result) = environment
                .subtypes(
                    self.0.clone(),
                    premise_term.0.clone(),
                    Variance::Covariant,
                )
                .await?
            {
                if !result.result.forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    result.constraints.clone(),
                ))));
            }
        }

        // satisfiable with equivalence
        for Succeeded { result: eq, constraints } in
            environment.get_equivalences(&self.0).await?.iter()
        {
            if let Some(result) = environment
                .query_with(&Self(eq.clone()), (), QuerySource::FromEquivalence)
                .await?
            {
                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    result
                        .constraints
                        .iter()
                        .cloned()
                        .chain(constraints.iter().cloned())
                        .collect(),
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
