use super::{contains_error, Satisfiability};
use crate::{
    symbol::table::{self, DisplayObject, State, Table},
    type_system::{
        compatible::{Compatibility, Compatible},
        equivalence::get_equivalences_with_context,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        query::{self, Context, Sealed},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, SymbolID, Type},
            Symbol, Term,
        },
        variance::Variance,
        visitor::{self, Element},
        Compute, Environment, Output, OverflowError, Satisfied, Succeeded,
    },
};

#[derive(Debug)]
struct Visitor<
    'a,
    'c,
    T: State,
    N: Normalizer<M, T>,
    O: Observer<M, T>,
    M: Model,
> {
    constant_type: Result<Output<Satisfied, M>, OverflowError>,
    environment: &'a Environment<'a, M, T, N, O>,
    context: &'c mut Context<M>,
}

impl<
        'a,
        'c,
        'v,
        M: Model,
        T: State,
        N: Normalizer<M, T>,
        O: Observer<M, T>,
    > visitor::Visitor<'v, Lifetime<M>> for Visitor<'a, 'c, T, N, O, M>
{
    fn visit(
        &mut self,
        _: &Lifetime<M>,
        _: <Lifetime<M> as Element>::Location,
    ) -> bool {
        if self.constant_type.is_ok() {
            self.constant_type = Ok(None)
        }

        false
    }
}

impl<
        'a,
        'c,
        'v,
        M: Model,
        T: State,
        N: Normalizer<M, T>,
        O: Observer<M, T>,
    > visitor::Visitor<'v, Type<M>> for Visitor<'a, 'c, T, N, O, M>
{
    fn visit(
        &mut self,
        term: &Type<M>,
        _: <Type<M> as Element>::Location,
    ) -> bool {
        match &mut self.constant_type {
            Ok(Some(satisified)) => {
                match ConstantType(term.clone()).query_with_context_full(
                    self.environment,
                    self.context,
                    (),
                    QuerySource::Normal,
                ) {
                    Ok(Some(new_satisfied)) => {
                        satisified
                            .constraints
                            .extend(new_satisfied.constraints);
                        true
                    }

                    result @ (Ok(None) | Err(_)) => {
                        self.constant_type = result;
                        false
                    }
                }
            }
            Ok(None) => false,
            Err(_) => false,
        }
    }
}

impl<
        'a,
        'c,
        'v,
        M: Model,
        T: State,
        N: Normalizer<M, T>,
        O: Observer<M, T>,
    > visitor::Visitor<'v, Constant<M>> for Visitor<'a, 'c, T, N, O, M>
{
    fn visit(
        &mut self,
        _: &Constant<M>,
        _: <Constant<M> as Element>::Location,
    ) -> bool {
        false
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType<M: Model>(pub Type<M>);

impl<M: Model> Compute for ConstantType<M> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        _: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
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

            Type::Symbol(Symbol { id, .. }) => match id {
                SymbolID::Struct(_) | SymbolID::Enum(_) => {
                    Satisfiability::Congruent
                }
            },

            Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::Local(_) => Satisfiability::Congruent,
        };

        // trivially satisfiable
        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Succeeded::satisfied()));
        }

        // if the term is congruent, then we need to check the sub-terms
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                constant_type: Ok(Some(Succeeded::satisfied())),
                environment,
                context,
            };

            let _ = self.0.accept_one_level(&mut visitor);

            if let Some(mut result) = visitor.constant_type? {
                // look for the fields of the term as well (if it's an ADT)
                let mut found_error = false;
                match self.0.get_adt_fields(environment.table) {
                    Some(fields) => {
                        for field in fields {
                            let Some(new_result) = ConstantType(field.clone())
                                .query_with_context_full(
                                    environment,
                                    context,
                                    (),
                                    QuerySource::Normal,
                                )?
                            else {
                                found_error = true;
                                break;
                            };

                            result.constraints.extend(new_result.constraints);
                        }
                    }
                    None => {}
                }

                if !found_error {
                    return Ok(Some(result));
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
            }) = self.0.compatible_with_context(
                &premise_term.0,
                Variance::Covariant,
                environment,
                context,
            )? {
                if !forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Succeeded::satisfied_with(constraints)));
            }
        }

        // satisfiable with equivalence
        for Succeeded { result: eq, constraints } in
            get_equivalences_with_context(&self.0, environment, context)?
        {
            if let Some(mut result) = ConstantType(eq.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (),
                    QuerySource::FromEquivalence,
                )?
            {
                result.constraints.extend(constraints);
                return Ok(Some(result));
            }
        }

        Ok(None)
    }

    #[allow(private_bounds, private_interfaces)]
    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        call_stacks: &[query::Record<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        for call in call_stacks.iter().skip(1) {
            let Some(query) = <Self as Sealed>::from_call(call) else {
                continue;
            };

            if query.in_progress == QuerySource::Normal {
                return Ok(Some(Succeeded::satisfied()));
            }
        }

        Ok(None)
    }
}

impl<S: State, M: Model> table::Display<S> for ConstantType<M>
where
    Type<M>: table::Display<S>,
{
    fn fmt(
        &self,
        table: &Table<S>,
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
}
