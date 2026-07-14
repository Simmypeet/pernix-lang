use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::{
        GenericParameterID, GenericParameters2, get_generic_parameters2,
    },
    substitution::{Substitutable, Substitution},
    symbol::{Symbol2, get_trait_ref_of_instance_symbol2},
    r#type::{Type2, inference::InferenceVariable, kind::TyKind},
};

use crate::{
    constraints::Constraints,
    instance_resolution::{InstanceResolutionFrame, ResolveSoftError},
    solver::{OverflowError, Solver},
};

/// The generic deduction and diagnostics produced for an instance symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeducedInstanceSymbol {
    substitution: Substitution,
    constraints: Constraints,
    soft_errors: Vec<ResolveSoftError>,
}

impl DeducedInstanceSymbol {
    const fn new(
        substitution: Substitution,
        constraints: Constraints,
        soft_errors: Vec<ResolveSoftError>,
    ) -> Self {
        Self { substitution, constraints, soft_errors }
    }

    /// Returns the deduced generic substitution.
    #[must_use]
    pub const fn substitution(&self) -> &Substitution { &self.substitution }

    /// Returns the constraints accumulated while deducing the symbol.
    #[must_use]
    pub const fn constraints(&self) -> &Constraints { &self.constraints }

    /// Returns the non-fatal errors from recursively resolved instances.
    #[must_use]
    pub fn soft_errors(&self) -> &[ResolveSoftError] { &self.soft_errors }

    /// Decomposes this result into its substitution, constraints, and soft
    /// errors.
    #[must_use]
    pub fn into_parts(
        self,
    ) -> (Substitution, Constraints, Vec<ResolveSoftError>) {
        (self.substitution, self.constraints, self.soft_errors)
    }
}

impl Solver<'_> {
    /// Deduces the generic arguments of an instance symbol from an expected
    /// trait reference.
    ///
    /// Returns `None` when the instance does not implement the expected trait,
    /// unification fails, or a lifetime/type parameter cannot be deduced.
    /// Instance parameters that are not deduced by unification are resolved
    /// recursively in declaration order.
    pub async fn deduce_instance_symbol(
        &mut self,
        symbol_id: GlobalSymbolID,
        expected_trait_ref: Symbol2,
    ) -> Result<Option<DeducedInstanceSymbol>, OverflowError> {
        let Some(instance_trait_ref) =
            self.engine().get_trait_ref_of_instance_symbol2(symbol_id).await
        else {
            return Ok(None);
        };
        let generic_parameters =
            self.engine().get_generic_parameters2(symbol_id).await;

        let generic_to_inference = self
            .fresh_generic_substitution_with_parameters(
                symbol_id,
                &generic_parameters,
                expected_trait_ref.max_universe(),
            );
        let required_parameters = get_required_parameters(
            symbol_id,
            &generic_parameters,
            &generic_to_inference,
        );
        let mut instance_parameters = Vec::new();

        for (id, parameter) in generic_parameters.iter() {
            let parameter_id = GenericParameterID::new(symbol_id, id);

            match parameter.kind() {
                TyKind::Type
                | TyKind::Lifetime
                | TyKind::EffectSignature
                | TyKind::EffectRow => {}
                TyKind::Instance => {
                    instance_parameters.push((
                        parameter_id,
                        get_parameter_inference(
                            &generic_to_inference,
                            parameter_id,
                        ),
                    ));
                }
            }
        }

        let instantiated_trait_ref = instance_trait_ref
            .as_ref()
            .apply_or_clone(&generic_to_inference, self.engine());

        let Some((mut unification, constraints)) = self
            .unify_trait_ref(instantiated_trait_ref, expected_trait_ref)
            .await?
        else {
            return Ok(None);
        };

        unification.compose(generic_to_inference, self.engine());

        if !are_required_parameters_deduced(&required_parameters, &unification)
        {
            return Ok(None);
        }

        let Some(mut deduction) = resolve_instance_parameters(
            self,
            symbol_id,
            &generic_parameters,
            instance_parameters,
            unification,
        )
        .await?
        else {
            return Ok(None);
        };

        deduction.constraints = constraints.union_into(deduction.constraints);

        Ok(Some(deduction))
    }
}

type ParameterInference = (GenericParameterID, InferenceVariable);

async fn resolve_instance_parameters(
    solver: &mut Solver<'_>,
    symbol_id: GlobalSymbolID,
    generic_parameters: &GenericParameters2,
    instance_parameters: Vec<ParameterInference>,
    mut substitution: Substitution,
) -> Result<Option<DeducedInstanceSymbol>, OverflowError> {
    let mut constraints = Constraints::default();
    let mut soft_errors = Vec::new();

    for (parameter_id, inference) in instance_parameters {
        let remains_undeduced =
            substitution.get_generic(parameter_id).is_some_and(|ty| {
                ty.as_ref() == &Type2::InferenceVariable(inference)
            });

        if !remains_undeduced {
            continue;
        }

        let Some(trait_ref) = generic_parameters[parameter_id.id()]
            .as_trait_ref_instance()
            .map(|trait_ref| {
                trait_ref.apply_or_clone(&substitution, solver.engine())
            })
        else {
            return Ok(None);
        };

        let Ok((resolved_instance, recursive_constraints)) =
            solver.resolve_instance(trait_ref.clone()).await?
        else {
            return Ok(None);
        };

        let frame = InstanceResolutionFrame::new(symbol_id, trait_ref);
        soft_errors.extend(
            resolved_instance.soft_errors().iter().cloned().map(|error| {
                error.prepend_instance_resolution_frame(frame.clone())
            }),
        );
        constraints = constraints.union_into(recursive_constraints);

        let mut recursive_substitution = Substitution::singleton(
            inference,
            resolved_instance.instance().clone(),
        );
        recursive_substitution.compose(substitution, solver.engine());
        substitution = recursive_substitution;
    }

    Ok(Some(DeducedInstanceSymbol::new(substitution, constraints, soft_errors)))
}

fn get_required_parameters(
    symbol_id: GlobalSymbolID,
    generic_parameters: &GenericParameters2,
    generic_to_inference: &Substitution,
) -> Vec<ParameterInference> {
    generic_parameters
        .iter()
        .filter_map(|(id, parameter)| match parameter.kind() {
            TyKind::Type
            | TyKind::Lifetime
            | TyKind::EffectSignature
            | TyKind::EffectRow => {
                let parameter_id = GenericParameterID::new(symbol_id, id);

                Some((
                    parameter_id,
                    get_parameter_inference(generic_to_inference, parameter_id),
                ))
            }
            TyKind::Instance => None,
        })
        .collect()
}

fn are_required_parameters_deduced(
    required_parameters: &[ParameterInference],
    substitution: &Substitution,
) -> bool {
    required_parameters.iter().all(|(parameter_id, inference)| {
        substitution.get_generic(*parameter_id).is_some_and(|ty| {
            ty.as_ref() != &Type2::InferenceVariable(*inference)
        })
    })
}

fn get_parameter_inference(
    substitution: &Substitution,
    parameter_id: GenericParameterID,
) -> InferenceVariable {
    substitution
        .get_generic(parameter_id)
        .expect("all generic parameters must be instantiated")
        .as_ref()
        .as_inference_variable()
        .copied()
        .unwrap()
}
