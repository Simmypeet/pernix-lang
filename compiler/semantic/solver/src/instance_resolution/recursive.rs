use std::sync::Arc;

use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::GenericParameterID,
    substitution::{Substitutable, Substitution},
    symbol::TraitRef2,
    r#type::Type2,
};
use qbice::storage::intern::Interned;

use super::{RecursiveError, ResolveError, deduction::Deduction};
use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    pub(super) async fn finish_deduction(
        &mut self,
        symbol_id: GlobalSymbolID,
        mut deduction: Deduction,
    ) -> Result<
        Result<(Interned<Type2>, Constraints), Arc<RecursiveError>>,
        OverflowError,
    > {
        let mut errors = Vec::new();
        let mut propagated_constraints = Constraints::new();

        for (parameter_id, parameter) in
            deduction.parameters.instance_parameters()
        {
            let parameter_id = GenericParameterID::new(symbol_id, parameter_id);
            let current = deduction
                .substitution
                .get_generic(parameter_id)
                .expect("every parameter is instantiated")
                .apply_or_clone(&deduction.substitution, self.engine());
            if !current.as_ref().contains_inference_variable() {
                continue;
            }

            let Some(required) = parameter.trait_ref() else {
                errors.push((
                    parameter_id,
                    ResolveError::NotFound,
                    TraitRef2::new(
                        symbol_id,
                        self.engine().intern_unsized(Vec::new()),
                        self.empty_binder(),
                    ),
                ));
                continue;
            };
            let required =
                required.apply_or_clone(&deduction.substitution, self.engine());
            let required =
                TraitRef2::from_symbol(required, self.empty_binder());

            match Box::pin(self.resolve_instance(&required)).await? {
                Ok((resolved, constraints)) => {
                    propagated_constraints =
                        propagated_constraints.union_into(constraints);
                    let Type2::InferenceVariable(variable) = &*current else {
                        errors.push((
                            parameter_id,
                            ResolveError::NotFound,
                            required,
                        ));
                        continue;
                    };
                    let mut replacement = Substitution::singleton(
                        *variable,
                        resolved.instance().clone(),
                    );
                    replacement.compose(deduction.substitution, self.engine());
                    deduction.substitution = replacement;
                }
                Err(error) => errors.push((parameter_id, error, required)),
            }
        }

        if !errors.is_empty() {
            return Ok(Err(Arc::new(RecursiveError {
                resolving_symbol: symbol_id,
                errors: errors.into(),
            })));
        }

        let mut arguments = Vec::with_capacity(deduction.parameters.len());
        for (id, _) in deduction.parameters.iter() {
            let id = GenericParameterID::new(symbol_id, id);
            let argument = deduction
                .substitution
                .get_generic(id)
                .expect("every generic parameter is instantiated")
                .apply_or_clone(&deduction.substitution, self.engine());
            if argument.as_ref().contains_inference_variable() {
                return Ok(Err(Arc::new(RecursiveError {
                    resolving_symbol: symbol_id,
                    errors: Arc::new([]),
                })));
            }
            arguments.push(argument);
        }

        Ok(Ok((
            Type2::new_symbolic_with_binder(
                symbol_id,
                self.empty_binder(),
                arguments,
                self.engine(),
            ),
            propagated_constraints,
        )))
    }
}
