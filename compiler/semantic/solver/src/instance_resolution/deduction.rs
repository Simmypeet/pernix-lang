use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::{GenericParameterID, get_generic_parameters2},
    substitution::{Substitutable, Substitution},
    symbol::{TraitRef2, get_trait_ref_of_instance_symbol2},
    r#type::{Type2, kind::TyKind},
};

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

pub(super) struct Deduction {
    pub(super) substitution: Substitution,
    pub(super) parameters: qbice::storage::intern::Interned<
        pernixc_type::generic_parameters::GenericParameters2,
    >,
}

impl Solver<'_> {
    pub(super) async fn deduce_instance(
        &mut self,
        symbol_id: GlobalSymbolID,
        requested: &TraitRef2,
        request_skolems: &[qbice::storage::intern::Interned<Type2>],
    ) -> Result<Option<Deduction>, OverflowError> {
        let Some(candidate) =
            self.engine().get_trait_ref_of_instance_symbol2(symbol_id).await
        else {
            return Ok(None);
        };
        if candidate.trait_id() != requested.trait_id()
            || candidate.generic_arguments().len()
                != requested.generic_arguments().len()
        {
            return Ok(None);
        }

        let parameters = self.engine().get_generic_parameters2(symbol_id).await;
        let generic_instantiations = self.create_inference_instantiations(
            parameters.iter().map(|(_, parameter)| parameter.kind()),
        );
        let mut generic_substitution = Substitution::new();
        for ((id, _), replacement) in
            parameters.iter().zip(generic_instantiations.iter())
        {
            generic_substitution.insert_generic(
                GenericParameterID::new(symbol_id, id),
                replacement.clone(),
            );
        }

        let binder_instantiations =
            self.create_inference_instantiations(candidate.binder().kinds());
        let candidate_arguments = candidate.binder().instantiate(
            candidate.generic_arguments(),
            &binder_instantiations,
            self.engine(),
        );
        let mut matched = Substitution::new();
        let mut constraints = Constraints::new();
        for (head, subject) in
            candidate_arguments.iter().zip(requested.generic_arguments().iter())
        {
            let head = head
                .apply_or_clone(&generic_substitution, self.engine())
                .apply_or_clone(&matched, self.engine());
            let subject = subject.apply_or_clone(&matched, self.engine());
            let Some((new_substitution, new_constraints)) =
                self.match_types(&head, &subject).await
            else {
                return Ok(None);
            };
            matched.merge(&new_substitution);
            constraints = constraints.union_into(new_constraints);
        }

        let variables = self.hrtb_variables_from_instantiations(
            request_skolems
                .iter()
                .chain(generic_instantiations.iter())
                .chain(binder_instantiations.iter()),
        );
        constraints = constraints.apply_or_self(&matched, self.engine());
        if request_skolems.is_empty() {
            if !self.all_constraints_hold(constraints).await? {
                return Ok(None);
            }
        } else {
            let Some(cleaned) =
                self.check_and_clean_hrtb_constraints(&constraints, &variables)
            else {
                return Ok(None);
            };
            if !self.all_constraints_hold(cleaned).await? {
                return Ok(None);
            }
        }

        matched.compose(generic_substitution, self.engine());
        for (id, parameter) in parameters.iter() {
            if matches!(parameter.kind(), TyKind::Instance) {
                continue;
            }
            let id = GenericParameterID::new(symbol_id, id);
            let Some(argument) = matched.get_generic(id) else {
                return Ok(None);
            };
            if argument
                .apply_or_clone(&matched, self.engine())
                .as_ref()
                .contains_inference_variable()
            {
                return Ok(None);
            }
        }

        Ok(Some(Deduction { substitution: matched, parameters }))
    }
}
