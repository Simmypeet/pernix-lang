use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
    parent::get_parent_global,
};
use pernixc_type::{
    generic_parameters::GenericParameterID,
    instance_associated::get_instance_associated_type2,
    substitution::{Substitutable, Substitution},
    symbol::TraitRef2,
    r#type::{
        Type2,
        constructor::{
            AnonymousTraitInstance, Constructor, InstanceAssociated,
        },
    },
};
use qbice::storage::intern::Interned;

use super::{InstanceSource, selection::Candidate};
use crate::solver::{OverflowError, Solver};

impl Solver<'_> {
    pub(super) async fn resolve_associated_candidate(
        &mut self,
        symbol: GlobalSymbolID,
        requested: &TraitRef2,
        request_skolems: &[Interned<Type2>],
    ) -> Result<Option<Candidate>, OverflowError> {
        let Some(deduction) =
            self.deduce_instance(symbol, requested, request_skolems).await?
        else {
            return Ok(None);
        };
        let parameters = deduction.parameters.clone();
        let resolved = self.finish_deduction(symbol, deduction).await?;
        let resolved = match resolved {
            Ok(resolved) => resolved,
            Err(error) => {
                return Ok(Some(Candidate {
                    value: Err(error),
                    source: InstanceSource::AssociatedInstance(symbol),
                }));
            }
        };
        let (resolved, constraints) = resolved;
        let Type2::Application(application) = &*resolved else {
            unreachable!("a finished instance deduction is symbolic")
        };

        let value = match self.engine().get_kind(symbol).await {
            Kind::TraitAssociatedInstance => {
                let parent = self
                    .engine()
                    .get_parent_global(symbol)
                    .await
                    .expect("an associated instance has a parent");
                let anonymous = Type2::new_application(
                    Constructor::AnonymousTraitInstance(
                        AnonymousTraitInstance::new(parent),
                    ),
                    [],
                    self.engine(),
                );
                Type2::new_application(
                    Constructor::InstanceAssociated(InstanceAssociated::new(
                        symbol,
                    )),
                    std::iter::once(anonymous)
                        .chain(application.arguments().iter().cloned()),
                    self.engine(),
                )
            }
            Kind::InstanceAssociatedInstance => {
                let mut substitution = Substitution::new();
                for ((id, _), argument) in
                    parameters.iter().zip(application.arguments().iter())
                {
                    substitution.insert_generic(
                        GenericParameterID::new(symbol, id),
                        argument.clone(),
                    );
                }
                self.engine()
                    .get_instance_associated_type2(symbol)
                    .await
                    .apply_or_clone(&substitution, self.engine())
            }
            Kind::Module
            | Kind::Struct
            | Kind::Trait
            | Kind::Enum
            | Kind::Type
            | Kind::Constant
            | Kind::Function
            | Kind::ExternFunction
            | Kind::Variant
            | Kind::TraitAssociatedType
            | Kind::TraitAssociatedFunction
            | Kind::TraitAssociatedConstant
            | Kind::Effect
            | Kind::EffectOperation
            | Kind::Marker
            | Kind::PositiveImplementation
            | Kind::NegativeImplementation
            | Kind::ImplementationAssociatedType
            | Kind::ImplementationAssociatedFunction
            | Kind::ImplementationAssociatedConstant
            | Kind::Instance
            | Kind::InstanceAssociatedType
            | Kind::InstanceAssociatedFunction
            | Kind::InstanceAssociatedConstant => {
                unreachable!("lexical candidate is not an associated instance")
            }
        };

        Ok(Some(Candidate {
            value: Ok((value, constraints)),
            source: InstanceSource::AssociatedInstance(symbol),
        }))
    }
}
