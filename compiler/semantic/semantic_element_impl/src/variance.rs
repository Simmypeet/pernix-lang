use std::sync::Arc;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::{
    fields::get_fields,
    variance::{get_variances, Variance, Variances},
    variant::get_variant_associated_type,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    get_all_adt_ids,
    kind::{get_kind, Kind},
    member::get_members,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    generic_parameters::{
        get_generic_parameters, LifetimeParameter, TypeParameter,
    },
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};
use pernixc_tokio::scoped;

/// An enumeration used for inferring variance of type/lifetime parameters.
///
/// FIXME: Find a better data structure for this; currently, this looks like
/// a linked list.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
enum VarianceVariable {
    /// The variance is already known.
    Constant(Variance),
    /// The variance is being transformed in the form of `0.xfrom(1)`.
    Transform(Box<VarianceVariable>, Box<VarianceVariable>),
    /// The variance of the type parameter is being inferred.
    InferringType(ID<TypeParameter>, pernixc_symbol::ID),
    /// The variance of the lifetime parameter is being inferred.
    InferringLifetime(ID<LifetimeParameter>, pernixc_symbol::ID),
}

impl VarianceVariable {
    fn xform(self, other: Self) -> Self {
        match (self, other) {
            (this, Self::Constant(Variance::Covariant)) => {
                // Applying a "covariant" transform is always a no-op
                this
            }

            (Self::Constant(c1), Self::Constant(c2)) => {
                Self::Constant(c1.xfrom(c2))
            }

            (this, other) => Self::Transform(Box::new(this), Box::new(other)),
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, StableHash)]
pub struct Constraints {
    type_parameter_constraints:
        Vec<(ID<TypeParameter>, pernixc_symbol::ID, VarianceVariable)>,
    lifetime_parameter_constraints:
        Vec<(ID<LifetimeParameter>, pernixc_symbol::ID, VarianceVariable)>,
}

pernixc_register::register!(ConstraintsKey, ConstraintsExecutor);

#[pernixc_query::query(
    key(ConstraintsKey),
    value(Arc<Constraints>),
    id(Global<pernixc_symbol::ID>),
    executor(ConstraintsExecutor)
)]
pub async fn constraints_executor(
    adt_id: Global<pernixc_symbol::ID>,
    engine: &TrackedEngine,
) -> Result<Arc<Constraints>, executor::CyclicError> {
    let mut constraints = Constraints::default();
    let kind = engine.get_kind(adt_id).await;

    match kind {
        Kind::Struct => {
            let fields = engine.get_fields(adt_id).await?;

            for field in fields.fields.items() {
                constraints
                    .collect_constraints_from_type(
                        adt_id.target_id,
                        &field.r#type,
                        VarianceVariable::Constant(Variance::Covariant),
                        engine,
                    )
                    .await?;
            }
        }

        Kind::Enum => {
            let members = engine.get_members(adt_id).await;

            for variant_id in members
                .member_ids_by_name
                .values()
                .copied()
                .chain(members.unnameds.iter().copied())
                .map(|x| adt_id.target_id.make_global(x))
            {
                let Some(ty) =
                    engine.get_variant_associated_type(variant_id).await?
                else {
                    continue;
                };

                constraints
                    .collect_constraints_from_type(
                        adt_id.target_id,
                        &ty,
                        VarianceVariable::Constant(Variance::Covariant),
                        engine,
                    )
                    .await?;
            }
        }

        _ => unreachable!("should've been invoked with adt"),
    }

    Ok(Arc::new(constraints))
}

fn evaluate(
    variance_variable: &VarianceVariable,
    map: &HashMap<pernixc_symbol::ID, Variances>,
) -> Variance {
    match variance_variable {
        VarianceVariable::Constant(variance) => *variance,
        VarianceVariable::Transform(this, other) => {
            let this = evaluate(this, map);
            let other = evaluate(other, map);

            this.xfrom(other)
        }
        VarianceVariable::InferringType(ty_id, sym_id) => map
            .get(sym_id)
            .and_then(|x| x.variances_by_type_ids.get(ty_id))
            .copied()
            .unwrap_or(Variance::Bivariant), // start off as bivariant
        VarianceVariable::InferringLifetime(lt_id, sym_id) => map
            .get(sym_id)
            .and_then(|x| x.variances_by_lifetime_ids.get(lt_id))
            .copied()
            .unwrap_or(Variance::Bivariant), // start off as bivariant
    }
}

impl Constraints {
    fn collect_constraints_from_lifetime(
        &mut self,
        target_id: TargetID,
        lt: &Lifetime,
        current_variance: VarianceVariable,
    ) {
        match lt {
            Lifetime::Error(_) | Lifetime::Static => {}

            Lifetime::Erased => {
                unreachable!("erased lifetime shouldn't be present")
            }

            Lifetime::Inference(_) => {
                unreachable!("inference lifetime shouldn't be present")
            }

            Lifetime::Elided(_) => {
                unreachable!("elided lifetime in adt shouldn't be present");
            }

            Lifetime::Parameter(member_id) => {
                // add the constraint to the context
                if member_id.parent_id.target_id == target_id {
                    self.lifetime_parameter_constraints.push((
                        member_id.id,
                        member_id.parent_id.id,
                        current_variance,
                    ));
                }
            }

            Lifetime::Forall(_) => {
                unreachable!(
                    "forall lifetime should only appear in where clause \
                     predicates"
                );
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn collect_constraints_from_type(
        &mut self,
        target_id: TargetID,
        ty: &Type,
        current_variance: VarianceVariable,
        engine: &TrackedEngine,
    ) -> Result<(), executor::CyclicError> {
        match ty {
            Type::Error(_) | Type::Primitive(_) => Ok(()),

            Type::Parameter(member_id) => {
                // add the constraint to the context
                if member_id.parent_id.target_id == target_id {
                    self.type_parameter_constraints.push((
                        member_id.id,
                        member_id.parent_id.id,
                        current_variance,
                    ));
                }

                Ok(())
            }

            Type::FunctionSignature(function_signature) => {
                Box::pin(async move {
                    for parameter in &function_signature.parameters {
                        let new_variance = current_variance.clone().xform(
                            VarianceVariable::Constant(Variance::Contravariant),
                        );

                        self.collect_constraints_from_type(
                            target_id,
                            parameter,
                            new_variance,
                            engine,
                        )
                        .await?;
                    }

                    let new_variance = current_variance
                        .xform(VarianceVariable::Constant(Variance::Covariant));

                    self.collect_constraints_from_type(
                        target_id,
                        &function_signature.return_type,
                        new_variance,
                        engine,
                    )
                    .await
                })
                .await
            }

            Type::Inference(_) => {
                unreachable!("inference type shouldn't be present in adt")
            }

            Type::Symbol(symbol) => {
                let symbol_kind = engine.get_kind(symbol.id).await;

                assert!(symbol_kind.is_adt(), "expected ADT kind");

                // if the symbol is in the other linked target, then
                // the variance information is already present;
                // otherwise,  we'll use the inference variable.
                let variance_map = if symbol.id.target_id == target_id {
                    None
                } else {
                    Some(engine.get_variances(symbol.id).await?)
                };

                let generic_parameters =
                    engine.get_generic_parameters(symbol.id).await?;

                for (lifetime, id) in symbol
                    .generic_arguments
                    .lifetimes
                    .iter()
                    .zip(generic_parameters.lifetime_order().iter())
                {
                    let other_variance = variance_map.as_ref().map_or(
                        VarianceVariable::InferringLifetime(*id, symbol.id.id),
                        |x| {
                            VarianceVariable::Constant(
                                x.variances_by_lifetime_ids
                                    .get(id)
                                    .copied()
                                    .unwrap(),
                            )
                        },
                    );
                    let new_variance =
                        current_variance.clone().xform(other_variance);

                    self.collect_constraints_from_lifetime(
                        target_id,
                        lifetime,
                        new_variance,
                    );
                }

                for (ty, id) in symbol
                    .generic_arguments
                    .types
                    .iter()
                    .zip(generic_parameters.type_order().iter())
                {
                    let other_variance = variance_map.as_ref().map_or(
                        VarianceVariable::InferringType(*id, symbol.id.id),
                        |x| {
                            VarianceVariable::Constant(
                                x.variances_by_type_ids
                                    .get(id)
                                    .copied()
                                    .unwrap(),
                            )
                        },
                    );
                    let new_variance =
                        current_variance.clone().xform(other_variance);

                    Box::pin(self.collect_constraints_from_type(
                        target_id,
                        ty,
                        new_variance,
                        engine,
                    ))
                    .await?;
                }

                Ok(())
            }

            Type::Pointer(pointer) => {
                let new_variance = current_variance
                    .xform(VarianceVariable::Constant(Variance::Bivariant));

                Box::pin(self.collect_constraints_from_type(
                    target_id,
                    &pointer.pointee,
                    new_variance,
                    engine,
                ))
                .await
            }

            Type::Reference(reference) => {
                self.collect_constraints_from_lifetime(
                    target_id,
                    &reference.lifetime,
                    current_variance.clone(),
                );

                let new_variance = if reference.qualifier == Qualifier::Mutable
                {
                    current_variance
                        .xform(VarianceVariable::Constant(Variance::Invariant))
                } else {
                    current_variance
                };

                Box::pin(self.collect_constraints_from_type(
                    target_id,
                    &reference.pointee,
                    new_variance,
                    engine,
                ))
                .await
            }

            Type::Array(array) => {
                Box::pin(self.collect_constraints_from_type(
                    target_id,
                    &array.r#type,
                    current_variance,
                    engine,
                ))
                .await
            }

            Type::Tuple(tuple) => {
                Box::pin(async move {
                    for ty in &tuple.elements {
                        self.collect_constraints_from_type(
                            target_id,
                            &ty.term,
                            current_variance.clone(),
                            engine,
                        )
                        .await?;
                    }

                    Ok(())
                })
                .await
            }

            Type::Phantom(phantom) => {
                Box::pin(self.collect_constraints_from_type(
                    target_id,
                    &phantom.0,
                    current_variance,
                    engine,
                ))
                .await
            }

            Type::MemberSymbol(_) => {
                unreachable!("member symbol should've been resolved");
            }

            Type::TraitMember(trait_member) => {
                for lifetime in trait_member
                    .0
                    .member_generic_arguments
                    .lifetimes
                    .iter()
                    .chain(
                        trait_member
                            .0
                            .parent_generic_arguments
                            .lifetimes
                            .iter(),
                    )
                {
                    let new_variance = current_variance
                        .clone()
                        .xform(VarianceVariable::Constant(Variance::Invariant));

                    self.collect_constraints_from_lifetime(
                        target_id,
                        lifetime,
                        new_variance,
                    );
                }

                Box::pin(async move {
                    for ty in trait_member
                        .0
                        .member_generic_arguments
                        .types
                        .iter()
                        .chain(
                            trait_member
                                .0
                                .parent_generic_arguments
                                .types
                                .iter(),
                        )
                    {
                        let new_variance = current_variance.clone().xform(
                            VarianceVariable::Constant(Variance::Invariant),
                        );

                        self.collect_constraints_from_type(
                            target_id,
                            ty,
                            new_variance,
                            engine,
                        )
                        .await?;
                    }

                    Ok(())
                })
                .await
            }
        }
    }
}

async fn collect_constraints(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<Vec<Arc<Constraints>>, executor::CyclicError> {
    // SAFETY: parallely collects the constraints for each ADT

    // NOTE: We have to retrieve all ADT ids first before entering parallel
    // mode, this makes sure that list of ADT ids is being verified first before
    // any of the constraint queries are being invoked.
    let adt_ids = engine.get_all_adt_ids(target_id).await;

    // Gets a list of constraints for each ADT id
    unsafe {
        engine.start_parallel();
    }

    let constraints_list = scoped!(|scoped| async move {
        let mut constraints_list = Vec::new();

        for constraint_id in adt_ids.iter().map(|x| target_id.make_global(*x)) {
            let engine = engine.clone();
            scoped.spawn(async move {
                engine.query(&ConstraintsKey(constraint_id)).await
            });
        }

        while let Some(res) = scoped.next().await {
            constraints_list.push(res?);
        }

        Ok(constraints_list)
    })?;

    unsafe {
        engine.end_parallel();
    }

    Ok(constraints_list)
}

pernixc_register::register!(MapKey, MapExecutor);

#[pernixc_query::query(
    key(MapKey),
    value(Arc<HashMap<pernixc_symbol::ID, Arc<Variances>>>),
    id(TargetID),
    executor(MapExecutor)
)]
pub async fn get_variance_maps(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<
    Arc<HashMap<pernixc_symbol::ID, Arc<Variances>>>,
    executor::CyclicError,
> {
    let constraints_list = collect_constraints(target_id, engine).await?;

    let mut variances_map = {
        let mut variances_map = HashMap::default();

        // keep iterating until there's no change in the context
        let mut changed = true;
        while changed {
            changed = false;

            for constraint in constraints_list
                .iter()
                .flat_map(|x| x.type_parameter_constraints.iter())
            {
                let (id, parent_id, constraint_variance) = constraint;

                let constraint_variance =
                    evaluate(constraint_variance, &variances_map);

                let old_variance = variances_map
                    .get(parent_id)
                    .and_then(|x| x.variances_by_type_ids.get(id))
                    .copied()
                    .unwrap_or(Variance::Bivariant);
                let new_variance =
                    constraint_variance.get_lower_bound(old_variance);

                if old_variance != new_variance {
                    *variances_map
                        .entry(*parent_id)
                        .or_default()
                        .variances_by_type_ids
                        .entry(*id)
                        .or_default() = new_variance;
                    changed = true;
                }
            }

            for constraint in constraints_list
                .iter()
                .flat_map(|x| x.lifetime_parameter_constraints.iter())
            {
                let (id, parent_id, constraint_variance) = constraint;

                let constraint_variance =
                    evaluate(constraint_variance, &variances_map);

                let old_variance = variances_map
                    .get(parent_id)
                    .and_then(|x| x.variances_by_lifetime_ids.get(id))
                    .copied()
                    .unwrap_or(Variance::Bivariant);
                let new_variance =
                    constraint_variance.get_lower_bound(old_variance);

                if old_variance != new_variance {
                    *variances_map
                        .entry(*parent_id)
                        .or_default()
                        .variances_by_lifetime_ids
                        .entry(*id)
                        .or_default() = new_variance;
                    changed = true;
                }
            }
        }

        variances_map
    };

    // finalizing: if the struct or enum doesn't appear in the variances map
    // due to their type/lifetime parameters are declared but not used, add to
    // to the map with default bivariant
    for adt_id in engine
        .get_all_adt_ids(target_id)
        .await
        .iter()
        .map(|x| target_id.make_global(*x))
    {
        let generic_parameters = engine.get_generic_parameters(adt_id).await?;

        let map = variances_map.entry(adt_id.id).or_default();

        for lt_param in generic_parameters.lifetime_order() {
            map.variances_by_lifetime_ids
                .entry(*lt_param)
                .or_insert(Variance::Bivariant);
        }

        for ty_param in generic_parameters.type_order() {
            map.variances_by_type_ids
                .entry(*ty_param)
                .or_insert(Variance::Bivariant);
        }
    }

    Ok(Arc::new(
        variances_map.into_iter().map(|(k, v)| (k, Arc::new(v))).collect(),
    ))
}

pernixc_register::register!(pernixc_semantic_element::variance::Key, Executor);

#[pernixc_query::executor(
    key(pernixc_semantic_element::variance::Key),
    name(Executor)
)]
pub async fn executor(
    key: &pernixc_semantic_element::variance::Key,
    engine: &TrackedEngine,
) -> Result<Arc<Variances>, executor::CyclicError> {
    let variance_maps = engine.query(&MapKey(key.0.target_id)).await?;

    let variances = variance_maps.get(&key.0.id).cloned().unwrap();

    Ok(variances)
}
