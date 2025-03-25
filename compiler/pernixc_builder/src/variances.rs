//! Contains the builder of the variance map component for the ADT.
//!
//! The variance must be calculated in target-level manner, meaning that to
//! calculate the variance of a particular type parameter, we must know the
//! variance of the whole target we're calculating the variance for.

use std::{collections::HashMap, sync::Arc};

use parking_lot::RwLock;
use pernixc_arena::ID;
use pernixc_component::{fields::Fields, variant::Variant};
use pernixc_handler::Handler;
use pernixc_table::{
    component::{Derived, Member, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table, TargetID,
};
use pernixc_term::{
    generic_parameter::{GenericParameters, LifetimeParameter, TypeParameter},
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
    variance::{Variance, Variances},
    Default,
};

use crate::builder;

/// An enumeration used for inferring variance of type/lifetime parameters.
///
/// FIXME: Find a better data structure for this; currently, this looks like
/// a linked list.
#[derive(Debug, Clone, PartialEq, Eq)]
enum VarianceVariable {
    /// The variance is already known.
    Constant(Variance),
    /// The variance is being transformed in the form of `0.xfrom(1)`.
    Transform(Box<VarianceVariable>, Box<VarianceVariable>),
    /// The variance of the type parameter is being inferred.
    InferringType(ID<TypeParameter>, pernixc_table::ID),
    /// The variance of the lifetime parameter is being inferred.
    InferringLifetime(ID<LifetimeParameter>, pernixc_table::ID),
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

#[derive(Default)]
struct Context {
    type_parameter_constraints:
        Vec<(ID<TypeParameter>, pernixc_table::ID, VarianceVariable)>,
    lifetime_parameter_constraints:
        Vec<(ID<LifetimeParameter>, pernixc_table::ID, VarianceVariable)>,

    variance_maps: HashMap<pernixc_table::ID, Variances>,
}

impl Context {
    fn evaluate(&self, variance_variable: &VarianceVariable) -> Variance {
        match variance_variable {
            VarianceVariable::Constant(variance) => *variance,
            VarianceVariable::Transform(this, other) => {
                let this = self.evaluate(this);
                let other = self.evaluate(other);

                this.xfrom(other)
            }
            VarianceVariable::InferringType(ty_id, sym_id) => self
                .variance_maps
                .get(sym_id)
                .and_then(|x| x.variances_by_type_ids.get(ty_id))
                .copied()
                .unwrap_or(Variance::Bivariant), // start off as bivariant
            VarianceVariable::InferringLifetime(lt_id, sym_id) => self
                .variance_maps
                .get(sym_id)
                .and_then(|x| x.variances_by_lifetime_ids.get(lt_id))
                .copied()
                .unwrap_or(Variance::Bivariant), // start off as bivariant
        }
    }

    fn collect_constraints(&mut self, target_id: TargetID, table: &Table) {
        for local_id in table.get_target(target_id).unwrap().all_symbols() {
            let global_id = GlobalID::new(target_id, local_id);
            let symbol_kind = *table.get::<SymbolKind>(global_id);

            // skip if the symbol is not an adt.
            if !symbol_kind.is_adt() {
                continue;
            }

            let Ok(generic_parameters) =
                table.query::<GenericParameters>(global_id)
            else {
                continue;
            };

            // if generic parameters doesn't have lifetime/typee parameters,
            // skip
            if generic_parameters.lifetimes().is_empty()
                && generic_parameters.types().is_empty()
            {
                continue;
            }

            match symbol_kind {
                SymbolKind::Struct => {
                    let Ok(fields) = table.query::<Fields>(global_id) else {
                        continue;
                    };

                    for field in fields.fields.items() {
                        self.collect_constraints_from_type(
                            target_id,
                            &field.r#type,
                            VarianceVariable::Constant(Variance::Covariant),
                            table,
                        );
                    }
                }
                SymbolKind::Enum => {
                    let members = table.get::<Member>(global_id);
                    for variant_id in members
                        .0
                        .values()
                        .copied()
                        .map(|x| GlobalID::new(target_id, x))
                    {
                        let Ok(variant) = table.query::<Variant>(variant_id)
                        else {
                            continue;
                        };

                        if let Some(associated_type) = &variant.associated_type
                        {
                            self.collect_constraints_from_type(
                                target_id,
                                associated_type,
                                VarianceVariable::Constant(Variance::Covariant),
                                table,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_constraints_from_lifetime(
        &mut self,
        target_id: TargetID,
        lt: &Lifetime<Default>,
        current_variance: VarianceVariable,
    ) {
        match lt {
            Lifetime::Error(_) | Lifetime::Static => {}

            Lifetime::Elided(_) => {
                unreachable!("elided lifetime in adt shouldn't be present");
            }

            Lifetime::Parameter(member_id) => {
                // add the constraint to the context
                if member_id.parent.target_id == target_id {
                    self.lifetime_parameter_constraints.push((
                        member_id.id,
                        member_id.parent.id,
                        current_variance,
                    ));
                }
            }
            Lifetime::Inference(never) => match *never {},
            Lifetime::Forall(_) => {
                unreachable!(
                    "forall lifetime should only appear in where clause \
                     predicates"
                );
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn collect_constraints_from_type(
        &mut self,
        target_id: TargetID,
        ty: &Type<Default>,
        current_variance: VarianceVariable,
        table: &Table,
    ) {
        match ty {
            Type::Error(_) | Type::Primitive(_) => {}

            Type::Parameter(member_id) => {
                // add the constraint to the context
                if member_id.parent.target_id == target_id {
                    self.type_parameter_constraints.push((
                        member_id.id,
                        member_id.parent.id,
                        current_variance,
                    ));
                }
            }

            Type::FunctionSignature(function_signature) => {
                for parameter in &function_signature.parameters {
                    let new_variance = current_variance.clone().xform(
                        VarianceVariable::Constant(Variance::Contravariant),
                    );

                    self.collect_constraints_from_type(
                        target_id,
                        parameter,
                        new_variance,
                        table,
                    );
                }

                let new_variance = current_variance
                    .xform(VarianceVariable::Constant(Variance::Covariant));

                self.collect_constraints_from_type(
                    target_id,
                    &function_signature.return_type,
                    new_variance,
                    table,
                );
            }

            Type::Inference(infer) => match *infer {},

            Type::Symbol(symbol) => {
                let symbol_kind = *table.get::<SymbolKind>(symbol.id);
                assert!(symbol_kind.is_adt(), "symbol must be an adt");

                // if the symbol is in the other linked target, then
                // the variance information is already present;
                // otherwise,  we'll use the inference variable.
                let variance_map = if symbol.id.target_id == target_id {
                    None
                } else {
                    Some(
                        table
                            .query::<Variances>(symbol.id)
                            .expect("should've been calculated"),
                    )
                };

                let Ok(generic_parameters) =
                    table.query::<GenericParameters>(symbol.id)
                else {
                    return;
                };

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

                    self.collect_constraints_from_type(
                        target_id,
                        ty,
                        new_variance,
                        table,
                    );
                }
            }

            Type::Pointer(pointer) => {
                let new_variance = current_variance
                    .xform(VarianceVariable::Constant(Variance::Bivariant));

                self.collect_constraints_from_type(
                    target_id,
                    &pointer.pointee,
                    new_variance,
                    table,
                );
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

                self.collect_constraints_from_type(
                    target_id,
                    &reference.pointee,
                    new_variance,
                    table,
                );
            }

            Type::Array(array) => {
                self.collect_constraints_from_type(
                    target_id,
                    &array.r#type,
                    current_variance,
                    table,
                );
            }

            Type::Tuple(tuple) => {
                for ty in &tuple.elements {
                    self.collect_constraints_from_type(
                        target_id,
                        &ty.term,
                        current_variance.clone(),
                        table,
                    );
                }
            }

            Type::Phantom(phantom) => {
                self.collect_constraints_from_type(
                    target_id,
                    &phantom.0,
                    current_variance,
                    table,
                );
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

                for ty in
                    trait_member.0.member_generic_arguments.types.iter().chain(
                        trait_member.0.parent_generic_arguments.types.iter(),
                    )
                {
                    let new_variance = current_variance
                        .clone()
                        .xform(VarianceVariable::Constant(Variance::Invariant));

                    self.collect_constraints_from_type(
                        target_id,
                        ty,
                        new_variance,
                        table,
                    );
                }
            }
        }
    }
}

pub(super) struct Builder {
    regular_builder: builder::Builder,
    context: RwLock<Option<Context>>,
}

impl Builder {
    pub(super) const fn new(regular_builder: builder::Builder) -> Self {
        Self { regular_builder, context: RwLock::new(None) }
    }
}

impl query::Builder<Variances> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Variances>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_variance_map() {
            return None;
        }

        let _scope = self.regular_builder.start_building(
            table,
            global_id,
            Variances::component_name(),
        );

        let target_id = global_id.target_id;

        let mut context = self.context.write();

        let context = context.get_or_insert_with(|| {
            let mut context = Context::default();

            context.collect_constraints(target_id, table);

            // keep iterating until there's no change in the context
            let mut changed = true;
            while changed {
                changed = false;

                for constraint in &context.type_parameter_constraints {
                    let (id, parent_id, constraint_variance) = constraint;

                    let constraint_variance =
                        context.evaluate(constraint_variance);

                    let old_variance = context
                        .variance_maps
                        .get(parent_id)
                        .and_then(|x| x.variances_by_type_ids.get(id))
                        .copied()
                        .unwrap_or(Variance::Bivariant);
                    let new_variance =
                        constraint_variance.get_lower_bound(old_variance);

                    if old_variance != new_variance {
                        *context
                            .variance_maps
                            .entry(*parent_id)
                            .or_default()
                            .variances_by_type_ids
                            .entry(*id)
                            .or_default() = new_variance;
                        changed = true;
                    }
                }

                for constraint in &context.lifetime_parameter_constraints {
                    let (id, parent_id, constraint_variance) = constraint;

                    let constraint_variance =
                        context.evaluate(constraint_variance);

                    let old_variance = context
                        .variance_maps
                        .get(parent_id)
                        .and_then(|x| x.variances_by_lifetime_ids.get(id))
                        .copied()
                        .unwrap_or(Variance::Bivariant);
                    let new_variance =
                        constraint_variance.get_lower_bound(old_variance);

                    if old_variance != new_variance {
                        *context
                            .variance_maps
                            .entry(*parent_id)
                            .or_default()
                            .variances_by_lifetime_ids
                            .entry(*id)
                            .or_default() = new_variance;
                        changed = true;
                    }
                }
            }

            context
        });

        let map =
            context.variance_maps.remove(&global_id.id).unwrap_or_else(|| {
                let Ok(generic_parameters) =
                    table.query::<GenericParameters>(global_id)
                else {
                    return Variances::default();
                };

                Variances {
                    variances_by_lifetime_ids: generic_parameters
                        .lifetime_order()
                        .iter()
                        .map(|x| (*x, Variance::Bivariant))
                        .collect(),
                    variances_by_type_ids: generic_parameters
                        .type_order()
                        .iter()
                        .map(|x| (*x, Variance::Bivariant))
                        .collect(),
                }
            });

        Some(Arc::new(map))
    }
}

#[cfg(test)]
mod test;
