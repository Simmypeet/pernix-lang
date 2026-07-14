use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{GlobalSymbolID, member::get_members, name::get_name};
use qbice::storage::intern::Interned;

use super::{
    Application, ApplicationView, Constructor, InstanceAssociatedView, Tuple,
};
use crate::{
    instance_associated::get_instance_associated_type2,
    substitution::{Substitutable, Substitution},
    r#type::Type2,
};

impl Application {
    #[must_use]
    pub async fn reduce(
        &self,
        engine: &TrackedEngine,
    ) -> Option<Interned<Type2>> {
        match self.view() {
            ApplicationView::Symbolic(_)
            | ApplicationView::Primitive(_)
            | ApplicationView::Lifetime(_)
            | ApplicationView::AnonymousTraitInstance(_)
            | ApplicationView::FunctionPointer(_)
            | ApplicationView::Reference(_)
            | ApplicationView::EffectRowExtend(_)
            | ApplicationView::EffectRowEmpty => None,

            ApplicationView::Tuple(tuple) => {
                if tuple.unpacked_positions().is_empty() {
                    return None;
                }

                if !tuple.unpacked_positions().iter().any(|position| {
                    matches!(
                        &*self.arguments[*position],
                        Type2::Application(Self {
                            constructor: Constructor::Tuple(_),
                            ..
                        })
                    )
                }) {
                    return None;
                }

                Some(engine.intern(Type2::Application(
                    self.reduce_tuple(tuple.unpacked_positions(), engine),
                )))
            }

            ApplicationView::InstanceAssociated(instance_associated) => {
                self.reduce_instance_associated(instance_associated, engine)
                    .await
            }
        }
    }

    fn reduce_tuple(
        &self,
        unpacked_positions: &[usize],
        engine: &TrackedEngine,
    ) -> Self {
        let mut reduced_arguments = Vec::new();
        let mut reduced_unpacked_positions = Vec::new();

        for (idx, argument) in self.arguments.iter().enumerate() {
            if !unpacked_positions.contains(&idx) {
                reduced_arguments.push(argument.clone());
                continue;
            }

            if let Some((inner_tuple, args)) = argument.as_tuple() {
                let current_len = reduced_arguments.len();

                reduced_arguments.extend(args.iter().cloned());
                reduced_unpacked_positions.extend(
                    inner_tuple
                        .unpacked_positions
                        .iter()
                        .map(|position| current_len + position),
                );
            } else {
                reduced_unpacked_positions.push(reduced_arguments.len());
                reduced_arguments.push(argument.clone());
            }
        }

        Self {
            constructor: Constructor::Tuple(Tuple {
                unpacked_positions: engine
                    .intern_unsized(reduced_unpacked_positions),
            }),
            arguments: engine.intern_unsized(reduced_arguments),
        }
    }

    async fn reduce_instance_associated(
        &self,
        instance_associated: InstanceAssociatedView<'_>,
        engine: &TrackedEngine,
    ) -> Option<Interned<Type2>> {
        let (symbol_id, generic_args) =
            instance_associated.instance().as_symbolic()?;

        let instance_associated_name =
            engine.get_name(instance_associated.trait_associated_id()).await;
        let instance_associated_symbol_id = symbol_id.target_id.make_global(
            engine
                .get_members(symbol_id)
                .await
                .get_by_name(&instance_associated_name)?,
        );

        let mut substitution = Substitution::new();
        substitution
            .append_generic_arguments(symbol_id, generic_args, engine)
            .await;
        substitution
            .append_generic_arguments(
                instance_associated_symbol_id,
                instance_associated.generic_arguments(),
                engine,
            )
            .await;

        let instance_associated_type = engine
            .get_instance_associated_type2(instance_associated_symbol_id)
            .await;

        Some(instance_associated_type.apply_or_clone(&substitution, engine))
    }
}

impl Type2 {
    #[must_use]
    pub fn as_tuple(&self) -> Option<(&Tuple, &[Interned<Self>])> {
        if let Self::Application(Application {
            constructor: Constructor::Tuple(con),
            arguments,
        }) = &self
        {
            Some((con, arguments))
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_symbolic(&self) -> Option<(GlobalSymbolID, &[Interned<Self>])> {
        let Self::Application(application) = self else {
            return None;
        };
        let ApplicationView::Symbolic(symbolic) = application.view() else {
            return None;
        };

        Some((symbolic.symbol_id(), symbolic.generic_arguments()))
    }
}

#[cfg(test)]
mod test;
