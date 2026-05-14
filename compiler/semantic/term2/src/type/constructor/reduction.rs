use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{GlobalSymbolID, member::get_members, name::get_name};
use qbice::storage::intern::Interned;

use super::{Application, Constructor, Tuple};
use crate::{
    instance_associated::get_instance_associated_type,
    instantiation::Instantiation,
    r#type::{Type, constructor::Symbolic, context::TyContext},
};

impl Application {
    #[must_use]
    pub async fn reduce(
        &self,
        engine: &TrackedEngine,
        _tyctx: &impl TyContext,
    ) -> Option<Interned<Type>> {
        match &self.constructor {
            Constructor::Symbolic(_)
            | Constructor::Primitive(_)
            | Constructor::Lifetime(_)
            | Constructor::Reference(_) => None,

            Constructor::Tuple(tuple) => {
                if tuple.unpacked_positions.is_empty() {
                    return None;
                }

                if !tuple.unpacked_positions.iter().any(|position| {
                    matches!(
                        &*self.arguments[*position],
                        Type::Application(Self {
                            constructor: Constructor::Tuple(_),
                            ..
                        })
                    )
                }) {
                    return None;
                }

                Some(engine.intern(Type::Application(
                    self.reduce_tuple(tuple, engine),
                )))
            }

            Constructor::InstanceAssociated(inst_assoc) => {
                self.reduce_instance_associated(inst_assoc, engine).await
            }
        }
    }

    fn reduce_tuple(&self, tuple: &Tuple, engine: &TrackedEngine) -> Self {
        let mut reduced_arguments = Vec::new();
        let mut reduced_unpacked_positions = Vec::new();

        for (idx, argument) in self.arguments.iter().enumerate() {
            if !tuple.unpacked_positions.contains(&idx) {
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
        inst_assoc: &super::InstanceAssociated,
        engine: &TrackedEngine,
    ) -> Option<Interned<Type>> {
        let instance = &self.arguments[0];
        let (symbol_id, generic_args) = instance.as_symbolic()?;

        let instance_associated_name =
            engine.get_name(inst_assoc.trait_associated_id).await;
        let instance_associated_symbol_id = symbol_id.target_id.make_global(
            engine
                .get_members(symbol_id)
                .await
                .get_by_name(&instance_associated_name)?,
        );

        let mut instantiation = Instantiation::default();
        instantiation
            .append_generic_arguments(symbol_id, generic_args, engine)
            .await;
        instantiation
            .append_generic_arguments(
                instance_associated_symbol_id,
                &self.arguments[1..],
                engine,
            )
            .await;

        let instance_associated_type = engine
            .get_instance_associated_type(instance_associated_symbol_id)
            .await;

        Some(instantiation.instantiate(&instance_associated_type, engine))
    }
}

impl Type {
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
        if let Self::Application(Application {
            constructor: Constructor::Symbolic(Symbolic { symbolic_id }),
            arguments,
        }) = &self
        {
            Some((*symbolic_id, arguments))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test;
