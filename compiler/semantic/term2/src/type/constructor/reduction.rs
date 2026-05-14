use pernixc_qbice::TrackedEngine;
use qbice::storage::intern::Interned;

use super::{Application, Constructor, Tuple};
use crate::r#type::{Type, context::TyContext};

impl Application {
    #[must_use]
    pub fn reduce(
        &self,
        engine: &TrackedEngine,
        tyctx: &impl TyContext,
    ) -> Option<Self> {
        let _ = tyctx;

        match &self.constructor {
            Constructor::Adt(_)
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

                Some(self.reduce_tuple(tuple, engine))
            }

            Constructor::InstanceAssociated(_instance_associated) => {
                todo!()
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
}

#[cfg(test)]
mod test;
