use pernixc_qbice::Interner;
use qbice::storage::intern::Interned;

use crate::{instantiation::Instantiation, r#type::Type};

impl Instantiation {
    fn instantiate_internal(
        &self,
        ty: &Interned<Type>,
        interner: &impl Interner,
    ) -> Option<Interned<Type>> {
        match &**ty {
            Type::GenericParameter(member_id) => {
                self.get_instantiated_type(*member_id).cloned()
            }

            Type::Application(application) => {
                let mut new_args = None::<Vec<_>>;

                for (i, arg) in application.arguments.iter().enumerate() {
                    let inst_arg = self.instantiate_internal(arg, interner);

                    match (inst_arg, new_args.as_mut()) {
                        // keep as it
                        (None, None) => {}

                        (None, Some(new_args)) => {
                            new_args.push(arg.clone());
                        }

                        (Some(inst_arg), None) => {
                            // recreate new_args with all the previous arguments
                            // that were not changed, and the new instantiated
                            // argument
                            let mut new_args_vec =
                                Vec::with_capacity(application.arguments.len());

                            for prev_arg in &application.arguments[..i] {
                                new_args_vec.push(prev_arg.clone());
                            }

                            new_args_vec.push(inst_arg);

                            new_args = Some(new_args_vec);
                        }

                        (Some(inst_arg), Some(new_args)) => {
                            new_args.push(inst_arg);
                        }
                    }
                }

                new_args.map(|x| {
                    interner.intern(Type::Application(super::Application {
                        constructor: application.constructor.clone(),
                        arguments: interner.intern_unsized(x),
                    }))
                })
            }

            Type::BoundVar(_) | Type::InferenceVariable(_) => None,
        }
    }

    pub fn instantiate(
        &self,
        ty: &Interned<Type>,
        interner: &impl Interner,
    ) -> Interned<Type> {
        self.instantiate_internal(ty, interner).unwrap_or_else(|| ty.clone())
    }
}
