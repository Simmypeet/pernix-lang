//! Contains the logic mapping from the Pernix type to LLVM's type.

use std::collections::HashMap;

use inkwell::{types::BasicTypeEnum, AddressSpace};
use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    r#type::{Primitive, Type},
    sub_term::TermLocation,
    visitor::MutableRecursive,
};

use crate::{context::Context, Model};

/// Represents the mapping between the Pernix type and the LLVM type.
#[derive(Debug)]
pub struct Map<'ctx> {
    llvm_types_by_pernix_type: HashMap<Type<Model>, BasicTypeEnum<'ctx>>,
}

struct PurgeLifetime;

impl MutableRecursive<Lifetime<Model>> for PurgeLifetime {
    fn visit(
        &mut self,
        term: &mut Lifetime<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        *term = Lifetime::Static;
        true
    }
}

impl MutableRecursive<Type<Model>> for PurgeLifetime {
    fn visit(
        &mut self,
        _: &mut Type<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<Model>> for PurgeLifetime {
    fn visit(
        &mut self,
        _: &mut Constant<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'ctx> Context<'ctx> {
    /// Retrieves the LLVM type from the Pernix type.
    pub fn get_type(&self, mut ty: Type<Model>) -> BasicTypeEnum<'ctx> {
        let mut pruge = PurgeLifetime;
        pernixc_term::visitor::accept_recursive_mut(&mut ty, &mut pruge);

        self.get_type_internal(&ty)
    }

    fn get_type_internal(&self, ty: &Type<Model>) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Primitive(primitive) => match primitive {
                Primitive::Int8 | Primitive::Uint8 => {
                    self.context().i8_type().into()
                }
                Primitive::Int16 | Primitive::Uint16 => {
                    self.context().i16_type().into()
                }
                Primitive::Int32 | Primitive::Uint32 => {
                    self.context().i32_type().into()
                }
                Primitive::Int64 | Primitive::Uint64 => {
                    self.context().i64_type().into()
                }

                Primitive::Float32 => self.context().f32_type().into(),
                Primitive::Float64 => self.context().f64_type().into(),
                Primitive::Bool => self.context().bool_type().into(),

                Primitive::Isize | Primitive::Usize => self
                    .context()
                    .ptr_sized_int_type(self.target_data(), None)
                    .into(),
            },

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }
            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }

            Type::Reference(_) | Type::Pointer(_) => {
                self.context().ptr_type(AddressSpace::default()).into()
            }

            Type::Inference(infer) => match *infer {},

            Type::Symbol(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::MemberSymbol(_)
            | Type::TraitMember(_) => todo!(),

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }
        }
    }
}
