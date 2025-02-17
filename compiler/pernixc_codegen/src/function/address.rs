use inkwell::values::{AsValueRef, PointerValue};
use pernixc_ir::address::Address;

use super::{Builder, Error, LlvmValue};
use crate::{into_basic, r#type::LlvmType, Model};

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    /// Gets the LLVM address value of the given address.
    #[allow(clippy::too_many_lines)]
    pub fn get_address_value(
        &mut self,
        address: &Address<Model>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        match address {
            Address::Memory(memory) => Ok(self
                .address_map
                .get(memory)
                .copied()
                .map_or(LlvmValue::Zst, Into::into)),

            Address::Field(field) => {
                let struct_ty = self.context.normalize_term(
                    self.function_ir
                        .values
                        .type_of_address(
                            &field.struct_address,
                            self.callable_id,
                            &self.environment,
                        )
                        .unwrap()
                        .result,
                );

                let struct_id = struct_ty.as_symbol().unwrap().id;

                let LlvmType::Basic(llvm_ty) =
                    self.context.get_type(struct_ty.clone())
                else {
                    return Ok(LlvmValue::Zst);
                };

                let Some(llvm_field_idx) = self
                    .context
                    .type_map()
                    .struct_sigantures()
                    .get(&struct_ty.into_symbol().unwrap())
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .llvm_field_indices_by_field_id
                    .get(&field.id)
                    .copied()
                else {
                    return Ok(LlvmValue::Zst);
                };

                let base_address =
                    into_basic!(self.get_address_value(&field.struct_address)?);

                Ok(LlvmValue::Basic(
                    self.inkwell_builder
                        .build_struct_gep(
                            llvm_ty,
                            base_address.into_pointer_value(),
                            llvm_field_idx.try_into().unwrap(),
                            &format!(
                                "struct_{:?}_field_{:?}_gep",
                                struct_id, field.id
                            ),
                        )
                        .unwrap()
                        .into(),
                ))
            }

            Address::Tuple(_) => todo!(),

            Address::Index(index) => {
                // left to right, avoiding inconsistencies
                let base_address =
                    self.get_address_value(&index.array_address)?;
                let index_value =
                    into_basic!(self.get_value(&index.indexing_value)?);
                let base_address = into_basic!(base_address);

                let LlvmType::Basic(pointee_type) = self.context.get_type(
                    self.context.monomorphize_term(
                        self.function_ir
                            .values
                            .type_of_address(
                                &index.array_address,
                                self.callable_id,
                                &self.environment,
                            )
                            .unwrap()
                            .result,
                        self.instantiation,
                    ),
                ) else {
                    return Ok(LlvmValue::Zst);
                };

                unsafe {
                    Ok(LlvmValue::Basic(
                        self.inkwell_builder
                            .build_gep(
                                pointee_type,
                                PointerValue::new(base_address.as_value_ref()),
                                &[
                                    self.context
                                        .context()
                                        .i64_type()
                                        .const_zero(),
                                    index_value.into_int_value(),
                                ],
                                "indexing",
                            )
                            .unwrap()
                            .into(),
                    ))
                }
            }
            Address::Variant(_) => todo!(),
            Address::Reference(reference) => {
                let pointee_address = into_basic!(
                    self.get_address_value(&reference.reference_address)?
                );

                let LlvmType::Basic(pointee_type) =
                    self.type_of_address(address)
                else {
                    return Ok(LlvmValue::Zst);
                };

                Ok(LlvmValue::Basic(
                    self.inkwell_builder
                        .build_load(
                            pointee_type,
                            pointee_address.into_pointer_value(),
                            "dereference",
                        )
                        .unwrap(),
                ))
            }
        }
    }
}
