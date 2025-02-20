use inkwell::values::{AsValueRef, PointerValue};
use pernixc_ir::address::Address;

use super::{Builder, Error, LlvmAddress, LlvmValue};
use crate::{r#type::LlvmEnumSignature, Model};

/// Shortcircuits the function if the value is a `Zero-Sized type`.
macro_rules! into_regular_address {
    ($e:expr) => {
        match $e {
            Some(address) => address,
            None => return Ok(None),
        }
    };
}

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    /// Gets the LLVM address value of the given address.
    ///
    /// # Returns
    ///
    /// `Ok(None)` meanings that the address is pointer to `ZST` (Zero Sized
    /// Type) and further instructions shall be reduced to no-op.
    #[allow(clippy::too_many_lines)]
    pub fn get_address(
        &mut self,
        address: &Address<Model>,
    ) -> Result<Option<LlvmAddress<'ctx>>, Error> {
        match address {
            Address::Memory(memory) => {
                Ok(self.address_map.get(memory).copied())
            }

            Address::Field(field) => {
                let struct_ty = self
                    .type_of_address_pnx(&field.struct_address)
                    .into_symbol()
                    .unwrap();

                let base_address = into_regular_address!(
                    self.get_address(&field.struct_address)?
                );

                let struct_id = struct_ty.id;
                let Ok(llvm_struct) = self.context.get_struct_type(struct_ty)
                else {
                    return Ok(None);
                };

                let Some(llvm_field_index) = llvm_struct
                    .llvm_field_indices_by_field_id
                    .get(&field.id)
                    .copied()
                else {
                    return Ok(None);
                };

                let field_ty = llvm_struct.llvm_field_types[llvm_field_index];
                let gep = self
                    .inkwell_builder
                    .build_struct_gep(
                        llvm_struct.llvm_struct_type,
                        base_address.address,
                        llvm_field_index.try_into().unwrap(),
                        &format!(
                            "struct_{:?}_field_{:?}_gep",
                            struct_id, field.id
                        ),
                    )
                    .unwrap();

                Ok(Some(LlvmAddress::new(gep, field_ty)))
            }

            Address::Tuple(index) => {
                let tuple_ty = self
                    .type_of_address_pnx(&index.tuple_address)
                    .into_tuple()
                    .unwrap();

                let base_address = into_regular_address!(
                    self.get_address(&index.tuple_address)?
                );

                let elements_len = tuple_ty.elements.len();
                let Ok(llvm_tuple) = self.context.get_tuple_type(tuple_ty)
                else {
                    return Ok(None);
                };

                let tuple_index = match index.offset {
                    pernixc_ir::address::Offset::FromStart(a) => a,
                    pernixc_ir::address::Offset::FromEnd(b) => {
                        elements_len - 1 - b
                    }
                    pernixc_ir::address::Offset::Unpacked => unreachable!(),
                };

                let Some(llvm_field_index) = llvm_tuple
                    .llvm_field_indices_by_tuple_idnex
                    .get(&tuple_index)
                    .copied()
                else {
                    return Ok(None);
                };

                let tuple_element_ty =
                    llvm_tuple.llvm_element_types[llvm_field_index];

                let gep = self
                    .inkwell_builder
                    .build_struct_gep(
                        llvm_tuple.llvm_tuple_type,
                        base_address.address,
                        llvm_field_index.try_into().unwrap(),
                        &format!(
                            "tuple_{tuple_index:?}_field_{llvm_field_index:?\
                             }_gep"
                        ),
                    )
                    .unwrap();

                Ok(Some(LlvmAddress::new(gep, tuple_element_ty)))
            }

            Address::Index(index) => {
                // left to right, avoiding inconsistencies
                let base_address = self.get_address(&index.array_address)?;
                let index_value = match self.get_value(&index.indexing_value)? {
                    Some(LlvmValue::Scalar(index)) => index,
                    Some(LlvmValue::TmpAggegate(_)) => {
                        unreachable!("indexing value is not an tmp aggregate")
                    }

                    None => return Ok(None),
                };
                let base_address = into_regular_address!(base_address);

                let Ok(pointee_type) = self.context.get_type(
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
                    return Ok(None);
                };

                let element_type =
                    pointee_type.into_array_type().get_element_type();

                let gep = unsafe {
                    self.inkwell_builder
                        .build_gep(
                            pointee_type,
                            PointerValue::new(
                                base_address.address.as_value_ref(),
                            ),
                            &[
                                self.context.context().i64_type().const_zero(),
                                index_value.into_int_value(),
                            ],
                            "indexing",
                        )
                        .unwrap()
                };

                Ok(Some(LlvmAddress::new(gep, element_type)))
            }
            Address::Variant(variant_address) => {
                let address =
                    self.get_address(&variant_address.enum_address)?;
                let symbol = self
                    .type_of_address_pnx(&variant_address.enum_address)
                    .into_symbol()
                    .unwrap();

                let enum_signature = self.context.get_enum_type(symbol);

                match &*enum_signature {
                    LlvmEnumSignature::Zst => Ok(None),
                    LlvmEnumSignature::NullablePointer(_) => {
                        assert!(address.is_some(), "pointer is not zst");

                        Ok(address)
                    }
                    LlvmEnumSignature::Transparent(_) => Ok(address),
                    LlvmEnumSignature::Numeric(_) => {
                        unreachable!(
                            "numeric enum shouldn't have associated value"
                        )
                    }
                    LlvmEnumSignature::TaggedUnion(tagged_union) => {
                        let layout = tagged_union.llvm_variant_types
                            [&variant_address.id];

                        let payload_address = self
                            .inkwell_builder
                            .build_struct_gep(
                                layout,
                                address.unwrap().address,
                                1,
                                &format!(
                                    "variant_{:?}_payload_gep",
                                    variant_address.id
                                ),
                            )
                            .unwrap();

                        let payload_ty =
                            layout.get_field_type_at_index(1).unwrap();

                        Ok(Some(LlvmAddress::new(payload_address, payload_ty)))
                    }
                }
            }
            Address::Reference(reference) => {
                let pointee_address = into_regular_address!(
                    self.get_address(&reference.reference_address)?
                );

                let Ok(pointee_type) =
                    self.type_of_address(&reference.reference_address)
                else {
                    return Ok(None);
                };

                let Ok(loaded_type) = self.type_of_address(address) else {
                    return Ok(None);
                };

                let load = self
                    .inkwell_builder
                    .build_load(
                        pointee_type,
                        pointee_address.address,
                        "dereference",
                    )
                    .unwrap();

                Ok(Some(LlvmAddress::new(
                    load.into_pointer_value(),
                    loaded_type,
                )))
            }
        }
    }
}
