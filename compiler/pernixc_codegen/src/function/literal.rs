use pernixc_ir::value::literal::{Literal, Numeric};
use pernixc_term::r#type::Primitive;

use super::{Builder, Error, LlvmValue};
use crate::Model;

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    /// Gets the LLVM integer value of the given type.
    pub fn get_integer_value(
        &self,
        value: u64,
        ty: Primitive,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let llvm_int_type = match ty {
            Primitive::Int8 | Primitive::Uint8 => {
                self.context.context().i8_type()
            }
            Primitive::Int16 | Primitive::Uint16 => {
                self.context.context().i16_type()
            }
            Primitive::Int32 | Primitive::Uint32 => {
                self.context.context().i32_type()
            }
            Primitive::Int64 | Primitive::Uint64 => {
                self.context.context().i64_type()
            }
            Primitive::Usize | Primitive::Isize => self
                .context
                .context()
                .ptr_sized_int_type(self.context.target_data(), None),

            _ => unreachable!(),
        };

        llvm_int_type.const_int(value, false).into()
    }

    /// Translates numeric literal to LLVM value.
    pub fn get_numeric_value(
        &self,
        numeric_value: &Numeric<Model>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let primitive_type = numeric_value.r#type.as_primitive().unwrap();

        match primitive_type {
            Primitive::Int8
            | Primitive::Int16
            | Primitive::Int32
            | Primitive::Int64
            | Primitive::Uint8
            | Primitive::Uint16
            | Primitive::Uint32
            | Primitive::Uint64
            | Primitive::Isize
            | Primitive::Usize => self.get_integer_value(
                numeric_value.integer_string.parse::<u64>().unwrap(),
                *primitive_type,
            ),

            Primitive::Float32 | Primitive::Float64 => {
                let value = numeric_value.decimal_stirng.as_ref().map_or_else(
                    || numeric_value.integer_string.parse::<f64>().unwrap(),
                    |decimal_string| {
                        format!(
                            "{}.{}",
                            numeric_value.integer_string, decimal_string
                        )
                        .parse::<f64>()
                        .unwrap()
                    },
                );

                let llvm_float_type = match primitive_type {
                    Primitive::Float32 => self.context.context().f32_type(),
                    Primitive::Float64 => self.context.context().f64_type(),
                    _ => unreachable!(),
                };

                llvm_float_type.const_float(value).into()
            }

            Primitive::Bool => self
                .context
                .context()
                .bool_type()
                .const_int(
                    numeric_value.integer_string.parse::<u64>().unwrap(),
                    false,
                )
                .into(),
        }
    }

    /// Gets the hardcoded string value. The string value is stored somewhere
    /// in the `.data` section or similar in the final binary.
    pub fn get_string(
        &mut self,
        string: &pernixc_ir::value::literal::String,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        self.context
            .get_global_const_string(string.value.as_ref())
            .as_pointer_value()
            .into()
    }

    pub fn get_literal_value(
        &mut self,
        literal: &Literal<Model>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        match literal {
            Literal::String(string) => Ok(Some(self.get_string(string).into())),
            Literal::Character(character) => Ok(Some(
                self.get_integer_value(
                    character.character as u64,
                    *character.r#type.as_primitive().unwrap(),
                )
                .into(),
            )),
            Literal::Unreachable(_) => Err(Error::Unreachable),
            Literal::Numeric(numeric) => {
                Ok(Some(self.get_numeric_value(numeric).into()))
            }
            Literal::Boolean(boolean) => {
                Ok(Some(LlvmValue::Scalar(if boolean.value {
                    self.context.context().bool_type().const_all_ones().into()
                } else {
                    self.context.context().bool_type().const_zero().into()
                })))
            }
            Literal::Error(error) => {
                unreachable!("error literal found {error:?}")
            }

            Literal::Unit(_) | Literal::Phantom(_) => Ok(None),
        }
    }
}
