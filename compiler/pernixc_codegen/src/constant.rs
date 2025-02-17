//! Mapping between the Pernix constant to the LLVM constant

use std::collections::HashMap;

use getset::Getters;
use inkwell::values::GlobalValue;

use crate::context::Context;

/// Represents the mapping between the Pernix constant and the LLVM constant.
#[derive(Debug, Default, Getters)]
pub struct Map<'ctx> {
    /// The global constant string that is used in the LLVM module.
    #[get = "pub"]
    global_values_by_string: HashMap<String, GlobalValue<'ctx>>,
}

impl<'ctx> Context<'_, 'ctx> {
    /// Gets the pointer to the global constant string that stores the given
    /// string value.
    pub fn get_global_const_string(
        &mut self,
        value: &str,
    ) -> inkwell::values::GlobalValue<'ctx> {
        if let Some(value) =
            self.constant_map().global_values_by_string().get(value).copied()
        {
            return value;
        };

        let string_array_type = self
            .context()
            .i8_type()
            .array_type(value.len().try_into().unwrap());

        let global = self.module().add_global(
            string_array_type,
            None,
            &format!("str_\"{value}\""),
        );

        global.set_linkage(inkwell::module::Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(
            &self.context().const_string(value.as_bytes(), false),
        );

        assert!(self
            .constant_map_mut()
            .global_values_by_string
            .insert(value.to_string(), global)
            .is_none());

        global
    }
}
