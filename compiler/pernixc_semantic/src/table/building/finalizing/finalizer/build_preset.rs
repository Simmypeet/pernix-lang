use std::{fmt::Debug, hash::Hash};

use crate::table::building::finalizing::finalize::{
    adt_implementation, adt_implementation_constant,
    adt_implementation_function, adt_implementation_type, constant, function,
    negative_trait_implementation, r#enum, r#struct, r#trait, r#type,
    trait_constant, trait_function, trait_implementation,
    trait_implementation_constant, trait_implementation_function,
    trait_implementation_type, variant, StateFlag,
};

/// A trait for determining the desired state of the symbol to build for each
/// kind of symbol.
pub trait BuildPreset:
    Clone
    + Debug
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + Send
    + Sync
    + 'static
{
    const ADT_IMPLEMENTATION: StateFlag;
    const ADT_IMPLEMENTATION_CONSTANT: StateFlag;
    const ADT_IMPLEMENTATION_FUNCTION: StateFlag;
    const ADT_IMPLEMENTATION_TYPE: StateFlag;
    const CONSTANT: StateFlag;
    const ENUM: StateFlag;
    const FUNCTION: StateFlag;
    const NEGATIVE_TRAIT_IMPLEMENTATION: StateFlag;
    const STRUCT: StateFlag;
    const TRAIT: StateFlag;
    const TRAIT_CONSTANT: StateFlag;
    const TRAIT_FUNCTION: StateFlag;
    const TRAIT_IMPLEMENTATION: StateFlag;
    const TRAIT_IMPLEMENTATION_CONSTANT: StateFlag;
    const TRAIT_IMPLEMENTATION_FUNCTION: StateFlag;
    const TRAIT_IMPLEMENTATION_TYPE: StateFlag;
    const TRAIT_TYPE: StateFlag;
    const TYPE: StateFlag;
    const VARIANT: StateFlag;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameter;

impl BuildPreset for GenericParameter {
    const ADT_IMPLEMENTATION: StateFlag =
        adt_implementation::GENERIC_PARAMETER_STATE;
    const ADT_IMPLEMENTATION_CONSTANT: StateFlag =
        adt_implementation_constant::GENERIC_PARAMETER_STATE;
    const ADT_IMPLEMENTATION_FUNCTION: StateFlag =
        adt_implementation_function::GENERIC_PARAMETER_STATE;
    const ADT_IMPLEMENTATION_TYPE: StateFlag =
        adt_implementation_type::GENERIC_PARAMETER_STATE;
    const CONSTANT: StateFlag = constant::GENERIC_PARAMETER_STATE;
    const ENUM: StateFlag = r#enum::GENERIC_PARAMETER_STATE;
    const FUNCTION: StateFlag = function::GENERIC_PARAMETER_STATE;
    const NEGATIVE_TRAIT_IMPLEMENTATION: StateFlag =
        negative_trait_implementation::GENERIC_PARAMETER_STATE;
    const STRUCT: StateFlag = r#struct::GENERIC_PARAMETER_STATE;
    const TRAIT: StateFlag = r#trait::GENERIC_PARAMETER_STATE;
    const TRAIT_CONSTANT: StateFlag = trait_constant::GENERIC_PARAMETER_STATE;
    const TRAIT_FUNCTION: StateFlag = trait_function::GENERIC_PARAMETER_STATE;
    const TRAIT_IMPLEMENTATION: StateFlag =
        trait_implementation::GENERIC_PARAMETER_STATE;
    const TRAIT_IMPLEMENTATION_CONSTANT: StateFlag =
        trait_implementation_constant::GENERIC_PARAMETER_STATE;
    const TRAIT_IMPLEMENTATION_FUNCTION: StateFlag =
        trait_implementation_function::GENERIC_PARAMETER_STATE;
    const TRAIT_IMPLEMENTATION_TYPE: StateFlag =
        trait_implementation_type::GENERIC_PARAMETER_STATE;
    const TRAIT_TYPE: StateFlag = trait_constant::GENERIC_PARAMETER_STATE;
    const TYPE: StateFlag = r#type::GENERIC_PARAMETER_STATE;
    const VARIANT: StateFlag = variant::GENERIC_PARAMETER_STATE;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Check;

impl BuildPreset for Check {
    const ADT_IMPLEMENTATION: StateFlag = adt_implementation::CHECK_STATE;
    const ADT_IMPLEMENTATION_CONSTANT: StateFlag =
        adt_implementation_constant::CHECK_STATE;
    const ADT_IMPLEMENTATION_FUNCTION: StateFlag =
        adt_implementation_function::DEFINITION_AND_CHECK_STATE;
    const ADT_IMPLEMENTATION_TYPE: StateFlag =
        adt_implementation_type::CHECK_STATE;
    const CONSTANT: StateFlag = constant::CHECK_STATE;
    const ENUM: StateFlag = r#enum::CHECK_STATE;
    const FUNCTION: StateFlag = function::DEFINITION_AND_CHECK_STATE;
    const NEGATIVE_TRAIT_IMPLEMENTATION: StateFlag =
        negative_trait_implementation::CHECK_STATE;
    const STRUCT: StateFlag = r#struct::CHECK_STATE;
    const TRAIT: StateFlag = r#trait::CHECK_STATE;
    const TRAIT_CONSTANT: StateFlag = trait_constant::CHECK_STATE;
    const TRAIT_FUNCTION: StateFlag = trait_function::CHECK_STATE;
    const TRAIT_IMPLEMENTATION: StateFlag = trait_implementation::CHECK_STATE;
    const TRAIT_IMPLEMENTATION_CONSTANT: StateFlag =
        trait_implementation_constant::CHECK_STATE;
    const TRAIT_IMPLEMENTATION_FUNCTION: StateFlag =
        trait_implementation_function::DEFINITION_AND_CHECK_STATE;
    const TRAIT_IMPLEMENTATION_TYPE: StateFlag =
        trait_implementation_type::CHECK_STATE;
    const TRAIT_TYPE: StateFlag = trait_constant::CHECK_STATE;
    const TYPE: StateFlag = r#type::CHECK_STATE;
    const VARIANT: StateFlag = variant::CHECK_STATE;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Complete;

impl BuildPreset for Complete {
    const ADT_IMPLEMENTATION: StateFlag = adt_implementation::COMPLETE_STATE;
    const ADT_IMPLEMENTATION_CONSTANT: StateFlag =
        adt_implementation_constant::COMPLETE_STATE;
    const ADT_IMPLEMENTATION_FUNCTION: StateFlag =
        adt_implementation_function::DEFINITION_AND_CHECK_STATE;
    const ADT_IMPLEMENTATION_TYPE: StateFlag =
        adt_implementation_type::COMPLETE_STATE;
    const CONSTANT: StateFlag = constant::COMPLETE_STATE;
    const ENUM: StateFlag = r#enum::COMPLETE_STATE;
    const FUNCTION: StateFlag = function::DEFINITION_AND_CHECK_STATE;
    const NEGATIVE_TRAIT_IMPLEMENTATION: StateFlag =
        negative_trait_implementation::COMPLETE_STATE;
    const STRUCT: StateFlag = r#struct::COMPLETE_STATE;
    const TRAIT: StateFlag = r#trait::IMPLEMENTATIONS_STATE;
    const TRAIT_CONSTANT: StateFlag = trait_constant::COMPLETE_STATE;
    const TRAIT_FUNCTION: StateFlag = trait_function::SIGNATURE_STATE;
    const TRAIT_IMPLEMENTATION: StateFlag =
        trait_implementation::COMPLETE_STATE;
    const TRAIT_IMPLEMENTATION_CONSTANT: StateFlag =
        trait_implementation_constant::COMPLETE_STATE;
    const TRAIT_IMPLEMENTATION_FUNCTION: StateFlag =
        trait_implementation_function::DEFINITION_AND_CHECK_STATE;
    const TRAIT_IMPLEMENTATION_TYPE: StateFlag =
        trait_implementation_type::COMPLETE_STATE;
    const TRAIT_TYPE: StateFlag = trait_constant::COMPLETE_STATE;
    const TYPE: StateFlag = r#type::COMPLETE_STATE;
    const VARIANT: StateFlag = variant::COMPLETE_STATE;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct PartialComplete;

impl BuildPreset for PartialComplete {
    const ADT_IMPLEMENTATION: StateFlag = adt_implementation::COMPLETE_STATE;
    const ADT_IMPLEMENTATION_CONSTANT: StateFlag =
        adt_implementation_constant::COMPLETE_STATE;
    const ADT_IMPLEMENTATION_FUNCTION: StateFlag =
        adt_implementation_function::DEFINITION_AND_CHECK_STATE;
    const ADT_IMPLEMENTATION_TYPE: StateFlag =
        adt_implementation_type::COMPLETE_STATE;
    const CONSTANT: StateFlag = constant::COMPLETE_STATE;
    const ENUM: StateFlag = r#enum::STRUCTURAL_AND_PARTIAL_VARIANCE_STATE;
    const FUNCTION: StateFlag = function::DEFINITION_AND_CHECK_STATE;
    const NEGATIVE_TRAIT_IMPLEMENTATION: StateFlag =
        negative_trait_implementation::COMPLETE_STATE;
    const STRUCT: StateFlag = r#struct::STRUCTURAL_AND_PARTIAL_VARIANCE_STATE;
    const TRAIT: StateFlag = r#trait::IMPLEMENTATIONS_STATE;
    const TRAIT_CONSTANT: StateFlag = trait_constant::COMPLETE_STATE;
    const TRAIT_FUNCTION: StateFlag = trait_function::SIGNATURE_STATE;
    const TRAIT_IMPLEMENTATION: StateFlag =
        trait_implementation::COMPLETE_STATE;
    const TRAIT_IMPLEMENTATION_CONSTANT: StateFlag =
        trait_implementation_constant::COMPLETE_STATE;
    const TRAIT_IMPLEMENTATION_FUNCTION: StateFlag =
        trait_implementation_function::DEFINITION_AND_CHECK_STATE;
    const TRAIT_IMPLEMENTATION_TYPE: StateFlag =
        trait_implementation_type::COMPLETE_STATE;
    const TRAIT_TYPE: StateFlag = trait_constant::COMPLETE_STATE;
    const TYPE: StateFlag = r#type::COMPLETE_STATE;
    const VARIANT: StateFlag = variant::COMPLETE_STATE;
}
