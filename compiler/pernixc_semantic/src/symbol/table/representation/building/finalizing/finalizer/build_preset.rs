use std::{fmt::Debug, hash::Hash};

use crate::symbol::table::representation::building::finalizing::finalize::{
    adt_implementation, adt_implementation_constant,
    adt_implementation_function, adt_implementation_type, constant, function,
    negative_trait_implementation, r#enum, r#struct, r#trait, r#type,
    trait_constant, trait_function, trait_implementation,
    trait_implementation_constant, trait_implementation_function,
    trait_implementation_type, trait_type, variant, StateFlag,
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
    fn adt_implementation() -> Option<StateFlag>;
    fn adt_implementation_constant() -> Option<StateFlag>;
    fn adt_implementation_function() -> Option<StateFlag>;
    fn adt_implementation_type() -> Option<StateFlag>;
    fn constant() -> Option<StateFlag>;
    fn r#enum() -> Option<StateFlag>;
    fn function() -> Option<StateFlag>;
    fn negative_trait_implementation() -> Option<StateFlag>;
    fn r#struct() -> Option<StateFlag>;
    fn r#trait() -> Option<StateFlag>;
    fn trait_constant() -> Option<StateFlag>;
    fn trait_function() -> Option<StateFlag>;
    fn trait_implementation() -> Option<StateFlag>;
    fn trait_implementation_constant() -> Option<StateFlag>;
    fn trait_implementation_function() -> Option<StateFlag>;
    fn trait_implementation_type() -> Option<StateFlag>;
    fn trait_type() -> Option<StateFlag>;
    fn r#type() -> Option<StateFlag>;
    fn variant() -> Option<StateFlag>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericParameter;

impl BuildPreset for GenericParameter {
    fn adt_implementation() -> Option<StateFlag> {
        Some(adt_implementation::GENERIC_PARAMETER_STATE)
    }
    fn adt_implementation_constant() -> Option<StateFlag> {
        Some(adt_implementation_constant::GENERIC_PARAMETER_STATE)
    }
    fn adt_implementation_function() -> Option<StateFlag> {
        Some(adt_implementation_function::GENERIC_PARAMETER_STATE)
    }
    fn adt_implementation_type() -> Option<StateFlag> {
        Some(adt_implementation_type::GENERIC_PARAMETER_STATE)
    }
    fn constant() -> Option<StateFlag> {
        Some(constant::GENERIC_PARAMETER_STATE)
    }
    fn r#enum() -> Option<StateFlag> { Some(r#enum::GENERIC_PARAMETER_STATE) }
    fn function() -> Option<StateFlag> {
        Some(function::GENERIC_PARAMETER_STATE)
    }
    fn negative_trait_implementation() -> Option<StateFlag> {
        Some(negative_trait_implementation::GENERIC_PARAMETER_STATE)
    }
    fn r#struct() -> Option<StateFlag> {
        Some(r#struct::GENERIC_PARAMETER_STATE)
    }
    fn r#trait() -> Option<StateFlag> { Some(r#trait::GENERIC_PARAMETER_STATE) }
    fn trait_constant() -> Option<StateFlag> {
        Some(trait_constant::GENERIC_PARAMETER_STATE)
    }
    fn trait_function() -> Option<StateFlag> {
        Some(trait_function::GENERIC_PARAMETER_STATE)
    }
    fn trait_implementation() -> Option<StateFlag> {
        Some(trait_implementation::GENERIC_PARAMETER_STATE)
    }
    fn trait_implementation_constant() -> Option<StateFlag> {
        Some(trait_implementation_constant::GENERIC_PARAMETER_STATE)
    }
    fn trait_implementation_function() -> Option<StateFlag> {
        Some(trait_implementation_function::GENERIC_PARAMETER_STATE)
    }
    fn trait_implementation_type() -> Option<StateFlag> {
        Some(trait_implementation_type::GENERIC_PARAMETER_STATE)
    }
    fn trait_type() -> Option<StateFlag> {
        Some(trait_type::GENERIC_PARAMETER_STATE)
    }
    fn r#type() -> Option<StateFlag> { Some(r#type::GENERIC_PARAMETER_STATE) }
    fn variant() -> Option<StateFlag> { Some(variant::GENERIC_PARAMETER_STATE) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Check;

impl BuildPreset for Check {
    fn adt_implementation() -> Option<StateFlag> {
        Some(adt_implementation::CHECK_STATE)
    }
    fn adt_implementation_constant() -> Option<StateFlag> {
        Some(adt_implementation_constant::CHECK_STATE)
    }
    fn adt_implementation_function() -> Option<StateFlag> {
        Some(adt_implementation_function::DEFINITION_AND_CHECK_STATE)
    }
    fn adt_implementation_type() -> Option<StateFlag> {
        Some(adt_implementation_type::CHECK_STATE)
    }
    fn constant() -> Option<StateFlag> { Some(constant::CHECK_STATE) }
    fn r#enum() -> Option<StateFlag> { Some(r#enum::CHECK_STATE) }
    fn function() -> Option<StateFlag> {
        Some(function::DEFINITION_AND_CHECK_STATE)
    }
    fn negative_trait_implementation() -> Option<StateFlag> {
        Some(negative_trait_implementation::CHECK_STATE)
    }
    fn r#struct() -> Option<StateFlag> { Some(r#struct::CHECK_STATE) }
    fn r#trait() -> Option<StateFlag> { Some(r#trait::IMPLEMENTATIONS_STATE) }
    fn trait_constant() -> Option<StateFlag> {
        Some(trait_constant::CHECK_STATE)
    }
    fn trait_function() -> Option<StateFlag> {
        Some(trait_function::CHECK_STATE)
    }
    fn trait_implementation() -> Option<StateFlag> {
        Some(trait_implementation::CHECK_STATE)
    }
    fn trait_implementation_constant() -> Option<StateFlag> {
        Some(trait_implementation_constant::CHECK_STATE)
    }
    fn trait_implementation_function() -> Option<StateFlag> {
        Some(trait_implementation_function::DEFINITION_AND_CHECK_STATE)
    }
    fn trait_implementation_type() -> Option<StateFlag> {
        Some(trait_implementation_type::CHECK_STATE)
    }
    fn trait_type() -> Option<StateFlag> { Some(trait_constant::CHECK_STATE) }
    fn r#type() -> Option<StateFlag> { Some(r#type::CHECK_STATE) }
    fn variant() -> Option<StateFlag> { Some(variant::CHECK_STATE) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Complete;

impl BuildPreset for Complete {
    fn adt_implementation() -> Option<StateFlag> {
        Some(adt_implementation::COMPLETE_STATE)
    }
    fn adt_implementation_constant() -> Option<StateFlag> {
        Some(adt_implementation_constant::COMPLETE_STATE)
    }
    fn adt_implementation_function() -> Option<StateFlag> {
        Some(adt_implementation_function::SIGNATURE_STATE)
    }
    fn adt_implementation_type() -> Option<StateFlag> {
        Some(adt_implementation_type::COMPLETE_STATE)
    }
    fn constant() -> Option<StateFlag> { Some(constant::COMPLETE_STATE) }
    fn r#enum() -> Option<StateFlag> { Some(r#enum::COMPLETE_STATE) }
    fn function() -> Option<StateFlag> { Some(function::SIGNATURE_STATE) }
    fn negative_trait_implementation() -> Option<StateFlag> {
        Some(negative_trait_implementation::COMPLETE_STATE)
    }
    fn r#struct() -> Option<StateFlag> { Some(r#struct::COMPLETE_STATE) }
    fn r#trait() -> Option<StateFlag> { Some(r#trait::IMPLEMENTATIONS_STATE) }
    fn trait_constant() -> Option<StateFlag> {
        Some(trait_constant::COMPLETE_STATE)
    }
    fn trait_function() -> Option<StateFlag> {
        Some(trait_function::SIGNATURE_STATE)
    }
    fn trait_implementation() -> Option<StateFlag> {
        Some(trait_implementation::COMPLETE_STATE)
    }
    fn trait_implementation_constant() -> Option<StateFlag> {
        Some(trait_implementation_constant::COMPLETE_STATE)
    }
    fn trait_implementation_function() -> Option<StateFlag> {
        Some(trait_implementation_function::SIGNATURE_STATE)
    }
    fn trait_implementation_type() -> Option<StateFlag> {
        Some(trait_implementation_type::COMPLETE_STATE)
    }
    fn trait_type() -> Option<StateFlag> {
        Some(trait_constant::COMPLETE_STATE)
    }
    fn r#type() -> Option<StateFlag> { Some(r#type::COMPLETE_STATE) }
    fn variant() -> Option<StateFlag> { Some(variant::COMPLETE_STATE) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct PartialComplete;

impl BuildPreset for PartialComplete {
    fn adt_implementation() -> Option<StateFlag> {
        Some(adt_implementation::COMPLETE_STATE)
    }
    fn adt_implementation_constant() -> Option<StateFlag> {
        Some(adt_implementation_constant::COMPLETE_STATE)
    }
    fn adt_implementation_function() -> Option<StateFlag> {
        Some(adt_implementation_function::DEFINITION_AND_CHECK_STATE)
    }
    fn adt_implementation_type() -> Option<StateFlag> {
        Some(adt_implementation_type::COMPLETE_STATE)
    }
    fn constant() -> Option<StateFlag> { Some(constant::COMPLETE_STATE) }
    fn r#enum() -> Option<StateFlag> {
        Some(r#enum::STRUCTURAL_AND_PARTIAL_VARIANCE_STATE)
    }
    fn function() -> Option<StateFlag> {
        Some(function::DEFINITION_AND_CHECK_STATE)
    }
    fn negative_trait_implementation() -> Option<StateFlag> {
        Some(negative_trait_implementation::COMPLETE_STATE)
    }
    fn r#struct() -> Option<StateFlag> {
        Some(r#struct::STRUCTURAL_AND_PARTIAL_VARIANCE_STATE)
    }
    fn r#trait() -> Option<StateFlag> { Some(r#trait::IMPLEMENTATIONS_STATE) }
    fn trait_constant() -> Option<StateFlag> {
        Some(trait_constant::COMPLETE_STATE)
    }
    fn trait_function() -> Option<StateFlag> {
        Some(trait_function::SIGNATURE_STATE)
    }
    fn trait_implementation() -> Option<StateFlag> {
        Some(trait_implementation::COMPLETE_STATE)
    }
    fn trait_implementation_constant() -> Option<StateFlag> {
        Some(trait_implementation_constant::COMPLETE_STATE)
    }
    fn trait_implementation_function() -> Option<StateFlag> {
        Some(trait_implementation_function::DEFINITION_AND_CHECK_STATE)
    }
    fn trait_implementation_type() -> Option<StateFlag> {
        Some(trait_implementation_type::COMPLETE_STATE)
    }
    fn trait_type() -> Option<StateFlag> {
        Some(trait_constant::COMPLETE_STATE)
    }
    fn r#type() -> Option<StateFlag> { Some(r#type::COMPLETE_STATE) }
    fn variant() -> Option<StateFlag> { Some(variant::COMPLETE_STATE) }
}
