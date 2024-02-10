use std::fmt::Debug;

mod adt_implementation;
mod adt_implementation_constant;
mod adt_implementation_function;
mod adt_implementation_type;
mod constant;
mod r#enum;
mod function;
mod negative_trait_implementation;
mod r#struct;
mod r#trait;
mod trait_constant;
mod trait_function;
mod trait_implementation;
mod trait_implementation_constant;
mod trait_implementation_function;
mod trait_implementation_type;
mod trait_type;
mod r#type;
mod variant;

/// A trait for building a symbol.
pub trait Symbol {
    type SyntaxTree: Debug;
    type Flag: Default;
    type Data: Debug + Send + Sync + Default;
}
