use crate::{arena::ID, symbol::GlobalID};

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

/// Contains all the generic information required by a symbol.
///
/// This trait is the main trait that is implemented by all the symbols.
/// It provides an interface to the symbol's arena in the table and mechanisms
/// to building the symbol.
pub(super) trait State: Sized + 'static
where
    ID<Self>: Into<GlobalID>,
{
    /// The syntax tree type that represents this symbol.
    type SyntaxTree;
}
