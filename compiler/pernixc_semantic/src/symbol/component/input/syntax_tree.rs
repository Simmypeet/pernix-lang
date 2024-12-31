//! Contains the definition of [`SyntaxTree`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use pernixc_syntax::syntax_tree;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        AdtImplementation, AdtImplementationFunction, Constant, Enum, Function,
        Global, Marker, NegativeMarkerImplementation,
        NegativeTraitImplementation, PositiveMarkerImplementation,
        PositiveTraitImplementation, Struct, Trait, TraitConstant,
        TraitFunction, TraitImplementationConstant,
        TraitImplementationFunction, TraitImplementationType, TraitType, Type,
    },
};

/// Used for storing the syntax tree for further processing.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct SyntaxTree<T>(pub T);

macro_rules! impl_syntax_tree {
    ($(($name:ident, $syn:ty)),*) => {
        paste! {
            #[derive(Debug, Clone, Default)]
            pub(in crate::symbol) struct Map {
                $(
                    [< $name:snake s >]:
                        HashMap<Global<ID<$name>>, SyntaxTree<$syn>>,
                )*
            }

            $(
                impl super::Input<SyntaxTree<$syn>> for $name {
                    type Requirement = super::Optional;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<ID<Self>>, SyntaxTree<$syn>> {
                        &representation.syntax_tree_map.[< $name:snake s >]
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<ID<Self>>, SyntaxTree<$syn>> {
                        &mut representation.syntax_tree_map.[< $name:snake s >]
                    }
                }
            )*
        }
    };
}

/// An enumeration of either a normal function syntax tee (with body) or an
/// extern function syntax (only signature).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum FunctionKind {
    Normal(syntax_tree::item::Function),
    Extern(syntax_tree::item::ExternFunction),
}

impl_syntax_tree!(
    (AdtImplementationFunction, syntax_tree::item::Function),
    (AdtImplementation, syntax_tree::item::ImplementationSignature),
    (Constant, syntax_tree::item::Constant),
    (Enum, syntax_tree::item::Enum),
    (Function, FunctionKind),
    (Marker, syntax_tree::item::Marker),
    (NegativeMarkerImplementation, syntax_tree::item::ImplementationSignature),
    (NegativeTraitImplementation, syntax_tree::item::ImplementationSignature),
    (PositiveMarkerImplementation, syntax_tree::item::ImplementationSignature),
    (PositiveTraitImplementation, syntax_tree::item::ImplementationSignature),
    (Struct, syntax_tree::item::Struct),
    (TraitConstant, syntax_tree::item::Constant),
    (TraitFunction, syntax_tree::item::Function),
    (TraitType, syntax_tree::item::Type),
    (TraitImplementationType, syntax_tree::item::Type),
    (TraitImplementationConstant, syntax_tree::item::Constant),
    (TraitImplementationFunction, syntax_tree::item::Function),
    (Trait, syntax_tree::item::TraitSignature),
    (Type, syntax_tree::item::Type)
);
