//!  Contains the definition of the [`Kind`] enum.

use std::{hash::Hash, sync::Arc};

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};
use pernixc_tokio::scoped;

use crate::{get_table_of_symbol, ID};

/// An enumeration used to identify the kind of a symbol in the Pernix. This
/// value should be set to every symbol that is defined in the compilation
/// target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Default,
    StableHash,
    Value,
)]
#[allow(missing_docs)]
#[id(Global<ID>)]
#[extend(method(get_kind), no_cyclic)]
pub enum Kind {
    #[default]
    Module,
    Struct,
    Trait,
    Enum,
    Type,
    Constant,
    Function,
    ExternFunction,
    Variant,
    TraitType,
    TraitFunction,
    TraitConstant,
    Marker,
    PositiveImplementation,
    NegativeImplementation,
    ImplementationType,
    ImplementationFunction,
    ImplementationConstant,
}

impl Kind {
    /// Checks if this kind of symbol has a [`Member`] component.
    #[must_use]
    pub const fn has_member(&self) -> bool {
        matches!(
            self,
            Self::Module
                | Self::Enum
                | Self::Trait
                | Self::PositiveImplementation
        )
    }

    /// Checks if this kind of symbol has a [`Implemented`] component.
    #[must_use]
    pub const fn has_implemented(&self) -> bool {
        matches!(self, Self::Trait | Self::Enum | Self::Struct | Self::Marker)
    }

    /// Checks if the symbol is either a struct or an enum.
    #[must_use]
    pub const fn is_adt(&self) -> bool {
        matches!(self, Self::Struct | Self::Enum)
    }

    /// Gets the description string of the kind.
    #[must_use]
    pub const fn kind_str(&self) -> &'static str {
        match self {
            Self::Module => "module",
            Self::Struct => "struct",
            Self::Trait => "trait",
            Self::Enum => "enum",
            Self::Type => "type",
            Self::Constant => "constant",
            Self::Function => "function",
            Self::Variant => "variant",
            Self::TraitType => "trait type",
            Self::TraitFunction => "trait function",
            Self::TraitConstant => "trait constant",
            Self::ExternFunction => "extern function",
            Self::PositiveImplementation => "implementation",
            Self::NegativeImplementation => "negative implementation",
            Self::ImplementationFunction => "implementation function",
            Self::ImplementationType => "implementation type",
            Self::ImplementationConstant => "implementation constant",
            Self::Marker => "marker",
        }
    }

    /// Checks if the symbol kind has generic parameters.
    #[must_use]
    pub const fn has_generic_parameters(&self) -> bool {
        matches!(
            self,
            Self::Struct
                | Self::Trait
                | Self::Enum
                | Self::Type
                | Self::Constant
                | Self::Function
                | Self::TraitType
                | Self::TraitFunction
                | Self::TraitConstant
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationType
                | Self::ImplementationFunction
                | Self::ImplementationConstant
        )
    }

    /// Checks if the symbol kind has a where clause.
    #[must_use]
    pub const fn has_where_clause(&self) -> bool {
        matches!(
            self,
            Self::Struct
                | Self::Trait
                | Self::Enum
                | Self::Type
                | Self::Constant
                | Self::Function
                | Self::TraitType
                | Self::TraitFunction
                | Self::TraitConstant
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationType
                | Self::ImplementationFunction
                | Self::ImplementationConstant
        )
    }

    /// Checks if the symbol is a kind of `implements SYMBOL`.
    #[must_use]
    pub const fn is_implementation(&self) -> bool {
        matches!(
            self,
            Self::PositiveImplementation | Self::NegativeImplementation
        )
    }

    /// Checks if the symbol has a type alias definition such as `type T =
    /// TYPE_ALIAS`.
    ///
    /// Trait type symbol is not included becuase it doesn't have the definition
    /// of the type alias such as `trait T { type U; }`
    #[must_use]
    pub const fn has_type_alias(&self) -> bool {
        matches!(self, Self::Type | Self::ImplementationType)
    }

    /// Checks if the symbol has a function signature.
    #[must_use]
    pub const fn has_function_signature(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has implied predicates component.
    #[must_use]
    pub const fn has_implied_predicates(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has elided lifetimes component.
    #[must_use]
    pub const fn has_elided_lifetimes(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has a variance map component.
    #[must_use]
    pub const fn has_variance_map(&self) -> bool {
        matches!(self, Self::Struct | Self::Enum)
    }

    /// Checks if the symbol has a function body
    #[must_use]
    pub const fn has_function_body(&self) -> bool {
        matches!(self, Self::Function | Self::ImplementationFunction)
    }
}

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Kind, CyclicError> {
    let table = engine.get_table_of_symbol(key.0).await;

    Ok(table
        .kinds
        .get(&key.0.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}

/// A query for retrieving all the symbol IDs of a specific kind in the
/// compilation target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<[ID]>)]
pub struct AllSymbolOfKindKey {
    /// The target ID to collect for the symbols.
    pub target_id: TargetID,

    /// The kind of the symbols to collect.
    pub kind: Kind,
}

#[pernixc_query::executor(
    key(AllSymbolOfKindKey),
    name(AllSymbolOfKindExecutor)
)]
pub async fn all_symbol_of_kind_executor(
    &AllSymbolOfKindKey { target_id, kind }: &AllSymbolOfKindKey,
    engine: &TrackedEngine,
) -> Result<Arc<[ID]>, CyclicError> {
    let map = engine.query(&crate::MapKey(target_id)).await?;

    scoped!(|handles| async move {
        for x in map.keys_by_symbol_id.keys() {
            let map = map.clone();
            let engine = engine.clone();
            let id = *x;

            handles.spawn(async move {
                let node_key = map
                    .keys_by_symbol_id
                    .get(&id)
                    .unwrap_or_else(|| panic!("invalid symbol ID: {id:?}"))
                    .as_ref()
                    .map_or_else(
                        || crate::Key::Root(target_id),
                        |x| crate::Key::Submodule {
                            external_submodule: x.clone(),
                            target_id,
                        },
                    );

                let node = engine.query(&crate::TableKey(node_key)).await?;

                if node.kinds.get(&id).unwrap() == &kind {
                    Ok(Some(id))
                } else {
                    Ok(None)
                }
            });
        }

        let mut results = Vec::new();
        while let Some(symbol) = handles.next().await {
            if let Some(symbol) = symbol? {
                results.push(symbol);
            }
        }

        Ok(Arc::from(results))
    })
}
