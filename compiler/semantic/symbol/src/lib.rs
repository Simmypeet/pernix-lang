//! Contains the logic for building the symbol table from the syntax tree.

use std::{hash::Hash, sync::Arc};

use pernixc_extend::extend;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_target::{
    Global, TargetID, get_invocation_arguments, get_target_seed,
};
use qbice::{Decode, Encode, Identifiable, Query, StableHash};
use siphasher::sip128::Hasher128;

pub mod accessibility;
pub mod final_implements;
pub mod kind;
pub mod linkage;
pub mod member;
pub mod module_kind;
pub mod name;
pub mod parent;
pub mod scope_span;
pub mod source_file_module;
pub mod source_map;
pub mod span;
pub mod syntax;
pub mod r#unsafe;
pub mod variant_declaration_order;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a unique identifier for the symbols in the compilation target.
/// This ID is only unique within the context of a single target. If wants to
/// use identifier across multiple targets, it should be combined with the
/// [`Global`]
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
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct ID {
    lo: u64,
    hi: u64,
}

impl ID {
    /// Creates a new ID from the given u128 value.
    #[allow(clippy::cast_possible_truncation)]
    #[must_use]
    pub const fn from_u128(value: u128) -> Self {
        Self { lo: value as u64, hi: (value >> 64) as u64 }
    }

    /// Creates a new ID from the given low and high u64 values.
    #[must_use]
    pub const fn from_lo_hi(lo: u64, hi: u64) -> Self { Self { lo, hi } }
}

/// A kind of ID used to unique identify a symbol inside a particular global
/// symbol.
///
/// For example, we can use this struct to uniquely identify a generic parameter
/// inside a particular function symbol. Where the [`Self::parent_id`] is the ID
/// of the function symbol and the [`Self::id`] is the ID of the generic
/// parameter.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    derive_new::new,
)]
pub struct MemberID<InnerID> {
    /// The parent ID of the member, which is the ID of the symbol that
    /// contains this member.
    pub parent_id: Global<ID>,

    /// The ID of the member.
    pub id: InnerID,
}

/// Calculates the ID of the symbol with the given sequence of qualified names
/// and the target ID.
///
/// The ID is calculated by hashing the sequence of names, the target ID, and
/// the declaration order.
///
/// The declaration order is used to handle cases of the redefinition of symbols
/// with the same name in the same scope. In case of the symbol with no
/// redefinition, passing `0` as the declaration order is sufficient.
#[extend]
#[allow(clippy::collection_is_never_read)]
pub async fn calculate_qualified_name_id<'a>(
    self: &TrackedEngine,
    qualified_name_sequence: impl IntoIterator<Item = &'a str>,
    target_id: TargetID,
    parent_id: Option<ID>,
    declaration_order: usize,
) -> ID {
    let mut hasher = siphasher::sip128::SipHasher24::default();
    let target_seed = self.get_target_seed(target_id).await;

    target_seed.hash(&mut hasher);

    // signify that we're generating ID for the qualified name
    true.hash(&mut hasher);
    parent_id.hash(&mut hasher);

    for name in qualified_name_sequence {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }

    declaration_order.hash(&mut hasher);

    ID::from_u128(hasher.finish128().into())
}

/// Calculates a symbol [`ID`] for the implements at the given qualified
/// identifier span.
#[extend]
pub async fn calculate_implements_id(
    self: &TrackedEngine,
    qualified_identifier_span: &RelativeSpan,
    target_id: TargetID,
) -> ID {
    let mut hasher = siphasher::sip128::SipHasher24::default();
    let target_seed = self.get_target_seed(target_id).await;

    target_seed.hash(&mut hasher);

    // signify that we're generating ID for the qualified name
    false.hash(&mut hasher);

    // relative span where the qualified identifier of the implements located
    // is unique for each implements
    qualified_identifier_span.hash(&mut hasher);

    ID::from_u128(hasher.finish128().into())
}

/// Calculates a symbol [`ID`] for the implements with the given unique name.
#[extend]
pub async fn calculate_implements_id_by_unique_name(
    self: &TrackedEngine,
    unique_name: &str,
    target_id: TargetID,
) -> ID {
    let mut hasher = siphasher::sip128::SipHasher24::default();
    let target_seed = self.get_target_seed(target_id).await;

    target_seed.hash(&mut hasher);

    // signify that we're generating ID for the qualified name
    false.hash(&mut hasher);
    unique_name.hash(&mut hasher);

    ID::from_u128(hasher.finish128().into())
}

/// Returns the root module ID for the given target ID.
#[extend]
pub async fn get_target_root_module_id(
    self: &TrackedEngine,
    target_id: TargetID,
) -> ID {
    if target_id == TargetID::CORE {
        self.calculate_qualified_name_id(
            std::iter::once("core"),
            target_id,
            None,
            0,
        )
        .await
    } else {
        let invocation_arguments =
            self.get_invocation_arguments(target_id).await;
        let target_name = invocation_arguments.command.input().target_name();

        self.calculate_qualified_name_id(
            std::iter::once(target_name.as_str()),
            target_id,
            None,
            0,
        )
        .await
    }
}

/// Retrieves all symbol IDs in the given target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<[ID]>)]
#[extend(name = get_all_symbol_ids, by_val)]
pub struct AllSymbolIDKey {
    /// The target ID to get all symbol IDs for.
    pub target_id: TargetID,
}

/// Retrieves all ADT symbol IDs in the given target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<[ID]>)]
#[extend(name = get_all_adt_ids, by_val)]
pub struct AllAdtIDKey {
    /// The target ID to get all ADT symbol IDs for.
    pub target_id: TargetID,
}

/// Retrieves all implements symbol IDs in the given target (including positive
/// and negative)
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<[ID]>)]
#[extend(name = get_all_implements_ids, by_val)]
pub struct AllImplementsIDKey {
    /// The target ID to get all implements symbol IDs for.
    pub target_id: TargetID,
}

/// Retrieves all function having body symbol IDs in the given target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<[ID]>)]
#[extend(name = get_all_function_with_body_ids, by_val)]
pub struct AllFunctionWithBodyIDKey {
    /// The target ID to get all function having body symbol IDs for.
    pub target_id: TargetID,
}
