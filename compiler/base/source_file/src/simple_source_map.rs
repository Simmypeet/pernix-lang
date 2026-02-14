//! Defines a simple source map for testing purposes.

use std::hash::Hash as _;

use dashmap::{
    DashMap,
    mapref::one::{Ref, RefMut},
};
use pernixc_target::TargetID;
use qbice::{Decode, Encode};
use siphasher::sip128::Hasher128;

use crate::{GlobalSourceID, LocalSourceID, SourceFile};

/// A map of source files, accessing through the [`GlobalSourceID`].
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct SimpleSourceMap {
    source_files_by_id: DashMap<GlobalSourceID, SourceFile>,
}

impl SimpleSourceMap {
    /// Creates a new empty [`SourceMap`].
    #[must_use]
    pub fn new() -> Self { Self { source_files_by_id: DashMap::new() } }

    /// Registers a source file in the map. If the source file is already
    /// registered, it returns the `Err(ID)` of the existing source file.
    /// If the source file is not registered, it returns the `Ok(ID)` of the
    /// newly registered source file.
    #[must_use]
    pub fn register(
        &self,
        target_id: TargetID,
        source: SourceFile,
    ) -> LocalSourceID {
        let mut hasher = siphasher::sip128::SipHasher24::default();
        source.path().hash(&mut hasher);

        let finalize_hash = |hasher: siphasher::sip128::SipHasher24| {
            let mut attempt = 0;
            loop {
                let mut final_hasher = hasher;
                attempt.hash(&mut final_hasher);

                let hash128 = final_hasher.finish128();
                let lo = hash128.h1;
                let hi = hash128.h2;

                let candidate_branch_id = LocalSourceID::new(lo, hi);

                // avoid hash collision
                if !self
                    .source_files_by_id
                    .contains_key(&target_id.make_global(candidate_branch_id))
                {
                    return candidate_branch_id;
                }

                attempt += 1;
            }
        };

        let hash = finalize_hash(hasher);

        // insert the source file into the map
        assert!(
            self.source_files_by_id
                .insert(target_id.make_global(hash), source)
                .is_none()
        );

        hash
    }

    /// Gets the source file by its ID.
    #[must_use]
    pub fn get(
        &self,
        id: GlobalSourceID,
    ) -> Option<Ref<'_, GlobalSourceID, SourceFile>> {
        self.source_files_by_id.get(&id)
    }

    /// Gets the source file by its ID.
    #[must_use]
    pub fn get_mut(
        &self,
        id: GlobalSourceID,
    ) -> Option<RefMut<'_, GlobalSourceID, SourceFile>> {
        self.source_files_by_id.get_mut(&id)
    }
}
