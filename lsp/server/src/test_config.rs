//! Testing configuration for the LSP server.

use pernixc_extend::extend;
use pernixc_query::{Key, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// A query key to determine if the LSP server is running in test mode.
///
/// This is beneficial for enabling test-speicific behaviors without affecting
/// production code paths.
///
/// For example, snapshot tests often require consistent outputs that may
/// require additional processing or normalization steps. By checking this
/// configuration flag, the LSP server can adjust its behavior accordingly.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Key,
)]
#[value(bool)]
pub struct TestConfig;

/// Checks if the LSP server is running in test mode.
#[extend]
pub async fn is_testing_lsp(self: &TrackedEngine) -> bool {
    self.query(&TestConfig).await.unwrap()
}
