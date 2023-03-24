use std::sync::Arc;

use derive_new::new;
use pernixc_common::source_file::{SourceFile, Span};

pub mod symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash, new)]
pub struct SourceSpan {
    pub source_file: Arc<SourceFile>,
    pub span:        Span,
}
