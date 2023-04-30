use derive_more::From;
use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs, clippy::module_name_repetitions)]
pub enum BindingError {}
