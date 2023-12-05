use super::Build;
use crate::{symbol::Enum, table::state::Config};

impl Build for Enum {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
