use super::Build;
use crate::{symbol::Variant, table::state::Config};

impl Build for Variant {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
