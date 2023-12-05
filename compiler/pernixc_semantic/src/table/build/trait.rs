use super::Build;
use crate::{symbol::Trait, table::state::Config};

impl Build for Trait {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
