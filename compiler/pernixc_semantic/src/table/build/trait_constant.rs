use super::Build;
use crate::{symbol::TraitConstant, table::state::Config};

impl Build for TraitConstant {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
