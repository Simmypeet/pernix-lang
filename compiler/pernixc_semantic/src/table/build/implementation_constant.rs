use super::Build;
use crate::{symbol::ImplementationConstant, table::state::Config};

impl Build for ImplementationConstant {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
