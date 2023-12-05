use super::Build;
use crate::{symbol::Type, table::state::Config};

impl Build for Type {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
