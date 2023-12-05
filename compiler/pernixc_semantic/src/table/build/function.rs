use super::Build;
use crate::{symbol::Function, table::state::Config};

impl Build for Function {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
