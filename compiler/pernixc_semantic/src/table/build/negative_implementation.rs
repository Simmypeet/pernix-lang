use super::Build;
use crate::{symbol::NegativeImplementation, table::state::Config};

impl Build for NegativeImplementation {
    fn build(config: Config<Self>, build_flag: Self::Flag) {}
}
