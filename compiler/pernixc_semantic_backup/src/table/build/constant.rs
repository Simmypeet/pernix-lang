use super::Build;
use crate::{symbol::Constant, table::state::Config};

impl Build for Constant {
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
    }
}
