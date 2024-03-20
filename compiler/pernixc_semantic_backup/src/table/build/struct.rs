use super::Build;
use crate::{symbol::Struct, table::state::Config};

impl Build for Struct {
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
    }
}
