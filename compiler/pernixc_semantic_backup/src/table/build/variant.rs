use super::Build;
use crate::{symbol::Variant, table::state::Config};

impl Build for Variant {
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
    }
}
