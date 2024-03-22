use super::Build;
use crate::{symbol::ImplementationType, table::state::Config};

impl Build for ImplementationType {
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
    }
}