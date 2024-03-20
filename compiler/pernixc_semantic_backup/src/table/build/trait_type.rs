use super::Build;
use crate::{symbol::TraitType, table::state::Config};

impl Build for TraitType {
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    ) {
    }
}
