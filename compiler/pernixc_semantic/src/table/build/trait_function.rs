use super::Build;
use crate::{symbol::TraitFunction, table::state::Config};

impl Build for TraitFunction {
    fn build(config: Config<Self>, data: &mut Self::Data, build_flag: Self::Flag) {}
}
