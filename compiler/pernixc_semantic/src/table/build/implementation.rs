use super::Build;
use crate::{symbol::Implementation, table::state::Config};

impl Build for Implementation {
    fn build(config: Config<Self>, data: &mut Self::Data, build_flag: Self::Flag) {}
}
