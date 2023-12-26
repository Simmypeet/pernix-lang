use super::Build;
use crate::{symbol::ImplementationFunction, table::state::Config};

impl Build for ImplementationFunction {
    fn build(config: Config<Self>, data: &mut Self::Data, build_flag: Self::Flag) {}
}
