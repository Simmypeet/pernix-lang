use super::{Build, ResolutionConfigChain};
use crate::{
    symbol::Trait,
    table::state::{self, Config},
};

impl Build for Trait {
    fn build(mut config: Config<Self>, data: &mut Self::Data, build_flag: Self::Flag) {
        let table = config.table();
        let handler = config.handler();
        let id = config.current_id();
        let syntax_tree = config.syntax_tree();

        // change the build flag
        data.set_curret_flag(build_flag);

        match build_flag {
            state::r#trait::Flag::Drafted => unreachable!("should not be called"),
            state::r#trait::Flag::GenericParameter => table.create_generic_parameters(
                id,
                syntax_tree.generic_parameters().as_ref(),
                &mut ResolutionConfigChain::new(&mut config, data),
                handler,
            ),
            state::r#trait::Flag::WhereClause => table.create_where_clause_predicates(
                id,
                syntax_tree.where_clause().as_ref(),
                &mut ResolutionConfigChain::new(&mut config, data),
                handler,
            ),
            state::r#trait::Flag::Check => todo!(),
        }
    }
}
