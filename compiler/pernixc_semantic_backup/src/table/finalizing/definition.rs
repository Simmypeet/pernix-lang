use crate::table::{state, Table};

pub(in crate::table) trait Finalize<Syn>
where
    Self: Sized,
{
    fn finalize(
        table: &Table,
        syntax_tree: &Syn,
        config: &mut state::Config<Self>,
    );
}
