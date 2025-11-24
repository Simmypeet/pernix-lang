use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::TrackedEngine;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;

use crate::formatter::accessbility::get_accessiblity_str;

#[extend]
pub async fn format_module_signature(
    self: &TrackedEngine,
    symbol: Global<pernixc_symbol::ID>,
) -> String {
    let mut markdown = String::new();
    let qualified_name = self.get_qualified_name(symbol).await;
    let accessiblity_str = self.get_accessiblity_str(symbol).await;

    markdown.push_str("```pernixc\n");
    write!(markdown, "{accessiblity_str} module {qualified_name}").unwrap();

    markdown.push_str("\n```\n");

    markdown
}
