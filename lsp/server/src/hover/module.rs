use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;

use crate::{
    formatter::accessbility::get_accessiblity_str,
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_module_signature(
    self: &TrackedEngine,
    symbol: Global<pernixc_symbol::ID>,
) -> String {
    let mut markdown = format!("```{PERNIX_FENCE}\n");
    let qualified_name = self.get_qualified_name(symbol).await;
    let accessiblity_str = self.get_accessiblity_str(symbol).await;

    write!(markdown, "{accessiblity_str} module {qualified_name}").unwrap();

    markdown.push_str("\n```\n");

    markdown
}
