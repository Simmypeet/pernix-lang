use super::Table;
use crate::WhereClause;

impl Table {
    pub(super) fn collect_required_where_trait(&self) -> WhereClause {}
}
