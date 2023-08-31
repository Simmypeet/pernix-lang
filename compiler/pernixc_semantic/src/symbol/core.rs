use std::collections::HashMap;

use super::{Accessibility, Table};

impl Table {
    pub(super) fn create_core_module(&mut self) {
        // core module
        let core_module = self.modules.insert(super::Module {
            accessibility: Accessibility::Public,
            name: "@core".to_string(),
            parent_module_id: None,
            children_ids_by_name: HashMap::new(),
        });
    }
}
