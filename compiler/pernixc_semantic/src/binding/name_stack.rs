use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct NameStack<T> {
    stack: Vec<HashMap<String, T>>,
}

impl<T> NameStack<T> {
    pub fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) { self.stack.push(HashMap::new()); }

    pub fn pop(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    pub fn insert(&mut self, name: String, value: T) {
        self.stack.last_mut().unwrap().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }

        None
    }
}
