//! Contains the definition of [`Equivalent`], a structure that holds a set of
//! equivalent terms.

use std::collections::HashSet;

use getset::Getters;

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term};

/// A set of equivalent terms.
#[derive(Debug, Clone, derive_more::Deref, derive_more::DerefMut)]
pub struct Class<T>(HashSet<T>);

pub(in crate::semantic) trait Get: Sized {
    fn get_equivalent_classes(equivalent: &Equivalent) -> &Vec<Class<Self>>;

    fn get_equivalent_classes_mut(
        equivalent: &mut Equivalent,
    ) -> &mut Vec<Class<Self>>;
}

impl Get for Lifetime {
    fn get_equivalent_classes(equivalent: &Equivalent) -> &Vec<Class<Self>> {
        &equivalent.lifetime_classes
    }

    fn get_equivalent_classes_mut(
        equivalent: &mut Equivalent,
    ) -> &mut Vec<Class<Self>> {
        &mut equivalent.lifetime_classes
    }
}

impl Get for Type {
    fn get_equivalent_classes(equivalent: &Equivalent) -> &Vec<Class<Self>> {
        &equivalent.type_classes
    }

    fn get_equivalent_classes_mut(
        equivalent: &mut Equivalent,
    ) -> &mut Vec<Class<Self>> {
        &mut equivalent.type_classes
    }
}

impl Get for Constant {
    fn get_equivalent_classes(equivalent: &Equivalent) -> &Vec<Class<Self>> {
        &equivalent.constant_classes
    }

    fn get_equivalent_classes_mut(
        equivalent: &mut Equivalent,
    ) -> &mut Vec<Class<Self>> {
        &mut equivalent.constant_classes
    }
}

/// Holds a set of equivalent classes for lifetimes, types, and constants.
#[derive(Debug, Clone, Default, Getters)]
pub struct Equivalent {
    /// The equivalent classes for lifetime terms
    #[get = "pub"]
    lifetime_classes: Vec<Class<Lifetime>>,

    /// The equivalent classes for type terms
    #[get = "pub"]
    type_classes: Vec<Class<Type>>,

    /// The equivalent classes for constant terms
    #[get = "pub"]
    constant_classes: Vec<Class<Constant>>,
}

impl Equivalent {
    /// Creates a new empty equivalent set.
    #[must_use]
    pub fn new() -> Self {
        Self {
            lifetime_classes: Vec::new(),
            type_classes: Vec::new(),
            constant_classes: Vec::new(),
        }
    }

    /// Remove the class that contains the given term.
    ///
    /// This method returns the [`Class`] that contains the given term.
    pub fn remove_matching_class<T: Term>(
        &mut self,
        term: &T,
    ) -> Option<Class<T>> {
        let classes = T::get_equivalent_classes_mut(self);

        for (idx, class) in classes.iter_mut().enumerate() {
            if class.contains(term) {
                // we can return right away because there should be only one
                // class that contains the term
                return Some(classes.remove(idx));
            }
        }

        None
    }

    /// Removes the class of the given kind at the given index.
    pub fn remove_class<T: Term>(
        &mut self,
        class_index: usize,
    ) -> Option<Class<T>> {
        let classes = T::get_equivalent_classes_mut(self);

        if class_index < classes.len() {
            Some(classes.remove(class_index))
        } else {
            None
        }
    }

    /// Inserts two equivalent terms into the equivalent set.
    pub fn insert<T: Term>(&mut self, first: T, second: T) {
        let classes = T::get_equivalent_classes_mut(self);

        // find the index where it contains the first and second
        let first_idx = classes.iter().enumerate().find_map(|(idx, class)| {
            if class.contains(&first) {
                Some(idx)
            } else {
                None
            }
        });
        let second_idx = classes.iter().enumerate().find_map(|(idx, class)| {
            if class.contains(&second) {
                Some(idx)
            } else {
                None
            }
        });

        match (first_idx, second_idx) {
            // both first and second are in different classes
            (Some(first_idx), Some(second_idx)) => {
                // if both are in the same class, do nothing
                if first_idx == second_idx {
                    return;
                }

                let index_to_remove = std::cmp::max(first_idx, second_idx);
                let index_to_keep = std::cmp::min(first_idx, second_idx);

                let class_to_remove = classes.remove(index_to_remove).0;
                classes[index_to_keep].0.extend(class_to_remove);
            }

            // insert in one of the classes
            (Some(idx), _) | (_, Some(idx)) => {
                classes[idx].insert(second);
                classes[idx].insert(first);
            }

            // insert in a new class
            (None, None) => {
                let mut new_class = HashSet::new();
                new_class.insert(first);
                new_class.insert(second);
                classes.push(Class(new_class));
            }
        }
    }
}

#[cfg(test)]
mod tests;
