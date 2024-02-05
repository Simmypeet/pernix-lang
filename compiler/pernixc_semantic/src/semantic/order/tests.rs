/// Property checks for the order of terms.
trait Property<T> {
    /// Generates a pair of terms that will be used to compare the specificity.
    fn generate(&self) -> (T, T);

    /// Returns a positive integer if the term has *one* more specialized term
    /// than the other or returns a negative integer if the term
    /// has *one* less specialized term than the other.
    ///
    /// Otherwise, returns 0 if both lhs and rhs are equally specialized.
    ///
    /// Returns `None` if the two terms are incompatible.
    fn get_specialization_point(&self) -> Option<isize>;
}

#[test]
const fn order_testing() {}
