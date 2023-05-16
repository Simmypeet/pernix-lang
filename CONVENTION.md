# Pernix Compiler Coding Convention

This coding convention serves as a guideline and reference for the Pernix Compiler. It's generally
recommended to follow this convention when contributing to the compiler.

---

## Making Invalid States Unrepresentable

### Motivation

When writing a function that works with some inputs from the outside, it's necessary to check for
the validity of the inputs. However, this is a tedious task and writing checks for every possible
cases is almost impossible. Therefore, it would be better to leverage the type system to prevent any
invalid states from being represented in the first place.

### When and How

All the structs that are exposed to the public function APIs (i.e. the struct types are being used
as function parameters) will be checked for validity and therefore should be designed to make
invalid states unrepresentable. This is done by making the struct having all private fields,
providing getters, providing a constructor function that makes sure the struct will be constructed
in a valid state.

Consider the following example:

``` rust
pub struct AverageNumber {
    numbers: Vec<f64>,
    average: f64
}

impl AverageNumber {
    pub fn new(numbers: Vec<f64>) -> Result<Self, AverageNumberCreateError> {
        if numbers.is_empty() {
            return Err(AverageNumberCreateError::EmptyNumbers);
        }

        let average = numbers.iter().sum::<f64>() / numbers.len() as f64;
        Self { numbers, average }
    }

    pub fn numbers(&self) -> &[f64] {
        &self.numbers
    }

    pub fn average(&self) -> f64 {
        self.average
    }
}
```

The `AverageNumber` struct contains a list of numbers and the average of the numbers. The struct is
designed to make invalid states unrepresentable. By creating the struct using the `new` function,
the struct will always have a correct average value and the list of numbers will never be empty.

### Tips

In some cases, we might want to **move** the data out of the struct since copying the data from the
getter functions might be expensive. Therefore, the struct should provide a `deconstruct` function
or any similar name that moves the fields out of the struct into a tuple so that the caller can
individally move the fields out of the tuple.

``` rust
pub struct AverageNumber {
    numbers: Vec<f64>,
    average: f64
}

impl AverageNumber {
    // elided

    /// Deconstructs the struct into a tuple of the numbers and the average.
    pub fn deconstruct(self) -> (Vec<f64>, f64) {
        (self.numbers, self.average)
    }
}
```

In this case, the caller can get the `numbers` vec by calling 
`let (numbers, _) = average_number.deconstruct()`. If there's no `deconstruct` function, the caller 
would have to call `average_number.numbers().to_vec()` which is more expensive.

---

## Avoid Borrowing in APIs

### Problem with Borrowing

Borrowing is a feature in Rust which allows us to avoid copying the data when passing the data
around. However, if overused, it can lead to a lot of lifetime annotations and make the code less
flexible introducing unnecessary restrictions. Therefore, it's worth to know when to borrow and
when to **own** the data.

### When to not Borrow

As stated earlier, over borrowing makes the code less flexible and is not ideal. For the most part,
the output of a pipeline should be an **owned** data. For exaple, the Syntax Tree should own the
tokens that it contains not borrow them.

``` rust
// Preferrable
pub struct Struct {
    pub struct_keyword: KeywordToken,
    pub name: IdentifierToken,
    // ...
}

// Not preferrable
pub struct Struct<'a> {
    pub struct_keyword: &'a KeywordToken,
    pub name: &'a IdentifierToken,
    // ...
}
```

``` rust
// Preferrable
pub fn syntactic_analysis(tokens: Vec<Token>) -> Result<SyntaxTree, SyntacticAnalysisError> {
    // ...
}

// Not preferrable
pub fn syntactic_analysis<'a>(
    tokens: &'a Vec<Token>) -> Result<SyntaxTree<'a>, SyntacticAnalysisError> {
    // ...
}
```

However, it's not to say the borrowing is prohibited at all. For some intermediate steps/structures
borrowing is preferrable.
