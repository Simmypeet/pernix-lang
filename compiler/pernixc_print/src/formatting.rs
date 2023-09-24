//! Contains various kinds of definition for formatting ASCII color/style codes.

use std::fmt::Display;

/// Represents a style that can be applied to the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Style {
    Bold,
    Underline,
}

impl Style {
    /// Applies the style to the given displayable object.
    #[allow(missing_docs)]
    pub fn with<T>(self, display: T) -> WithStyle<T> {
        WithStyle {
            style: self,
            display,
        }
    }
}

/// Is a struct implementing [`Display`] that represents a displayable object with a style applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithStyle<T> {
    /// The style applied to the displayable object.
    pub style: Style,

    /// The displayable object.
    pub display: T,
}

impl<T: Display> Display for WithStyle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let style = match self.style {
            Style::Bold => "\x1B[1m",
            Style::Underline => "\x1B[4m",
        };

        write!(f, "{}{}\x1B[0m", style, self.display,)
    }
}

/// Represents a color that can be applied to the text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

impl Color {
    /// Applies the color to the given displayable object.
    pub fn with<T>(self, display: T) -> WithColor<T> {
        WithColor {
            color: self,
            display,
        }
    }
}

/// Is a struct implementing [`Display`] that represents a displayable object with a color applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithColor<T> {
    /// The color applied to the displayable object.
    pub color: Color,

    /// The displayable object.
    pub display: T,
}

impl<T: Display> Display for WithColor<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = match self.color {
            Color::Black => "\x1B[30m",
            Color::Red => "\x1B[31m",
            Color::Green => "\x1B[32m",
            Color::Yellow => "\x1B[33m",
            Color::Blue => "\x1B[34m",
            Color::Magenta => "\x1B[35m",
            Color::Cyan => "\x1B[36m",
            Color::White => "\x1B[37m",
        };

        write!(f, "{}{}\x1B[0m", color, self.display,)
    }
}
