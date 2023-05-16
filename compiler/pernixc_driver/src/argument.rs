//! Contains the definition of [`Compilation`], the struct primarily used in [`super::compile`]
use std::path::PathBuf;

pub use clap::Parser;
use getset::Getters;

/// Is a struct containing the ocmpilation argument used by the compiler.
///
/// This struct is parsed from the command line arguments using `clap`.
#[derive(clap::Parser, Debug, Clone, Getters)]
#[command(author, version, about)]
pub struct Compilation {
    /// The path to the source file to compile.
    ///
    /// The source file extension must be `.pnx`
    #[arg(value_parser = input_parser)]
    #[get = "pub"]
    input: PathBuf,

    /// The path to the output file.
    ///
    /// If not specified, the output will be written in the same directory as the input file and
    /// has the same name as its target name.
    #[clap(short, long)]
    output: Option<PathBuf>,

    /// The target name of the output file.
    ///
    /// If not specified, the target name will be the same as the input file name.
    #[clap(short, long)]
    #[arg(value_parser = target_name_parser)]
    target_name: Option<String>,
}

impl Compilation {
    /// Gets the target name of this compilation.
    #[must_use]
    pub fn get_target_name(&self) -> String {
        self.target_name.clone().unwrap_or_else(|| {
            self.input
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
        })
    }
}

fn target_name_parser(input: &str) -> Result<String, String> {
    if input.is_empty() {
        return Err("The target name cannot be empty".to_string());
    }

    if !is_identifier_string(input) {
        return Err(format!(
            "The target name `{input}` is not a valid identifier"
        ));
    }

    Ok(input.to_owned())
}

fn input_parser(input: &str) -> Result<PathBuf, String> {
    let path = PathBuf::from(input);

    // must be a `.pnx` file
    if path.extension() != Some("pnx".as_ref()) {
        return Err("The input file extension must be `pnx`".to_string());
    }

    // must be exists
    if !path.exists() {
        return Err("The input file does not exists".to_string());
    }

    // must be a file
    if !path.is_file() {
        return Err("The input path must be a file".to_string());
    }

    // file name must be a valid identifier string
    if !is_identifier_string(
        path.file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
            .as_str(),
    ) {
        return Err("The input file name must be a valid identifier string".to_string());
    }

    Ok(path)
}

/// check if the given string is a valid identifier in Pernix
fn is_identifier_string(string: &str) -> bool {
    /// Checks if the given character is a valid first character of an identifier.
    fn is_first_identifier_character(character: char) -> bool {
        character == '_'
            || character == '@'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation()
                && !character.is_ascii_digit())
    }

    /// Checks if the given character is a valid character of an identifier.
    fn is_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation())
    }

    if string.is_empty() {
        return false;
    }

    string.chars().enumerate().all(|(index, character)| {
        if index == 0 {
            is_first_identifier_character(character)
        } else {
            is_identifier_character(character)
        }
    })
}
