use console::Style;
use pernix_project::source_code::SourceCode;

// create a global mutex to lock the printing
lazy_static::lazy_static! {
    pub static ref PRINTING_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());
}

pub fn print_error(error: &str) {
    let _lock = PRINTING_MUTEX.lock().unwrap();

    let error_bold = Style::new().bold().red();
    let white_bold = Style::new().bold().white();

    println!(
        "{}{} {}",
        error_bold.apply_to("error"),
        white_bold.apply_to(':'),
        error
    );
}

pub fn print_lexical_error(
    _source_code: &SourceCode,
    error: &pernix_lexer::error::Error,
) {
    match error {
        pernix_lexer::error::Error::InvalidCharacter {
            position: _,
            character,
        } => {
            print_error(format!("invalid character `{}`", character).as_str());
        }
        pernix_lexer::error::Error::UnterminatedMultilineComment {
            multiline_comment_position: _,
        } => {
            print_error("unterminated multiline comment");
        }
    }
}

pub fn print_syntactic_error(
    source_code: &SourceCode,
    error: &pernix_parser::error::Error,
) {
    match error {
        pernix_parser::error::Error::LexicalError(lex) => {
            print_lexical_error(source_code, lex);
        }
        pernix_parser::error::Error::KeywordExpected { expected_keyword, found_token } => {
            print_error(format!("expected keyword `{}`, found `{}`", expected_keyword.get_keyword_string(), found_token.lexeme()).as_str());
        }
        pernix_parser::error::Error::IdentifierExpected { found_token } => {
            print_error(format!("expected identifier, found `{}`", found_token.lexeme()).as_str());
        }
        pernix_parser::error::Error::PunctuatorExpected { expected_punctuator, found_token } => {
            print_error(format!("expected punctuator `{}`, found `{}`", expected_punctuator, found_token.lexeme()).as_str());
        }
        pernix_parser::error::Error::UnexpectedToken { context: _, found_token } => {
            print_error(format!("unexpected token `{}`", found_token.lexeme()).as_str());
        }
        pernix_parser::error::Error::UsingDirectiveMustAppearPriorToAllDeclarations { using_directive: _ } => {
            print_error("using directive must appear prior to all declarations");
        }
    }
}
