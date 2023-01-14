
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken(String, String, usize),
    EndOfToken(String, usize),
    UnknownToken(String, usize),
    NotFoundVariable(String),
    OutOfReferenceIndex(usize),
    ErrorMessage(String, Option<usize>),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::InvalidToken(parse_point, token, index) => write!(f, "Invalid token in {} : {}, index: {}", parse_point, token, index),
            Self::EndOfToken(parse_point, index ) => write!(f, "End of token in {} : index: {}", parse_point, index),
            Self::UnknownToken(token, index) => write!(f, "Unknown token : {}, index: {}", token, index),
            Self::NotFoundVariable(key) => write!(f, "Not found variable: {}", key),
            Self::OutOfReferenceIndex(index) => write!(f, "Out of reference index: {}", index),
            Self::ErrorMessage(message, index) => {
                match index {
                    Some(index) => write!(f, "{}: index: {}", message, index),
                    None => write!(f, "{}", message),
                }
            }
        }
    }
}