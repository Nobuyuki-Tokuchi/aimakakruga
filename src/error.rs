
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken(String, String, Option<(u64, u64)>),
    EndOfToken(String, Option<(u64, u64)>),
    UnknownToken(String, Option<(u64, u64)>),
    NotFoundVariable(String, Option<(u64, u64)>),
    NotDefinedFunction(String),
    OutOfReferenceIndex(usize, Option<(u64, u64)>),
    ErrorMessage(String, Option<(u64, u64)>),
}

impl Error {
    pub(crate) fn invalid_with(parse_point: impl Into<String>, value: impl Display, row: u64, column: u64) -> Self {
        Self::InvalidToken(parse_point.into(), value.to_string(), Some((row, column)))
    }

    pub(crate) fn invalid(parse_point: impl Into<String>, value: impl Display) -> Self {
        Self::InvalidToken(parse_point.into(), value.to_string(), None)
    }

    // pub(crate) fn end_of_with(parse_point: impl Into<String>, row: u64, column: u64) -> Self {
    //     Self::EndOfToken(parse_point.into(), Some((row, column)))
    // }

    pub(crate) fn end_of(parse_point: impl Into<String>) -> Self {
        Self::EndOfToken(parse_point.into(), None)
    }

    pub(crate) fn not_defined_function(name: impl Into<String>) -> Self {
        Self::NotDefinedFunction(name.into())
    }

    pub(crate) fn unknown_with(value: impl Display, row: u64, column: u64) -> Self {
        Self::UnknownToken(value.to_string(), Some((row, column)))
    }

    // pub(crate) fn unknown(value: impl Display) -> Self {
    //     Self::UnknownToken(value.to_string(), None)
    // }

    pub(crate) fn message_with(message: impl Into<String>, row: u64, column: u64) -> Self {
        Self::ErrorMessage(message.into(), Some((row, column)))
    }

    pub(crate) fn message(message: impl Into<String>) -> Self {
        Self::ErrorMessage(message.into(), None)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::InvalidToken(parse_point, token, pos) => if let Some((row, column)) = pos {
                write!(f, "Invalid token in {} : {}, row: {}, column: {}", parse_point, token, row, column)
            } else {
                write!(f, "Invalid token in {} : {}", parse_point, token)
            },
            Self::EndOfToken(parse_point, pos ) => if let Some((row, column)) = pos {
                write!(f, "End of token in {}, row: {}, column: {}", parse_point, row, column)
            } else {
                write!(f, "End of token in {}", parse_point)
            },
            Self::UnknownToken(token, pos) => if let Some((row, column)) = pos {
                write!(f, "Unknown token : {}, row: {}, column: {}", token, row, column)
            } else {
                write!(f, "Unknown token : {}", token)
            },
            Self::NotFoundVariable(key, pos) => if let Some((row, column)) = pos {
                write!(f, "Not found variable: {}, row: {}, column: {}", key, row, column)
            } else {
                write!(f, "Not found variable: {}", key)
            },
            Self::NotDefinedFunction(name) => write!(f, "Not defined function: {}", name),
            Self::OutOfReferenceIndex(index, pos) => if let Some((row, column)) = pos {
                write!(f, "Out of reference index: {}, row: {}, column: {}", index, row, column)
            } else {
                write!(f, "Out of reference index: {}", index)
            },
            Self::ErrorMessage(message, pos) => if let Some((row, column)) = pos {
                write!(f, "{}, row: {}, column: {}", message, row, column)
            } else {
                write!(f, "{}", message)
            }
        }
    }
}