
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken(String, String, Option<(u64, u64)>),
    EndOfToken(String, Option<(u64, u64)>),
    UnknownToken(String, Option<(u64, u64)>),
    NotFoundVariable(String, Option<(u64, u64)>),
    OutOfReferenceIndex(usize, Option<(u64, u64)>),
    ErrorMessage(String, Option<(u64, u64)>),
}

impl Error {
    pub(crate) fn invalid_with(parse_point: impl Into<String>, value: impl Display, row: u64, column: u64) -> Self {
        Error::InvalidToken(parse_point.into(), value.to_string(), Some((row, column)))
    }

    pub(crate) fn invalid(parse_point: impl Into<String>, value: impl Display) -> Self {
        Error::InvalidToken(parse_point.into(), value.to_string(), None)
    }

    // pub(crate) fn end_of_with(parse_point: impl Into<String>, row: u64, column: u64) -> Self {
    //     Error::EndOfToken(parse_point.into(), Some((row, column)))
    // }

    pub(crate) fn end_of(parse_point: impl Into<String>) -> Self {
        Error::EndOfToken(parse_point.into(), None)
    }

    pub(crate) fn unknown_with(value: impl Display, row: u64, column: u64) -> Self {
        Error::UnknownToken(value.to_string(), Some((row, column)))
    }

    // pub(crate) fn unknown(value: impl Display) -> Self {
    //     Error::UnknownToken(value.to_string(), None)
    // }

    pub(crate) fn message_with(message: impl Into<String>, row: u64, column: u64) -> Self {
        Error::ErrorMessage(message.into(), Some((row, column)))
    }

    pub(crate) fn message(message: impl Into<String>) -> Self {
        Error::ErrorMessage(message.into(), None)
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