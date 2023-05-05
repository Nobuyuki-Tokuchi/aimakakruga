use std::fmt::Display;

use crate::common;

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub row: u64,
    pub column: u64,
    pub tokentype: TokenType,
}

impl Token {
    pub fn new(row: u64, column: u64, tokentype: TokenType) -> Self {
        Self {
            row,
            column,
            tokentype,
        }
    }

    pub fn value(row: u64, column: u64, token_str: impl Into<String>) -> Self {
        Self {
            row,
            column,
            tokentype: TokenType::Value(token_str.into()),
        }
    }

    pub fn variable(row: u64, column: u64, token_str: impl Into<String>) -> Self {
        Self {
            row,
            column,
            tokentype: TokenType::Variable(token_str.into()),
        }
    }

    pub fn reference(row: u64, column: u64, ref_index: usize) -> Self {
        Self {
            row,
            column,
            tokentype: TokenType::Reference(ref_index - 1),
        }
    }

    pub fn newline(row: u64, column: u64) -> Self {
        Self {
            row,
            column,
            tokentype: TokenType::NewLine
        }
    }

    pub fn unknown(row: u64, column: u64, token_str: impl Into<String>) -> Self {
        Self {
            row,
            column,
            tokentype: TokenType::Unknown(token_str.into()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum TokenType {
    Unknown(String),
    VerticalBar,
    Bind,
    Circumflex,
    Dollar,
    RightArrow,
    Value(String),
    Variable(String),
    Reference(usize),
    Semicolon,
    NewLine,
    When,
    Equal,
    NotEqual,
    Original,
    Part,
    NowForm,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Like,
    LeftCircle,
    RightCircle,
    AnyChar,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Unknown(token) => write!(f, "{}", token),
            Self::VerticalBar => write!(f, "|"),
            Self::Bind => write!(f, "="),
            Self::Circumflex => write!(f, "^"),
            Self::Dollar => write!(f, "$"),
            Self::RightArrow => write!(f, "->"),
            Self::Value(token) => write!(f, "\"{}\"", token),
            Self::Variable(token) => write!(f, "{}", token),
            Self::Reference(index) => write!(f, "@{}", (index + 1)),
            Self::Semicolon => write!(f, ";"),
            Self::NewLine => write!(f, "\\n"),
            Self::When => write!(f, "when"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "/="),
            Self::Original => write!(f, "{}", common::ORIGINAL_KEY),
            Self::Part => write!(f, "{}", common::PART_KEY),
            Self::NowForm => write!(f, "{}", common::NOW_WORD_KEY),
            Self::LogicalAnd => write!(f, "and"),
            Self::LogicalOr => write!(f, "or"),
            Self::LogicalNot => write!(f, "not"),
            Self::Like => write!(f, "like"),
            Self::LeftCircle => write!(f, "("),
            Self::RightCircle => write!(f, ")"),
            Self::AnyChar => write!(f, "."),
        }
    }
}
