
use std::cmp::Ordering;
use std::rc::Rc;
use crate::lexer::TokenType;
use crate::error::Error;

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Define(Rc<DefineStruct>),
    Shift(ShiftStruct)
}

#[derive(Debug, Clone)]
pub(crate) struct DefineStruct {
    pub name: String,
    pub expr: Vec<Pattern>,
}

#[derive(Debug, Clone)]
pub(crate) struct ShiftStruct {
    pub lhs: Vec<Pattern>,
    pub when: Option<Vec<LogicalValue>>,
    pub rhs: Pattern,
}

#[derive(Debug, Clone)]
pub(crate) struct Pattern {
    pub values: Vec<Value>,
    pub mode: Mode,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Mode {
    None,
    Forward,
    Backward,
    Exact,
}

#[derive(Debug, Clone)]
pub(crate) enum LogicalValue {
    And,
    Or,
    Equal,
    NotEqual,
    Literal(String),
    Reference(usize),
    Original,
    NowForm,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Literal(String),
    Variable(String),
    Reference(usize)
}

impl DefineStruct {
    pub fn new(name: &str, expr: Vec<Pattern>) -> Self {
        Self {
            name: name.to_owned(),
            expr,
        }
    }
}

impl ShiftStruct {
    pub fn new(lhs: Vec<Pattern>, when: Option<Vec<LogicalValue>>, rhs: Pattern) -> Self {
        Self {
            lhs,
            when,
            rhs,
        }
    }
}

pub(crate) fn parse(tokens: &Vec<TokenType>) -> Result<Vec<Statement>, Error> {
    let mut statements = vec![];

    let mut index = 0;
    let length = tokens.len();
    while index < length {
        if let Some(value) = tokens.get(index) {
            match value {
                TokenType::Variable(value) => {
                    let (statement, next_index) = if let Some(next) = tokens.get(index + 1) {
                        match next {
                            &TokenType::Bind => parse_define(value, tokens, index + 1),
                            _ => parse_shift(&tokens, index),
                        }?
                    } else {
                        return Err(Error::EndOfToken(String::from("statement"), index))
                    };

                    statements.push(statement);
                    index = next_index;
                },
                TokenType::Value(_) | TokenType::Circumflex => {
                    let (statement, next_index) = parse_shift(&tokens, index)?;

                    statements.push(statement);
                    index = next_index;
                },
                TokenType::Unknown(value) => {
                    return Err(Error::UnknownToken(value.clone(), index));
                },
                TokenType::NewLine | TokenType::Semicolon => {
                    index = index + 1;
                }
                _ => {
                    return Err(Error::InvalidToken(String::from("statement"), value.to_string(), index));
                }
            }
        } else {
            return Err(Error::ErrorMessage(format!("End of token in statement. length: {}, index: {}", length, index), None));
        }
    }
    
    Ok(statements)
}

fn parse_define(name: &str, tokens: &[TokenType], index: usize) -> Result<(Statement, usize), Error> {
    let next_index = match tokens.get(index) {
        Some(TokenType::Bind) => index + 1,
        Some(other) => return Err(Error::InvalidToken(String::from("define variable"), other.to_string(), index)),
        None => return Err(Error::EndOfToken(String::from("define variable"), index))
    };

    let (expr, next_index) = parse_expression(&tokens, next_index)?;

    if let Some(token) = tokens.get(next_index) {
        if &TokenType::Semicolon == token || &TokenType::NewLine == token {
            let statement = Statement::Define(Rc::new(DefineStruct::new(name, expr)));
            Ok((statement, next_index + 1))
        } else {
            Err(Error::InvalidToken(String::from("expression of define variable"), token.to_string(), index))
        }
    } else {
        Err(Error::EndOfToken(String::from("expression of define variable"), index))
    }
}

fn parse_expression(tokens: &[TokenType], index: usize) -> Result<(Vec<Pattern>, usize), Error> {
    let mut patterns: Vec<Pattern> = Vec::new();
    let (pattern, mut next_index) = parse_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if let Some(value) = tokens.get(next_index) {
            if &TokenType::ValueOr == value {
                next_index = next_index + 1;
            } else {
                break;
            }
        } else {
            return Err(Error::EndOfToken(String::from("patterns of expression"), next_index))
        };

        if let Ok((pattern, index)) = parse_pattern(&tokens, next_index) {
            patterns.push(pattern);
            next_index = index;
        } else {
            return Err(Error::ErrorMessage(String::from("Next pattern is nothing in patterns"), Some(next_index)))
        }
    }

    Ok((patterns, next_index))
}

fn parse_pattern(tokens: &[TokenType], index: usize) -> Result<(Pattern, usize), Error> {
    let mut values = Vec::new();

    let (is_prefix, next_index) = if let Some(token) = tokens.get(index) {
        match token {
            TokenType::Circumflex => (true, index + 1),
            _ => (false, index),
        }
    } else {
        return Err(Error::EndOfToken(String::from("pattern (prefix)"), index))
    };

    let (value, mut next_index) = parse_value(tokens, next_index)?;
    values.push(value);

    loop {
        if let Ok((value, index)) = parse_value(&tokens, next_index) {
            values.push(value);
            next_index = index;
        } else {
            break;
        }
    }

    let (is_postfix, next_index) = if let Some(token) = tokens.get(next_index) {
        match token {
            TokenType::Circumflex => (true, next_index + 1),
            _ => (false, next_index),
        }
    } else {
        (false, next_index)
    };

    let mode = match (is_prefix, is_postfix) {
        (false, false) => Mode::None,
        (false, true) => Mode::Backward,
        (true, false) => Mode::Forward,
        (true, true) => Mode::Exact,
    };

    Ok((Pattern { values, mode }, next_index))
}

fn parse_value(tokens: &[TokenType], index: usize) -> Result<(Value, usize), Error> {
    if let Some(token) = tokens.get(index) {
        match token {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            TokenType::Reference(ref_index) => Ok((Value::Reference(*ref_index), index + 1)),
            _ => Err(Error::InvalidToken(String::from("value"), token.to_string(), index)),
        }
    } else {
        Err(Error::EndOfToken(String::from("value"), index))
    }
}

fn parse_shift(tokens: &[TokenType], index: usize) -> Result<(Statement, usize), Error> {
    let (lhs, next_index) = parse_expression(&tokens, index)?;
    let (when_, next_index) = if let Some(TokenType::When) = tokens.get(next_index) {
        parse_when(&tokens, next_index + 1).map(|(w, i)| (Some(w), i))?
    } else {
        (None, next_index)
    };
    let next_index = match tokens.get(next_index) {
        Some(TokenType::RightArrow) => next_index + 1,
        Some(other) => return Err(Error::InvalidToken(String::from("shift statement"), other.to_string(), next_index)),
        None => return Err(Error::EndOfToken(String::from("shift statement"), next_index))
    };
    let (rhs, next_index) = parse_pattern(&tokens, next_index)?;

    if let Some(Value::Variable(other)) = rhs.values.iter().find(|x| if let Value::Variable(_) = x { true } else { false }) {
        return Err(Error::ErrorMessage(format!("Cannot use variable in right of shift statement: `{}`", other), Some(next_index)));
    }

    if let Some(token) = tokens.get(next_index) {
        if &TokenType::Semicolon == token || &TokenType::NewLine == token {
            let statement = Statement::Shift(ShiftStruct::new(lhs, when_, rhs));
            Ok((statement, next_index + 1))
        } else {
            Err(Error::InvalidToken(String::from("expression of shift statement"), token.to_string(), next_index))
        }
    } else {
        Err(Error::EndOfToken(String::from("expression of shift statement"), next_index))
    }
}

fn parse_when(tokens: &[TokenType], index: usize) -> Result<(Vec<LogicalValue>, usize), Error> {
    let mut stack_operator: Vec<LogicalValue> = Vec::new();
    let mut stack_value: Vec<LogicalValue> = Vec::new();
    let mut values: Vec<LogicalValue> = Vec::new();
    let mut next_index = index;

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token {
                TokenType::Value(value) => stack_value.push(LogicalValue::Literal(value.to_owned())),
                TokenType::Variable(value) => return Err(Error::ErrorMessage(format!("Cannot use variable in when expression: `{}`", value), Some(index))),
                TokenType::Reference(index) => stack_value.push(LogicalValue::Reference(index.to_owned())),
                TokenType::Original => stack_value.push(LogicalValue::Original),
                TokenType::NowForm => stack_value.push(LogicalValue::NowForm),
                TokenType::Equal | TokenType::NotEqual | TokenType::LogicalAnd | TokenType::LogicalOr => {
                    let operator = match token {
                        TokenType::Equal => LogicalValue::Equal,
                        TokenType::NotEqual => LogicalValue::NotEqual,
                        TokenType::LogicalAnd => LogicalValue::And,
                        TokenType::LogicalOr => LogicalValue::Or,
                        _ => return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index))
                    };

                    if let Some(prev) = stack_operator.pop() {
                        let ord = check_precedence(&prev, &operator);
                        if ord == Ordering::Greater {
                            values.append(&mut stack_value);
                            values.push(prev);
                        } else {
                            stack_operator.push(prev);
                        }
                    }
                    stack_operator.push(operator);
                }
                _ => break,
            }

            next_index = next_index + 1;
        } else {
            break;
        }
    }

    if !stack_operator.is_empty() {
        if !stack_value.is_empty() {
            values.append(&mut stack_value);
            stack_value.clear();
        }
        if values.is_empty() {
            return Err(Error::EndOfToken(String::from("when expression"), next_index));
        }
        stack_operator.reverse();
        values.append(&mut stack_operator);
    } else if values.is_empty() {
        return Err(Error::EndOfToken(String::from("when expression"), next_index));
    }

    Ok((values, next_index))
}


fn check_precedence(prev: &LogicalValue, now: &LogicalValue) -> Ordering {
    match prev {
        LogicalValue::And => match now {
            LogicalValue::And => Ordering::Equal,
            LogicalValue::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
        LogicalValue::Or => match now {
            LogicalValue::Or => Ordering::Equal,
            _ => Ordering::Less,
        },
        LogicalValue::Literal(_) | LogicalValue::Reference(_) | LogicalValue::Original | LogicalValue::NowForm => match now {
            LogicalValue::Literal(_) | LogicalValue::Reference(_) | LogicalValue::Original | LogicalValue::NowForm => Ordering::Equal,
            _ => Ordering::Greater,
        },
        LogicalValue::Equal => match now {
            LogicalValue::Equal | LogicalValue::NotEqual => Ordering::Equal,
            LogicalValue::And | LogicalValue::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
        LogicalValue::NotEqual => match now {
            LogicalValue::Equal | LogicalValue::NotEqual => Ordering::Equal,
            LogicalValue::And | LogicalValue::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
    }
}

#[cfg(test)]
mod parser_test {
    use crate::lexer::*;
    use crate::parser::*;

    fn execute(s: &str) -> Result<Vec<Statement>, Error> {
        let tokens = lexer(&s);
        parse(&tokens)
    }

    #[test]
    fn default() {
        let result = execute(r#"
        # 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        C = "p" | "t" | "k" | "f" | "s" | "h" | "l" | "y"
        
        ^ "s" "k" V -> "s" @3
        "e" "a" | "i" "a" -> "y" "a"
        "i" V when @2 == "i" or @2 == "e" -> "i" "i"
        "l" "l" V ^ -> "l" @3
        C "l" V ^ when @1 /= "l" -> @1 @3
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn use_semicolon() {
        let result = execute(r#"
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        C = "p" | "t" | "k" | "f" | "s" | "h" | "l" | "y"
        T = "p" | "t" | "k"

        # セミコロンを使用すると一行に複数のパターンを記述できる
        ^ "s" "k" V -> "s" @3
        "e" "a" | "i" "a" -> "y" "a"; "i" V when @2 == "i" or @2 == "e" -> "i" "i"
        V T T V when @2 == @3 -> @1 @2 @4;
        "l" "l" V ^ -> "l" @3; C "l" V | C "l" "y" when @1 /= "l" -> @1 @3
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }
}
