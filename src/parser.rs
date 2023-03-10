
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
    pub patterns: Vec<Vec<Value>>,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Literal(String),
    Variable(String),
    Reference(usize),
    Part,
    AnyChar,
}

#[derive(Debug, Clone)]
pub(crate) struct ShiftStruct {
    pub lhs: Vec<ShiftPattern>,
    pub when: Option<Vec<LogicalNode>>,
    pub rhs: ShiftPattern,
}

#[derive(Debug, Clone)]
pub(crate) struct ShiftPattern {
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
pub(crate) enum LogicalNode {
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    Like,
    Operand(ShiftPattern),
    Original,
    Part,
    NowForm,
    LeftCircle,
}

impl DefineStruct {
    pub fn new(name: &str, expr: Vec<Vec<Value>>) -> Self {
        Self {
            name: name.to_owned(),
            patterns: expr,
        }
    }
}

impl ShiftStruct {
    pub fn new(lhs: Vec<ShiftPattern>, when: Option<Vec<LogicalNode>>, rhs: ShiftPattern) -> Self {
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

    let (patterns, next_index) = parse_define_patterns(&tokens, next_index)?;

    if let Some(token) = tokens.get(next_index) {
        if &TokenType::Semicolon == token || &TokenType::NewLine == token {
            let statement = Statement::Define(Rc::new(DefineStruct::new(name, patterns)));
            Ok((statement, next_index + 1))
        } else {
            Err(Error::InvalidToken(String::from("expression of define variable"), token.to_string(), index))
        }
    } else {
        Err(Error::EndOfToken(String::from("expression of define variable"), index))
    }
}

fn parse_define_patterns(tokens: &[TokenType], index: usize) -> Result<(Vec<Vec<Value>>, usize), Error> {
    let mut patterns: Vec<Vec<Value>> = Vec::new();
    let (pattern, mut next_index) = parse_define_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if let Some(value) = tokens.get(next_index) {
            match value {
                &TokenType::VerticalBar => {
                    next_index = next_index + 1;
                },
                _ => break,
            }
        } else {
            break;
        };

        if let Ok((pattern, index)) = parse_define_pattern(&tokens, next_index) {
            patterns.push(pattern);
            next_index = index;
        } else {
            return Err(Error::ErrorMessage(String::from("Next pattern is nothing in patterns"), Some(next_index)))
        }
    }

    Ok((patterns, next_index))
}

fn parse_define_pattern(tokens: &[TokenType], index: usize) -> Result<(Vec<Value>, usize), Error> {
    let mut values = Vec::new();

    let (value, mut next_index) = parse_value(tokens, index)?;
    values.push(value);

    loop {
        if let Ok((value, index)) = parse_value(&tokens, next_index) {
            values.push(value);
            next_index = index;
        } else {
            break;
        }
    }
    
    Ok((values, next_index))
}

fn parse_value(tokens: &[TokenType], index: usize) -> Result<(Value, usize), Error> {
    if let Some(token) = tokens.get(index) {
        match token {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            _ => Err(Error::InvalidToken(String::from("value"), token.to_string(), index)),
        }
    } else {
        Err(Error::EndOfToken(String::from("value"), index))
    }
}

fn parse_shift(tokens: &[TokenType], index: usize) -> Result<(Statement, usize), Error> {
    let (lhs, mut next_index) = parse_match_patterns(&tokens, index)?;
    
    while let Some(TokenType::NewLine) = tokens.get(next_index) {
        next_index = next_index + 1;
    }

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

    let (rhs, next_index) = parse_convert_pattern(&tokens, next_index)?;

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

fn parse_match_patterns(tokens: &[TokenType], index: usize) -> Result<(Vec<ShiftPattern>, usize), Error> {
    let mut patterns: Vec<ShiftPattern> = Vec::new();
    let (pattern, mut next_index) = parse_match_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if let Some(value) = tokens.get(next_index) {
            match value {
                &TokenType::VerticalBar => {
                    next_index = next_index + 1;
                },
                _ => break,
            }
        } else {
            break;
        };

        if let Ok((pattern, index)) = parse_match_pattern(&tokens, next_index) {
            patterns.push(pattern);
            next_index = index;
        } else {
            return Err(Error::ErrorMessage(String::from("Next pattern is nothing in patterns"), Some(next_index)))
        }
    }

    Ok((patterns, next_index))
}

fn parse_match_pattern(tokens: &[TokenType], index: usize) -> Result<(ShiftPattern, usize), Error> {
    let mut values = Vec::new();

    let (is_prefix, next_index) = if let Some(token) = tokens.get(index) {
        match token {
            TokenType::Circumflex => (true, index + 1),
            _ => (false, index),
        }
    } else {
        return Err(Error::EndOfToken(String::from("pattern (prefix)"), index))
    };

    let (value, mut next_index) = parse_shift_value(tokens, next_index)?;
    values.push(value);

    loop {
        if let Ok((value, index)) = parse_shift_value(&tokens, next_index) {
            values.push(value);
            next_index = index;
        } else {
            break;
        }
    }

    let (is_suffix, next_index) = if let Some(token) = tokens.get(next_index) {
        match token {
            TokenType::Dollar => (true, next_index + 1),
            _ => (false, next_index),
        }
    } else {
        (false, next_index)
    };

    let mode = get_mode(is_prefix, is_suffix);

    Ok((ShiftPattern { values, mode } , next_index))
}

fn get_mode(is_prefix: bool, is_suffix: bool) -> Mode {
    match (is_prefix, is_suffix) {
        (false, false) => Mode::None,
        (false, true) => Mode::Backward,
        (true, false) => Mode::Forward,
        (true, true) => Mode::Exact,
    }
}

fn parse_shift_value(tokens: &[TokenType], index: usize) -> Result<(Value, usize), Error> {
    if let Some(token) = tokens.get(index) {
        match token {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            TokenType::Reference(value) => Ok((Value::Reference(*value), index + 1)),
            TokenType::AnyChar => Ok((Value::AnyChar, index + 1)),
            _ => Err(Error::InvalidToken(String::from("value"), token.to_string(), index)),
        }
    } else {
        Err(Error::EndOfToken(String::from("value"), index))
    }
}

fn parse_convert_pattern(tokens: &[TokenType], index: usize) -> Result<(ShiftPattern, usize), Error> {
    let mut values = Vec::new();

    let (value, mut next_index) = parse_shift_value(tokens, index)?;
    values.push(value);

    loop {
        if let Ok((value, index)) = parse_shift_value(&tokens, next_index) {
            values.push(value);
            next_index = index;
        } else {
            break;
        }
    }

    Ok((ShiftPattern { values, mode: Mode::None } , next_index))
}

fn parse_when(tokens: &[TokenType], index: usize) -> Result<(Vec<LogicalNode>, usize), Error> {
    let mut stack_operator: Vec<LogicalNode> = Vec::new();
    let mut stack_value: Vec<&TokenType> = Vec::new();
    let mut values: Vec<LogicalNode> = Vec::new();
    let mut next_index = index;

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token {
                TokenType::Circumflex => {
                    if stack_value.is_empty() {
                        stack_value.push(token);
                    } else {
                        return Err(Error::InvalidToken(String::from("when expression"), TokenType::Dollar.to_string(), next_index));
                    }
                },
                TokenType::Dollar => {
                    stack_value.push(token);
                },
                TokenType::Value(_) | TokenType::Variable(_) | TokenType::Reference(_) => {
                    if let Some(TokenType::Dollar) = stack_value.last() {
                        return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                    } else {
                        stack_value.push(token);
                    }
                },
                TokenType::Original => {
                    if !stack_value.is_empty(){
                        return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                    }
                    values.push(LogicalNode::Original);
                },
                TokenType::NowForm => {
                    if !stack_value.is_empty(){
                        return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                    }
                    values.push(LogicalNode::NowForm);
                },
                TokenType::Part => {
                    if !stack_value.is_empty(){
                        return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                    }
                    values.push(LogicalNode::Part);
                }
                TokenType::Equal | TokenType::NotEqual | TokenType::LogicalAnd | TokenType::LogicalOr | TokenType::LogicalNot | TokenType::Like => {
                    let operator = match token {
                        TokenType::Equal => LogicalNode::Equal,
                        TokenType::NotEqual => LogicalNode::NotEqual,
                        TokenType::LogicalAnd => LogicalNode::And,
                        TokenType::LogicalOr => LogicalNode::Or,
                        TokenType::LogicalNot => LogicalNode::Not,
                        TokenType::Like => LogicalNode::Like,
                        _ => return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index))
                    };

                    if !stack_value.is_empty() {
                        let is_prefix = if let TokenType::Circumflex = stack_value[0] {
                            stack_value.remove(0);
                            true
                        } else {
                            false
                        };
                        let is_suffix = if let TokenType::Dollar = stack_value[stack_value.len() - 1] {
                            stack_value.pop();
                            true
                        } else {
                            false
                        };

                        let mut logic_values = Vec::new();
                        for value in stack_value.into_iter() {
                            match value {
                                TokenType::Value(s) => {
                                    logic_values.push(Value::Literal(s.to_owned()));
                                },
                                TokenType::Variable(s) => {
                                    logic_values.push(Value::Variable(s.to_owned()));
                                },
                                TokenType::Reference(i) => {
                                    logic_values.push(Value::Reference(*i));
                                },
                                TokenType::Part => {
                                    logic_values.push(Value::Part);
                                },
                                _ => (),
                            }
                        }

                        values.push(LogicalNode::Operand(ShiftPattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
                        stack_value = Vec::new();
                    }

                    if let Some(prev) = stack_operator.pop() {
                        if let LogicalNode::LeftCircle = prev {
                            stack_operator.push(prev);
                        } else {
                            let ord = check_precedence(&prev, &operator);
                            if ord != Ordering::Less {
                                values.push(prev);
                            } else {
                                stack_operator.push(prev);
                            }
                        }
                    }
                    stack_operator.push(operator);
                },
                TokenType::LeftCircle => {
                    if !stack_value.is_empty() {
                        return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                    }

                    stack_operator.push(LogicalNode::LeftCircle);
                },
                TokenType::RightCircle => {
                    if !stack_value.is_empty() {
                        let is_prefix = if let TokenType::Circumflex = stack_value[0] {
                            stack_value.remove(0);
                            true
                        } else {
                            false
                        };
                        let is_suffix = if let TokenType::Dollar = stack_value[stack_value.len() - 1] {
                            stack_value.pop();
                            true
                        } else {
                            false
                        };

                        let mut logic_values = Vec::new();
                        for value in stack_value.into_iter() {
                            match value {
                                TokenType::Value(s) => {
                                    logic_values.push(Value::Literal(s.to_owned()));
                                },
                                TokenType::Variable(s) => {
                                    logic_values.push(Value::Variable(s.to_owned()));
                                },
                                TokenType::Reference(i) => {
                                    logic_values.push(Value::Reference(*i));
                                },
                                TokenType::Part => {
                                    logic_values.push(Value::Part);
                                },
                                _ => (),
                            }
                        }

                        values.push(LogicalNode::Operand(ShiftPattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
                        stack_value = Vec::new();
                    }

                    loop {
                        if let Some(node) = stack_operator.pop() {
                            match node {
                                LogicalNode::LeftCircle => break,
                                _ => values.push(node),
                            }
                        } else {
                            return Err(Error::InvalidToken(String::from("when expression"), token.to_string(), next_index));
                        }
                    }
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
            let is_prefix = if let TokenType::Circumflex = stack_value[0] {
                stack_value.remove(0);
                true
            } else {
                false
            };
            let is_suffix = if let TokenType::Dollar = stack_value[stack_value.len() - 1] {
                stack_value.pop();
                true
            } else {
                false
            };

            let mut  logic_values = Vec::new();
            for value in stack_value.into_iter() {
                match value {
                    TokenType::Value(s) => logic_values.push(Value::Literal(s.to_owned())),
                    TokenType::Variable(s) => logic_values.push(Value::Variable(s.to_owned())),
                    TokenType::Reference(i) => logic_values.push(Value::Reference(*i)),
                    _ => (),
                }
            }

            values.push(LogicalNode::Operand(ShiftPattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
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


fn check_precedence(prev: &LogicalNode, now: &LogicalNode) -> Ordering {
    match prev {
        LogicalNode::And => match now {
            LogicalNode::And => Ordering::Equal,
            LogicalNode::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
        LogicalNode::Or => match now {
            LogicalNode::Or => Ordering::Equal,
            _ => Ordering::Less,
        },
        LogicalNode::Not => match now {
            LogicalNode::Not => Ordering::Less,
            LogicalNode::And | LogicalNode::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
        LogicalNode::Equal | LogicalNode::NotEqual | LogicalNode::Like => match now {
            LogicalNode::Equal | LogicalNode::NotEqual => Ordering::Equal,
            LogicalNode::And | LogicalNode::Or => Ordering::Greater,
            _ => Ordering::Less,
        },
        LogicalNode::Operand(_) | LogicalNode::Original | LogicalNode::NowForm | LogicalNode::Part => match now {
            LogicalNode::Operand(_) | LogicalNode::Original | LogicalNode::NowForm | LogicalNode::Part => Ordering::Equal,
            _ => Ordering::Greater,
        },
        LogicalNode::LeftCircle => Ordering::Greater,
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
        -- ??????????????????
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        T = "p" | "t" | "k"
        C = T | "f" | "s" | "h"
            -- `|`????????????????????????????????????
            | "l" | "y"

        -- `->`,`when`,`and`,`or`???????????????????????????????????????
        ^ "s" "k" V -> "s" @3
        "e" "a"
        | "i" "a"
        ->
            "y" "a"
        "i" . when @2 == "i" or @2 == "e" -> "i" "i"
        V T T V
            when @2 == @3
            -> @1 @2 @4
        "l" "l" V $
            -> "l" @3
        C "l" V | C "l" "y" 
            when @1 /= "l" -> @1 @3
        "t" "s" V
            when
                not (@0 like @1 @2 "a" $ or @0 like @1 @2 "u" $)
            ->
                "s" @3
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn use_semicolon() {
        let result = execute(r#"
        -- ??????????????????
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        T = "p" | "t" | "k"; C = T | "f" | "s" | "h" 
            -- `|`????????????????????????????????????
            | "l" | "y"

        -- ?????????????????????????????????????????????????????????????????????????????????
        ^ "s" "k" V -> "s" @3
        "e" "a" | "i" "a" -> "y" "a"; "i" . when @2 == "i" or @2 == "e" -> "i" "i"
        V T T V
            when @2 == @3
            -> @1 @2 @4
        "l" "l" V $
            -> "l" @3 ; C "l" V | C "l" "y" when @1 /= "l" -> @1 @3
        "t" "s" V
            when
                not (@0 like @1 @2 "a" $ or @0 like @1 @2 "u" $)
            ->
                "s" @3;
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }
}
