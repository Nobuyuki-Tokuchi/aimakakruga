
use std::cmp::Ordering;
use std::rc::Rc;
use crate::error::Error;
use crate::token::{Token, TokenType};

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
    Inner(Box<Vec<ShiftPattern>>),
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

pub(crate) fn parse(tokens: &Vec<Token>) -> Result<Vec<Statement>, Error> {
    let mut statements = vec![];

    let mut index = 0;
    let length = tokens.len();
    while index < length {
        if let Some(token) = tokens.get(index) {
            match &token.tokentype {
                TokenType::Variable(value) => {
                    let (statement, next_index) = if let Some(next) = tokens.get(index + 1) {
                        match next.tokentype {
                            TokenType::Bind => parse_define(&value, tokens, index + 1),
                            _ => parse_shift(&tokens, index),
                        }?
                    } else {
                        return Err(Error::end_of("statement"));
                    };

                    statements.push(statement);
                    index = next_index;
                },
                TokenType::Value(_) | TokenType::Circumflex | TokenType::LeftCircle => {
                    let (statement, next_index) = parse_shift(&tokens, index)?;

                    statements.push(statement);
                    index = next_index;
                },
                TokenType::Unknown(_) => {
                    return Err(Error::unknown_with(&token.tokentype, token.row, token.column));
                },
                TokenType::NewLine | TokenType::Semicolon => {
                    index = index + 1;
                }
                _ => {
                    return Err(Error::invalid_with("statement", &token.tokentype, token.row, token.column));
                }
            }
        } else {
            return Err(Error::end_of("statement"));
        }
    }
    
    Ok(statements)
}

fn parse_define(name: &str, tokens: &[Token], index: usize) -> Result<(Statement, usize), Error> {
    let next_index = match tokens.get(index) {
        Some(Token { row: _, column: _, tokentype: TokenType::Bind } ) => index + 1,
        Some(other) => return Err(Error::invalid_with("define variable", &other.tokentype, other.row, other.column)),
        None => return Err(Error::end_of("define variable"))
    };

    let (patterns, next_index) = parse_define_patterns(&tokens, next_index)?;

    if let Some(token) = tokens.get(next_index) {
        if TokenType::Semicolon == token.tokentype || TokenType::NewLine == token.tokentype {
            let statement = Statement::Define(Rc::new(DefineStruct::new(name, patterns)));
            Ok((statement, next_index + 1))
        } else {
            Err(Error::invalid_with("expression of define variable", &token.tokentype, token.row, token.column))
        }
    } else {
        Err(Error::end_of("expression of define variable"))
    }
}

fn parse_define_patterns(tokens: &[Token], index: usize) -> Result<(Vec<Vec<Value>>, usize), Error> {
    let mut patterns: Vec<Vec<Value>> = Vec::new();
    let (pattern, mut next_index) = parse_define_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token.tokentype {
                TokenType::VerticalBar => {
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
            let next_token = tokens.get(next_index).or(tokens.last()).map(|x| (x.row, x.column));
            let message = "Next pattern is nothing in patterns";

            return if let Some((row, column)) = next_token {
                Err(Error::message_with(message, row, column))
            } else {
                Err(Error::message(message))
            }
        }
    }

    Ok((patterns, next_index))
}

fn parse_define_pattern(tokens: &[Token], index: usize) -> Result<(Vec<Value>, usize), Error> {
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

fn parse_value(tokens: &[Token], index: usize) -> Result<(Value, usize), Error> {
    if let Some(token) = tokens.get(index) {
        match &token.tokentype {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            _ => Err(Error::invalid_with("value", &token.tokentype, token.row, token.column)),
        }
    } else {
        Err(Error::end_of("value"))
    }
}

fn parse_shift(tokens: &[Token], index: usize) -> Result<(Statement, usize), Error> {
    let (lhs, mut next_index) = parse_match_patterns(&tokens, index)?;
    
    while let Some(TokenType::NewLine) = tokens.get(next_index).map(|x| &x.tokentype) {
        next_index = next_index + 1;
    }

    let (when_, next_index) = if let Some(TokenType::When) = tokens.get(next_index).map(|x| &x.tokentype) {
        parse_when(&tokens, next_index + 1).map(|(w, i)| (Some(w), i))?
    } else {
        (None, next_index)
    };

    let next_index = match tokens.get(next_index) {
        Some(token) => {
            if let TokenType::RightArrow = token.tokentype {
                next_index + 1
            } else {
                return Err(Error::invalid_with("shift statement", &token.tokentype, token.row, token.column));
            }
        }
        None => return Err(Error::end_of("shift statement"))
    };

    let (rhs, next_index) = parse_convert_pattern(&tokens, next_index)?;

    if let Some(Value::Variable(other)) = rhs.values.iter().find(|x| if let Value::Variable(_) = x { true } else { false }) {
        return Err(Error::message(format!("Cannot use variable in right of shift statement: `{}`", other)));
    }

    if let Some(token) = tokens.get(next_index) {
        if TokenType::Semicolon == token.tokentype || TokenType::NewLine == token.tokentype {
            let statement = Statement::Shift(ShiftStruct::new(lhs, when_, rhs));
            Ok((statement, next_index + 1))
        } else {
            Err(Error::invalid_with("expression of shift statement", &token.tokentype, token.row, token.column))
        }
    } else {
        Err(Error::end_of("expression of shift statement"))
    }
}

fn parse_match_patterns(tokens: &[Token], index: usize) -> Result<(Vec<ShiftPattern>, usize), Error> {
    let mut patterns: Vec<ShiftPattern> = Vec::new();
    let (pattern, mut next_index) = parse_match_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token.tokentype {
                TokenType::VerticalBar => {
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
            let next_token = tokens.get(next_index).or(tokens.last()).map(|x| (x.row, x.column));
            let message = "Next pattern is nothing in patterns";

            return if let Some((row, column)) = next_token {
                Err(Error::message_with(message, row, column))
            } else {
                Err(Error::message(message))
            }
        }
    }

    Ok((patterns, next_index))
}

fn parse_match_pattern(tokens: &[Token], index: usize) -> Result<(ShiftPattern, usize), Error> {
    let mut values = Vec::new();

    let (is_prefix, next_index) = if let Some(token) = tokens.get(index) {
        match token.tokentype {
            TokenType::Circumflex => (true, index + 1),
            _ => (false, index),
        }
    } else {
        return Err(Error::end_of("pattern (prefix)"))
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
        match token.tokentype {
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

fn parse_shift_value(tokens: &[Token], index: usize) -> Result<(Value, usize), Error> {
    if let Some(token) = tokens.get(index) {
        match &token.tokentype {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            TokenType::Reference(value) => Ok((Value::Reference(*value), index + 1)),
            TokenType::AnyChar => Ok((Value::AnyChar, index + 1)),
            TokenType::LeftCircle => {
                let (value, next_index) = parse_match_patterns(tokens, index + 1)?;
                if let Some(token) = tokens.get(next_index) {
                    if token.tokentype == TokenType::RightCircle {
                        Ok((Value::Inner(Box::new(value)), next_index + 1))
                    } else {
                        Err(Error::invalid_with("value", &token.tokentype, token.row, token.column))
                    }
                } else {
                    Err(Error::end_of("shift value"))
                }
            }
            _ => Err(Error::invalid_with("value", &token.tokentype, token.row, token.column)),
        }
    } else {
        Err(Error::end_of("value"))
    }
}

fn parse_convert_pattern(tokens: &[Token], index: usize) -> Result<(ShiftPattern, usize), Error> {
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

fn parse_when(tokens: &[Token], index: usize) -> Result<(Vec<LogicalNode>, usize), Error> {
    let mut stack_operator: Vec<LogicalNode> = Vec::new();
    let mut stack_value: Vec<&TokenType> = Vec::new();
    let mut values: Vec<LogicalNode> = Vec::new();
    let mut next_index = index;

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token.tokentype {
                TokenType::Circumflex => {
                    if stack_value.is_empty() {
                        stack_value.push(&token.tokentype);
                    } else {
                        return Err(Error::invalid_with("when expression", TokenType::Dollar.to_string(), token.row, token.column));
                    }
                },
                TokenType::Dollar => {
                    stack_value.push(&token.tokentype);
                },
                TokenType::Value(_) | TokenType::Variable(_) | TokenType::Reference(_) => {
                    if let Some(TokenType::Dollar) = stack_value.last() {
                        return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
                    } else {
                        stack_value.push(&token.tokentype);
                    }
                },
                TokenType::Original => {
                    if !stack_value.is_empty(){
                        return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
                    }
                    values.push(LogicalNode::Original);
                },
                TokenType::NowForm => {
                    if !stack_value.is_empty(){
                        return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
                    }
                    values.push(LogicalNode::NowForm);
                },
                TokenType::Part => {
                    if !stack_value.is_empty(){
                        return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
                    }
                    values.push(LogicalNode::Part);
                }
                TokenType::Equal | TokenType::NotEqual | TokenType::LogicalAnd | TokenType::LogicalOr | TokenType::LogicalNot | TokenType::Like => {
                    let operator = match token.tokentype {
                        TokenType::Equal => LogicalNode::Equal,
                        TokenType::NotEqual => LogicalNode::NotEqual,
                        TokenType::LogicalAnd => LogicalNode::And,
                        TokenType::LogicalOr => LogicalNode::Or,
                        TokenType::LogicalNot => LogicalNode::Not,
                        TokenType::Like => LogicalNode::Like,
                        _ => return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column))
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
                        return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
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
                            return Err(Error::invalid_with("when expression", &token.tokentype, token.row, token.column));
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
            return Err(Error::end_of("when expression"));
        }
        stack_operator.reverse();
        values.append(&mut stack_operator);
    } else if values.is_empty() {
        return Err(Error::end_of("when expression"));
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
        let tokens = lexer(s);
        parse(&tokens)
    }

    #[test]
    fn default() {
        let result = execute(r#"
        -- 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        T = "p" | "t" | "k"
        C = T | "f" | "s" | "h"
            -- `|`の前で改行することが可能
            | "l" | "y"

        -- `->`,`when`,`and`,`or`の前後で改行することが可能
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
        -- 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        T = "p" | "t" | "k"; C = T | "f" | "s" | "h" 
            -- `|`の前で改行することが可能
            | "l" | "y"

        -- セミコロンを使用すると一行に複数のパターンを記述できる
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
