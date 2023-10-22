
use std::cmp::Ordering;
use std::rc::Rc;
use crate::error::Error;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub is_private: bool,
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    DefineVariable(Rc<DefineInfo>),
    Shift(ShiftInfo),
    If(Vec<LogicalNode>, Vec<Statement>),
    Elif(Vec<LogicalNode>, Vec<Statement>),
    Else(Vec<Statement>),
    Call(bool, String),
}

#[derive(Debug, Clone)]
pub(crate) struct DefineInfo {
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
    Inner(Box<Vec<Pattern>>),
}

#[derive(Debug, Clone)]
pub(crate) struct ShiftInfo {
    pub lhs: Vec<Pattern>,
    pub when: Option<Vec<LogicalNode>>,
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
pub(crate) enum LogicalNode {
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    Like,
    Operand(Pattern),
    Original,
    Part,
    NowForm,
    LeftCircle,
}

impl DefineInfo {
    pub fn new(name: impl Into<String>, expr: Vec<Vec<Value>>) -> Self {
        Self {
            name: name.into(),
            patterns: expr,
        }
    }
}

impl ShiftInfo {
    pub fn new(lhs: Vec<Pattern>, when: Option<Vec<LogicalNode>>, rhs: Pattern) -> Self {
        Self {
            lhs,
            when,
            rhs,
        }
    }
}

pub(crate) fn parse(tokens: &Vec<Token>) -> Result<Vec<Function>, Error> {
    let mut functions = Vec::new();
    let mut index = 0;
    let length = tokens.len();

    while index < length {
        if let Some(token) = tokens.get(index) {
            match &token.tokentype {
                TokenType::Unknown(_) => {
                    return Err(Error::unknown_with(&token.tokentype, token.row, token.column));
                },
                TokenType::NewLine | TokenType::Semicolon => {
                    index = index + 1;
                },
                _ => {
                    let (function, next_index) = parse_define_function(&tokens, index)?;

                    functions.push(function);
                    index = next_index;
                }
            }
        } else {
            return Err(Error::end_of("function"));
        }
    }

    Ok(functions)
}

fn check_token<'a, F, T>(tokens: &'a [Token], index: usize, parse_point: &str, op: F) -> Result<T, Error> 
    where F: FnOnce(&TokenType) -> Option<T> {
    match tokens.get(index) {
        Some(Token { row, column, tokentype }) => match op(tokentype) {
            Some(value) => Ok(value),
            None => Err(Error::invalid_with(parse_point, tokentype, *row, *column)),
        },
        None => Err(Error::end_of(parse_point)),
    }
}

fn parse_define_function(tokens: &[Token], index: usize) -> Result<(Function, usize), Error> {
    let parse_point = "define_function";

    let next_index = check_token(tokens, index, parse_point, |x| {
        if let TokenType::LeftBracket = x {
            Some(index + 1)
        } else {
            None
        }
    })?;

    let (is_private, name, next_index) = parse_function_name(tokens, next_index)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::RightBracket = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::NewLine = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let (statements, next_index) = parse_statements(tokens, next_index, false)?;

    Ok((Function { is_private, name, statements }, next_index))
}

fn parse_function_name(tokens: &[Token], index: usize) -> Result<(bool, String, usize), Error> {
    let (is_private, next_index) = match tokens.get(index) {
        Some(Token { row: _, column: _, tokentype: TokenType::NumberSign }) => (true, index + 1),
        _ => (false, index),
    };

    let parse_point = "function name";

    let (name, next_index) = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::Variable(name) = x {
            Some((name.to_owned(), next_index + 1))
        } else {
            None
        }
    })?;

    Ok((is_private, name, next_index))
}

fn parse_statements(tokens: &[Token], index: usize, is_local_statement: bool) -> Result<(Vec<Statement>, usize), Error> {
    let parse_point = "statements";
    let mut statements = Vec::new();
    let mut next_index = index;

    let length = tokens.len();
    while next_index < length {
        if matches!(tokens.get(next_index), Some(Token { row: _, column: _, tokentype: TokenType::NewLine | TokenType::Semicolon })) {
            next_index = next_index + 1;
            continue
        };

        if let Some(Token { row, column, tokentype }) = tokens.get(next_index) {
            if matches!(tokentype, TokenType::Elif | TokenType::Else) {
                statements.last().and_then(|x| {
                    if matches!(x, Statement::If(_, _) | Statement::Elif(_, _)) {
                        Some(())
                    } else {
                        None
                    }
                }).ok_or(Error::invalid_with(parse_point, tokentype, *row, *column))?;
            }
        }

        match parse_statement(tokens, next_index, is_local_statement) {
            Ok((statement, index)) => {
                statements.push(statement);
                next_index = index;
            },
            Err(error) => {
                match &error {
                    Error::InvalidToken(_, _, _) => {
                        if tokens.get(next_index).map(|x| x.tokentype == TokenType::RightBrace).unwrap_or_default() {
                            break;
                        } else {
                            return Err(error)
                        }
                    }
                    _ => return Err(error),
                }
            },
        }
    }
    
    Ok((statements, next_index))
}

fn parse_statement(tokens: &[Token], index: usize, is_local_statement: bool) -> Result<(Statement, usize), Error> {
    let parse_point = "statement";

    match tokens.get(index) {
        Some(Token { row, column, tokentype }) => {
            match tokentype {
                TokenType::If => parse_if(tokens, index),
                TokenType::Elif => parse_elif(tokens, index),
                TokenType::Else => parse_else(tokens, index),
                TokenType::Call => parse_call(tokens, index, is_local_statement),
                TokenType::Value(_) | TokenType::Circumflex | TokenType::LeftCircle => parse_shift(tokens, index, is_local_statement),
                TokenType::Variable(_) => {
                    if let Some(Token { row: _, column: _, tokentype }) = tokens.get(index + 1) {
                        if matches!(tokentype, TokenType::Bind) {
                            parse_define(tokens, index, is_local_statement)
                        } else {
                            parse_shift(tokens, index, is_local_statement)
                        }
                    } else {
                        Err(Error::end_of(parse_point))
                    }
                },
                _ => Err(Error::invalid_with(parse_point, tokentype, *row, *column)),
            }
        },
        None => Err(Error::end_of(parse_point)),
    }
}

fn parse_if(tokens: &[Token], index: usize) -> Result<(Statement, usize), Error> {
    let parse_point = "if statements";
    let next_index = check_token(tokens, index, parse_point, |x| {
        if let TokenType::If = x {
            Some(index + 1)
        } else {
            None
        }
    })?;

    let (condition, next_index) = parse_condition(tokens, next_index)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::LeftBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let (statements, next_index) = parse_statements(tokens, next_index, true)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::RightBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    Ok((Statement::If(condition, statements), next_index))
}

fn parse_elif(tokens: &[Token], index: usize) -> Result<(Statement, usize), Error> {
    let parse_point = "elif statements";
    let next_index = check_token(tokens, index, parse_point, |x| {
        if let TokenType::Elif = x {
            Some(index + 1)
        } else {
            None
        }
    })?;

    let (condition, next_index) = parse_condition(tokens, next_index)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::LeftBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let (statements, next_index) = parse_statements(tokens, next_index, true)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::RightBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    Ok((Statement::Elif(condition, statements), next_index))
}

fn parse_else(tokens: &[Token], index: usize) -> Result<(Statement, usize), Error> {
    let parse_point = "else statements";
    let next_index = check_token(tokens, index, parse_point, |x| {
        if let TokenType::Else = x {
            Some(index + 1)
        } else {
            None
        }
    })?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::LeftBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let (statements, next_index) = parse_statements(tokens, next_index, true)?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if let TokenType::RightBrace = x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    Ok((Statement::Else(statements), next_index))
}

fn parse_call(tokens: &[Token], index: usize, is_local_statement: bool) -> Result<(Statement, usize), Error> {
    let parse_point = "call statement";
    let next_index = check_token(tokens, index, parse_point, |x| {
        if let TokenType::Call = x {
            Some(index + 1)
        } else {
            None
        }
    })?;

    let (is_private, name, next_index) = parse_function_name(tokens, next_index)?;

    check_token(tokens, next_index, parse_point, |x| {
        match x {
            TokenType::Semicolon | TokenType::NewLine => {
                Some((Statement::Call(is_private, name), next_index + 1))
            },
            TokenType::RightBrace if is_local_statement => {
                Some((Statement::Call(is_private, name), next_index))
            },
            _ => None,
        }
    })
}

fn parse_define(tokens: &[Token], index: usize, is_local_statement: bool) -> Result<(Statement, usize), Error> {
    let parse_point = "define variable";

    let (name, next_index) = check_token(tokens, index, parse_point, |x| {
        if let TokenType::Variable(name) = x {
            Some((name.to_owned(), index + 1))
        } else {
            None
        }
    })?;

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        if &TokenType::Bind == x {
            Some(next_index + 1)
        } else {
            None
        }
    })?;

    let (patterns, next_index) = parse_define_patterns(&tokens, next_index)?;

    check_token(tokens, next_index, parse_point, |x| {
        match x {
            TokenType::Semicolon | TokenType::NewLine => {
                Some((Statement::DefineVariable(Rc::new(DefineInfo::new(name, patterns))), next_index + 1))
            },
            TokenType::RightBrace if is_local_statement => {
                Some((Statement::DefineVariable(Rc::new(DefineInfo::new(name, patterns))), next_index))
            },
            _ => None,
        }
    })
}

fn parse_define_patterns(tokens: &[Token], index: usize) -> Result<(Vec<Vec<Value>>, usize), Error> {
    let mut patterns: Vec<Vec<Value>> = Vec::new();
    let (pattern, mut next_index) = parse_define_pattern(tokens, index)?;
    patterns.push(pattern);

    loop {
        if matches!(tokens.get(next_index), Some(Token { row: _, column: _, tokentype: TokenType::VerticalBar })) {
            next_index = next_index + 1;
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
    check_token(tokens, index, "value", |x| {
        match x {
            TokenType::Value(value) => Some((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Some((Value::Variable(value.clone()), index + 1)),
            _ => None,
        }
    })
}

fn parse_shift(tokens: &[Token], index: usize, is_local_statement: bool) -> Result<(Statement, usize), Error> {
    let (lhs, mut next_index) = parse_match_patterns(&tokens, index)?;
    
    while matches!(tokens.get(next_index).map(|x| &x.tokentype), Some(TokenType::NewLine)) {
        next_index = next_index + 1;
    }

    let (when_, next_index) = if matches!(tokens.get(next_index).map(|x| &x.tokentype), Some(TokenType::When)) {
        parse_condition(&tokens, next_index + 1).map(|(w, i)| (Some(w), i))?
    } else {
        (None, next_index)
    };

    let parse_point = "shift statement";

    let next_index = check_token(tokens, next_index, parse_point, |x| {
        match x {
            TokenType::RightArrow => Some(next_index + 1),
            _ => None
        }
    })?;

    let (rhs, next_index) = parse_convert_pattern(&tokens, next_index)?;

    if let Some(Value::Variable(other)) = rhs.values.iter().find(|x| matches!(x, Value::Variable(_))) {
        return Err(Error::message(format!("Cannot use variable in right of shift statement: `{}`", other)));
    }

    check_token(tokens, next_index, parse_point, |x| {
        match x {
            TokenType::Semicolon | TokenType::NewLine => {
                Some((Statement::Shift(ShiftInfo::new(lhs, when_, rhs)), next_index + 1))
            },
            TokenType::RightBrace if is_local_statement => {
                Some((Statement::Shift(ShiftInfo::new(lhs, when_, rhs)), next_index))
            },
            _ => None
        }
    })
}

fn parse_match_patterns(tokens: &[Token], index: usize) -> Result<(Vec<Pattern>, usize), Error> {
    let mut patterns: Vec<Pattern> = Vec::new();
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

fn parse_match_pattern(tokens: &[Token], index: usize) -> Result<(Pattern, usize), Error> {
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

    Ok((Pattern { values, mode } , next_index))
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
    let parse_point = "shift value";

    if let Some(token) = tokens.get(index) {
        match &token.tokentype {
            TokenType::Value(value) => Ok((Value::Literal(value.clone()), index + 1)),
            TokenType::Variable(value) => Ok((Value::Variable(value.clone()), index + 1)),
            TokenType::Reference(value) => Ok((Value::Reference(*value), index + 1)),
            TokenType::AnyChar => Ok((Value::AnyChar, index + 1)),
            TokenType::LeftCircle => {
                let (value, next_index) = parse_match_patterns(tokens, index + 1)?;
                check_token(tokens, next_index, parse_point, |x| {
                    match x {
                        TokenType::RightCircle => Some((Value::Inner(Box::new(value)), next_index + 1)),
                        _ => None,
                    }
                })
            }
            _ => Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column)),
        }
    } else {
        Err(Error::end_of(parse_point))
    }
}

fn parse_convert_pattern(tokens: &[Token], index: usize) -> Result<(Pattern, usize), Error> {
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

    Ok((Pattern { values, mode: Mode::None } , next_index))
}

fn parse_condition(tokens: &[Token], index: usize) -> Result<(Vec<LogicalNode>, usize), Error> {
    let mut stack_operator: Vec<LogicalNode> = Vec::new();
    let mut stack_value: Vec<&TokenType> = Vec::new();
    let mut values: Vec<LogicalNode> = Vec::new();
    let mut next_index = index;
    
    let parse_point = "condition";

    loop {
        if let Some(token) = tokens.get(next_index) {
            match token.tokentype {
                TokenType::Circumflex => {
                    if stack_value.is_empty() {
                        stack_value.push(&token.tokentype);
                    } else {
                        return Err(Error::invalid_with(parse_point, TokenType::Dollar.to_string(), token.row, token.column));
                    }
                },
                TokenType::Dollar => {
                    stack_value.push(&token.tokentype);
                },
                TokenType::Value(_) | TokenType::Variable(_) | TokenType::Reference(_) => {
                    if let Some(TokenType::Dollar) = stack_value.last() {
                        return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
                    } else {
                        stack_value.push(&token.tokentype);
                    }
                },
                TokenType::Original => {
                    if !stack_value.is_empty() {
                        return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
                    }
                    values.push(LogicalNode::Original);
                },
                TokenType::NowForm => {
                    if !stack_value.is_empty() {
                        return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
                    }
                    values.push(LogicalNode::NowForm);
                },
                TokenType::Part => {
                    if !stack_value.is_empty() {
                        return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
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
                        _ => return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column))
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

                        values.push(LogicalNode::Operand(Pattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
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
                        return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
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

                        values.push(LogicalNode::Operand(Pattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
                        stack_value = Vec::new();
                    }

                    loop {
                        if let Some(node) = stack_operator.pop() {
                            match node {
                                LogicalNode::LeftCircle => break,
                                _ => values.push(node),
                            }
                        } else {
                            return Err(Error::invalid_with(parse_point, &token.tokentype, token.row, token.column));
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

            values.push(LogicalNode::Operand(Pattern { values: logic_values, mode: get_mode(is_prefix, is_suffix) }));
        }
        if values.is_empty() {
            return Err(Error::end_of(parse_point));
        }
        stack_operator.reverse();
        values.append(&mut stack_operator);
    } else if values.is_empty() {
        return Err(Error::end_of(parse_point));
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

    fn execute(s: &str) -> Result<Vec<Function>, Error> {
        let tokens = lexer(s);
        parse(&tokens)
    }

    #[test]
    fn default() {
        let result = execute(r#"
        -- 一行コメント
        [main]
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
    fn call_function() {
        let result = execute(r#"
        -- 複数の関数定義
        [func1]
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
            -- `|`の前で改行することが可能
            | "l" | "y"

        -- `->`,`when`,`and`,`or`の前後で改行することが可能
        ^ "s" "k" V -> "s" @3
        "e" "a"
        | "i" "a"
        ->
            "y" "a"
        call #func2

        [#func2]
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        T = "p" | "t" | "k"
        C = T | "f" | "s" | "h"
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
        [main]
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

    #[test]
    fn if_statement() {
        let result = execute(r#"
            [main]
            V = "a" | "e" | "i" | "o" | "u"
            if @2 == @3 or @3 == "" {
                "st" V V | "st" V -> "s" @2
            }
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn if_else_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        } else {
            ("s" | "k") ("i" | "e") V -> @1 "j" @3
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn if_elif_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        } elif @n like "mu" {
            "mu" -> "m"
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn if_elif_else_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        } elif @n like "mu" {
            "mu" -> "m"
        } else {
            ("s" | "k") ("i" | "e") V -> @1 "j" @3
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn elif_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u";
        elif @n like "mu" {
            "mu" -> "m"
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_err());
    }

    #[test]
    fn else_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u";
        else {
            ("s" | "k") ("i" | "e") V -> @1 "j" @3
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_err());
    }

    #[test]
    fn elif_else_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u";
        elif @n like "mu" {
            "mu" -> "m"
        } else {
            ("s" | "k") ("i" | "e") V -> @1 "j" @3
        }
        "#);

        println!("{:?}", result);
        assert!(result.is_err());
    }

}
