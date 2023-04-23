use std::{fs::File, io::Read};
use std::{collections::HashMap, rc::Rc};

use crate::error::Error;
use crate::lexer::lexer;
use crate::{parser::*, common};

#[derive(Debug, Clone)]
pub struct Data {
    replace_patterns: Vec<ReplacePattern>,
}

#[derive(Debug, Clone)]
pub(crate) struct ReplacePattern {
    pub from_list: Vec<String>,
    pub to: String,
    pub when: Option<Vec<WhenValue>>,
}

#[derive(Debug, Clone)]
pub(crate) enum WhenValue {
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    Like,
    Operand(Vec<Value>),
    LikeOperand(Vec<Value>, Mode),
    Original,
    Part,
    NowForm,
}


impl Data {
    pub(crate) fn get_replace_patterns_ref(&self) -> &Vec<ReplacePattern> {
        &self.replace_patterns
    }

    pub fn read_file<P>(filename: P) -> Result<Self, Error>
    where
        P: AsRef<std::path::Path>
    {
        let text = {
            let mut f = File::open(filename).map_err(|_| Error::ErrorMessage(String::from("file not found"), None))?;
            let mut contents = String::new();
            f.read_to_string(&mut contents).map_err(|x| Error::ErrorMessage(x.to_string(), None))?;
            contents
        };

        Self::try_from(text.as_str())
    }
}

// impl TryFrom<Vec<&str>> for Data {
//     type Error = Error;

//     fn try_from(value: Vec<&str>) -> Result<Self, Self::Error> {
//         let tokens = lexer_by_vec(value);
//         parse(&tokens).map(|x| Self {
//             statements: x
//         })
//     }
// }

impl TryFrom<&str> for Data {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let tokens = lexer(value);
        parse(&tokens).and_then(|x| {
            create_regex_str(&x)
        }).map(|x| Self {
            replace_patterns: x,
        })
    }
}

impl TryFrom<String> for Data {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

impl TryFrom<&String> for Data {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

impl std::str::FromStr for Data {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

fn create_regex_str(operators: &Vec<Statement>) -> Result<Vec<ReplacePattern>, Error> {
    let mut variables: HashMap<String, Rc<DefineStruct>> = HashMap::new();
    let mut regex_str: Vec<ReplacePattern> = Vec::new();

    for operator in operators.iter() {
        match operator {
            Statement::Define(data) => {
                variables.insert(data.name.to_string(), Rc::clone(&data));
            },
            Statement::Shift(shift) => {
                let to = create_to_pattern(&shift.rhs)?;
                let mut from_list = Vec::new();
            
                for pattern in shift.lhs.iter() {
                    from_list.push(create_from_pattern(pattern, &variables, false)?);
                }

                let when = match &shift.when {
                    Some(when) => Some(create_when(&when, &variables)?),
                    None => None,
                };

                regex_str.push(ReplacePattern {
                    from_list,
                    to,
                    when,
                });
            },
        }
    }

    Ok(regex_str)
}

fn create_from_pattern(pattern: &ShiftPattern, variables: &HashMap<String, Rc<DefineStruct>>, anonymous_pattern: bool) -> Result<String, Error> {
    let mut pattern_str = String::default();
    let values = convert_from_values(&pattern.values, variables, anonymous_pattern)?;

    if pattern.mode == Mode::Exact || pattern.mode == Mode::Forward {
        pattern_str.push('^');
    }

    for (index, value) in values.iter().enumerate() {
        let regex = if anonymous_pattern {
            if value.contains("|") {
                format!("({})", value)
            } else {
                value.to_string()
            }
        } else {
            format!("(?P<x{}>{})", index, value)
        };
        pattern_str.push_str(regex.as_str());
    }

    if pattern.mode == Mode::Exact || pattern.mode == Mode::Backward {
        pattern_str.push('$');
    }

    Ok(pattern_str)
}

fn convert_from_values(values: &Vec<Value>, variables: &HashMap<String, Rc<DefineStruct>>, anonymous_pattern: bool) -> Result<Vec<String>, Error> {
    let mut values_str = Vec::default();
    
    for value in values.iter() {
        let s = match value {
            Value::Literal(s) => s.to_owned(),
            Value::Variable(var) => {
                if let Some(data) = variables.get(var) {
                    let result: Vec<Result<String, Error>> = data.patterns.iter().map(|x| {
                        convert_from_values(x, variables, anonymous_pattern).map(|x| x.join(""))
                    }).collect();

                    if let Some(err) = result.iter().find_map(|x| x.as_ref().err()) {
                        return Err(err.to_owned());
                    } else {
                        let result = result.iter().map(|x| {
                            x.as_ref().unwrap().as_str()
                        }).collect::<Vec<&str>>().join("|");

                        if result.contains("|") {
                            format!("({})", result)
                        } else {
                            result.to_string()
                        }
                    }
                } else {
                    return Err(Error::NotFoundVariable(var.to_owned()));
                }
            },
            Value::Reference(index) => return Err(Error::ErrorMessage(format!("Invalid token: `@{}`", index + 1), None)),
            Value::Part => return Err(Error::ErrorMessage(format!("Invalid token: `{}`", common::PART_KEY), None)),
            Value::AnyChar => String::from("."),
            Value::Inner(b) => {
                let inner: Vec<Result<String, Error>> = b.iter().map(|x| create_from_pattern(x, variables, true)).collect();

                if let Some(err) = inner.iter().find_map(|x| x.as_ref().err()) {
                    return Err(err.to_owned());
                } else {
                    let inner = inner.iter().map(|x| {
                        x.as_ref().unwrap().as_str()
                    }).collect::<Vec<&str>>().join("|");

                    if inner.contains("|") {
                        format!("({})", inner)
                    } else {
                        inner.to_string()
                    }
                }
            },
        };

        values_str.push(s);
    }

    Ok(values_str)
}


fn create_to_pattern(pattern: &ShiftPattern) -> Result<String, Error> {
    let mut pattern_str = String::default();

    for value in pattern.values.iter() {
        match value {
            Value::Literal(s) => pattern_str.push_str(s),
            Value::Variable(var) => return Err(Error::ErrorMessage(format!("Invalid token: `{}`", var), None)),
            Value::Reference(index) => pattern_str.push_str(&format!("${{x{}}}", index)),
            Value::Part => pattern_str.push_str(&format!("$0")),
            Value::AnyChar  => return Err(Error::ErrorMessage("Invalid token: `.`".to_string(), None)),
            Value::Inner(_) => return Err(Error::ErrorMessage(format!("Invalid token: (..)"), None)),
        };
    }

    Ok(pattern_str)
}

fn create_when(when: &Vec<LogicalNode>, variables: &HashMap<String, Rc<DefineStruct>>) -> Result<Vec<WhenValue>, Error> {
    let mut result = Vec::new();

    for node in when.iter() {
        match node {
            LogicalNode::Operand(operand) => {
                let mut values = Vec::new();
                let mut like_operand = operand.mode != Mode::None;

                for value in operand.values.iter() {
                    let value = match value {
                        Value::Literal(s) => Value::Literal(s.to_owned()),
                        Value::Variable(var) => {
                            if let Some(data) = variables.get(var) {
                                let result: Vec<Result<String, Error>> = data.patterns.iter().map(|x| {
                                    convert_from_values(x, variables, false).map(|x| x.join(""))
                                }).collect();
            
                                if let Some(err) = result.iter().find_map(|x| x.as_ref().err()) {
                                    return Err(err.to_owned());
                                } else {
                                    let result = result.iter().map(|x| {
                                        x.as_ref().unwrap().as_str()
                                    }).collect::<Vec<&str>>().join("|");
                                    like_operand = true;

                                    if result.contains("|") {
                                        Value::Variable(format!("({})", result))
                                    } else {
                                        Value::Variable(result.to_string())
                                    }
                                }
                            } else {
                                return Err(Error::NotFoundVariable(var.to_owned()));
                            }
                        },
                        Value::Part => Value::Part,
                        Value::Reference(index) => Value::Reference(*index),
                        Value::AnyChar => {
                            like_operand = true;
                            Value::AnyChar
                        },
                        Value::Inner(b) => {
                            let inner: Vec<Result<String, Error>> = b.iter().map(|x| create_from_pattern(x, variables, true)).collect();
            
                            if let Some(err) = inner.iter().find_map(|x| x.as_ref().err()) {
                                return Err(err.to_owned());
                            } else {
                                let inner = inner.iter().map(|x| {
                                    x.as_ref().unwrap().as_str()
                                }).collect::<Vec<&str>>().join("|");

                                if inner.contains("|") {
                                    Value::Variable(format!("({})", inner))
                                } else {
                                    Value::Variable(inner.to_string())
                                }
                            }
                        },
                    };
                    values.push(value);
                }

                if like_operand {
                    result.push(WhenValue::LikeOperand(values, operand.mode));
                } else {
                    result.push(WhenValue::Operand(values));
                }
            },
            LogicalNode::And => result.push(WhenValue::And),
            LogicalNode::Or => result.push(WhenValue::Or),
            LogicalNode::Not => result.push(WhenValue::Not),
            LogicalNode::Equal => result.push(WhenValue::Equal),
            LogicalNode::NotEqual => result.push(WhenValue::NotEqual),
            LogicalNode::Like => result.push(WhenValue::Like),
            LogicalNode::Original => result.push(WhenValue::Original),
            LogicalNode::Part => result.push(WhenValue::Part),
            LogicalNode::NowForm => result.push(WhenValue::NowForm),
            LogicalNode::LeftCircle => (),
        }
    }

    Ok(result)
}