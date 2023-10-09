use std::{fs::File, io::Read};
use std::{collections::HashMap, rc::Rc};

use crate::error::Error;
use crate::lexer::lexer;
use crate::{parser::{*, self}, common};

#[derive(Debug, Clone)]
pub struct Data {
    export_functions: HashMap<String, Vec<Statement>>,
    internal_functions: HashMap<String, Vec<Statement>>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Shift(Shift),
    If(Vec<ConditionValue>, Vec<Statement>),
    Call(bool, String),
} 

#[derive(Debug, Clone)]
pub(crate) struct Shift {
    pub from_list: Vec<String>,
    pub to: String,
    pub when: Option<Vec<ConditionValue>>,
}

#[derive(Debug, Clone)]
pub(crate) enum ConditionValue {
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
    pub(crate) fn get_export_ref(&self, name: impl Into<String>) -> Option<&Vec<Statement>> {
        self.export_functions.get(&name.into())
    }

    pub(crate) fn get_internal_ref(&self, name: impl Into<String>) -> Option<&Vec<Statement>> {
        self.internal_functions.get(&name.into())
    }

    pub fn read_file<P>(filename: P) -> Result<Self, Error>
    where
        P: AsRef<std::path::Path>
    {
        let text = {
            let mut f = File::open(filename).map_err(|_| Error::message("file not found"))?;
            let mut contents = String::new();
            f.read_to_string(&mut contents).map_err(|x| Error::message(x.to_string()))?;
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
            create_data(&x)
        }).map(|(export_functions, internal_functions)| Self {
            export_functions,
            internal_functions
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

fn create_data(functions: &Vec<Function>) -> Result<(HashMap<String, Vec<Statement>>, HashMap<String, Vec<Statement>>), Error> {
    let mut export_functions: HashMap<String, Vec<Statement>> = HashMap::new();
    let mut internal_functions: HashMap<String, Vec<Statement>> = HashMap::new();

    for function in functions.iter() {
        let name = function.name.to_owned();
        let statements = from_function(function)?;
        if function.is_private {
            internal_functions.insert(name, statements);
        } else {
            export_functions.insert(name, statements);
        }
    }

    Ok((export_functions, internal_functions))
}

fn from_function(operator: &Function) -> Result<Vec<Statement>, Error> {
    let mut variables: HashMap<String, Rc<DefineInfo>> = HashMap::new();
    let mut statements = Vec::new();

    for statement in operator.statements.iter() {
        create_statement(statement, &mut statements, &mut variables)?;
    }

    Ok(statements)
}

fn create_statement(statement: &parser::Statement, statements: &mut Vec<Statement>, variables: &mut HashMap<String, Rc<DefineInfo>>) -> Result<(), Error> {
    match statement {
        parser::Statement::DefineVariable(define) => {
            variables.insert(define.name.to_owned(), Rc::clone(&define));
        },
        parser::Statement::Shift(shift) => {
            let to = create_to_pattern(&shift.rhs)?;
            let mut from_list = Vec::new();
        
            for pattern in shift.lhs.iter() {
                from_list.push(create_from_pattern(pattern, &variables, false)?);
            }

            let when = match &shift.when {
                Some(when) => Some(create_condition(&when, variables)?),
                None => None,
            };

            statements.push(Statement::Shift(Shift {
                from_list,
                to,
                when
            }));
        },
        parser::Statement::If(logical_nodes, parser_statements) => {
            let condition = create_condition(logical_nodes, variables)?;
            let mut local_statements = Vec::new();

            for statement in parser_statements.iter() {
                let mut local_variables = HashMap::new();
                local_variables.clone_from(variables);

                create_statement(statement, &mut local_statements, &mut local_variables)?;
            }

            statements.push(Statement::If(condition, local_statements))
        },
        parser::Statement::Call(is_private, name) => {
            statements.push(Statement::Call(*is_private, name.to_owned()))
        },
    };

    Ok(())
}

fn create_from_pattern(pattern: &Pattern, variables: &HashMap<String, Rc<DefineInfo>>, anonymous_pattern: bool) -> Result<String, Error> {
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

fn convert_from_values(values: &Vec<Value>, variables: &HashMap<String, Rc<DefineInfo>>, anonymous_pattern: bool) -> Result<Vec<String>, Error> {
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
                    return Err(Error::NotFoundVariable(var.to_owned(), None));
                }
            },
            Value::Reference(index) => return Err(Error::invalid("match pattern", format!("`@{}`", index + 1))),
            Value::Part => return Err(Error::invalid("match pattern", common::PART_KEY.to_string())),
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


fn create_to_pattern(pattern: &Pattern) -> Result<String, Error> {
    let mut pattern_str = String::default();

    for value in pattern.values.iter() {
        match value {
            Value::Literal(s) => pattern_str.push_str(s),
            Value::Variable(var) => return Err(Error::message(format!("Invalid token: `{}`", var))),
            Value::Reference(index) => pattern_str.push_str(&format!("${{x{}}}", index)),
            Value::Part => pattern_str.push_str(&format!("$0")),
            Value::AnyChar  => return Err(Error::message("Invalid token: `.`")),
            Value::Inner(_) => return Err(Error::message("Invalid token: (..)")),
        };
    }

    Ok(pattern_str)
}

fn create_condition(logical_nodes: &Vec<LogicalNode>, variables: &HashMap<String, Rc<DefineInfo>>) -> Result<Vec<ConditionValue>, Error> {
    let mut result = Vec::new();

    for node in logical_nodes.iter() {
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
                                return Err(Error::NotFoundVariable(var.to_owned(), None));
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
                    result.push(ConditionValue::LikeOperand(values, operand.mode));
                } else {
                    result.push(ConditionValue::Operand(values));
                }
            },
            LogicalNode::And => result.push(ConditionValue::And),
            LogicalNode::Or => result.push(ConditionValue::Or),
            LogicalNode::Not => result.push(ConditionValue::Not),
            LogicalNode::Equal => result.push(ConditionValue::Equal),
            LogicalNode::NotEqual => result.push(ConditionValue::NotEqual),
            LogicalNode::Like => result.push(ConditionValue::Like),
            LogicalNode::Original => result.push(ConditionValue::Original),
            LogicalNode::Part => result.push(ConditionValue::Part),
            LogicalNode::NowForm => result.push(ConditionValue::NowForm),
            LogicalNode::LeftCircle => (),
        }
    }

    Ok(result)
}