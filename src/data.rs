use std::{fs::File, io::Read};
use std::{collections::HashMap, rc::Rc};

use crate::error::Error;
use crate::lexer::lexer;
use crate::parser::*;

#[derive(Debug, Clone)]
pub struct Data {
    replace_patterns: Vec<ReplacePattern>,
}

#[derive(Debug, Clone)]
pub(crate) struct ReplacePattern {
    pub from_list: Vec<String>,
    pub to: String,
    pub when: Option<Vec<LogicalValue>>
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
                    from_list.push(create_from_pattern(pattern, &variables)?);
                }

                regex_str.push(ReplacePattern {
                    from_list,
                    to,
                    when: shift.when.clone()
                });
            },
        }
    }

    Ok(regex_str)
}

fn create_from_pattern(pattern: &Pattern, variables: &HashMap<String, Rc<DefineStruct>>) -> Result<String, Error> {
    let mut pattern_str = String::default();
    let values = convert_from_values(&pattern.values, variables)?;

    if pattern.mode == Mode::Exact || pattern.mode == Mode::Forward {
        pattern_str.push('^');
    }

    for (index, value) in values.iter().enumerate() {
        let regex = format!("(?P<x{}>{})", index, value);
        pattern_str.push_str(regex.as_str());
    }

    if pattern.mode == Mode::Exact || pattern.mode == Mode::Backward {
        pattern_str.push('$');
    }

    Ok(pattern_str)
}

fn convert_from_values(values: &Vec<Value>, variables: &HashMap<String, Rc<DefineStruct>>) -> Result<Vec<String>, Error> {
    let mut values_str = Vec::default();
    
    for value in values.iter() {
        let s = match value {
            Value::Literal(s) => s.to_owned(),
            Value::Variable(var) => {
                if let Some(data) = variables.get(var) {
                    let result: Vec<Result<String, Error>> = data.expr.iter().map(|x| {
                        convert_from_values(&x.values, variables).map(|x| x.join("|"))
                    }).collect();

                    if let Some(err) = result.iter().find_map(|x| x.as_ref().err()) {
                        return Err(err.to_owned());
                    } else {
                        let result = result.iter().map(|x| {
                            x.as_ref().unwrap().as_str()
                        }).collect::<Vec<&str>>().join("|");
                        format!("({})", result)
                    }
                } else {
                    return Err(Error::NotFoundVariable(var.to_owned()));
                }
            },
            Value::Reference(index) => return Err(Error::ErrorMessage(format!("Invalid token: `@{}`", index + 1), None)),
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
            Value::Variable(var) => return Err(Error::ErrorMessage(format!("Invalid token: `{}`", var), None)),
            Value::Reference(index) => pattern_str.push_str(&format!("${{x{}}}", index)),
        };
    }

    Ok(pattern_str)
}
