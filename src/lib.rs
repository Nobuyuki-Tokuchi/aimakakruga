mod error;
mod data;
mod lexer;
mod parser;

use std::collections::HashMap;

pub use data::Data;
use data::WhenValue;
pub use error::Error;
use parser::Mode;
use regex::Regex;

pub fn execute(word: &str, data: &Data) -> Result<String, Error> {
    let mut static_reference: HashMap<String, String> = HashMap::from_iter(vec![
        (String::from("@@"), word.to_owned()),
        (String::from("@%"), word.to_owned())
    ]);
    let replace_patterns = data.get_replace_patterns_ref();

    for pattern in replace_patterns.iter() {
        let mut now_word = static_reference.get("@%").unwrap().to_owned();

        for from in pattern.from_list.iter() {
            let regex = Regex::new(from).map_err(|x| Error::ErrorMessage(format!("{}", x), None))?;

            now_word = if let Some(when) = &pattern.when {
                let mut temporary = now_word.clone();
                for captures in regex.captures_iter(&now_word) {
                    if check_when(&captures, when, &static_reference)? {
                        let mut to_str = String::default();
                        captures.expand(&pattern.to, &mut to_str);

                        temporary = temporary.replace(captures.get(0).unwrap().as_str(), &to_str);
                    };
                }
                temporary
            }
            else {
                regex.replace_all(&now_word, &pattern.to).to_string()
            };
        }

        *static_reference.get_mut("@%").unwrap() = now_word;
    }

    Ok(static_reference.get("@%").unwrap().clone())
}

pub fn execute_many(word_list: &[String], data: &Data) -> Vec<Result<String, Error>> {
    let mut result_list = Vec::new();

    for word in word_list.iter() {
        result_list.push(execute(word, data));
    }

    result_list
}

fn check_when(captures: &regex::Captures, when: &[WhenValue], static_reference: &HashMap<String, String>, ) -> Result<bool, Error> {
    let mut stack = Vec::new();
    let mut stack_bool = Vec::new();

    for value in when.iter() {
        match value {
            WhenValue::And => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first && second;
            },
            WhenValue::Or => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first || second;
            },
            WhenValue::Operand(values) => {
                let mut temp = String::default();

                for value in values.iter() {
                    match value {
                        parser::Value::Literal(s) => {
                            temp.push_str(s.as_str());
                        },
                        parser::Value::Variable(s) => {
                            temp.push_str(s.as_str());
                        },
                        parser::Value::Reference(index) => {
                            let name = format!("x{}", index);
                            let match_str = captures.name(&name).ok_or_else(|| Error::OutOfReferenceIndex(*index))?;
                            temp.push_str(match_str.as_str());
                        },
                    }
                }

                stack.push(temp.clone());
            },
            WhenValue::LikeOperand(values, mode) => {
                let mut temp = String::default();

                if *mode == Mode::Exact || *mode == Mode::Forward {
                    temp.push('^');
                }

                for value in values.iter() {
                    match value {
                        parser::Value::Literal(s) => {
                            temp.push_str(s.as_str());
                        },
                        parser::Value::Variable(s) => {
                            temp.push_str(s.as_str());
                        },
                        parser::Value::Reference(index) => {
                            let name = format!("x{}", index);
                            let match_str = captures.name(&name).ok_or_else(|| Error::OutOfReferenceIndex(*index))?;
                            temp.push_str(match_str.as_str());
                        },
                    }
                }

                if *mode == Mode::Exact || *mode == Mode::Backward {
                    temp.push('$');
                }

                stack.push(temp.clone());
            }
            WhenValue::Original => {
                stack.push(static_reference.get("@@").unwrap().to_string());
            },
            WhenValue::NowForm => {
                stack.push(static_reference.get("@%").unwrap().to_string());
            },
            WhenValue::Equal => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first == second);
            },
            WhenValue::NotEqual => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first != second);
            },
            WhenValue::Not => todo!("Not supported now"),
            WhenValue::Like => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(Regex::new(&second).unwrap().is_match(&first));
            },
        }
    }

    Ok(stack_bool.pop().unwrap_or(false))
}

#[cfg(test)]
mod lib_test {
    use crate::{Data, Error};

    fn execute(words: &Vec<String>, s: &str) -> Vec<Result<String, Error>> {
        match Data::try_from(s) {
            Ok(data) => crate::execute_many(words, &data),
            Err(err) => vec![Err(err)],
        }
    }

    #[test]
    fn default() {
        let words = vec![
            String::from("skea"),
            String::from("sella"),
            String::from("fieties"),
            String::from("liefion"),
            String::from("hioktsufitsa"),
            String::from("pleatten"),
            String::from("flokta"),
            String::from("atsillentsa"),
        ];
        let result = execute(&words, r#"
        -- 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        C = "p" | "t" | "k" | "f"
            -- `|`の前で改行することが可能
            | "s" | "h" | "l" | "y"
        T = "p" | "t" | "k"

        -- その他に`->`,`when`,`and`,`or`の前後で改行することが可能
        ^ "s" "k" V -> "s" @3
        "e" "a"
        | "i" "a"
        ->
            "y" "a"
        "i" V when @2 == "i" or @2 == "e" -> "i" "i"
        V T T V
            when @2 == @3
            -> @1 @2 @4
        "l" "l" V $
            -> "l" @3
        C "l" V | C "l" "y" 
            when @1 /= "l" -> @1 @3
        "t" "s" V
            when
                @0 like @1 @2 "a" $
            and
                @3 /= "i"
            ->
                "s" @3
        -- "t" "s" when not ( @0 like @1 @2 V $ ) -> "s" "s"
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn use_semicolon() {
        let words = vec![
            String::from("skea"),
            String::from("sella"),
            String::from("fieties"),
            String::from("liefion"),
            String::from("hioktsufitsa"),
            String::from("pleatten"),
            String::from("flokta"),
            String::from("atsillentsa"),
        ];
        let result = execute(&words, r#"
        -- 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        C = "p" | "t" | "k" | "f"
            -- `|`の前で改行することが可能
            | "s" | "h" | "l" | "y"; T = "p" | "t" | "k"

        -- セミコロンを使用すると一行に複数のパターンを記述できる
        ^ "s" "k" V -> "s" @3
        "e" "a" | "i" "a" -> "y" "a"; "i" V when @2 == "i" or @2 == "e" -> "i" "i"
        V T T V
            when @2 == @3
            -> @1 @2 @4
        "l" "l" V $
            -> "l" @3 ; C "l" V | C "l" "y" when @1 /= "l" -> @1 @3
        "t" "s" V
            when
                @0 like @1 @2 "a" $
            and
                @3 /= "i"
            ->
                "s" @3;
        -- "t" "s" when not ( @0 like @1 @2 V $ ) -> "s" "s"
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }
}
