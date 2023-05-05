mod error;
mod data;
mod lexer;
mod parser;
mod token;
mod common;

use std::collections::HashMap;
use regex::Regex;

pub use data::Data;
use data::WhenValue;
pub use error::Error;
use parser::Mode;

pub fn execute(word: &str, data: &Data) -> Result<String, Error> {
    let mut static_reference: HashMap<String, String> = HashMap::from_iter(vec![
        (String::from(common::ORIGINAL_KEY), word.to_owned()),
        (String::from(common::NOW_WORD_KEY), word.to_owned())
    ]);
    let replace_patterns = data.get_replace_patterns_ref();

    for pattern in replace_patterns.iter() {
        let mut now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();

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

        *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = now_word;
    }

    Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().clone())
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
                            let match_str = captures.name(&name).map(|x| x.as_str()).unwrap_or("");
                            temp.push_str(match_str);
                        },
                        parser::Value::Part => {
                            let match_str = captures.get(0).ok_or_else(|| Error::invalid("when expression", common::NOW_WORD_KEY))?;
                            temp.push_str(match_str.as_str());
                        },
                        parser::Value::AnyChar => {
                            return Err(Error::invalid("when expression", "."));
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("when expression", "inner"));
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
                            let match_str = captures.name(&name).map(|x| x.as_str()).unwrap_or("");
                            temp.push_str(match_str);
                        },
                        parser::Value::Part => {
                            let match_str = captures.get(0).ok_or_else(|| Error::invalid("when expression", common::PART_KEY))?;
                            temp.push_str(match_str.as_str());
                        },
                        parser::Value::AnyChar => {
                            temp.push('.');
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("when expression", "inner"));
                        },
                    }
                }

                if *mode == Mode::Exact || *mode == Mode::Backward {
                    temp.push('$');
                }

                stack.push(temp.clone());
            }
            WhenValue::Original => {
                stack.push(static_reference.get(common::ORIGINAL_KEY).unwrap().to_string());
            },
            WhenValue::NowForm => {
                stack.push(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string());
            },
            WhenValue::Part => {
                let match_str = captures.get(0).ok_or_else(|| Error::invalid("when expression", common::PART_KEY))?;
                stack.push(match_str.as_str().to_string());
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
            WhenValue::Not => {
                if stack_bool.len() > 0 && stack.len() != 0 {
                    return Err(Error::ErrorMessage("Invalid operation".to_string(), None));
                }

                let not_value = !stack_bool.pop().unwrap();
                stack_bool.push(not_value);
            },
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
            Ok(data) => {
                println!("{:?}", data);
                crate::execute_many(words, &data)
            },
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
            String::from("hioktsafitsu"),
            String::from("pleatten"),
            String::from("flokta"),
            String::from("kotsnai"),
            String::from("atsillentsa"),
        ];
        let result = execute(&words, r#"
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
            String::from("hioktsafitsu"),
            String::from("pleatten"),
            String::from("flokta"),
            String::from("kotsnai"),
            String::from("atsillentsa"),
        ];
        let result = execute(&words, r#"
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

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }


    #[test]
    fn shift_struct_left_circle() {
        let words = vec![
            String::from("skeasto"),
            String::from("stella"),
        ];
        let result = execute(&words, r#"
        (^ "st" | "sk") ("a" | "e" | "i" | "o" | "u" | "a" "i") -> "s" @2
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
    }

    #[test]
    fn shift_struct_left_circle2() {
        let words = vec![
            String::from("skeasto"),
            String::from("stella"),
        ];
        let result = execute(&words, r#"
        ^ ("st" | "sk") (^ "e" | "o" $) -> "s" @2
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(s, x)| x.is_ok() && s.as_str() == x.as_ref().unwrap().as_str()));
    }

    #[test]
    fn nothing_reference() {
        // 参照が無い場合には空文字列として扱うようにする
        let words = vec![
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, r#"
        V = "a" | "e" | "i" | "o" | "u"
        "st" V V | "st" V
            when @2 == @3 or @3 == ""
            -> "s" @2
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

}
