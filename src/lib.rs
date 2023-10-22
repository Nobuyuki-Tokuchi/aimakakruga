mod error;
mod data;
mod lexer;
mod parser;
mod token;
mod common;

use std::collections::HashMap;
use regex::Regex;

pub use data::Data;
use data::{ConditionValue, Shift, Statement};
pub use error::Error;
use parser::Mode;

pub fn execute(word: &str, data: &Data, name: &str) -> Result<String, Error> {
    let mut static_reference: HashMap<String, String> = HashMap::from_iter(vec![
        (String::from(common::ORIGINAL_KEY), word.into()),
        (String::from(common::NOW_WORD_KEY), word.into()),
    ]);

    let use_statements = match data.get_export_ref(name) {
        Some(export_ref) => export_ref,
        None => return Err(Error::message("")), // TODO: 後でエラー内容を書く
    };

    let mut execute_if_stack = Vec::default();

    for statement in use_statements.iter() {
        if !matches!(statement, Statement::If(_, _) | Statement::Elif(_, _) | Statement::Else(_)) {
            execute_if_stack.pop();
        }

        let now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();
        *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = execute_statement(&now_word, statement, data, &mut static_reference, &mut execute_if_stack)?;
    }

    Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().clone())
}

fn execute_statement(now_word: &str, statement: &Statement, data: &Data, static_reference: &mut HashMap<String, String>, execute_if_stack: &mut Vec<bool>) -> Result<String, Error> {
    match statement {
        Statement::Shift(shift) => {
            execute_shift(now_word, shift, &static_reference)
        },
        Statement::If(condition, local_statements) => {
            if check_if(now_word, condition, static_reference)? {
                for statement in local_statements.iter() {
                    let now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();
                    *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = execute_statement(&now_word, statement, data, static_reference, execute_if_stack)?;
                }
                execute_if_stack.push(false);
            }
            else {
                execute_if_stack.push(true);
            }

            Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string())
        },
        Statement::Elif(condition, local_statements) => {
            if *execute_if_stack.last().unwrap() && check_if(now_word, condition, static_reference)? {
                for statement in local_statements.iter() {
                    let now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();
                    *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = execute_statement(&now_word, statement, data, static_reference, execute_if_stack)?;
                }
            }

            Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string())
        },
        Statement::Else(local_statements) => {
            if execute_if_stack.pop().unwrap() {
                for statement in local_statements.iter() {
                    let now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();
                    *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = execute_statement(&now_word, statement, data, static_reference, execute_if_stack)?;
                }
            }

            Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string())
        },
        Statement::Call(is_private, call_name) => {
            let mut static_reference: HashMap<String, String> = HashMap::from_iter(vec![
                (String::from(common::ORIGINAL_KEY), now_word.into()),
                (String::from(common::NOW_WORD_KEY), now_word.into())
            ]);
            let mut execute_if_stack = Vec::default();
        
            let call_statements = if *is_private {
                match data.get_internal_ref(call_name) {
                    Some(export_ref) => export_ref,
                    None => return Err(Error::not_defined_function("#".to_owned() + call_name)),
                }
            } else {
                match data.get_export_ref(call_name) {
                    Some(export_ref) => export_ref,
                    None => return Err(Error::not_defined_function(call_name)),
                }
            };

            for statement in call_statements.iter() {
                let now_word = static_reference.get(common::NOW_WORD_KEY).unwrap().to_owned();
                *static_reference.get_mut(common::NOW_WORD_KEY).unwrap() = execute_statement(&now_word, statement, data, &mut static_reference, &mut execute_if_stack)?;
            }
        
            Ok(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string())
        },
    }
}

fn execute_shift(now_word: &str, shift: &Shift, static_reference: &HashMap<String, String>) -> Result<String, Error> {
    let mut now_word = String::from(now_word);
    for from in shift.from_list.iter() {
        let regex = Regex::new(from).map_err(|x| Error::ErrorMessage(format!("{}", x), None))?;

        now_word = if let Some(when) = &shift.when {
            let mut temporary = now_word.clone();
            for captures in regex.captures_iter(&now_word) {
                if check_when(&captures, when, &static_reference)? {
                    let mut to_str = String::default();
                    captures.expand(&shift.to, &mut to_str);

                    temporary = temporary.replace(captures.get(0).unwrap().as_str(), &to_str);
                };
            }
            temporary
        }
        else {
            regex.replace_all(&now_word, &shift.to).to_string()
        };
    }

    Ok(now_word)
}

pub fn execute_many(word_list: &[String], data: &Data, name: &str) -> Vec<Result<String, Error>> {
    let mut result_list = Vec::new();

    for word in word_list.iter() {
        result_list.push(execute(word, data, name));
    }

    result_list
}

fn check_when(captures: &regex::Captures, condition: &[ConditionValue], static_reference: &HashMap<String, String>) -> Result<bool, Error> {
    let mut stack = Vec::new();
    let mut stack_bool = Vec::new();

    for value in condition.iter() {
        match value {
            ConditionValue::And => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first && second;
            },
            ConditionValue::Or => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first || second;
            },
            ConditionValue::Operand(values) => {
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
                            let match_str = captures.get(0).ok_or_else(|| Error::invalid("when condition", common::NOW_WORD_KEY))?;
                            temp.push_str(match_str.as_str());
                        },
                        parser::Value::AnyChar => {
                            return Err(Error::invalid("when condition", "."));
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("when condition", "inner"));
                        },
                    }
                }

                stack.push(temp.clone());
            },
            ConditionValue::LikeOperand(values, mode) => {
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
                            let match_str = captures.get(0).ok_or_else(|| Error::invalid("when condition", common::PART_KEY))?;
                            temp.push_str(match_str.as_str());
                        },
                        parser::Value::AnyChar => {
                            temp.push('.');
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("when condition", "inner"));
                        },
                    }
                }

                if *mode == Mode::Exact || *mode == Mode::Backward {
                    temp.push('$');
                }

                stack.push(temp.clone());
            }
            ConditionValue::Original => {
                stack.push(static_reference.get(common::ORIGINAL_KEY).unwrap().to_string());
            },
            ConditionValue::NowForm => {
                stack.push(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string());
            },
            ConditionValue::Part => {
                let match_str = captures.get(0).ok_or_else(|| Error::invalid("when condition", common::PART_KEY))?;
                stack.push(match_str.as_str().to_string());
            },
            ConditionValue::Equal => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }


                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first == second);
            },
            ConditionValue::NotEqual => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first != second);
            },
            ConditionValue::Not => {
                if stack_bool.len() > 0 && stack.len() != 0 {
                    return Err(Error::ErrorMessage("Invalid operation".to_string(), None));
                }

                let not_value = !stack_bool.pop().unwrap();
                stack_bool.push(not_value);
            },
            ConditionValue::Like => {
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

fn check_if(str: &str, condition: &[ConditionValue], static_reference: &HashMap<String, String>) -> Result<bool, Error> {
    let chars: Vec<char> = str.chars().collect();

    let mut stack = Vec::new();
    let mut stack_bool = Vec::new();

    for value in condition.iter() {
        match value {
            ConditionValue::And => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first && second;
            },
            ConditionValue::Or => {
                if stack_bool.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack_bool.pop().unwrap();
                let first = stack_bool.last_mut().unwrap();
                *first = *first || second;
            },
            ConditionValue::Operand(values) => {
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
                            let match_char = chars.get(*index).unwrap_or(&'\0');
                            temp.push(*match_char);
                        },
                        parser::Value::Part => {
                            let match_str = static_reference.get(common::NOW_WORD_KEY).map(|x| x.as_str()).unwrap_or("");
                            temp.push_str(match_str);
                        },
                        parser::Value::AnyChar => {
                            return Err(Error::invalid("if condition", "."));
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("if condition", "inner"));
                        },
                    }
                }

                stack.push(temp.clone());
            },
            ConditionValue::LikeOperand(values, mode) => {
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
                            let match_char = chars.get(*index).unwrap_or(&'\0');
                            temp.push(*match_char);
                        },
                        parser::Value::Part => {
                            let match_str = static_reference.get(common::NOW_WORD_KEY).map(|x| x.as_str()).unwrap_or("");
                            temp.push_str(match_str);
                        },
                        parser::Value::AnyChar => {
                            temp.push('.');
                        },
                        parser::Value::Inner(_)=> {
                            return Err(Error::invalid("if condition", "inner"));
                        },
                    }
                }

                if *mode == Mode::Exact || *mode == Mode::Backward {
                    temp.push('$');
                }

                stack.push(temp.clone());
            }
            ConditionValue::Original => {
                stack.push(static_reference.get(common::ORIGINAL_KEY).unwrap().to_string());
            },
            ConditionValue::NowForm | ConditionValue::Part=> {
                stack.push(static_reference.get(common::NOW_WORD_KEY).unwrap().to_string());
            },
            ConditionValue::Equal => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }


                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first == second);
            },
            ConditionValue::NotEqual => {
                if stack.len() < 2 {
                    return Err(Error::ErrorMessage("Not enough operands".to_string(), None));
                }

                let second = stack.pop().unwrap();
                let first = stack.pop().unwrap();
                stack_bool.push(first != second);
            },
            ConditionValue::Not => {
                if stack_bool.len() > 0 && stack.len() != 0 {
                    return Err(Error::ErrorMessage("Invalid operation".to_string(), None));
                }

                let not_value = !stack_bool.pop().unwrap();
                stack_bool.push(not_value);
            },
            ConditionValue::Like => {
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
    static IS_DEBUG_DATA : bool = true;

    fn execute(words: &Vec<String>, name: &str, s: &str) -> Vec<Result<String, Error>> {
        match Data::try_from(s) {
            Ok(data) => {
                if IS_DEBUG_DATA { println!("{:?}", data); }
                crate::execute_many(words, &data, name)
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
        let result = execute(&words, "main", r#"
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

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn call_function() {
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
        let result = execute(&words, "func1", r#"
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
        let result = execute(&words, "main", r#"
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
        let result = execute(&words, "main", r#"
        [main]
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
        let result = execute(&words, "main", r#"
        [main]
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
            String::from("skea"),
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, "main", r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        "st" V V | "st" V
            when @2 == @3 or @3 == ""
            -> "s" @2
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn if_statement() {
        let words = vec![
            String::from("skea"),
            String::from("skio"),
            String::from("simuno"),
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, "main", r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        }
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn if_else_statement() {
        let words = vec![
            String::from("skea"),
            String::from("skio"),
            String::from("simuno"),
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, "main", r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        } else {
            ("s" | "k") ("i" | "e") V -> @1 "j" @3
        }
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn if_elif_statement() {
        let words = vec![
            String::from("skea"),
            String::from("skio"),
            String::from("simuno"),
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, "main", r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        if @3 == @4 or @4 == "" {
            "st" V V | "st" V -> "s" @2
        } elif @n like "mu" {
            "mu" -> "m"
        }
        "#);

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }

    #[test]
    fn if_elif_else_statement() {
        let words = vec![
            String::from("skea"),
            String::from("skio"),
            String::from("simuno"),
            String::from("staa"),
            String::from("sta"),
        ];
        let result = execute(&words, "main", r#"
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

        let result: Vec<(&String, &Result<String, Error>)> = words.iter().zip(result.iter()).collect();
        println!("{:?}", result);
        assert!(result.iter().all(|(_, x)| x.is_ok()));
    }
}
