use std::fmt::Display;
use crate::common;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Unknown(String),
    VerticalBar,
    Bind,
    Circumflex,
    Dollar,
    RightArrow,
    Value(String),
    Variable(String),
    Reference(usize),
    Semicolon,
    NewLine,
    When,
    Equal,
    NotEqual,
    Original,
    Part,
    NowForm,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Like,
    LeftCircle,
    RightCircle,
    AnyChar,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Unknown(token) => write!(f, "{}", token),
            Self::VerticalBar => write!(f, "|"),
            Self::Bind => write!(f, "="),
            Self::Circumflex => write!(f, "^"),
            Self::Dollar => write!(f, "$"),
            Self::RightArrow => write!(f, "->"),
            Self::Value(token) => write!(f, "\"{}\"", token),
            Self::Variable(token) => write!(f, "{}", token),
            Self::Reference(index) => write!(f, "@{}", (index + 1)),
            Self::Semicolon => write!(f, ";"),
            Self::NewLine => write!(f, "(NewLine)"),
            Self::When => write!(f, "when"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "/="),
            Self::Original => write!(f, "{}", common::ORIGINAL_KEY),
            Self::Part => write!(f, "{}", common::PART_KEY),
            Self::NowForm => write!(f, "{}", common::NOW_WORD_KEY),
            Self::LogicalAnd => write!(f, "and"),
            Self::LogicalOr => write!(f, "or"),
            Self::LogicalNot => write!(f, "not"),
            Self::Like => write!(f, "like"),
            Self::LeftCircle => write!(f, "("),
            Self::RightCircle => write!(f, ")"),
            Self::AnyChar => write!(f, "."),
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
enum TokenizeMode {
    Normal,
    String,
    Comment,
}

pub fn lexer(text: &str) -> Vec<TokenType> {
    let text = text.chars();

    let mut tokens: Vec<TokenType> = vec![];
    let mut buffer: Vec<char> = vec![];
    let mut mode = TokenizeMode::Normal;

    for c in text {
        if mode == TokenizeMode::Comment {
            if c == '\r' || c == '\n' {
                mode = TokenizeMode::Normal;
            }
        } else if c.is_ascii_whitespace() {
            if mode == TokenizeMode::String {
                buffer.push(c);
            } else {
                if !buffer.is_empty() {
                    let token = String::from_iter(buffer.iter());
                    tokens.push(get_value(&token));
                    buffer.clear();
                }

                if c == '\r' || c == '\n' {
                    tokens.push(TokenType::NewLine);
                }
            }
        } else {
            match c {
                '|' | ';' | '^' | '/' | '$' | '(' | ')' | '.' => {
                    if !buffer.is_empty() {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(&token));
                        buffer.clear();
                    }

                    tokens.push(get_tokentype(c));
                },
                '-' => {
                    let token = String::from_iter(buffer.iter());
                    if token == "-" {
                        mode = TokenizeMode::Comment;
                        buffer.clear();
                    } else {
                        buffer.push(c);
                    }
                },
                '"' => {
                    if mode == TokenizeMode::String {
                        mode = TokenizeMode::Normal;
                        buffer.push(c);

                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(&token));
                        buffer.clear();
                    } else {
                        mode = TokenizeMode::String;
                        buffer.push(c);
                    }
                },
                '=' => {
                    if buffer.is_empty() {
                        match tokens.pop() {
                            Some(TokenType::Bind) => {
                                tokens.push(TokenType::Equal);
                            },
                            Some(TokenType::Unknown(value)) => {
                                if value == "/" {
                                    tokens.push(TokenType::NotEqual);
                                } else {
                                    tokens.push(TokenType::Unknown(value));
                                    tokens.push(get_tokentype(c));
                                }
                            },
                            Some(other) => {
                                tokens.push(other);
                                tokens.push(get_tokentype(c));
                            },
                            None => tokens.push(get_tokentype(c)),
                        }
                    } else {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(&token));
                        buffer.clear();
    
                        tokens.push(get_tokentype(c));
                    }
                },
                '>' => {
                    let token = String::from_iter(buffer.iter());
                    if token == "-" {
                        tokens.push(TokenType::RightArrow);
                        buffer.clear();
                    } else {
                        if !buffer.is_empty() {
                            tokens.push(get_value(&token));
                            buffer.clear();
                        }

                        tokens.push(get_tokentype(c));
                    }
                },
                _ => {
                    buffer.push(c);
                }
            }
        }
    }

    tokens
}

fn get_value(value: &str) -> TokenType {
    match value {
        "when" => TokenType::When,
        "and" => TokenType::LogicalAnd,
        "or" => TokenType::LogicalOr,
        "not" => TokenType::LogicalNot,
        "like" => TokenType::Like,
        _ => {
            if value.starts_with("@") {
                if let Some(x) = value.get(1..) {
                    match usize::from_str_radix(x, 10) {
                        Ok(0) => TokenType::Part,
                        Ok(index) => TokenType::Reference(index - 1),
                        Err(_) => match x {
                            "n" => TokenType::NowForm,
                            "@" => TokenType::Original,
                            _ => TokenType::Unknown(String::from(value)),
                        },
                    }
                } else {
                    TokenType::Unknown(String::from(value))
                }
            }
            else if value.starts_with('"') && value.ends_with('"') {
                let len = value.len();
                if len > 2 {
                    TokenType::Value(String::from(&value[1..(value.len() - 1)]))
                } else if len == 2 {
                    TokenType::Value(String::default())
                } else {
                    TokenType::Unknown(String::from(value))
                }
            } else {
                TokenType::Variable(String::from(value))
            }
        }
    }
}

fn get_tokentype(value: char) -> TokenType {
    match value {
        '|' => TokenType::VerticalBar,
        ';' => TokenType::Semicolon,
        '^' => TokenType::Circumflex,
        '=' => TokenType::Bind,
        '$' => TokenType::Dollar,
        '(' => TokenType::LeftCircle,
        ')' => TokenType::RightCircle,
        '.' => TokenType::AnyChar,
        _ => TokenType::Unknown(String::from(value)),
    }
}

#[cfg(test)]
mod lexer_test {
    use crate::lexer::*;

    fn execute(s: &str) -> Vec<TokenType> {
        lexer(&s)
    }

    #[test]
    fn default() {
        let result = execute(r#"
        -- 一行コメント
        V = "a" | "e" | "i" | "o" | "u" | "a" "i"
        C = "p" | "t" | "k" | "f"
            -- `|`の前で改行することが可能
            | "s" | "h" | "l" | "y"
        T = "p" | "t" | "k"

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
        
        let unknown_tokens: Vec<(usize, &TokenType)> = result.iter().enumerate().filter(|(_, x)| {
            match x {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
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
        
        let unknown_tokens: Vec<(usize, &TokenType)> = result.iter().enumerate().filter(|(_, x)| {
            match x {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
    }
}
