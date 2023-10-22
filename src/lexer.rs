use crate::token::{Token, TokenType};

#[derive(PartialEq, Copy, Clone)]
enum TokenizeMode {
    Normal,
    String,
    Comment,
}

pub(crate) fn lexer(text: impl Into<String>) -> Vec<Token> {
    let mut temporary = text.into();
    temporary = temporary.replace("\r\n", "\n");
    temporary.push('\n');

    let text = temporary.chars();
    let mut tokens: Vec<Token> = vec![];
    let mut buffer: Vec<char> = vec![];
    let mut mode = TokenizeMode::Normal;
    let mut row: u64 = 1;
    let mut column: u64 = 0;

    for c in text {
        if mode == TokenizeMode::Comment {
            if c == '\n' {
                mode = TokenizeMode::Normal;
                row += 1;
                column = 0;
                continue;
            }
        } else if c.is_ascii_whitespace() {
            if mode == TokenizeMode::String {
                buffer.push(c);
            } else {
                if !buffer.is_empty() {
                    let token = String::from_iter(buffer.iter());
                    tokens.push(get_value(row, column, &token));
                    buffer.clear();
                }

                if c == '\n' {
                    tokens.push(Token::newline(row, column));
                    row += 1;
                    column = 0;
                    continue;
                }
            }
        } else {
            match c {
                '|' | ';' | '^' | '/' | '$' | '(' | ')' | '.' | '[' | ']' | '{' | '}' | '#' => {
                    if !buffer.is_empty() {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(row, column, &token));
                        buffer.clear();
                    }

                    tokens.push(get_tokentype(row, column, c));
                },
                '-' => {
                    if buffer.is_empty() {
                        match tokens.pop() {
                            Some(token) => {
                                if let TokenType::Unknown(value) = &token.tokentype {
                                    if value == "-" {
                                        mode = TokenizeMode::Comment;
                                    } else {
                                        tokens.push(token);
                                        tokens.push(get_tokentype(row, column, c));
                                    }
                                } else {
                                    tokens.push(token);
                                    tokens.push(get_tokentype(row, column, c));
                                }
                            },
                            None => tokens.push(get_tokentype(row, column, c)),
                        }
                    } else {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(row, column, &token));
                        buffer.clear();
    
                        tokens.push(get_tokentype(row, column, c));
                    }
                },
                '"' => {
                    if mode == TokenizeMode::String {
                        mode = TokenizeMode::Normal;
                        buffer.push(c);

                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(row, column, &token));
                        buffer.clear();
                    } else {
                        mode = TokenizeMode::String;
                        buffer.push(c);
                    }
                },
                '=' => {
                    if buffer.is_empty() {
                        match tokens.pop() {
                            Some(token) => {
                                match &token.tokentype {
                                    TokenType::Bind => {
                                        tokens.push(Token::new(token.row, token.column, TokenType::Equal));
                                    },
                                    TokenType::Unknown(value) => {
                                        if value == "/" {
                                            tokens.push(Token::new(token.row, token.column, TokenType::NotEqual));
                                        } else {
                                            tokens.push(token);
                                            tokens.push(get_tokentype(row, column, c));
                                        }
                                    },
                                    _ => {
                                        tokens.push(token);
                                        tokens.push(get_tokentype(row, column, c));
                                    },
                                }
                            },
                            None => tokens.push(get_tokentype(row, column, c)),
                        }
                    } else {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(row, column, &token));
                        buffer.clear();
    
                        tokens.push(get_tokentype(row, column, c));
                    }
                },
                '>' => {
                    if buffer.is_empty() {
                        match tokens.pop() {
                            Some(token) => {
                                match &token.tokentype {
                                    TokenType::Unknown(value) => {
                                        if value == "-" {
                                            tokens.push(Token::new(token.row, token.column, TokenType::RightArrow));
                                        } else {
                                            tokens.push(token);
                                            tokens.push(get_tokentype(row, column, c));
                                        }
                                    },
                                    _ => {
                                        tokens.push(token);
                                        tokens.push(get_tokentype(row, column, c));
                                    },
                                }
                            },
                            None => tokens.push(get_tokentype(row, column, c)),
                        }
                    } else {
                        let token = String::from_iter(buffer.iter());
                        tokens.push(get_value(row, column, &token));
                        buffer.clear();
    
                        tokens.push(get_tokentype(row, column, c));
                    }
                },
                _ => {
                    buffer.push(c);
                }
            }
        }
        column += 1;
    }

    let mut ret_tokens = Vec::with_capacity(tokens.capacity());
    for token in tokens.into_iter() {
        match token.tokentype {
            TokenType::NewLine => {
                match ret_tokens.last() {
                    Some(Token{
                        row: _, column: _,
                        tokentype:
                            TokenType::LeftCircle
                            | TokenType::RightCircle
                            | TokenType::Like
                            | TokenType::LogicalAnd
                            | TokenType::LogicalOr
                            | TokenType::NewLine
                            | TokenType::RightArrow
                            | TokenType::Semicolon
                            | TokenType::When
                            | TokenType::If
                            | TokenType::Elif
                            | TokenType::Else
                            | TokenType::LeftBrace
                            | TokenType::RightBrace
                    }) => (),
                    None => (),
                    _ => ret_tokens.push(token),
                }
            },
            TokenType::LeftCircle
            | TokenType::RightCircle
            | TokenType::Like
            | TokenType::LogicalAnd
            | TokenType::LogicalOr
            | TokenType::RightArrow
            | TokenType::Semicolon
            | TokenType::VerticalBar
            | TokenType::When
            | TokenType::Elif
            | TokenType::Else
            | TokenType::LeftBrace
            | TokenType::RightBrace => {
                if let Some(Token{ row: _, column: _, tokentype: TokenType::NewLine }) = ret_tokens.last() {
                    ret_tokens.pop();
                }

                ret_tokens.push(token);
            },
            _ => ret_tokens.push(token),
        }
    }

    ret_tokens
}

fn get_value(row: u64, column: u64, value: &str) -> Token {
    let column = column - u64::try_from(value.len()).unwrap();

    match value {
        "when" => Token::new(row, column, TokenType::When),
        "and" => Token::new(row, column, TokenType::LogicalAnd),
        "or" => Token::new(row, column, TokenType::LogicalOr),
        "not" => Token::new(row, column, TokenType::LogicalNot),
        "like" => Token::new(row, column, TokenType::Like),
        "if" => Token::new(row, column, TokenType::If),
        "elif" => Token::new(row, column, TokenType::Elif),
        "else" => Token::new(row, column, TokenType::Else),
        "call" => Token::new(row, column, TokenType::Call),
        _ => {
            if value.starts_with("@") {
                if let Some(x) = value.get(1..) {
                    match usize::from_str_radix(x, 10) {
                        Ok(0) => Token::new(row, column, TokenType::Part),
                        Ok(index) => Token::reference(row, column, index),
                        Err(_) => match x {
                            "n" => Token::new(row, column, TokenType::NowForm),
                            "@" => Token::new(row, column, TokenType::Original),
                            _ => Token::unknown(row, column, value),
                        },
                    }
                } else {
                    Token::unknown(row, column, value)
                }
            }
            else if value.starts_with('"') && value.ends_with('"') {
                let len = value.len();
                if len > 2 {
                    Token::value(row, column, &value[1..(value.len() - 1)])
                } else if len == 2 {
                    Token::value(row, column, "")
                } else {
                    Token::unknown(row, column, value)
                }
            } else {
                Token::variable(row, column, value)
            }
        }
    }
}

fn get_tokentype(row: u64, column: u64, value: char) -> Token {
    match value {
        '|' => Token::new(row, column, TokenType::VerticalBar),
        ';' => Token::new(row, column, TokenType::Semicolon),
        '^' => Token::new(row, column, TokenType::Circumflex),
        '=' => Token::new(row, column, TokenType::Bind),
        '$' => Token::new(row, column, TokenType::Dollar),
        '(' => Token::new(row, column, TokenType::LeftCircle),
        ')' => Token::new(row, column, TokenType::RightCircle),
        '.' => Token::new(row, column, TokenType::AnyChar),
        '{' => Token::new(row, column, TokenType::LeftBrace),
        '}' => Token::new(row, column, TokenType::RightBrace),
        '[' => Token::new(row, column, TokenType::LeftBracket),
        ']' => Token::new(row, column, TokenType::RightBracket),
        '#' => Token::new(row, column, TokenType::NumberSign),
        _ => Token::unknown(row, column, value),
    }
}

#[cfg(test)]
mod lexer_test {
    use crate::lexer::*;

    fn execute(s: &str) -> Vec<Token> {
        lexer(s)
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
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
        
        let unknown_tokens: Vec<(usize, &Token)> = result.iter().enumerate().filter(|(_, x)| {
            match x.tokentype {
                TokenType::Unknown(_) => true,
                _ => false
            }
        }).collect();

        println!("{:?}", unknown_tokens);
        assert!(unknown_tokens.is_empty());
    }

    #[test]
    fn elif_statement() {
        let result = execute(r#"
        [main]
        V = "a" | "e" | "i" | "o" | "u"
        elif @n like "mu" {
            "mu" -> "m"
        }
        "#);

        println!("{:?}", result);
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
    }

}
