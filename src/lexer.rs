use regex::Regex;
use phf::{ Map, phf_map };

const KEYWORDS: Map<&str, KeywordType> = phf_map! {
    "AND" => KeywordType::And,
    "AS" => KeywordType::As,
    "CASE" => KeywordType::Case,
    "EXCEPT" => KeywordType::Except,
    "FROM" => KeywordType::From,
    "SELECT" => KeywordType::Select,
    "THEN" => KeywordType::Then,
    "WHEN" => KeywordType::When,
    "WHERE" => KeywordType::Where,
    "WITH" => KeywordType::With,
};

#[derive(Debug)]
pub enum LexerError {
    InvalidToken(String),
}

#[derive(Clone, Debug, strum_macros::Display, PartialEq)]
pub enum KeywordType {
    And,
    As,
    Case,
    Except,
    From,
    Safe,
    Select,
    Then,
    When,
    Where,
    With,
}

#[derive(Debug)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub token_content: TokenType,
}

#[derive(Debug, Clone, PartialEq, strum_macros::Display)]
pub enum TokenType {
    Keyword(KeywordType, String),
    Identifier(String),
    EOF,
    Equal,
    Integer(i64),
    Float(f64),
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Dot,
    Comma,
    Newline,
    Space,
    Backtick,
    Tab,
    Plus,
    Minus,
}

pub struct LexerContext<'a> {
    content: &'a str,
    position: usize,
    line: usize,
    column: usize,
    keyword_regex: Regex,
    identifier_regex: Regex,
    int_regex: Regex,
    float_regex: Regex,
}

impl<'a> Iterator for LexerContext<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        match token {
            Err(_) => None,
            Ok(token) => match token.token_content {
                TokenType::EOF => None,
                _ => Some(token)
            }
        }
    }
}

impl<'a> LexerContext<'a> {
    pub fn new(content: &str) -> LexerContext {
        return LexerContext {
            content,
            position: 0,
            line: 0,
            column: 0,
            keyword_regex: Regex::new(format!(r"(?i)^\b({})\b",
                KEYWORDS.keys().map(|item| {return format!("({})", item).to_string();})
                    .collect::<Vec<String>>().join("|").as_str()).as_str()).unwrap(),
            identifier_regex: Regex::new(r"^[a-zA-Z_\-]+").unwrap(),
            int_regex: Regex::new(r"^[0-9]+").unwrap(),
            float_regex: Regex::new(r"^[0-9]+\.[0-9]+").unwrap(),
        };
    }

    // Handle popping tokens and iterating for regex values given a length
    fn pop_token(&mut self, length: usize) -> (&str, usize, usize) {
        let rng = (&self.content[self.position..self.position + length], self.line, self.column);

        self.position += length;
        self.column += length;

        return rng;
    }

    fn next_line(&mut self) -> () {
        self.column = 0;
        self.line += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let res = match self.content.chars().nth(self.position) {
            Some(c) => {
                match c {
                    '=' => Some(TokenType::Equal),
                    ',' => Some(TokenType::Comma),
                    '.' => Some(TokenType::Dot),
                    '`' => Some(TokenType::Backtick),
                    '{' => Some(TokenType::OpenCurly),
                    '}' => Some(TokenType::CloseCurly),
                    ' ' => Some(TokenType::Space),
                    '(' => Some(TokenType::OpenParen),
                    ')' => Some(TokenType::CloseParen),
                    '+' => Some(TokenType::Plus),
                    '-' => Some(TokenType::Minus),
                    '\t' => Some(TokenType::Tab),
                    '\n' => Some(TokenType::Newline),
                    _ => None
                }
            },
            None => None
        };

        if let Some(token_content) = res {
            let pos = (self.line, self.column);

            if let TokenType::Newline = token_content {
                self.next_line();
            } else {
                self.column += 1;
            }

            self.position += 1;
            return Ok(Token { line: pos.0, column: pos.1, token_content });
        }

        if let Some(range) = self.keyword_regex.find(&self.content[self.position..]) {
            let token = self.pop_token(range.range().len());
            let token_type = KEYWORDS.get(token.0.to_uppercase().as_str());

            if let None = token_type {
                return Err(LexerError::InvalidToken(String::from("Invalid token provided")));
            }
            
            return Ok(Token {
                token_content: TokenType::Keyword(token_type.unwrap().clone(), token.0.to_string()),
                line: token.1,
                column: token.2,
            });
        }

        if let Some(range) = self.float_regex.find(&self.content[self.position..]) {
            let token = self.pop_token(range.range().len());
            return Ok(Token {
                token_content: TokenType::Float(token.0.parse::<f64>().unwrap()),
                line: token.1,
                column: token.2,
            });
        }

        if let Some(range) = self.int_regex.find(&self.content[self.position..]) {
            let token = self.pop_token(range.range().len());
            return Ok(Token {
                token_content: TokenType::Integer(token.0.parse::<i64>().unwrap()),
                line: token.1,
                column: token.2,
            });
        }

        if let Some(range) = self.identifier_regex.find(&self.content[self.position..]) {
            let token = self.pop_token(range.range().len());
            return Ok(Token {
                token_content: TokenType::Identifier(token.0.to_string()),
                line: token.1,
                column: token.2,
            });
        }

        if self.position == self.content.len() {
            return Ok(Token{line: self.line, column: self.column, token_content: TokenType::EOF});
        }

        Err(LexerError::InvalidToken(String::from("Something")))
    }
}
