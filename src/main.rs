// use logos::{Lexer, Logos};
use regex::Regex;
use std::fs;

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

// const KEYWORDS: [(&str, KeywordType); 9] = [
//     ("AS", KeywordType::As),
//     ("CASE", KeywordType::Case),
//     ("EXCEPT", KeywordType::Except),
//     ("FROM", KeywordType::From),
//     ("SELECT", KeywordType::Select),
//     ("THEN", KeywordType::Then),
//     ("WHEN", KeywordType::When),
//     ("WHERE", KeywordType::Where),
//     ("WITH", KeywordType::With),
// ];

// pub fn handle_keyword(lex: &mut Lexer<Token>) -> Keyword {
//     let st = String::from(lex.slice());

//     match st.to_uppercase().as_str() {
//         "SELECT" => Keyword::Select(st),
//         "FROM" => Keyword::From(st),
//         _ => panic!("Invalid keyword"),
//     }
// }

#[derive(Clone, Debug, PartialEq)]
pub enum KeywordType {
	And,
    As,
    Case,
    Except,
    From,
    Select,
    Then,
    When,
    Where,
    With,
}

// #[derive(Logos, Debug, PartialEq)]
// pub enum Token {
//     #[error]
//     Error,

//     #[token("select", handle_keyword)]
//     Keyword(Keyword),
//     // #[regex(r"(?i)((SELECT)|(FROM)|(AS))", handle_keyword)]
//     // Keyword(Keyword),

//     // #[regex(r"[a-zA-Z][a-zA-Z_]*", priority = 2)]
//     // Identifier,
//     #[regex(r"[a-zA-Z][a-zA-Z_-]*", priority = 1)]
//     ExtendedIdentifier,

//     #[token("{")]
//     OpenCurly,

//     #[token("}")]
//     CloseCurly,

//     #[token(".")]
//     Dot,

//     #[token(",")]
//     Comma,

//     #[token("\n")]
//     Newline,

//     #[token(" ")]
//     Space,

//     #[token("`")]
//     Backtick,

//     #[token("\t")]
//     Tab,
// }

#[derive(Debug)]
pub struct Token {
    line: usize,
    column: usize,
    token_content: TokenType,
}

#[derive(Debug)]
pub enum TokenType {
    Keyword(KeywordType, String),
	Identifier(String),
	EOF,
	Equal,
    ExtendedIdentifier(String),
	Integer(i64),
	Float(f64),
    OpenCurly,
    CloseCurly,
    Dot,
    Comma,
    Newline,
    Space,
    Backtick,
    Tab,
}

#[derive(Debug)]
pub enum GrammerPart {
    Select,
}

#[derive(Debug)]
pub enum ParserError {
    InvalidToken(String),
}

#[derive(Debug)]
pub struct ParserContext {
    tokens: Vec<Token>,
    errors: Vec<ParserError>,
}

impl ParserContext {
    pub fn new(tokens: Vec<Token>) -> ParserContext {
        return ParserContext {
            tokens,
            errors: Vec::new(),
        };
    }

    // pub fn next_token(&mut self, skip_whitespace: bool) -> Result<Token, ParserError> {
    //     loop {
    //         match self.tokens.pop() {
    //             None => {
    //                 return Err(ParserError {
    //                     message: String::from("End of token stream"),
    //                 })
    //             }
    //             Some(TokenType::Newline) => {
    //                 continue;
    //             }
    //             Some(TokenType::Tab) => {
    //                 continue;
    //             }
    //             Some(TokenType::Space) => {
    //                 continue;
    //             }
    //             Some(token) => {
    //                 if let TokenType::Newline = token {
    //                     self.line += 1;
    //                 }

    //                 return Ok(token);
    //             }
    //         }
    //     }
    // }

    //     pub fn parse_select(&mut self) -> Result<GrammerPart, ParserError> {
    //         return Ok(GrammerPart::Select);
    //     }

    //     pub fn parse_main(&mut self) -> Result<GrammerPart, ParserError> {
    //         match self.next_token(true)? {
    //             Token::Keyword(Keyword::Select(_)) => self.parse_select(),
    //             _ => {
    //                 return Err(ParserError {
    //                     message: String::from("Unexpected token"),
    //                 })
    //             }
    //         }
    //     }
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

impl<'a> LexerContext<'a> {
    pub fn new(content: &str) -> LexerContext {
        // Map keywords to giant regex
        let keyword_regex = Regex::new(
            format!(
                r"(?i)^\b({})\b",
                KEYWORDS.keys().map(|item| {return format!("({})", item).to_string();})
                    .collect::<Vec<String>>().join("|").as_str()).as_str()).unwrap();

        return LexerContext {
            content,
			position: 0,
			line: 0,
			column: 0,
            keyword_regex,
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

    pub fn next_token(&mut self) -> Result<Token, ParserError> {
		let res = match self.content.chars().nth(self.position) {
			Some(c) => {
				match c {
					'=' => Some(TokenType::Equal),
					',' => Some(TokenType::Comma),
					'.' => Some(TokenType::Dot),
					'`' => Some(TokenType::Backtick),
					'{' => Some(TokenType::OpenCurly),
					'}' => Some(TokenType::CloseCurly),
					' '  => Some(TokenType::Space),
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

		match self.keyword_regex.find(&self.content[self.position..]) {
			Some(range) => {
				let token = self.pop_token(range.range().len());
				let token_type = KEYWORDS.get(token.0.to_uppercase().as_str());

				if let None = token_type {
					return Err(ParserError::InvalidToken(String::from("Invalid token provided")));
				}
				
				return Ok(Token {
					token_content: TokenType::Keyword(token_type.unwrap().clone(), token.0.to_string()),
					line: token.1,
					column: token.2,
				});
			},
			None => {}
		}
		
		match self.int_regex.find(&self.content[self.position..]) {
			Some(range) => {
				let token = self.pop_token(range.range().len());
				return Ok(Token {
					token_content: TokenType::Integer(token.0.parse::<i64>().unwrap()),
					line: token.1,
					column: token.2,
				});
			},
			None => {}
		}

		match self.identifier_regex.find(&self.content[self.position..]) {
			Some(range) => {
				let token = self.pop_token(range.range().len());
				return Ok(Token {
					token_content: TokenType::Identifier(token.0.to_string()),
					line: token.1,
					column: token.2,
				});
			},
			None => {}
		}

		if self.position == self.content.len() {
			return Ok(Token{line: self.line, column: self.column, token_content: TokenType::EOF});
		}

		Err(ParserError::InvalidToken(String::from("Something")))
	}
}

fn main() {
    let sql_contents =
        fs::read_to_string("/Users/curtiswhite/Projects/scratch-dbt-parser/src/test.sql")
            .expect("Error");

	let mut lexer = LexerContext::new(sql_contents.as_str());

	loop {
		match lexer.next_token() {
			Ok(val) => {
				if let TokenType::EOF = val.token_content {
					break;
				}

				println!("Value: {:?}", val);
			},
			Err(err) => {
				println!("Error: {:?}", err);
				break;
			}
		}
	}
}
