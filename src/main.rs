use phf::{ Map, phf_map };
use regex::Regex;
use std::fs;
use std::string::ToString;
use strum_macros;

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

#[derive(Clone, Debug, strum_macros::Display, PartialEq)]
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

#[derive(Debug)]
pub struct Token {
	line: usize,
	column: usize,
	token_content: TokenType,
}

#[derive(Debug, Clone, strum_macros::Display)]
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
	OpenParen,
	CloseParen,
	Dot,
	Comma,
	Newline,
	Space,
	Backtick,
	Tab,
}

#[derive(Debug)]
pub struct ColumnSelectorGrammarPart {
	expression: ExpressionGrammarPart,
	as_kw: Option<String>,
	alias: Option<String>
}

#[derive(Debug)]
pub enum LiteralType {
	String,
	Float,
	Integer
}

#[derive(Debug)]
pub enum BooleanOperation {
	Equal,
	NotEqual,
	GreaterThan,
	LessThan,
}

#[derive(Debug)]
pub enum ArithmeticOperation {
	Add,
	Substract,
	Multiply,
	Subtract
}

#[derive(Debug)]
pub struct FromGrammarPart {
	from_kw: String,
}

#[derive(Debug)]
pub struct SelectGrammarPart {
	select_kw: String,
	select_columns: Vec<ColumnSelectorGrammarPart>,
	from: Option<FromGrammarPart>,
}

#[derive(Debug)]
pub struct ExpressionGrammarPart {
	expression: ExpressionType,
	return_type: LiteralType
}

#[derive(Debug)]
pub enum ExpressionType {
	Function,
	SubQuery(Box<SelectGrammarPart>),
	Identifier(String),
	Literal(LiteralType),
	Arithmetic(Box<ExpressionType>, ArithmeticOperation, Box<ExpressionType>),
	Boolean(Box<ExpressionType>, BooleanOperation, Box<ExpressionType>)
}

#[derive(Debug)]
pub enum SQLOperation {
	Select(SelectGrammarPart)
}

#[derive(Debug)]
pub enum ParserError {
	InvalidToken(String),
	OutOfTokens(String),
}

#[derive(Debug)]
pub struct ParserContext {
	tokens: Vec<Token>,
	errors: Vec<ParserError>,
}

impl ParserContext {
	pub fn new(mut tokens: Vec<Token>) -> ParserContext {
		// TODO: deal with this reversing, still more efficient then pop front
		tokens.reverse();

		return ParserContext {
			tokens: tokens,
			errors: Vec::new(),
		};
	}

	pub fn peek(&self, skip_whitespace: bool) -> Result<TokenType, ParserError> {
		let mut i = self.tokens.len() - 1;

		loop {
			if let Some(token) = self.tokens.get(i) {
				match (&token.token_content, skip_whitespace) {
					(TokenType::Newline, true) => {},
					(TokenType::Tab, true) => {},
					(TokenType::Space, true) => {},
					_ => { return Ok(token.token_content.clone()); },
				}
				
			}

			if i == 0 {
				return Err(ParserError::OutOfTokens(String::from("No more tokens available")));
			}

			i -= 1;
		}
	}

	pub fn next(&mut self, skip_whitespace: bool) -> Result<Token, ParserError> {
		loop {
			if let Some(token) = self.tokens.pop() {
				match (&token.token_content, skip_whitespace) {
					(TokenType::Newline, true) => {},
					(TokenType::Tab, true) => {},
					(TokenType::Space, true) => {},
					_ => { return Ok(token); },
				}
			}
		}
	}

	// pub fn parse_sub_query(&mut self) -> Result<GrammarPart, ParserError> {

	// }

	fn parse_select_column(&mut self) -> Result<ColumnSelectorGrammarPart, ParserError> {
		let expr = self.parse_expression()?;

		match self.peek(true)? {
			TokenType::Keyword(KeywordType::As, as_kw) => {
				let _as_token = self.next(true)?;
				let _ident = self.next(true);

				match _ident {
					Ok(token) => {
						if let TokenType::Identifier(ident) = token.token_content {
							// TODO: Build the select column parsed
							Ok(ColumnSelectorGrammarPart { 
								expression: expr,
								as_kw: Some(as_kw),
								alias: Some(ident),
							})
						} else {
							return Err(ParserError::InvalidToken(
								format!("Expected identifier but found {}", token.token_content)
							))
						}
					},
					Err(_) => {
						return Err(ParserError::OutOfTokens(String::from("Unexpected end of stream")));
					}
				}
			},
			TokenType::Identifier(ident) => {
				// TODO: Also build select column parsed
				Ok(ColumnSelectorGrammarPart { 
					expression: expr,
					as_kw: None,
					alias: Some(ident),
				})
			},
			_ => {
				if let ExpressionType::Identifier(_) = expr.expression {} else if true {
					// TODO: can be used to suggest or require the use of a name!
					// This can be configured with rules checked where true is above
				}

				// Is ok to have nothing here and for it to be an error as the end of stream
				Ok(ColumnSelectorGrammarPart {
					expression: expr,
					as_kw: None,
					alias: None,
				})
			}
		}
	}

	/*
		Expression can be:
			* (expression) <- In brackets expression
			* (sub-query) <- Can detect with the select keyword in brackets
			* Function(expression) <- function call identifier before brackets
		
		Grammar:
		expr => 
		term =>
		factor => "(" expr ")" | item
		item => function_call | identifier
		function_call => (identifier | path) "(" expr ")"
	*/
	fn parse_expression(&mut self) -> Result<ExpressionGrammarPart, ParserError> {
		match self.peek(true)? {
			TokenType::Identifier(ident) => {
				self.next(true)?;
				
				Ok(ExpressionGrammarPart {
					expression: ExpressionType::Identifier(ident),
					// TODO: Return type should defs not always be a string
					return_type: LiteralType::String
				})
			}
			_ => return Err(ParserError::InvalidToken(String::from("Invalid token")))
		}
	}

	fn parse_select(&mut self) -> Result<SelectGrammarPart, ParserError> {
		use TokenType::*;
		use KeywordType::*;

		let mut select_columns: Vec<ColumnSelectorGrammarPart> = Vec::new();
		let select_kw = if let Keyword(Select, select_kw) = self.next(true)?.token_content {
			select_kw
		} else {
			return Err(ParserError::InvalidToken(String::from("Expected SELECT token")))
		};
		
		loop {
			select_columns.push(self.parse_select_column()?);
			if let Ok(Comma) = self.peek(true) {
				self.next(true)?;
			} else {
				break;
			}
		}

		let from_gp = match self.peek(true) {
			Ok(TokenType::Keyword(From, from_kw)) => {
				Some(FromGrammarPart { from_kw })
			},
			Ok(t @ _) => {
				let s = "maybe a comma is missing after the previous column";

				return Err(ParserError::InvalidToken(
					format!("Expected FROM but found {} instead; {}", t, s)
				))
			},
			Err(ParserError::OutOfTokens(_)) => {
				None
			},
			Err(err) => {
				return Err(err);
			}
		};

		return Ok(SelectGrammarPart {
			select_kw,
			select_columns,
			from: from_gp
		});
	}

	pub fn parse_main(&mut self) -> Result<SQLOperation, ParserError> {
		match self.peek(true)? {
			TokenType::Keyword(KeywordType::Select, _) => {
				Ok(SQLOperation::Select(self.parse_select()?))
			},
			_ => return Err(ParserError::InvalidToken(String::from("Invalid token")))
		}
	}
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
					' ' => Some(TokenType::Space),
					'(' => Some(TokenType::OpenParen),
					')' => Some(TokenType::CloseParen),
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
				return Err(ParserError::InvalidToken(String::from("Invalid token provided")));
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

		Err(ParserError::InvalidToken(String::from("Something")))
	}
}

fn main() {
	let sql_contents =
		fs::read_to_string("/Users/curtiswhite/Projects/scratch-dbt-parser/src/test.sql")
			.expect("Error");

	let lexer = LexerContext::new(sql_contents.as_str());
	let tokens: Vec<Token> = lexer.collect();
	
	let mut parser_context = ParserContext::new(tokens);

	let res = parser_context.parse_main();
	println!("{:?}", res);
}
