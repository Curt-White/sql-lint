use crate::lexer::{Token, TokenType, KeywordType};
use crate::ast::{ColumnAST, ExpressionAST, ExpressionType, FromAST, OperationAST, Operator, SQLType, SelectAST, SourceAST};

use std::fmt;

use TokenType::*;
use KeywordType::*;

#[derive(Debug)]
pub enum ParserError {
    InvalidToken(String),
    OutOfTokens(String),
}

impl ParserError {
    pub fn throw_invalid_token_err<T>(expected_sequence: &str, line: usize, col: usize, found: T) -> ParserError 
        where T: fmt::Display {

        ParserError::InvalidToken(format!("expected {} @ line {}, column {} but found {}", 
            expected_sequence, line, col, found,
        ))
    }
}

#[derive(Debug)]
pub struct ParserContext {
    pub tokens: Vec<Token>,
    pub errors: Vec<ParserError>,
}

impl ParserContext {
    pub fn new(mut tokens: Vec<Token>) -> ParserContext {
        tokens.reverse();
        return ParserContext { tokens: tokens, errors: Vec::new() };
    }

    pub fn peek(&self, skip_whitespace: bool) -> Result<TokenType, ParserError> {

        for i in (0 .. self.tokens.len()).rev() {
            if let Some(token) = self.tokens.get(i) {
                match (&token.token_content, skip_whitespace) {
                    (TokenType::Newline, true) => {},
                    (TokenType::Tab, true) => {},
                    (TokenType::Space, true) => {},
                    _ => { return Ok(token.token_content.clone()); },
                }
            }
        }
        
        Err(ParserError::OutOfTokens(String::from("No more tokens available")))
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
}

impl ParserContext {
    pub fn parse_expression(&mut self) -> Result<ExpressionAST, ParserError> { 
        let op_stack: Vec<Operator> = Vec::new();
        let expr_stack: Vec<Box<ExpressionAST>> = Vec::new();
        
        loop {
            let next = self.next(true)?;
            match next.token_content {
                Identifier(ident) => return Ok(ExpressionAST {expression: ExpressionType::Identifier(ident), return_type: SQLType::Unresolved }),
                _ => return Err(ParserError::OutOfTokens(String::from("No more tokens available")))
            }
        }
    }

    pub fn parse_as(&mut self) -> Result<(String, String), ParserError> {
        let as_kw = self.next(true)?;
        let alias = self.next(true)?;

        match (as_kw.token_content, alias.token_content) {
            (Keyword(As, as_kw), Identifier(alias)) => Ok((as_kw, alias)),
            (kw, alias) => Err(
                ParserError::throw_invalid_token_err(
                    "AS {{alias}}", as_kw.line, as_kw.column, 
                    format!("{} {}", kw, alias)
                )
            )
        }
    }

    pub fn parse_select_column(&mut self) -> Result<ColumnAST, ParserError> {
        let expression = self.parse_expression()?;

        Ok(match self.peek(true)? {
            Keyword(As, as_kw) => ColumnAST { expression, as_kw: Some(as_kw), alias: Some(self.parse_as()?.1) },
            Identifier(ident) => ColumnAST { expression, as_kw: None, alias: Some(ident) },
            _ => ColumnAST { expression, as_kw: None, alias: None }
        })
    }

    pub fn parse_select(&mut self) -> Result<SelectAST, ParserError> {
        let mut columns: Vec<ColumnAST> = Vec::new();

        let select_token = self.next(true)?;
        let select_kw = match select_token.token_content {
            Keyword(Select, select_kw) => select_kw,
            _ => return Err(ParserError::throw_invalid_token_err(
                "SELECT", select_token.line, select_token.column, 
                select_token.token_content)
            )
        };

        loop {
            columns.push(self.parse_select_column()?);
            if let Ok(Comma) = self.peek(true) {
                self.next(true)?;
            } else {
                break;
            }
        }

        let from_ast = self.parse_from()?;

        return Ok(SelectAST { select_kw, columns, from: from_ast });
    }

    pub fn parse_from(&mut self) -> Result<Option<FromAST>, ParserError> {
        match self.next(true)?.token_content {
            Keyword(From, from_kw) => {
                Ok(Some(FromAST { from_kw, source: SourceAST::Name(String::from("my_table")) }))
            },
            t @ _ => {
                Err(ParserError::InvalidToken(
                    format!("Expected FROM but found {} instead; {}", t, "maybe a comma is missing after the previous column")
                ))
            },
        }
    }

    pub fn parse_operation(&mut self) -> Result<OperationAST, ParserError> {
        match self.peek(true)? {
            Keyword(Select, _) => {
                Ok(OperationAST::Select(self.parse_select()?))
            },
            _ => return Err(ParserError::InvalidToken(String::from("Unsupported operation")))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{ LexerContext, LexerError, Token };
    use crate::parser::ParserContext;

    fn lex_string(query: &str) -> Result<Vec<Token>, LexerError> {
        let lexer = LexerContext::new(query);
        let tokens: Vec<Token> = lexer.collect();
        Ok(tokens)
    }

    #[test]
    fn test_parse_expression() {
        let tokens = lex_string("a = 1 + 2 AND b = 1.0").unwrap();
    }

    #[test]
    fn test_parse_select() {
        let tokens = lex_string(r#"
            SELECT
                a AS aim,
                b AS bim
            FROM `receiptor-bq-prod.my_dataset.my_table`
            WHERE a = 1 AND b = 1.0
        "#).unwrap();

        println!("{:?}", tokens);

        let mut parser_context = ParserContext::new(tokens);
        let output = parser_context.parse_operation();
        println!("{:?}", output);
    }
}
