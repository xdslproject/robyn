use std::{
    arch::x86_64,
    borrow::Borrow,
    io::Read,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use lexer::{Lexer, LexerError, LexicalSpan, Punctuation, Token, TokenKind};
use thiserror::Error;

use crate::{
    dialect::builtin::ModuleOp,
    ir::{Accessor, Context, Operation, PatternResult, RewritePattern, Rewriter, SingleUseRewritePattern},
    utils::u8_to_ascii_or_value,
};

mod lexer;

pub struct SourceFileParserPattern<'src> {
    stream: &'src [u8],
}

impl<'src, C: Context> SingleUseRewritePattern<C> for SourceFileParserPattern<'src> {
    fn match_and_rewrite(self, accessor: C::Accessor<'_>) -> PatternResult {
        let root_op = accessor.get_root().opaque();
        let rewriter = accessor.rewrite();

        match Parser::<'_, '_, 'src, C>::new(self.stream, &rewriter).parse_source_file() {
            Ok(op) => {
                rewriter.replace_op(root_op, op);
                PatternResult::Success
            }
            Err(_) => PatternResult::Failure,
        }
    }
}

pub enum ListDelimiter {
    None,
    Paren,
    Square,
    Brace,
    Angle,
}

pub type ParseResult<T> = Result<T, ParserDiagnostic>;

pub struct ParserDiagnostic {
    pub msg: String,
    pub position: usize,
}

impl From<LexerError> for ParserDiagnostic {
    fn from(value: LexerError) -> Self {
        match value {
            LexerError::Unexpected(c, pos) => ParserDiagnostic {
                msg: format!("unexpected character '{}'", u8_to_ascii_or_value(c)),
                position: pos,
            },
            LexerError::MalformedEllipsis(pos) => ParserDiagnostic {
                msg: "unexpected character '.' (malformed ellipsis?)".to_string(),
                position: pos,
            },
            LexerError::ExpectedSuffixIdentifier(pos) => ParserDiagnostic {
                msg: "expected suffix identifier character".to_string(),
                position: pos,
            },
            LexerError::UnclosedStringLiteralAt(pos) => ParserDiagnostic {
                msg: "string literal is never closed".to_string(),
                position: pos,
            },
            LexerError::MalformedFloatExponent(pos) => ParserDiagnostic {
                msg: "malformed float exponent (expected digits)".to_string(),
                position: pos,
            },
        }
    }
}

/// Central utility to parse MLIR source code.
pub struct Parser<'rewriter, 'parsing, 'src, C: 'parsing + Context> {
    source: &'src [u8],
    lexer: Lexer<'src>,

    rewriter: &'rewriter C::Rewriter<'parsing>,
}

impl<'rewriter, 'parsing, 'src, C: Context> Parser<'rewriter, 'parsing, 'src, C> {
    pub fn new(source: &'src [u8], rewriter: &'rewriter C::Rewriter<'parsing>) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),
            rewriter,
        }
    }

    pub fn register_ssa_value() {}

    pub fn parse_opt_keyword(&mut self, keyword: impl AsRef<[u8]>) -> ParseResult<Option<()>> {
        todo!()
    }

    pub fn parse_opt_identifier(&mut self) -> ParseResult<Option<&'src [u8]>> {
        todo!()
    }

    /// Parses an MLIR source file.
    ///
    /// If the source file contains a single operation, returns it. Otherwise,
    /// returns a builtin.module containing the top level operations of the
    /// source file, if any.
    pub fn parse_source_file(&mut self) -> ParseResult<C::OpaqueOperation<'parsing>> {
        let mut operations = Vec::new();
        while let Some(op) = self.parse_opt_operation()? {
            operations.push(op);
        }

        if operations.len() == 1 {
            Ok(operations.pop().unwrap())
        } else {
            Ok(ModuleOp::create::<C>(self.rewriter, &operations))
        }
    }

    /// Parses a single operation.
    pub fn parse_opt_operation(&mut self) -> ParseResult<Option<C::OpaqueOperation<'parsing>>> {
        let tok = self.lexer.lex()?;
        if tok.is_none() {
            return Ok(None);
        }

        todo!()
    }

    /// Parses a list of result values, potentially none.
    /// For each result value, returns the span of the %-identifier and the
    /// size of the value.
    fn parse_result_list(&mut self) -> ParseResult<Vec<(LexicalSpan, usize)>> {
        todo!()
    }

    pub fn parse_comma_separated_list<T>(
        &mut self,
        delimiter: ListDelimiter,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        self.parse_separated_list(delimiter, parser, Punctuation::Comma)
    }

    pub fn parse_separated_list<T>(
        &mut self,
        delimiter: ListDelimiter,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        separator: Punctuation,
    ) -> ParseResult<Vec<T>> {
        let parse_left_delimiter = |parser: &mut Self| match delimiter {
            ListDelimiter::None => Ok(()),
            ListDelimiter::Paren => parser.parse_punctuation(Punctuation::LParen),
            ListDelimiter::Square => parser.parse_punctuation(Punctuation::LSquare),
            ListDelimiter::Brace => parser.parse_punctuation(Punctuation::LBrace),
            ListDelimiter::Angle => parser.parse_punctuation(Punctuation::Less),
        };

        let parse_right_delimiter = |parser: &mut Self| match delimiter {
            ListDelimiter::None => Ok(()),
            ListDelimiter::Paren => parser.parse_punctuation(Punctuation::RParen),
            ListDelimiter::Square => parser.parse_punctuation(Punctuation::RSquare),
            ListDelimiter::Brace => parser.parse_punctuation(Punctuation::RBrace),
            ListDelimiter::Angle => parser.parse_punctuation(Punctuation::Greater),
        };

        let parse_opt_right_delimiter = |parser: &mut Self| match delimiter {
            ListDelimiter::None => Ok(None),
            ListDelimiter::Paren => parser.parse_opt_punctuation(Punctuation::RParen),
            ListDelimiter::Square => parser.parse_opt_punctuation(Punctuation::RSquare),
            ListDelimiter::Brace => parser.parse_opt_punctuation(Punctuation::RBrace),
            ListDelimiter::Angle => parser.parse_opt_punctuation(Punctuation::Greater),
        };

        let mut result = Vec::new();

        parse_left_delimiter(self)?;
        if parse_opt_right_delimiter(self)?.is_some() {
            return Ok(result);
        }

        result.push(parser(self)?);
        while self.parse_opt_punctuation(separator)?.is_some() {
            result.push(parser(self)?);
        }

        parse_right_delimiter(self)?;
        Ok(result)
    }

    pub fn parse_punctuation(&mut self, punctuation: Punctuation) -> ParseResult<()> {
        self.parse_opt_punctuation(punctuation)
            .and_then(|x| x.ok_or_else(|| self.diagnostic_here(format!("expected '{}'", punctuation.as_str()))))
    }

    pub fn parse_opt_punctuation(&mut self, punctuation: Punctuation) -> ParseResult<Option<()>> {
        self.parse_opt_single_token(TokenKind::Punctuation(punctuation))
            .map(|x| x.map(drop))
    }

    fn parse_opt_single_token(&mut self, expected_kind: TokenKind) -> ParseResult<Option<Token>> {
        match self.lexer.peek()? {
            Some(Token { kind, .. }) if kind == expected_kind => Ok(self.lexer.lex()?),
            _ => Ok(None),
        }
    }

    /// Parses a sequence of elements accepted by the provided parser, until None or an error is returned.
    pub fn parse_many<T>(&mut self, f: impl Fn(&mut Self) -> ParseResult<Option<T>>) -> ParseResult<Vec<T>> {
        let mut result = Vec::new();
        while let Some(x) = f(self)? {
            result.push(x);
        }
        ParseResult::Ok(result)
    }

    pub fn diagnostic_here(&self, msg: String) -> ParserDiagnostic {
        ParserDiagnostic {
            msg,
            position: self.lexer.position(),
        }
    }
}
