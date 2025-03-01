use std::{
    arch::x86_64,
    borrow::Borrow,
    io::Read,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::ir::{
    Accessor, Context, Operation, PatternResult, RewritePattern, Rewriter, SingleUseRewritePattern,
};

mod lexer;

pub struct SourceFileParserPattern<'src> {
    stream: &'src [u8],
}

impl<'src, C: Context> SingleUseRewritePattern<C> for SourceFileParserPattern<'src> {
    fn match_and_rewrite(self, accessor: C::Accessor<'_>) -> PatternResult {
        let root_op = accessor.get_root().opaque();
        let rewriter = accessor.rewrite();

        match ParserEngine::<'_, '_, 'src, C>::new(self.stream, &rewriter).parse_source_file() {
            Ok(op) => {
                rewriter.replace_op(root_op, op);
                PatternResult::Success
            }
            Err(_) => PatternResult::Failure,
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct ParseError {}

/// Central utility to parse MLIR source code.
pub struct ParserEngine<'rewriter, 'parsing, 'src, C: 'parsing + Context> {
    stream: &'src [u8],

    rewriter: &'rewriter C::Rewriter<'parsing>,
    _phantom: PhantomData<&'src ()>,
}

pub enum ParserDelimiter {
    None,
    Paren,
    Square,
    Curly,
    Angle,
}

impl<'rewriter, 'parsing, 'src, C: Context> ParserEngine<'rewriter, 'parsing, 'src, C> {
    pub fn new(stream: &'src [u8], rewriter: &'rewriter C::Rewriter<'parsing>) -> Self {
        Self {
            stream,
            rewriter,
            _phantom: PhantomData,
        }
    }

    pub fn parse_opt_keyword(&mut self, keyword: &'static [u8]) -> ParseResult<Option<()>> {
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
        todo!()
    }

    /// Parses a single operation.
    pub fn parse_opt_operation(&mut self) -> ParseResult<Option<C::OpaqueOperation<'parsing>>> {
        todo!()
    }

    /// Parses a sequence of elements accepted by the provided parser, until None or an error is returned.
    pub fn parse_many<T>(
        &mut self,
        f: impl Fn(&mut Self) -> ParseResult<Option<T>>,
    ) -> ParseResult<Vec<T>> {
        let mut result = Vec::new();
        while let Some(x) = f(self)? {
            result.push(x);
        }
        ParseResult::Ok(result)
    }
}
