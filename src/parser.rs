use std::{borrow::Borrow, io::Read, marker::PhantomData};

use crate::ir::{
    Accessor, Context, Operation, PatternResult, RewritePattern, Rewriter, SingleUseRewritePattern,
};

pub struct SourceFileParserPattern<'src, S: Read + 'src> {
    stream: S,
    _phantom: PhantomData<&'src ()>,
}

impl<'src, C: Context, S: Read + 'src> SingleUseRewritePattern<C>
    for SourceFileParserPattern<'src, S>
{
    fn match_and_rewrite(self, accessor: C::Accessor<'_>) -> PatternResult {
        let root_op = accessor.get_root().opaque();
        let rewriter = accessor.rewrite();

        match Parser::<'_, 'src, C, S>::new(self.stream, &rewriter).parse_source_file() {
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

pub struct Parser<'parser, 'src, C: 'parser + Context, S: Read + 'src> {
    stream: S,
    rewriter: &'parser C::Rewriter<'parser>,
    _phantom: PhantomData<&'src ()>,
}

impl<'parser, 'src, C: Context, S: Read + 'src> Parser<'parser, 'src, C, S> {
    pub fn new(stream: S, rewriter: &'parser C::Rewriter<'parser>) -> Self {
        Self {
            stream,
            rewriter,
            _phantom: PhantomData,
        }
    }

    /// Parses an MLIR source file.
    ///
    /// If the source file contains a single operation, returns it. Otherwise,
    /// returns a builtin.module containing the top level operations of the
    /// source file.
    pub fn parse_source_file(&mut self) -> ParseResult<C::OpaqueOperation<'parser>> {
        todo!()
    }
}
