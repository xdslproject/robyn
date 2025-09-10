use std::{
    error::Error,
    fmt,
    io::{Bytes, Read},
    marker::PhantomData,
    ops::Deref,
};

use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Punctuation {
    Arrow,
    Colon,
    Comma,
    Ellipsis,
    Equal,
    Greater,
    LBrace,
    LParen,
    LSquare,
    Less,
    Minus,
    Plus,
    Question,
    RBrace,
    RParen,
    RSquare,
    Star,
    VerticalBar,
    FileMetadataBegin,
    FileMetadataEnd,
}

impl Punctuation {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Punctuation::Arrow => "->",
            Punctuation::Colon => ":",
            Punctuation::Comma => ",",
            Punctuation::Ellipsis => "...",
            Punctuation::Equal => "=",
            Punctuation::Greater => ">",
            Punctuation::LBrace => "{",
            Punctuation::LParen => "(",
            Punctuation::LSquare => "[",
            Punctuation::Less => "<",
            Punctuation::Minus => "-",
            Punctuation::Plus => "+",
            Punctuation::Question => "?",
            Punctuation::RBrace => "}",
            Punctuation::RParen => ")",
            Punctuation::RSquare => "]",
            Punctuation::Star => "*",
            Punctuation::VerticalBar => "|",
            Punctuation::FileMetadataBegin => "{-#",
            Punctuation::FileMetadataEnd => "#-}",
        }
    }

    pub const fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl fmt::Display for Punctuation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    /// bare-id ::= (letter|[_]) (letter|digit|[_$.])*
    BareIdentifier,
    /// at-ident ::= `@` (bare-id | string-literal)
    AtIdentifier,
    /// hash-ident ::= `#` (digit+ | (letter|[$._-]) (letter|[$._-]|digit)*)
    HashIdentifier,
    /// percent-ident ::= `%` (digit+ | (letter|[$._-]) (letter|[$._-]|digit)*)
    PercentIdentifier,
    /// caret-ident ::= `^` (digit+ | (letter|[$._-]) (letter|[$._-]|digit)*)
    CaretIdentifier,
    /// exclamation-ident ::= `!` (digit+ | (letter|[$._-]) (letter|[$._-]|digit)*)
    ExclamationIdentifier,

    IntLit,
    FloatLit,
    StringLit,
    BytesLit,

    Punctuation(Punctuation),
}

/// Represents a span within the source file.
/// The start character is included in the span while the end character is excluded.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LexicalSpan {
    pub start: usize,
    pub end: usize,
}

impl fmt::Display for LexicalSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<(usize, usize)> for LexicalSpan {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: LexicalSpan,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token({:?}, {})", self.kind, self.span)
    }
}

#[derive(Error, Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexerError {
    #[error("unexpected character '{0}' at position {1}")]
    Unexpected(u8, usize),
    #[error("malformed ellipsis at position {0}")]
    MalformedEllipsis(usize),
    #[error("expected suffix identifier at position {0}")]
    ExpectedSuffixIdentifier(usize),
    #[error("string literal opened at {0} is not closed")]
    UnclosedStringLiteralAt(usize),
    #[error("malformed float exponent at position {0}")]
    MalformedFloatExponent(usize),
}

type LexerResult<T> = Result<T, LexerError>;

pub struct Lexer<'src> {
    source: &'src [u8],
    next_position: usize,
    peek_cache: LexerResult<Option<Token>>,
    peek_cache_position: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src [u8]) -> Self {
        let mut lexer = Self {
            source,
            next_position: 0,
            peek_cache: Ok(None),
            peek_cache_position: 0,
        };
        lexer.peek_cache = lexer.lex_next();
        lexer
    }

    /// Return the end of an assumed valid bare identifier starting at
    /// `position` in the source.
    fn find_bare_identifier_end(&self, position: usize) -> usize {
        position
            + 1
            + self.source[(position + 1)..]
                .iter()
                .take_while(|&&x| x.is_ascii_alphanumeric() || x == b'.' || x == b'_' || x == b'$')
                .count()
    }

    /// Return the end position of an assumed valid string literal starting at
    /// `position` in the source (the position of the opening quote). The end
    /// position is the potentially non-existent character after the closing quote.
    /// Returns None if there is no closure to the literal.
    fn find_string_literal_end(&self, position: usize) -> Option<usize> {
        let mut end_position = position + 1;
        while let Some(&c) = self.source.get(end_position) {
            end_position += 1;

            if c == b'\\' {
                // Skip escaped character.
                end_position += 1;
                continue;
            }

            if c == b'"' {
                return Some(end_position);
            }
        }
        None
    }

    /// Skip whitespaces and MLIR comments.
    fn skip_whitespaces(&mut self) {
        let mut comment = false;
        let mut end_position = self.next_position;
        while let Some(&c) = self.source.get(end_position) {
            if c == b'\n' {
                comment = false;
                end_position += 1;
                continue;
            }

            if comment || c.is_ascii_whitespace() {
                end_position += 1;
                continue;
            }

            if c == b'/' && matches!(self.source.get(end_position + 1), Some(b'/')) {
                comment = true;
                end_position += 2; // Skip second `/`.
                continue;
            }

            break;
        }
        self.next_position = end_position;
    }

    fn lex_punctuation(&mut self, kind: Punctuation) -> Token {
        let position = self.next_position;
        self.next_position += kind.len();
        Token {
            kind: TokenKind::Punctuation(kind),
            span: (position, self.next_position).into(),
        }
    }

    /// Assumes next_position is the first character of the number literal.
    fn lex_number(&mut self) -> LexerResult<Token> {
        let start_position = self.next_position;
        let token = if self
            .source
            .get(self.next_position..self.next_position + 3)
            .filter(|x| x[0] == b'0' && x[1] == b'x' && x[2].is_ascii_hexdigit())
            .is_some()
        {
            // Hexadecimal case.
            let hex_start_pos = start_position + 3;
            let end_position = hex_start_pos
                + self.source[hex_start_pos..]
                    .iter()
                    .take_while(|x| x.is_ascii_hexdigit())
                    .count();
            Token {
                kind: TokenKind::IntLit,
                span: (start_position, end_position).into(),
            }
        } else {
            // Decimal and float case.
            let int_part_end_position = start_position
                + self.source[start_position..]
                    .iter()
                    .take_while(|x| x.is_ascii_digit())
                    .count();

            if self.source.get(int_part_end_position) == Some(&b'.') {
                // Float case.
                let mut float_lit_end_position = int_part_end_position + 1;

                // Parse fractional part.
                float_lit_end_position += self.source[float_lit_end_position..]
                    .iter()
                    .take_while(|x| x.is_ascii_digit())
                    .count();

                // Parse optional exponent part.
                let potential_e = self.source.get(float_lit_end_position);
                if potential_e == Some(&b'e') || potential_e == Some(&b'E') {
                    float_lit_end_position += 1;

                    // Parse optional sign.
                    let potential_sign = self.source.get(float_lit_end_position);
                    if potential_sign == Some(&b'+') || potential_sign == Some(&b'-') {
                        float_lit_end_position += 1;
                    }

                    // Parse exponent value.
                    let exponent_length = self.source[float_lit_end_position..]
                        .iter()
                        .take_while(|x| x.is_ascii_digit())
                        .count();
                    if exponent_length == 0 {
                        return Err(LexerError::MalformedFloatExponent(float_lit_end_position));
                    }
                    float_lit_end_position += exponent_length;
                }

                Token {
                    kind: TokenKind::FloatLit,
                    span: (start_position, float_lit_end_position).into(),
                }
            } else {
                // Decimal case.
                Token {
                    kind: TokenKind::IntLit,
                    span: (start_position, int_part_end_position).into(),
                }
            }
        };

        self.next_position = token.span.end;
        Ok(token)
    }

    fn lex_bare_identifier(&mut self) -> Token {
        let position = self.next_position;
        let end_position = self.find_bare_identifier_end(position);
        self.next_position = end_position;
        Token {
            kind: TokenKind::BareIdentifier,
            span: (position, end_position).into(),
        }
    }

    /// Assumes next position is at prefix.
    fn lex_prefixed_identifier(&mut self, kind: TokenKind) -> LexerResult<Token> {
        let position = self.next_position;
        let &identifier_first_character = self
            .source
            .get(self.next_position + 1)
            .ok_or(LexerError::ExpectedSuffixIdentifier(self.next_position + 1))?;
        let end_position = if identifier_first_character.is_ascii_digit() {
            // Lexing number-like identifier
            self.next_position
                + 2
                + self.source[(self.next_position + 2)..]
                    .iter()
                    .copied()
                    .take_while(u8::is_ascii_digit)
                    .count()
        } else if identifier_first_character.is_ascii_alphabetic()
            || identifier_first_character == b'$'
            || identifier_first_character == b'.'
            || identifier_first_character == b'_'
            || identifier_first_character == b'-'
        {
            // Lexing text-like identifier
            self.next_position
                + 2
                + self.source[(self.next_position + 2)..]
                    .iter()
                    .take_while(|&&x| x.is_ascii_alphanumeric() || x == b'.' || x == b'_' || x == b'$' || x == b'-')
                    .count()
        } else {
            return Err(LexerError::ExpectedSuffixIdentifier(self.next_position + 1));
        };
        self.next_position = end_position;
        Ok(Token {
            kind,
            span: (position, end_position).into(),
        })
    }

    fn lex_string_literal(&mut self) -> LexerResult<Token> {
        let start_position = self.next_position;
        let end_position = self
            .find_string_literal_end(start_position)
            .ok_or(LexerError::UnclosedStringLiteralAt(start_position))?;
        self.next_position = end_position;
        Ok(Token {
            kind: TokenKind::StringLit,
            span: (start_position, end_position).into(),
        })
    }

    fn lex_symbol(&mut self) -> LexerResult<Token> {
        if self.source.get(self.next_position + 1) == Some(&b'"') {
            let start_position = self.next_position;
            let end_position = self
                .find_string_literal_end(start_position + 1)
                .ok_or(LexerError::UnclosedStringLiteralAt(start_position))?;
            self.next_position = end_position;
            Ok(Token {
                kind: TokenKind::AtIdentifier,
                span: (start_position, end_position).into(),
            })
        } else {
            self.lex_prefixed_identifier(TokenKind::AtIdentifier)
        }
    }

    fn lex_next(&mut self) -> LexerResult<Option<Token>> {
        self.skip_whitespaces();

        match self.source.get(self.next_position).copied() {
            Some(b':') => Ok(Some(self.lex_punctuation(Punctuation::Colon))),
            Some(b',') => Ok(Some(self.lex_punctuation(Punctuation::Comma))),
            Some(b'(') => Ok(Some(self.lex_punctuation(Punctuation::LParen))),
            Some(b')') => Ok(Some(self.lex_punctuation(Punctuation::RParen))),
            Some(b'}') => Ok(Some(self.lex_punctuation(Punctuation::RBrace))),
            Some(b'[') => Ok(Some(self.lex_punctuation(Punctuation::LSquare))),
            Some(b']') => Ok(Some(self.lex_punctuation(Punctuation::RSquare))),
            Some(b'<') => Ok(Some(self.lex_punctuation(Punctuation::Less))),
            Some(b'>') => Ok(Some(self.lex_punctuation(Punctuation::Greater))),
            Some(b'=') => Ok(Some(self.lex_punctuation(Punctuation::Equal))),
            Some(b'+') => Ok(Some(self.lex_punctuation(Punctuation::Plus))),
            Some(b'*') => Ok(Some(self.lex_punctuation(Punctuation::Star))),
            Some(b'?') => Ok(Some(self.lex_punctuation(Punctuation::Question))),
            Some(b'|') => Ok(Some(self.lex_punctuation(Punctuation::VerticalBar))),
            Some(b'.') => {
                if self.source.get(self.next_position + 1..self.next_position + 3) == Some(b"..") {
                    // Lexing: ...
                    Ok(Some(self.lex_punctuation(Punctuation::Ellipsis)))
                } else {
                    Err(LexerError::MalformedEllipsis(self.next_position))
                }
            }
            Some(b'-') => {
                if self.source.get(self.next_position + 1) == Some(&b'>') {
                    // Lexing: ->
                    Ok(Some(self.lex_punctuation(Punctuation::Arrow)))
                } else {
                    // Lexing: -
                    Ok(Some(self.lex_punctuation(Punctuation::Minus)))
                }
            }
            Some(b'{') => {
                if self.source.get(self.next_position + 1..self.next_position + 3) == Some(b"-#") {
                    // Lexing: {-#
                    Ok(Some(self.lex_punctuation(Punctuation::FileMetadataBegin)))
                } else {
                    // Lexing: {
                    Ok(Some(self.lex_punctuation(Punctuation::LBrace)))
                }
            }
            Some(b'#') => {
                if self.source.get(self.next_position + 1..self.next_position + 3) == Some(b"-}") {
                    // Lexing: #-}
                    Ok(Some(self.lex_punctuation(Punctuation::FileMetadataEnd)))
                } else {
                    // Lexing: hash-ident
                    self.lex_prefixed_identifier(TokenKind::HashIdentifier).map(Some)
                }
            }
            Some(b'!') => self.lex_prefixed_identifier(TokenKind::ExclamationIdentifier).map(Some),
            Some(b'^') => self.lex_prefixed_identifier(TokenKind::CaretIdentifier).map(Some),
            Some(b'%') => self.lex_prefixed_identifier(TokenKind::PercentIdentifier).map(Some),
            Some(b'@') => self.lex_symbol().map(Some),
            Some(b'"') => self.lex_string_literal().map(Some),
            Some(c) if c.is_ascii_digit() => self.lex_number().map(Some),
            Some(c) if c == b'_' || c.is_ascii_alphabetic() => Ok(Some(self.lex_bare_identifier())),
            Some(c) => Err(LexerError::Unexpected(c, self.next_position)),
            None => Ok(None),
        }
    }

    pub fn lex(&mut self) -> LexerResult<Option<Token>> {
        self.peek_cache_position = self.next_position;
        let next_token = self.lex_next();
        std::mem::replace(&mut self.peek_cache, next_token)
    }

    /// Start position of the peekable token in the source text.
    pub fn position(&self) -> usize {
        self.peek_cache_position
    }

    pub fn resume_at(&mut self, position: usize) {
        self.next_position = position;
        self.peek_cache_position = position;
        self.peek_cache = self.lex_next();
    }

    pub fn peek(&self) -> LexerResult<Option<Token>> {
        self.peek_cache
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, LexerError, Punctuation, Token, TokenKind};

    struct LexerIter<'src>(Lexer<'src>);

    impl Iterator for LexerIter<'_> {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.lex().unwrap()
        }
    }

    #[test]
    fn test_string_end() {
        let lexer = Lexer::new(br#""test_hello" hey"#);
        assert!(lexer.find_string_literal_end(0) == Some(12));
        assert!(lexer.find_string_literal_end(11).is_none());

        let lexer = Lexer::new(br#""test\n \" hey""#);
        assert!(lexer.find_string_literal_end(0) == Some(15));

        let lexer = Lexer::new(br#""test_hello"#);
        assert!(lexer.find_string_literal_end(0).is_none());
    }

    #[test]
    fn test_lex_numbers() {
        let mut lexer = Lexer::new(b"123456");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (0, 6).into()
            }))
        );

        let mut lexer = Lexer::new(b"123 456");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (0, 3).into()
            }))
        );
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (4, 7).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.124");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::FloatLit,
                span: (0, 7).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.e23");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::FloatLit,
                span: (0, 7).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.01e23+");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::FloatLit,
                span: (0, 9).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.01e-23");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::FloatLit,
                span: (0, 10).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.01e+23");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::FloatLit,
                span: (0, 10).into()
            }))
        );

        let mut lexer = Lexer::new(b"123.01e");
        assert_eq!(lexer.lex(), Err(LexerError::MalformedFloatExponent(7)));

        let mut lexer = Lexer::new(b"123e23");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (0, 3).into()
            }))
        );
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::BareIdentifier,
                span: (3, 6).into()
            }))
        );

        let mut lexer = Lexer::new(b"0xabcd123");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (0, 9).into()
            }))
        );

        let mut lexer = Lexer::new(b"0xi32");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::IntLit,
                span: (0, 1).into()
            }))
        );
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::BareIdentifier,
                span: (1, 5).into()
            }))
        );
    }

    #[test]
    fn test_lex_literal_ident() {
        let lexer = Lexer::new(br#"ident _someid  "test" @"hel\"lo" @hello"#);
        let expected = [
            Token {
                kind: TokenKind::BareIdentifier,
                span: (0, 5).into(),
            },
            Token {
                kind: TokenKind::BareIdentifier,
                span: (6, 13).into(),
            },
            Token {
                kind: TokenKind::StringLit,
                span: (15, 21).into(),
            },
            Token {
                kind: TokenKind::AtIdentifier,
                span: (22, 32).into(),
            },
            Token {
                kind: TokenKind::AtIdentifier,
                span: (33, 39).into(),
            },
        ];

        itertools::assert_equal(LexerIter(lexer), expected);
    }

    #[test]
    fn test_punctuation() {
        let lexer = Lexer::new(b"(a + %b) - c");
        let expected = [
            Token {
                kind: TokenKind::Punctuation(Punctuation::LParen),
                span: (0, 1).into(),
            },
            Token {
                kind: TokenKind::BareIdentifier,
                span: (1, 2).into(),
            },
            Token {
                kind: TokenKind::Punctuation(Punctuation::Plus),
                span: (3, 4).into(),
            },
            Token {
                kind: TokenKind::PercentIdentifier,
                span: (5, 7).into(),
            },
            Token {
                kind: TokenKind::Punctuation(Punctuation::RParen),
                span: (7, 8).into(),
            },
            Token {
                kind: TokenKind::Punctuation(Punctuation::Minus),
                span: (9, 10).into(),
            },
            Token {
                kind: TokenKind::BareIdentifier,
                span: (11, 12).into(),
            },
        ];

        itertools::assert_equal(LexerIter(lexer), expected);
    }

    #[test]
    fn test_whitespaces() {
        let lexer = Lexer::new(b"foo 	 // hello, world! \nbar");
        let expected = [
            Token {
                kind: TokenKind::BareIdentifier,
                span: (0, 3).into(),
            },
            Token {
                kind: TokenKind::BareIdentifier,
                span: (24, 27).into(),
            },
        ];

        itertools::assert_equal(LexerIter(lexer), expected);
    }

    #[test]
    fn test_weird_identifiers() {
        let mut lexer = Lexer::new(b"^foo$bar.test-dash_under");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::CaretIdentifier,
                span: (0, 24).into()
            }))
        );
        assert!(lexer.lex() == Ok(None));

        let mut lexer = Lexer::new(b"!0123a");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::ExclamationIdentifier,
                span: (0, 5).into()
            }))
        );
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::BareIdentifier,
                span: (5, 6).into()
            }))
        );
        assert!(lexer.lex() == Ok(None));

        let mut lexer = Lexer::new(b"#-_$.");
        assert_eq!(
            lexer.lex(),
            Ok(Some(Token {
                kind: TokenKind::HashIdentifier,
                span: (0, 5).into()
            }))
        );
        assert!(lexer.lex() == Ok(None));
    }
}
