#![cfg_attr(feature = "nightly", feature(specialization))]

mod iter;
mod reader;

pub use frunk_core::hlist;
pub use iter::{IntoParserIter, ParserIter};
pub use reader::{Reader, ReaderBuild, ReaderBuildIter, ReaderRead};

use std::{
    fmt,
    io::{self, Read, Seek},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Error<E> {
    Error { error: E, offset: Option<u64> },
    Eof,
}

impl<E: fmt::Display> fmt::Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Error { error, offset } => {
                if let Some(o) = offset {
                    write!(f, "@{}: {}", o, error)
                } else {
                    write!(f, "@???: {}", error)
                }
            }
            Error::Eof => write!(f, "@eof"),
        }
    }
}

impl<E> Error<E> {
    pub fn new<I: Into<E>>(error: I, offset: Option<u64>) -> Self {
        Error::Error {
            error: error.into(),
            offset,
        }
    }
}

/// Helper trait to allow users to _possibly_ get a location in the stream, where
/// types that cannot report this information fall back to reporting `None`.
pub trait MaybePosition {
    fn position(&mut self) -> Option<u64>;
}

#[cfg(feature = "nightly")]
impl<T> MaybePosition for T {
    default fn position(&mut self) -> Option<u64> {
        None
    }
}

impl<T: Seek> MaybePosition for T {
    fn position(&mut self) -> Option<u64> {
        self.seek(std::io::SeekFrom::Current(0)).ok()
    }
}

pub type ParseResult<T, N, E> = Result<(T, N), E>;

pub struct JustResult<T, E> {
    inner: Result<T, E>,
}

impl<T, E> JustResult<T, E> {
    pub fn new(inner: Result<T, E>) -> Self {
        JustResult { inner }
    }
}

impl<T, E> Parser for JustResult<T, E> {
    type Item = T;
    type Next = ();
    type Error = Error<E>;

    fn next<R: Read>(self, _reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        #[cfg(feature = "nightly")]
        {
            self.inner
                .map(|v| (v, ()))
                .map_err(|e| Error::new(e, _reader.position()))
        }
        #[cfg(not(feature = "nightly"))]
        {
            self.inner.map(|v| (v, ())).map_err(|e| Error::new(e, None))
        }
    }
}

pub trait ParseOne: Parser<Next = ()> {
    fn parse<R: Read>(self, reader: &mut R) -> Result<Self::Item, Self::Error>;
}

impl<T> ParseOne for T
where
    T: Parser<Next = ()>,
{
    fn parse<R: Read>(self, reader: &mut R) -> Result<Self::Item, Self::Error> {
        Parser::next(self, reader).map(|(v, ())| v)
    }
}

pub trait IntoParser {
    type Item;
    type Next;
    type Error;
    type Parser: Parser<Item = Self::Item, Next = Self::Next, Error = Self::Error>;

    fn into(self) -> Self::Parser;
}

impl<T> IntoParser for T
where
    T: Parser,
{
    type Item = <Self as Parser>::Item;
    type Next = <Self as Parser>::Next;
    type Error = <Self as Parser>::Error;
    type Parser = Self;

    fn into(self) -> Self::Parser {
        self
    }
}

impl<T, E> IntoParser for Result<T, E> {
    type Item = T;
    type Next = ();
    type Error = Error<E>;
    type Parser = JustResult<T, E>;

    fn into(self) -> Self::Parser {
        JustResult { inner: self }
    }
}

pub trait Parser {
    type Item;
    type Next;
    type Error;

    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error>;
}

macro_rules! tuple_parser {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        #[allow(unused_parens)]
        impl<$first, $($rest,)*> Parser for ((), $first, $($rest,)*)
        where
            ($first $(, $rest)*): Parser
        {
            type Item = <($first $(, $rest)*) as Parser>::Item;
            type Next = <($first $(, $rest)*) as Parser>::Next;
            type Error = <($first $(, $rest)*) as Parser>::Error;

            #[allow(non_snake_case)]
            fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
                let ((), $first, $($rest,)*) = self;
                ($first $(, $rest)*).next(reader)
            }
        }

        impl<$first: Parser, $($rest: Parser<Error = $first::Error>,)*> Parser for ($first, $($rest,)*) {
            type Item = $first::Item;
            type Next = ($first::Next, $($rest,)*);
            type Error = $first::Error;

            #[allow(non_snake_case)]
            fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
                let ($first, $($rest,)*) = self;
                $first.next(reader).map(move |(i, n)| (i, (n, $($rest,)*)))
            }
        }

        tuple_parser!($($rest),*);
    }
}

tuple_parser!(A, B, C, D, E, F, G, H);

pub trait CollectHelper<E> {
    type Item;

    fn collect<R: Read>(self, reader: &mut R) -> Result<Self::Item, E>;
}

impl<E> CollectHelper<E> for () {
    type Item = ();

    fn collect<R: Read>(self, _reader: &mut R) -> Result<Self::Item, E> {
        Ok(())
    }
}

fn make_eof_unexpected<T, E>(r: Result<T, Error<E>>, pos: Option<u64>) -> Result<T, Error<E>>
where
    E: From<io::Error>,
{
    match r {
        Err(Error::Eof) => Err(Error::Error {
            error: io::Error::from(io::ErrorKind::UnexpectedEof).into(),
            offset: pos,
        }),
        other => other,
    }
}

impl<P, E> CollectHelper<P::Error> for P
where
    P: Parser<Error = Error<E>>,
    P::Next: CollectHelper<P::Error>,
    E: From<io::Error>,
{
    type Item = (P::Item, <P::Next as CollectHelper<P::Error>>::Item);

    fn collect<R: Read>(self, reader: &mut R) -> Result<Self::Item, P::Error> {
        let (cur, next) = self.next(reader)?;

        #[cfg(not(feature = "nightly"))]
        let pos = None;
        #[cfg(feature = "nightly")]
        let pos = reader.position();

        // We want to either consume the full reader or nothing at all, so we fail if we
        // hit an EOF halfway through.
        let rest = make_eof_unexpected(next.collect(reader), pos)?;

        Ok((cur, rest))
    }
}
