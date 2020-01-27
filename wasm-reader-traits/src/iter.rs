use crate::Parser;
use std::io::Read;

pub struct ParserIter<R, P> {
    reader: R,
    parser: P,
}

impl<R, P> ParserIter<R, P> {
    pub fn new(reader: R, parser: P) -> Self {
        ParserIter { reader, parser }
    }
}

impl<R, P> Iterator for ParserIter<R, Option<P>>
where
    R: Read,
    P: Parser<Next = Option<P>>,
{
    type Item = Result<P::Item, P::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (val, next) = match self.parser.take()?.next(&mut self.reader) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };

        self.parser = next;

        Some(Ok(val))
    }
}

impl<R, P> Iterator for ParserIter<R, &'_ mut Option<P>>
where
    R: Read,
    P: Parser<Next = Option<P>>,
{
    type Item = Result<P::Item, P::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (val, next) = match self.parser.take()?.next(&mut self.reader) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };

        *self.parser = next;

        Some(Ok(val))
    }
}

pub trait IntoParserIter: Sized {
    type Item;
    type Error;

    fn parse_iter<R: Read>(self, reader: R) -> ParserIter<R, Self>
    where
        ParserIter<R, Self>: Iterator<Item = Result<Self::Item, Self::Error>>;
}

impl<T> IntoParserIter for Option<T>
where
    T: Parser<Next = Option<T>>,
{
    type Item = T::Item;
    type Error = T::Error;

    fn parse_iter<R: Read>(self, reader: R) -> ParserIter<R, Self> {
        ParserIter::new(reader, self)
    }
}

impl<T> IntoParserIter for &'_ mut Option<T>
where
    T: Parser<Next = Option<T>>,
{
    type Item = T::Item;
    type Error = T::Error;

    fn parse_iter<R: Read>(self, reader: R) -> ParserIter<R, Self> {
        ParserIter::new(reader, self)
    }
}
