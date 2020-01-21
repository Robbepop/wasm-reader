use crate::{ints::VarReader, Error, ParseResult, Parser};
use std::{io::Read, num::NonZeroU32};

#[derive(Clone, Debug)]
pub struct ManyBuilder<T> {
    inner: T,
}

impl<T> ManyBuilder<T> {
    pub fn new(inner: T) -> Self {
        ManyBuilder { inner }
    }
}

#[derive(Clone, Debug)]
pub struct Many<T> {
    len: NonZeroU32,
    inner: T,
}

impl<T> Many<T> {
    pub fn len(&self) -> u32 {
        self.len.get()
    }
}

impl<T> Parser for ManyBuilder<T>
where
    T: Parser,
{
    type Item = Option<Many<T>>;
    type Next = ();
    type Error = Error;

    #[inline]
    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        match VarReader::<u32>::new().next(reader) {
            Ok((len, ())) => Ok((
                NonZeroU32::new(len).map(|len| Many {
                    len,
                    inner: self.inner,
                }),
                (),
            )),
            Err(e) => Err(e),
        }
    }
}

impl<T> Parser for Many<T>
where
    T: Parser<Next = ()> + Clone,
{
    type Item = T::Item;
    type Next = Option<Many<T>>;
    type Error = T::Error;

    #[inline]
    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        match self.inner.clone().next(reader) {
            Ok((item, ())) => Ok((
                item,
                NonZeroU32::new(self.len.get() - 1).map(|len| Many {
                    len,
                    inner: self.inner,
                }),
            )),
            Err(e) => Err(e),
        }
    }
}
