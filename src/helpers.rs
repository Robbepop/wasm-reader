use crate::{IntoParser, ParseResult, Parser};
use std::io::Read;
use wasm_reader_traits::CollectHelper;

#[derive(Clone)]
pub struct Map<T, F> {
    inner: T,
    map_func: F,
}

impl<T, F, O> Parser for Map<T, F>
where
    T: Parser,
    F: FnOnce(T::Item) -> O,
{
    type Item = O;
    type Next = T::Next;
    type Error = T::Error;

    #[inline]
    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        let Self { inner, map_func } = self;
        inner.next(reader).map(move |(i, n)| (map_func(i), n))
    }
}

#[derive(Clone)]
pub struct AndThen<T, F> {
    inner: T,
    map_func: F,
}

impl<T, F, O> Parser for AndThen<T, F>
where
    F: FnOnce(T::Item) -> O,
    O: IntoParser,
    T: Parser<Next = (), Error = <O::Parser as Parser>::Error>,
{
    type Item = <O::Parser as Parser>::Item;
    type Next = <O::Parser as Parser>::Next;
    type Error = T::Error;

    #[inline]
    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        let Self { inner, map_func } = self;
        let val = inner.next(reader);
        val.and_then(move |(i, ())| map_func(i).into().next(reader))
    }
}

#[derive(Clone)]
pub struct Collect<T> {
    inner: T,
}

impl<T> Parser for Collect<T>
where
    T: Parser + CollectHelper<<T as Parser>::Error>,
    <T as CollectHelper<<T as Parser>::Error>>::Item: Flatten,
{
    type Item = <<T as CollectHelper<<T as Parser>::Error>>::Item as Flatten>::Flattened;
    type Next = ();
    type Error = T::Error;

    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        self.inner.collect(reader).map(|val| (val.flatten(), ()))
    }
}

pub trait Flatten: Sized {
    type Flattened;

    fn flatten(self) -> Self::Flattened;
}

macro_rules! tuple_flatten {
    () => {};
    (@expand ) => {
        ()
    };
    (@expand $first:ident, $($rest:ident,)*) => {
        ($first, tuple_flatten!(@expand $($rest,)*))
    };
    ($first:ident $(, $rest:ident)*) => {
        impl<$first, $($rest,)*> Flatten for tuple_flatten!(
            @expand $first, $($rest,)*
        ) {
            type Flattened = ($first, $($rest,)*);

            #[allow(non_snake_case)]
            fn flatten(self) -> Self::Flattened {
                let tuple_flatten!(@expand $first, $($rest,)*) = self;
                ($first, $($rest,)*)
            }
        }

        tuple_flatten!($($rest),*);
    }
}

tuple_flatten!(A, B, C, D, E, F, G, H);

pub trait ParserExt: Parser + Sized {
    fn map<O, F: FnOnce(Self::Item) -> O>(self, func: F) -> Map<Self, F>;
    fn and_then<O, F: FnOnce(Self::Item) -> O>(self, func: F) -> AndThen<Self, F>
    where
        O: IntoParser;
    fn collect(self) -> Collect<Self>
    where
        Collect<Self>: Parser;
}

impl<T> ParserExt for T
where
    T: Parser,
{
    fn map<O, F: FnOnce(Self::Item) -> O>(self, func: F) -> Map<Self, F> {
        Map {
            inner: self,
            map_func: func,
        }
    }

    fn and_then<O, F: FnOnce(Self::Item) -> O>(self, func: F) -> AndThen<Self, F>
    where
        O: IntoParser,
    {
        AndThen {
            inner: self,
            map_func: func,
        }
    }

    fn collect(self) -> Collect<Self>
    where
        Collect<Self>: Parser,
    {
        Collect { inner: self }
    }
}
