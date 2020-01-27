use crate::{
    iter::{IntoParserIter, ParserIter},
    Parser,
};
use frunk_core::hlist::{HCons, HNil};
use std::{io::Read, marker::PhantomData};

pub struct Iter<F>(pub F);
pub struct Simple<F>(pub F);

pub trait ReaderRead {
    type Out;
    type Error;

    fn read<R: Read>(self, reader: R) -> Result<Self::Out, Self::Error>;
}

pub struct Reader<P, S, F = HNil> {
    parsers: P,
    funcs: F,
    state: S,
}

impl<P, S> Reader<P, S> {
    #[inline]
    pub fn new(parsers: P, state: S) -> Self {
        Reader {
            parsers: parsers,
            funcs: HNil,
            state,
        }
    }
}

pub trait ReaderReadHelper {
    type Out;
    type Error;

    fn exec<R: Read>(self, reader: R) -> Result<Self::Out, Self::Error>;
}

impl<S, P, F, Rest> ReaderReadHelper for (P, HCons<Simple<F>, Rest>, S)
where
    P: Parser,
    (P::Next, Rest, S): ReaderReadHelper<Out = S, Error = P::Error>,
    F: FnOnce(&mut S, P::Item) -> Result<(), P::Error>,
{
    type Out = S;
    type Error = P::Error;

    #[inline]
    fn exec<R: Read>(self, mut reader: R) -> Result<S, Self::Error> {
        let (
            p,
            HCons {
                head: Simple(f),
                tail,
            },
            mut state,
        ) = self;
        let (item, next) = p.next(&mut reader)?;
        f(&mut state, item)?;
        Ok((next, tail, state).exec(reader)?)
    }
}

impl<S, P, F> ReaderReadHelper for (P, HCons<Simple<F>, HNil>, S)
where
    P: Parser<Next = ()>,
    F: FnOnce(&mut S, P::Item) -> Result<(), P::Error>,
{
    type Out = S;
    type Error = P::Error;

    #[inline]
    fn exec<R: Read>(self, mut reader: R) -> Result<S, Self::Error> {
        let (
            p,
            HCons {
                head: Simple(f),
                tail: HNil,
            },
            mut state,
        ) = self;
        let (item, ()) = p.next(&mut reader)?;
        f(&mut state, item)?;
        Ok(state)
    }
}

impl<S, I, P, F, Rest> ReaderReadHelper for (P, HCons<Iter<F>, Rest>, S)
where
    P: Parser<Item = Option<I>>,
    I: Parser<Next = Option<I>>,
    (P::Next, Rest, S): ReaderReadHelper<Out = S, Error = P::Error>,
    F: FnOnce(&mut S, ParserIter<&mut dyn Read, Option<I>>) -> Result<(), P::Error>,
{
    type Out = S;
    type Error = P::Error;

    #[inline]
    fn exec<R: Read>(self, mut reader: R) -> Result<S, Self::Error> {
        let (
            p,
            HCons {
                head: Iter(f),
                tail,
            },
            mut state,
        ) = self;
        let (item, next) = p.next(&mut reader)?;
        f(&mut state, item.parse_iter(&mut reader))?;
        Ok((next, tail, state).exec(reader)?)
    }
}

impl<PS, FS, S> ReaderRead for Reader<PS, S, FS>
where
    (PS, FS, S): ReaderReadHelper<Out = S>,
{
    type Out = S;
    type Error = <(PS, FS, S) as ReaderReadHelper>::Error;

    #[inline]
    fn read<R: Read>(self, reader: R) -> Result<S, Self::Error> {
        let Reader {
            parsers,
            funcs,
            state,
        } = self;

        (parsers, funcs, state).exec(reader)
    }
}

pub trait ReaderBuildHelper<S, F> {
    type New;

    fn then(self, func: F) -> Self::New;
}

impl<S, P, F, O> ReaderBuildHelper<S, Simple<F>> for (PhantomData<P>, HNil)
where
    P: Parser,
    F: FnOnce(&mut S, P::Item) -> O,
    O: Into<Result<(), P::Error>>,
{
    type New = HCons<Simple<F>, HNil>;

    #[inline]
    fn then(self, func: Simple<F>) -> Self::New {
        HCons {
            head: func,
            tail: HNil,
        }
    }
}

impl<S, P, I, F, O> ReaderBuildHelper<S, Iter<F>> for (PhantomData<P>, HNil)
where
    P: Parser<Item = Option<I>>,
    I: Parser<Next = Option<I>>,
    F: FnOnce(&mut S, ParserIter<&mut dyn Read, Option<I>>) -> O,
    O: Into<Result<(), P::Error>>,
{
    type New = HCons<Iter<F>, HNil>;

    #[inline]
    fn then(self, func: Iter<F>) -> Self::New {
        HCons {
            head: func,
            tail: HNil,
        }
    }
}

impl<S, P, ExistingF, Rest, F> ReaderBuildHelper<S, F> for (PhantomData<P>, HCons<ExistingF, Rest>)
where
    P: Parser,
    (PhantomData<P::Next>, Rest): ReaderBuildHelper<S, F>,
{
    type New = HCons<ExistingF, <(PhantomData<P::Next>, Rest) as ReaderBuildHelper<S, F>>::New>;

    #[inline]
    fn then(self, func: F) -> Self::New {
        let (_, HCons { head, tail }) = self;
        HCons {
            head,
            tail: (PhantomData, tail).then(func),
        }
    }
}

pub trait ReaderBuild<F> {
    type New;

    fn then(self, func: F) -> Self::New;
}

pub trait ReaderBuildIter<F> {
    type New;

    fn then_iter(self, func: F) -> Self::New;
}

impl<PS, FS, F, S> ReaderBuild<F> for Reader<PS, S, FS>
where
    (PhantomData<PS>, FS): ReaderBuildHelper<S, Simple<F>>,
{
    type New = Reader<PS, S, <(PhantomData<PS>, FS) as ReaderBuildHelper<S, Simple<F>>>::New>;

    #[inline]
    fn then(self, func: F) -> Self::New {
        Reader {
            parsers: self.parsers,
            funcs: (PhantomData, self.funcs).then(Simple(func)),
            state: self.state,
        }
    }
}

impl<PS, FS, F, S> ReaderBuildIter<F> for Reader<PS, S, FS>
where
    (PhantomData<PS>, FS): ReaderBuildHelper<S, Iter<F>>,
{
    type New = Reader<PS, S, <(PhantomData<PS>, FS) as ReaderBuildHelper<S, Iter<F>>>::New>;

    #[inline]
    fn then_iter(self, func: F) -> Self::New {
        Reader {
            parsers: self.parsers,
            funcs: (PhantomData, self.funcs).then(Iter(func)),
            state: self.state,
        }
    }
}
