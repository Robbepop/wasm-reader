use crate::{
    helpers::ParserExt,
    ints::{VarReader, I33},
    Error, Parser, ParserError,
};
use std::io::Read;
use wasmparser::{MemoryImmediate, Type, TypeOrFuncType};

pub trait Parse: Sized {
    type Parser: Parser<Item = Self, Next = ()>;

    fn parser() -> Self::Parser;
}

pub trait ParseExt: Parse {
    fn parse<R: Read>(reader: &mut R) -> Result<Self, <Self::Parser as Parser>::Error>;
}

impl<T> ParseExt for T
where
    T: Parse,
{
    fn parse<R: Read>(reader: &mut R) -> Result<Self, <Self::Parser as Parser>::Error> {
        Self::parser().next(reader).map(|(i, ())| i)
    }
}

impl Parse for TypeOrFuncType {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>
        + wasm_reader_traits::CollectHelper<Error>
        + Clone;

    fn parser() -> Self::Parser {
        use std::convert::TryFrom;

        VarReader::<I33>::new().and_then(|i| {
            if i < 0 {
                let ty = match i {
                    -0x01 => Type::I32,
                    -0x02 => Type::I64,
                    -0x03 => Type::F32,
                    -0x04 => Type::F64,
                    -0x05 => Type::V128,
                    -0x10 => Type::AnyFunc,
                    -0x11 => Type::AnyRef,
                    -0x12 => Type::NullRef,
                    -0x20 => Type::Func,
                    -0x40 => Type::EmptyBlockType,
                    _ => return Err(ParserError::InvalidType),
                };

                Ok(TypeOrFuncType::Type(ty))
            } else {
                u32::try_from(i)
                    .map(TypeOrFuncType::FuncType)
                    .map_err(|_| ParserError::InvalidType)
            }
        })
    }
}

impl Parse for Type {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>
        + wasm_reader_traits::CollectHelper<Error>
        + Clone;

    fn parser() -> Self::Parser {
        VarReader::<I33>::new().and_then(|i| match i {
            -0x01 => Ok(Type::I32),
            -0x02 => Ok(Type::I64),
            -0x03 => Ok(Type::F32),
            -0x04 => Ok(Type::F64),
            -0x05 => Ok(Type::V128),
            -0x10 => Ok(Type::AnyFunc),
            -0x11 => Ok(Type::AnyRef),
            -0x12 => Ok(Type::NullRef),
            -0x20 => Ok(Type::Func),
            -0x40 => Ok(Type::EmptyBlockType),
            _ => Err(ParserError::InvalidType),
        })
    }
}

impl Parse for MemoryImmediate {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>;

    fn parser() -> Self::Parser {
        (VarReader::<u32>::new(), VarReader::<u32>::new())
            .collect()
            .map(|(flags, offset)| MemoryImmediate { flags, offset })
    }
}

impl<T> Parse for T
where
    VarReader<T>: Parser<Item = T, Next = ()>,
{
    type Parser = VarReader<T>;

    fn parser() -> Self::Parser {
        VarReader::<T>::new()
    }
}
