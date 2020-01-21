use crate::{Error, ParseResult, Parser, ParserError};
use byteorder::ByteOrder;
use std::{io::Read, marker::PhantomData};
use wasm_reader_traits::MaybePosition;

#[derive(Debug)]
pub struct FixedReader<T> {
    _marker: PhantomData<T>,
}

impl<T> Clone for FixedReader<T> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<T> Default for FixedReader<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> FixedReader<T> {
    pub fn new() -> Self {
        FixedReader {
            _marker: Default::default(),
        }
    }
}

impl<T> Parser for FixedReader<T>
where
    T: FixedIntDecode,
{
    type Item = T;
    type Next = ();
    type Error = Error;

    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        match T::deserialize::<byteorder::LittleEndian, _>(reader) {
            Ok(item) => ParseResult::Ok((item, ())),
            Err(e) => ParseResult::Err(e),
        }
    }
}

pub trait FixedIntDecode: Sized {
    fn deserialize<T: ByteOrder, R: Read>(reader: &mut R) -> Result<Self, Error>;
}

impl FixedIntDecode for bool {
    fn deserialize<T: ByteOrder, R: Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buffer = [0; 1];

        let pos = reader.position();

        let size = reader.read(&mut buffer).map_err(|e| Error::new(e, pos))?;

        if size < 1 {
            return Err(Error::Eof);
        }

        let val = buffer[0];

        match val {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::new(ParserError::InvalidNumber, pos)),
        }
    }
}

impl FixedIntDecode for u8 {
    fn deserialize<T: ByteOrder, R: Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buffer = [0; 1];

        let pos = reader.position();
        let size = reader.read(&mut buffer).map_err(|e| Error::new(e, pos))?;

        if size < 1 {
            return Err(Error::Eof);
        }

        Ok(buffer[0])
    }
}

impl FixedIntDecode for i8 {
    fn deserialize<T: ByteOrder, R: Read>(reader: &mut R) -> Result<Self, Error> {
        let mut buffer = [0; 1];

        let pos = reader.position();
        let size = reader.read(&mut buffer).map_err(|e| Error::new(e, pos))?;

        if size < 1 {
            return Err(Error::Eof);
        }

        Ok(buffer[0] as i8)
    }
}

macro_rules! impl_byteorder_trait {
    ($(($int_name:ident, $byteorder_method_name:ident, $size:expr),)*) => {
        $(
            impl FixedIntDecode for $int_name {
                fn deserialize<T: ByteOrder, R: Read>(reader: &mut R) -> Result<Self, Error> {
                    let mut buffer = [0; $size];

                    let pos = reader.position();
                    let size = reader.read(&mut buffer).map_err(|e| Error::new(e, pos))?;

                    if size == 0 {
                        return Err(Error::Eof);
                    } else if size < $size {
                        return Err(Error::Eof);
                    }

                    Ok(T::$byteorder_method_name(&buffer))
                }
            }
        )*
    }
}

impl_byteorder_trait! {
    (u16, read_u16, 2),
    (i16, read_i16, 2),
    (u32, read_u32, 4),
    (i32, read_i32, 4),
    (u64, read_u64, 8),
    (i64, read_i64, 8),
    (u128, read_u128, 16),
    (i128, read_i128, 16),
}

pub enum I33 {}
pub enum U7 {}
pub enum I7 {}

#[derive(Debug)]
pub struct VarReader<T> {
    _marker: PhantomData<T>,
}

impl<T> Clone for VarReader<T> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<T> Default for VarReader<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> VarReader<T> {
    pub fn new() -> Self {
        VarReader {
            _marker: Default::default(),
        }
    }
}

impl<T> Parser for VarReader<T>
where
    T: VarIntDecode,
{
    type Item = T::Output;
    type Next = ();
    type Error = Error;

    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        match T::deserialize(reader) {
            Ok(item) => ParseResult::Ok((item, ())),
            Err(e) => ParseResult::Err(e),
        }
    }
}

// This is the "continuation bit" - i.e. the marker that says whether we need to continue or not
const MARKER_BIT: u8 = 0b1000_0000;

pub trait VarIntDecode {
    type Output;

    fn deserialize<R: Read>(reader: &mut R) -> Result<Self::Output, Error>;
}

const fn ceil_div(a: usize, b: usize) -> usize {
    1 + ((a - 1) / b)
}

macro_rules! varint_unsigned {
    ($(($marker:ident, $bits:expr, $output:ident),)*) => {
        $(
            impl VarIntDecode for $marker {
                type Output = $output;

                fn deserialize<R: Read>(reader: &mut R) -> Result<Self::Output, Error> {
                    let pos = reader.position();
                    let mut bytes = reader.bytes().take(ceil_div($bits, 7));
                    let out = bytes
                        .next()
                        .ok_or_else(|| Error::Eof)?
                        .map_err(|e| Error::new(e, pos))?;
                    let (marker, mut out) = (out & MARKER_BIT, (out & !MARKER_BIT) as Self::Output);

                    if marker == 0 {
                        return Ok(out);
                    }

                    let mut shift: u8 = 7;

                    for i in bytes {
                        let i = i.map_err(|e| Error::new(e, pos))?;
                        let (marker, i) = (i & MARKER_BIT, (i & !MARKER_BIT) as Self::Output);
                        out |= i << shift;
                        shift += 7;

                        if marker == 0 {
                            return Ok(out);
                        }
                    }

                    Err(Error::new(ParserError::InvalidNumber, pos))
                }
            }
        )*
    }
}

macro_rules! varint_signed {
    ($(($marker:ident, $bits:expr, $output:ident),)*) => {
        $(
            impl VarIntDecode for $marker {
                type Output = $output;

                // TODO: Extend the last sign bit
                fn deserialize<R: Read>(reader: &mut R) -> Result<Self::Output, Error> {
                    let pos = reader.position();
                    let mut bytes = reader.bytes().take(ceil_div($bits, 7));
                    let out = bytes
                        .next()
                        .ok_or_else(|| Error::Eof)?
                        .map_err(|e| Error::new(e, pos))?;

                    let (marker, mut out) = (out & MARKER_BIT, (out & !MARKER_BIT) as Self::Output);

                    if marker == 0 {
                        let out = out as i8;
                        return Ok(((out << 1) >> 1) as Self::Output);
                    }

                    let mut shift: u8 = 7;

                    for i in bytes {
                        let i = i.map_err(|e| Error::new(e, pos))?;
                        let (marker, i) = (i & MARKER_BIT, i & !MARKER_BIT);
                        out |= (i as Self::Output) << shift;

                        shift += 7;

                        if marker == 0 {
                            let total_outputs_bits = std::mem::size_of::<Self::Output>() * 8;
                            let correct_sign_shift = total_outputs_bits.saturating_sub(shift as usize);

                            return Ok((out << correct_sign_shift) >> correct_sign_shift);
                        }
                    }

                    Err(Error::new(ParserError::InvalidNumber, pos))
                }
            }
        )*
    }
}

varint_unsigned! {
    (u32, 32, u32),
    (u64, 64, u64),
    (U7, 7, u8),
}

varint_signed! {
    (i32, 32, i32),
    (i64, 64, i64),
    (I7, 7, i8),
    (I33, 33, i64),
}

#[cfg(test)]
mod test {
    use crate::ints::{I33, U7};
    use quickcheck_macros::quickcheck;

    macro_rules! varint_test {
        ($name:ident, $ty:ty, $method_name:ident) => {
            #[quickcheck]
            fn $name(bytes: Vec<u8>) -> quickcheck::TestResult {
                use crate::ParseOne;
                use wasmparser::BinaryReader;

                let mut read = BinaryReader::new(&bytes);

                quickcheck::TestResult::from_bool(
                    read.$method_name()
                        .ok()
                        .map(|i| i as <$ty as super::VarIntDecode>::Output)
                        == super::VarReader::<$ty>::new().parse(&mut &bytes[..]).ok(),
                )
            }
        };
    }

    varint_test!(decode_u32, u32, read_var_u32);
    varint_test!(decode_u64, u64, read_var_u32);
    varint_test!(decode_u7, U7, read_var_u32);
    varint_test!(decode_i32, i32, read_var_i32);
    varint_test!(decode_i64, i64, read_var_i64);
    varint_test!(decode_i33, I33, read_var_s33);
}
