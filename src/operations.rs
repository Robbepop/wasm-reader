use crate::{
    helpers::{Collect, ParserExt},
    ints::{FixedReader, VarReader},
    parse::Parse,
    vec::{Many, ManyBuilder},
    Error, JustResult, ParseOne, ParseResult, Parser, ParserError,
};
use auto_enums::auto_enum;
use std::{fmt, io::Read};
use wasm_reader_traits::ParserIter;
use wasmparser::{MemoryImmediate, Type, TypeOrFuncType};

type LocalParser = Many<Collect<(VarReader<u32>, <Type as Parse>::Parser)>>;

pub struct FunctionBody {
    locals: Option<LocalParser>,
}

impl FunctionBody {
    pub fn new<R: Read>(reader: &mut R) -> Result<Self, Error> {
        let locals =
            ManyBuilder::new((VarReader::<u32>::new(), Type::parser()).collect()).parse(reader)?;

        Ok(FunctionBody { locals })
    }

    pub fn into_locals_and_code<'a, R: Read>(
        self,
        reader: &'a mut R,
    ) -> (
        impl Iterator<Item = Result<(u32, Type), Error>> + 'a,
        OperatorsReader,
    ) {
        (ParserIter::new(reader, self.locals), OperatorsReader::new())
    }
}

pub struct OperatorsReader {
    parser: <Operator as Parse>::Parser,
}

impl OperatorsReader {
    fn new() -> Self {
        OperatorsReader {
            parser: Operator::parser(),
        }
    }

    pub fn next<R: Read>(&mut self, reader: &mut R) -> Option<Result<Operator, Error>> {
        match self.parser.clone().next(reader) {
            Ok((val, ())) => Some(Ok(val)),
            Err(e) => match e {
                wasm_reader_traits::Error::Eof => None,
                other => Some(Err(other)),
            },
        }
    }
}

pub struct BrTable {
    inner: (Option<Many<VarReader<u32>>>, VarReader<u32>),
}

impl fmt::Debug for BrTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BrTable {{ .. }}")
    }
}

impl BrTable {
    pub fn targets_and_default<R: Read>(
        self,
        reader: &mut R,
    ) -> (
        impl Iterator<Item = Result<u32, Error>> + '_,
        impl ParseOne<Item = u32, Error = Error>,
    ) {
        (ParserIter::new(reader, self.inner.0), self.inner.1)
    }
}

impl Parse for BrTable {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>;

    fn parser() -> Self::Parser {
        ManyBuilder::new(VarReader::<u32>::new()).map(|many| BrTable {
            inner: (many, VarReader::new()),
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ieee32(pub u32);
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ieee64(pub u64);

impl Parse for Ieee32 {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>;

    fn parser() -> Self::Parser {
        FixedReader::<u32>::new().map(Ieee32)
    }
}

impl Parse for Ieee64 {
    type Parser = impl Parser<Item = Self, Next = (), Error = Error>;

    fn parser() -> Self::Parser {
        FixedReader::<u64>::new().map(Ieee64)
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub enum Operator {
    Unreachable,
    Nop,
    Block { ty: TypeOrFuncType },
    Loop { ty: TypeOrFuncType },
    If { ty: TypeOrFuncType },
    Else,
    End,
    Br { relative_depth: u32 },
    BrIf { relative_depth: u32 },
    BrTable { table: BrTable },
    Return,
    Call { function_index: u32 },
    CallIndirect { index: u32, table_index: u32 },
    Drop,
    Select,
    LocalGet { local_index: u32 },
    LocalSet { local_index: u32 },
    LocalTee { local_index: u32 },
    GlobalGet { global_index: u32 },
    GlobalSet { global_index: u32 },
    I32Load { memarg: MemoryImmediate },
    I64Load { memarg: MemoryImmediate },
    F32Load { memarg: MemoryImmediate },
    F64Load { memarg: MemoryImmediate },
    I32Load8S { memarg: MemoryImmediate },
    I32Load8U { memarg: MemoryImmediate },
    I32Load16S { memarg: MemoryImmediate },
    I32Load16U { memarg: MemoryImmediate },
    I64Load8S { memarg: MemoryImmediate },
    I64Load8U { memarg: MemoryImmediate },
    I64Load16S { memarg: MemoryImmediate },
    I64Load16U { memarg: MemoryImmediate },
    I64Load32S { memarg: MemoryImmediate },
    I64Load32U { memarg: MemoryImmediate },
    I32Store { memarg: MemoryImmediate },
    I64Store { memarg: MemoryImmediate },
    F32Store { memarg: MemoryImmediate },
    F64Store { memarg: MemoryImmediate },
    I32Store8 { memarg: MemoryImmediate },
    I32Store16 { memarg: MemoryImmediate },
    I64Store8 { memarg: MemoryImmediate },
    I64Store16 { memarg: MemoryImmediate },
    I64Store32 { memarg: MemoryImmediate },
    MemorySize { reserved: u32 },
    MemoryGrow { reserved: u32 },
    I32Const { value: i32 },
    I64Const { value: i64 },
    F32Const { value: Ieee32 },
    F64Const { value: Ieee64 },
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    RefNull,
    RefIsNull,
    RefFunc { function_index: u32 },

    // 0xFC operators
    // Non-trapping Float-to-int Conversions
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,

    // 0xFC operators
    // bulk memory https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
    MemoryInit { segment: u32 },
    DataDrop { segment: u32 },
    MemoryCopy,
    MemoryFill,
    TableInit { segment: u32, table: u32 },
    ElemDrop { segment: u32 },
    TableCopy { dst_table: u32, src_table: u32 },
    TableFill { table: u32 },
    TableGet { table: u32 },
    TableSet { table: u32 },
    TableGrow { table: u32 },
    TableSize { table: u32 },

    // 0xFE operators
    // https://github.com/WebAssembly/threads/blob/master/proposals/threads/Overview.md
    AtomicNotify { memarg: MemoryImmediate },
    I32AtomicWait { memarg: MemoryImmediate },
    I64AtomicWait { memarg: MemoryImmediate },
    AtomicFence { flags: u8 },
    I32AtomicLoad { memarg: MemoryImmediate },
    I64AtomicLoad { memarg: MemoryImmediate },
    I32AtomicLoad8U { memarg: MemoryImmediate },
    I32AtomicLoad16U { memarg: MemoryImmediate },
    I64AtomicLoad8U { memarg: MemoryImmediate },
    I64AtomicLoad16U { memarg: MemoryImmediate },
    I64AtomicLoad32U { memarg: MemoryImmediate },
    I32AtomicStore { memarg: MemoryImmediate },
    I64AtomicStore { memarg: MemoryImmediate },
    I32AtomicStore8 { memarg: MemoryImmediate },
    I32AtomicStore16 { memarg: MemoryImmediate },
    I64AtomicStore8 { memarg: MemoryImmediate },
    I64AtomicStore16 { memarg: MemoryImmediate },
    I64AtomicStore32 { memarg: MemoryImmediate },
    I32AtomicRmwAdd { memarg: MemoryImmediate },
    I64AtomicRmwAdd { memarg: MemoryImmediate },
    I32AtomicRmw8AddU { memarg: MemoryImmediate },
    I32AtomicRmw16AddU { memarg: MemoryImmediate },
    I64AtomicRmw8AddU { memarg: MemoryImmediate },
    I64AtomicRmw16AddU { memarg: MemoryImmediate },
    I64AtomicRmw32AddU { memarg: MemoryImmediate },
    I32AtomicRmwSub { memarg: MemoryImmediate },
    I64AtomicRmwSub { memarg: MemoryImmediate },
    I32AtomicRmw8SubU { memarg: MemoryImmediate },
    I32AtomicRmw16SubU { memarg: MemoryImmediate },
    I64AtomicRmw8SubU { memarg: MemoryImmediate },
    I64AtomicRmw16SubU { memarg: MemoryImmediate },
    I64AtomicRmw32SubU { memarg: MemoryImmediate },
    I32AtomicRmwAnd { memarg: MemoryImmediate },
    I64AtomicRmwAnd { memarg: MemoryImmediate },
    I32AtomicRmw8AndU { memarg: MemoryImmediate },
    I32AtomicRmw16AndU { memarg: MemoryImmediate },
    I64AtomicRmw8AndU { memarg: MemoryImmediate },
    I64AtomicRmw16AndU { memarg: MemoryImmediate },
    I64AtomicRmw32AndU { memarg: MemoryImmediate },
    I32AtomicRmwOr { memarg: MemoryImmediate },
    I64AtomicRmwOr { memarg: MemoryImmediate },
    I32AtomicRmw8OrU { memarg: MemoryImmediate },
    I32AtomicRmw16OrU { memarg: MemoryImmediate },
    I64AtomicRmw8OrU { memarg: MemoryImmediate },
    I64AtomicRmw16OrU { memarg: MemoryImmediate },
    I64AtomicRmw32OrU { memarg: MemoryImmediate },
    I32AtomicRmwXor { memarg: MemoryImmediate },
    I64AtomicRmwXor { memarg: MemoryImmediate },
    I32AtomicRmw8XorU { memarg: MemoryImmediate },
    I32AtomicRmw16XorU { memarg: MemoryImmediate },
    I64AtomicRmw8XorU { memarg: MemoryImmediate },
    I64AtomicRmw16XorU { memarg: MemoryImmediate },
    I64AtomicRmw32XorU { memarg: MemoryImmediate },
    I32AtomicRmwXchg { memarg: MemoryImmediate },
    I64AtomicRmwXchg { memarg: MemoryImmediate },
    I32AtomicRmw8XchgU { memarg: MemoryImmediate },
    I32AtomicRmw16XchgU { memarg: MemoryImmediate },
    I64AtomicRmw8XchgU { memarg: MemoryImmediate },
    I64AtomicRmw16XchgU { memarg: MemoryImmediate },
    I64AtomicRmw32XchgU { memarg: MemoryImmediate },
    I32AtomicRmwCmpxchg { memarg: MemoryImmediate },
    I64AtomicRmwCmpxchg { memarg: MemoryImmediate },
    I32AtomicRmw8CmpxchgU { memarg: MemoryImmediate },
    I32AtomicRmw16CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw8CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw16CmpxchgU { memarg: MemoryImmediate },
    I64AtomicRmw32CmpxchgU { memarg: MemoryImmediate },
}

struct AlignedMemargParser(pub u32);

impl Parser for AlignedMemargParser {
    type Item = MemoryImmediate;
    type Next = ();
    type Error = Error;

    fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error> {
        use crate::MaybePosition as _;

        let pos = reader.position();
        MemoryImmediate::parser()
            .next(reader)
            .and_then(|(imm, ())| {
                if imm.flags > self.0 {
                    Err(Error::new(ParserError::MisalignedMemoryImmediate, pos))
                } else {
                    Ok((imm, ()))
                }
            })
    }
}

impl Parse for Operator {
    type Parser = impl_parser_hack::OpParser;

    fn parser() -> Self::Parser {
        impl_parser_hack::parser()
    }
}

// Workaround for compiler ICE related to `impl Trait` in traits
mod impl_parser_hack {
    use super::*;

    pub type OpParser = impl crate::Parser<Item = Operator, Next = (), Error = Error> + Clone;

    pub fn parser() -> OpParser {
        #[auto_enum(derive_parse::Parser)]
        fn atomic_parser(tag: u8) -> impl crate::Parser<Item = Operator, Next = (), Error = Error> {
            match tag {
                0x00 => AlignedMemargParser(2).map(|memarg| Operator::AtomicNotify { memarg }),
                0x01 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicWait { memarg }),
                0x02 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicWait { memarg }),
                0x03 => FixedReader::<u8>::new().map(|flags| Operator::AtomicFence { flags }),
                0x10 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicLoad { memarg }),
                0x11 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicLoad { memarg }),
                0x12 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicLoad8U { memarg })
                }
                0x13 => AlignedMemargParser(1).map(|memarg| Operator::I32AtomicLoad16U { memarg }),
                0x14 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicLoad8U { memarg })
                }
                0x15 => AlignedMemargParser(1).map(|memarg| Operator::I64AtomicLoad16U { memarg }),
                0x16 => AlignedMemargParser(2).map(|memarg| Operator::I64AtomicLoad32U { memarg }),
                0x17 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicStore { memarg }),
                0x18 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicStore { memarg }),
                0x19 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicStore8 { memarg })
                }
                0x1a => AlignedMemargParser(1).map(|memarg| Operator::I32AtomicStore16 { memarg }),
                0x1b => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicStore8 { memarg })
                }
                0x1c => AlignedMemargParser(1).map(|memarg| Operator::I64AtomicStore16 { memarg }),
                0x1d => AlignedMemargParser(2).map(|memarg| Operator::I64AtomicStore32 { memarg }),
                0x1e => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwAdd { memarg }),
                0x1f => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwAdd { memarg }),
                0x20 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8AddU { memarg })
                }
                0x21 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16AddU { memarg })
                }
                0x22 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8AddU { memarg })
                }
                0x23 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16AddU { memarg })
                }
                0x24 => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32AddU { memarg })
                }
                0x25 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwSub { memarg }),
                0x26 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwSub { memarg }),
                0x27 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8SubU { memarg })
                }
                0x28 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16SubU { memarg })
                }
                0x29 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8SubU { memarg })
                }
                0x2a => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16SubU { memarg })
                }
                0x2b => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32SubU { memarg })
                }
                0x2c => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwAnd { memarg }),
                0x2d => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwAnd { memarg }),
                0x2e => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8AndU { memarg })
                }
                0x2f => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16AndU { memarg })
                }
                0x30 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8AndU { memarg })
                }
                0x31 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16AndU { memarg })
                }
                0x32 => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32AndU { memarg })
                }
                0x33 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwOr { memarg }),
                0x34 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwOr { memarg }),
                0x35 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8OrU { memarg })
                }
                0x36 => AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16OrU { memarg }),
                0x37 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8OrU { memarg })
                }
                0x38 => AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16OrU { memarg }),
                0x39 => AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32OrU { memarg }),
                0x3a => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwXor { memarg }),
                0x3b => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwXor { memarg }),
                0x3c => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8XorU { memarg })
                }
                0x3d => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16XorU { memarg })
                }
                0x3e => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8XorU { memarg })
                }
                0x3f => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16XorU { memarg })
                }
                0x40 => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32XorU { memarg })
                }
                0x41 => AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwXchg { memarg }),
                0x42 => AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwXchg { memarg }),
                0x43 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I32AtomicRmw8XchgU { memarg })
                }
                0x44 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16XchgU { memarg })
                }
                0x45 => {
                    MemoryImmediate::parser().map(|memarg| Operator::I64AtomicRmw8XchgU { memarg })
                }
                0x46 => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16XchgU { memarg })
                }
                0x47 => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32XchgU { memarg })
                }
                0x48 => {
                    AlignedMemargParser(2).map(|memarg| Operator::I32AtomicRmwCmpxchg { memarg })
                }
                0x49 => {
                    AlignedMemargParser(3).map(|memarg| Operator::I64AtomicRmwCmpxchg { memarg })
                }
                0x4a => MemoryImmediate::parser()
                    .map(|memarg| Operator::I32AtomicRmw8CmpxchgU { memarg }),
                0x4b => {
                    AlignedMemargParser(1).map(|memarg| Operator::I32AtomicRmw16CmpxchgU { memarg })
                }
                0x4c => MemoryImmediate::parser()
                    .map(|memarg| Operator::I64AtomicRmw8CmpxchgU { memarg }),
                0x4d => {
                    AlignedMemargParser(1).map(|memarg| Operator::I64AtomicRmw16CmpxchgU { memarg })
                }
                0x4e => {
                    AlignedMemargParser(2).map(|memarg| Operator::I64AtomicRmw32CmpxchgU { memarg })
                }
                _ => JustResult::new(Err(ParserError::UnknownOperator)),
            }
        }

        #[auto_enum(derive_parse::Parser)]
        fn extended_opset_parser(
            tag: u8,
        ) -> impl crate::Parser<Item = Operator, Next = (), Error = Error> {
            match tag {
                0x00 => JustResult::new(Ok(Operator::I32TruncSatF32S)),
                0x01 => JustResult::new(Ok(Operator::I32TruncSatF32U)),
                0x02 => JustResult::new(Ok(Operator::I32TruncSatF64S)),
                0x03 => JustResult::new(Ok(Operator::I32TruncSatF64U)),
                0x04 => JustResult::new(Ok(Operator::I64TruncSatF32S)),
                0x05 => JustResult::new(Ok(Operator::I64TruncSatF32U)),
                0x06 => JustResult::new(Ok(Operator::I64TruncSatF64S)),
                0x07 => JustResult::new(Ok(Operator::I64TruncSatF64U)),

                0x08 => (VarReader::<u32>::new(), FixedReader::<u8>::new())
                    .collect()
                    .and_then(|(segment, mem)| {
                        if mem != 0 {
                            Err(ParserError::InvalidMemoryIndex)
                        } else {
                            Ok(Operator::MemoryInit { segment })
                        }
                    }),
                0x09 => VarReader::<u32>::new().map(|segment| Operator::DataDrop { segment }),
                0x0a => (FixedReader::<u8>::new(), FixedReader::<u8>::new())
                    .collect()
                    .and_then(|(src, dst)| {
                        if src != 0 || dst != 0 {
                            Err(ParserError::InvalidMemoryIndex)
                        } else {
                            Ok(Operator::MemoryCopy)
                        }
                    }),
                0x0b => FixedReader::<u8>::new().and_then(|mem| {
                    if mem != 0 {
                        Err(ParserError::InvalidMemoryIndex)
                    } else {
                        Ok(Operator::MemoryFill)
                    }
                }),
                0x0c => (VarReader::<u32>::new(), VarReader::<u32>::new())
                    .collect()
                    .map(|(segment, table)| Operator::TableInit { segment, table }),
                0x0d => VarReader::<u32>::new().map(|segment| Operator::ElemDrop { segment }),
                0x0e => (VarReader::<u32>::new(), VarReader::<u32>::new())
                    .collect()
                    .map(|(src_table, dst_table)| Operator::TableCopy {
                        src_table,
                        dst_table,
                    }),

                0x0f => VarReader::<u32>::new().map(|table| Operator::TableGrow { table }),
                0x10 => VarReader::<u32>::new().map(|table| Operator::TableSize { table }),
                0x11 => VarReader::<u32>::new().map(|table| Operator::TableFill { table }),

                _ => JustResult::new(Err(ParserError::UnknownOperator)),
            }
        }

        #[auto_enum(derive_parse::Parser)]
        fn operator_parser(
            tag: u8,
        ) -> impl crate::Parser<Item = Operator, Next = (), Error = Error> {
            match tag {
                0x02 => TypeOrFuncType::parser().map(|ty| Operator::Block { ty }),
                0x03 => TypeOrFuncType::parser().map(|ty| Operator::Loop { ty }),
                0x04 => TypeOrFuncType::parser().map(|ty| Operator::If { ty }),
                0x0c => {
                    VarReader::<u32>::new().map(|relative_depth| Operator::Br { relative_depth })
                }
                0x0d => {
                    VarReader::<u32>::new().map(|relative_depth| Operator::BrIf { relative_depth })
                }
                0x0e => BrTable::parser().map(|table| Operator::BrTable { table }),
                0x10 => {
                    VarReader::<u32>::new().map(|function_index| Operator::Call { function_index })
                }
                0x11 => (VarReader::<u32>::new(), VarReader::<u32>::new())
                    .collect()
                    .map(|(index, table_index)| Operator::CallIndirect { index, table_index }),
                0x20 => {
                    VarReader::<u32>::new().map(|local_index| Operator::LocalGet { local_index })
                }
                0x21 => {
                    VarReader::<u32>::new().map(|local_index| Operator::LocalSet { local_index })
                }
                0x22 => {
                    VarReader::<u32>::new().map(|local_index| Operator::LocalTee { local_index })
                }
                0x23 => {
                    VarReader::<u32>::new().map(|global_index| Operator::GlobalGet { global_index })
                }
                0x24 => {
                    VarReader::<u32>::new().map(|global_index| Operator::GlobalSet { global_index })
                }
                0x28 => MemoryImmediate::parser().map(|memarg| Operator::I32Load { memarg }),
                0x29 => MemoryImmediate::parser().map(|memarg| Operator::I64Load { memarg }),
                0x2a => MemoryImmediate::parser().map(|memarg| Operator::F32Load { memarg }),
                0x2b => MemoryImmediate::parser().map(|memarg| Operator::F64Load { memarg }),
                0x2c => MemoryImmediate::parser().map(|memarg| Operator::I32Load8S { memarg }),
                0x2d => MemoryImmediate::parser().map(|memarg| Operator::I32Load8U { memarg }),
                0x2e => MemoryImmediate::parser().map(|memarg| Operator::I32Load16S { memarg }),
                0x2f => MemoryImmediate::parser().map(|memarg| Operator::I32Load16U { memarg }),
                0x30 => MemoryImmediate::parser().map(|memarg| Operator::I64Load8S { memarg }),
                0x31 => MemoryImmediate::parser().map(|memarg| Operator::I64Load8U { memarg }),
                0x32 => MemoryImmediate::parser().map(|memarg| Operator::I64Load16S { memarg }),
                0x33 => MemoryImmediate::parser().map(|memarg| Operator::I64Load16U { memarg }),
                0x34 => MemoryImmediate::parser().map(|memarg| Operator::I64Load32S { memarg }),
                0x35 => MemoryImmediate::parser().map(|memarg| Operator::I64Load32U { memarg }),
                0x36 => MemoryImmediate::parser().map(|memarg| Operator::I32Store { memarg }),
                0x37 => MemoryImmediate::parser().map(|memarg| Operator::I64Store { memarg }),
                0x38 => MemoryImmediate::parser().map(|memarg| Operator::F32Store { memarg }),
                0x39 => MemoryImmediate::parser().map(|memarg| Operator::F64Store { memarg }),
                0x3a => MemoryImmediate::parser().map(|memarg| Operator::I32Store8 { memarg }),
                0x3b => MemoryImmediate::parser().map(|memarg| Operator::I32Store16 { memarg }),
                0x3c => MemoryImmediate::parser().map(|memarg| Operator::I64Store8 { memarg }),
                0x3d => MemoryImmediate::parser().map(|memarg| Operator::I64Store16 { memarg }),
                0x3e => MemoryImmediate::parser().map(|memarg| Operator::I64Store32 { memarg }),
                0x3f => FixedReader::<bool>::new().map(|reserved| Operator::MemorySize {
                    reserved: if reserved { 1 } else { 0 },
                }),
                0x40 => FixedReader::<bool>::new().map(|reserved| Operator::MemoryGrow {
                    reserved: if reserved { 1 } else { 0 },
                }),
                0x41 => VarReader::<i32>::new().map(|value| Operator::I32Const { value }),
                0x42 => VarReader::<i64>::new().map(|value| Operator::I64Const { value }),
                0x43 => Ieee32::parser().map(|value| Operator::F32Const { value }),
                0x44 => Ieee64::parser().map(|value| Operator::F64Const { value }),

                0xd2 => VarReader::<u32>::new()
                    .map(|function_index| Operator::RefFunc { function_index }),

                0xfc => FixedReader::<u8>::new().and_then(extended_opset_parser),
                0xfe => FixedReader::<u8>::new().and_then(atomic_parser),

                // Split into sub-match to prevent `auto_enum` from making a separate type
                // parameter for each op here
                other => JustResult::new(match other {
                    0x00 => Ok(Operator::Unreachable),
                    0x01 => Ok(Operator::Nop),

                    0x05 => Ok(Operator::Else),
                    0x0b => Ok(Operator::End),

                    0x0f => Ok(Operator::Return),

                    0x1a => Ok(Operator::Drop),
                    0x1b => Ok(Operator::Select),

                    0xd0 => Ok(Operator::RefNull),
                    0xd1 => Ok(Operator::RefIsNull),

                    0x45 => Ok(Operator::I32Eqz),
                    0x46 => Ok(Operator::I32Eq),
                    0x47 => Ok(Operator::I32Ne),
                    0x48 => Ok(Operator::I32LtS),
                    0x49 => Ok(Operator::I32LtU),
                    0x4a => Ok(Operator::I32GtS),
                    0x4b => Ok(Operator::I32GtU),
                    0x4c => Ok(Operator::I32LeS),
                    0x4d => Ok(Operator::I32LeU),
                    0x4e => Ok(Operator::I32GeS),
                    0x4f => Ok(Operator::I32GeU),
                    0x50 => Ok(Operator::I64Eqz),
                    0x51 => Ok(Operator::I64Eq),
                    0x52 => Ok(Operator::I64Ne),
                    0x53 => Ok(Operator::I64LtS),
                    0x54 => Ok(Operator::I64LtU),
                    0x55 => Ok(Operator::I64GtS),
                    0x56 => Ok(Operator::I64GtU),
                    0x57 => Ok(Operator::I64LeS),
                    0x58 => Ok(Operator::I64LeU),
                    0x59 => Ok(Operator::I64GeS),
                    0x5a => Ok(Operator::I64GeU),
                    0x5b => Ok(Operator::F32Eq),
                    0x5c => Ok(Operator::F32Ne),
                    0x5d => Ok(Operator::F32Lt),
                    0x5e => Ok(Operator::F32Gt),
                    0x5f => Ok(Operator::F32Le),
                    0x60 => Ok(Operator::F32Ge),
                    0x61 => Ok(Operator::F64Eq),
                    0x62 => Ok(Operator::F64Ne),
                    0x63 => Ok(Operator::F64Lt),
                    0x64 => Ok(Operator::F64Gt),
                    0x65 => Ok(Operator::F64Le),
                    0x66 => Ok(Operator::F64Ge),
                    0x67 => Ok(Operator::I32Clz),
                    0x68 => Ok(Operator::I32Ctz),
                    0x69 => Ok(Operator::I32Popcnt),
                    0x6a => Ok(Operator::I32Add),
                    0x6b => Ok(Operator::I32Sub),
                    0x6c => Ok(Operator::I32Mul),
                    0x6d => Ok(Operator::I32DivS),
                    0x6e => Ok(Operator::I32DivU),
                    0x6f => Ok(Operator::I32RemS),
                    0x70 => Ok(Operator::I32RemU),
                    0x71 => Ok(Operator::I32And),
                    0x72 => Ok(Operator::I32Or),
                    0x73 => Ok(Operator::I32Xor),
                    0x74 => Ok(Operator::I32Shl),
                    0x75 => Ok(Operator::I32ShrS),
                    0x76 => Ok(Operator::I32ShrU),
                    0x77 => Ok(Operator::I32Rotl),
                    0x78 => Ok(Operator::I32Rotr),
                    0x79 => Ok(Operator::I64Clz),
                    0x7a => Ok(Operator::I64Ctz),
                    0x7b => Ok(Operator::I64Popcnt),
                    0x7c => Ok(Operator::I64Add),
                    0x7d => Ok(Operator::I64Sub),
                    0x7e => Ok(Operator::I64Mul),
                    0x7f => Ok(Operator::I64DivS),
                    0x80 => Ok(Operator::I64DivU),
                    0x81 => Ok(Operator::I64RemS),
                    0x82 => Ok(Operator::I64RemU),
                    0x83 => Ok(Operator::I64And),
                    0x84 => Ok(Operator::I64Or),
                    0x85 => Ok(Operator::I64Xor),
                    0x86 => Ok(Operator::I64Shl),
                    0x87 => Ok(Operator::I64ShrS),
                    0x88 => Ok(Operator::I64ShrU),
                    0x89 => Ok(Operator::I64Rotl),
                    0x8a => Ok(Operator::I64Rotr),
                    0x8b => Ok(Operator::F32Abs),
                    0x8c => Ok(Operator::F32Neg),
                    0x8d => Ok(Operator::F32Ceil),
                    0x8e => Ok(Operator::F32Floor),
                    0x8f => Ok(Operator::F32Trunc),
                    0x90 => Ok(Operator::F32Nearest),
                    0x91 => Ok(Operator::F32Sqrt),
                    0x92 => Ok(Operator::F32Add),
                    0x93 => Ok(Operator::F32Sub),
                    0x94 => Ok(Operator::F32Mul),
                    0x95 => Ok(Operator::F32Div),
                    0x96 => Ok(Operator::F32Min),
                    0x97 => Ok(Operator::F32Max),
                    0x98 => Ok(Operator::F32Copysign),
                    0x99 => Ok(Operator::F64Abs),
                    0x9a => Ok(Operator::F64Neg),
                    0x9b => Ok(Operator::F64Ceil),
                    0x9c => Ok(Operator::F64Floor),
                    0x9d => Ok(Operator::F64Trunc),
                    0x9e => Ok(Operator::F64Nearest),
                    0x9f => Ok(Operator::F64Sqrt),
                    0xa0 => Ok(Operator::F64Add),
                    0xa1 => Ok(Operator::F64Sub),
                    0xa2 => Ok(Operator::F64Mul),
                    0xa3 => Ok(Operator::F64Div),
                    0xa4 => Ok(Operator::F64Min),
                    0xa5 => Ok(Operator::F64Max),
                    0xa6 => Ok(Operator::F64Copysign),
                    0xa7 => Ok(Operator::I32WrapI64),
                    0xa8 => Ok(Operator::I32TruncF32S),
                    0xa9 => Ok(Operator::I32TruncF32U),
                    0xaa => Ok(Operator::I32TruncF64S),
                    0xab => Ok(Operator::I32TruncF64U),
                    0xac => Ok(Operator::I64ExtendI32S),
                    0xad => Ok(Operator::I64ExtendI32U),
                    0xae => Ok(Operator::I64TruncF32S),
                    0xaf => Ok(Operator::I64TruncF32U),
                    0xb0 => Ok(Operator::I64TruncF64S),
                    0xb1 => Ok(Operator::I64TruncF64U),
                    0xb2 => Ok(Operator::F32ConvertI32S),
                    0xb3 => Ok(Operator::F32ConvertI32U),
                    0xb4 => Ok(Operator::F32ConvertI64S),
                    0xb5 => Ok(Operator::F32ConvertI64U),
                    0xb6 => Ok(Operator::F32DemoteF64),
                    0xb7 => Ok(Operator::F64ConvertI32S),
                    0xb8 => Ok(Operator::F64ConvertI32U),
                    0xb9 => Ok(Operator::F64ConvertI64S),
                    0xba => Ok(Operator::F64ConvertI64U),
                    0xbb => Ok(Operator::F64PromoteF32),
                    0xbc => Ok(Operator::I32ReinterpretF32),
                    0xbd => Ok(Operator::I64ReinterpretF64),
                    0xbe => Ok(Operator::F32ReinterpretI32),
                    0xbf => Ok(Operator::F64ReinterpretI64),

                    0xc0 => Ok(Operator::I32Extend8S),
                    0xc1 => Ok(Operator::I32Extend16S),
                    0xc2 => Ok(Operator::I64Extend8S),
                    0xc3 => Ok(Operator::I64Extend16S),
                    0xc4 => Ok(Operator::I64Extend32S),

                    _ => Err(ParserError::UnknownOperator),
                }),
            }
        }

        FixedReader::<u8>::new().and_then(operator_parser)
    }
}
