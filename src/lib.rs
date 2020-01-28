#![feature(never_type, type_alias_impl_trait, specialization)]

mod helpers;
mod ints;
mod operations;
mod parse;
mod vec;

use failure::Fail;

pub use operations::{BrTable, FunctionBody, Ieee32, Ieee64, Operator, OperatorsReader};
pub use parse::{Parse, ParseExt};
pub use wasm_reader_traits::{
    IntoParser, JustResult, MaybePosition, ParseOne, ParseResult, Parser,
};

use derive_more::From;
use std::io;

pub type Error = wasm_reader_traits::Error<ParserError>;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(From, Debug, Fail)]
pub enum ParserError {
    #[fail(display = "Invalid type")]
    InvalidType,
    #[fail(display = "Invalid number")]
    InvalidNumber,
    #[fail(display = "Unknown operator")]
    UnknownOperator,
    #[fail(display = "Misaligned memory immediate")]
    MisalignedMemoryImmediate,
    #[fail(display = "Invalid memory index (must be 0)")]
    InvalidMemoryIndex,
    #[fail(display = "IO error: {}", 0)]
    IoError(io::Error),
}

impl From<io::ErrorKind> for ParserError {
    fn from(kind: io::ErrorKind) -> Self {
        Self::from(io::Error::from(kind))
    }
}

#[cfg(test)]
mod tests {
    use glob::glob;

    #[test]
    fn test_reader_helper() {
        use crate::{ints::VarReader, vec::ManyBuilder};
        use std::convert::TryInto;
        use wasm_reader_traits::{Reader, ReaderBuild, ReaderBuildIter, ReaderRead};

        let state: Vec<_> = vec![];

        assert_eq!(
            Reader::new((
                ManyBuilder::new(VarReader::<u32>::new()),
                VarReader::<u64>::new(),
            ))
            .with_state(state)
            .then_iter(|state, vals| {
                for val in vals {
                    state.push(val?);
                }

                Ok(())
            })
            .then(|state, val| {
                // Check that type inference works both for arguments and for the vector
                state.push(val.try_into().unwrap());

                Ok(())
            })
            .read(&mut std::io::Cursor::new(&[
                // 2 elements...
                0b0_0000010,
                // First element: 1
                0b0_0000001,
                // Second element: 255
                0b1_1111111,
                0b0_0000001,
                // Next part: 2
                0b0_0000010
            ]))
            .unwrap(),
            vec![1, 255, 2]
        );
    }

    #[test]
    fn test_cases() {
        use wasmparser::{ModuleReader, SectionCode};

        'read_file: for file in glob("./tests/*.wasm").unwrap() {
            let file = file.unwrap();

            let filename = file.to_str().unwrap();

            let bytes = std::fs::read(&file).expect(filename);
            let mut reader = ModuleReader::new(&bytes).expect(filename);
            let mut section = match reader.read() {
                Ok(s) => s,
                Err(_) => continue,
            };

            while section.code != SectionCode::Code {
                section = match reader.read() {
                    Ok(s) => s,
                    Err(_) => continue 'read_file,
                };
            }

            let mut functions = section.get_code_section_reader().expect(filename);
            let mut func_idx = 0;

            'func_iter: while let Ok(func) = functions.read() {
                let func_name = format!("{}[{}]", filename, func_idx);
                func_idx += 1;

                let range = func.range();
                let mut reader = std::io::Cursor::new(range.slice(&bytes));
                let body = crate::operations::FunctionBody::new(&mut reader).expect(&func_name);
                let (my_locals, mut my_ops) = body.into_locals_and_code(&mut reader);
                let my_locals = my_locals.collect::<Result<Vec<_>, _>>().expect(&func_name);

                let wp_locals = func
                    .get_locals_reader()
                    .expect(&func_name)
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()
                    .expect(&func_name);

                assert_eq!(my_locals, wp_locals);

                let mut wp_ops = func.get_operators_reader().expect(&func_name);

                loop {
                    use crate::Operator as WR;
                    use wasmparser::Operator as WP;

                    let my_op = if let Some(op) = my_ops.next(&mut reader) {
                        op.expect(&func_name)
                    } else {
                        wp_ops.ensure_end().unwrap();
                        continue 'func_iter;
                    };

                    let wp_op = wp_ops.read().expect(&func_name);

                    match (my_op, wp_op) {
                        (WR::Unreachable, WP::Unreachable) | (WR::Nop, WP::Nop) => {}

                        (WR::Block { ty: wr_ty }, WP::Block { ty: wp_ty })
                        | (WR::Loop { ty: wr_ty }, WP::Loop { ty: wp_ty })
                        | (WR::If { ty: wr_ty }, WP::If { ty: wp_ty }) => {
                            assert_eq!(wr_ty, wp_ty, "{}", func_name)
                        }

                        (WR::Else, WP::Else) | (WR::End, WP::End) => {}

                        (
                            WR::Br {
                                relative_depth: wr_relative_depth,
                            },
                            WP::Br {
                                relative_depth: wp_relative_depth,
                            },
                        )
                        | (
                            WR::BrIf {
                                relative_depth: wr_relative_depth,
                            },
                            WP::BrIf {
                                relative_depth: wp_relative_depth,
                            },
                        ) => assert_eq!(wr_relative_depth, wp_relative_depth, "{}", func_name),

                        (WR::BrTable { table: wr_table }, WP::BrTable { table: wp_table }) => {
                            use crate::ParseOne as _;

                            let (wp_table_targets, wp_table_default) =
                                wp_table.read_table().expect(&func_name);
                            let (wr_table_targets, wr_table_default) =
                                wr_table.targets_and_default(&mut reader);
                            let wr_table_targets = wr_table_targets
                                .collect::<Result<Vec<_>, _>>()
                                .expect(&func_name);
                            let wr_table_default =
                                wr_table_default.parse(&mut reader).expect(&func_name);

                            assert_eq!(
                                &wr_table_targets[..],
                                &wp_table_targets[..],
                                "{}",
                                func_name
                            );
                            assert_eq!(wr_table_default, wp_table_default, "{}", func_name);
                        }

                        (WR::Return, WP::Return) => {}

                        (
                            WR::Call {
                                function_index: wr_function_index,
                            },
                            WP::Call {
                                function_index: wp_function_index,
                            },
                        ) => assert_eq!(wr_function_index, wp_function_index, "{}", func_name),

                        (
                            WR::CallIndirect {
                                index: wr_index,
                                table_index: wr_table_index,
                            },
                            WP::CallIndirect {
                                index: wp_index,
                                table_index: wp_table_index,
                            },
                        ) => {
                            assert_eq!(wr_index, wp_index, "{}", func_name);
                            assert_eq!(wr_table_index, wp_table_index, "{}", func_name);
                        }

                        (WR::Drop, WP::Drop) => {}
                        (WR::Select, WP::Select) => {}

                        (
                            WR::LocalGet {
                                local_index: wr_local_index,
                            },
                            WP::LocalGet {
                                local_index: wp_local_index,
                            },
                        )
                        | (
                            WR::LocalSet {
                                local_index: wr_local_index,
                            },
                            WP::LocalSet {
                                local_index: wp_local_index,
                            },
                        )
                        | (
                            WR::LocalTee {
                                local_index: wr_local_index,
                            },
                            WP::LocalTee {
                                local_index: wp_local_index,
                            },
                        ) => assert_eq!(wr_local_index, wp_local_index, "{}", func_name),

                        (
                            WR::GlobalGet {
                                global_index: wr_global_index,
                            },
                            WP::GlobalGet {
                                global_index: wp_global_index,
                            },
                        )
                        | (
                            WR::GlobalSet {
                                global_index: wr_global_index,
                            },
                            WP::GlobalSet {
                                global_index: wp_global_index,
                            },
                        ) => assert_eq!(wr_global_index, wp_global_index, "{}", func_name),

                        (WR::I32Load { memarg: wr_memarg }, WP::I32Load { memarg: wp_memarg }) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }
                        (WR::I64Load { memarg: wr_memarg }, WP::I64Load { memarg: wp_memarg }) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }
                        (WR::F32Load { memarg: wr_memarg }, WP::F32Load { memarg: wp_memarg }) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }
                        (WR::F64Load { memarg: wr_memarg }, WP::F64Load { memarg: wp_memarg }) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }

                        (
                            WR::I32Load8S { memarg: wr_memarg },
                            WP::I32Load8S { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Load8U { memarg: wr_memarg },
                            WP::I32Load8U { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Load16S { memarg: wr_memarg },
                            WP::I32Load16S { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Load16U { memarg: wr_memarg },
                            WP::I32Load16U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load8S { memarg: wr_memarg },
                            WP::I64Load8S { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load8U { memarg: wr_memarg },
                            WP::I64Load8U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load16S { memarg: wr_memarg },
                            WP::I64Load16S { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load16U { memarg: wr_memarg },
                            WP::I64Load16U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load32S { memarg: wr_memarg },
                            WP::I64Load32S { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Load32U { memarg: wr_memarg },
                            WP::I64Load32U { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Store { memarg: wr_memarg },
                            WP::I32Store { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Store { memarg: wr_memarg },
                            WP::I64Store { memarg: wp_memarg },
                        )
                        | (
                            WR::F32Store { memarg: wr_memarg },
                            WP::F32Store { memarg: wp_memarg },
                        )
                        | (
                            WR::F64Store { memarg: wr_memarg },
                            WP::F64Store { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Store8 { memarg: wr_memarg },
                            WP::I32Store8 { memarg: wp_memarg },
                        )
                        | (
                            WR::I32Store16 { memarg: wr_memarg },
                            WP::I32Store16 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Store8 { memarg: wr_memarg },
                            WP::I64Store8 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Store16 { memarg: wr_memarg },
                            WP::I64Store16 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64Store32 { memarg: wr_memarg },
                            WP::I64Store32 { memarg: wp_memarg },
                        ) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }

                        (
                            WR::MemorySize {
                                reserved: wr_reserved,
                            },
                            WP::MemorySize {
                                reserved: wp_reserved,
                            },
                        )
                        | (
                            WR::MemoryGrow {
                                reserved: wr_reserved,
                            },
                            WP::MemoryGrow {
                                reserved: wp_reserved,
                            },
                        ) => assert_eq!(wr_reserved, wp_reserved, "{}", func_name),

                        (WR::I32Const { value: wr_value }, WP::I32Const { value: wp_value }) => {
                            assert_eq!(wr_value, wp_value, "{}", func_name)
                        }
                        (WR::I64Const { value: wr_value }, WP::I64Const { value: wp_value }) => {
                            assert_eq!(wr_value, wp_value, "{}", func_name)
                        }
                        (WR::F32Const { value: wr_value }, WP::F32Const { value: wp_value }) => {
                            assert_eq!(wr_value.0, wp_value.bits(), "{}", func_name)
                        }
                        (WR::F64Const { value: wr_value }, WP::F64Const { value: wp_value }) => {
                            assert_eq!(wr_value.0, wp_value.bits(), "{}", func_name)
                        }

                        (WR::RefNull, WP::RefNull) | (WR::RefIsNull, WP::RefIsNull) => {}

                        (
                            WR::RefFunc {
                                function_index: wr_function_index,
                            },
                            WP::RefFunc {
                                function_index: wp_function_index,
                            },
                        ) => assert_eq!(wr_function_index, wp_function_index),

                        (WR::I32Eqz, WP::I32Eqz)
                        | (WR::I32Eq, WP::I32Eq)
                        | (WR::I32Ne, WP::I32Ne)
                        | (WR::I32LtS, WP::I32LtS)
                        | (WR::I32LtU, WP::I32LtU)
                        | (WR::I32GtS, WP::I32GtS)
                        | (WR::I32GtU, WP::I32GtU)
                        | (WR::I32LeS, WP::I32LeS)
                        | (WR::I32LeU, WP::I32LeU)
                        | (WR::I32GeS, WP::I32GeS)
                        | (WR::I32GeU, WP::I32GeU)
                        | (WR::I64Eqz, WP::I64Eqz)
                        | (WR::I64Eq, WP::I64Eq)
                        | (WR::I64Ne, WP::I64Ne)
                        | (WR::I64LtS, WP::I64LtS)
                        | (WR::I64LtU, WP::I64LtU)
                        | (WR::I64GtS, WP::I64GtS)
                        | (WR::I64GtU, WP::I64GtU)
                        | (WR::I64LeS, WP::I64LeS)
                        | (WR::I64LeU, WP::I64LeU)
                        | (WR::I64GeS, WP::I64GeS)
                        | (WR::I64GeU, WP::I64GeU)
                        | (WR::F32Eq, WP::F32Eq)
                        | (WR::F32Ne, WP::F32Ne)
                        | (WR::F32Lt, WP::F32Lt)
                        | (WR::F32Gt, WP::F32Gt)
                        | (WR::F32Le, WP::F32Le)
                        | (WR::F32Ge, WP::F32Ge)
                        | (WR::F64Eq, WP::F64Eq)
                        | (WR::F64Ne, WP::F64Ne)
                        | (WR::F64Lt, WP::F64Lt)
                        | (WR::F64Gt, WP::F64Gt)
                        | (WR::F64Le, WP::F64Le)
                        | (WR::F64Ge, WP::F64Ge)
                        | (WR::I32Clz, WP::I32Clz)
                        | (WR::I32Ctz, WP::I32Ctz)
                        | (WR::I32Popcnt, WP::I32Popcnt)
                        | (WR::I32Add, WP::I32Add)
                        | (WR::I32Sub, WP::I32Sub)
                        | (WR::I32Mul, WP::I32Mul)
                        | (WR::I32DivS, WP::I32DivS)
                        | (WR::I32DivU, WP::I32DivU)
                        | (WR::I32RemS, WP::I32RemS)
                        | (WR::I32RemU, WP::I32RemU)
                        | (WR::I32And, WP::I32And)
                        | (WR::I32Or, WP::I32Or)
                        | (WR::I32Xor, WP::I32Xor)
                        | (WR::I32Shl, WP::I32Shl)
                        | (WR::I32ShrS, WP::I32ShrS)
                        | (WR::I32ShrU, WP::I32ShrU)
                        | (WR::I32Rotl, WP::I32Rotl)
                        | (WR::I32Rotr, WP::I32Rotr)
                        | (WR::I64Clz, WP::I64Clz)
                        | (WR::I64Ctz, WP::I64Ctz)
                        | (WR::I64Popcnt, WP::I64Popcnt)
                        | (WR::I64Add, WP::I64Add)
                        | (WR::I64Sub, WP::I64Sub)
                        | (WR::I64Mul, WP::I64Mul)
                        | (WR::I64DivS, WP::I64DivS)
                        | (WR::I64DivU, WP::I64DivU)
                        | (WR::I64RemS, WP::I64RemS)
                        | (WR::I64RemU, WP::I64RemU)
                        | (WR::I64And, WP::I64And)
                        | (WR::I64Or, WP::I64Or)
                        | (WR::I64Xor, WP::I64Xor)
                        | (WR::I64Shl, WP::I64Shl)
                        | (WR::I64ShrS, WP::I64ShrS)
                        | (WR::I64ShrU, WP::I64ShrU)
                        | (WR::I64Rotl, WP::I64Rotl)
                        | (WR::I64Rotr, WP::I64Rotr)
                        | (WR::F32Abs, WP::F32Abs)
                        | (WR::F32Neg, WP::F32Neg)
                        | (WR::F32Ceil, WP::F32Ceil)
                        | (WR::F32Floor, WP::F32Floor)
                        | (WR::F32Trunc, WP::F32Trunc)
                        | (WR::F32Nearest, WP::F32Nearest)
                        | (WR::F32Sqrt, WP::F32Sqrt)
                        | (WR::F32Add, WP::F32Add)
                        | (WR::F32Sub, WP::F32Sub)
                        | (WR::F32Mul, WP::F32Mul)
                        | (WR::F32Div, WP::F32Div)
                        | (WR::F32Min, WP::F32Min)
                        | (WR::F32Max, WP::F32Max)
                        | (WR::F32Copysign, WP::F32Copysign)
                        | (WR::F64Abs, WP::F64Abs)
                        | (WR::F64Neg, WP::F64Neg)
                        | (WR::F64Ceil, WP::F64Ceil)
                        | (WR::F64Floor, WP::F64Floor)
                        | (WR::F64Trunc, WP::F64Trunc)
                        | (WR::F64Nearest, WP::F64Nearest)
                        | (WR::F64Sqrt, WP::F64Sqrt)
                        | (WR::F64Add, WP::F64Add)
                        | (WR::F64Sub, WP::F64Sub)
                        | (WR::F64Mul, WP::F64Mul)
                        | (WR::F64Div, WP::F64Div)
                        | (WR::F64Min, WP::F64Min)
                        | (WR::F64Max, WP::F64Max)
                        | (WR::F64Copysign, WP::F64Copysign)
                        | (WR::I32WrapI64, WP::I32WrapI64)
                        | (WR::I32TruncF32S, WP::I32TruncF32S)
                        | (WR::I32TruncF32U, WP::I32TruncF32U)
                        | (WR::I32TruncF64S, WP::I32TruncF64S)
                        | (WR::I32TruncF64U, WP::I32TruncF64U)
                        | (WR::I64ExtendI32S, WP::I64ExtendI32S)
                        | (WR::I64ExtendI32U, WP::I64ExtendI32U)
                        | (WR::I64TruncF32S, WP::I64TruncF32S)
                        | (WR::I64TruncF32U, WP::I64TruncF32U)
                        | (WR::I64TruncF64S, WP::I64TruncF64S)
                        | (WR::I64TruncF64U, WP::I64TruncF64U)
                        | (WR::F32ConvertI32S, WP::F32ConvertI32S)
                        | (WR::F32ConvertI32U, WP::F32ConvertI32U)
                        | (WR::F32ConvertI64S, WP::F32ConvertI64S)
                        | (WR::F32ConvertI64U, WP::F32ConvertI64U)
                        | (WR::F32DemoteF64, WP::F32DemoteF64)
                        | (WR::F64ConvertI32S, WP::F64ConvertI32S)
                        | (WR::F64ConvertI32U, WP::F64ConvertI32U)
                        | (WR::F64ConvertI64S, WP::F64ConvertI64S)
                        | (WR::F64ConvertI64U, WP::F64ConvertI64U)
                        | (WR::F64PromoteF32, WP::F64PromoteF32)
                        | (WR::I32ReinterpretF32, WP::I32ReinterpretF32)
                        | (WR::I64ReinterpretF64, WP::I64ReinterpretF64)
                        | (WR::F32ReinterpretI32, WP::F32ReinterpretI32)
                        | (WR::F64ReinterpretI64, WP::F64ReinterpretI64)
                        | (WR::I32Extend8S, WP::I32Extend8S)
                        | (WR::I32Extend16S, WP::I32Extend16S)
                        | (WR::I64Extend8S, WP::I64Extend8S)
                        | (WR::I64Extend16S, WP::I64Extend16S)
                        | (WR::I64Extend32S, WP::I64Extend32S) => {}

                        // 0xFC operators
                        // Non-trapping Float-to-int Conversions
                        (WR::I32TruncSatF32S, WP::I32TruncSatF32S)
                        | (WR::I32TruncSatF32U, WP::I32TruncSatF32U)
                        | (WR::I32TruncSatF64S, WP::I32TruncSatF64S)
                        | (WR::I32TruncSatF64U, WP::I32TruncSatF64U)
                        | (WR::I64TruncSatF32S, WP::I64TruncSatF32S)
                        | (WR::I64TruncSatF32U, WP::I64TruncSatF32U)
                        | (WR::I64TruncSatF64S, WP::I64TruncSatF64S)
                        | (WR::I64TruncSatF64U, WP::I64TruncSatF64U) => {}

                        // 0xFC operators
                        // bulk memory https://github.com/WebAssembly/bulk-memory-operations/blob/master/proposals/bulk-memory-operations/Overview.md
                        (
                            WR::MemoryInit {
                                segment: wr_segment,
                            },
                            WP::MemoryInit {
                                segment: wp_segment,
                            },
                        )
                        | (
                            WR::DataDrop {
                                segment: wr_segment,
                            },
                            WP::DataDrop {
                                segment: wp_segment,
                            },
                        ) => assert_eq!(wr_segment, wp_segment),

                        (WR::MemoryCopy, WP::MemoryCopy) | (WR::MemoryFill, WP::MemoryFill) => {}

                        (
                            WR::TableInit {
                                segment: wr_segment,
                                table: wr_table,
                            },
                            WP::TableInit {
                                segment: wp_segment,
                                table: wp_table,
                            },
                        ) => {
                            assert_eq!(wr_segment, wp_segment);
                            assert_eq!(wr_table, wp_table);
                        }

                        (
                            WR::ElemDrop {
                                segment: wr_segment,
                            },
                            WP::ElemDrop {
                                segment: wp_segment,
                            },
                        ) => assert_eq!(wr_segment, wp_segment),

                        (
                            WR::TableCopy {
                                dst_table: wr_dst_table,
                                src_table: wr_src_table,
                            },
                            WP::TableCopy {
                                dst_table: wp_dst_table,
                                src_table: wp_src_table,
                            },
                        ) => {
                            assert_eq!(wr_dst_table, wp_dst_table);
                            assert_eq!(wr_src_table, wp_src_table);
                        }

                        (WR::TableFill { table: wr_table }, WP::TableFill { table: wp_table })
                        | (WR::TableGet { table: wr_table }, WP::TableGet { table: wp_table })
                        | (WR::TableSet { table: wr_table }, WP::TableSet { table: wp_table })
                        | (WR::TableGrow { table: wr_table }, WP::TableGrow { table: wp_table })
                        | (WR::TableSize { table: wr_table }, WP::TableSize { table: wp_table }) => {
                            assert_eq!(wr_table, wp_table)
                        }

                        (
                            WR::AtomicFence { flags: wr_flags },
                            WP::AtomicFence { flags: wp_flags },
                        ) => assert_eq!(wr_flags, wp_flags, "{}", func_name),

                        (
                            WR::AtomicNotify { memarg: wr_memarg },
                            WP::AtomicNotify { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicWait { memarg: wr_memarg },
                            WP::I32AtomicWait { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicWait { memarg: wr_memarg },
                            WP::I64AtomicWait { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicLoad { memarg: wr_memarg },
                            WP::I32AtomicLoad { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicLoad { memarg: wr_memarg },
                            WP::I64AtomicLoad { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicLoad8U { memarg: wr_memarg },
                            WP::I32AtomicLoad8U { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicLoad16U { memarg: wr_memarg },
                            WP::I32AtomicLoad16U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicLoad8U { memarg: wr_memarg },
                            WP::I64AtomicLoad8U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicLoad16U { memarg: wr_memarg },
                            WP::I64AtomicLoad16U { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicLoad32U { memarg: wr_memarg },
                            WP::I64AtomicLoad32U { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicStore { memarg: wr_memarg },
                            WP::I32AtomicStore { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicStore { memarg: wr_memarg },
                            WP::I64AtomicStore { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicStore8 { memarg: wr_memarg },
                            WP::I32AtomicStore8 { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicStore16 { memarg: wr_memarg },
                            WP::I32AtomicStore16 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicStore8 { memarg: wr_memarg },
                            WP::I64AtomicStore8 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicStore16 { memarg: wr_memarg },
                            WP::I64AtomicStore16 { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicStore32 { memarg: wr_memarg },
                            WP::I64AtomicStore32 { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwAdd { memarg: wr_memarg },
                            WP::I32AtomicRmwAdd { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwAdd { memarg: wr_memarg },
                            WP::I64AtomicRmwAdd { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8AddU { memarg: wr_memarg },
                            WP::I32AtomicRmw8AddU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16AddU { memarg: wr_memarg },
                            WP::I32AtomicRmw16AddU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8AddU { memarg: wr_memarg },
                            WP::I64AtomicRmw8AddU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16AddU { memarg: wr_memarg },
                            WP::I64AtomicRmw16AddU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32AddU { memarg: wr_memarg },
                            WP::I64AtomicRmw32AddU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwSub { memarg: wr_memarg },
                            WP::I32AtomicRmwSub { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwSub { memarg: wr_memarg },
                            WP::I64AtomicRmwSub { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8SubU { memarg: wr_memarg },
                            WP::I32AtomicRmw8SubU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16SubU { memarg: wr_memarg },
                            WP::I32AtomicRmw16SubU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8SubU { memarg: wr_memarg },
                            WP::I64AtomicRmw8SubU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16SubU { memarg: wr_memarg },
                            WP::I64AtomicRmw16SubU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32SubU { memarg: wr_memarg },
                            WP::I64AtomicRmw32SubU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwAnd { memarg: wr_memarg },
                            WP::I32AtomicRmwAnd { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwAnd { memarg: wr_memarg },
                            WP::I64AtomicRmwAnd { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8AndU { memarg: wr_memarg },
                            WP::I32AtomicRmw8AndU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16AndU { memarg: wr_memarg },
                            WP::I32AtomicRmw16AndU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8AndU { memarg: wr_memarg },
                            WP::I64AtomicRmw8AndU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16AndU { memarg: wr_memarg },
                            WP::I64AtomicRmw16AndU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32AndU { memarg: wr_memarg },
                            WP::I64AtomicRmw32AndU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwOr { memarg: wr_memarg },
                            WP::I32AtomicRmwOr { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwOr { memarg: wr_memarg },
                            WP::I64AtomicRmwOr { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8OrU { memarg: wr_memarg },
                            WP::I32AtomicRmw8OrU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16OrU { memarg: wr_memarg },
                            WP::I32AtomicRmw16OrU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8OrU { memarg: wr_memarg },
                            WP::I64AtomicRmw8OrU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16OrU { memarg: wr_memarg },
                            WP::I64AtomicRmw16OrU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32OrU { memarg: wr_memarg },
                            WP::I64AtomicRmw32OrU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwXor { memarg: wr_memarg },
                            WP::I32AtomicRmwXor { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwXor { memarg: wr_memarg },
                            WP::I64AtomicRmwXor { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8XorU { memarg: wr_memarg },
                            WP::I32AtomicRmw8XorU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16XorU { memarg: wr_memarg },
                            WP::I32AtomicRmw16XorU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8XorU { memarg: wr_memarg },
                            WP::I64AtomicRmw8XorU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16XorU { memarg: wr_memarg },
                            WP::I64AtomicRmw16XorU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32XorU { memarg: wr_memarg },
                            WP::I64AtomicRmw32XorU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwXchg { memarg: wr_memarg },
                            WP::I32AtomicRmwXchg { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwXchg { memarg: wr_memarg },
                            WP::I64AtomicRmwXchg { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8XchgU { memarg: wr_memarg },
                            WP::I32AtomicRmw8XchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16XchgU { memarg: wr_memarg },
                            WP::I32AtomicRmw16XchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8XchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw8XchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16XchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw16XchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32XchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw32XchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmwCmpxchg { memarg: wr_memarg },
                            WP::I32AtomicRmwCmpxchg { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmwCmpxchg { memarg: wr_memarg },
                            WP::I64AtomicRmwCmpxchg { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw8CmpxchgU { memarg: wr_memarg },
                            WP::I32AtomicRmw8CmpxchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I32AtomicRmw16CmpxchgU { memarg: wr_memarg },
                            WP::I32AtomicRmw16CmpxchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw8CmpxchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw8CmpxchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw16CmpxchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw16CmpxchgU { memarg: wp_memarg },
                        )
                        | (
                            WR::I64AtomicRmw32CmpxchgU { memarg: wr_memarg },
                            WP::I64AtomicRmw32CmpxchgU { memarg: wp_memarg },
                        ) => {
                            assert_eq!(wr_memarg.flags, wp_memarg.flags, "{}", func_name);
                            assert_eq!(wr_memarg.offset, wp_memarg.offset, "{}", func_name);
                        }

                        other => panic!("{:?}", other),
                    }
                }
            }
        }
    }
}
