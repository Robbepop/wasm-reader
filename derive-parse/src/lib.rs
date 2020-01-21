extern crate proc_macro;

use derive_utils::quick_derive;
use proc_macro::TokenStream;

#[proc_macro_derive(Parser)]
pub fn derive_parser(input: TokenStream) -> TokenStream {
    quick_derive! {
        input,
        (wasm_reader_traits::Parser),
        trait Parser {
            type Item;
            type Next;
            type Error;

            fn next<R: Read>(self, reader: &mut R) -> ParseResult<Self::Item, Self::Next, Self::Error>;
        }
    }
}

#[proc_macro_derive(IntoParser)]
pub fn derive_intoparser(input: TokenStream) -> TokenStream {
    quick_derive! {
        input,
        (wasm_reader_traits::IntoParser),
        pub trait IntoParser {
            type Item;
            type Next;
            type Error;
            type Parser: Parser<Item = Self::Item, Next = Self::Next, Error = Self::Error>;

            fn into(self) -> Self::Parser;
        }
    }
}
