use syn::{
    Type,
    parse::{Parse, ParseStream},
};

struct Task {
    name: String,
    cx_name: String,
    cx_typ: Type,
    inputs: Vec<(String, Type)>,
    output: Type,
    volatile: bool,
}

impl Parse for Task {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

// Turn build function into key struct + value struct + realize function + build function.
#[proc_macro_attribute]
pub fn task(
    _args: proc_macro::TokenStream,
    _item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    todo!()
}

struct ImplTasks {
    names: Vec<String>,
}

impl Parse for ImplTasks {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

// Turn list of task names into match expression unwrapping keys, dispatching to build functions,
// and wrapping values.
#[proc_macro]
pub fn impl_tasks(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}
