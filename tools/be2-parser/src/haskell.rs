#![allow(dead_code)] // TODO: Remove

use crate::common::{Context, node_text, query, query_structured};
use anyhow::Context as _;
use serde::Serialize;
use tree_sitter::{Node, Parser};

#[derive(Serialize)]
pub struct Haskell {
    imports: Vec<Import>,
    declarations: Vec<Declaration>,
}

pub fn init(source_code: &'static str) -> anyhow::Result<Context> {
    let mut parser = Parser::new();
    let language = tree_sitter_haskell::LANGUAGE.into();
    parser.set_language(&language)?;
    let tree = parser
        .parse(source_code, None)
        .context("Failed to parse Haskell code")?;
    let cx = Context {
        language,
        source_code,
        tree,
    };
    Ok(cx)
}

pub fn parse(cx: &Context) -> anyhow::Result<Haskell> {
    Ok(Haskell {
        imports: query_imports(cx)?,
        declarations: query_declarations(cx)?,
    })
}

#[derive(Serialize)]
struct Import {
    module: &'static str,
    imports: Option<&'static str>,
    // TODO: Say whether qualified, and if as an alias
}

// TODO: Is it possible to query for optional attributes? Or do I have to write a new query for
// every variation?
fn query_imports(cx: &Context) -> anyhow::Result<Vec<Import>> {
    let mut imports = query_implicit_imports(cx)?;
    imports.extend(query_explicit_imports(cx)?);
    Ok(imports)
}

fn query_implicit_imports(cx: &Context) -> anyhow::Result<Vec<Import>> {
    let nodes = query(cx, "(import module: (module) @module)")?;
    let mut imports = Vec::with_capacity(nodes.len());
    for node in nodes {
        imports.push(Import {
            module: node_text(cx, &node).unwrap(),
            imports: None,
        });
    }
    Ok(imports)
}

fn query_explicit_imports(cx: &Context) -> anyhow::Result<Vec<Import>> {
    let nodes = query_structured(
        cx,
        "(import module: (module) @module names: (import_list) @imports)",
    )?;
    let mut imports = Vec::with_capacity(nodes.len());
    for node in nodes {
        imports.push(Import {
            module: node_text(cx, &node["module"]).unwrap(),
            imports: Some(node_text(cx, &node["imports"]).unwrap()),
        });
    }
    Ok(imports)
}

// fn query_exports(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
//     let explicit = query_explicit_exports(cx)?;
//     if explicit.is_empty() {
//         query_declarations(cx)
//     } else {
//         Ok(explicit)
//     }
// }

fn query_explicit_exports(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(cx, "(haskell (header (exports export: (_) @export)))")
}

#[derive(Serialize)]
struct Declaration {
    text: &'static str,
}

fn query_declarations(cx: &Context) -> anyhow::Result<Vec<Declaration>> {
    let mut nodes = query_data_type(cx)?;
    nodes.extend(query_newtype(cx)?);
    nodes.extend(query_type_synonym(cx)?);
    nodes.extend(query_class(cx)?);
    nodes.extend(query_type_family(cx)?);
    nodes.extend(query_function(cx)?);
    nodes.extend(query_function_infix(cx)?);
    nodes.extend(query_bind(cx)?);
    let mut declarations = Vec::with_capacity(nodes.len());
    for node in nodes {
        declarations.push(Declaration {
            text: node_text(cx, &node).unwrap(),
        });
    }
    Ok(declarations)
}

fn query_data_type(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(
        cx,
        "(haskell (declarations (data_type name: (_) @data_type)))",
    )
}

fn query_newtype(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(cx, "(haskell (declarations (newtype name: (_) @newtype)))")
}

fn query_type_synonym(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(
        cx,
        "(haskell (declarations (type_synomym name: (_) @type_synonym)))",
    )
}

fn query_class(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(cx, "(haskell (declarations (class name: (_) @class)))")
}

fn query_type_family(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(
        cx,
        "(haskell (declarations (type_family name: (_) @type_family)))",
    )
}

fn query_function(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(
        cx,
        "(haskell (declarations (function name: (_) @function)))",
    )
}

fn query_function_infix(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(
        cx,
        "(haskell (declarations (function (infix operator: (_) @function))))",
    )
}

fn query_bind(cx: &Context) -> anyhow::Result<Vec<Node<'_>>> {
    query(cx, "(haskell (declarations (bind name: (_) @bind)))")
}
