#![allow(dead_code)] // TODO: Remove

use crate::common::{Context, node_text, query};
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

#[derive(Debug, Default, PartialEq, Serialize)]
struct Import {
    package: Option<&'static str>,
    module: &'static str,
    // qualified: bool,
    alias: Option<&'static str>,
    // hiding: bool,
    // TODO: Distinguish `import Foo` from `import Foo ()`
    // TODO: Rename this to something like "names" or whatever so when hiding it isn't confusing.
    imports: Vec<&'static str>,
}

fn query_imports(cx: &Context) -> anyhow::Result<Vec<Import>> {
    let nodes = query(cx, "(import) @import")?;
    let mut imports = Vec::with_capacity(nodes.len());
    for node in nodes {
        let mut import = Import::default();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                // TODO: Strip double quotes surrounding package name
                "import_package" => {
                    import.package = Some(node_text(cx, &child).unwrap());
                }
                "module" => {
                    if import.module.is_empty() {
                        import.module = node_text(cx, &child).unwrap();
                    } else {
                        import.alias = Some(node_text(cx, &child).unwrap());
                    }
                }
                "import_list" => {
                    let mut list_cursor = child.walk();
                    for list_child in child.children(&mut list_cursor) {
                        if list_child.kind() == "import_name" {
                            import.imports.push(node_text(cx, &list_child).unwrap());
                        }
                    }
                }
                _ => {}
            }
        }
        imports.push(import);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_imports() -> anyhow::Result<()> {
        use pretty_assertions::assert_eq;
        let cx = init(
            r#"
            import Foo (FooData (..), fooFun1, fooFun2)
            import Bar ()
            import qualified Baz
            import "qux" Qux qualified as Q
            import Prelude hiding (id)
            "#,
        )?;
        let expected_imports = vec![
            Import {
                package: None,
                module: "Foo",
                alias: None,
                imports: vec!["FooData (..)", "fooFun1", "fooFun2"],
            },
            Import {
                package: None,
                module: "Bar",
                alias: None,
                imports: vec![],
            },
            Import {
                package: None,
                module: "Baz",
                alias: None,
                imports: vec![],
            },
            Import {
                package: Some("\"qux\""),
                module: "Qux",
                alias: Some("Q"),
                imports: vec![],
            },
            Import {
                package: None,
                module: "Prelude",
                alias: None,
                imports: vec!["id"],
            },
        ];
        let actual_imports = query_imports(&cx)?;
        assert_eq!(expected_imports, actual_imports);
        Ok(())
    }
}
