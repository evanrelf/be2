#![allow(dead_code)] // TODO: Remove

use crate::common::{Context, node_text, query};
use anyhow::Context as _;
use serde::Serialize;
use tree_sitter::{Node, Parser};

#[derive(Serialize)]
pub struct Haskell {
    module_name: Option<&'static str>,
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
        module_name: query_module_name(cx)?,
        imports: query_imports(cx)?,
        declarations: query_declarations(cx)?,
    })
}

fn query_module_name(cx: &Context) -> anyhow::Result<Option<&'static str>> {
    let nodes = query(cx, "(haskell (header (module) @module))")?;
    if let Some(node) = nodes.first() {
        Ok(Some(node_text(cx, node).unwrap()))
    } else {
        Ok(None)
    }
}

#[derive(Debug, Default, PartialEq, Serialize)]
struct Import {
    package: Option<&'static str>,
    module: &'static str,
    qualified: bool,
    alias: Option<&'static str>,
    hiding: bool,
    names: Option<Vec<&'static str>>,
}

fn query_imports(cx: &Context) -> anyhow::Result<Vec<Import>> {
    let nodes = query(cx, "(import) @import")?;
    let mut imports = Vec::with_capacity(nodes.len());
    for node in nodes {
        let mut import = Import::default();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "import_package" => {
                    import.package = Some(node_text(cx, &child).unwrap().trim_matches('"'));
                }
                "module" => {
                    if import.module.is_empty() {
                        import.module = node_text(cx, &child).unwrap();
                    } else {
                        import.alias = Some(node_text(cx, &child).unwrap());
                    }
                }
                "qualified" => {
                    import.qualified = true;
                }
                "import_list" => {
                    let mut list_cursor = child.walk();
                    let mut names = Vec::new();
                    for list_child in child.children(&mut list_cursor) {
                        if list_child.kind() == "import_name" {
                            names.push(node_text(cx, &list_child).unwrap());
                        }
                    }
                    import.names = Some(names);
                }
                "hiding" => {
                    import.hiding = true;
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
enum DeclarationKind {
    /// data Foo = ...
    Data,

    /// newtype Foo = Foo ...
    Newtype,

    /// type Foo = ...
    Type,

    /// class Foo ...
    Class,

    /// type family Foo ...
    TypeFamily,

    /// foo x y = ...
    Function,

    /// x `foo` y = ...
    FunctionInfix,

    /// foo = ...
    Bind,
}

#[derive(Serialize)]
struct Declaration {
    kind: DeclarationKind,
    text: &'static str,
}

// TODO: Parse nested declarations (e.g. associated type families, type class methods, etc).
fn query_declarations(cx: &Context) -> anyhow::Result<Vec<Declaration>> {
    let mut declarations = Vec::new();
    for node in query_data_type(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Data,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_newtype(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Newtype,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_type_synonym(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Type,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_class(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Class,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_type_family(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::TypeFamily,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_function(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Function,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_function_infix(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::FunctionInfix,
            text: node_text(cx, &node).unwrap(),
        });
    }
    for node in query_bind(cx)? {
        declarations.push(Declaration {
            kind: DeclarationKind::Bind,
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
    use pretty_assertions::assert_eq;

    #[test]
    fn test_query_module_name() -> anyhow::Result<()> {
        let cx = init("import Data.List")?;
        let module_name = query_module_name(&cx)?;
        assert_eq!(None, module_name);

        let cx = init("module Foo.Bar where")?;
        let module_name = query_module_name(&cx)?;
        assert_eq!(Some("Foo.Bar"), module_name);

        let cx = init("module Foo.Bar (Baz (..), qux) where")?;
        let module_name = query_module_name(&cx)?;
        assert_eq!(Some("Foo.Bar"), module_name);

        Ok(())
    }

    #[test]
    fn test_query_imports() -> anyhow::Result<()> {
        let cx = init(
            r#"
            import Foo (FooData (..), fooFun1, fooFun2)
            import Bar1
            import Bar2 ()
            import qualified Baz
            import "qux" Qux qualified as Q
            import Prelude hiding (id)
            "#,
        )?;

        let expected_imports = vec![
            Import {
                package: None,
                module: "Foo",
                qualified: false,
                alias: None,
                hiding: false,
                names: Some(vec!["FooData (..)", "fooFun1", "fooFun2"]),
            },
            Import {
                package: None,
                module: "Bar1",
                qualified: false,
                alias: None,
                hiding: false,
                names: None,
            },
            Import {
                package: None,
                module: "Bar2",
                qualified: false,
                alias: None,
                hiding: false,
                names: Some(vec![]),
            },
            Import {
                package: None,
                module: "Baz",
                qualified: true,
                alias: None,
                hiding: false,
                names: None,
            },
            Import {
                package: Some("qux"),
                module: "Qux",
                qualified: true,
                alias: Some("Q"),
                hiding: false,
                names: None,
            },
            Import {
                package: None,
                module: "Prelude",
                qualified: false,
                alias: None,
                hiding: true,
                names: Some(vec!["id"]),
            },
        ];

        let actual_imports = query_imports(&cx)?;
        assert_eq!(expected_imports, actual_imports);

        Ok(())
    }
}
