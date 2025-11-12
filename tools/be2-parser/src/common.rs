use tree_sitter::{Language, Node, QueryCursor, StreamingIterator as _, Tree};

pub struct Context {
    pub language: Language,
    pub source_code: &'static str,
    pub tree: Tree,
}

pub fn query<'a>(cx: &'a Context, query: &str) -> anyhow::Result<Vec<Node<'a>>> {
    let root_node = cx.tree.root_node();
    let query = tree_sitter::Query::new(&cx.language, query)?;
    let mut query_cursor = QueryCursor::new();
    let mut query_matches = query_cursor.matches(&query, root_node, cx.source_code.as_bytes());
    let mut results = Vec::with_capacity(query_matches.size_hint().0);
    while let Some(query_match) = query_matches.next() {
        for match_capture in query_match.captures {
            results.push(match_capture.node);
        }
    }
    Ok(results)
}

pub fn node_text(cx: &Context, node: &Node) -> Option<&'static str> {
    cx.source_code.get(node.byte_range())
}
