// Cem Akarsubasi 2024-06-10
module TikzView

open TreeModel

let private createPoint name label pos depth parent =
    $"\\node ({name}) at ({pos+parent}, {depth}) {{{label}}};\n"

let private createLine start finish =
    $"\\draw [thick] ({start}) -- ({finish});\n"

let rec private drawTreeH (pos_tree: Tree<'a * float>) depth parent =
    match (pos_tree, depth) with
    // top node
    | (Node((a, pos), subs), 0.0) ->
        let e1 = createPoint a a pos (-depth) parent
        let vline = createLine a $"{parent+pos}, {-depth-0.5}"
        let folder acc subtree = acc + (drawTreeH subtree (depth + 1.0) (pos + parent))
        List.fold folder (e1 + vline + "\n") subs
    // bottom node
    | (Node((a, pos), []), depth) ->
        let e1 = createPoint a a pos (-depth) parent
        let vline = createLine a $"{parent+pos}, {-depth+0.5}"
        let hline = createLine $"{parent}, {0.5-depth}" $"{parent+pos}, {0.5-depth}"
        e1  + vline + hline + "\n"
    // vers node
    | (Node((a, pos), subs), depth) ->
        let e1 = createPoint a a pos (-depth) parent
        let vline_top = createLine a $"{parent+pos}, {-depth+0.5}"
        let vline_bot = createLine a $"{parent+pos}, {-depth-0.5}"
        let hline = createLine $"{parent}, {0.5-depth}" $"{parent+pos}, {0.5-depth}"
        let folder acc subtree = acc + (drawTreeH subtree (depth + 1.0) (pos + parent))
        List.fold folder (e1 + vline_top + vline_bot + hline + "\n") subs
    
let private preamble =
    "\\begin{tikzpicture}\n"

let private postamble = 
    "\\end{tikzpicture}\n"

/// Returns a latex string that draws the graph using basic Tikz commands
let rec drawTreeTikZ design = 
    let (postree,extent) = design
    let center = drawTreeH postree 0.0 0.0
    preamble + center + postamble
