// Cem Akarsubasi 2024-06-10
[<RequireQualifiedAccess>]
module TreeView.Tikz

open TreeModel

let private createPoint name label pos depth parent =
    $"\\node (%O{name}) at (%f{pos + parent}, %f{depth}) {{%O{label}}};\n"

let private createLine start finish =
    $"\\draw [thick] (%O{start}) -- (%O{finish});\n"

let rec private drawTreeH (pos_tree: Tree<'a * float>) depth parent =
    let drawTreeH' subs toappend pos =
        subs
        |> Seq.map (fun tree -> drawTreeH tree (depth + 1.0) (pos + parent))
        |> Seq.concat
        |> Seq.append toappend

    match (pos_tree, depth) with
    // top node
    | (Node((a, pos), subs), 0.0) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLine a $"%f{parent + pos}, %f{-depth - 0.5}"
        drawTreeH' subs [ point; vline; "\n" ] pos
    // bottom node
    | (Node((a, pos), []), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLine a $"%f{parent + pos}, %f{-depth + 0.5}"
        let hline = createLine $"%f{parent}, %f{0.5 - depth}" $"%f{parent + pos}, %f{0.5 - depth}"
        [ point; vline; hline; "\n" ]
    // vers node
    | (Node((a, pos), subs), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline_top = createLine a $"%f{parent + pos}, %f{-depth + 0.5}"
        let vline_bot = createLine a $"%f{parent + pos}, %f{-depth - 0.5}"
        let hline = createLine $"%f{parent}, %f{0.5 - depth}" $"%f{parent + pos}, %f{0.5 - depth}"
        drawTreeH' subs [ point; vline_top; vline_bot; hline; "\n" ] pos

let private preamble = "\\begin{tikzpicture}\n"

let private postamble = "\\end{tikzpicture}\n"

/// Returns a latex string that draws the graph using basic Tikz commands
let rec drawTree design =
    let (postree, _) = design

    Seq.append (drawTreeH postree 0.0 0.0) [ postamble ]
    |> Seq.append [ preamble ]
    |> String.concat ""
