// Cem Akarsubasi 2024-06-10
[<RequireQualifiedAccess>]
module TreeView.Tikz

open TreeModel

let private createPoint name label pos depth parent =
    $"\\node (%O{name}) at (%f{pos + parent}, %f{depth}) {{%O{label}}};\n"

let private createLine start finish =
    $"\\draw [thick] (%O{start}) -- (%O{finish});\n"

// Gets the left most position in a list of trees.
let private findLeftMost =
    function
    | [] -> 0.0
    | Node((_, f), _) :: _ -> f

// Gets the right most position in a list of trees.
let rec private findRightMost =
    function
    | [] -> 0.0
    | [ Node((_, f), _) ] -> f
    | _ :: xs -> findRightMost xs

let private createHLine subs parent pos depth =
        let (left, right) = (findLeftMost subs, findRightMost subs)
        match (left, right) with
        | (0.0, 0.0) -> ""
        | (l, r) -> createLine $"%f{l+parent+pos}, %f{-depth - 0.5}" $"%f{r+parent+pos}, %f{-depth - 0.5}" 

let rec private drawTreeH (pos_tree: Tree<'a * float>) depth parent =
    let drawTreeH' subs toappend pos =
        subs
        |> List.map (fun tree -> drawTreeH tree (depth + 1.0) (pos + parent))
        |> List.concat
        |> List.append toappend

    match (pos_tree, depth) with
    // top node
    | (Node((a, pos), subs), 0.0) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLine a $"%f{parent + pos}, %f{-depth - 0.5}"
        let hline = createHLine subs parent pos depth
        drawTreeH' subs [ point; vline; hline; "\n" ] pos
    // bottom node
    | (Node((a, pos), []), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLine a $"%f{parent + pos}, %f{-depth + 0.5}"
        [ point; vline; "\n" ]
    // vers node
    | (Node((a, pos), subs), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline_top = createLine a $"%f{parent + pos}, %f{-depth + 0.5}"
        let vline_bot = createLine a $"%f{parent + pos}, %f{-depth - 0.5}"
        let hline = createHLine subs parent pos depth
        drawTreeH' subs [ point; vline_top; vline_bot; hline; "\n" ] pos

let private preamble = "\\begin{tikzpicture}\n"

let private postamble = "\\end{tikzpicture}\n"

/// Returns a latex string that draws the graph using basic Tikz commands
let rec drawTree design =
    let (postree, _) = design

    List.append (drawTreeH postree 0.0 0.0) [ postamble ]
    |> List.append [ preamble ]
    |> String.concat ""
