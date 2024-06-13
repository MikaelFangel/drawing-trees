// Cem Akarsubasi 2024-06-10
[<RequireQualifiedAccess>]
module TreeView.Tikz

open TreeModel

let inline private createPoint name label pos depth parent =
    $"\\node (%O{name}) at (%.2f{pos + parent}, %.2f{depth}) {{%O{label}}};\n"

let inline private createLinePXY p x2 y2 = 
    $"\\draw [thick] (%O{p}) -- (%.2f{x2}, %.2f{y2});\n"

let inline private createLineXY x1 y1 x2 y2 = 
    $"\\draw [thick] (%.2f{x1}, %.2f{y1}) -- (%.2f{x2}, %.2f{y2});\n"

// Gets the left most position in a list of trees.
let private findLeftMost =
    function
    | [] -> 0.0
    | Node((_, f), _) :: _ -> f

// Gets the right most position in a list of trees.
[<TailCall>]
let rec private findRightMost =
    function
    | [] -> 0.0
    | [ Node((_, f), _) ] -> f
    | _ :: xs -> findRightMost xs

let inline private createHLine subs parent pos depth =
        let (left, right) = (findLeftMost subs, findRightMost subs)
        match (left, right) with
        | (0.0, 0.0) -> ""
        | (l, r) -> createLineXY (l+parent+pos) (-depth - 0.5) (r+parent+pos) (-depth - 0.5)

[<TailCall>]
let rec private drawTree' (pos_tree) depth parent: string list =
    let inline drawTreeH' subs pos =
        subs |> List.collect (fun tree -> drawTree' tree (depth + 1.0) (pos + parent))

    match (pos_tree, depth) with
    // top node
    | (Node((a, pos), subs), 0.0) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLinePXY a (parent + pos) (-depth - 0.5)
        let hline = createHLine subs parent pos depth
        [ point; vline; hline ] @ drawTreeH' subs pos
    // bottom node
    | (Node((a, pos), []), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline = createLinePXY a (parent + pos) (-depth + 0.5)
        [ point; vline ]
    // vers node
    | (Node((a, pos), subs), depth) ->
        let point = createPoint a a pos (-depth) parent
        let vline_top = createLinePXY a (parent + pos) (-depth + 0.5)
        let vline_bot = createLinePXY a (parent + pos) (-depth - 0.5)
        let hline = createHLine subs parent pos depth
        [ point; vline_top; vline_bot; hline; ] @ drawTreeH' subs pos

let private preamble = "\\begin{tikzpicture}\n"

let private postamble = "\\end{tikzpicture}\n"

/// Returns a latex string that draws the graph using basic Tikz commands
let drawTree design =
    let (postree, _) = design
    let all = [preamble] @ (drawTree' postree 0.0 0.0) @ [postamble] 
    all
    |> String.concat ""
