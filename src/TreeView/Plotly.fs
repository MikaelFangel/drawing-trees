[<RequireQualifiedAccess>]
module TreeView.Plotly

open TreeModel
open Plotly.NET


let private createPoint a pos depth parent =
    Chart.Point(
        [ (pos + parent, depth) ],
        MultiText = [ a ],
        MultiTextPosition = [ StyleParam.TextPosition.TopLeft ],
        ShowLegend = false
    )

//creates a line to the parent
let private createParentLine pos depth parent =
    Chart.Line(
        [ parent; pos + parent ],
        [ depth + 1.0; depth ],
        LineColor = Color.fromString "black",

        ShowLegend = false
    )

// draws a vertical line from (pos+parent, depth) to (parent,depth)
let private createHLine pos depth parent =
    Chart.Line(
        [ pos+parent; parent],
        [ depth; depth],
        LineColor = Color.fromString "black",
        ShowLegend = false
    )

// draws a vertical line from pos,depth straigth down
let private createVLine pos depth =
    Chart.Line(
        [ pos; pos],
        [ depth; depth - 0.5],
        LineColor = Color.fromString "black",
        ShowLegend = false
    )

let rec private drawTreeH (pos_tree: Tree<'a * float>) depth parent =
    match (pos_tree, depth) with
    | (Node((a, pos), subs), 0.0) ->
        let e1 = createPoint a pos depth parent
        let e2 = createVLine pos depth

        List.fold (fun a x -> List.append (drawTreeH x (depth + 1.0) (pos + parent)) a) [ e1;e2] subs
    | (Node((a, pos), []), depth) ->
        let e1 = createPoint a pos -depth parent
        let e2 = createVLine (pos+parent) (-depth+0.5)
        let e3 = createHLine pos (-depth+0.5) parent
        [e1;e2;e3]
    | (Node((a, pos), subs), depth) ->
        let e1 = createPoint a pos -depth parent
        let e2 = createVLine (pos+parent) (-depth+0.5)
        let e3 = createHLine pos (-depth+0.5) parent
        let e4 = createVLine (pos+parent) -depth   

        List.fold (fun a x -> List.append (drawTreeH x (depth + 1.0) (pos + parent)) a) [e1;e2;e3;e4] subs

// let rec drawTreeH (pos_tree: Tree<'a * float>) depth parent =
//     match (pos_tree, depth) with
//     | (Node((a, pos), subs), 0.0) ->
//         let elem = createPoint a pos depth parent
//         List.fold (fun a x -> List.append (drawTreeH x (depth + 1.0) (pos + parent)) a) [ elem ] subs
//     | (Node((a, pos), subs), depth) ->
//         let e1 = createPoint a pos -depth parent
//         let e2 = createParentLine pos -depth parent
//         List.fold (fun a x -> List.append (drawTreeH x (depth + 1.0) (pos + parent)) a) [ e1; e2 ] subs

let rec drawTree design = 
    let postree = design |> fst
    drawTreeH postree 0.0 0.0 |> Chart.combine |> Chart.show;

let rec drawTreeNoShow design = 
    let postree = design |> fst
    drawTreeH postree 0.0 0.0 |> Chart.combine
