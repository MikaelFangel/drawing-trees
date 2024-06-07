module TreeView

open TreeModel
open Plotly.NET

let rec drawTreeH (pos_tree: Tree<'a*float>, extent) depth parent= 
    match (pos_tree, depth) with
    | (Node ((a,pos),subs), 0.0)    ->  let elem = Chart.Point([(pos+parent, depth)], MultiText=[a], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false)
                                        List.fold (fun a e -> List.append (drawTreeH (e,extent) (depth+1.0) (pos+parent)) a) [elem] subs; 
    | (Node ((a,pos),subs), depth)  ->  let e1 = Chart.Point([(pos+parent,-depth)], MultiText=[a], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false)
                                        let e2 = Chart.Line([parent; pos+parent], [-(depth-1.0);-depth] ,LineColor = Color.fromString "black", ShowLegend = false)
                                        List.fold (fun a e -> List.append (drawTreeH (e,extent) (depth+1.0) (pos+parent)) a) [e1;e2] subs;




let rec drawTree design =   drawTreeH design 0.0 0.0
                            |> Chart.combine 
                            |> Chart.show


