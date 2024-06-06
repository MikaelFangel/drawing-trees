module TreeView

#r "nuget: Plotly.NET, 4.0.0";;


open Plotly.NET

open Plotly.NET.LayoutObjects
let rec drawTree design = drawTreeH design 0 0.0

let rec drawTreeH (pos_tree, extent) depth parent = 
    match (pos_tree, depth) with
    | (Node ((a,pos),subs), 0)          ->  Chart.Point([(pos+parent,0)], MultiText=[a], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false)
                                            map (fun e -> drawTreeH (e,extent) depth pos) subs; 
    | (Node ((a,pos),subs), depth)      ->  Chart.Point([(pos+parent,-depth)], MultiText=[a], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false)
                                            Chart.Line([pos+parent;-depth], [parent;-(depth-1)] ,LineColor = Color.fromString "black", ShowLegend = false)
                                            map (fun e -> drawTreeH (e,extent) depth pos) subs;