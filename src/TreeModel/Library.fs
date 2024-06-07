﻿module TreeModel

type 'a Tree = 
    | Node of 'a * ('a Tree list)

type Extent = (float*float) list 

let moveTree =
    function
    | (Node((label, x), subtrees), x': float) 
        -> Node((label, x+x'), subtrees)

let moveextent (e: Extent, x) = 
    List.map (fun (p, q) -> (p+x, q+x)) e 

let rec merge (e1: Extent)  (e2: Extent) =
    match (e1, e2) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _)::ps, (_, q)::qs) -> (p, q) :: merge ps qs

let mergelist es= List.fold merge [] es

let rmax (p: float, q: float) = if p > q then p else q
let rec fit l1 l2 = 
    match l1, l2 with
    | (((_, p)::ps), ((q, _)::qs)) -> rmax( fit ps qs, p - q + 1.0)
    | (_, _) -> 0.0

let rec fitlistl es =
    let rec fitlistl' acc l = 
        match acc, l with
        | _, [] -> []
        | acc, (e::es) -> 
            let x = fit acc e
            x :: fitlistl' (merge acc (moveextent(e, x))) es
    fitlistl' [] es

let rec fitlistr es =
    let rec fitlistr' acc l = 
        match acc, l with
        | _, [] -> []
        | acc, (e::es) -> 
            let x = -(fit e acc)
            x :: fitlistr' (merge (moveextent(e, x)) acc) es
    List.rev (fitlistr' [] (List.rev es))
            
let flipextent: Extent -> Extent = List.map (fun (p, q) -> (-q, -p))

let mean = fun (x, y) -> (x+y)/2.0
let fitlist = fun es -> List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
    let rec design' = fun (Node(label, subtrees)) ->
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map moveTree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultExtent = (0.0, 0.0) :: mergelist pextents
        let resultTree = Node((label, 0.0), ptrees)
        (resultTree, resultExtent)
    design' tree