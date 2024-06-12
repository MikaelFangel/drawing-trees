module TreeModel

type Tree<'a> = Node of 'a * (list<Tree<'a>>)

type Extent = (float * float) list

let internal moveTree =
    function
    | (Node((label, x), subtrees), x': float) -> Node((label, x + x'), subtrees)

let internal moveextent (e: Extent, x) =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec internal merge (e1: Extent) (e2: Extent) =
    match (e1, e2) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs

let internal mergelist es = List.fold merge [] es

let internal rmax (p: float, q: float) = if p > q then p else q

let rec internal fit l1 l2 =
    match l1, l2 with
    | (((_, p) :: ps), ((q, _) :: qs)) -> rmax (fit ps qs, p - q + 1.0)
    | (_, _) -> 0.0

let rec internal fitlistl es =
    let rec fitlistl' acc l =
        match acc, l with
        | _, [] -> []
        | acc, (e :: es) ->
            let x = fit acc e
            x :: fitlistl' (merge acc (moveextent (e, x))) es

    fitlistl' [] es

let rec internal fitlistr es =
    let rec fitlistr' acc l =
        match acc, l with
        | _, [] -> []
        | acc, (e :: es) ->
            let x = -(fit e acc)
            x :: fitlistr' (merge (moveextent (e, x)) acc) es

    List.rev (fitlistr' [] (List.rev es))

let internal mean = fun (x, y) -> (x + y) / 2.0

let internal fitlist =
    fun es -> List.map mean (List.zip (fitlistl es) (fitlistr es))


/// Given a Tree<'a>, return a Tree<'a * float> where the second element of the tuple of 
/// each node indicates the horizontal position of the given node relative to its parent
let rec design =
    function
    | Node(label, subtrees) ->
        let (trees, extents) = List.unzip (List.map design subtrees)
        let positions = fitlist extents
        let ptrees = List.map moveTree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultExtent = (0.0, 0.0) :: mergelist pextents
        let resultTree = Node((label, 0.0), ptrees)
        (resultTree, resultExtent)
