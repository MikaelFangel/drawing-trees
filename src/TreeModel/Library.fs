module TreeModel

type Tree<'a> = Node of 'a * 'a Tree list
type Extent = (float * float) list

let movetree (Node((label, x), subtrees), x': float) = Node((label, x + x'), subtrees)

let moveextent (e: Extent) x : Extent =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec merge (ps: Extent) (qs: Extent) =
    match (ps, qs) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs

let mergelist es = List.fold merge [] es
