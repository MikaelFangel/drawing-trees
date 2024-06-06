type Tree<'a> = Node of 'a * 'a Tree list
type Extent = (float * float) list

let rec merge =
    function
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge (ps, qs)

let mergelist es =
    List.fold (fun s e -> merge e :: s) [] es
