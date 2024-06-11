module public Tests

open FsCheck
open FsCheck.Xunit

open TreeModel


[<Property>]
let public ``Test`` () =
    let tree2 = Node('a', [ Node('b', []); Node('c', []); Node('d', []) ])
    let result = design tree2

    let prop =
        fun () ->
            ((=)
                result
                (Node(('a', 0.0), [ Node(('b', -1.0), []); Node(('c', 0.0), []); Node(('d', 1.0), []) ]),
                 [ (0.0, 0.0); (-1.0, 1.0) ]))

    Check.Quick prop

// TODO: Check if some of these helper functions shouldn't be moved from the testing lib to some src lib.
// Gets the left most position in a list of trees.
let findLeftMost =
    function
    | [] -> 0.0
    | Node((_, f), _) :: _ -> f

// Gets the right most position in a list of trees.
let rec findRightMost =
    function
    | [] -> 0.0
    | [ Node((_, f), _) ] -> f
    | _ :: xs -> findRightMost xs

// Gets the mean of the right most and left most position in a list of trees.
let subTreeMean sub =
    (findLeftMost sub + findRightMost sub) / 2.0

// Calculates the absolute positions in a tree and returns the tree with absolute positions.
let rec absoluteTree parent (t: Tree<'a * float>) =
    match t with
    | Node((a, f), []) -> Node((a, parent + f), [])
    | Node((a, f), subtrees) ->
        let replacement = List.map (fun t' -> absoluteTree (f + parent) t') subtrees

        Node((a, parent + f), replacement)

// Appends a value to a keys list.
let addToMap key map value =
    match Map.tryFind key map with
    | Some(values) -> Map.add key (value :: values) map
    | None -> Map.add key [ value ] map

// Converts a tree to a map where each layers is a key and the value is a list of all values at that layer.
let rec treeToMap acc depth (t: Tree<'a * float>) =
    match t with
    | Node((_, f), []) -> addToMap depth acc f
    | Node((_, f), subtrees) ->
        let updatedAcc = addToMap depth acc f
        List.fold (fun acc e -> treeToMap acc (depth + 1) e) updatedAcc subtrees

// Mirrors a tree.
let rec mirrorTree (Node(a, s)) =
    Node(a, s |> List.rev |> List.map mirrorTree)

// Mirrors a positional tree.
let rec mirrorTree' (t: Tree<'a * float>) =
    match t with
    | Node((a, f), []) -> Node((a, -f), [])
    | Node((a, f), subtrees) -> Node((a, -f), List.rev subtrees |> List.map mirrorTree')

[<Property>]
let ``Rule 1 - There is at least a given distance between nodes at the same level`` (tree: TreeModel.Tree<int>) =
    TreeModel.design tree
    |> fst
    |> absoluteTree 0.0
    |> treeToMap Map.empty 0
    |> Map.forall (fun _ v -> List.pairwise v |> List.forall (fun (x, y) -> x >= y + 1.0))

[<Property>]
let ``Rule 2 - Relative; A parent should be centered over its children`` (tree: TreeModel.Tree<int>) =
    let rec checkTree =
        function
        | Node(_, []) -> true
        | Node(_, subtrees) when 0.0 = subTreeMean subtrees -> List.fold (fun s e -> s && checkTree e) true subtrees
        | _ -> false

    TreeModel.design tree |> fst |> checkTree

[<Property>]
let ``Rule 2 - Absolute; A parent should be centered over its children`` (tree: TreeModel.Tree<int>) =
    let rec checkTree =
        function
        | Node(_, []) -> true
        | Node((_, f), subtrees) when f = subTreeMean subtrees -> List.fold (fun s e -> s && checkTree e) true subtrees
        | _ -> false

    TreeModel.design tree |> fst |> absoluteTree 0.0 |> checkTree

[<Property>]
let ``Rule 3 - The tree should be symmetric with respect to reflection`` (tree: TreeModel.Tree<int>) =
    let originalDesign = tree |> TreeModel.design |> fst
    let mirroredDesign = tree |> mirrorTree |> TreeModel.design |> fst |> mirrorTree'
    originalDesign = mirroredDesign
