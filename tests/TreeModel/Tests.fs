module public Tests

open FsCheck
open FsCheck.Xunit

open TreeModel

type SafeFloat() =
    static member Float() =
        Arb.Default.Float() |> Arb.filter (fun f -> not <| System.Double.IsNaN(f))
       
let treegen =
    let rec tree' s = 
        match s with
        | 0 -> Gen.choose (0,9) |> Gen.map (fun x -> Node (x,[]))
        | n when n>0 -> 
            let subtree = tree' (n/2) 
            Gen.oneof [Gen.choose (0,9) |> Gen.map3 (fun s1 s2 x -> Node (x,[s1; s2])) subtree subtree;
                       Gen.choose (0,9) |> Gen.map4 (fun s1 s2 s3 x-> Node (x,[s1; s2; s3])) subtree subtree subtree;
                       Gen.choose (0,9) |> Gen.map (fun x -> Node (x,[]))]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

type MyGenerators =
  static member Tree() =
      {new Arbitrary<TreeModel.Tree<int>>() with
          override _.Generator = treegen
          override _.Shrinker _ = Seq.empty }

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

let rec subtreeMap (map: Map<int, list<Tree<'a>>>) t =
     match t with
     | Node(_, []) -> (map, 0)
     | Node(x, subtrees) -> 
        let (map, height) = List.fold (fun acc e -> let (m1, h1) = acc
                                                    let (a,h) = subtreeMap m1 e
                                                    if h>h1 then (a,h) else (a,h1)
                                                    ) (map, 0) subtrees
        let updatedMap = addToMap (height+1) map (Node(x, subtrees))
        (updatedMap, height+1)

let rec equalTree t1 t2 =
    match (t1, t2) with
    | (Node (_, []), Node (_, []))  -> true
    | (Node (_, x1), Node (_, x2))  ->  if List.length(x1) = List.length(x2) 
                                        then List.fold2 (fun a x y -> (equalTree x y) && a ) true x1 x2 
                                        else false

let rec posEqual t1 t2 root =
    match (t1, t2, root) with
    | (Node (_, x1), Node (_, x2), true)  -> List.fold2 (fun a x y -> (posEqual x y false) && a ) true x1 x2
    | (Node ((_, f1), x1), Node ((_, f2), x2), false)   ->  if f1 = f2 
                                                            then if List.length(x1) = List.length(x2)
                                                                 then List.fold2 (fun a x y -> (posEqual x y false) && a ) true x1 x2 
                                                                 else false
                                                            else false
 
let negNum num = if num <> 0.0 then -num else num

// Mirrors a tree.
let rec mirrorTree (Node(a, s)) =
    Node(a, s |> List.rev |> List.map mirrorTree)


// Mirrors a positional tree.
let rec mirrorTree' (Node((a, b), s)) =
    Node((a, -b), s |> List.rev |> List.map mirrorTree')

// Helper function tests.
[<Property(Arbitrary = [| typeof<SafeFloat> |])>]
let ```findLeftMost - Return the first element of the list`` (tree: TreeModel.Tree<int * float> list) =
    (findLeftMost tree) = if tree = [] then
                              0.0
                          else
                              (tree |> List.head |> (fun (Node((_, f), _)) -> f))

[<Property(Arbitrary = [| typeof<SafeFloat> |])>]
let ``findRightMost - Return the last element of a list`` (tree: TreeModel.Tree<int * float> list) =
    (findRightMost tree) = if tree = [] then
                               0.0
                           else
                               (tree |> List.rev |> List.head |> (fun (Node((_, f), _)) -> f))

[<Property>]
let ``mirrorTree - Mirroring should return to original`` (tree: TreeModel.Tree<int>) =
    tree = (tree |> mirrorTree |> mirrorTree)

[<Property(Arbitrary = [| typeof<SafeFloat> |])>]
let ``mirrorTree' - Mirroring should return to original`` (tree: TreeModel.Tree<int * float>) =
    tree = (tree |> mirrorTree' |> mirrorTree')


// Tree prroperty tests.
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

[<Property>]
let ``Rule 4 - identical subtrees are rendered the same`` (tree: TreeModel.Tree<int>) =
    let postree = TreeModel.design tree |> fst
    subtreeMap Map.empty postree |> fst
    |> Map.forall (fun _ x -> List.allPairs x x |> List.forall (fun (x, y) -> if equalTree x y then posEqual x y true else true))
    
