module public Helpers

open FsCheck
open FsCheck.Xunit

open TreeModel

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

let rec findDepth =
    function
    | Node (_, []) -> 0
    | Node(_, xs) -> let depth = (List.fold (fun acc a -> let d = (findDepth a)
                                                          if d> acc then d else acc ) 0 xs) 
                     depth + 1

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
        let (map, height) =
            List.fold
                (fun acc e ->
                    let (m1, h1) = acc
                    let (a, h) = subtreeMap m1 e
                    if h > h1 then (a, h) else (a, h1))
                (map, 0)
                subtrees

        let updatedMap = addToMap (height + 1) map (Node(x, subtrees))
        (updatedMap, height + 1)

let rec equalTree t1 t2 =
    match (t1, t2) with
    | (Node(_, []), Node(_, [])) -> true
    | (Node(_, x1), Node(_, x2)) ->
        if List.length (x1) = List.length (x2) then
            List.fold2 (fun a x y -> (equalTree x y) && a) true x1 x2
        else
            false

let rec posEqual t1 t2 root =
    match (t1, t2, root) with
    | (Node(_, x1), Node(_, x2), _) when List.length (x1) <> List.length (x2) -> false
    | (Node(_, x1), Node(_, x2), true) -> List.fold2 (fun a x y -> (posEqual x y false) && a) true x1 x2
    | (Node((_, f1), x1), Node((_, f2), x2), false) ->
        if f1 = f2 then
            if List.length (x1) = List.length (x2) then
                List.fold2 (fun a x y -> (posEqual x y false) && a) true x1 x2
            else
                false
        else
            false

// Mirrors a tree.
let rec mirrorTree (Node(a, s)) =
    Node(a, s |> List.rev |> List.map mirrorTree)

// Mirrors a positional tree.
let rec mirrorTree' (Node((a, b: float), s)) =
    Node((a, -b), s |> List.rev |> List.map mirrorTree')

type SafeFloat() =
    static member Float() =
        Arb.Default.Float() |> Arb.filter (fun f -> not <| System.Double.IsNaN(f))

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

[<Property>]
let ``EqualTree will see two identical trees as equal`` (tree: TreeModel.Tree<int>) = equalTree tree tree

[<Property(Arbitrary = [| typedefof<SafeFloat> |])>]
let ``PosEqual will see two identical trees as equal`` (tree: TreeModel.Tree<int * float>) = posEqual tree tree true

[<Property>]
let ``AddToMap inserts the element``(key:int, a: int, map: Map<int, int list>) =
    let newmap = addToMap key map a
    match (Map.tryFind key newmap) with
    | Some xs   -> List.exists (fun x -> x = a) xs
    | None      -> false

[<Property>]
let ``treeToMap makes a map with as many keys as the depth``(tree: TreeModel.Tree<int*float>) =
    let depth = findDepth tree
    let keys = treeToMap Map.empty 0 tree
               |> Map.keys |> Seq.length
    keys = (depth + 1) 


[<Property>]
let ``subTreeMap makes a map with as many keys as the depth minus one``(tree: TreeModel.Tree<int>) =
    let depth = findDepth tree
    let keys = subtreeMap Map.empty tree |> fst
               |> Map.keys |> Seq.length
    keys = depth

//Should be Fact but it is not recognized
[<Property>]
let ``treeToMap works as intended`` () =
    let expected = Map.empty.Add(0,[0.0]).Add(1,[1.0;0.0;-1.0])
    let out = Node ((1,0.0),[Node((2,-1.0),[]);Node((3,0.0),[]);Node((4,1.0),[])]) |> treeToMap Map.empty 0
    assert (expected = out) 


[<Property>]
let ``subtreeMap works as intended`` () =
    let expected1 = Map.empty.Add(1,[Node (1,[Node(2,[]);Node(3,[]);Node(4,[])])])
    let out1 = Node (1,[Node(2,[]);Node(3,[]);Node(4,[])]) |> subtreeMap Map.empty |> fst
    let expected2 = Map.empty.
                       Add(2,[Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[])])])]).
                       Add(1,[Node("o",[Node("p",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("B",[Node ("C",[]);Node("P",[])])])
    let out2 = Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[])])]) |> subtreeMap Map.empty |> fst
    assert (expected1 = out1 && expected2 = out2) 
