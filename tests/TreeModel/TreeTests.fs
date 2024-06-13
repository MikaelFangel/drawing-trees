module public Tests

open FsCheck
open FsCheck.Xunit

open Helpers
open TreeModel

let treegen =
    let rec tree' s =
        match s with
        | 0 -> Gen.choose (0, 9) |> Gen.map (fun x -> Node(x, []))
        | n when n > 0 ->
            let subtree = tree' (n / 2)

            Gen.oneof
                [ Gen.choose (0, 9)
                  |> Gen.map3 (fun s1 s2 x -> Node(x, [ s1; s2 ])) subtree subtree
                  Gen.choose (0, 9)
                  |> Gen.map4 (fun s1 s2 s3 x -> Node(x, [ s1; s2; s3 ])) subtree subtree subtree
                  Gen.choose (0, 9) |> Gen.map (fun x -> Node(x, [])) ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'


let treewsubtree =
    let rec tree' s =
        match s with
        | 0 -> Gen.choose (0, 9) |> Gen.map (fun x -> Node(x, []))
        | n when n > 0 ->
            let subtree = tree' (n / 2)

            Gen.oneof
                [ Gen.choose (0, 9)
                  |> Gen.map2 (fun s1 x -> Node(x, [ s1; s1 ])) subtree
                  Gen.choose (0, 9)
                  |> Gen.map3 (fun s1 s2 x -> Node(x, [ s1; s2])) subtree subtree
                  Gen.choose (0, 9) |> Gen.map (fun x -> Node(x, [])) ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

type SubtreeGen () =
    static member Tree() =
        { new Arbitrary<TreeModel.Tree<int>>() with
            override _.Generator = treewsubtree
            override _.Shrinker _ = Seq.empty }


// Model property tests
[<Property>]
let ``Design conserves tree structure`` (tree: TreeModel.Tree<int>) =
    let postree = TreeModel.design tree |> fst
    equalTree postree tree

// Tree property tests.
[<Property>]
let ``Rule 1 - There is at least a given distance between nodes at the same level`` (tree: TreeModel.Tree<int>) =
    TreeModel.design tree
    |> fst
    |> absoluteTree 0.0
    |> treeToMap Map.empty 0
    |> Map.forall (fun _ v -> List.pairwise v |> List.forall (fun (x, y) -> x >= y + 1.0))
    |> Prop.classify (oneNode tree) "only one node"

[<Property>]
let ``Rule 2 - Relative; A parent should be centered over its children`` (tree: TreeModel.Tree<int>) =
    let rec checkTree =
        function
        | Node(_, []) -> true
        | Node(_, subtrees) when 0.0 = subTreeMean subtrees -> List.fold (fun s e -> s && checkTree e) true subtrees
        | _ -> false

    TreeModel.design tree |> fst |> checkTree
    |> Prop.classify (oneNode tree) "only one node"

[<Property>]
let ``Rule 2 - Absolute; A parent should be centered over its children`` (tree: TreeModel.Tree<int>) =
    let rec checkTree =
        function
        | Node(_, []) -> true
        | Node((_, f), subtrees) when f = subTreeMean subtrees -> List.fold (fun s e -> s && checkTree e) true subtrees
        | _ -> false

    TreeModel.design tree |> fst |> absoluteTree 0.0 |> checkTree
    |> Prop.classify (oneNode tree) "only one node"


[<Property>]
let ``Rule 3 - The tree should be symmetric with respect to reflection`` (tree: TreeModel.Tree<int>) =
    let originalDesign = tree |> TreeModel.design |> fst
    let mirroredDesign = tree |> mirrorTree |> TreeModel.design |> fst |> mirrorTree'
    originalDesign = mirroredDesign
    |> Prop.classify (oneNode tree) "only one node" 

[<Property>]
let ``Rule 4 - identical subtrees are rendered the same`` (tree: TreeModel.Tree<int>) =
    let postree = TreeModel.design tree |> fst

    let map = subtreeMap Map.empty postree |> fst
    Map.forall (fun _ x ->
        List.allPairs x x
        |> List.forall (fun (x, y) -> if equalTree x y then posEqual x y true else true)) map
    |> Prop.classify (oneNode tree) "one"
    |> Prop.classify (Map.forall (fun _ x -> List.length x <=1) map) "no subtrees of same size"
    

    