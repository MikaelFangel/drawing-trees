#r "src/TreeModel/bin/Debug/net8.0/TreeModel.dll"
#r "tests/TreeModel/bin/Debug/net8.0/TestTreeModel.dll"
#r "nuget:FsCheck"
open FsCheck
open Helpers

let test tree = 
    let postree = TreeModel.design tree |> fst

    let map = subtreeMap Map.empty postree |> fst
    Map.forall (fun _ x ->
        List.allPairs x x
        |> List.forall (fun (x, y) -> if equalTree x y then posEqual x y true else true)) map
    |> Prop.classify (Map.forall (fun _ x ->
                        List.allPairs x x
                         |> List.forall (fun (x, y) -> not (equalTree x y))) map) "false precondition"

Check.Quick test 