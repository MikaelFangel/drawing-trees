open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open TreeModel

let rec buildTree =
    function
    | 0 -> Node(1, [])
    | n -> Node(1, [ buildTree (n - 1); buildTree (n - 1); buildTree (n - 1) ])

type TreeModelBenchmarking() =
    let mutable tree = None

    [<Params(1, 2, 5, 10)>]
    member val TreeSize = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() = tree <- Some(buildTree this.TreeSize)

    [<Benchmark>]
    member _.DesignTree() =
        match tree with
        | Some tree -> TreeModel.design tree
        | None -> failwith "No tree to benchmark."

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<TreeModelBenchmarking>() |> ignore
    0
