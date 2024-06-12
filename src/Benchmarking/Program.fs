open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open TreeModel

let rec buildTree =
    function
    | 0 -> Node(1, [])
    | n -> Node(1, [ buildTree (n - 1); buildTree (n - 1); buildTree (n - 1) ])

type TreeBenchmarking() =
    let mutable tree = None
    let mutable dtree = None

    [<Params(1, 2, 5, 10)>]
    member val TreeSize = 0 with get, set

    [<GlobalSetup(Target = "DesignTree")>]
    member this.SetupDesignTree() = tree <- Some(buildTree this.TreeSize)

    [<GlobalSetup(Targets = [| "RenderTikz"; "RenderPlotly" |])>]
    member this.SetupRenderTikz() =
        dtree <- Some(buildTree 5 |> TreeModel.design)

    [<Benchmark>]
    member _.DesignTree() =
        match tree with
        | Some tree -> TreeModel.design tree |> ignore
        | None -> failwith "No tree to benchmark."

    [<Benchmark>]
    member _.RenderTikz() =
        match dtree with
        | Some dtree -> TreeView.Tikz.drawTree dtree |> ignore
        | None -> failwith "No tree to benchmark."

    [<Benchmark>]
    member _.RenderPlotly() =
        match dtree with
        | Some dtree -> TreeView.Plotly.drawTreeNoShow dtree |> ignore
        | None -> failwith "No tree to benchmark."

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<TreeBenchmarking>() |> ignore
    0
