open TreeModel
open TreeView

type OutputType =
    | Tikz
    | Plotly

let rec chooseOutputType () =
    System.Console.WriteLine("Choose your desired output format: (Tikz or Plotly)")

    match System.Console.ReadLine().ToLower().Trim() with
    | "tikz" -> Tikz
    | "plotly" -> Plotly
    | _ ->
        System.Console.WriteLine("Invalid input. Please try again.")
        chooseOutputType ()

let rec addchildren (Node(s, sub)) =
    System.Console.WriteLine("Labels for the children of: " + s)
    System.Console.WriteLine("(empty indicates previous node is a leaf or previous was the last child)")

    match System.Console.ReadLine() with
    | "" -> Node(s, sub |> List.rev |> List.map addchildren)
    | label -> addchildren (Node(s, sub |> List.append [ Node(label, []) ]))

[<EntryPoint>]
let main args =
    match args with
    | [| "example"; n |] ->
        match n with
        | "1" -> Plotly.drawTree (design Examples.tree1)
        | "2" -> Plotly.drawTree (design Examples.tree2)
        | "3" -> Plotly.drawTree (design Examples.tree3)
        | "4" -> Plotly.drawTree (design Examples.tree4)
        | "5" -> Plotly.drawTree (design Examples.tree5)
        | _ -> failwith "Invalid example number"

        0
    | [||] ->
        let outputType = chooseOutputType ()

        System.Console.WriteLine("Enter the label for the root of the tree:")
        let root = System.Console.ReadLine() |> fun s -> Node(s, [])

        let tree = addchildren root

        match outputType with
        | Tikz -> System.Console.WriteLine(Tikz.drawTree (design tree))
        | Plotly -> Plotly.drawTree (design tree)

        0
    | _ -> 1
