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
    System.Console.WriteLine("Labels for the children of:" + s)
    System.Console.WriteLine("(empty indicates previous node is a leaf or previous was the last child)")

    match System.Console.ReadLine() with
    | "" -> Node(s, sub |> List.rev |> List.map addchildren)
    | label -> addchildren (Node(s, sub |> List.append [ Node(label, []) ]))

[<EntryPoint>]
let main args =
    let outputType = chooseOutputType ()

    System.Console.WriteLine("Enter the label for the root of the tree:")
    let root = System.Console.ReadLine() |> fun s -> Node(s, [])

    let tree = addchildren root

    match outputType with
    | Tikz -> System.Console.WriteLine(Tikz.drawTree (design tree))
    | Plotly -> Plotly.drawTree (design tree)

    0

//let tree4 = Node (1,[Node(2,[Node(2,[]);Node(3,[])]);Node(3,[Node (1,[Node(2,[]);Node(3,[])])])])
//let tree5 = Node (1,[Node(2,[Node(2,[]);Node(3,[])]);Node(3,[Node (1,[Node(2,[]);Node(3,[])])]);Node(4,[])])
//let (pos4,e1) = design tree4
//let (pos5,e2) = design tree5
//printfn "%A" pos4
//printfn "%A" pos5
//
//equalTree pos4 pos5 |> printfn "%A"
//posEqual pos4 pos5 true |> printfn "%A"
//
//let tree1 = Node (1,[Node(2,[]);Node(3,[]);Node(4,[])])
//let design1 = design tree1
//Plotly.drawTree (design1)
//
//let tree2 = Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[])])])
//let design2 = design tree2
//Plotly.drawTree (design2)
//
//let tree3 = Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[Node("q",[Node("r",[]);Node("s",[]);Node("t",[]);Node("u",[])]);Node("v",[Node("w",[]);Node("x",[Node("y",[]);Node("z",[])]);Node("0",[]);Node("1",[])]);Node("2",[])])])])
//let design3 = design tree3
//Plotly.drawTree (design3)
//
//printf "%s" (Tikz.drawTree (design3))
