open TreeModel
open TreeView

// TODO: Use the tree from the tree design
let tree1 = Node (1,[Node(2,[]);Node(3,[]);Node(4,[])])
let design1 = design tree1
drawTree (design1)

let tree2 = Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[])])])
let design2 = design tree2
drawTree (design2)

let tree3 = Node ("A",[Node("B",[Node ("C",[]);Node("P",[])]);Node("S",[Node("T",[]);Node("e",[])]);Node("o",[Node("p",[Node("q",[Node("r",[]);Node("s",[]);Node("t",[]);Node("u",[])]);Node("v",[Node("w",[]);Node("x",[Node("y",[]);Node("z",[])]);Node("0",[]);Node("1",[])]);Node("2",[])])])])
let design3 = design tree3
drawTree (design3)
