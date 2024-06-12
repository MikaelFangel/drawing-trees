module Examples

open TreeModel

let tree1 = Node(1, [ Node(2, []); Node(3, []); Node(4, []) ])

let tree2 =
    Node(
        "A",
        [ Node("B", [ Node("C", []); Node("P", []) ])
          Node("S", [ Node("T", []); Node("e", []) ])
          Node("o", [ Node("p", []) ]) ]
    )

let tree3 =
    Node(
        "A",
        [ Node("B", [ Node("C", []); Node("P", []) ])
          Node("S", [ Node("T", []); Node("e", []) ])
          Node(
              "o",
              [ Node(
                    "p",
                    [ Node("q", [ Node("r", []); Node("s", []); Node("t", []); Node("u", []) ])
                      Node(
                          "v",
                          [ Node("w", [])
                            Node("x", [ Node("y", []); Node("z", []) ])
                            Node("0", [])
                            Node("1", []) ]
                      )
                      Node("2", []) ]
                ) ]
          ) ]
    )

let tree4 =
    Node(
        1,
        [ Node(2, [ Node(2, []); Node(3, []) ])
          Node(3, [ Node(1, [ Node(2, []); Node(3, []) ]) ]) ]
    )

let tree5 =
    Node(
        1,
        [ Node(2, [ Node(2, []); Node(3, []) ])
          Node(3, [ Node(1, [ Node(2, []); Node(3, []) ]) ])
          Node(4, []) ]
    )
