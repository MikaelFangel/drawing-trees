module public Tests

open FsCheck
open FsCheck.Xunit

open TreeModel


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
