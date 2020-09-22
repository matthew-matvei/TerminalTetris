namespace TerminalTetris

module ArrayHelpers2D =

    let tryItem index1 index2 (array2d: 'a array array) =
        Array.tryItem index1 array2d
        |> Option.bind (Array.tryItem index2)
