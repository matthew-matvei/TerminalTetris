namespace TerminalTetris

module ArrayHelpers2D =

    let tryItem index1 index2 (array2d: 'a array array) =
        Array.tryItem index1 array2d
        |> Option.bind (Array.tryItem index2)

    let iteri action (array2d: 'a array array) =
        let rowLength =
            Array.tryHead array2d
            |> Option.map Array.length
            |> Option.defaultValue 0

        for rowIndex in seq { 0 .. array2d.Length - 1 } do
            for columnIndex in seq { 0 .. rowLength - 1 } do
                let currentValue =
                    Array.item rowIndex array2d
                    |> Array.item columnIndex

                action (rowIndex, columnIndex) currentValue
