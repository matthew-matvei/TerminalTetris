module ArrayHelpers

let trySet (array: 'a array) (index: uint32) (value: 'a) =
    if Array.tryItem (int index) array |> Option.isSome then
        Array.set array (int index) value
