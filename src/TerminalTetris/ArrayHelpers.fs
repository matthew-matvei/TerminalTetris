module ArrayHelpers

let trySet (array: 'a array) index (value: 'a) =
    if Array.tryItem index array |> Option.isSome then
        Array.set array index value