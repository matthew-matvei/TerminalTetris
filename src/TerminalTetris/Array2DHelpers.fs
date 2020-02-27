module Array2DHelpers

let item i j (array2D: 'a array[]) =
    Array.item i array2D
    |> Array.item j

let tryItem i j (array2D: 'a array[]) =
    Array.tryItem i array2D
    |> Option.bind (Array.tryItem j)