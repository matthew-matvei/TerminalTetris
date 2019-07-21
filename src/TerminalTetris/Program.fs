[<EntryPoint>]
let main argv =
    let exitCode = 0

    let (numRows, numColumns) = (10, 5)
    let gameGrid = GameGrid.create numRows numColumns
    GameGrid.render gameGrid |> Draw.matrix

    exitCode
