[<EntryPoint>]
let main _ =
    let exitCode = 0

    let (numRows, numColumns) = (10, 5)
    let mutable gameGrid = GameGrid.create numRows numColumns

    let updateGrid (_: int64) =
        gameGrid <- GameGrid.update gameGrid Move.blockDown
        GameGrid.render gameGrid |> Draw.matrix

    GameEngine.run updateGrid

    exitCode
